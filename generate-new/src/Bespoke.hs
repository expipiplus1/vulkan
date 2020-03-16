{-# language QuasiQuotes #-}
{-# language TemplateHaskellQuotes #-}
module Bespoke
  ( forbiddenConstants
  , bespokeElements
  , bespokeSizes
  , bespokeSchemes
  , BespokeScheme(..)
  )
where

import           Relude                  hiding ( Reader
                                                , ask
                                                )
import           Data.Text.Prettyprint.Doc
import           Polysemy
import           Polysemy.Reader
import           Data.Vector                    ( Vector )
import           Foreign.Ptr
import           Foreign.C.Types
import           Text.InterpolatedString.Perl6.Unindented

import           Haskell                       as H
import           Render.Element
import           Error
import           Marshal.Scheme
import           Marshal.Marshalable
import           CType

----------------------------------------------------------------
-- Changes to the spec
----------------------------------------------------------------

-- | These constants are defined elsewhere
forbiddenConstants :: [Text]
forbiddenConstants = ["VK_TRUE", "VK_FALSE"]

----------------------------------------------------------------
-- Schemes
----------------------------------------------------------------

data BespokeScheme where
  BespokeScheme :: (forall a. Marshalable a => Text -> a -> Maybe (MarshalScheme a)) -> BespokeScheme

bespokeSchemes :: [BespokeScheme]
bespokeSchemes =
  [ BespokeScheme $ const $ \case
    a | t@(Ptr _ (TypeName "xcb_connection_t")) <- type' a -> Just (Normal t)
    a | t@(Ptr _ (TypeName "wl_display")) <- type' a -> Just (Normal t)
    _ -> Nothing
  , -- So we render the dual purpose command properly
    BespokeScheme $ \case
    c
      | c `elem` ["vkGetPipelineCacheData", "vkGetValidationCacheDataEXT"] -> \case
        a | (Ptr NonConst Void) <- type' a, "pData" <- name a ->
          Just (Returned ByteString)
        _ -> Nothing
      | c == "vkGetShaderInfoAMD" -> \case
        a | (Ptr NonConst Void) <- type' a, "pInfo" <- name a ->
          Just (Returned ByteString)
        _ -> Nothing
    _ -> const Nothing
  ]

----------------------------------------------------------------
-- Things which are easier to write by hand
----------------------------------------------------------------

bespokeSizes :: [(Text, (Int, Int))]
bespokeSizes =
  (fst <$> concat [win32 @'[Reader RenderParams], x11, xcb2, zircon, ggp])
    <> [ ("VkSampleMask"   , (4, 4))
       , ("VkFlags"        , (4, 4))
       , ("VkDeviceSize"   , (8, 8))
       , ("VkDeviceAddress", (8, 8))
       ]


bespokeElements
  :: (HasErr r, Member (Reader RenderParams) r) => Vector (Sem r RenderElement)
bespokeElements =
  fromList
    $  [ namedType
       , baseType "VkSampleMask"    ''Word32
       , baseType "VkFlags"         ''Word32
       , baseType "VkDeviceSize"    ''Word64
       , baseType "VkDeviceAddress" ''Word64
       ]
    <> [nullHandle]
    <> wsiTypes

wsiTypes :: (HasErr r, Member (Reader RenderParams) r) => [Sem r RenderElement]
wsiTypes =
  putInWSI <$> (snd <$> concat [win32, x11, xcb2, zircon, ggp]) <> concat
    [win32', xcb1, wayland, metal, android]
 where
  putInWSI = fmap $ \re -> re
    { reExplicitModule = reExplicitModule re
      <|> Just (ModName "Graphics.Vulkan.Extensions.WSITypes")
    }


namedType :: HasErr r => Sem r RenderElement
namedType = genRe "namedType" $ do
  tellExplicitModule (ModName "Graphics.Vulkan.NamedType")
  tellExport (EType ":::")
  tellDoc "-- | Annotate a type with a name\ntype (name :: k) ::: a = a"

baseType :: (HasRenderParams r, HasErr r) => Text -> Name -> Sem r RenderElement
baseType n t = genRe ("base type " <> n) $ do
  tellExplicitModule (ModName "Graphics.Vulkan.BaseType")
  tellExport (EType n)
  tDoc <- renderType (ConT t)
  tellDoc ("type" <+> pretty n <+> "=" <+> tDoc)

----------------------------------------------------------------
-- Base Vulkan stuff
----------------------------------------------------------------

nullHandle :: Member (Reader RenderParams) r => Sem r RenderElement
nullHandle = genRe "null handle" $ do
  tellExport (EPat "VK_NULL_HANDLE")
  tellImport 'nullPtr
  tDoc <- renderType (ConT ''Ptr :@ VarT (typeName "a"))
  tellDoc [qqi|
    pattern VK_NULL_HANDLE :: {tDoc}
    pattern VK_NULL_HANDLE <- ((== nullPtr) -> True)
      where VK_NULL_HANDLE = nullPtr
  |]

----------------------------------------------------------------
-- Platform specific nonsense
----------------------------------------------------------------

type BespokeAlias r = ((Text, (Int, Int)), Sem r RenderElement)

win32 :: Member (Reader RenderParams) r => [BespokeAlias r]
win32 =
  [ alias (APtr ''())     "HINSTANCE"
  , alias (APtr ''())     "HWND"
  , alias (APtr ''())     "HMONITOR"
  , alias (APtr ''())     "HANDLE"
  , alias AWord32         "DWORD"
  , alias (APtr ''CWchar) "LPCWSTR"
  ]

win32' :: Member (Reader RenderParams) r => [Sem r RenderElement]
win32' = [voidData "SECURITY_ATTRIBUTES"]

x11 :: Member (Reader RenderParams) r => [BespokeAlias r]
x11 =
  [ alias (APtr ''()) "Display"
  , alias AWord64 "VisualID"
  , alias AWord64 "Window"
  , alias AWord64 "RROutput"
  ]

xcb1 :: Member (Reader RenderParams) r => [Sem r RenderElement]
xcb1 = [voidData "xcb_connection_t"]

xcb2 :: Member (Reader RenderParams) r => [BespokeAlias r]
xcb2 = [alias AWord32 "xcb_visualid_t", alias AWord32 "xcb_window_t"]

ggp :: Member (Reader RenderParams) r => [BespokeAlias r]
ggp =
  [ alias AWord32 "GgpStreamDescriptor"
  , alias AWord32 "GgpFrameToken"
  ]

metal :: Member (Reader RenderParams) r => [Sem r RenderElement]
metal =
  [ voidData "CAMetalLayer"
  ]

wayland :: Member (Reader RenderParams) r => [Sem r RenderElement]
wayland =
  [ voidData "wl_display"
  , voidData "wl_surface"
  ]

zircon :: Member (Reader RenderParams) r => [BespokeAlias r]
zircon =
  [alias AWord32 "zx_handle_t"]

android :: Member (Reader RenderParams) r => [Sem r RenderElement]
android =
  [voidData "AHardwareBuffer", voidData "ANativeWindow"]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

data AType = AWord32 | AWord64 | APtr Name

aTypeSize :: AType -> (Int, Int)
aTypeSize = \case
  AWord32  -> (4, 4)
  AWord64  -> (8, 8)
  APtr _ -> (8, 8)

aTypeType :: AType -> H.Type
aTypeType = \case
  AWord32 -> ConT ''Word32
  AWord64 -> ConT ''Word64
  APtr n  -> ConT ''Ptr :@ ConT n

voidData :: Member (Reader RenderParams) r => Text -> Sem r RenderElement
voidData n = genRe ("data " <> n) $ do
  RenderParams {..} <- ask
  let n' = mkTyName n
  tellExport (EType n')
  tellDoc $ "data" <+> pretty n'

alias :: Member (Reader RenderParams) r => AType -> Text -> BespokeAlias r
alias t n =
  ( (n, aTypeSize t)
  , genRe ("alias " <> n) $ do
    RenderParams {..} <- ask
    let n' = mkTyName n
    tDoc <- renderType (aTypeType t)
    tellExport (EType n')
    tellDoc $ "type" <+> pretty n' <+> "=" <+> tDoc
  )
