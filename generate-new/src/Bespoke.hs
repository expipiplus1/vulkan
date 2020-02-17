{-# language QuasiQuotes #-}
module Bespoke
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
import           Render.Type
import           Error

bespokeElements
  :: (HasErr r, Member (Reader RenderParams) r) => Sem r (Vector RenderElement)
bespokeElements =
  fmap fromList
    .  sequenceV
    $  [ namedType
       , baseType "VkSampleMask"    ''Word32
       , baseType "VkFlags"         ''Word32
       , baseType "VkDeviceSize"    ''Word64
       , baseType "VkDeviceAddress" ''Word64
       ]
    <> [ nullHandle
       ]
    <> concat [win32, x11, xcb, wayland, zircon, ggp, metal, android]

namedType :: Sem r RenderElement
namedType = genRe "namedType" $ do
  tellExport (EType ":::")
  tellDoc "-- | Annotate a type with a name\ntype (name :: k) ::: a = a"

baseType :: Text -> Name -> Sem r RenderElement
baseType n t = genRe ("base type " <> n) $ do
  tellExport (EType n)
  tDoc <- renderType (ConT t)
  tellDoc ("type" <+> pretty n <+> "=" <+> tDoc)

----------------------------------------------------------------
-- Base Vulkan stuff
----------------------------------------------------------------

nullHandle :: Member (Reader RenderParams) r => Sem r RenderElement
nullHandle = genRe "null handle" $ do
  tellExport (EPat "VK_NULL_HANDLE")
  tellDoc [qi|
    pattern VK_NULL_HANDLE :: Ptr a
    pattern VK_NULL_HANDLE <- ((== nullPtr) -> True)
      where VK_NULL_HANDLE = nullPtr
  |]


----------------------------------------------------------------
-- Platform specific nonsense
----------------------------------------------------------------

win32 :: Member (Reader RenderParams) r => [Sem r RenderElement]
win32 =
  [ unitPtrAlias "HINSTANCE"
  , unitPtrAlias "HWND"
  , unitPtrAlias "HMONITOR"
  , unitPtrAlias "HANDLE"
  , voidData "SECURITY_ATTRIBUTES"
  , alias (ConT ''Word32)              "DWORD"
  , alias (ConT ''Ptr :@ ConT ''CWchar) "LPCWSTR"
  ]

x11 :: Member (Reader RenderParams) r => [Sem r RenderElement]
x11 =
  [ selfPtr "Display"
  , alias (ConT ''Word64) "VisualID"
  , alias (ConT ''Word64) "Window"
  , alias (ConT ''Word64) "RROutput"
  ]

xcb :: Member (Reader RenderParams) r => [Sem r RenderElement]
xcb =
  [ voidData "Xcb_connection_t"
  , alias (ConT ''Word32) "Xcb_visualid_t"
  , alias (ConT ''Word32) "Xcb_window_t"
  ]

ggp :: Member (Reader RenderParams) r => [Sem r RenderElement]
ggp =
  [ alias (ConT ''Word32) "GgpStreamDescriptor"
  , alias (ConT ''Word32) "GgpFrameToken"
  ]

metal :: Member (Reader RenderParams) r => [Sem r RenderElement]
metal =
  [ voidData "CAMetalLayer"
  ]

wayland :: Member (Reader RenderParams) r => [Sem r RenderElement]
wayland =
  [ voidData "Wl_display"
  , voidData "Wl_surface"
  ]

zircon :: Member (Reader RenderParams) r => [Sem r RenderElement]
zircon =
  [alias (ConT ''Word32) "Zx_handle_t"]

android :: Member (Reader RenderParams) r => [Sem r RenderElement]
android =
  [voidData "AHardwareBuffer", voidData "ANativeWindow"]

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

voidData :: Member (Reader RenderParams) r => Text -> Sem r RenderElement
voidData n = genRe ("data " <> n) $ do
  RenderParams {..} <- ask
  let n' = mkTyName n
  tellExport (EType n')
  tellDoc $ "data" <+> pretty n'

unitPtrAlias :: Member (Reader RenderParams) r => Text -> Sem r RenderElement
unitPtrAlias = alias (ConT ''Ptr :@ TupleT 0)

alias :: Member (Reader RenderParams) r => H.Type -> Text -> Sem r RenderElement
alias t n = genRe ("alias " <> n) $ do
  RenderParams {..} <- ask
  let n' = mkTyName n
  tDoc <- renderType t
  tellExport (EType n')
  tellDoc $ "type" <+> pretty n' <+> "=" <+> tDoc

selfPtr :: Member (Reader RenderParams) r => Text -> Sem r RenderElement
selfPtr n = genRe ("data " <> n) $ do
  RenderParams {..} <- ask
  let n' = mkTyName n
      c  = mkConName n
      t  = ConT ''Ptr :@ ConT (mkName (toString n'))
  tDoc <- renderType t
  tellExport (EData n')
  tellDoc
    $   "newtype" <+> pretty n' <+> "="
    <+> pretty c <+> "{" <+> "un" <>  pretty c <+> "::" <+> tDoc <+> "}"
