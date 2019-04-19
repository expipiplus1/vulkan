{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Write.Bespoke
  ( bespokeWriteElements
  , unmarshalledTypes
  ) where

import           Data.Text                                (Text)
import qualified Data.Text.Extra as T
import           Data.Text.Prettyprint.Doc
import           Data.Word
import           Prelude                                  hiding (Enum)
import           Text.InterpolatedString.Perl6.Unindented

import           Spec.Savvy.Enum
import           Write.Element
import           Write.Type.Enum
import           Write.Util

bespokeWriteElements :: [WriteElement]
bespokeWriteElements =
  [ namedType
    , versions
    , apiVersionWriteElement 1 0
    , apiVersionWriteElement 1 1
    , nullHandle
    , zero
    , bools
    ]
    ++ concat [win32, x11, xcb, wayland, zircon, ggp, metal, android]
    ++ bespokeMarshalledWriteElements

namedType :: WriteElement
namedType =
  let weDoc = const [qci|
        -- | Annotate a type with a name
        type (name :: k) ::: a = a
      |]
      weImports = []
      weExtensions = ["PolyKinds", "TypeOperators"]
      weName = "NamedType"
      weProvides = Unguarded <$> [ TypeAlias "(:::)" ]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
      weDepends = []
  in WriteElement{..}

versions :: WriteElement
versions =
  let weDoc getDoc = [qci|
        {document getDoc (TopLevel "VK_MAKE_VERSION")}
        pattern VK_MAKE_VERSION :: Word32 -> Word32 -> Word32 -> Word32
        pattern VK_MAKE_VERSION major minor patch <-
          (\v -> (_VK_VERSION_MAJOR v, _VK_VERSION_MINOR v, _VK_VERSION_PATCH v) -> (major, minor, patch))
          where VK_MAKE_VERSION major minor patch = major `shiftL` 22 .|. minor `shiftL` 12 .|. patch

        {document getDoc (TopLevel "VK_VERSION_MAJOR")}
        _VK_VERSION_MAJOR :: Word32 -> Word32
        _VK_VERSION_MAJOR v = v `shiftR` 22

        {document getDoc (TopLevel "VK_VERSION_MINOR")}
        _VK_VERSION_MINOR :: Word32 -> Word32
        _VK_VERSION_MINOR v = v `shiftR` 12 .&. 0x3ff

        {document getDoc (TopLevel "VK_VERSION_PATCH")}
        _VK_VERSION_PATCH :: Word32 -> Word32
        _VK_VERSION_PATCH v = v .&. 0xfff
      |]
      weImports = Unguarded <$>
                  [ Import "Data.Word" ["Word32"]
                  , Import "Data.Bits" ["(.&.)", "(.|.)", "shiftL", "shiftR"]
                  ]
      weExtensions = ["PatternSynonyms", "ViewPatterns"]
      weName = "Version Macros"
      weProvides = Unguarded <$>
                   [ Pattern "VK_MAKE_VERSION"
                   , Term "_VK_VERSION_MAJOR"
                   , Term "_VK_VERSION_MINOR"
                   , Term "_VK_VERSION_PATCH"
                   ]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
      weDepends = []
  in WriteElement{..}

apiVersionWriteElement :: Word32 -> Word32 -> WriteElement
apiVersionWriteElement major minor =
  let patternName = "VK_API_VERSION_" <> T.tShow major <> "_" <> T.tShow minor
      weDoc getDoc = [qci|
        {document getDoc (TopLevel patternName)}
        pattern {patternName} :: Word32
        pattern {patternName} = VK_MAKE_VERSION {major} {minor} 0
      |]
      weImports = [ Unguarded $ Import "Data.Word" ["Word32"] ]
      weExtensions = ["PatternSynonyms"]
      weName = "Version Macro" T.<+> T.tShow major T.<+> T.tShow minor
      weProvides = [ Unguarded $ Pattern patternName ]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
      weDepends = [Unguarded (PatternName "VK_MAKE_VERSION")]
  in WriteElement{..}

nullHandle :: WriteElement
nullHandle =
  let weDoc getDoc = [qci|
        {document getDoc (TopLevel "VK_NULL_HANDLE")}
        pattern VK_NULL_HANDLE :: Ptr a
        pattern VK_NULL_HANDLE <- ((== nullPtr) -> True)
          where VK_NULL_HANDLE = nullPtr
|]
      weImports = [Unguarded $ Import "Foreign.Ptr" ["Ptr", "nullPtr"]]
      weExtensions = ["PatternSynonyms", "ViewPatterns"]
      weName = "Null handle"
      weProvides = [Unguarded $ Pattern "VK_NULL_HANDLE"]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
      weDepends = []
  in WriteElement{..}

zero :: WriteElement
zero =
  let weDoc _ = [qci|
        -- | A class for initializing things with all zero data
        class Zero a where
          zero :: a

        instance (KnownNat n, Storable a, Zero a) => Zero (Data.Vector.Storable.Sized.Vector n a) where
          zero = Data.Vector.Storable.Sized.replicate zero

        instance Zero (Ptr a) where
          zero = nullPtr

        instance Zero Int8 where
          zero = 0

        instance Zero Int16 where
          zero = 0

        instance Zero Int32 where
          zero = 0

        instance Zero Int64 where
          zero = 0

        instance Zero Word8 where
          zero = 0

        instance Zero Word16 where
          zero = 0

        instance Zero Word32 where
          zero = 0

        instance Zero Word64 where
          zero = 0

        instance Zero Float where
          zero = 0

        instance Zero CFloat where
          zero = 0

        instance Zero CChar where
          zero = 0

        instance Zero CSize where
          zero = 0

        instance Zero CInt where
          zero = 0
      |]
      weImports = [ Unguarded (Import "Foreign.Ptr" ["nullPtr"])
                  , Unguarded (Import "Foreign.C.Types" ["CFloat", "CChar", "CSize", "CInt"])
                  , Unguarded (Import "Data.Int" ["Int8", "Int16", "Int32", "Int64"])
                  , Unguarded (Import "Data.Word" ["Word8", "Word16", "Word32", "Word64"])
                  , Unguarded (QualifiedImport "Data.Vector.Storable.Sized" ["Vector", "replicate"])
                  , Unguarded (Import "GHC.TypeNats" ["KnownNat"])
                  , Unguarded (Import "Foreign.Storable" ["Storable"])
                  ]
      weExtensions = ["FlexibleInstances", "TypeSynonymInstances"]
      weName = "Zero struct"
      weProvides = [Unguarded $ WithConstructors (TypeName "Zero")]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
      weDepends = []
  in WriteElement{..}

bools :: WriteElement
bools = writeEnum
  Enum { eName = "VkBool32"
       , eType = EnumTypeEnum
       , eAliases = []
       , eComment = Just "Note that VkBool32 is not strongly typed in the specification"
       , eElements = [EnumElement "VK_FALSE" (Left 0) Nothing
                     ,EnumElement "VK_TRUE" (Left 1) Nothing
                     ]
       , eExtensions = []
       }

voidDataWriteElement :: Text -> WriteElement
voidDataWriteElement n =
  let weDoc _ = [qci|
        -- | Opaque data
        data {n}
      |]
      weImports = []
      weExtensions = []
      weName = n
      weProvides = [Unguarded $ WithoutConstructors (TypeName n)]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Just WriteElement{..}
      weDepends = []
  in WriteElement{..}

unitPtrAliasWriteElement :: Text -> WriteElement
unitPtrAliasWriteElement t =
  aliasWriteElement t "Ptr ()" [Import "Foreign.Ptr" ["Ptr"]] []

selfPtrWriteElement :: Text -> WriteElement
selfPtrWriteElement t =
  newtypeWriteElement t [qci|{t} (Ptr {t})|] [Import "Foreign.Ptr" ["Ptr"]] []

aliasWriteElement
  :: Text
  -- ^ Name
  -> Text
  -- ^ Alias
  -> [Import]
  -- ^ Imports
  -> [HaskellName]
  -- ^ Depends
  -> WriteElement
aliasWriteElement = newtypeOrTypeWriteElement "type"

newtypeWriteElement
  :: Text
  -- ^ Name
  -> Text
  -- ^ Alias
  -> [Import]
  -- ^ Imports
  -> [HaskellName]
  -- ^ Depends
  -> WriteElement
newtypeWriteElement = newtypeOrTypeWriteElement "newtype"

newtypeOrTypeWriteElement
  :: Text
  -- ^ Declarator
  -> Text
  -- ^ Name
  -> Text
  -- ^ Alias
  -> [Import]
  -- ^ Imports
  -> [HaskellName]
  -- ^ Depends
  -> WriteElement
newtypeOrTypeWriteElement decl n t is ds =
  let weDoc getDoc = [qci|
        {document getDoc (TopLevel n)}
        {decl} {n} = {t}
          {if decl == "newtype"
          then pretty ("deriving (Storable)" :: Text)
          else mempty}
|]
      weImports = Unguarded <$>
        (is ++ [Import "Foreign.Storable" ["Storable"] | decl == "newtype"])
      weExtensions =
        ["GeneralizedNewtypeDeriving" | decl == "newtype"]
      weName = t
      -- TODO: Tidy
      weProvides = Unguarded <$>
                   if decl == "newtype"
                     then [WithConstructors (TypeName n)]
                     else [WithoutConstructors (TypeName n)]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = case decl of
        "type" -> Just WriteElement
          { weImports    = Unguarded <$> is
          , weExtensions = []
          , weProvides   = [Unguarded (WithoutConstructors (TypeName n))]
          , ..
          }
        "newtype" -> Just WriteElement{..}
          { weImports    = Unguarded <$> is
          , weExtensions = []
          , weProvides   = [Unguarded (WithoutConstructors (TypeName n))]
          , weDoc        = \_ -> pretty $ "data" T.<+> n
          }
        _ -> Nothing
      weDepends              = Unguarded <$> ds
  in WriteElement{..}


win32 :: [WriteElement]
win32 =
  [ unitPtrAliasWriteElement "HINSTANCE"
  , unitPtrAliasWriteElement "HWND"
  , unitPtrAliasWriteElement "HMONITOR"
  , unitPtrAliasWriteElement "HANDLE"
  , voidDataWriteElement "SECURITY_ATTRIBUTES"
  , aliasWriteElement "DWORD" "Word32" [Import "Data.Word" ["Word32"]] []
  , aliasWriteElement
    "LPCWSTR"
    "Ptr CWchar"
    [Import "Foreign.Ptr" ["Ptr"], Import "Foreign.C.Types" ["CWchar"]] []
  ]

x11 :: [WriteElement]
x11 =
  [ selfPtrWriteElement "Display"
  , aliasWriteElement "VisualID" "Word64" [Import "Data.Word" ["Word64"]] []
  , aliasWriteElement "Window"   "Word64" [Import "Data.Word" ["Word64"]] []
  , aliasWriteElement "RROutput" "Word64" [Import "Data.Word" ["Word64"]] []
  ]

xcb :: [WriteElement]
xcb =
  [ voidDataWriteElement "Xcb_connection_t"
  , aliasWriteElement "Xcb_visualid_t" "Word32" [Import "Data.Word" ["Word32"]] []
  , aliasWriteElement "Xcb_window_t"   "Word32" [Import "Data.Word" ["Word32"]] []
  ]

ggp :: [WriteElement]
ggp =
  [ aliasWriteElement "GgpStreamDescriptor"
                      "Word32"
                      [Import "Data.Word" ["Word32"]]
                      []
  , aliasWriteElement "GgpFrameToken"
                      "Word32"
                      [Import "Data.Word" ["Word32"]]
                      []
  ]

metal :: [WriteElement]
metal =
  [ voidDataWriteElement "CAMetalLayer"
  ]

wayland :: [WriteElement]
wayland =
  [ voidDataWriteElement "Wl_display"
  , voidDataWriteElement "Wl_surface"
  ]

zircon :: [WriteElement]
zircon =
  [aliasWriteElement "Zx_handle_t" "Word32" [Import "Data.Word" ["Word32"]] []]

android :: [WriteElement]
android =
  [voidDataWriteElement "AHardwareBuffer", voidDataWriteElement "ANativeWindow"]

----------------------------------------------------------------
-- Marshalled bespoke bits
----------------------------------------------------------------

unmarshalledTypes :: [HaskellName]
unmarshalledTypes = [TypeName "VkBaseOutStructure", TypeName "VkBaseInStructure"]

bespokeMarshalledWriteElements :: [WriteElement]
bespokeMarshalledWriteElements =
  [ aliasWriteElement "DeviceSize" "VkDeviceSize" [] [TypeName "VkDeviceSize"]
  , aliasWriteElement "DeviceAddress"
                      "VkDeviceAddress"
                      []
                      [TypeName "VkDeviceAddress"]
  , aliasWriteElement "SampleMask" "VkSampleMask" [] [TypeName "VkSampleMask"]
  , aliasWriteElement "VendorId" "VkVendorId" [] [TypeName "VkVendorId"]
  , boolConversion
  ]

boolConversion :: WriteElement
boolConversion =
  let weName = "Bool conversion"
      weImports = []
      weExtensions = [ "LambdaCase" ]
      weProvides   = Unguarded . Term <$> ["bool32ToBool", "boolToBool32"]
      weUndependableProvides = []
      weSourceDepends        = []
      weBootElement          = Nothing
      weDepends =
        Unguarded
          <$> [TypeName "VkBool32", PatternName "VK_FALSE", PatternName "VK_TRUE"]
      weDoc _ = [qci|
        bool32ToBool :: VkBool32 -> Bool
        bool32ToBool = \case
          VK_FALSE -> False
          VK_TRUE  -> True
          -- TODO: add pattern totality
          _        -> error "unhandled VkBool32 Value"

        boolToBool32 :: Bool -> VkBool32
        boolToBool32 = \case
          False -> VK_FALSE
          True  -> VK_TRUE
     |]
  in  WriteElement {..}
