{-# language CPP #-}
-- | = Name
--
-- XR_KHR_loader_init_android - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init_android  XR_KHR_loader_init_android>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 90
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @@
--
-- = See Also
--
-- 'LoaderInitInfoAndroidKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init_android OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_loader_init_android  ( LoaderInitInfoAndroidKHR(..)
                                                     , KHR_loader_init_android_SPEC_VERSION
                                                     , pattern KHR_loader_init_android_SPEC_VERSION
                                                     , KHR_LOADER_INIT_ANDROID_EXTENSION_NAME
                                                     , pattern KHR_LOADER_INIT_ANDROID_EXTENSION_NAME
                                                     , IsLoaderInitInfoKHR(..)
                                                     , LoaderInitInfoBaseHeaderKHR(..)
                                                     ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.Extensions.XR_KHR_loader_init (IsLoaderInitInfoKHR(..))
import OpenXR.Extensions.XR_KHR_loader_init (LoaderInitInfoBaseHeaderKHR(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_LOADER_INIT_INFO_ANDROID_KHR))
import OpenXR.Extensions.XR_KHR_loader_init (IsLoaderInitInfoKHR(..))
import OpenXR.Extensions.XR_KHR_loader_init (LoaderInitInfoBaseHeaderKHR(..))
-- | XrLoaderInitInfoAndroidKHR - Initializes OpenXR loader on Android
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrLoaderInitInfoAndroidKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'LoaderInitInfoAndroidKHR'
--
-- -   #VUID-XrLoaderInitInfoAndroidKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_LOADER_INIT_INFO_ANDROID_KHR'
--
-- -   #VUID-XrLoaderInitInfoAndroidKHR-next-next# @next@ /must/ be @NULL@
--     or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrLoaderInitInfoAndroidKHR-applicationVM-parameter#
--     @applicationVM@ /must/ be a pointer value
--
-- -   #VUID-XrLoaderInitInfoAndroidKHR-applicationContext-parameter#
--     @applicationContext@ /must/ be a pointer value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Extensions.XR_KHR_loader_init.initializeLoaderKHR'
data LoaderInitInfoAndroidKHR = LoaderInitInfoAndroidKHR
  { -- | @applicationVM@ is a pointer to the JNI’s opaque @JavaVM@ structure,
    -- cast to a void pointer.
    applicationVM :: Ptr ()
  , -- | @applicationContext@ is a JNI reference to an @android.content.Context@
    -- associated with the application, cast to a void pointer.
    applicationContext :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LoaderInitInfoAndroidKHR)
#endif
deriving instance Show LoaderInitInfoAndroidKHR

instance IsLoaderInitInfoKHR LoaderInitInfoAndroidKHR where
  toLoaderInitInfoBaseHeaderKHR LoaderInitInfoAndroidKHR{} = LoaderInitInfoBaseHeaderKHR{type' = TYPE_LOADER_INIT_INFO_ANDROID_KHR}

instance ToCStruct LoaderInitInfoAndroidKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LoaderInitInfoAndroidKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_LOADER_INIT_INFO_ANDROID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (applicationVM)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (applicationContext)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_LOADER_INIT_INFO_ANDROID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct LoaderInitInfoAndroidKHR where
  peekCStruct p = do
    applicationVM <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    applicationContext <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ LoaderInitInfoAndroidKHR
             applicationVM applicationContext

instance Storable LoaderInitInfoAndroidKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LoaderInitInfoAndroidKHR where
  zero = LoaderInitInfoAndroidKHR
           zero
           zero


type KHR_loader_init_android_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_KHR_loader_init_android_SPEC_VERSION"
pattern KHR_loader_init_android_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_loader_init_android_SPEC_VERSION = 1


type KHR_LOADER_INIT_ANDROID_EXTENSION_NAME = "XR_KHR_loader_init_android"

-- No documentation found for TopLevel "XR_KHR_LOADER_INIT_ANDROID_EXTENSION_NAME"
pattern KHR_LOADER_INIT_ANDROID_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_LOADER_INIT_ANDROID_EXTENSION_NAME = "XR_KHR_loader_init_android"

