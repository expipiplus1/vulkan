{-# language CPP #-}
-- | = Name
--
-- XR_KHR_android_create_instance - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_create_instance  XR_KHR_android_create_instance>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 9
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'InstanceCreateInfoAndroidKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_create_instance OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_android_create_instance  ( InstanceCreateInfoAndroidKHR(..)
                                                         , KHR_android_create_instance_SPEC_VERSION
                                                         , pattern KHR_android_create_instance_SPEC_VERSION
                                                         , KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME
                                                         , pattern KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME
                                                         ) where

import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (FromCStruct(..))
import OpenXR.Core10.Enums.StructureType (StructureType)
import OpenXR.CStruct (ToCStruct)
import OpenXR.CStruct (ToCStruct(..))
import OpenXR.Zero (Zero(..))
import OpenXR.Core10.Enums.StructureType (StructureType(TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR))
-- | XrInstanceCreateInfoAndroidKHR - Creates an OpenXR Instance
--
-- == Member Descriptions
--
-- = Description
--
-- 'InstanceCreateInfoAndroidKHR' contains additional Android specific
-- information needed when calling 'OpenXR.Core10.Instance.createInstance'.
-- The @applicationVM@ field should be populated with the @JavaVM@
-- structure received by the @JNI_OnLoad@ function, while the
-- @applicationActivity@ field will typically contain a reference to a Java
-- activity object received through an application-specific native method.
-- The 'InstanceCreateInfoAndroidKHR' structure /must/ be provided in the
-- @next@ chain of the 'OpenXR.Core10.Instance.InstanceCreateInfo'
-- structure when calling 'OpenXR.Core10.Instance.createInstance'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-XrInstanceCreateInfoAndroidKHR-extension-notenabled# The @@
--     extension /must/ be enabled prior to using
--     'InstanceCreateInfoAndroidKHR'
--
-- -   #VUID-XrInstanceCreateInfoAndroidKHR-type-type# @type@ /must/ be
--     'OpenXR.Core10.Enums.StructureType.TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR'
--
-- -   #VUID-XrInstanceCreateInfoAndroidKHR-next-next# @next@ /must/ be
--     @NULL@ or a valid pointer to the
--     <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#valid-usage-for-structure-pointer-chains next structure in a structure chain>
--
-- -   #VUID-XrInstanceCreateInfoAndroidKHR-applicationVM-parameter#
--     @applicationVM@ /must/ be a pointer value
--
-- -   #VUID-XrInstanceCreateInfoAndroidKHR-applicationActivity-parameter#
--     @applicationActivity@ /must/ be a pointer value
--
-- = See Also
--
-- 'OpenXR.Core10.Enums.StructureType.StructureType',
-- 'OpenXR.Core10.Instance.createInstance',
-- 'OpenXR.Core10.Instance.destroyInstance'
data InstanceCreateInfoAndroidKHR = InstanceCreateInfoAndroidKHR
  { -- | @applicationVM@ is a pointer to the JNI’s opaque @JavaVM@ structure,
    -- cast to a void pointer.
    applicationVM :: Ptr ()
  , -- | @applicationActivity@ is a JNI reference to an @android.app.Activity@
    -- that will drive the session lifecycle of this instance, cast to a void
    -- pointer.
    applicationActivity :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (InstanceCreateInfoAndroidKHR)
#endif
deriving instance Show InstanceCreateInfoAndroidKHR

instance ToCStruct InstanceCreateInfoAndroidKHR where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p InstanceCreateInfoAndroidKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (applicationVM)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (applicationActivity)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (TYPE_INSTANCE_CREATE_INFO_ANDROID_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr ()))) (zero)
    poke ((p `plusPtr` 24 :: Ptr (Ptr ()))) (zero)
    f

instance FromCStruct InstanceCreateInfoAndroidKHR where
  peekCStruct p = do
    applicationVM <- peek @(Ptr ()) ((p `plusPtr` 16 :: Ptr (Ptr ())))
    applicationActivity <- peek @(Ptr ()) ((p `plusPtr` 24 :: Ptr (Ptr ())))
    pure $ InstanceCreateInfoAndroidKHR
             applicationVM applicationActivity

instance Storable InstanceCreateInfoAndroidKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero InstanceCreateInfoAndroidKHR where
  zero = InstanceCreateInfoAndroidKHR
           zero
           zero


type KHR_android_create_instance_SPEC_VERSION = 3

-- No documentation found for TopLevel "XR_KHR_android_create_instance_SPEC_VERSION"
pattern KHR_android_create_instance_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_android_create_instance_SPEC_VERSION = 3


type KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME = "XR_KHR_android_create_instance"

-- No documentation found for TopLevel "XR_KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME"
pattern KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_ANDROID_CREATE_INSTANCE_EXTENSION_NAME = "XR_KHR_android_create_instance"

