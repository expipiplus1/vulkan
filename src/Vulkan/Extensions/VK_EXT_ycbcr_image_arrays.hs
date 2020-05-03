{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_ycbcr_image_arrays  ( PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
                                                    , EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
                                                    , pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION
                                                    , EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
                                                    , pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME
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
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.BaseType (bool32ToBool)
import Vulkan.Core10.BaseType (boolToBool32)
import Vulkan.Core10.BaseType (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT))
-- | VkPhysicalDeviceYcbcrImageArraysFeaturesEXT - Structure describing
-- extended Y’CbCr image creation features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceYcbcrImageArraysFeaturesEXT' structure
-- describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceYcbcrImageArraysFeaturesEXT' structure is included
-- in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceYcbcrImageArraysFeaturesEXT' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceYcbcrImageArraysFeaturesEXT = PhysicalDeviceYcbcrImageArraysFeaturesEXT
  { -- | @ycbcrImageArrays@ indicates that the implementation supports creating
    -- images with a format that requires
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion Y′CBCR conversion>
    -- and has multiple array layers.
    ycbcrImageArrays :: Bool }
  deriving (Typeable)
deriving instance Show PhysicalDeviceYcbcrImageArraysFeaturesEXT

instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceYcbcrImageArraysFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (ycbcrImageArrays))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  peekCStruct p = do
    ycbcrImageArrays <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceYcbcrImageArraysFeaturesEXT
             (bool32ToBool ycbcrImageArrays)

instance Storable PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  zero = PhysicalDeviceYcbcrImageArraysFeaturesEXT
           zero


type EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION"
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_YCBCR_IMAGE_ARRAYS_SPEC_VERSION = 1


type EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = "VK_EXT_ycbcr_image_arrays"

-- No documentation found for TopLevel "VK_EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME"
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_YCBCR_IMAGE_ARRAYS_EXTENSION_NAME = "VK_EXT_ycbcr_image_arrays"

