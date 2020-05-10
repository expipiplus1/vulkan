{-# language CPP #-}
module Vulkan.Extensions.VK_NV_corner_sampled_image  ( PhysicalDeviceCornerSampledImageFeaturesNV(..)
                                                     , NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
                                                     , pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION
                                                     , NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
                                                     , pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV))
-- | VkPhysicalDeviceCornerSampledImageFeaturesNV - Structure describing
-- corner sampled image features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceCornerSampledImageFeaturesNV'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceCornerSampledImageFeaturesNV' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether each feature is supported.
-- 'PhysicalDeviceCornerSampledImageFeaturesNV' /can/ also be included in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable
-- features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.BaseType.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceCornerSampledImageFeaturesNV = PhysicalDeviceCornerSampledImageFeaturesNV
  { -- | @cornerSampledImage@ specifies whether images can be created with a
    -- 'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
    -- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'.
    -- See
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-images-corner-sampled Corner-Sampled Images>.
    cornerSampledImage :: Bool }
  deriving (Typeable, Eq)
deriving instance Show PhysicalDeviceCornerSampledImageFeaturesNV

instance ToCStruct PhysicalDeviceCornerSampledImageFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceCornerSampledImageFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (cornerSampledImage))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceCornerSampledImageFeaturesNV where
  peekCStruct p = do
    cornerSampledImage <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceCornerSampledImageFeaturesNV
             (bool32ToBool cornerSampledImage)

instance Storable PhysicalDeviceCornerSampledImageFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceCornerSampledImageFeaturesNV where
  zero = PhysicalDeviceCornerSampledImageFeaturesNV
           zero


type NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION"
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_CORNER_SAMPLED_IMAGE_SPEC_VERSION = 2


type NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = "VK_NV_corner_sampled_image"

-- No documentation found for TopLevel "VK_NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME"
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_CORNER_SAMPLED_IMAGE_EXTENSION_NAME = "VK_NV_corner_sampled_image"

