{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_corner_sampled_image"
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
import GHC.Generics (Generic)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV))

-- No documentation found for TopLevel "VkPhysicalDeviceCornerSampledImageFeaturesNV"
data PhysicalDeviceCornerSampledImageFeaturesNV = PhysicalDeviceCornerSampledImageFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceCornerSampledImageFeaturesNV" "cornerSampledImage"
    cornerSampledImage :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceCornerSampledImageFeaturesNV)
#endif
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

