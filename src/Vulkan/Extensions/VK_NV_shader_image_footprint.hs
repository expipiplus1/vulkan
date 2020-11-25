{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_shader_image_footprint"
module Vulkan.Extensions.VK_NV_shader_image_footprint  ( PhysicalDeviceShaderImageFootprintFeaturesNV(..)
                                                       , NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
                                                       , pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION
                                                       , NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
                                                       , pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV))

-- No documentation found for TopLevel "VkPhysicalDeviceShaderImageFootprintFeaturesNV"
data PhysicalDeviceShaderImageFootprintFeaturesNV = PhysicalDeviceShaderImageFootprintFeaturesNV
  { -- No documentation found for Nested "VkPhysicalDeviceShaderImageFootprintFeaturesNV" "imageFootprint"
    imageFootprint :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceShaderImageFootprintFeaturesNV)
#endif
deriving instance Show PhysicalDeviceShaderImageFootprintFeaturesNV

instance ToCStruct PhysicalDeviceShaderImageFootprintFeaturesNV where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceShaderImageFootprintFeaturesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (imageFootprint))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceShaderImageFootprintFeaturesNV where
  peekCStruct p = do
    imageFootprint <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceShaderImageFootprintFeaturesNV
             (bool32ToBool imageFootprint)


instance Storable PhysicalDeviceShaderImageFootprintFeaturesNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceShaderImageFootprintFeaturesNV where
  zero = PhysicalDeviceShaderImageFootprintFeaturesNV
           zero


type NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION"
pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADER_IMAGE_FOOTPRINT_SPEC_VERSION = 2


type NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = "VK_NV_shader_image_footprint"

-- No documentation found for TopLevel "VK_NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME"
pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADER_IMAGE_FOOTPRINT_EXTENSION_NAME = "VK_NV_shader_image_footprint"

