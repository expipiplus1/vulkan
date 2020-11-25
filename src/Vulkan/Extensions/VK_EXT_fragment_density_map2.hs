{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_fragment_density_map2"
module Vulkan.Extensions.VK_EXT_fragment_density_map2  ( PhysicalDeviceFragmentDensityMap2FeaturesEXT(..)
                                                       , PhysicalDeviceFragmentDensityMap2PropertiesEXT(..)
                                                       , EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION
                                                       , pattern EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION
                                                       , EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME
                                                       , pattern EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME
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
import Data.Word (Word32)
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
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMap2FeaturesEXT"
data PhysicalDeviceFragmentDensityMap2FeaturesEXT = PhysicalDeviceFragmentDensityMap2FeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMap2FeaturesEXT" "fragmentDensityMapDeferred"
    fragmentDensityMapDeferred :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMap2FeaturesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMap2FeaturesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMap2FeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (fragmentDensityMapDeferred))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  peekCStruct p = do
    fragmentDensityMapDeferred <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceFragmentDensityMap2FeaturesEXT
             (bool32ToBool fragmentDensityMapDeferred)


instance Storable PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMap2FeaturesEXT where
  zero = PhysicalDeviceFragmentDensityMap2FeaturesEXT
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceFragmentDensityMap2PropertiesEXT"
data PhysicalDeviceFragmentDensityMap2PropertiesEXT = PhysicalDeviceFragmentDensityMap2PropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMap2PropertiesEXT" "subsampledLoads"
    subsampledLoads :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMap2PropertiesEXT" "subsampledCoarseReconstructionEarlyAccess"
    subsampledCoarseReconstructionEarlyAccess :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMap2PropertiesEXT" "maxSubsampledArrayLayers"
    maxSubsampledArrayLayers :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceFragmentDensityMap2PropertiesEXT" "maxDescriptorSetSubsampledSamplers"
    maxDescriptorSetSubsampledSamplers :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceFragmentDensityMap2PropertiesEXT)
#endif
deriving instance Show PhysicalDeviceFragmentDensityMap2PropertiesEXT

instance ToCStruct PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceFragmentDensityMap2PropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (subsampledLoads))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (subsampledCoarseReconstructionEarlyAccess))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxSubsampledArrayLayers)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (maxDescriptorSetSubsampledSamplers)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  peekCStruct p = do
    subsampledLoads <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    subsampledCoarseReconstructionEarlyAccess <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    maxSubsampledArrayLayers <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    maxDescriptorSetSubsampledSamplers <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pure $ PhysicalDeviceFragmentDensityMap2PropertiesEXT
             (bool32ToBool subsampledLoads) (bool32ToBool subsampledCoarseReconstructionEarlyAccess) maxSubsampledArrayLayers maxDescriptorSetSubsampledSamplers


instance Storable PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceFragmentDensityMap2PropertiesEXT where
  zero = PhysicalDeviceFragmentDensityMap2PropertiesEXT
           zero
           zero
           zero
           zero


type EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION"
pattern EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_FRAGMENT_DENSITY_MAP_2_SPEC_VERSION = 1


type EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME = "VK_EXT_fragment_density_map2"

-- No documentation found for TopLevel "VK_EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME"
pattern EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_FRAGMENT_DENSITY_MAP_2_EXTENSION_NAME = "VK_EXT_fragment_density_map2"

