{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_subgroup_size_control"
module Vulkan.Extensions.VK_EXT_subgroup_size_control  ( PhysicalDeviceSubgroupSizeControlFeaturesEXT(..)
                                                       , PhysicalDeviceSubgroupSizeControlPropertiesEXT(..)
                                                       , PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT(..)
                                                       , EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION
                                                       , pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION
                                                       , EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME
                                                       , pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME
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
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT))

-- No documentation found for TopLevel "VkPhysicalDeviceSubgroupSizeControlFeaturesEXT"
data PhysicalDeviceSubgroupSizeControlFeaturesEXT = PhysicalDeviceSubgroupSizeControlFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceSubgroupSizeControlFeaturesEXT" "subgroupSizeControl"
    subgroupSizeControl :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupSizeControlFeaturesEXT" "computeFullSubgroups"
    computeFullSubgroups :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupSizeControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceSubgroupSizeControlFeaturesEXT

instance ToCStruct PhysicalDeviceSubgroupSizeControlFeaturesEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubgroupSizeControlFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (subgroupSizeControl))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (computeFullSubgroups))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSubgroupSizeControlFeaturesEXT where
  peekCStruct p = do
    subgroupSizeControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    computeFullSubgroups <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceSubgroupSizeControlFeaturesEXT
             (bool32ToBool subgroupSizeControl) (bool32ToBool computeFullSubgroups)


instance Storable PhysicalDeviceSubgroupSizeControlFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubgroupSizeControlFeaturesEXT where
  zero = PhysicalDeviceSubgroupSizeControlFeaturesEXT
           zero
           zero



-- No documentation found for TopLevel "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT"
data PhysicalDeviceSubgroupSizeControlPropertiesEXT = PhysicalDeviceSubgroupSizeControlPropertiesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT" "minSubgroupSize"
    minSubgroupSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT" "maxSubgroupSize"
    maxSubgroupSize :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT" "maxComputeWorkgroupSubgroups"
    maxComputeWorkgroupSubgroups :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT" "requiredSubgroupSizeStages"
    requiredSubgroupSizeStages :: ShaderStageFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupSizeControlPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceSubgroupSizeControlPropertiesEXT

instance ToCStruct PhysicalDeviceSubgroupSizeControlPropertiesEXT where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubgroupSizeControlPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minSubgroupSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxSubgroupSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxComputeWorkgroupSubgroups)
    poke ((p `plusPtr` 28 :: Ptr ShaderStageFlags)) (requiredSubgroupSizeStages)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceSubgroupSizeControlPropertiesEXT where
  peekCStruct p = do
    minSubgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxSubgroupSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxComputeWorkgroupSubgroups <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    requiredSubgroupSizeStages <- peek @ShaderStageFlags ((p `plusPtr` 28 :: Ptr ShaderStageFlags))
    pure $ PhysicalDeviceSubgroupSizeControlPropertiesEXT
             minSubgroupSize maxSubgroupSize maxComputeWorkgroupSubgroups requiredSubgroupSizeStages


instance Storable PhysicalDeviceSubgroupSizeControlPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubgroupSizeControlPropertiesEXT where
  zero = PhysicalDeviceSubgroupSizeControlPropertiesEXT
           zero
           zero
           zero
           zero



-- No documentation found for TopLevel "VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT"
data PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
  { -- No documentation found for Nested "VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT" "requiredSubgroupSize"
    requiredSubgroupSize :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT)
#endif
deriving instance Show PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT

instance ToCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (requiredSubgroupSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT where
  peekCStruct p = do
    requiredSubgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
             requiredSubgroupSize


instance Storable PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT where
  zero = PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
           zero


type EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION"
pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION = 2


type EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME = "VK_EXT_subgroup_size_control"

-- No documentation found for TopLevel "VK_EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME"
pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME = "VK_EXT_subgroup_size_control"

