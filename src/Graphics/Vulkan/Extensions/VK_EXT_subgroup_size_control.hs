{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control  ( PhysicalDeviceSubgroupSizeControlFeaturesEXT(..)
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
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Kind (Type)
import Graphics.Vulkan.Core10.BaseType (bool32ToBool)
import Graphics.Vulkan.Core10.BaseType (boolToBool32)
import Graphics.Vulkan.Core10.BaseType (Bool32)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT))
-- | VkPhysicalDeviceSubgroupSizeControlFeaturesEXT - Structure describing
-- the subgroup size control features that can be supported by an
-- implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceSubgroupSizeControlFeaturesEXT'
-- structure describe the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceSubgroupSizeControlFeaturesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- it is filled with values indicating whether the feature is supported.
-- 'PhysicalDeviceSubgroupSizeControlFeaturesEXT' /can/ also be included in
-- the @pNext@ chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to
-- enable the feature.
--
-- Note
--
-- The 'PhysicalDeviceSubgroupSizeControlFeaturesEXT' structure was added
-- in version 2 of the @VK_EXT_subgroup_size_control@ extension. Version 1
-- implementations of this extension will not fill out the features
-- structure but applications may assume that both @subgroupSizeControl@
-- and @computeFullSubgroups@ are supported if the extension is supported.
-- (See also the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-requirements Feature Requirements>
-- section.) Applications are advised to add a
-- 'PhysicalDeviceSubgroupSizeControlFeaturesEXT' structure to the @pNext@
-- chain of 'Graphics.Vulkan.Core10.Device.DeviceCreateInfo' to enable the
-- features regardless of the version of the extension supported by the
-- implementation. If the implementation only supports version 1, it will
-- safely ignore the 'PhysicalDeviceSubgroupSizeControlFeaturesEXT'
-- structure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.BaseType.Bool32',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubgroupSizeControlFeaturesEXT = PhysicalDeviceSubgroupSizeControlFeaturesEXT
  { -- | @subgroupSizeControl@ indicates whether the implementation supports
    -- controlling shader subgroup sizes via the
    -- 'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
    -- flag and the 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
    -- structure.
    subgroupSizeControl :: Bool
  , -- | @computeFullSubgroups@ indicates whether the implementation supports
    -- requiring full subgroups in compute shaders via the
    -- 'Graphics.Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
    -- flag.
    computeFullSubgroups :: Bool
  }
  deriving (Typeable)
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


-- | VkPhysicalDeviceSubgroupSizeControlPropertiesEXT - Structure describing
-- the control subgroup size properties of an implementation
--
-- = Members
--
-- The members of the 'PhysicalDeviceSubgroupSizeControlPropertiesEXT'
-- structure describe the following properties:
--
-- = Description
--
-- If the 'PhysicalDeviceSubgroupSizeControlPropertiesEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- If
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties'::@supportedOperations@
-- includes
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroup-quad >,
-- @minSubgroupSize@ /must/ be greater than or equal to 4.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubgroupSizeControlPropertiesEXT = PhysicalDeviceSubgroupSizeControlPropertiesEXT
  { -- | @minSubgroupSize@ is the minimum subgroup size supported by this device.
    -- @minSubgroupSize@ is at least one if any of the physical device’s queues
    -- support 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT'
    -- or 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    -- @minSubgroupSize@ is a power-of-two. @minSubgroupSize@ is less than or
    -- equal to @maxSubgroupSize@. @minSubgroupSize@ is less than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    minSubgroupSize :: Word32
  , -- | @maxSubgroupSize@ is the maximum subgroup size supported by this device.
    -- @maxSubgroupSize@ is at least one if any of the physical device’s queues
    -- support 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT'
    -- or 'Graphics.Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'.
    -- @maxSubgroupSize@ is a power-of-two. @maxSubgroupSize@ is greater than
    -- or equal to @minSubgroupSize@. @maxSubgroupSize@ is greater than or
    -- equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    maxSubgroupSize :: Word32
  , -- | @maxComputeWorkgroupSubgroups@ is the maximum number of subgroups
    -- supported by the implementation within a workgroup.
    maxComputeWorkgroupSubgroups :: Word32
  , -- | @requiredSubgroupSizeStages@ is a bitfield of what shader stages support
    -- having a required subgroup size specified.
    requiredSubgroupSizeStages :: ShaderStageFlags
  }
  deriving (Typeable)
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


-- | VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT - Structure
-- specifying the required subgroup size of a newly created pipeline shader
-- stage
--
-- == Valid Usage
--
-- = Description
--
-- If a 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT' structure is
-- included in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo', it
-- specifies that the pipeline shader stage being compiled has a required
-- subgroup size.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
  { -- | @requiredSubgroupSize@ /must/ be less than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size maxSubgroupSize>
    requiredSubgroupSize :: Word32 }
  deriving (Typeable)
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

