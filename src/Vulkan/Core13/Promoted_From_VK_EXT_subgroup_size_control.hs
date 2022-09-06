{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_subgroup_size_control"
module Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control  ( PhysicalDeviceSubgroupSizeControlFeatures(..)
                                                                 , PhysicalDeviceSubgroupSizeControlProperties(..)
                                                                 , PipelineShaderStageRequiredSubgroupSizeCreateInfo(..)
                                                                 , StructureType(..)
                                                                 , PipelineShaderStageCreateFlagBits(..)
                                                                 , PipelineShaderStageCreateFlags
                                                                 ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO))
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlagBits(..))
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import Vulkan.Core10.Enums.StructureType (StructureType(..))
-- | VkPhysicalDeviceSubgroupSizeControlFeatures - Structure describing the
-- subgroup size control features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceSubgroupSizeControlFeatures' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSubgroupSizeControlFeatures' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- Note
--
-- The
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT'
-- structure was added in version 2 of the @VK_EXT_subgroup_size_control@
-- extension. Version 1 implementations of this extension will not fill out
-- the features structure but applications may assume that both
-- @subgroupSizeControl@ and @computeFullSubgroups@ are supported if the
-- extension is supported. (See also the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-requirements Feature Requirements>
-- section.) Applications are advised to add a
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT'
-- structure to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the features
-- regardless of the version of the extension supported by the
-- implementation. If the implementation only supports version 1, it will
-- safely ignore the
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT'
-- structure.
--
-- Vulkan 1.3 implementations always support the features structure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subgroup_size_control VK_EXT_subgroup_size_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubgroupSizeControlFeatures = PhysicalDeviceSubgroupSizeControlFeatures
  { -- | #extension-features-subgroupSizeControl# @subgroupSizeControl@ indicates
    -- whether the implementation supports controlling shader subgroup sizes
    -- via the
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT'
    -- flag and the 'PipelineShaderStageRequiredSubgroupSizeCreateInfo'
    -- structure.
    subgroupSizeControl :: Bool
  , -- | #extension-features-computeFullSubgroups# @computeFullSubgroups@
    -- indicates whether the implementation supports requiring full subgroups
    -- in compute , mesh, or task shaders via the
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT'
    -- flag.
    computeFullSubgroups :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupSizeControlFeatures)
#endif
deriving instance Show PhysicalDeviceSubgroupSizeControlFeatures

instance ToCStruct PhysicalDeviceSubgroupSizeControlFeatures where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubgroupSizeControlFeatures{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (subgroupSizeControl))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (computeFullSubgroups))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceSubgroupSizeControlFeatures where
  peekCStruct p = do
    subgroupSizeControl <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    computeFullSubgroups <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    pure $ PhysicalDeviceSubgroupSizeControlFeatures
             (bool32ToBool subgroupSizeControl) (bool32ToBool computeFullSubgroups)

instance Storable PhysicalDeviceSubgroupSizeControlFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubgroupSizeControlFeatures where
  zero = PhysicalDeviceSubgroupSizeControlFeatures
           zero
           zero


-- | VkPhysicalDeviceSubgroupSizeControlProperties - Structure describing the
-- control subgroup size properties of an implementation
--
-- = Description
--
-- If the 'PhysicalDeviceSubgroupSizeControlProperties' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- If
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties'::@supportedOperations@
-- includes
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-subgroup-quad >,
-- @minSubgroupSize@ /must/ be greater than or equal to 4.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subgroup_size_control VK_EXT_subgroup_size_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubgroupSizeControlProperties = PhysicalDeviceSubgroupSizeControlProperties
  { -- | #extension-limits-minSubgroupSize# @minSubgroupSize@ is the minimum
    -- subgroup size supported by this device. @minSubgroupSize@ is at least
    -- one if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @minSubgroupSize@
    -- is a power-of-two. @minSubgroupSize@ is less than or equal to
    -- @maxSubgroupSize@. @minSubgroupSize@ is less than or equal to
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    minSubgroupSize :: Word32
  , -- | #extension-limits-maxSubgroupSize# @maxSubgroupSize@ is the maximum
    -- subgroup size supported by this device. @maxSubgroupSize@ is at least
    -- one if any of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @maxSubgroupSize@
    -- is a power-of-two. @maxSubgroupSize@ is greater than or equal to
    -- @minSubgroupSize@. @maxSubgroupSize@ is greater than or equal to
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    maxSubgroupSize :: Word32
  , -- | #extension-limits-maxComputeWorkgroupSubgroups#
    -- @maxComputeWorkgroupSubgroups@ is the maximum number of subgroups
    -- supported by the implementation within a workgroup.
    maxComputeWorkgroupSubgroups :: Word32
  , -- | #extension-limits-requiredSubgroupSizeStages#
    -- @requiredSubgroupSizeStages@ is a bitfield of what shader stages support
    -- having a required subgroup size specified.
    requiredSubgroupSizeStages :: ShaderStageFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupSizeControlProperties)
#endif
deriving instance Show PhysicalDeviceSubgroupSizeControlProperties

instance ToCStruct PhysicalDeviceSubgroupSizeControlProperties where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceSubgroupSizeControlProperties{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (minSubgroupSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxSubgroupSize)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxComputeWorkgroupSubgroups)
    poke ((p `plusPtr` 28 :: Ptr ShaderStageFlags)) (requiredSubgroupSizeStages)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr ShaderStageFlags)) (zero)
    f

instance FromCStruct PhysicalDeviceSubgroupSizeControlProperties where
  peekCStruct p = do
    minSubgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxSubgroupSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxComputeWorkgroupSubgroups <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    requiredSubgroupSizeStages <- peek @ShaderStageFlags ((p `plusPtr` 28 :: Ptr ShaderStageFlags))
    pure $ PhysicalDeviceSubgroupSizeControlProperties
             minSubgroupSize maxSubgroupSize maxComputeWorkgroupSubgroups requiredSubgroupSizeStages

instance Storable PhysicalDeviceSubgroupSizeControlProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceSubgroupSizeControlProperties where
  zero = PhysicalDeviceSubgroupSizeControlProperties
           zero
           zero
           zero
           zero


-- | VkPipelineShaderStageRequiredSubgroupSizeCreateInfo - Structure
-- specifying the required subgroup size of a newly created pipeline shader
-- stage
--
-- = Description
--
-- If a 'PipelineShaderStageRequiredSubgroupSizeCreateInfo' structure is
-- included in the @pNext@ chain of
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo', it specifies
-- that the pipeline shader stage being compiled has a required subgroup
-- size.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subgroup_size_control VK_EXT_subgroup_size_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineShaderStageRequiredSubgroupSizeCreateInfo = PipelineShaderStageRequiredSubgroupSizeCreateInfo
  { -- | #pipelines-required-subgroup-size# @requiredSubgroupSize@ is an unsigned
    -- integer value specifying the required subgroup size for the newly
    -- created pipeline shader stage.
    --
    -- #VUID-VkPipelineShaderStageRequiredSubgroupSizeCreateInfo-requiredSubgroupSize-02760#
    -- @requiredSubgroupSize@ /must/ be a power-of-two integer
    --
    -- #VUID-VkPipelineShaderStageRequiredSubgroupSizeCreateInfo-requiredSubgroupSize-02761#
    -- @requiredSubgroupSize@ /must/ be greater or equal to
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-minSubgroupSize minSubgroupSize>
    --
    -- #VUID-VkPipelineShaderStageRequiredSubgroupSizeCreateInfo-requiredSubgroupSize-02762#
    -- @requiredSubgroupSize@ /must/ be less than or equal to
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxSubgroupSize maxSubgroupSize>
    requiredSubgroupSize :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageRequiredSubgroupSizeCreateInfo)
#endif
deriving instance Show PipelineShaderStageRequiredSubgroupSizeCreateInfo

instance ToCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfo where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineShaderStageRequiredSubgroupSizeCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (requiredSubgroupSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfo where
  peekCStruct p = do
    requiredSubgroupSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pure $ PipelineShaderStageRequiredSubgroupSizeCreateInfo
             requiredSubgroupSize

instance Storable PipelineShaderStageRequiredSubgroupSizeCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineShaderStageRequiredSubgroupSizeCreateInfo where
  zero = PipelineShaderStageRequiredSubgroupSizeCreateInfo
           zero

