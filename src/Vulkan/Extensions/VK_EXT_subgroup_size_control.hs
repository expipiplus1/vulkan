{-# language CPP #-}
-- | = Name
--
-- VK_EXT_subgroup_size_control - device extension
--
-- == VK_EXT_subgroup_size_control
--
-- [__Name String__]
--     @VK_EXT_subgroup_size_control@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     226
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Neil Henning
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_EXT_subgroup_size_control:%20&body=@sheredom%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-05
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Jason Ekstrand, Intel
--
--     -   Sławek Grajewski, Intel
--
--     -   Jesse Hall, Google
--
--     -   Neil Henning, AMD
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Leger, Qualcomm
--
--     -   Graeme Leese, Broadcom
--
--     -   Allan MacKinnon, Google
--
--     -   Mariusz Merecki, Intel
--
--     -   Graham Wihlidal, Electronic Arts
--
-- == Description
--
-- This extension enables an implementation to control the subgroup size by
-- allowing a varying subgroup size and also specifying a required subgroup
-- size.
--
-- It extends the subgroup support in Vulkan 1.1 to allow an implementation
-- to expose a varying subgroup size. Previously Vulkan exposed a single
-- subgroup size per physical device, with the expectation that
-- implementations will behave as if all subgroups have the same size. Some
-- implementations /may/ dispatch shaders with a varying subgroup size for
-- different subgroups. As a result they could implicitly split a large
-- subgroup into smaller subgroups or represent a small subgroup as a
-- larger subgroup, some of whose invocations were inactive on launch.
--
-- To aid developers in understanding the performance characteristics of
-- their programs, this extension exposes a minimum and maximum subgroup
-- size that a physical device supports and a pipeline create flag to
-- enable that pipeline to vary its subgroup size. If enabled, any
-- @SubgroupSize@ decorated variables in the SPIR-V shader modules provided
-- to pipeline creation /may/ vary between the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-min-subgroup-size minimum>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size maximum>
-- subgroup sizes.
--
-- An implementation is also optionally allowed to support specifying a
-- required subgroup size for a given pipeline stage. Implementations
-- advertise which
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-required-subgroup-size-stages stages support a required subgroup size>,
-- and any pipeline of a supported stage can be passed a
-- 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT' structure to set
-- the subgroup size for that shader stage of the pipeline. For compute
-- shaders, this requires the developer to query the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroups-per-workgroup maxComputeWorkgroupSubgroups>
-- and ensure that:
--
-- \[s = { WorkGroupSize.x \times WorkGroupSize.y \times WorkgroupSize.z \leq SubgroupSize \times maxComputeWorkgroupSubgroups }\]
--
-- Developers can also specify a new pipeline shader stage create flag that
-- requires the implementation to have fully populated subgroups within
-- local workgroups. This requires the workgroup size in the X dimension to
-- be a multiple of the subgroup size.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceSubgroupSizeControlFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceSubgroupSizeControlPropertiesEXT'
--
-- -   Extending 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo':
--
--     -   'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--
-- == New Enum Constants
--
-- -   'EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME'
--
-- -   'EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PipelineShaderStageCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-03-05 (Neil Henning)
--
--     -   Initial draft
--
-- -   Revision 2, 2019-07-26 (Jason Ekstrand)
--
--     -   Add the missing 'PhysicalDeviceSubgroupSizeControlFeaturesEXT'
--         for querying subgroup size control features.
--
-- = See Also
--
-- 'PhysicalDeviceSubgroupSizeControlFeaturesEXT',
-- 'PhysicalDeviceSubgroupSizeControlPropertiesEXT',
-- 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_subgroup_size_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_subgroup_size_control  ( PhysicalDeviceSubgroupSizeControlFeaturesEXT(..)
                                                       , PhysicalDeviceSubgroupSizeControlPropertiesEXT(..)
                                                       , PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT(..)
                                                       , EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION
                                                       , pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION
                                                       , EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME
                                                       , pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME
                                                       ) where

import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT))
-- | VkPhysicalDeviceSubgroupSizeControlFeaturesEXT - Structure describing
-- the subgroup size control features that can be supported by an
-- implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- If the 'PhysicalDeviceSubgroupSizeControlFeaturesEXT' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceSubgroupSizeControlFeaturesEXT' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
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
-- chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to enable the features
-- regardless of the version of the extension supported by the
-- implementation. If the implementation only supports version 1, it will
-- safely ignore the 'PhysicalDeviceSubgroupSizeControlFeaturesEXT'
-- structure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubgroupSizeControlFeaturesEXT = PhysicalDeviceSubgroupSizeControlFeaturesEXT
  { -- | #features-subgroupSizeControl# @subgroupSizeControl@ indicates whether
    -- the implementation supports controlling shader subgroup sizes via the
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
    -- flag and the 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
    -- structure.
    subgroupSizeControl :: Bool
  , -- | #features-computeFullSubgroups# @computeFullSubgroups@ indicates whether
    -- the implementation supports requiring full subgroups in compute shaders
    -- via the
    -- 'Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits.PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
    -- flag.
    computeFullSubgroups :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupSizeControlFeaturesEXT)
#endif
deriving instance Show PhysicalDeviceSubgroupSizeControlFeaturesEXT

instance ToCStruct PhysicalDeviceSubgroupSizeControlFeaturesEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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
-- = Description
--
-- If the 'PhysicalDeviceSubgroupSizeControlPropertiesEXT' structure is
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-subgroup-quad >,
-- @minSubgroupSize@ /must/ be greater than or equal to 4.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceSubgroupSizeControlPropertiesEXT = PhysicalDeviceSubgroupSizeControlPropertiesEXT
  { -- | #limits-min-subgroup-size# @minSubgroupSize@ is the minimum subgroup
    -- size supported by this device. @minSubgroupSize@ is at least one if any
    -- of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @minSubgroupSize@
    -- is a power-of-two. @minSubgroupSize@ is less than or equal to
    -- @maxSubgroupSize@. @minSubgroupSize@ is less than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    minSubgroupSize :: Word32
  , -- | #limits-max-subgroup-size# @maxSubgroupSize@ is the maximum subgroup
    -- size supported by this device. @maxSubgroupSize@ is at least one if any
    -- of the physical device’s queues support
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_GRAPHICS_BIT' or
    -- 'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_COMPUTE_BIT'. @maxSubgroupSize@
    -- is a power-of-two. @maxSubgroupSize@ is greater than or equal to
    -- @minSubgroupSize@. @maxSubgroupSize@ is greater than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subgroup-size subgroupSize>.
    maxSubgroupSize :: Word32
  , -- | #limits-max-subgroups-per-workgroup# @maxComputeWorkgroupSubgroups@ is
    -- the maximum number of subgroups supported by the implementation within a
    -- workgroup.
    maxComputeWorkgroupSubgroups :: Word32
  , -- | #limits-required-subgroup-size-stages# @requiredSubgroupSizeStages@ is a
    -- bitfield of what shader stages support having a required subgroup size
    -- specified.
    requiredSubgroupSizeStages :: ShaderStageFlags
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceSubgroupSizeControlPropertiesEXT)
#endif
deriving instance Show PhysicalDeviceSubgroupSizeControlPropertiesEXT

instance ToCStruct PhysicalDeviceSubgroupSizeControlPropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
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
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo', it specifies
-- that the pipeline shader stage being compiled has a required subgroup
-- size.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
  { -- | #VUID-VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT-requiredSubgroupSize-02760#
    -- @requiredSubgroupSize@ /must/ be a power-of-two integer
    --
    -- #VUID-VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT-requiredSubgroupSize-02761#
    -- @requiredSubgroupSize@ /must/ be greater or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-min-subgroup-size minSubgroupSize>
    --
    -- #VUID-VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT-requiredSubgroupSize-02762#
    -- @requiredSubgroupSize@ /must/ be less than or equal to
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-max-subgroup-size maxSubgroupSize>
    requiredSubgroupSize :: Word32 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT)
#endif
deriving instance Show PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT

instance ToCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
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

