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
--     -   Requires support for Vulkan 1.1
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3-promotions Vulkan 1.3>
--
-- [__Contact__]
--
--     -   Neil Henning
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_subgroup_size_control] @sheredom%0A*Here describe the issue or question you have about the VK_EXT_subgroup_size_control extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-05
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.3 Core
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-minSubgroupSize minimum>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxSubgroupSize maximum>
-- subgroup sizes.
--
-- An implementation is also optionally allowed to support specifying a
-- required subgroup size for a given pipeline stage. Implementations
-- advertise which
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-requiredSubgroupSizeStages stages support a required subgroup size>,
-- and any pipeline of a supported stage can be passed a
-- 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT' structure to set
-- the subgroup size for that shader stage of the pipeline. For compute
-- shaders, this requires the developer to query the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#limits-maxComputeWorkgroupSubgroups maxComputeWorkgroupSubgroups>
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
--     -   'PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT'
--
--     -   'PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT'
--
-- == Promotion to Vulkan 1.3
--
-- Functionality in this extension is included in core Vulkan 1.3, with the
-- EXT suffix omitted. The original type, enum and command names are still
-- available as aliases of the core functionality.
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
-- == See Also
--
-- 'PhysicalDeviceSubgroupSizeControlFeaturesEXT',
-- 'PhysicalDeviceSubgroupSizeControlPropertiesEXT',
-- 'PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_subgroup_size_control Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_subgroup_size_control  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT
                                                       , pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT
                                                       , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT
                                                       , pattern PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT
                                                       , pattern PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT
                                                       , PhysicalDeviceSubgroupSizeControlFeaturesEXT
                                                       , PhysicalDeviceSubgroupSizeControlPropertiesEXT
                                                       , PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
                                                       , EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION
                                                       , pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION
                                                       , EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME
                                                       , pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)
import Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlFeatures)
import Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PhysicalDeviceSubgroupSizeControlProperties)
import Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control (PipelineShaderStageRequiredSubgroupSizeCreateInfo)
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlagBits(PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT))
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlags)
import Vulkan.Core10.Enums.PipelineShaderStageCreateFlagBits (PipelineShaderStageCreateFlagBits(PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES


-- No documentation found for TopLevel "VK_PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT"
pattern PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT_EXT = PIPELINE_SHADER_STAGE_CREATE_ALLOW_VARYING_SUBGROUP_SIZE_BIT


-- No documentation found for TopLevel "VK_PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT"
pattern PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT_EXT = PIPELINE_SHADER_STAGE_CREATE_REQUIRE_FULL_SUBGROUPS_BIT


-- No documentation found for TopLevel "VkPhysicalDeviceSubgroupSizeControlFeaturesEXT"
type PhysicalDeviceSubgroupSizeControlFeaturesEXT = PhysicalDeviceSubgroupSizeControlFeatures


-- No documentation found for TopLevel "VkPhysicalDeviceSubgroupSizeControlPropertiesEXT"
type PhysicalDeviceSubgroupSizeControlPropertiesEXT = PhysicalDeviceSubgroupSizeControlProperties


-- No documentation found for TopLevel "VkPipelineShaderStageRequiredSubgroupSizeCreateInfoEXT"
type PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT = PipelineShaderStageRequiredSubgroupSizeCreateInfo


type EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION"
pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SUBGROUP_SIZE_CONTROL_SPEC_VERSION = 2


type EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME = "VK_EXT_subgroup_size_control"

-- No documentation found for TopLevel "VK_EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME"
pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SUBGROUP_SIZE_CONTROL_EXTENSION_NAME = "VK_EXT_subgroup_size_control"

