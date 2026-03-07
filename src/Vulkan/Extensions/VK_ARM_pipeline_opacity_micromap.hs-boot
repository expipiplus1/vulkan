{-# language CPP #-}
-- | = Name
--
-- VK_ARM_pipeline_opacity_micromap - device extension
--
-- = VK_ARM_pipeline_opacity_micromap
--
-- [__Name String__]
--     @VK_ARM_pipeline_opacity_micromap@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     597
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
--
-- [__Contact__]
--
--     -   Mathieu Robart
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_pipeline_opacity_micromap] @mathieurobart-arm%0A*Here describe the issue or question you have about the VK_ARM_pipeline_opacity_micromap extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_ARM_pipeline_opacity_micromap.adoc VK_ARM_pipeline_opacity_micromap>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-01-07
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mathieu Robart, Arm
--
--     -   Marius Bjorge, Arm
--
--     -   Yaozhong Zhang, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- The Opacity Micromap extension @VK_EXT_opacity_micromap@ supports the
-- new pipeline creation flag
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT',
-- indicating that the ray tracing pipeline may be used with acceleration
-- structures referencing micromaps. This allows for possible
-- optimizations, knowing beforehand that opacity micromaps may be used
-- with the pipeline.
--
-- An equivalent flag does not exist for pipelines supporting Ray Query
-- with opacity micromaps, such as graphics and compute. Consequently, it
-- is currently not possible to optimize such pipelines for no-opacity,
-- e.g. when opacity micromaps are supported by an application but not used
-- by the pipeline. This may lead to performance degradation.
--
-- This extension adds a new flag,
-- 'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM',
-- indicating that a pipeline will NOT be used with an acceleration
-- structure referencing an opacity micromap, therefore allowing possible
-- pipeline optimizations.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineOpacityMicromapFeaturesARM'
--
-- == New Enum Constants
--
-- -   'ARM_PIPELINE_OPACITY_MICROMAP_EXTENSION_NAME'
--
-- -   'ARM_PIPELINE_OPACITY_MICROMAP_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineCreateFlags2.PipelineCreateFlagBits2':
--
--     -   'Vulkan.Core14.Enums.PipelineCreateFlags2.PIPELINE_CREATE_2_DISALLOW_OPACITY_MICROMAP_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_OPACITY_MICROMAP_FEATURES_ARM'
--
-- == Issues
--
-- None.
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-01-07 (Mathieu Robart)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_pipeline_opacity_micromap Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_pipeline_opacity_micromap  (PhysicalDevicePipelineOpacityMicromapFeaturesARM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineOpacityMicromapFeaturesARM

instance ToCStruct PhysicalDevicePipelineOpacityMicromapFeaturesARM
instance Show PhysicalDevicePipelineOpacityMicromapFeaturesARM

instance FromCStruct PhysicalDevicePipelineOpacityMicromapFeaturesARM

