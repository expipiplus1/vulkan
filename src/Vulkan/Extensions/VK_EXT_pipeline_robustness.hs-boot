{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_robustness - device extension
--
-- == VK_EXT_pipeline_robustness
--
-- [__Name String__]
--     @VK_EXT_pipeline_robustness@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     69
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Jarred Davies
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-07-12
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_EXT_robustness2@
--
--     -   Interacts with @VK_EXT_image_robustness@
--
--     -   Interacts with @VK_KHR_ray_tracing_pipeline@
--
-- [__Contributors__]
--
--     -   Jarred Davies, Imagination Technologies
--
--     -   Alex Walters, Imagination Technologies
--
--     -   Piers Daniell, NVIDIA
--
--     -   Graeme Leese, Broadcom Corporation
--
--     -   Jeff Leger, Qualcomm Technologies, Inc.
--
--     -   Jason Ekstrand, Intel
--
--     -   Lionel Landwerlin, Intel
--
--     -   Shahbaz Youssefi, Google, Inc.
--
-- == Description
--
-- This extension allows users to request robustness on a per-pipeline
-- stage basis.
--
-- As
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- and other robustness features may have an adverse effect on performance,
-- this extension is designed to allow users to request robust behavior
-- only where it may be needed.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR':
--
--     -   'PipelineRobustnessCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineRobustnessFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePipelineRobustnessPropertiesEXT'
--
-- == New Enums
--
-- -   'PipelineRobustnessBufferBehaviorEXT'
--
-- -   'PipelineRobustnessImageBehaviorEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME'
--
-- -   'EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2022-07-12 (Jarred Davies)
--
--     -   Initial version
--
-- == See Also
--
-- 'PhysicalDevicePipelineRobustnessFeaturesEXT',
-- 'PhysicalDevicePipelineRobustnessPropertiesEXT',
-- 'PipelineRobustnessBufferBehaviorEXT',
-- 'PipelineRobustnessCreateInfoEXT', 'PipelineRobustnessImageBehaviorEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_pipeline_robustness Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_robustness  ( PhysicalDevicePipelineRobustnessFeaturesEXT
                                                     , PhysicalDevicePipelineRobustnessPropertiesEXT
                                                     , PipelineRobustnessCreateInfoEXT
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineRobustnessFeaturesEXT

instance ToCStruct PhysicalDevicePipelineRobustnessFeaturesEXT
instance Show PhysicalDevicePipelineRobustnessFeaturesEXT

instance FromCStruct PhysicalDevicePipelineRobustnessFeaturesEXT


data PhysicalDevicePipelineRobustnessPropertiesEXT

instance ToCStruct PhysicalDevicePipelineRobustnessPropertiesEXT
instance Show PhysicalDevicePipelineRobustnessPropertiesEXT

instance FromCStruct PhysicalDevicePipelineRobustnessPropertiesEXT


data PipelineRobustnessCreateInfoEXT

instance ToCStruct PipelineRobustnessCreateInfoEXT
instance Show PipelineRobustnessCreateInfoEXT

instance FromCStruct PipelineRobustnessCreateInfoEXT

