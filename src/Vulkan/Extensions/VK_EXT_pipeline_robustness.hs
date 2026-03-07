{-# language CPP #-}
-- | = Name
--
-- VK_EXT_pipeline_robustness - device extension
--
-- = VK_EXT_pipeline_robustness
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
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.4-promotions Vulkan 1.4>
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
--     -   Faith Ekstrand, Intel
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-robustBufferAccess robustBufferAccess>
-- and other robustness features may have an adverse effect on performance,
-- this extension is designed to allow users to request robust behavior
-- only where it may be needed.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core10.GraphicsPipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.ComputePipeline.ComputePipelineCreateInfo',
--     'Vulkan.Core10.ComputePipeline.PipelineShaderStageCreateInfo',
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
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior.PipelineRobustnessBufferBehavior':
--
--     -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT'
--
--     -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT'
--
--     -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT'
--
--     -   'PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT'
--
-- -   Extending
--     'Vulkan.Core14.Enums.PipelineRobustnessImageBehavior.PipelineRobustnessImageBehavior':
--
--     -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT'
--
--     -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT'
--
--     -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT'
--
--     -   'PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT'
--
--     -   'STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT'
--
-- == Promotion to Vulkan 1.4
--
-- Functionality in this extension is included in core Vulkan 1.4 with the
-- EXT suffix omitted. The original type, enum, and command names are still
-- available as aliases of the core functionality.
--
-- == Version History
--
-- -   Revision 1, 2022-07-12 (Jarred Davies)
--
--     -   Initial version
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_pipeline_robustness Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_pipeline_robustness  ( pattern STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT
                                                     , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT
                                                     , pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT
                                                     , PipelineRobustnessBufferBehaviorEXT
                                                     , PipelineRobustnessImageBehaviorEXT
                                                     , PhysicalDevicePipelineRobustnessFeaturesEXT
                                                     , PipelineRobustnessCreateInfoEXT
                                                     , PhysicalDevicePipelineRobustnessPropertiesEXT
                                                     , EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION
                                                     , pattern EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION
                                                     , EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME
                                                     , pattern EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)
import Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality' (PhysicalDevicePipelineRobustnessFeatures)
import Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality' (PhysicalDevicePipelineRobustnessProperties)
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior)
import Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality' (PipelineRobustnessCreateInfo)
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior)
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior(PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT))
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior(PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED))
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior(PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS))
import Vulkan.Core14.Enums.PipelineRobustnessBufferBehavior (PipelineRobustnessBufferBehavior(PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2))
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior(PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT))
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior(PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED))
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior(PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS))
import Vulkan.Core14.Enums.PipelineRobustnessImageBehavior (PipelineRobustnessImageBehavior(PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO_EXT = STRUCTURE_TYPE_PIPELINE_ROBUSTNESS_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_ROBUSTNESS_PROPERTIES


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT_EXT = PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DEVICE_DEFAULT


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED_EXT = PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_DISABLED


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_EXT = PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT"
pattern PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2_EXT = PIPELINE_ROBUSTNESS_BUFFER_BEHAVIOR_ROBUST_BUFFER_ACCESS_2


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT_EXT = PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DEVICE_DEFAULT


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED_EXT = PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_DISABLED


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_EXT = PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS


-- No documentation found for TopLevel "VK_PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT"
pattern PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2_EXT = PIPELINE_ROBUSTNESS_IMAGE_BEHAVIOR_ROBUST_IMAGE_ACCESS_2


-- No documentation found for TopLevel "VkPipelineRobustnessBufferBehaviorEXT"
type PipelineRobustnessBufferBehaviorEXT = PipelineRobustnessBufferBehavior


-- No documentation found for TopLevel "VkPipelineRobustnessImageBehaviorEXT"
type PipelineRobustnessImageBehaviorEXT = PipelineRobustnessImageBehavior


-- No documentation found for TopLevel "VkPhysicalDevicePipelineRobustnessFeaturesEXT"
type PhysicalDevicePipelineRobustnessFeaturesEXT = PhysicalDevicePipelineRobustnessFeatures


-- No documentation found for TopLevel "VkPipelineRobustnessCreateInfoEXT"
type PipelineRobustnessCreateInfoEXT = PipelineRobustnessCreateInfo


-- No documentation found for TopLevel "VkPhysicalDevicePipelineRobustnessPropertiesEXT"
type PhysicalDevicePipelineRobustnessPropertiesEXT = PhysicalDevicePipelineRobustnessProperties


type EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION"
pattern EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PIPELINE_ROBUSTNESS_SPEC_VERSION = 1


type EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_pipeline_robustness"

-- No documentation found for TopLevel "VK_EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME"
pattern EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PIPELINE_ROBUSTNESS_EXTENSION_NAME = "VK_EXT_pipeline_robustness"

