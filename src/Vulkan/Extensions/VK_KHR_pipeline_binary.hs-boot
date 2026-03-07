{-# language CPP #-}
-- | = Name
--
-- VK_KHR_pipeline_binary - device extension
--
-- = VK_KHR_pipeline_binary
--
-- [__Name String__]
--     @VK_KHR_pipeline_binary@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     484
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_pipeline_binary] @stu-s%0A*Here describe the issue or question you have about the VK_KHR_pipeline_binary extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_pipeline_binary.adoc VK_KHR_pipeline_binary>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-07-01
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Alan Harrison, AMD
--
--     -   Maciej Jesionowski, AMD
--
--     -   Younggwan Kim, Arm
--
--     -   Jan-Harald Fredriksen, Arm
--
--     -   Ting Wei, Arm
--
--     -   Chris Glover, Google
--
--     -   Shahbaz Youssefi, Google
--
--     -   Jakub Kuderski, Google
--
--     -   Piotr Byszewski, Mobica
--
--     -   Piers Daniell, NVIDIA
--
--     -   Ralph Potter, Samsung
--
--     -   Matthew Netsch, Qualcomm
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Samuel Pitoiset, Valve
--
--     -   Tatsuyuki Ishi, Valve
--
-- == Description
--
-- This extension provides a method to obtain binary data associated with
-- individual pipelines such that applications can manage caching
-- themselves instead of using VkPipelineCache objects.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.PipelineBinaryKHR'
--
-- == New Commands
--
-- -   'createPipelineBinariesKHR'
--
-- -   'destroyPipelineBinaryKHR'
--
-- -   'getPipelineBinaryDataKHR'
--
-- -   'getPipelineKeyKHR'
--
-- -   'releaseCapturedPipelineDataKHR'
--
-- == New Structures
--
-- -   'PipelineBinaryCreateInfoKHR'
--
-- -   'PipelineBinaryDataInfoKHR'
--
-- -   'PipelineBinaryDataKHR'
--
-- -   'PipelineBinaryHandlesInfoKHR'
--
-- -   'PipelineBinaryKeyKHR'
--
-- -   'PipelineBinaryKeysAndDataKHR'
--
-- -   'PipelineCreateInfoKHR'
--
-- -   'ReleaseCapturedPipelineDataInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DevicePipelineBinaryInternalCacheControlKHR'
--
-- -   Extending 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR':
--
--     -   'PipelineBinaryInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineBinaryFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePipelineBinaryPropertiesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PIPELINE_BINARY_EXTENSION_NAME'
--
-- -   'KHR_PIPELINE_BINARY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_PIPELINE_BINARY_KEY_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_PIPELINE_BINARY_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_maintenance5.PipelineCreateFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_KHR_maintenance5.PIPELINE_CREATE_2_CAPTURE_DATA_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_ENOUGH_SPACE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_BINARY_MISSING_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_PIPELINE_BINARY_INTERNAL_CACHE_CONTROL_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_BINARY_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_DATA_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_HANDLES_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_BINARY_KEY_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RELEASE_CAPTURED_PIPELINE_DATA_INFO_KHR'
--
-- == Version History
--
-- -   Revision 1, 2021-12-10 (Chris Glover)
--
--     -   Initial draft.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_pipeline_binary Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_pipeline_binary  ( DevicePipelineBinaryInternalCacheControlKHR
                                                 , PhysicalDevicePipelineBinaryFeaturesKHR
                                                 , PhysicalDevicePipelineBinaryPropertiesKHR
                                                 , PipelineBinaryCreateInfoKHR
                                                 , PipelineBinaryDataInfoKHR
                                                 , PipelineBinaryDataKHR
                                                 , PipelineBinaryHandlesInfoKHR
                                                 , PipelineBinaryInfoKHR
                                                 , PipelineBinaryKeyKHR
                                                 , PipelineBinaryKeysAndDataKHR
                                                 , PipelineCreateInfoKHR
                                                 , ReleaseCapturedPipelineDataInfoKHR
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DevicePipelineBinaryInternalCacheControlKHR

instance ToCStruct DevicePipelineBinaryInternalCacheControlKHR
instance Show DevicePipelineBinaryInternalCacheControlKHR

instance FromCStruct DevicePipelineBinaryInternalCacheControlKHR


data PhysicalDevicePipelineBinaryFeaturesKHR

instance ToCStruct PhysicalDevicePipelineBinaryFeaturesKHR
instance Show PhysicalDevicePipelineBinaryFeaturesKHR

instance FromCStruct PhysicalDevicePipelineBinaryFeaturesKHR


data PhysicalDevicePipelineBinaryPropertiesKHR

instance ToCStruct PhysicalDevicePipelineBinaryPropertiesKHR
instance Show PhysicalDevicePipelineBinaryPropertiesKHR

instance FromCStruct PhysicalDevicePipelineBinaryPropertiesKHR


data PipelineBinaryCreateInfoKHR

instance ToCStruct PipelineBinaryCreateInfoKHR
instance Show PipelineBinaryCreateInfoKHR

instance FromCStruct PipelineBinaryCreateInfoKHR


data PipelineBinaryDataInfoKHR

instance ToCStruct PipelineBinaryDataInfoKHR
instance Show PipelineBinaryDataInfoKHR

instance FromCStruct PipelineBinaryDataInfoKHR


data PipelineBinaryDataKHR

instance ToCStruct PipelineBinaryDataKHR
instance Show PipelineBinaryDataKHR

instance FromCStruct PipelineBinaryDataKHR


data PipelineBinaryHandlesInfoKHR

instance ToCStruct PipelineBinaryHandlesInfoKHR
instance Show PipelineBinaryHandlesInfoKHR

instance FromCStruct PipelineBinaryHandlesInfoKHR


data PipelineBinaryInfoKHR

instance ToCStruct PipelineBinaryInfoKHR
instance Show PipelineBinaryInfoKHR

instance FromCStruct PipelineBinaryInfoKHR


data PipelineBinaryKeyKHR

instance ToCStruct PipelineBinaryKeyKHR
instance Show PipelineBinaryKeyKHR

instance FromCStruct PipelineBinaryKeyKHR


data PipelineBinaryKeysAndDataKHR

instance ToCStruct PipelineBinaryKeysAndDataKHR
instance Show PipelineBinaryKeysAndDataKHR

instance FromCStruct PipelineBinaryKeysAndDataKHR


data PipelineCreateInfoKHR

instance ToCStruct PipelineCreateInfoKHR
instance Show PipelineCreateInfoKHR

instance FromCStruct PipelineCreateInfoKHR


data ReleaseCapturedPipelineDataInfoKHR

instance ToCStruct ReleaseCapturedPipelineDataInfoKHR
instance Show ReleaseCapturedPipelineDataInfoKHR

instance FromCStruct ReleaseCapturedPipelineDataInfoKHR

