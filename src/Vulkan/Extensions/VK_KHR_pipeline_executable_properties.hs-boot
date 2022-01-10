{-# language CPP #-}
-- | = Name
--
-- VK_KHR_pipeline_executable_properties - device extension
--
-- == VK_KHR_pipeline_executable_properties
--
-- [__Name String__]
--     @VK_KHR_pipeline_executable_properties@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     270
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse Developer tools>
--
-- [__Contact__]
--
--     -   Jason Ekstrand
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_pipeline_executable_properties] @jekstrand%0A<<Here describe the issue or question you have about the VK_KHR_pipeline_executable_properties extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Jason Ekstrand, Intel
--
--     -   Ian Romanick, Intel
--
--     -   Kenneth Graunke, Intel
--
--     -   Baldur Karlsson, Valve
--
--     -   Jesse Hall, Google
--
--     -   Jeff Bolz, Nvidia
--
--     -   Piers Daniel, Nvidia
--
--     -   Tobias Hector, AMD
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Tom Olson, ARM
--
--     -   Daniel Koch, Nvidia
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- When a pipeline is created, its state and shaders are compiled into zero
-- or more device-specific executables, which are used when executing
-- commands against that pipeline. This extension adds a mechanism to query
-- properties and statistics about the different executables produced by
-- the pipeline compilation process. This is intended to be used by
-- debugging and performance tools to allow them to provide more detailed
-- information to the user. Certain compile-time shader statistics provided
-- through this extension may be useful to developers for debugging or
-- performance analysis.
--
-- == New Commands
--
-- -   'getPipelineExecutableInternalRepresentationsKHR'
--
-- -   'getPipelineExecutablePropertiesKHR'
--
-- -   'getPipelineExecutableStatisticsKHR'
--
-- == New Structures
--
-- -   'PipelineExecutableInfoKHR'
--
-- -   'PipelineExecutableInternalRepresentationKHR'
--
-- -   'PipelineExecutablePropertiesKHR'
--
-- -   'PipelineExecutableStatisticKHR'
--
-- -   'PipelineInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR'
--
-- == New Unions
--
-- -   'PipelineExecutableStatisticValueKHR'
--
-- == New Enums
--
-- -   'PipelineExecutableStatisticFormatKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PIPELINE_EXECUTABLE_PROPERTIES_EXTENSION_NAME'
--
-- -   'KHR_PIPELINE_EXECUTABLE_PROPERTIES_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_CAPTURE_STATISTICS_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INFO_KHR'
--
-- == Issues
--
-- 1) What should we call the pieces of the pipeline which are produced by
-- the compilation process and about which you can query properties and
-- statistics?
--
-- __RESOLVED__: Call them “executables”. The name “binary” was used in
-- early drafts of the extension but it was determined that “pipeline
-- binary” could have a fairly broad meaning (such as a binary serialized
-- form of an entire pipeline) and was too big of a namespace for the very
-- specific needs of this extension.
--
-- == Version History
--
-- -   Revision 1, 2019-05-28 (Jason Ekstrand)
--
--     -   Initial draft
--
-- == See Also
--
-- 'PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
-- 'PipelineExecutableInfoKHR',
-- 'PipelineExecutableInternalRepresentationKHR',
-- 'PipelineExecutablePropertiesKHR',
-- 'PipelineExecutableStatisticFormatKHR',
-- 'PipelineExecutableStatisticKHR', 'PipelineExecutableStatisticValueKHR',
-- 'PipelineInfoKHR', 'getPipelineExecutableInternalRepresentationsKHR',
-- 'getPipelineExecutablePropertiesKHR',
-- 'getPipelineExecutableStatisticsKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_pipeline_executable_properties  ( PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
                                                                , PipelineExecutableInfoKHR
                                                                , PipelineExecutableInternalRepresentationKHR
                                                                , PipelineExecutablePropertiesKHR
                                                                , PipelineExecutableStatisticKHR
                                                                , PipelineInfoKHR
                                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineExecutablePropertiesFeaturesKHR

instance ToCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
instance Show PhysicalDevicePipelineExecutablePropertiesFeaturesKHR

instance FromCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR


data PipelineExecutableInfoKHR

instance ToCStruct PipelineExecutableInfoKHR
instance Show PipelineExecutableInfoKHR

instance FromCStruct PipelineExecutableInfoKHR


data PipelineExecutableInternalRepresentationKHR

instance ToCStruct PipelineExecutableInternalRepresentationKHR
instance Show PipelineExecutableInternalRepresentationKHR

instance FromCStruct PipelineExecutableInternalRepresentationKHR


data PipelineExecutablePropertiesKHR

instance ToCStruct PipelineExecutablePropertiesKHR
instance Show PipelineExecutablePropertiesKHR

instance FromCStruct PipelineExecutablePropertiesKHR


data PipelineExecutableStatisticKHR

instance ToCStruct PipelineExecutableStatisticKHR
instance Show PipelineExecutableStatisticKHR

instance FromCStruct PipelineExecutableStatisticKHR


data PipelineInfoKHR

instance ToCStruct PipelineInfoKHR
instance Show PipelineInfoKHR

instance FromCStruct PipelineInfoKHR

