{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph_optical_flow - device extension
--
-- = VK_ARM_data_graph_optical_flow
--
-- [__Name String__]
--     @VK_ARM_data_graph_optical_flow@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     632
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph_optical_flow] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph_optical_flow extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-04-08
--
-- [__Contributors__]
--
--     -   Contributors to VK_NV_optical_flow
--
--     -   Kevin Petit, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Steve Suzuki, Arm Ltd.
--
--     -   Liam O’Neil, Arm Ltd.
--
-- == Description
--
-- This extension allows applications to estimate the 2D displacement of
-- pixels between two images.
--
-- == New Commands
--
-- -   'Vulkan.Extensions.VK_ARM_data_graph_instruction_set_tosa.getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM'
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphOpticalFlowImageFormatsARM'
--
-- == New Structures
--
-- -   'DataGraphOpticalFlowImageFormatPropertiesARM'
--
-- -   'DataGraphPipelineSingleNodeConnectionARM'
--
-- -   'QueueFamilyDataGraphOpticalFlowPropertiesARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineOpticalFlowCreateInfoARM'
--
--     -   'DataGraphPipelineSingleNodeCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineDispatchInfoARM':
--
--     -   'DataGraphPipelineOpticalFlowDispatchInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineResourceInfoARM':
--
--     -   'DataGraphPipelineResourceInfoImageLayoutARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphOpticalFlowFeaturesARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
--     'Vulkan.Core10.Image.ImageCreateInfo':
--
--     -   'DataGraphOpticalFlowImageFormatInfoARM'
--
-- == New Enums
--
-- -   'DataGraphOpticalFlowCreateFlagBitsARM'
--
-- -   'DataGraphOpticalFlowExecuteFlagBitsARM'
--
-- -   'DataGraphOpticalFlowGridSizeFlagBitsARM'
--
-- -   'DataGraphOpticalFlowImageUsageFlagBitsARM'
--
-- -   'DataGraphOpticalFlowPerformanceLevelARM'
--
-- -   'DataGraphPipelineNodeConnectionTypeARM'
--
-- -   'DataGraphPipelineNodeTypeARM'
--
-- == New Bitmasks
--
-- -   'DataGraphOpticalFlowCreateFlagsARM'
--
-- -   'DataGraphOpticalFlowExecuteFlagsARM'
--
-- -   'DataGraphOpticalFlowGridSizeFlagsARM'
--
-- -   'DataGraphOpticalFlowImageUsageFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_OPTICAL_FLOW_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_OPTICAL_FLOW_SPEC_VERSION'
--
-- -   Extending 'DataGraphPipelineNodeConnectionTypeARM':
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_COST_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_FLOW_VECTOR_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_HINT_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_INPUT_ARM'
--
--     -   'DATA_GRAPH_PIPELINE_NODE_CONNECTION_TYPE_OPTICAL_FLOW_REFERENCE_ARM'
--
-- -   Extending 'DataGraphPipelineNodeTypeARM':
--
--     -   'DATA_GRAPH_PIPELINE_NODE_TYPE_OPTICAL_FLOW_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionBindPointARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_OPTICAL_FLOW_CACHE_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionCreateFlagBitsARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_SESSION_CREATE_OPTICAL_FLOW_CACHE_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DATA_GRAPH_OPTICAL_FLOW_COST_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DATA_GRAPH_OPTICAL_FLOW_IMAGE_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_DATA_GRAPH_OPTICAL_FLOW_VECTOR_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationTypeARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_OPTICAL_FLOW_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_OPTICAL_FLOW_IMAGE_FORMAT_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_OPTICAL_FLOW_DISPATCH_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_IMAGE_LAYOUT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CONNECTION_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SINGLE_NODE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_OPTICAL_FLOW_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_OPTICAL_FLOW_PROPERTIES_ARM'
--
-- == Version History
--
-- -   Revision 1, 2026-04-08 (Kevin Petit)
--
--     -   Internal revisions
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph_optical_flow Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph_optical_flow  ( DataGraphOpticalFlowImageFormatInfoARM
                                                         , DataGraphOpticalFlowImageFormatPropertiesARM
                                                         , DataGraphPipelineOpticalFlowCreateInfoARM
                                                         , DataGraphPipelineOpticalFlowDispatchInfoARM
                                                         , DataGraphPipelineResourceInfoImageLayoutARM
                                                         , DataGraphPipelineSingleNodeConnectionARM
                                                         , DataGraphPipelineSingleNodeCreateInfoARM
                                                         , PhysicalDeviceDataGraphOpticalFlowFeaturesARM
                                                         , QueueFamilyDataGraphOpticalFlowPropertiesARM
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DataGraphOpticalFlowImageFormatInfoARM

instance ToCStruct DataGraphOpticalFlowImageFormatInfoARM
instance Show DataGraphOpticalFlowImageFormatInfoARM

instance FromCStruct DataGraphOpticalFlowImageFormatInfoARM


data DataGraphOpticalFlowImageFormatPropertiesARM

instance ToCStruct DataGraphOpticalFlowImageFormatPropertiesARM
instance Show DataGraphOpticalFlowImageFormatPropertiesARM

instance FromCStruct DataGraphOpticalFlowImageFormatPropertiesARM


data DataGraphPipelineOpticalFlowCreateInfoARM

instance ToCStruct DataGraphPipelineOpticalFlowCreateInfoARM
instance Show DataGraphPipelineOpticalFlowCreateInfoARM

instance FromCStruct DataGraphPipelineOpticalFlowCreateInfoARM


data DataGraphPipelineOpticalFlowDispatchInfoARM

instance ToCStruct DataGraphPipelineOpticalFlowDispatchInfoARM
instance Show DataGraphPipelineOpticalFlowDispatchInfoARM

instance FromCStruct DataGraphPipelineOpticalFlowDispatchInfoARM


data DataGraphPipelineResourceInfoImageLayoutARM

instance ToCStruct DataGraphPipelineResourceInfoImageLayoutARM
instance Show DataGraphPipelineResourceInfoImageLayoutARM

instance FromCStruct DataGraphPipelineResourceInfoImageLayoutARM


data DataGraphPipelineSingleNodeConnectionARM

instance ToCStruct DataGraphPipelineSingleNodeConnectionARM
instance Show DataGraphPipelineSingleNodeConnectionARM

instance FromCStruct DataGraphPipelineSingleNodeConnectionARM


data DataGraphPipelineSingleNodeCreateInfoARM

instance ToCStruct DataGraphPipelineSingleNodeCreateInfoARM
instance Show DataGraphPipelineSingleNodeCreateInfoARM

instance FromCStruct DataGraphPipelineSingleNodeCreateInfoARM


data PhysicalDeviceDataGraphOpticalFlowFeaturesARM

instance ToCStruct PhysicalDeviceDataGraphOpticalFlowFeaturesARM
instance Show PhysicalDeviceDataGraphOpticalFlowFeaturesARM

instance FromCStruct PhysicalDeviceDataGraphOpticalFlowFeaturesARM


data QueueFamilyDataGraphOpticalFlowPropertiesARM

instance ToCStruct QueueFamilyDataGraphOpticalFlowPropertiesARM
instance Show QueueFamilyDataGraphOpticalFlowPropertiesARM

instance FromCStruct QueueFamilyDataGraphOpticalFlowPropertiesARM

