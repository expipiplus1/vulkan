{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph - device extension
--
-- = VK_ARM_data_graph
--
-- [__Name String__]
--     @VK_ARM_data_graph@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     508
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Vulkan Version 1.3>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_deferred_host_operations VK_KHR_deferred_host_operations>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_ARM_tensors
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/ARM/SPV_ARM_graph.html SPV_ARM_graph>
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-18
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_ARM_graph.html SPV_ARM_graph>
--
--     -   This extension interacts with @VK_EXT_mutable_descriptor_type@
--
--     -   This extension interacts with @VK_EXT_pipeline_protected_access@
--
--     -   This extension interacts with @VK_ARM_tensors@
--
--     -   This extension interacts with @VK_EXT_descriptor_buffer@
--
--     -   This extension interacts with @VK_KHR_maintenance6@
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Emma Ben Yossef, Arm Ltd.
--
--     -   Stefano Bucciarelli, Arm Ltd.
--
--     -   Marco Cattani, Arm Ltd.
--
--     -   Aaron DeBattista, Arm Ltd.
--
--     -   Jan-Harald Fredriksen, Arm Ltd.
--
--     -   Einar Hov, Arm Ltd.
--
--     -   Robert Hughes, Arm Ltd.
--
--     -   Oualid Khelifi, Arm Ltd.
--
--     -   Derek Lamberti, Arm Ltd.
--
--     -   Chetan Mistry, Arm Ltd.
--
--     -   Georgios Teneketzis, Arm Ltd.
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension adds support for a new type of pipeline, data graph
-- pipelines, that provide an encapsulation construct for computational
-- graphs operating on full resources (e.g. ML\/AI graphs, image processing
-- pipelines, etc). This extension only supports tensor resources and does
-- not define any operations that can be used within those graphs. These
-- operations will be defined by separate extensions.
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.DataGraphPipelineSessionARM'
--
-- == New Commands
--
-- -   'bindDataGraphPipelineSessionMemoryARM'
--
-- -   'cmdDispatchDataGraphARM'
--
-- -   'createDataGraphPipelineSessionARM'
--
-- -   'createDataGraphPipelinesARM'
--
-- -   'destroyDataGraphPipelineSessionARM'
--
-- -   'getDataGraphPipelineAvailablePropertiesARM'
--
-- -   'getDataGraphPipelinePropertiesARM'
--
-- -   'getDataGraphPipelineSessionBindPointRequirementsARM'
--
-- -   'getDataGraphPipelineSessionMemoryRequirementsARM'
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphProcessingEnginePropertiesARM'
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphPropertiesARM'
--
-- == New Structures
--
-- -   'BindDataGraphPipelineSessionMemoryInfoARM'
--
-- -   'DataGraphPipelineConstantARM'
--
-- -   'DataGraphPipelineCreateInfoARM'
--
-- -   'DataGraphPipelineDispatchInfoARM'
--
-- -   'DataGraphPipelineInfoARM'
--
-- -   'DataGraphPipelinePropertyQueryResultARM'
--
-- -   'DataGraphPipelineResourceInfoARM'
--
-- -   'DataGraphPipelineSessionBindPointRequirementARM'
--
-- -   'DataGraphPipelineSessionBindPointRequirementsInfoARM'
--
-- -   'DataGraphPipelineSessionCreateInfoARM'
--
-- -   'DataGraphPipelineSessionMemoryRequirementsInfoARM'
--
-- -   'PhysicalDeviceDataGraphOperationSupportARM'
--
-- -   'PhysicalDeviceDataGraphProcessingEngineARM'
--
-- -   'PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM'
--
-- -   'QueueFamilyDataGraphProcessingEnginePropertiesARM'
--
-- -   'QueueFamilyDataGraphPropertiesARM'
--
-- -   Extending 'DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineCompilerControlCreateInfoARM'
--
--     -   'DataGraphPipelineIdentifierCreateInfoARM'
--
--     -   'DataGraphPipelineShaderModuleCreateInfoARM'
--
-- -   Extending 'DataGraphPipelineCreateInfoARM',
--     'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo',
--     'Vulkan.Core10.CommandPool.CommandPoolCreateInfo':
--
--     -   'DataGraphProcessingEngineCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphFeaturesARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   Extending 'DataGraphPipelineConstantARM':
--
--     -   'DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM'
--
-- == New Enums
--
-- -   'DataGraphPipelineDispatchFlagBitsARM'
--
-- -   'DataGraphPipelinePropertyARM'
--
-- -   'DataGraphPipelineSessionBindPointARM'
--
-- -   'DataGraphPipelineSessionBindPointTypeARM'
--
-- -   'DataGraphPipelineSessionCreateFlagBitsARM'
--
-- -   'PhysicalDeviceDataGraphOperationTypeARM'
--
-- -   'PhysicalDeviceDataGraphProcessingEngineTypeARM'
--
-- == New Bitmasks
--
-- -   'DataGraphPipelineDispatchFlagsARM'
--
-- -   'DataGraphPipelineSessionCreateFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_SET_NAME_SIZE_ARM'
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DATA_GRAPH_READ_BIT_ARM'
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_DATA_GRAPH_WRITE_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core14.Enums.BufferUsageFlags2.BufferUsageFlagBits2':
--
--     -   'Vulkan.Core14.Enums.BufferUsageFlags2.BUFFER_USAGE_2_DATA_GRAPH_FOREIGN_DESCRIPTOR_BIT_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FormatFeatureFlagBits2':
--
--     -   'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_TENSOR_DATA_GRAPH_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_DATA_GRAPH_PIPELINE_SESSION_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint':
--
--     -   'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_DATA_GRAPH_ARM'
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_DATA_GRAPH_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.QueueFlagBits.QueueFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueueFlagBits.QUEUE_DATA_GRAPH_BIT_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_DATA_GRAPH_PIPELINE_SESSION_MEMORY_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_COMPILER_CONTROL_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_DISPATCH_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_IDENTIFIER_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_PROPERTY_QUERY_RESULT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_RESOURCE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENTS_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_REQUIREMENT_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_MEMORY_REQUIREMENTS_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SHADER_MODULE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PROCESSING_ENGINE_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_FEATURES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROCESSING_ENGINE_PROPERTIES_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_PROPERTIES_ARM'
--
-- -   Extending 'Vulkan.Extensions.VK_ARM_tensors.TensorUsageFlagBitsARM':
--
--     -   'Vulkan.Extensions.VK_ARM_tensors.TENSOR_USAGE_DATA_GRAPH_BIT_ARM'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_tensors VK_ARM_tensors>
-- is supported:
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_CONSTANT_TENSOR_SEMI_STRUCTURED_SPARSITY_INFO_ARM'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-GraphARM GraphARM>
--
-- == Issues
--
-- 1) Should graph pipeline resource info structures be integrated into
-- pipeline layouts? Would a new graph pipeline layout be a better fit?
--
-- __RESOLVED__: Graph pipeline resource info are passed separately at
-- pipeline creation time.
--
-- 2) Do we need a new shader stage for graph pipelines for use in creating
-- descriptor set layouts?
--
-- __RESOLVED__: Currently using
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ALL'.
--
-- 3) Should this extension provide applications with a way of knowing
-- which combinations of sparsity information implementations can take
-- advantage of when processing graph constants?
--
-- __RESOLVED__: No. Describing the exact combinations is in some cases
-- complex and it is always valid for implementations to ignore the
-- sparsity information and treat the data as dense. Specific
-- implementations can provide guidance to application writers if they so
-- desire and applications are encouraged to always provide sparsity
-- information that they have.
--
-- == Version History
--
-- -   Revision 1, 2025-06-18 (Kévin Petit)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph  ( BindDataGraphPipelineSessionMemoryInfoARM
                                            , DataGraphPipelineCompilerControlCreateInfoARM
                                            , DataGraphPipelineConstantARM
                                            , DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM
                                            , DataGraphPipelineCreateInfoARM
                                            , DataGraphPipelineDispatchInfoARM
                                            , DataGraphPipelineIdentifierCreateInfoARM
                                            , DataGraphPipelineInfoARM
                                            , DataGraphPipelinePropertyQueryResultARM
                                            , DataGraphPipelineResourceInfoARM
                                            , DataGraphPipelineSessionBindPointRequirementARM
                                            , DataGraphPipelineSessionBindPointRequirementsInfoARM
                                            , DataGraphPipelineSessionCreateInfoARM
                                            , DataGraphPipelineSessionMemoryRequirementsInfoARM
                                            , DataGraphPipelineShaderModuleCreateInfoARM
                                            , DataGraphProcessingEngineCreateInfoARM
                                            , PhysicalDeviceDataGraphFeaturesARM
                                            , PhysicalDeviceDataGraphOperationSupportARM
                                            , PhysicalDeviceDataGraphProcessingEngineARM
                                            , PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM
                                            , QueueFamilyDataGraphProcessingEnginePropertiesARM
                                            , QueueFamilyDataGraphPropertiesARM
                                            , DataGraphPipelinePropertyARM
                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BindDataGraphPipelineSessionMemoryInfoARM

instance ToCStruct BindDataGraphPipelineSessionMemoryInfoARM
instance Show BindDataGraphPipelineSessionMemoryInfoARM

instance FromCStruct BindDataGraphPipelineSessionMemoryInfoARM


data DataGraphPipelineCompilerControlCreateInfoARM

instance ToCStruct DataGraphPipelineCompilerControlCreateInfoARM
instance Show DataGraphPipelineCompilerControlCreateInfoARM

instance FromCStruct DataGraphPipelineCompilerControlCreateInfoARM


type role DataGraphPipelineConstantARM nominal
data DataGraphPipelineConstantARM (es :: [Type])

instance ( Extendss DataGraphPipelineConstantARM es
         , PokeChain es ) => ToCStruct (DataGraphPipelineConstantARM es)
instance Show (Chain es) => Show (DataGraphPipelineConstantARM es)

instance ( Extendss DataGraphPipelineConstantARM es
         , PeekChain es ) => FromCStruct (DataGraphPipelineConstantARM es)


data DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM

instance ToCStruct DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM
instance Show DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM

instance FromCStruct DataGraphPipelineConstantTensorSemiStructuredSparsityInfoARM


type role DataGraphPipelineCreateInfoARM nominal
data DataGraphPipelineCreateInfoARM (es :: [Type])

instance ( Extendss DataGraphPipelineCreateInfoARM es
         , PokeChain es ) => ToCStruct (DataGraphPipelineCreateInfoARM es)
instance Show (Chain es) => Show (DataGraphPipelineCreateInfoARM es)

instance ( Extendss DataGraphPipelineCreateInfoARM es
         , PeekChain es ) => FromCStruct (DataGraphPipelineCreateInfoARM es)


data DataGraphPipelineDispatchInfoARM

instance ToCStruct DataGraphPipelineDispatchInfoARM
instance Show DataGraphPipelineDispatchInfoARM

instance FromCStruct DataGraphPipelineDispatchInfoARM


data DataGraphPipelineIdentifierCreateInfoARM

instance ToCStruct DataGraphPipelineIdentifierCreateInfoARM
instance Show DataGraphPipelineIdentifierCreateInfoARM

instance FromCStruct DataGraphPipelineIdentifierCreateInfoARM


data DataGraphPipelineInfoARM

instance ToCStruct DataGraphPipelineInfoARM
instance Show DataGraphPipelineInfoARM

instance FromCStruct DataGraphPipelineInfoARM


data DataGraphPipelinePropertyQueryResultARM

instance ToCStruct DataGraphPipelinePropertyQueryResultARM
instance Show DataGraphPipelinePropertyQueryResultARM

instance FromCStruct DataGraphPipelinePropertyQueryResultARM


type role DataGraphPipelineResourceInfoARM nominal
data DataGraphPipelineResourceInfoARM (es :: [Type])

instance ( Extendss DataGraphPipelineResourceInfoARM es
         , PokeChain es ) => ToCStruct (DataGraphPipelineResourceInfoARM es)
instance Show (Chain es) => Show (DataGraphPipelineResourceInfoARM es)

instance ( Extendss DataGraphPipelineResourceInfoARM es
         , PeekChain es ) => FromCStruct (DataGraphPipelineResourceInfoARM es)


data DataGraphPipelineSessionBindPointRequirementARM

instance ToCStruct DataGraphPipelineSessionBindPointRequirementARM
instance Show DataGraphPipelineSessionBindPointRequirementARM

instance FromCStruct DataGraphPipelineSessionBindPointRequirementARM


data DataGraphPipelineSessionBindPointRequirementsInfoARM

instance ToCStruct DataGraphPipelineSessionBindPointRequirementsInfoARM
instance Show DataGraphPipelineSessionBindPointRequirementsInfoARM

instance FromCStruct DataGraphPipelineSessionBindPointRequirementsInfoARM


data DataGraphPipelineSessionCreateInfoARM

instance ToCStruct DataGraphPipelineSessionCreateInfoARM
instance Show DataGraphPipelineSessionCreateInfoARM

instance FromCStruct DataGraphPipelineSessionCreateInfoARM


data DataGraphPipelineSessionMemoryRequirementsInfoARM

instance ToCStruct DataGraphPipelineSessionMemoryRequirementsInfoARM
instance Show DataGraphPipelineSessionMemoryRequirementsInfoARM

instance FromCStruct DataGraphPipelineSessionMemoryRequirementsInfoARM


data DataGraphPipelineShaderModuleCreateInfoARM

instance ToCStruct DataGraphPipelineShaderModuleCreateInfoARM
instance Show DataGraphPipelineShaderModuleCreateInfoARM

instance FromCStruct DataGraphPipelineShaderModuleCreateInfoARM


data DataGraphProcessingEngineCreateInfoARM

instance ToCStruct DataGraphProcessingEngineCreateInfoARM
instance Show DataGraphProcessingEngineCreateInfoARM

instance FromCStruct DataGraphProcessingEngineCreateInfoARM


data PhysicalDeviceDataGraphFeaturesARM

instance ToCStruct PhysicalDeviceDataGraphFeaturesARM
instance Show PhysicalDeviceDataGraphFeaturesARM

instance FromCStruct PhysicalDeviceDataGraphFeaturesARM


data PhysicalDeviceDataGraphOperationSupportARM

instance ToCStruct PhysicalDeviceDataGraphOperationSupportARM
instance Show PhysicalDeviceDataGraphOperationSupportARM

instance FromCStruct PhysicalDeviceDataGraphOperationSupportARM


data PhysicalDeviceDataGraphProcessingEngineARM

instance ToCStruct PhysicalDeviceDataGraphProcessingEngineARM
instance Show PhysicalDeviceDataGraphProcessingEngineARM

instance FromCStruct PhysicalDeviceDataGraphProcessingEngineARM


data PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM

instance ToCStruct PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM
instance Show PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM

instance FromCStruct PhysicalDeviceQueueFamilyDataGraphProcessingEngineInfoARM


data QueueFamilyDataGraphProcessingEnginePropertiesARM

instance ToCStruct QueueFamilyDataGraphProcessingEnginePropertiesARM
instance Show QueueFamilyDataGraphProcessingEnginePropertiesARM

instance FromCStruct QueueFamilyDataGraphProcessingEnginePropertiesARM


data QueueFamilyDataGraphPropertiesARM

instance ToCStruct QueueFamilyDataGraphPropertiesARM
instance Show QueueFamilyDataGraphPropertiesARM

instance FromCStruct QueueFamilyDataGraphPropertiesARM


data DataGraphPipelinePropertyARM

