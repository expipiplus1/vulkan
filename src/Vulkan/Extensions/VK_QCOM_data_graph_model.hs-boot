{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_data_graph_model - device extension
--
-- = VK_QCOM_data_graph_model
--
-- [__Name String__]
--     @VK_QCOM_data_graph_model@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     630
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
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_data_graph_model] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_data_graph_model extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_data_graph_model.adoc VK_QCOM_data_graph_model>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-06-24
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension interacts with @VK_ARM_tensors@
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Rob VanReenen, Qualcomm Technologies, Inc
--
--     -   Balaji Calidas, Qualcomm Technologies, Inc
--
--     -   Jacob Yenney, Qualcomm Technologies, Inc
--
--     -   Kévin Petit, Arm Ltd.
--
-- == Description
--
-- This extension supports new
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphProcessingEngineTypeARM',
-- and
-- 'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationTypeARM'
-- types for data graph pipelines added in @VK_ARM_data_graph@.
--
-- A new pipeline cache type is also added to seamlessly import ML models
-- such as ONNX through QNN workflow, and run them on the device or an
-- external compute engine.
--
-- == New Structures
--
-- -   'PipelineCacheHeaderVersionDataGraphQCOM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineBuiltinModelCreateInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphModelFeaturesQCOM'
--
-- == New Enums
--
-- -   'DataGraphModelCacheTypeQCOM'
--
-- == New Enum Constants
--
-- -   'Vulkan.Core10.APIConstants.DATA_GRAPH_MODEL_TOOLCHAIN_VERSION_LENGTH_QCOM'
--
-- -   'QCOM_DATA_GRAPH_MODEL_EXTENSION_NAME'
--
-- -   'QCOM_DATA_GRAPH_MODEL_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphOperationTypeARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_BUILTIN_MODEL_QCOM'
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_OPERATION_TYPE_NEURAL_MODEL_QCOM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.PhysicalDeviceDataGraphProcessingEngineTypeARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_COMPUTE_QCOM'
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.PHYSICAL_DEVICE_DATA_GRAPH_PROCESSING_ENGINE_TYPE_NEURAL_QCOM'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PipelineCacheHeaderVersion':
--
--     -   'Vulkan.Core10.Enums.PipelineCacheHeaderVersion.PIPELINE_CACHE_HEADER_VERSION_DATA_GRAPH_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_BUILTIN_MODEL_CREATE_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_MODEL_FEATURES_QCOM'
--
-- == Version History
--
-- -   Revision 1, 2025-06-24 (Matthew Netsch)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_data_graph_model Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_data_graph_model  ( DataGraphPipelineBuiltinModelCreateInfoQCOM
                                                   , PhysicalDeviceDataGraphModelFeaturesQCOM
                                                   , PipelineCacheHeaderVersionDataGraphQCOM
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DataGraphPipelineBuiltinModelCreateInfoQCOM

instance ToCStruct DataGraphPipelineBuiltinModelCreateInfoQCOM
instance Show DataGraphPipelineBuiltinModelCreateInfoQCOM

instance FromCStruct DataGraphPipelineBuiltinModelCreateInfoQCOM


data PhysicalDeviceDataGraphModelFeaturesQCOM

instance ToCStruct PhysicalDeviceDataGraphModelFeaturesQCOM
instance Show PhysicalDeviceDataGraphModelFeaturesQCOM

instance FromCStruct PhysicalDeviceDataGraphModelFeaturesQCOM


data PipelineCacheHeaderVersionDataGraphQCOM

instance ToCStruct PipelineCacheHeaderVersionDataGraphQCOM
instance Show PipelineCacheHeaderVersionDataGraphQCOM

instance FromCStruct PipelineCacheHeaderVersionDataGraphQCOM

