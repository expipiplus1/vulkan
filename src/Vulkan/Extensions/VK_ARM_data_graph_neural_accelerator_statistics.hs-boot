{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph_neural_accelerator_statistics - device extension
--
-- = VK_ARM_data_graph_neural_accelerator_statistics
--
-- [__Name String__]
--     @VK_ARM_data_graph_neural_accelerator_statistics@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     677
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__Contact__]
--
--     -   Kevin Petit
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph_neural_accelerator_statistics] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph_neural_accelerator_statistics extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-04-28
--
-- [__Interactions and External Dependencies__]
--
--     -   None
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
--     -   Emma Lynn Mulier (Benyossef), Arm Ltd.
--
-- == Description
--
-- This extension adds support for getting data graph execution statistics
-- for Arm neural accelerators.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineCreateInfoARM':
--
--     -   'DataGraphPipelineNeuralStatisticsCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionCreateInfoARM':
--
--     -   'DataGraphPipelineSessionNeuralStatisticsCreateInfoARM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM'
--
-- == New Enums
--
-- -   'NeuralAcceleratorStatisticsModeARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelinePropertyARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_PROPERTY_NEURAL_ACCELERATOR_DEBUG_DATABASE_ARM'
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_PROPERTY_NEURAL_ACCELERATOR_STATISTICS_INFO_ARM'
--
-- -   Extending
--     'Vulkan.Extensions.VK_ARM_data_graph.DataGraphPipelineSessionBindPointARM':
--
--     -   'Vulkan.Extensions.VK_ARM_data_graph.DATA_GRAPH_PIPELINE_SESSION_BIND_POINT_NEURAL_ACCELERATOR_STATISTICS_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_NEURAL_STATISTICS_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DATA_GRAPH_PIPELINE_SESSION_NEURAL_STATISTICS_CREATE_INFO_ARM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DATA_GRAPH_NEURAL_ACCELERATOR_STATISTICS_FEATURES_ARM'
--
-- == New SPIR-V Capabilities
--
-- None.
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2026-04-28 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph_neural_accelerator_statistics Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph_neural_accelerator_statistics  ( DataGraphPipelineNeuralStatisticsCreateInfoARM
                                                                          , DataGraphPipelineSessionNeuralStatisticsCreateInfoARM
                                                                          , PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM
                                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DataGraphPipelineNeuralStatisticsCreateInfoARM

instance ToCStruct DataGraphPipelineNeuralStatisticsCreateInfoARM
instance Show DataGraphPipelineNeuralStatisticsCreateInfoARM

instance FromCStruct DataGraphPipelineNeuralStatisticsCreateInfoARM


data DataGraphPipelineSessionNeuralStatisticsCreateInfoARM

instance ToCStruct DataGraphPipelineSessionNeuralStatisticsCreateInfoARM
instance Show DataGraphPipelineSessionNeuralStatisticsCreateInfoARM

instance FromCStruct DataGraphPipelineSessionNeuralStatisticsCreateInfoARM


data PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM

instance ToCStruct PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM
instance Show PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM

instance FromCStruct PhysicalDeviceDataGraphNeuralAcceleratorStatisticsFeaturesARM

