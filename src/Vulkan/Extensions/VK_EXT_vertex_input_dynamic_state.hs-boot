{-# language CPP #-}
-- | = Name
--
-- VK_EXT_vertex_input_dynamic_state - device extension
--
-- == VK_EXT_vertex_input_dynamic_state
--
-- [__Name String__]
--     @VK_EXT_vertex_input_dynamic_state@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     353
--
-- [__Revision__]
--     2
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
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_vertex_input_dynamic_state] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_vertex_input_dynamic_state extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- One of the states that contributes to the combinatorial explosion of
-- pipeline state objects that need to be created, is the vertex input
-- binding and attribute descriptions. By allowing them to be dynamic
-- applications may reduce the number of pipeline objects they need to
-- create.
--
-- This extension adds dynamic state support for what is normally static
-- state in 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo'.
--
-- == New Commands
--
-- -   'cmdSetVertexInputEXT'
--
-- == New Structures
--
-- -   'VertexInputAttributeDescription2EXT'
--
-- -   'VertexInputBindingDescription2EXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexInputDynamicStateFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VERTEX_INPUT_DYNAMIC_STATE_EXTENSION_NAME'
--
-- -   'EXT_VERTEX_INPUT_DYNAMIC_STATE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_VERTEX_INPUT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_INPUT_DYNAMIC_STATE_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_ATTRIBUTE_DESCRIPTION_2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VERTEX_INPUT_BINDING_DESCRIPTION_2_EXT'
--
-- == Version History
--
-- -   Revision 2, 2020-11-05 (Piers Daniell)
--
--     -   Make 'VertexInputBindingDescription2EXT' extensible
--
--     -   Add new 'VertexInputAttributeDescription2EXT' struct for the
--         @pVertexAttributeDescriptions@ parameter to
--         'cmdSetVertexInputEXT' so it is also extensible
--
-- -   Revision 1, 2020-08-21 (Piers Daniell)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceVertexInputDynamicStateFeaturesEXT',
-- 'VertexInputAttributeDescription2EXT',
-- 'VertexInputBindingDescription2EXT', 'cmdSetVertexInputEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_vertex_input_dynamic_state Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state  ( PhysicalDeviceVertexInputDynamicStateFeaturesEXT
                                                            , VertexInputAttributeDescription2EXT
                                                            , VertexInputBindingDescription2EXT
                                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVertexInputDynamicStateFeaturesEXT

instance ToCStruct PhysicalDeviceVertexInputDynamicStateFeaturesEXT
instance Show PhysicalDeviceVertexInputDynamicStateFeaturesEXT

instance FromCStruct PhysicalDeviceVertexInputDynamicStateFeaturesEXT


data VertexInputAttributeDescription2EXT

instance ToCStruct VertexInputAttributeDescription2EXT
instance Show VertexInputAttributeDescription2EXT

instance FromCStruct VertexInputAttributeDescription2EXT


data VertexInputBindingDescription2EXT

instance ToCStruct VertexInputBindingDescription2EXT
instance Show VertexInputBindingDescription2EXT

instance FromCStruct VertexInputBindingDescription2EXT

