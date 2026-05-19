{-# language CPP #-}
-- | = Name
--
-- VK_ARM_data_graph_instruction_set_tosa - device extension
--
-- = VK_ARM_data_graph_instruction_set_tosa
--
-- [__Name String__]
--     @VK_ARM_data_graph_instruction_set_tosa@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     509
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_ARM_data_graph_instruction_set_tosa] @kpet%0A*Here describe the issue or question you have about the VK_ARM_data_graph_instruction_set_tosa extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-30
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://github.khronos.org/SPIRV-Registry//extended/TOSA.001000.1.html the TOSA SPIR-V 001000.1 extended instruction set>
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Kévin Petit, Arm Ltd.
--
-- == Description
--
-- This extensions adds support for the @TOSA.001000.1@ extended
-- instruction set for use in data graphs as defined by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_ARM_data_graph VK_ARM_data_graph>.
-- It also adds detailed queries to report the TOSA profiles, levels, and
-- extensions that are supported along with the quality of the
-- implementation (e.g. accelerated, experimental, deprecated, etc).
--
-- == New Commands
--
-- -   'getPhysicalDeviceQueueFamilyDataGraphEngineOperationPropertiesARM'
--
-- == New Structures
--
-- -   'DataGraphTOSANameQualityARM'
--
-- -   'QueueFamilyDataGraphTOSAPropertiesARM'
--
-- == New Enums
--
-- -   'DataGraphTOSALevelARM'
--
-- -   'DataGraphTOSAQualityFlagBitsARM'
--
-- == New Bitmasks
--
-- -   'DataGraphTOSAQualityFlagsARM'
--
-- == New Enum Constants
--
-- -   'ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_EXTENSION_NAME'
--
-- -   'ARM_DATA_GRAPH_INSTRUCTION_SET_TOSA_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_DATA_GRAPH_TOSA_NAME_SIZE_ARM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_DATA_GRAPH_TOSA_PROPERTIES_ARM'
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
-- -   Revision 1, 2026-03-30 (Kévin Petit)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_ARM_data_graph_instruction_set_tosa Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_ARM_data_graph_instruction_set_tosa  ( DataGraphTOSANameQualityARM
                                                                 , QueueFamilyDataGraphTOSAPropertiesARM
                                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DataGraphTOSANameQualityARM

instance ToCStruct DataGraphTOSANameQualityARM
instance Show DataGraphTOSANameQualityARM

instance FromCStruct DataGraphTOSANameQualityARM


data QueueFamilyDataGraphTOSAPropertiesARM

instance ToCStruct QueueFamilyDataGraphTOSAPropertiesARM
instance Show QueueFamilyDataGraphTOSAPropertiesARM

instance FromCStruct QueueFamilyDataGraphTOSAPropertiesARM

