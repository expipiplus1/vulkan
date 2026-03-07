{-# language CPP #-}
-- | = Name
--
-- VK_EXT_legacy_vertex_attributes - device extension
--
-- = VK_EXT_legacy_vertex_attributes
--
-- [__Name String__]
--     @VK_EXT_legacy_vertex_attributes@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     496
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_vertex_input_dynamic_state VK_EXT_vertex_input_dynamic_state>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_legacy_vertex_attributes] @zmike%0A*Here describe the issue or question you have about the VK_EXT_legacy_vertex_attributes extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_legacy_vertex_attributes.adoc VK_EXT_legacy_vertex_attributes>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-02-23
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Piers Daniell, NVIDIA
--
--     -   Spencer Fricke, LunarG
--
--     -   Alyssa Rosenzweig, Valve
--
-- == Description
--
-- This extension adds support for legacy features of (non-64-bit) vertex
-- attributes as found in OpenGL:
--
-- -   Vertex attributes loaded from arbitrary buffer alignments
--
-- -   Vertex attributes using arbitrary strides
--
-- -   Vertex attributes where the component data type of the binding does
--     not match the component numeric type of the shader input
--
-- These features are only usable with dynamic vertex input. Unaligned
-- loads of vertex attributes may incur performance penalties, indicated
-- with a property.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceLegacyVertexAttributesFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceLegacyVertexAttributesPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_LEGACY_VERTEX_ATTRIBUTES_EXTENSION_NAME'
--
-- -   'EXT_LEGACY_VERTEX_ATTRIBUTES_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_LEGACY_VERTEX_ATTRIBUTES_PROPERTIES_EXT'
--
-- == Issues
--
-- 1.) Should implementations convert float\/integer values?
--
-- __RESOLVED__: No. When fetching an integer data type from float values
-- or float data types from integer values, the resulting shader values are
-- implementation-dependent.
--
-- == Version History
--
-- -   Revision 1, 2024-02-16 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_legacy_vertex_attributes Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_legacy_vertex_attributes  ( PhysicalDeviceLegacyVertexAttributesFeaturesEXT
                                                          , PhysicalDeviceLegacyVertexAttributesPropertiesEXT
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceLegacyVertexAttributesFeaturesEXT

instance ToCStruct PhysicalDeviceLegacyVertexAttributesFeaturesEXT
instance Show PhysicalDeviceLegacyVertexAttributesFeaturesEXT

instance FromCStruct PhysicalDeviceLegacyVertexAttributesFeaturesEXT


data PhysicalDeviceLegacyVertexAttributesPropertiesEXT

instance ToCStruct PhysicalDeviceLegacyVertexAttributesPropertiesEXT
instance Show PhysicalDeviceLegacyVertexAttributesPropertiesEXT

instance FromCStruct PhysicalDeviceLegacyVertexAttributesPropertiesEXT

