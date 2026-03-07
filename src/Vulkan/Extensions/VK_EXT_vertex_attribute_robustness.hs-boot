{-# language CPP #-}
-- | = Name
--
-- VK_EXT_vertex_attribute_robustness - device extension
--
-- = VK_EXT_vertex_attribute_robustness
--
-- [__Name String__]
--     @VK_EXT_vertex_attribute_robustness@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     609
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Deprecation State__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>
--         extension
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_vertex_attribute_robustness] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_EXT_vertex_attribute_robustness extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-11-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- It can be detrimental to performance for applications to have to define
-- fake vertex attribute locations and buffer bindings for vertex shaders
-- that may reference attribute locations for which there is no vertex
-- data.
--
-- This extension allows applications to not have to specify fake vertex
-- attribute locations, and if the vertex shader reads those attributes it
-- will read (0,0,0,0) or (0,0,0,1).
--
-- == Promotion to @VK_KHR_maintenance9@
--
-- The same functionality is provided by
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance9 VK_KHR_maintenance9>,
-- but enabled by the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance9 maintenance9>
-- feature instead. The
-- 'PhysicalDeviceVertexAttributeRobustnessFeaturesEXT' structure was not
-- included in the maintenance extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceVertexAttributeRobustnessFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_EXTENSION_NAME'
--
-- -   'EXT_VERTEX_ATTRIBUTE_ROBUSTNESS_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_ROBUSTNESS_FEATURES_EXT'
--
-- == Issues
--
-- None
--
-- == Version History
--
-- -   Revision 1, 2024-11-01 (Piers Daniell)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_vertex_attribute_robustness Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_vertex_attribute_robustness  (PhysicalDeviceVertexAttributeRobustnessFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVertexAttributeRobustnessFeaturesEXT

instance ToCStruct PhysicalDeviceVertexAttributeRobustnessFeaturesEXT
instance Show PhysicalDeviceVertexAttributeRobustnessFeaturesEXT

instance FromCStruct PhysicalDeviceVertexAttributeRobustnessFeaturesEXT

