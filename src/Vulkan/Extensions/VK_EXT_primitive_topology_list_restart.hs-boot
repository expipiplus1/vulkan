{-# language CPP #-}
-- | = Name
--
-- VK_EXT_primitive_topology_list_restart - device extension
--
-- == VK_EXT_primitive_topology_list_restart
--
-- [__Name String__]
--     @VK_EXT_primitive_topology_list_restart@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     357
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Shahbaz Youssefi
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_primitive_topology_list_restart] @syoussefi%0A*Here describe the issue or question you have about the VK_EXT_primitive_topology_list_restart extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-01-11
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Courtney Goeltzenleuchter, Google
--
--     -   Shahbaz Youssefi, Google
--
-- == Description
--
-- This extension allows list primitives to use the primitive restart index
-- value. This provides a more efficient implementation when layering
-- OpenGL functionality on Vulkan by avoiding emulation which incurs data
-- copies.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_EXTENSION_NAME'
--
-- -   'EXT_PRIMITIVE_TOPOLOGY_LIST_RESTART_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_TOPOLOGY_LIST_RESTART_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 0, 2020-09-14 (Courtney Goeltzenleuchter)
--
--     -   Internal revisions
--
-- -   Revision 1, 2021-01-11 (Shahbaz Youssefi)
--
--     -   Add the @primitiveTopologyPatchListRestart@ feature
--
--     -   Internal revisions
--
-- == See Also
--
-- 'PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_primitive_topology_list_restart Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_primitive_topology_list_restart  (PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT

instance ToCStruct PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT
instance Show PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT

instance FromCStruct PhysicalDevicePrimitiveTopologyListRestartFeaturesEXT

