{-# language CPP #-}
-- | = Name
--
-- VK_EXT_primitive_restart_index - device extension
--
-- = VK_EXT_primitive_restart_index
--
-- [__Name String__]
--     @VK_EXT_primitive_restart_index@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     679
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
-- [__Special Use__]
--
--     -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#extendingvulkan-compatibility-specialuse OpenGL \/ ES support>
--
-- [__Contact__]
--
--     -   Mike Blumenkrantz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_primitive_restart_index] @zmike%0A*Here describe the issue or question you have about the VK_EXT_primitive_restart_index extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_primitive_restart_index.adoc VK_EXT_primitive_restart_index>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-10
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Spencer Fricke, LunarG
--
--     -   Ricardo Garcia, Igalia
--
--     -   Piers Daniell, NVIDIA
--
-- == Description
--
-- This extension allows setting a custom primitive restart index. It is
-- primarily intended to support GL emulation.
--
-- == New Commands
--
-- -   'cmdSetPrimitiveRestartIndexEXT'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePrimitiveRestartIndexFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRIMITIVE_RESTART_INDEX_EXTENSION_NAME'
--
-- -   'EXT_PRIMITIVE_RESTART_INDEX_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIMITIVE_RESTART_INDEX_FEATURES_EXT'
--
-- == Version History
--
-- -   Revision 1, 2026-03-10 (Mike Blumenkrantz)
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
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_primitive_restart_index Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_primitive_restart_index  (PhysicalDevicePrimitiveRestartIndexFeaturesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePrimitiveRestartIndexFeaturesEXT

instance ToCStruct PhysicalDevicePrimitiveRestartIndexFeaturesEXT
instance Show PhysicalDevicePrimitiveRestartIndexFeaturesEXT

instance FromCStruct PhysicalDevicePrimitiveRestartIndexFeaturesEXT

