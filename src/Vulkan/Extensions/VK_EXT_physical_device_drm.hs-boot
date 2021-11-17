{-# language CPP #-}
-- | = Name
--
-- VK_EXT_physical_device_drm - device extension
--
-- == VK_EXT_physical_device_drm
--
-- [__Name String__]
--     @VK_EXT_physical_device_drm@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     354
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Simon Ser
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_physical_device_drm] @emersion%0A<<Here describe the issue or question you have about the VK_EXT_physical_device_drm extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-06-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Simon Ser
--
-- == Description
--
-- This extension provides new facilities to query DRM properties for
-- physical devices, enabling users to match Vulkan physical devices with
-- DRM nodes on Linux.
--
-- Its functionality closely overlaps with
-- @EGL_EXT_device_drm@<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_physical_device_drm-fn1 1>^.
-- Unlike the EGL extension, this extension does not expose a string
-- containing the name of the device file and instead exposes device minor
-- numbers.
--
-- DRM defines multiple device node types. Each physical device may have
-- one primary node and one render node associated. Physical devices may
-- have no primary node (e.g. if the device does not have a display
-- subsystem), may have no render node (e.g. if it is a software rendering
-- engine), or may have neither (e.g. if it is a software rendering engine
-- without a display subsystem).
--
-- To query DRM properties for a physical device, chain
-- 'PhysicalDeviceDrmPropertiesEXT' to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceDrmPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PHYSICAL_DEVICE_DRM_EXTENSION_NAME'
--
-- -   'EXT_PHYSICAL_DEVICE_DRM_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DRM_PROPERTIES_EXT'
--
-- == References
--
-- 1.  #VK_EXT_physical_device_drm-fn1#
--     <https://www.khronos.org/registry/EGL/extensions/EXT/EGL_EXT_device_drm.txt EGL_EXT_device_drm>
--
-- == Version History
--
-- -   Revision 1, 2021-06-09
--
--     -   First stable revision
--
-- == See Also
--
-- 'PhysicalDeviceDrmPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_physical_device_drm Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_physical_device_drm  (PhysicalDeviceDrmPropertiesEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceDrmPropertiesEXT

instance ToCStruct PhysicalDeviceDrmPropertiesEXT
instance Show PhysicalDeviceDrmPropertiesEXT

instance FromCStruct PhysicalDeviceDrmPropertiesEXT

