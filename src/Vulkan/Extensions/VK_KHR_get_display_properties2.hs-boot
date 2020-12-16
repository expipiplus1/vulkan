{-# language CPP #-}
-- | = Name
--
-- VK_KHR_get_display_properties2 - instance extension
--
-- == VK_KHR_get_display_properties2
--
-- [__Name String__]
--     @VK_KHR_get_display_properties2@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     122
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_display@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_get_display_properties2:%20&body=@cubanismo%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   James Jones, NVIDIA
--
-- == Description
--
-- This extension provides new entry points to query device display
-- properties and capabilities in a way that can be easily extended by
-- other extensions, without introducing any further entry points. This
-- extension can be considered the @VK_KHR_display@ equivalent of the
-- @VK_KHR_get_physical_device_properties2@ extension.
--
-- == New Commands
--
-- -   'getDisplayModeProperties2KHR'
--
-- -   'getDisplayPlaneCapabilities2KHR'
--
-- -   'getPhysicalDeviceDisplayPlaneProperties2KHR'
--
-- -   'getPhysicalDeviceDisplayProperties2KHR'
--
-- == New Structures
--
-- -   'DisplayModeProperties2KHR'
--
-- -   'DisplayPlaneCapabilities2KHR'
--
-- -   'DisplayPlaneInfo2KHR'
--
-- -   'DisplayPlaneProperties2KHR'
--
-- -   'DisplayProperties2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_GET_DISPLAY_PROPERTIES_2_EXTENSION_NAME'
--
-- -   'KHR_GET_DISPLAY_PROPERTIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR'
--
-- == Issues
--
-- 1) What should this extension be named?
--
-- __RESOLVED__: @VK_KHR_get_display_properties2@. Other alternatives:
--
-- -   @VK_KHR_display2@
--
-- -   One extension, combined with @VK_KHR_surface_capabilites2@.
--
-- 2) Should extensible input structs be added for these new functions:
--
-- __RESOLVED__:
--
-- -   'getPhysicalDeviceDisplayProperties2KHR': No. The only current input
--     is a 'Vulkan.Core10.Handles.PhysicalDevice'. Other inputs wouldn’t
--     make sense.
--
-- -   'getPhysicalDeviceDisplayPlaneProperties2KHR': No. The only current
--     input is a 'Vulkan.Core10.Handles.PhysicalDevice'. Other inputs
--     wouldn’t make sense.
--
-- -   'getDisplayModeProperties2KHR': No. The only current inputs are a
--     'Vulkan.Core10.Handles.PhysicalDevice' and a
--     'Vulkan.Extensions.Handles.DisplayModeKHR'. Other inputs wouldn’t
--     make sense.
--
-- 3) Should additional display query functions be extended?
--
-- __RESOLVED__:
--
-- -   'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneSupportedDisplaysKHR':
--     No. Extensions should instead extend
--     'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR'().
--
-- == Version History
--
-- -   Revision 1, 2017-02-21 (James Jones)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'DisplayModeProperties2KHR', 'DisplayPlaneCapabilities2KHR',
-- 'DisplayPlaneInfo2KHR', 'DisplayPlaneProperties2KHR',
-- 'DisplayProperties2KHR', 'getDisplayModeProperties2KHR',
-- 'getDisplayPlaneCapabilities2KHR',
-- 'getPhysicalDeviceDisplayPlaneProperties2KHR',
-- 'getPhysicalDeviceDisplayProperties2KHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_display_properties2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_get_display_properties2  ( DisplayModeProperties2KHR
                                                         , DisplayPlaneCapabilities2KHR
                                                         , DisplayPlaneInfo2KHR
                                                         , DisplayPlaneProperties2KHR
                                                         , DisplayProperties2KHR
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DisplayModeProperties2KHR

instance ToCStruct DisplayModeProperties2KHR
instance Show DisplayModeProperties2KHR

instance FromCStruct DisplayModeProperties2KHR


data DisplayPlaneCapabilities2KHR

instance ToCStruct DisplayPlaneCapabilities2KHR
instance Show DisplayPlaneCapabilities2KHR

instance FromCStruct DisplayPlaneCapabilities2KHR


data DisplayPlaneInfo2KHR

instance ToCStruct DisplayPlaneInfo2KHR
instance Show DisplayPlaneInfo2KHR

instance FromCStruct DisplayPlaneInfo2KHR


data DisplayPlaneProperties2KHR

instance ToCStruct DisplayPlaneProperties2KHR
instance Show DisplayPlaneProperties2KHR

instance FromCStruct DisplayPlaneProperties2KHR


data DisplayProperties2KHR

instance ToCStruct DisplayProperties2KHR
instance Show DisplayProperties2KHR

instance FromCStruct DisplayProperties2KHR

