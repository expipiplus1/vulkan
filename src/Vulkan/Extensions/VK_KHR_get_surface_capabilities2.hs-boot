{-# language CPP #-}
-- | = Name
--
-- VK_KHR_get_surface_capabilities2 - instance extension
--
-- == VK_KHR_get_surface_capabilities2
--
-- [__Name String__]
--     @VK_KHR_get_surface_capabilities2@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     120
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   James Jones
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_get_surface_capabilities2] @cubanismo%0A<<Here describe the issue or question you have about the VK_KHR_get_surface_capabilities2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-02-27
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
--     -   Alon Or-bach, Samsung
--
-- == Description
--
-- This extension provides new entry points to query device surface
-- capabilities in a way that can be easily extended by other extensions,
-- without introducing any further entry points. This extension can be
-- considered the @VK_KHR_surface@ equivalent of the
-- @VK_KHR_get_physical_device_properties2@ extension.
--
-- == New Commands
--
-- -   'getPhysicalDeviceSurfaceCapabilities2KHR'
--
-- -   'getPhysicalDeviceSurfaceFormats2KHR'
--
-- == New Structures
--
-- -   'PhysicalDeviceSurfaceInfo2KHR'
--
-- -   'SurfaceCapabilities2KHR'
--
-- -   'SurfaceFormat2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_GET_SURFACE_CAPABILITIES_2_EXTENSION_NAME'
--
-- -   'KHR_GET_SURFACE_CAPABILITIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR'
--
-- == Issues
--
-- 1) What should this extension be named?
--
-- __RESOLVED__: @VK_KHR_get_surface_capabilities2@. Other alternatives:
--
-- -   @VK_KHR_surface2@
--
-- -   One extension, combining a separate display-specific query
--     extension.
--
-- 2) Should additional WSI query functions be extended?
--
-- __RESOLVED__:
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR':
--     Yes. The need for this motivated the extension.
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR':
--     No. Currently only has boolean output. Extensions should instead
--     extend 'getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR':
--     Yes.
--
-- -   'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR':
--     No. Recent discussion concluded this introduced too much variability
--     for applications to deal with. Extensions should instead extend
--     'getPhysicalDeviceSurfaceCapabilities2KHR'.
--
-- -   'Vulkan.Extensions.VK_KHR_xlib_surface.getPhysicalDeviceXlibPresentationSupportKHR':
--     Not in this extension.
--
-- -   'Vulkan.Extensions.VK_KHR_xcb_surface.getPhysicalDeviceXcbPresentationSupportKHR':
--     Not in this extension.
--
-- -   'Vulkan.Extensions.VK_KHR_wayland_surface.getPhysicalDeviceWaylandPresentationSupportKHR':
--     Not in this extension.
--
-- -   'Vulkan.Extensions.VK_KHR_win32_surface.getPhysicalDeviceWin32PresentationSupportKHR':
--     Not in this extension.
--
-- == Version History
--
-- -   Revision 1, 2017-02-27 (James Jones)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceSurfaceInfo2KHR', 'SurfaceCapabilities2KHR',
-- 'SurfaceFormat2KHR', 'getPhysicalDeviceSurfaceCapabilities2KHR',
-- 'getPhysicalDeviceSurfaceFormats2KHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_get_surface_capabilities2  ( PhysicalDeviceSurfaceInfo2KHR
                                                           , SurfaceCapabilities2KHR
                                                           , SurfaceFormat2KHR
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
type role PhysicalDeviceSurfaceInfo2KHR nominal
data PhysicalDeviceSurfaceInfo2KHR (es :: [Type])

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PokeChain es) => ToCStruct (PhysicalDeviceSurfaceInfo2KHR es)
instance Show (Chain es) => Show (PhysicalDeviceSurfaceInfo2KHR es)

instance (Extendss PhysicalDeviceSurfaceInfo2KHR es, PeekChain es) => FromCStruct (PhysicalDeviceSurfaceInfo2KHR es)


type role SurfaceCapabilities2KHR nominal
data SurfaceCapabilities2KHR (es :: [Type])

instance (Extendss SurfaceCapabilities2KHR es, PokeChain es) => ToCStruct (SurfaceCapabilities2KHR es)
instance Show (Chain es) => Show (SurfaceCapabilities2KHR es)

instance (Extendss SurfaceCapabilities2KHR es, PeekChain es) => FromCStruct (SurfaceCapabilities2KHR es)


data SurfaceFormat2KHR

instance ToCStruct SurfaceFormat2KHR
instance Show SurfaceFormat2KHR

instance FromCStruct SurfaceFormat2KHR

