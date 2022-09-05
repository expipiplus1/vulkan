{-# language CPP #-}
-- | = Name
--
-- VK_KHR_xcb_surface - instance extension
--
-- == VK_KHR_xcb_surface
--
-- [__Name String__]
--     @VK_KHR_xcb_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     6
--
-- [__Revision__]
--     6
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@ to be enabled
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_xcb_surface] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_xcb_surface extension>> >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_xcb_surface] @ianelliottus%0A<<Here describe the issue or question you have about the VK_KHR_xcb_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2015-11-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Patrick Doane, Blizzard
--
--     -   Jason Ekstrand, Intel
--
--     -   Ian Elliott, LunarG
--
--     -   Courtney Goeltzenleuchter, LunarG
--
--     -   Jesse Hall, Google
--
--     -   James Jones, NVIDIA
--
--     -   Antoine Labour, Google
--
--     -   Jon Leech, Khronos
--
--     -   David Mao, AMD
--
--     -   Norbert Nopper, Freescale
--
--     -   Alon Or-bach, Samsung
--
--     -   Daniel Rakos, AMD
--
--     -   Graham Sellers, AMD
--
--     -   Ray Smith, ARM
--
--     -   Jeff Vigil, Qualcomm
--
--     -   Chia-I Wu, LunarG
--
-- == Description
--
-- The @VK_KHR_xcb_surface@ extension is an instance extension. It provides
-- a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR' object
-- (defined by the @VK_KHR_surface@ extension) that refers to an X11
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.Window', using the XCB
-- client-side library, as well as a query to determine support for
-- rendering via XCB.
--
-- == New Commands
--
-- -   'createXcbSurfaceKHR'
--
-- -   'getPhysicalDeviceXcbPresentationSupportKHR'
--
-- == New Structures
--
-- -   'XcbSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'XcbSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_XCB_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_XCB_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does XCB need a way to query for compatibility between a particular
-- physical device and a specific screen? This would be a more general
-- query than
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR':
-- If it returned 'Vulkan.Core10.FundamentalTypes.TRUE', then the physical
-- device could be assumed to support presentation to any window on that
-- screen.
--
-- __RESOLVED__: Yes, this is needed for toolkits that want to create a
-- 'Vulkan.Core10.Handles.Device' before creating a window. To ensure the
-- query is reliable, it must be made against a particular X visual rather
-- than the screen in general.
--
-- == Version History
--
-- -   Revision 1, 2015-09-23 (Jesse Hall)
--
--     -   Initial draft, based on the previous contents of
--         VK_EXT_KHR_swapchain (later renamed VK_EXT_KHR_surface).
--
-- -   Revision 2, 2015-10-02 (James Jones)
--
--     -   Added presentation support query for an (xcb_connection_t*,
--         xcb_visualid_t) pair.
--
--     -   Removed “root” parameter from CreateXcbSurfaceKHR(), as it is
--         redundant when a window on the same screen is specified as well.
--
--     -   Adjusted wording of issue #1 and added agreed upon resolution.
--
-- -   Revision 3, 2015-10-14 (Ian Elliott)
--
--     -   Removed “root” parameter from CreateXcbSurfaceKHR() in one more
--         place.
--
-- -   Revision 4, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_xcb_surface to VK_KHR_xcb_surface.
--
-- -   Revision 5, 2015-10-23 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateXcbSurfaceKHR.
--
-- -   Revision 6, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- == See Also
--
-- 'XcbSurfaceCreateFlagsKHR', 'XcbSurfaceCreateInfoKHR',
-- 'createXcbSurfaceKHR', 'getPhysicalDeviceXcbPresentationSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_xcb_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_xcb_surface  ( XcbSurfaceCreateInfoKHR
                                             , Xcb_connection_t
                                             , Xcb_visualid_t
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Word (Word32)
import Data.Kind (Type)

data XcbSurfaceCreateInfoKHR

instance ToCStruct XcbSurfaceCreateInfoKHR
instance Show XcbSurfaceCreateInfoKHR

instance FromCStruct XcbSurfaceCreateInfoKHR


data Xcb_connection_t


type Xcb_visualid_t = Word32

