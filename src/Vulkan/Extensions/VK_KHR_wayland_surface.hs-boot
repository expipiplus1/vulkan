{-# language CPP #-}
-- | = Name
--
-- VK_KHR_wayland_surface - instance extension
--
-- == VK_KHR_wayland_surface
--
-- [__Name String__]
--     @VK_KHR_wayland_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     7
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_wayland_surface] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_wayland_surface extension>> >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_wayland_surface] @ianelliottus%0A<<Here describe the issue or question you have about the VK_KHR_wayland_surface extension>> >
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
-- The @VK_KHR_wayland_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a
-- Wayland @wl_surface@, as well as a query to determine support for
-- rendering to a Wayland compositor.
--
-- == New Commands
--
-- -   'createWaylandSurfaceKHR'
--
-- -   'getPhysicalDeviceWaylandPresentationSupportKHR'
--
-- == New Structures
--
-- -   'WaylandSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'WaylandSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_WAYLAND_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_WAYLAND_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does Wayland need a way to query for compatibility between a
-- particular physical device and a specific Wayland display? This would be
-- a more general query than
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR':
-- if the Wayland-specific query returned
-- 'Vulkan.Core10.FundamentalTypes.TRUE' for a
-- ('Vulkan.Core10.Handles.PhysicalDevice', @struct wl_display*@) pair,
-- then the physical device could be assumed to support presentation to any
-- 'Vulkan.Extensions.Handles.SurfaceKHR' for surfaces on the display.
--
-- __RESOLVED__: Yes. 'getPhysicalDeviceWaylandPresentationSupportKHR' was
-- added to address this issue.
--
-- 2) Should we require surfaces created with 'createWaylandSurfaceKHR' to
-- support the 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR'
-- present mode?
--
-- __RESOLVED__: Yes. Wayland is an inherently mailbox window system and
-- mailbox support is required for some Wayland compositor interactions to
-- work as expected. While handling these interactions may be possible with
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR', it is much
-- more difficult to do without deadlock and requiring all Wayland
-- applications to be able to support implementations which only support
-- 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' would be an
-- onerous restriction on application developers.
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
--     -   Added vkGetPhysicalDeviceWaylandPresentationSupportKHR() to
--         resolve issue #1.
--
--     -   Adjusted wording of issue #1 to match the agreed-upon solution.
--
--     -   Renamed “window” parameters to “surface” to match Wayland
--         conventions.
--
-- -   Revision 3, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_wayland_surface to
--         VK_KHR_wayland_surface.
--
-- -   Revision 4, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateWaylandSurfaceKHR.
--
-- -   Revision 5, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- -   Revision 6, 2017-02-08 (Jason Ekstrand)
--
--     -   Added the requirement that implementations support
--         'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR'.
--
--     -   Added wording about interactions between
--         'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' and the
--         Wayland requests sent to the compositor.
--
-- == See Also
--
-- 'WaylandSurfaceCreateFlagsKHR', 'WaylandSurfaceCreateInfoKHR',
-- 'createWaylandSurfaceKHR',
-- 'getPhysicalDeviceWaylandPresentationSupportKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_wayland_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_wayland_surface  ( WaylandSurfaceCreateInfoKHR
                                                 , Wl_display
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data WaylandSurfaceCreateInfoKHR

instance ToCStruct WaylandSurfaceCreateInfoKHR
instance Show WaylandSurfaceCreateInfoKHR

instance FromCStruct WaylandSurfaceCreateInfoKHR


data Wl_display

