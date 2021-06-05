{-# language CPP #-}
-- | = Name
--
-- VK_KHR_xlib_surface - instance extension
--
-- == VK_KHR_xlib_surface
--
-- [__Name String__]
--     @VK_KHR_xlib_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     5
--
-- [__Revision__]
--     6
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Contact__]
--
--     -   Jesse Hall
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_xlib_surface:%20&body=@critsec%20 >
--
--     -   Ian Elliott
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_xlib_surface:%20&body=@ianelliottus%20 >
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
-- The @VK_KHR_xlib_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to an X11
-- 'Window', using the Xlib client-side library, as well as a query to
-- determine support for rendering via Xlib.
--
-- == New Commands
--
-- -   'createXlibSurfaceKHR'
--
-- -   'getPhysicalDeviceXlibPresentationSupportKHR'
--
-- == New Structures
--
-- -   'XlibSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'XlibSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_XLIB_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_XLIB_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does X11 need a way to query for compatibility between a particular
-- physical device and a specific screen? This would be a more general
-- query than
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR';
-- if it returned 'Vulkan.Core10.FundamentalTypes.TRUE', then the physical
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
--     -   Added presentation support query for (Display*, VisualID) pair.
--
--     -   Removed “root” parameter from CreateXlibSurfaceKHR(), as it is
--         redundant when a window on the same screen is specified as well.
--
--     -   Added appropriate X errors.
--
--     -   Adjusted wording of issue #1 and added agreed upon resolution.
--
-- -   Revision 3, 2015-10-14 (Ian Elliott)
--
--     -   Renamed this extension from VK_EXT_KHR_x11_surface to
--         VK_EXT_KHR_xlib_surface.
--
-- -   Revision 4, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_xlib_surface to VK_KHR_xlib_surface.
--
-- -   Revision 5, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to vkCreateXlibSurfaceKHR.
--
-- -   Revision 6, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- = See Also
--
-- 'XlibSurfaceCreateFlagsKHR', 'XlibSurfaceCreateInfoKHR',
-- 'createXlibSurfaceKHR', 'getPhysicalDeviceXlibPresentationSupportKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_xlib_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_xlib_surface  ( XlibSurfaceCreateInfoKHR
                                              , Display
                                              , VisualID
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.Kind (Type)

data XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR
instance Show XlibSurfaceCreateInfoKHR

instance FromCStruct XlibSurfaceCreateInfoKHR


type Display = Ptr ()


type VisualID = Word64

