{-# language CPP #-}
-- | = Name
--
-- VK_KHR_android_surface - instance extension
--
-- == VK_KHR_android_surface
--
-- [__Name String__]
--     @VK_KHR_android_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     9
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_android_surface] @critsec%0A<<Here describe the issue or question you have about the VK_KHR_android_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-01-14
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
-- The @VK_KHR_android_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to an
-- 'ANativeWindow', Android’s native surface type. The 'ANativeWindow'
-- represents the producer endpoint of any buffer queue, regardless of
-- consumer endpoint. Common consumer endpoints for @ANativeWindows@ are
-- the system window compositor, video encoders, and application-specific
-- compositors importing the images through a @SurfaceTexture@.
--
-- == New Base Types
--
-- -   'ANativeWindow'
--
-- == New Commands
--
-- -   'createAndroidSurfaceKHR'
--
-- == New Structures
--
-- -   'AndroidSurfaceCreateInfoKHR'
--
-- == New Bitmasks
--
-- -   'AndroidSurfaceCreateFlagsKHR'
--
-- == New Enum Constants
--
-- -   'KHR_ANDROID_SURFACE_EXTENSION_NAME'
--
-- -   'KHR_ANDROID_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR'
--
-- == Issues
--
-- 1) Does Android need a way to query for compatibility between a
-- particular physical device (and queue family?) and a specific Android
-- display?
--
-- __RESOLVED__: No. Currently on Android, any physical device is expected
-- to be able to present to the system compositor, and all queue families
-- must support the necessary image layout transitions and synchronization
-- operations.
--
-- == Version History
--
-- -   Revision 1, 2015-09-23 (Jesse Hall)
--
--     -   Initial draft.
--
-- -   Revision 2, 2015-10-26 (Ian Elliott)
--
--     -   Renamed from VK_EXT_KHR_android_surface to
--         VK_KHR_android_surface.
--
-- -   Revision 3, 2015-11-03 (Daniel Rakos)
--
--     -   Added allocation callbacks to surface creation function.
--
-- -   Revision 4, 2015-11-10 (Jesse Hall)
--
--     -   Removed VK_ERROR_INVALID_ANDROID_WINDOW_KHR.
--
-- -   Revision 5, 2015-11-28 (Daniel Rakos)
--
--     -   Updated the surface create function to take a pCreateInfo
--         structure.
--
-- -   Revision 6, 2016-01-14 (James Jones)
--
--     -   Moved VK_ERROR_NATIVE_WINDOW_IN_USE_KHR from the
--         VK_KHR_android_surface to the VK_KHR_surface extension.
--
-- == See Also
--
-- 'ANativeWindow', 'AndroidSurfaceCreateFlagsKHR',
-- 'AndroidSurfaceCreateInfoKHR', 'createAndroidSurfaceKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_android_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_android_surface  (AndroidSurfaceCreateInfoKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data AndroidSurfaceCreateInfoKHR

instance ToCStruct AndroidSurfaceCreateInfoKHR
instance Show AndroidSurfaceCreateInfoKHR

instance FromCStruct AndroidSurfaceCreateInfoKHR

