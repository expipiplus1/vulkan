{-# language CPP #-}
-- | = Name
--
-- VK_QNX_screen_surface - instance extension
--
-- == VK_QNX_screen_surface
--
-- [__Name String__]
--     @VK_QNX_screen_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     379
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
--     -   Mike Gorchak
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QNX_screen_surface] @mgorchak-blackberry%0A<<Here describe the issue or question you have about the VK_QNX_screen_surface extension>> >
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
--     -   Mike Gorchak, BlackBerry Limited
--
-- == Description
--
-- The @VK_QNX_screen_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a QNX
-- Screen @window@, as well as a query to determine support for rendering
-- to a QNX Screen compositor.
--
-- == New Commands
--
-- -   'createScreenSurfaceQNX'
--
-- -   'getPhysicalDeviceScreenPresentationSupportQNX'
--
-- == New Structures
--
-- -   'ScreenSurfaceCreateInfoQNX'
--
-- == New Bitmasks
--
-- -   'ScreenSurfaceCreateFlagsQNX'
--
-- == New Enum Constants
--
-- -   'QNX_SCREEN_SURFACE_EXTENSION_NAME'
--
-- -   'QNX_SCREEN_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SCREEN_SURFACE_CREATE_INFO_QNX'
--
-- == Version History
--
-- -   Revision 1, 2021-01-11 (Mike Gorchak)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'ScreenSurfaceCreateFlagsQNX', 'ScreenSurfaceCreateInfoQNX',
-- 'createScreenSurfaceQNX',
-- 'getPhysicalDeviceScreenPresentationSupportQNX'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_QNX_screen_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QNX_screen_surface  ( ScreenSurfaceCreateInfoQNX
                                                , Screen_window
                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ScreenSurfaceCreateInfoQNX

instance ToCStruct ScreenSurfaceCreateInfoQNX
instance Show ScreenSurfaceCreateInfoQNX

instance FromCStruct ScreenSurfaceCreateInfoQNX


data Screen_window

