{-# language CPP #-}
-- | = Name
--
-- VK_EXT_display_surface_counter - instance extension
--
-- == VK_EXT_display_surface_counter
--
-- [__Name String__]
--     @VK_EXT_display_surface_counter@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     91
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_display_surface_counter] @cubanismo%0A<<Here describe the issue or question you have about the VK_EXT_display_surface_counter extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-13
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Pierre Boudier, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Damien Leone, NVIDIA
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Daniel Vetter, Intel
--
-- == Description
--
-- This extension defines a vertical blanking period counter associated
-- with display surfaces. It provides a mechanism to query support for such
-- a counter from a 'Vulkan.Extensions.Handles.SurfaceKHR' object.
--
-- == New Commands
--
-- -   'getPhysicalDeviceSurfaceCapabilities2EXT'
--
-- == New Structures
--
-- -   'SurfaceCapabilities2EXT'
--
-- == New Enums
--
-- -   'SurfaceCounterFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'SurfaceCounterFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DISPLAY_SURFACE_COUNTER_EXTENSION_NAME'
--
-- -   'EXT_DISPLAY_SURFACE_COUNTER_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_SURFACE_CAPABILITIES2_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT'
--
-- == Version History
--
-- -   Revision 1, 2016-12-13 (James Jones)
--
--     -   Initial draft
--
-- = See Also
--
-- 'SurfaceCapabilities2EXT', 'SurfaceCounterFlagBitsEXT',
-- 'SurfaceCounterFlagsEXT', 'getPhysicalDeviceSurfaceCapabilities2EXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_display_surface_counter Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_display_surface_counter  ( SurfaceCapabilities2EXT
                                                         , SurfaceCounterFlagsEXT
                                                         , SurfaceCounterFlagBitsEXT
                                                         ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data SurfaceCapabilities2EXT

instance ToCStruct SurfaceCapabilities2EXT
instance Show SurfaceCapabilities2EXT

instance FromCStruct SurfaceCapabilities2EXT


type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT

data SurfaceCounterFlagBitsEXT

