{-# language CPP #-}
-- | = Name
--
-- VK_EXT_headless_surface - instance extension
--
-- == VK_EXT_headless_surface
--
-- [__Name String__]
--     @VK_EXT_headless_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     257
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
--     -   Lisa Wu
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_headless_surface] @chengtianww%0A<<Here describe the issue or question you have about the VK_EXT_headless_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-03-21
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ray Smith, Arm
--
-- == Description
--
-- The @VK_EXT_headless_surface@ extension is an instance extension. It
-- provides a mechanism to create 'Vulkan.Extensions.Handles.SurfaceKHR'
-- objects independently of any window system or display device. The
-- presentation operation for a swapchain created from a headless surface
-- is by default a no-op, resulting in no externally-visible result.
--
-- Because there is no real presentation target, future extensions can
-- layer on top of the headless surface to introduce arbitrary or
-- customisable sets of restrictions or features. These could include
-- features like saving to a file or restrictions to emulate a particular
-- presentation target.
--
-- This functionality is expected to be useful for application and driver
-- development because it allows any platform to expose an arbitrary or
-- customisable set of restrictions and features of a presentation engine.
-- This makes it a useful portable test target for applications targeting a
-- wide range of presentation engines where the actual target presentation
-- engines might be scarce, unavailable or otherwise undesirable or
-- inconvenient to use for general Vulkan application development.
--
-- == New Commands
--
-- -   'createHeadlessSurfaceEXT'
--
-- == New Structures
--
-- -   'HeadlessSurfaceCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'HeadlessSurfaceCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_HEADLESS_SURFACE_EXTENSION_NAME'
--
-- -   'EXT_HEADLESS_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2019-03-21 (Ray Smith)
--
--     -   Initial draft
--
-- = See Also
--
-- 'HeadlessSurfaceCreateFlagsEXT', 'HeadlessSurfaceCreateInfoEXT',
-- 'createHeadlessSurfaceEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_headless_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_headless_surface  (HeadlessSurfaceCreateInfoEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data HeadlessSurfaceCreateInfoEXT

instance ToCStruct HeadlessSurfaceCreateInfoEXT
instance Show HeadlessSurfaceCreateInfoEXT

instance FromCStruct HeadlessSurfaceCreateInfoEXT

