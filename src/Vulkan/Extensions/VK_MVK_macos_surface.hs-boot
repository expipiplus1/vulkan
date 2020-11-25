{-# language CPP #-}
-- | = Name
--
-- VK_MVK_macos_surface - instance extension
--
-- = Registered Extension Number
--
-- 124
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_surface@
--
-- = Deprecation state
--
-- -   /Deprecated/ by @VK_EXT_metal_surface@ extension
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-07-31
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Bill Hollings, The Brenwill Workshop Ltd.
--
-- == Description
--
-- The @VK_MVK_macos_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) based on an @NSView@,
-- the native surface type of macOS, which is underpinned by a
-- 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer', to support
-- rendering to the surface using Apple’s Metal framework.
--
-- == Deprecation by @VK_EXT_metal_surface@
--
-- The @VK_MVK_macos_surface@ extension is considered deprecated and has
-- been superseded by the @VK_EXT_metal_surface@ extension.
--
-- == New Commands
--
-- -   'createMacOSSurfaceMVK'
--
-- == New Structures
--
-- -   'MacOSSurfaceCreateInfoMVK'
--
-- == New Bitmasks
--
-- -   'MacOSSurfaceCreateFlagsMVK'
--
-- == New Enum Constants
--
-- -   'MVK_MACOS_SURFACE_EXTENSION_NAME'
--
-- -   'MVK_MACOS_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK'
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Bill Hollings)
--
--     -   Initial draft.
--
-- -   Revision 2, 2017-02-24 (Bill Hollings)
--
--     -   Minor syntax fix to emphasize firm requirement for @NSView@ to
--         be backed by a
--         'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'.
--
-- -   Revision 3, 2020-07-31 (Bill Hollings)
--
--     -   Update documentation on requirements for @NSView@.
--
--     -   Mark as deprecated by @VK_EXT_metal_surface@.
--
-- = See Also
--
-- 'MacOSSurfaceCreateFlagsMVK', 'MacOSSurfaceCreateInfoMVK',
-- 'createMacOSSurfaceMVK'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MVK_macos_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MVK_macos_surface  (MacOSSurfaceCreateInfoMVK) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data MacOSSurfaceCreateInfoMVK

instance ToCStruct MacOSSurfaceCreateInfoMVK
instance Show MacOSSurfaceCreateInfoMVK

instance FromCStruct MacOSSurfaceCreateInfoMVK

