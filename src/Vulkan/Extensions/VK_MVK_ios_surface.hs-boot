{-# language CPP #-}
-- | = Name
--
-- VK_MVK_ios_surface - instance extension
--
-- == VK_MVK_ios_surface
--
-- [__Name String__]
--     @VK_MVK_ios_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     123
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_surface@
--
-- [__Deprecation state__]
--
--     -   /Deprecated/ by @VK_EXT_metal_surface@ extension
--
-- [__Contact__]
--
--     -   Bill Hollings
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_MVK_ios_surface:%20&body=@billhollings%20 >
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
-- The @VK_MVK_ios_surface@ extension is an instance extension. It provides
-- a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR' object
-- (defined by the @VK_KHR_surface@ extension) based on a @UIView@, the
-- native surface type of iOS, which is underpinned by a
-- 'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer', to support
-- rendering to the surface using Apple’s Metal framework.
--
-- == Deprecation by @VK_EXT_metal_surface@
--
-- The @VK_MVK_ios_surface@ extension is considered deprecated and has been
-- superseded by the @VK_EXT_metal_surface@ extension.
--
-- == New Commands
--
-- -   'createIOSSurfaceMVK'
--
-- == New Structures
--
-- -   'IOSSurfaceCreateInfoMVK'
--
-- == New Bitmasks
--
-- -   'IOSSurfaceCreateFlagsMVK'
--
-- == New Enum Constants
--
-- -   'MVK_IOS_SURFACE_EXTENSION_NAME'
--
-- -   'MVK_IOS_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK'
--
-- == Version History
--
-- -   Revision 1, 2017-02-15 (Bill Hollings)
--
--     -   Initial draft.
--
-- -   Revision 2, 2017-02-24 (Bill Hollings)
--
--     -   Minor syntax fix to emphasize firm requirement for @UIView@ to
--         be backed by a
--         'Vulkan.Extensions.VK_EXT_metal_surface.CAMetalLayer'.
--
-- -   Revision 3, 2020-07-31 (Bill Hollings)
--
--     -   Update documentation on requirements for UIView.
--
--     -   Mark as deprecated by @VK_EXT_metal_surface@.
--
-- = See Also
--
-- 'IOSSurfaceCreateFlagsMVK', 'IOSSurfaceCreateInfoMVK',
-- 'createIOSSurfaceMVK'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_MVK_ios_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_MVK_ios_surface  (IOSSurfaceCreateInfoMVK) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data IOSSurfaceCreateInfoMVK

instance ToCStruct IOSSurfaceCreateInfoMVK
instance Show IOSSurfaceCreateInfoMVK

instance FromCStruct IOSSurfaceCreateInfoMVK

