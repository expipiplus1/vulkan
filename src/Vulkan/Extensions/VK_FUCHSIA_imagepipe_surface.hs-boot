{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_imagepipe_surface - instance extension
--
-- == VK_FUCHSIA_imagepipe_surface
--
-- [__Name String__]
--     @VK_FUCHSIA_imagepipe_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     215
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
--     -   Craig Stout
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_FUCHSIA_imagepipe_surface:%20&body=@cdotstout%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-27
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Craig Stout, Google
--
--     -   Ian Elliott, Google
--
--     -   Jesse Hall, Google
--
-- == Description
--
-- The @VK_FUCHSIA_imagepipe_surface@ extension is an instance extension.
-- It provides a mechanism to create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object (defined by the
-- @VK_KHR_surface@ extension) that refers to a Fuchsia @imagePipeHandle@.
--
-- == New Commands
--
-- -   'createImagePipeSurfaceFUCHSIA'
--
-- == New Structures
--
-- -   'ImagePipeSurfaceCreateInfoFUCHSIA'
--
-- == New Bitmasks
--
-- -   'ImagePipeSurfaceCreateFlagsFUCHSIA'
--
-- == New Enum Constants
--
-- -   'FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME'
--
-- -   'FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA'
--
-- == Version History
--
-- -   Revision 1, 2018-07-27 (Craig Stout)
--
--     -   Initial draft.
--
-- = See Also
--
-- 'ImagePipeSurfaceCreateFlagsFUCHSIA',
-- 'ImagePipeSurfaceCreateInfoFUCHSIA', 'createImagePipeSurfaceFUCHSIA'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_FUCHSIA_imagepipe_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface  (ImagePipeSurfaceCreateInfoFUCHSIA) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ImagePipeSurfaceCreateInfoFUCHSIA

instance ToCStruct ImagePipeSurfaceCreateInfoFUCHSIA
instance Show ImagePipeSurfaceCreateInfoFUCHSIA

instance FromCStruct ImagePipeSurfaceCreateInfoFUCHSIA

