{-# language CPP #-}
-- | = Name
--
-- VK_FUCHSIA_imagepipe_surface - instance extension
--
-- = Registered Extension Number
--
-- 215
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_surface@
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

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ImagePipeSurfaceCreateInfoFUCHSIA

instance ToCStruct ImagePipeSurfaceCreateInfoFUCHSIA
instance Show ImagePipeSurfaceCreateInfoFUCHSIA

instance FromCStruct ImagePipeSurfaceCreateInfoFUCHSIA

