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
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--
-- [__Contact__]
--
--     -   Craig Stout
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_FUCHSIA_imagepipe_surface] @cdotstout%0A*Here describe the issue or question you have about the VK_FUCHSIA_imagepipe_surface extension* >
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
-- == See Also
--
-- 'ImagePipeSurfaceCreateFlagsFUCHSIA',
-- 'ImagePipeSurfaceCreateInfoFUCHSIA', 'createImagePipeSurfaceFUCHSIA'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_FUCHSIA_imagepipe_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface  ( ImagePipeSurfaceCreateInfoFUCHSIA
                                                       , Zx_handle_t
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Word (Word32)
import Data.Kind (Type)

data ImagePipeSurfaceCreateInfoFUCHSIA

instance ToCStruct ImagePipeSurfaceCreateInfoFUCHSIA
instance Show ImagePipeSurfaceCreateInfoFUCHSIA

instance FromCStruct ImagePipeSurfaceCreateInfoFUCHSIA


type Zx_handle_t = Word32

