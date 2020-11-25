{-# language CPP #-}
-- | = Name
--
-- VK_EXT_directfb_surface - instance extension
--
-- = Registered Extension Number
--
-- 347
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
--     2020-06-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Nicolas Caramelli
--
-- == Description
--
-- The @VK_EXT_directfb_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) that refers to a
-- DirectFB 'IDirectFBSurface', as well as a query to determine support for
-- rendering via DirectFB.
--
-- == New Commands
--
-- -   'createDirectFBSurfaceEXT'
--
-- -   'getPhysicalDeviceDirectFBPresentationSupportEXT'
--
-- == New Structures
--
-- -   'DirectFBSurfaceCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'DirectFBSurfaceCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_DIRECTFB_SURFACE_EXTENSION_NAME'
--
-- -   'EXT_DIRECTFB_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2020-06-16 (Nicolas Caramelli)
--
--     -   Initial version
--
-- = See Also
--
-- 'DirectFBSurfaceCreateFlagsEXT', 'DirectFBSurfaceCreateInfoEXT',
-- 'createDirectFBSurfaceEXT',
-- 'getPhysicalDeviceDirectFBPresentationSupportEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_directfb_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_directfb_surface  ( DirectFBSurfaceCreateInfoEXT
                                                  , IDirectFB
                                                  ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DirectFBSurfaceCreateInfoEXT

instance ToCStruct DirectFBSurfaceCreateInfoEXT
instance Show DirectFBSurfaceCreateInfoEXT

instance FromCStruct DirectFBSurfaceCreateInfoEXT


data IDirectFB

