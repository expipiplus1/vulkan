{-# language CPP #-}
-- | = Name
--
-- VK_EXT_metal_surface - instance extension
--
-- == VK_EXT_metal_surface
--
-- [__Name String__]
--     @VK_EXT_metal_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     218
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
--     -   Dzmitry Malyshau
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_metal_surface] @kvark%0A<<Here describe the issue or question you have about the VK_EXT_metal_surface extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-10-01
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Dzmitry Malyshau, Mozilla Corp.
--
-- == Description
--
-- The @VK_EXT_metal_surface@ extension is an instance extension. It
-- provides a mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR'
-- object (defined by the @VK_KHR_surface@ extension) from 'CAMetalLayer',
-- which is the native rendering surface of Apple’s Metal framework.
--
-- == New Base Types
--
-- -   'CAMetalLayer'
--
-- == New Commands
--
-- -   'createMetalSurfaceEXT'
--
-- == New Structures
--
-- -   'MetalSurfaceCreateInfoEXT'
--
-- == New Bitmasks
--
-- -   'MetalSurfaceCreateFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_METAL_SURFACE_EXTENSION_NAME'
--
-- -   'EXT_METAL_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 1, 2018-10-01 (Dzmitry Malyshau)
--
--     -   Initial version
--
-- = See Also
--
-- 'CAMetalLayer', 'MetalSurfaceCreateFlagsEXT',
-- 'MetalSurfaceCreateInfoEXT', 'createMetalSurfaceEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_metal_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_metal_surface  (MetalSurfaceCreateInfoEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data MetalSurfaceCreateInfoEXT

instance ToCStruct MetalSurfaceCreateInfoEXT
instance Show MetalSurfaceCreateInfoEXT

instance FromCStruct MetalSurfaceCreateInfoEXT

