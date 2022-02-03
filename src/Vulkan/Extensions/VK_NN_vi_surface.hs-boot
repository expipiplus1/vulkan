{-# language CPP #-}
-- | = Name
--
-- VK_NN_vi_surface - instance extension
--
-- == VK_NN_vi_surface
--
-- [__Name String__]
--     @VK_NN_vi_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     63
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
--     -   Mathias Heyer <<data:image/png;base64, GitLab>>mheyer
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-12-02
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Mathias Heyer, NVIDIA
--
--     -   Michael Chock, NVIDIA
--
--     -   Yasuhiro Yoshioka, Nintendo
--
--     -   Daniel Koch, NVIDIA
--
-- == Description
--
-- The @VK_NN_vi_surface@ extension is an instance extension. It provides a
-- mechanism to create a 'Vulkan.Extensions.Handles.SurfaceKHR' object
-- (defined by the @VK_KHR_surface@ extension) associated with an
-- @nn@::@vi@::@Layer@.
--
-- == New Commands
--
-- -   'createViSurfaceNN'
--
-- == New Structures
--
-- -   'ViSurfaceCreateInfoNN'
--
-- == New Bitmasks
--
-- -   'ViSurfaceCreateFlagsNN'
--
-- == New Enum Constants
--
-- -   'NN_VI_SURFACE_EXTENSION_NAME'
--
-- -   'NN_VI_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN'
--
-- == Issues
--
-- 1) Does VI need a way to query for compatibility between a particular
-- physical device (and queue family?) and a specific VI display?
--
-- __RESOLVED__: No. It is currently always assumed that the device and
-- display will always be compatible.
--
-- 2) 'ViSurfaceCreateInfoNN'::@pWindow@ is intended to store an
-- @nn@::@vi@::@NativeWindowHandle@, but its declared type is a bare
-- @void*@ to store the window handle. Why the discrepancy?
--
-- __RESOLVED__: It is for C compatibility. The definition for the VI
-- native window handle type is defined inside the @nn@::@vi@ C++
-- namespace. This prevents its use in C source files.
-- @nn@::@vi@::@NativeWindowHandle@ is always defined to be @void*@, so
-- this extension uses @void*@ to match.
--
-- == Version History
--
-- -   Revision 1, 2016-12-2 (Michael Chock)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'ViSurfaceCreateFlagsNN', 'ViSurfaceCreateInfoNN', 'createViSurfaceNN'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NN_vi_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NN_vi_surface  (ViSurfaceCreateInfoNN) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data ViSurfaceCreateInfoNN

instance ToCStruct ViSurfaceCreateInfoNN
instance Show ViSurfaceCreateInfoNN

instance FromCStruct ViSurfaceCreateInfoNN

