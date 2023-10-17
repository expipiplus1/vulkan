{-# language CPP #-}
-- | = Name
--
-- VK_GGP_stream_descriptor_surface - instance extension
--
-- == VK_GGP_stream_descriptor_surface
--
-- [__Name String__]
--     @VK_GGP_stream_descriptor_surface@
--
-- [__Extension Type__]
--     Instance extension
--
-- [__Registered Extension Number__]
--     50
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--
-- [__Contact__]
--
--     -   Jean-Francois Roy
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_GGP_stream_descriptor_surface] @jfroy%0A*Here describe the issue or question you have about the VK_GGP_stream_descriptor_surface extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-28
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Jean-Francois Roy, Google
--
--     -   Brad Grantham, Google
--
--     -   Connor Smith, Google
--
--     -   Cort Stratton, Google
--
--     -   Hai Nguyen, Google
--
--     -   Ian Elliott, Google
--
--     -   Jesse Hall, Google
--
--     -   Jim Ray, Google
--
--     -   Katherine Wu, Google
--
--     -   Kaye Mason, Google
--
--     -   Kuangye Guo, Google
--
--     -   Mark Segal, Google
--
--     -   Nicholas Vining, Google
--
--     -   Paul Lalonde, Google
--
--     -   Richard O’Grady, Google
--
-- == Description
--
-- The @VK_GGP_stream_descriptor_surface@ extension is an instance
-- extension. It provides a mechanism to create a
-- 'Vulkan.Extensions.Handles.SurfaceKHR' object (defined by the
-- @VK_KHR_surface@ extension) that refers to a Google Games Platform
-- 'GgpStreamDescriptor'.
--
-- == New Commands
--
-- -   'createStreamDescriptorSurfaceGGP'
--
-- == New Structures
--
-- -   'StreamDescriptorSurfaceCreateInfoGGP'
--
-- == New Bitmasks
--
-- -   'StreamDescriptorSurfaceCreateFlagsGGP'
--
-- == New Enum Constants
--
-- -   'GGP_STREAM_DESCRIPTOR_SURFACE_EXTENSION_NAME'
--
-- -   'GGP_STREAM_DESCRIPTOR_SURFACE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP'
--
-- == Version History
--
-- -   Revision 1, 2018-11-26 (Jean-Francois Roy)
--
--     -   Initial revision.
--
-- == See Also
--
-- 'StreamDescriptorSurfaceCreateFlagsGGP',
-- 'StreamDescriptorSurfaceCreateInfoGGP',
-- 'createStreamDescriptorSurfaceGGP'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_GGP_stream_descriptor_surface Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_GGP_stream_descriptor_surface  (StreamDescriptorSurfaceCreateInfoGGP) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data StreamDescriptorSurfaceCreateInfoGGP

instance ToCStruct StreamDescriptorSurfaceCreateInfoGGP
instance Show StreamDescriptorSurfaceCreateInfoGGP

instance FromCStruct StreamDescriptorSurfaceCreateInfoGGP

