{-# language CPP #-}
-- | = Name
--
-- VK_NV_fill_rectangle - device extension
--
-- = Registered Extension Number
--
-- 154
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-05-22
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension adds a new 'Vulkan.Core10.Enums.PolygonMode.PolygonMode'
-- @enum@ where a triangle is rasterized by computing and filling its
-- axis-aligned screen-space bounding box, disregarding the actual triangle
-- edges. This can be useful for drawing a rectangle without being split
-- into two triangles with an internal edge. It is also useful to minimize
-- the number of primitives that need to be drawn, particularly for a user
-- interface.
--
-- == New Enum Constants
--
-- -   'NV_FILL_RECTANGLE_EXTENSION_NAME'
--
-- -   'NV_FILL_RECTANGLE_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.PolygonMode.PolygonMode':
--
--     -   'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_FILL_RECTANGLE_NV'
--
-- == Version History
--
-- -   Revision 1, 2017-05-22 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_fill_rectangle Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_fill_rectangle  ( NV_FILL_RECTANGLE_SPEC_VERSION
                                               , pattern NV_FILL_RECTANGLE_SPEC_VERSION
                                               , NV_FILL_RECTANGLE_EXTENSION_NAME
                                               , pattern NV_FILL_RECTANGLE_EXTENSION_NAME
                                               ) where

import Data.String (IsString)

type NV_FILL_RECTANGLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_SPEC_VERSION"
pattern NV_FILL_RECTANGLE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FILL_RECTANGLE_SPEC_VERSION = 1


type NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"

-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_EXTENSION_NAME"
pattern NV_FILL_RECTANGLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"

