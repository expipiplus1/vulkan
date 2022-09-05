{-# language CPP #-}
-- | = Name
--
-- VK_NV_fill_rectangle - device extension
--
-- == VK_NV_fill_rectangle
--
-- [__Name String__]
--     @VK_NV_fill_rectangle@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     154
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_fill_rectangle] @jeffbolznv%0A<<Here describe the issue or question you have about the VK_NV_fill_rectangle extension>> >
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
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_fill_rectangle Vulkan Specification>
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

