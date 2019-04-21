{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_fill_rectangle
  ( pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME
  , pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION
  , pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Pipeline
  ( VkPolygonMode(..)
  )


-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_EXTENSION_NAME"
pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"

-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_SPEC_VERSION"
pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION :: Integral a => a
pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION = 1

-- | 'VK_POLYGON_MODE_FILL_RECTANGLE_NV' specifies that polygons are rendered
-- using polygon rasterization rules, modified to consider a sample within
-- the primitive if the sample location is inside the axis-aligned bounding
-- box of the triangle after projection. Note that the barycentric weights
-- used in attribute interpolation /can/ extend outside the range [0,1]
-- when these primitives are shaded. Special treatment is given to a sample
-- position on the boundary edge of the bounding box. In such a case, if
-- two rectangles lie on either side of a common edge (with identical
-- endpoints) on which a sample position lies, then exactly one of the
-- triangles /must/ produce a fragment that covers that sample during
-- rasterization.
--
-- Polygons rendered in 'VK_POLYGON_MODE_FILL_RECTANGLE_NV' mode /may/ be
-- clipped by the frustum or by user clip planes. If clipping is applied,
-- the triangle is culled rather than clipped.
--
-- Area calculation and facingness are determined for
-- 'VK_POLYGON_MODE_FILL_RECTANGLE_NV' mode using the triangle’s vertices.
pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV :: VkPolygonMode
pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV = VkPolygonMode 1000153000
