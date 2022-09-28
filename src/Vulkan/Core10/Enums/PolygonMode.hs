{-# language CPP #-}
-- No documentation found for Chapter "PolygonMode"
module Vulkan.Core10.Enums.PolygonMode  (PolygonMode( POLYGON_MODE_FILL
                                                    , POLYGON_MODE_LINE
                                                    , POLYGON_MODE_POINT
                                                    , POLYGON_MODE_FILL_RECTANGLE_NV
                                                    , ..
                                                    )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkPolygonMode - Control polygon rasterization mode
--
-- = Description
--
-- These modes affect only the final rasterization of polygons: in
-- particular, a polygon’s vertices are shaded and the polygon is clipped
-- and possibly culled before these modes are applied.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
newtype PolygonMode = PolygonMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'POLYGON_MODE_FILL' specifies that polygons are rendered using the
-- polygon rasterization rules in this section.
pattern POLYGON_MODE_FILL = PolygonMode 0

-- | 'POLYGON_MODE_LINE' specifies that polygon edges are drawn as line
-- segments.
pattern POLYGON_MODE_LINE = PolygonMode 1

-- | 'POLYGON_MODE_POINT' specifies that polygon vertices are drawn as
-- points.
pattern POLYGON_MODE_POINT = PolygonMode 2

-- | 'POLYGON_MODE_FILL_RECTANGLE_NV' specifies that polygons are rendered
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
-- Polygons rendered in 'POLYGON_MODE_FILL_RECTANGLE_NV' mode /may/ be
-- clipped by the frustum or by user clip planes. If clipping is applied,
-- the triangle is culled rather than clipped.
--
-- Area calculation and facingness are determined for
-- 'POLYGON_MODE_FILL_RECTANGLE_NV' mode using the triangle’s vertices.
pattern POLYGON_MODE_FILL_RECTANGLE_NV = PolygonMode 1000153000

{-# COMPLETE
  POLYGON_MODE_FILL
  , POLYGON_MODE_LINE
  , POLYGON_MODE_POINT
  , POLYGON_MODE_FILL_RECTANGLE_NV ::
    PolygonMode
  #-}

conNamePolygonMode :: String
conNamePolygonMode = "PolygonMode"

enumPrefixPolygonMode :: String
enumPrefixPolygonMode = "POLYGON_MODE_"

showTablePolygonMode :: [(PolygonMode, String)]
showTablePolygonMode =
  [ (POLYGON_MODE_FILL, "FILL")
  , (POLYGON_MODE_LINE, "LINE")
  , (POLYGON_MODE_POINT, "POINT")
  , (POLYGON_MODE_FILL_RECTANGLE_NV, "FILL_RECTANGLE_NV")
  ]

instance Show PolygonMode where
  showsPrec =
    enumShowsPrec
      enumPrefixPolygonMode
      showTablePolygonMode
      conNamePolygonMode
      (\(PolygonMode x) -> x)
      (showsPrec 11)

instance Read PolygonMode where
  readPrec =
    enumReadPrec
      enumPrefixPolygonMode
      showTablePolygonMode
      conNamePolygonMode
      PolygonMode
