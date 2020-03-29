{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.PolygonMode  (PolygonMode( POLYGON_MODE_FILL
                                                             , POLYGON_MODE_LINE
                                                             , POLYGON_MODE_POINT
                                                             , POLYGON_MODE_FILL_RECTANGLE_NV
                                                             , ..
                                                             )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
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
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
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
{-# complete POLYGON_MODE_FILL,
             POLYGON_MODE_LINE,
             POLYGON_MODE_POINT,
             POLYGON_MODE_FILL_RECTANGLE_NV :: PolygonMode #-}

instance Show PolygonMode where
  showsPrec p = \case
    POLYGON_MODE_FILL -> showString "POLYGON_MODE_FILL"
    POLYGON_MODE_LINE -> showString "POLYGON_MODE_LINE"
    POLYGON_MODE_POINT -> showString "POLYGON_MODE_POINT"
    POLYGON_MODE_FILL_RECTANGLE_NV -> showString "POLYGON_MODE_FILL_RECTANGLE_NV"
    PolygonMode x -> showParen (p >= 11) (showString "PolygonMode " . showsPrec 11 x)

instance Read PolygonMode where
  readPrec = parens (choose [("POLYGON_MODE_FILL", pure POLYGON_MODE_FILL)
                            , ("POLYGON_MODE_LINE", pure POLYGON_MODE_LINE)
                            , ("POLYGON_MODE_POINT", pure POLYGON_MODE_POINT)
                            , ("POLYGON_MODE_FILL_RECTANGLE_NV", pure POLYGON_MODE_FILL_RECTANGLE_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "PolygonMode")
                       v <- step readPrec
                       pure (PolygonMode v)))

