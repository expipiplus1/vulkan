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
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkPolygonMode"
newtype PolygonMode = PolygonMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_FILL"
pattern POLYGON_MODE_FILL              = PolygonMode 0
-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_LINE"
pattern POLYGON_MODE_LINE              = PolygonMode 1
-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_POINT"
pattern POLYGON_MODE_POINT             = PolygonMode 2
-- No documentation found for Nested "VkPolygonMode" "VK_POLYGON_MODE_FILL_RECTANGLE_NV"
pattern POLYGON_MODE_FILL_RECTANGLE_NV = PolygonMode 1000153000
{-# complete POLYGON_MODE_FILL,
             POLYGON_MODE_LINE,
             POLYGON_MODE_POINT,
             POLYGON_MODE_FILL_RECTANGLE_NV :: PolygonMode #-}

conNamePolygonMode :: String
conNamePolygonMode = "PolygonMode"

enumPrefixPolygonMode :: String
enumPrefixPolygonMode = "POLYGON_MODE_"

showTablePolygonMode :: [(PolygonMode, String)]
showTablePolygonMode =
  [ (POLYGON_MODE_FILL             , "FILL")
  , (POLYGON_MODE_LINE             , "LINE")
  , (POLYGON_MODE_POINT            , "POINT")
  , (POLYGON_MODE_FILL_RECTANGLE_NV, "FILL_RECTANGLE_NV")
  ]


instance Show PolygonMode where
showsPrec =
  enumShowsPrec enumPrefixPolygonMode showTablePolygonMode conNamePolygonMode (\(PolygonMode x) -> x) (showsPrec 11)


instance Read PolygonMode where
  readPrec = enumReadPrec enumPrefixPolygonMode showTablePolygonMode conNamePolygonMode PolygonMode

