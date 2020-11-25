{-# language CPP #-}
-- No documentation found for Chapter "BorderColor"
module Vulkan.Core10.Enums.BorderColor  (BorderColor( BORDER_COLOR_FLOAT_TRANSPARENT_BLACK
                                                    , BORDER_COLOR_INT_TRANSPARENT_BLACK
                                                    , BORDER_COLOR_FLOAT_OPAQUE_BLACK
                                                    , BORDER_COLOR_INT_OPAQUE_BLACK
                                                    , BORDER_COLOR_FLOAT_OPAQUE_WHITE
                                                    , BORDER_COLOR_INT_OPAQUE_WHITE
                                                    , BORDER_COLOR_INT_CUSTOM_EXT
                                                    , BORDER_COLOR_FLOAT_CUSTOM_EXT
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
-- No documentation found for TopLevel "VkBorderColor"
newtype BorderColor = BorderColor Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_TRANSPARENT_BLACK"
pattern BORDER_COLOR_FLOAT_TRANSPARENT_BLACK = BorderColor 0
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_TRANSPARENT_BLACK"
pattern BORDER_COLOR_INT_TRANSPARENT_BLACK   = BorderColor 1
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_OPAQUE_BLACK"
pattern BORDER_COLOR_FLOAT_OPAQUE_BLACK      = BorderColor 2
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_OPAQUE_BLACK"
pattern BORDER_COLOR_INT_OPAQUE_BLACK        = BorderColor 3
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_OPAQUE_WHITE"
pattern BORDER_COLOR_FLOAT_OPAQUE_WHITE      = BorderColor 4
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_OPAQUE_WHITE"
pattern BORDER_COLOR_INT_OPAQUE_WHITE        = BorderColor 5
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_INT_CUSTOM_EXT"
pattern BORDER_COLOR_INT_CUSTOM_EXT          = BorderColor 1000287004
-- No documentation found for Nested "VkBorderColor" "VK_BORDER_COLOR_FLOAT_CUSTOM_EXT"
pattern BORDER_COLOR_FLOAT_CUSTOM_EXT        = BorderColor 1000287003
{-# complete BORDER_COLOR_FLOAT_TRANSPARENT_BLACK,
             BORDER_COLOR_INT_TRANSPARENT_BLACK,
             BORDER_COLOR_FLOAT_OPAQUE_BLACK,
             BORDER_COLOR_INT_OPAQUE_BLACK,
             BORDER_COLOR_FLOAT_OPAQUE_WHITE,
             BORDER_COLOR_INT_OPAQUE_WHITE,
             BORDER_COLOR_INT_CUSTOM_EXT,
             BORDER_COLOR_FLOAT_CUSTOM_EXT :: BorderColor #-}

conNameBorderColor :: String
conNameBorderColor = "BorderColor"

enumPrefixBorderColor :: String
enumPrefixBorderColor = "BORDER_COLOR_"

showTableBorderColor :: [(BorderColor, String)]
showTableBorderColor =
  [ (BORDER_COLOR_FLOAT_TRANSPARENT_BLACK, "FLOAT_TRANSPARENT_BLACK")
  , (BORDER_COLOR_INT_TRANSPARENT_BLACK  , "INT_TRANSPARENT_BLACK")
  , (BORDER_COLOR_FLOAT_OPAQUE_BLACK     , "FLOAT_OPAQUE_BLACK")
  , (BORDER_COLOR_INT_OPAQUE_BLACK       , "INT_OPAQUE_BLACK")
  , (BORDER_COLOR_FLOAT_OPAQUE_WHITE     , "FLOAT_OPAQUE_WHITE")
  , (BORDER_COLOR_INT_OPAQUE_WHITE       , "INT_OPAQUE_WHITE")
  , (BORDER_COLOR_INT_CUSTOM_EXT         , "INT_CUSTOM_EXT")
  , (BORDER_COLOR_FLOAT_CUSTOM_EXT       , "FLOAT_CUSTOM_EXT")
  ]


instance Show BorderColor where
showsPrec =
  enumShowsPrec enumPrefixBorderColor showTableBorderColor conNameBorderColor (\(BorderColor x) -> x) (showsPrec 11)


instance Read BorderColor where
  readPrec = enumReadPrec enumPrefixBorderColor showTableBorderColor conNameBorderColor BorderColor

