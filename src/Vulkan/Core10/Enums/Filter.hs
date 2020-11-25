{-# language CPP #-}
-- No documentation found for Chapter "Filter"
module Vulkan.Core10.Enums.Filter  (Filter( FILTER_NEAREST
                                          , FILTER_LINEAR
                                          , FILTER_CUBIC_IMG
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
-- No documentation found for TopLevel "VkFilter"
newtype Filter = Filter Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFilter" "VK_FILTER_NEAREST"
pattern FILTER_NEAREST   = Filter 0
-- No documentation found for Nested "VkFilter" "VK_FILTER_LINEAR"
pattern FILTER_LINEAR    = Filter 1
-- No documentation found for Nested "VkFilter" "VK_FILTER_CUBIC_IMG"
pattern FILTER_CUBIC_IMG = Filter 1000015000
{-# complete FILTER_NEAREST,
             FILTER_LINEAR,
             FILTER_CUBIC_IMG :: Filter #-}

conNameFilter :: String
conNameFilter = "Filter"

enumPrefixFilter :: String
enumPrefixFilter = "FILTER_"

showTableFilter :: [(Filter, String)]
showTableFilter = [(FILTER_NEAREST, "NEAREST"), (FILTER_LINEAR, "LINEAR"), (FILTER_CUBIC_IMG, "CUBIC_IMG")]


instance Show Filter where
showsPrec = enumShowsPrec enumPrefixFilter showTableFilter conNameFilter (\(Filter x) -> x) (showsPrec 11)


instance Read Filter where
  readPrec = enumReadPrec enumPrefixFilter showTableFilter conNameFilter Filter

