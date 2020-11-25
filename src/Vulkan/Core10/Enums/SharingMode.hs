{-# language CPP #-}
-- No documentation found for Chapter "SharingMode"
module Vulkan.Core10.Enums.SharingMode  (SharingMode( SHARING_MODE_EXCLUSIVE
                                                    , SHARING_MODE_CONCURRENT
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
-- No documentation found for TopLevel "VkSharingMode"
newtype SharingMode = SharingMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_EXCLUSIVE"
pattern SHARING_MODE_EXCLUSIVE  = SharingMode 0
-- No documentation found for Nested "VkSharingMode" "VK_SHARING_MODE_CONCURRENT"
pattern SHARING_MODE_CONCURRENT = SharingMode 1
{-# complete SHARING_MODE_EXCLUSIVE,
             SHARING_MODE_CONCURRENT :: SharingMode #-}

conNameSharingMode :: String
conNameSharingMode = "SharingMode"

enumPrefixSharingMode :: String
enumPrefixSharingMode = "SHARING_MODE_"

showTableSharingMode :: [(SharingMode, String)]
showTableSharingMode = [(SHARING_MODE_EXCLUSIVE, "EXCLUSIVE"), (SHARING_MODE_CONCURRENT, "CONCURRENT")]


instance Show SharingMode where
showsPrec =
  enumShowsPrec enumPrefixSharingMode showTableSharingMode conNameSharingMode (\(SharingMode x) -> x) (showsPrec 11)


instance Read SharingMode where
  readPrec = enumReadPrec enumPrefixSharingMode showTableSharingMode conNameSharingMode SharingMode

