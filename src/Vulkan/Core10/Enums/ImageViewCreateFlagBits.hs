{-# language CPP #-}
-- No documentation found for Chapter "ImageViewCreateFlagBits"
module Vulkan.Core10.Enums.ImageViewCreateFlagBits  ( ImageViewCreateFlags
                                                    , ImageViewCreateFlagBits( IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT
                                                                             , IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT
                                                                             , ..
                                                                             )
                                                    ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ImageViewCreateFlags = ImageViewCreateFlagBits

-- No documentation found for TopLevel "VkImageViewCreateFlagBits"
newtype ImageViewCreateFlagBits = ImageViewCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkImageViewCreateFlagBits" "VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT"
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT = ImageViewCreateFlagBits 0x00000002
-- No documentation found for Nested "VkImageViewCreateFlagBits" "VK_IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT"
pattern IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT  = ImageViewCreateFlagBits 0x00000001

conNameImageViewCreateFlagBits :: String
conNameImageViewCreateFlagBits = "ImageViewCreateFlagBits"

enumPrefixImageViewCreateFlagBits :: String
enumPrefixImageViewCreateFlagBits = "IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_D"

showTableImageViewCreateFlagBits :: [(ImageViewCreateFlagBits, String)]
showTableImageViewCreateFlagBits =
  [ (IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DEFERRED_BIT_EXT, "EFERRED_BIT_EXT")
  , (IMAGE_VIEW_CREATE_FRAGMENT_DENSITY_MAP_DYNAMIC_BIT_EXT , "YNAMIC_BIT_EXT")
  ]


instance Show ImageViewCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixImageViewCreateFlagBits
                          showTableImageViewCreateFlagBits
                          conNameImageViewCreateFlagBits
                          (\(ImageViewCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ImageViewCreateFlagBits where
  readPrec = enumReadPrec enumPrefixImageViewCreateFlagBits
                          showTableImageViewCreateFlagBits
                          conNameImageViewCreateFlagBits
                          ImageViewCreateFlagBits

