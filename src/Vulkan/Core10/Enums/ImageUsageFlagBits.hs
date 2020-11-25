{-# language CPP #-}
-- No documentation found for Chapter "ImageUsageFlagBits"
module Vulkan.Core10.Enums.ImageUsageFlagBits  ( ImageUsageFlags
                                               , ImageUsageFlagBits( IMAGE_USAGE_TRANSFER_SRC_BIT
                                                                   , IMAGE_USAGE_TRANSFER_DST_BIT
                                                                   , IMAGE_USAGE_SAMPLED_BIT
                                                                   , IMAGE_USAGE_STORAGE_BIT
                                                                   , IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_INPUT_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
                                                                   , IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
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
type ImageUsageFlags = ImageUsageFlagBits

-- No documentation found for TopLevel "VkImageUsageFlagBits"
newtype ImageUsageFlagBits = ImageUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
pattern IMAGE_USAGE_TRANSFER_SRC_BIT             = ImageUsageFlagBits 0x00000001
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
pattern IMAGE_USAGE_TRANSFER_DST_BIT             = ImageUsageFlagBits 0x00000002
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SAMPLED_BIT"
pattern IMAGE_USAGE_SAMPLED_BIT                  = ImageUsageFlagBits 0x00000004
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_STORAGE_BIT"
pattern IMAGE_USAGE_STORAGE_BIT                  = ImageUsageFlagBits 0x00000008
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT         = ImageUsageFlagBits 0x00000010
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000020
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT     = ImageUsageFlagBits 0x00000040
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT         = ImageUsageFlagBits 0x00000080
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = ImageUsageFlagBits 0x00000200
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV    = ImageUsageFlagBits 0x00000100

conNameImageUsageFlagBits :: String
conNameImageUsageFlagBits = "ImageUsageFlagBits"

enumPrefixImageUsageFlagBits :: String
enumPrefixImageUsageFlagBits = "IMAGE_USAGE_"

showTableImageUsageFlagBits :: [(ImageUsageFlagBits, String)]
showTableImageUsageFlagBits =
  [ (IMAGE_USAGE_TRANSFER_SRC_BIT            , "TRANSFER_SRC_BIT")
  , (IMAGE_USAGE_TRANSFER_DST_BIT            , "TRANSFER_DST_BIT")
  , (IMAGE_USAGE_SAMPLED_BIT                 , "SAMPLED_BIT")
  , (IMAGE_USAGE_STORAGE_BIT                 , "STORAGE_BIT")
  , (IMAGE_USAGE_COLOR_ATTACHMENT_BIT        , "COLOR_ATTACHMENT_BIT")
  , (IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, "DEPTH_STENCIL_ATTACHMENT_BIT")
  , (IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT    , "TRANSIENT_ATTACHMENT_BIT")
  , (IMAGE_USAGE_INPUT_ATTACHMENT_BIT        , "INPUT_ATTACHMENT_BIT")
  , (IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT, "FRAGMENT_DENSITY_MAP_BIT_EXT")
  , (IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV   , "SHADING_RATE_IMAGE_BIT_NV")
  ]


instance Show ImageUsageFlagBits where
showsPrec = enumShowsPrec enumPrefixImageUsageFlagBits
                          showTableImageUsageFlagBits
                          conNameImageUsageFlagBits
                          (\(ImageUsageFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ImageUsageFlagBits where
  readPrec =
    enumReadPrec enumPrefixImageUsageFlagBits showTableImageUsageFlagBits conNameImageUsageFlagBits ImageUsageFlagBits

