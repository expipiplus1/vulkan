{-# language CPP #-}
-- No documentation found for Chapter "SwapchainUsageFlagBits"
module OpenXR.Core10.Enums.SwapchainUsageFlagBits  ( SwapchainUsageFlags
                                                   , SwapchainUsageFlagBits( SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT
                                                                           , SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                           , SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT
                                                                           , SWAPCHAIN_USAGE_TRANSFER_SRC_BIT
                                                                           , SWAPCHAIN_USAGE_TRANSFER_DST_BIT
                                                                           , SWAPCHAIN_USAGE_SAMPLED_BIT
                                                                           , SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT
                                                                           , SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND
                                                                           , ..
                                                                           )
                                                   ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
type SwapchainUsageFlags = SwapchainUsageFlagBits

-- No documentation found for TopLevel "XrSwapchainUsageFlagBits"
newtype SwapchainUsageFlagBits = SwapchainUsageFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT"
pattern SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT         = SwapchainUsageFlagBits 0x0000000000000001
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = SwapchainUsageFlagBits 0x0000000000000002
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT"
pattern SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT         = SwapchainUsageFlagBits 0x0000000000000004
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_TRANSFER_SRC_BIT"
pattern SWAPCHAIN_USAGE_TRANSFER_SRC_BIT             = SwapchainUsageFlagBits 0x0000000000000008
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_TRANSFER_DST_BIT"
pattern SWAPCHAIN_USAGE_TRANSFER_DST_BIT             = SwapchainUsageFlagBits 0x0000000000000010
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_SAMPLED_BIT"
pattern SWAPCHAIN_USAGE_SAMPLED_BIT                  = SwapchainUsageFlagBits 0x0000000000000020
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT"
pattern SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT           = SwapchainUsageFlagBits 0x0000000000000040
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND"
pattern SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND     = SwapchainUsageFlagBits 0x0000000000000080

conNameSwapchainUsageFlagBits :: String
conNameSwapchainUsageFlagBits = "SwapchainUsageFlagBits"

enumPrefixSwapchainUsageFlagBits :: String
enumPrefixSwapchainUsageFlagBits = "SWAPCHAIN_USAGE_"

showTableSwapchainUsageFlagBits :: [(SwapchainUsageFlagBits, String)]
showTableSwapchainUsageFlagBits =
  [ (SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT        , "COLOR_ATTACHMENT_BIT")
  , (SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, "DEPTH_STENCIL_ATTACHMENT_BIT")
  , (SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT        , "UNORDERED_ACCESS_BIT")
  , (SWAPCHAIN_USAGE_TRANSFER_SRC_BIT            , "TRANSFER_SRC_BIT")
  , (SWAPCHAIN_USAGE_TRANSFER_DST_BIT            , "TRANSFER_DST_BIT")
  , (SWAPCHAIN_USAGE_SAMPLED_BIT                 , "SAMPLED_BIT")
  , (SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT          , "MUTABLE_FORMAT_BIT")
  , (SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND    , "INPUT_ATTACHMENT_BIT_MND")
  ]

instance Show SwapchainUsageFlagBits where
  showsPrec = enumShowsPrec enumPrefixSwapchainUsageFlagBits
                            showTableSwapchainUsageFlagBits
                            conNameSwapchainUsageFlagBits
                            (\(SwapchainUsageFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SwapchainUsageFlagBits where
  readPrec = enumReadPrec enumPrefixSwapchainUsageFlagBits
                          showTableSwapchainUsageFlagBits
                          conNameSwapchainUsageFlagBits
                          SwapchainUsageFlagBits

