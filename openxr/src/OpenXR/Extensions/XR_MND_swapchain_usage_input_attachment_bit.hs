{-# language CPP #-}
-- | = Name
--
-- XR_MND_swapchain_usage_input_attachment_bit - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MND_swapchain_usage_input_attachment_bit  XR_MND_swapchain_usage_input_attachment_bit>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 97
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MND_swapchain_usage_input_attachment_bit OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MND_swapchain_usage_input_attachment_bit  ( SwapchainUsageFlagBits( SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT
                                                                                              , SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                                              , SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT
                                                                                              , SWAPCHAIN_USAGE_TRANSFER_SRC_BIT
                                                                                              , SWAPCHAIN_USAGE_TRANSFER_DST_BIT
                                                                                              , SWAPCHAIN_USAGE_SAMPLED_BIT
                                                                                              , SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT
                                                                                              , SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND
                                                                                              , ..
                                                                                              )
                                                                      , MND_swapchain_usage_input_attachment_bit_SPEC_VERSION
                                                                      , pattern MND_swapchain_usage_input_attachment_bit_SPEC_VERSION
                                                                      , MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME
                                                                      , pattern MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME
                                                                      ) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Data.String (IsString)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
-- No documentation found for TopLevel "XrSwapchainUsageFlagBits"
newtype SwapchainUsageFlagBits = SwapchainUsageFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT"
pattern SWAPCHAIN_USAGE_COLOR_ATTACHMENT_BIT         = SwapchainUsageFlagBits 0x00000001
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern SWAPCHAIN_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = SwapchainUsageFlagBits 0x00000002
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT"
pattern SWAPCHAIN_USAGE_UNORDERED_ACCESS_BIT         = SwapchainUsageFlagBits 0x00000004
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_TRANSFER_SRC_BIT"
pattern SWAPCHAIN_USAGE_TRANSFER_SRC_BIT             = SwapchainUsageFlagBits 0x00000008
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_TRANSFER_DST_BIT"
pattern SWAPCHAIN_USAGE_TRANSFER_DST_BIT             = SwapchainUsageFlagBits 0x00000010
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_SAMPLED_BIT"
pattern SWAPCHAIN_USAGE_SAMPLED_BIT                  = SwapchainUsageFlagBits 0x00000020
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT"
pattern SWAPCHAIN_USAGE_MUTABLE_FORMAT_BIT           = SwapchainUsageFlagBits 0x00000040
-- No documentation found for Nested "XrSwapchainUsageFlagBits" "XR_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND"
pattern SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_MND     = SwapchainUsageFlagBits 0x00000080

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


type MND_swapchain_usage_input_attachment_bit_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_MND_swapchain_usage_input_attachment_bit_SPEC_VERSION"
pattern MND_swapchain_usage_input_attachment_bit_SPEC_VERSION :: forall a . Integral a => a
pattern MND_swapchain_usage_input_attachment_bit_SPEC_VERSION = 2


type MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME = "XR_MND_swapchain_usage_input_attachment_bit"

-- No documentation found for TopLevel "XR_MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME"
pattern MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MND_SWAPCHAIN_USAGE_INPUT_ATTACHMENT_BIT_EXTENSION_NAME = "XR_MND_swapchain_usage_input_attachment_bit"

