{-# language CPP #-}
-- No documentation found for Chapter "SwapchainCreateFlagBits"
module OpenXR.Core10.Enums.SwapchainCreateFlagBits  ( SwapchainCreateFlags
                                                    , SwapchainCreateFlagBits( SWAPCHAIN_CREATE_PROTECTED_CONTENT_BIT
                                                                             , SWAPCHAIN_CREATE_STATIC_IMAGE_BIT
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
type SwapchainCreateFlags = SwapchainCreateFlagBits

-- No documentation found for TopLevel "XrSwapchainCreateFlagBits"
newtype SwapchainCreateFlagBits = SwapchainCreateFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrSwapchainCreateFlagBits" "XR_SWAPCHAIN_CREATE_PROTECTED_CONTENT_BIT"
pattern SWAPCHAIN_CREATE_PROTECTED_CONTENT_BIT = SwapchainCreateFlagBits 0x0000000000000001

-- No documentation found for Nested "XrSwapchainCreateFlagBits" "XR_SWAPCHAIN_CREATE_STATIC_IMAGE_BIT"
pattern SWAPCHAIN_CREATE_STATIC_IMAGE_BIT = SwapchainCreateFlagBits 0x0000000000000002

conNameSwapchainCreateFlagBits :: String
conNameSwapchainCreateFlagBits = "SwapchainCreateFlagBits"

enumPrefixSwapchainCreateFlagBits :: String
enumPrefixSwapchainCreateFlagBits = "SWAPCHAIN_CREATE_"

showTableSwapchainCreateFlagBits :: [(SwapchainCreateFlagBits, String)]
showTableSwapchainCreateFlagBits =
  [
    ( SWAPCHAIN_CREATE_PROTECTED_CONTENT_BIT
    , "PROTECTED_CONTENT_BIT"
    )
  ,
    ( SWAPCHAIN_CREATE_STATIC_IMAGE_BIT
    , "STATIC_IMAGE_BIT"
    )
  ]

instance Show SwapchainCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSwapchainCreateFlagBits
      showTableSwapchainCreateFlagBits
      conNameSwapchainCreateFlagBits
      (\(SwapchainCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SwapchainCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSwapchainCreateFlagBits
      showTableSwapchainCreateFlagBits
      conNameSwapchainCreateFlagBits
      SwapchainCreateFlagBits
