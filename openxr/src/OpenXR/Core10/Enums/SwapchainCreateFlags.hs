{-# language CPP #-}
-- No documentation found for Chapter "SwapchainCreateFlags"
module OpenXR.Core10.Enums.SwapchainCreateFlags  (SwapchainCreateFlags(..)) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
import OpenXR.Zero (Zero)
-- | XrSwapchainCreateFlags - Swapchain creation flags
--
-- == Flag Descriptions
--
-- A runtime /may/ implement any of these, but is not required to. A
-- runtime /must/ return
-- 'OpenXR.Core10.Enums.Result.ERROR_FEATURE_UNSUPPORTED' from
-- 'OpenXR.Core10.Image.createSwapchain' if an 'SwapchainCreateFlags' bit
-- is requested but not implemented.
--
-- = See Also
--
-- 'OpenXR.Core10.Image.SwapchainCreateInfo',
-- 'OpenXR.Core10.Image.createSwapchain'
newtype SwapchainCreateFlags = SwapchainCreateFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSwapchainCreateFlags :: String
conNameSwapchainCreateFlags = "SwapchainCreateFlags"

enumPrefixSwapchainCreateFlags :: String
enumPrefixSwapchainCreateFlags = ""

showTableSwapchainCreateFlags :: [(SwapchainCreateFlags, String)]
showTableSwapchainCreateFlags = []

instance Show SwapchainCreateFlags where
  showsPrec = enumShowsPrec enumPrefixSwapchainCreateFlags
                            showTableSwapchainCreateFlags
                            conNameSwapchainCreateFlags
                            (\(SwapchainCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SwapchainCreateFlags where
  readPrec = enumReadPrec enumPrefixSwapchainCreateFlags
                          showTableSwapchainCreateFlags
                          conNameSwapchainCreateFlags
                          SwapchainCreateFlags

