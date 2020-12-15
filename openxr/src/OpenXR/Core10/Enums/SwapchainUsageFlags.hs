{-# language CPP #-}
-- No documentation found for Chapter "SwapchainUsageFlags"
module OpenXR.Core10.Enums.SwapchainUsageFlags  (SwapchainUsageFlags(..)) where

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
-- | XrSwapchainUsageFlags - Swapchain usage flags
--
-- == Flag Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.Image.SwapchainCreateInfo',
-- 'OpenXR.Core10.Image.createSwapchain',
-- 'OpenXR.Core10.Image.enumerateSwapchainFormats'
newtype SwapchainUsageFlags = SwapchainUsageFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSwapchainUsageFlags :: String
conNameSwapchainUsageFlags = "SwapchainUsageFlags"

enumPrefixSwapchainUsageFlags :: String
enumPrefixSwapchainUsageFlags = ""

showTableSwapchainUsageFlags :: [(SwapchainUsageFlags, String)]
showTableSwapchainUsageFlags = []

instance Show SwapchainUsageFlags where
  showsPrec = enumShowsPrec enumPrefixSwapchainUsageFlags
                            showTableSwapchainUsageFlags
                            conNameSwapchainUsageFlags
                            (\(SwapchainUsageFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SwapchainUsageFlags where
  readPrec = enumReadPrec enumPrefixSwapchainUsageFlags
                          showTableSwapchainUsageFlags
                          conNameSwapchainUsageFlags
                          SwapchainUsageFlags

