{-# language CPP #-}
-- No documentation found for Chapter "EventCreateFlags"
module Vulkan.Core10.Enums.EventCreateFlags  (EventCreateFlags(..)) where

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
-- | VkEventCreateFlags - Reserved for future use
--
-- = Description
--
-- 'EventCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Event.EventCreateInfo'
newtype EventCreateFlags = EventCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameEventCreateFlags :: String
conNameEventCreateFlags = "EventCreateFlags"

enumPrefixEventCreateFlags :: String
enumPrefixEventCreateFlags = ""

showTableEventCreateFlags :: [(EventCreateFlags, String)]
showTableEventCreateFlags = []

instance Show EventCreateFlags where
  showsPrec = enumShowsPrec enumPrefixEventCreateFlags
                            showTableEventCreateFlags
                            conNameEventCreateFlags
                            (\(EventCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read EventCreateFlags where
  readPrec = enumReadPrec enumPrefixEventCreateFlags showTableEventCreateFlags conNameEventCreateFlags EventCreateFlags

