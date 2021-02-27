{-# language CPP #-}
-- No documentation found for Chapter "EventCreateFlagBits"
module Vulkan.Core10.Enums.EventCreateFlagBits  ( EventCreateFlags
                                                , EventCreateFlagBits( EVENT_CREATE_DEVICE_ONLY_BIT_KHR
                                                                     , ..
                                                                     )
                                                ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type EventCreateFlags = EventCreateFlagBits

-- | VkEventCreateFlagBits - Event creation flag bits
--
-- = Description
--
-- -   VK_EVENT_CREATE_DEVICE_ONLY_BIT_KHR specifies that host event
--     commands will not be used with this event.
--
-- = See Also
--
-- 'EventCreateFlags'
newtype EventCreateFlagBits = EventCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkEventCreateFlagBits" "VK_EVENT_CREATE_DEVICE_ONLY_BIT_KHR"
pattern EVENT_CREATE_DEVICE_ONLY_BIT_KHR = EventCreateFlagBits 0x00000001

conNameEventCreateFlagBits :: String
conNameEventCreateFlagBits = "EventCreateFlagBits"

enumPrefixEventCreateFlagBits :: String
enumPrefixEventCreateFlagBits = "EVENT_CREATE_DEVICE_ONLY_BIT_KHR"

showTableEventCreateFlagBits :: [(EventCreateFlagBits, String)]
showTableEventCreateFlagBits = [(EVENT_CREATE_DEVICE_ONLY_BIT_KHR, "")]

instance Show EventCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixEventCreateFlagBits
                            showTableEventCreateFlagBits
                            conNameEventCreateFlagBits
                            (\(EventCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read EventCreateFlagBits where
  readPrec = enumReadPrec enumPrefixEventCreateFlagBits
                          showTableEventCreateFlagBits
                          conNameEventCreateFlagBits
                          EventCreateFlagBits

