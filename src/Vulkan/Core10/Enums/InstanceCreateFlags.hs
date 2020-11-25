{-# language CPP #-}
-- No documentation found for Chapter "InstanceCreateFlags"
module Vulkan.Core10.Enums.InstanceCreateFlags  (InstanceCreateFlags(..)) where

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
-- | VkInstanceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'InstanceCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo'
newtype InstanceCreateFlags = InstanceCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameInstanceCreateFlags :: String
conNameInstanceCreateFlags = "InstanceCreateFlags"

enumPrefixInstanceCreateFlags :: String
enumPrefixInstanceCreateFlags = ""

showTableInstanceCreateFlags :: [(InstanceCreateFlags, String)]
showTableInstanceCreateFlags = []

instance Show InstanceCreateFlags where
  showsPrec = enumShowsPrec enumPrefixInstanceCreateFlags
                            showTableInstanceCreateFlags
                            conNameInstanceCreateFlags
                            (\(InstanceCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read InstanceCreateFlags where
  readPrec = enumReadPrec enumPrefixInstanceCreateFlags
                          showTableInstanceCreateFlags
                          conNameInstanceCreateFlags
                          InstanceCreateFlags

