{-# language CPP #-}
-- No documentation found for Chapter "SemaphoreCreateFlags"
module Vulkan.Core10.Enums.SemaphoreCreateFlags  (SemaphoreCreateFlags(..)) where

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
-- | VkSemaphoreCreateFlags - Reserved for future use
--
-- = Description
--
-- 'SemaphoreCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo'
newtype SemaphoreCreateFlags = SemaphoreCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameSemaphoreCreateFlags :: String
conNameSemaphoreCreateFlags = "SemaphoreCreateFlags"

enumPrefixSemaphoreCreateFlags :: String
enumPrefixSemaphoreCreateFlags = ""

showTableSemaphoreCreateFlags :: [(SemaphoreCreateFlags, String)]
showTableSemaphoreCreateFlags = []

instance Show SemaphoreCreateFlags where
  showsPrec = enumShowsPrec enumPrefixSemaphoreCreateFlags
                            showTableSemaphoreCreateFlags
                            conNameSemaphoreCreateFlags
                            (\(SemaphoreCreateFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SemaphoreCreateFlags where
  readPrec = enumReadPrec enumPrefixSemaphoreCreateFlags
                          showTableSemaphoreCreateFlags
                          conNameSemaphoreCreateFlags
                          SemaphoreCreateFlags

