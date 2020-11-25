{-# language CPP #-}
-- No documentation found for Chapter "MemoryMapFlags"
module Vulkan.Core10.Enums.MemoryMapFlags  (MemoryMapFlags(..)) where

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
-- | VkMemoryMapFlags - Reserved for future use
--
-- = Description
--
-- 'MemoryMapFlags' is a bitmask type for setting a mask, but is currently
-- reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Memory.mapMemory'
newtype MemoryMapFlags = MemoryMapFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameMemoryMapFlags :: String
conNameMemoryMapFlags = "MemoryMapFlags"

enumPrefixMemoryMapFlags :: String
enumPrefixMemoryMapFlags = ""

showTableMemoryMapFlags :: [(MemoryMapFlags, String)]
showTableMemoryMapFlags = []

instance Show MemoryMapFlags where
  showsPrec = enumShowsPrec enumPrefixMemoryMapFlags
                            showTableMemoryMapFlags
                            conNameMemoryMapFlags
                            (\(MemoryMapFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read MemoryMapFlags where
  readPrec = enumReadPrec enumPrefixMemoryMapFlags showTableMemoryMapFlags conNameMemoryMapFlags MemoryMapFlags

