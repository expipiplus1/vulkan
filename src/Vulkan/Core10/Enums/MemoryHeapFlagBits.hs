{-# language CPP #-}
-- No documentation found for Chapter "MemoryHeapFlagBits"
module Vulkan.Core10.Enums.MemoryHeapFlagBits  ( MemoryHeapFlags
                                               , MemoryHeapFlagBits( MEMORY_HEAP_DEVICE_LOCAL_BIT
                                                                   , MEMORY_HEAP_MULTI_INSTANCE_BIT
                                                                   , ..
                                                                   )
                                               ) where

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
type MemoryHeapFlags = MemoryHeapFlagBits

-- | VkMemoryHeapFlagBits - Bitmask specifying attribute flags for a heap
--
-- = See Also
--
-- 'MemoryHeapFlags'
newtype MemoryHeapFlagBits = MemoryHeapFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'MEMORY_HEAP_DEVICE_LOCAL_BIT' specifies that the heap corresponds to
-- device local memory. Device local memory /may/ have different
-- performance characteristics than host local memory, and /may/ support
-- different memory property flags.
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT   = MemoryHeapFlagBits 0x00000001
-- | 'MEMORY_HEAP_MULTI_INSTANCE_BIT' specifies that in a logical device
-- representing more than one physical device, there is a per-physical
-- device instance of the heap memory. By default, an allocation from such
-- a heap will be replicated to each physical device’s instance of the
-- heap.
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT = MemoryHeapFlagBits 0x00000002

conNameMemoryHeapFlagBits :: String
conNameMemoryHeapFlagBits = "MemoryHeapFlagBits"

enumPrefixMemoryHeapFlagBits :: String
enumPrefixMemoryHeapFlagBits = "MEMORY_HEAP_"

showTableMemoryHeapFlagBits :: [(MemoryHeapFlagBits, String)]
showTableMemoryHeapFlagBits =
  [(MEMORY_HEAP_DEVICE_LOCAL_BIT, "DEVICE_LOCAL_BIT"), (MEMORY_HEAP_MULTI_INSTANCE_BIT, "MULTI_INSTANCE_BIT")]

instance Show MemoryHeapFlagBits where
  showsPrec = enumShowsPrec enumPrefixMemoryHeapFlagBits
                            showTableMemoryHeapFlagBits
                            conNameMemoryHeapFlagBits
                            (\(MemoryHeapFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read MemoryHeapFlagBits where
  readPrec =
    enumReadPrec enumPrefixMemoryHeapFlagBits showTableMemoryHeapFlagBits conNameMemoryHeapFlagBits MemoryHeapFlagBits

