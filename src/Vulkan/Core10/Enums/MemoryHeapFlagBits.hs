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

-- No documentation found for TopLevel "VkMemoryHeapFlagBits"
newtype MemoryHeapFlagBits = MemoryHeapFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkMemoryHeapFlagBits" "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT   = MemoryHeapFlagBits 0x00000001
-- No documentation found for Nested "VkMemoryHeapFlagBits" "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT"
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

