{-# language CPP #-}
module Vulkan.Core10.Enums.MemoryHeapFlagBits  ( MemoryHeapFlagBits( MEMORY_HEAP_DEVICE_LOCAL_BIT
                                                                   , MEMORY_HEAP_MULTI_INSTANCE_BIT
                                                                   , ..
                                                                   )
                                               , MemoryHeapFlags
                                               ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkMemoryHeapFlagBits - Bitmask specifying attribute flags for a heap
--
-- = See Also
--
-- 'MemoryHeapFlags'
newtype MemoryHeapFlagBits = MemoryHeapFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'MEMORY_HEAP_DEVICE_LOCAL_BIT' specifies that the heap corresponds to
-- device local memory. Device local memory /may/ have different
-- performance characteristics than host local memory, and /may/ support
-- different memory property flags.
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT = MemoryHeapFlagBits 0x00000001
-- | 'MEMORY_HEAP_MULTI_INSTANCE_BIT' specifies that in a logical device
-- representing more than one physical device, there is a per-physical
-- device instance of the heap memory. By default, an allocation from such
-- a heap will be replicated to each physical device’s instance of the
-- heap.
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT = MemoryHeapFlagBits 0x00000002

type MemoryHeapFlags = MemoryHeapFlagBits

instance Show MemoryHeapFlagBits where
  showsPrec p = \case
    MEMORY_HEAP_DEVICE_LOCAL_BIT -> showString "MEMORY_HEAP_DEVICE_LOCAL_BIT"
    MEMORY_HEAP_MULTI_INSTANCE_BIT -> showString "MEMORY_HEAP_MULTI_INSTANCE_BIT"
    MemoryHeapFlagBits x -> showParen (p >= 11) (showString "MemoryHeapFlagBits 0x" . showHex x)

instance Read MemoryHeapFlagBits where
  readPrec = parens (choose [("MEMORY_HEAP_DEVICE_LOCAL_BIT", pure MEMORY_HEAP_DEVICE_LOCAL_BIT)
                            , ("MEMORY_HEAP_MULTI_INSTANCE_BIT", pure MEMORY_HEAP_MULTI_INSTANCE_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "MemoryHeapFlagBits")
                       v <- step readPrec
                       pure (MemoryHeapFlagBits v)))

