{-# language CPP #-}
module Vulkan.Core10.Enums.MemoryPropertyFlagBits  ( MemoryPropertyFlagBits( MEMORY_PROPERTY_DEVICE_LOCAL_BIT
                                                                           , MEMORY_PROPERTY_HOST_VISIBLE_BIT
                                                                           , MEMORY_PROPERTY_HOST_COHERENT_BIT
                                                                           , MEMORY_PROPERTY_HOST_CACHED_BIT
                                                                           , MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
                                                                           , MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD
                                                                           , MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD
                                                                           , MEMORY_PROPERTY_PROTECTED_BIT
                                                                           , ..
                                                                           )
                                                   , MemoryPropertyFlags
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
-- | VkMemoryPropertyFlagBits - Bitmask specifying properties for a memory
-- type
--
-- = Description
--
-- For any memory allocated with both the
-- 'MEMORY_PROPERTY_HOST_COHERENT_BIT' and the
-- 'MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD', host or device accesses also
-- perform automatic memory domain transfer operations, such that writes
-- are always automatically available and visible to both host and device
-- memory domains.
--
-- Note
--
-- Device coherence is a useful property for certain debugging use cases
-- (e.g. crash analysis, where performing separate coherence actions could
-- mean values are not reported correctly). However, device coherent
-- accesses may be slower than equivalent accesses without device
-- coherence, particularly if they are also device uncached. For device
-- uncached memory in particular, repeated accesses to the same or
-- neighbouring memory locations over a short time period (e.g. within a
-- frame) may be slower than it would be for the equivalent cached memory
-- type. As such, it is generally inadvisable to use device coherent or
-- device uncached memory except when really needed.
--
-- = See Also
--
-- 'MemoryPropertyFlags'
newtype MemoryPropertyFlagBits = MemoryPropertyFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'MEMORY_PROPERTY_DEVICE_LOCAL_BIT' bit specifies that memory allocated
-- with this type is the most efficient for device access. This property
-- will be set if and only if the memory type belongs to a heap with the
-- 'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_DEVICE_LOCAL_BIT'
-- set.
pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT = MemoryPropertyFlagBits 0x00000001
-- | 'MEMORY_PROPERTY_HOST_VISIBLE_BIT' bit specifies that memory allocated
-- with this type /can/ be mapped for host access using
-- 'Vulkan.Core10.Memory.mapMemory'.
pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT = MemoryPropertyFlagBits 0x00000002
-- | 'MEMORY_PROPERTY_HOST_COHERENT_BIT' bit specifies that the host cache
-- management commands 'Vulkan.Core10.Memory.flushMappedMemoryRanges' and
-- 'Vulkan.Core10.Memory.invalidateMappedMemoryRanges' are not needed to
-- flush host writes to the device or make device writes visible to the
-- host, respectively.
pattern MEMORY_PROPERTY_HOST_COHERENT_BIT = MemoryPropertyFlagBits 0x00000004
-- | 'MEMORY_PROPERTY_HOST_CACHED_BIT' bit specifies that memory allocated
-- with this type is cached on the host. Host memory accesses to uncached
-- memory are slower than to cached memory, however uncached memory is
-- always host coherent.
pattern MEMORY_PROPERTY_HOST_CACHED_BIT = MemoryPropertyFlagBits 0x00000008
-- | 'MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT' bit specifies that the memory
-- type only allows device access to the memory. Memory types /must/ not
-- have both 'MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT' and
-- 'MEMORY_PROPERTY_HOST_VISIBLE_BIT' set. Additionally, the object’s
-- backing memory /may/ be provided by the implementation lazily as
-- specified in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-device-lazy_allocation Lazily Allocated Memory>.
pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = MemoryPropertyFlagBits 0x00000010
-- | 'MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD' bit specifies that memory
-- allocated with this type is not cached on the device. Uncached device
-- memory is always device coherent.
pattern MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD = MemoryPropertyFlagBits 0x00000080
-- | 'MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD' bit specifies that device
-- accesses to allocations of this memory type are automatically made
-- available and visible.
pattern MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD = MemoryPropertyFlagBits 0x00000040
-- | 'MEMORY_PROPERTY_PROTECTED_BIT' bit specifies that the memory type only
-- allows device access to the memory, and allows protected queue
-- operations to access the memory. Memory types /must/ not have
-- 'MEMORY_PROPERTY_PROTECTED_BIT' set and any of
-- 'MEMORY_PROPERTY_HOST_VISIBLE_BIT' set, or
-- 'MEMORY_PROPERTY_HOST_COHERENT_BIT' set, or
-- 'MEMORY_PROPERTY_HOST_CACHED_BIT' set.
pattern MEMORY_PROPERTY_PROTECTED_BIT = MemoryPropertyFlagBits 0x00000020

type MemoryPropertyFlags = MemoryPropertyFlagBits

instance Show MemoryPropertyFlagBits where
  showsPrec p = \case
    MEMORY_PROPERTY_DEVICE_LOCAL_BIT -> showString "MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
    MEMORY_PROPERTY_HOST_VISIBLE_BIT -> showString "MEMORY_PROPERTY_HOST_VISIBLE_BIT"
    MEMORY_PROPERTY_HOST_COHERENT_BIT -> showString "MEMORY_PROPERTY_HOST_COHERENT_BIT"
    MEMORY_PROPERTY_HOST_CACHED_BIT -> showString "MEMORY_PROPERTY_HOST_CACHED_BIT"
    MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT -> showString "MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
    MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD -> showString "MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD"
    MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD -> showString "MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD"
    MEMORY_PROPERTY_PROTECTED_BIT -> showString "MEMORY_PROPERTY_PROTECTED_BIT"
    MemoryPropertyFlagBits x -> showParen (p >= 11) (showString "MemoryPropertyFlagBits 0x" . showHex x)

instance Read MemoryPropertyFlagBits where
  readPrec = parens (choose [("MEMORY_PROPERTY_DEVICE_LOCAL_BIT", pure MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                            , ("MEMORY_PROPERTY_HOST_VISIBLE_BIT", pure MEMORY_PROPERTY_HOST_VISIBLE_BIT)
                            , ("MEMORY_PROPERTY_HOST_COHERENT_BIT", pure MEMORY_PROPERTY_HOST_COHERENT_BIT)
                            , ("MEMORY_PROPERTY_HOST_CACHED_BIT", pure MEMORY_PROPERTY_HOST_CACHED_BIT)
                            , ("MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT", pure MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)
                            , ("MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD", pure MEMORY_PROPERTY_DEVICE_UNCACHED_BIT_AMD)
                            , ("MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD", pure MEMORY_PROPERTY_DEVICE_COHERENT_BIT_AMD)
                            , ("MEMORY_PROPERTY_PROTECTED_BIT", pure MEMORY_PROPERTY_PROTECTED_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "MemoryPropertyFlagBits")
                       v <- step readPrec
                       pure (MemoryPropertyFlagBits v)))

