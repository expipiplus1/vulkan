{-# language CPP #-}
-- No documentation found for Chapter "SparseMemoryBindFlagBits"
module Vulkan.Core10.Enums.SparseMemoryBindFlagBits  ( SparseMemoryBindFlags
                                                     , SparseMemoryBindFlagBits( SPARSE_MEMORY_BIND_METADATA_BIT
                                                                               , ..
                                                                               )
                                                     ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type SparseMemoryBindFlags = SparseMemoryBindFlagBits

-- | VkSparseMemoryBindFlagBits - Bitmask specifying usage of a sparse memory
-- binding operation
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'SparseMemoryBindFlags'
newtype SparseMemoryBindFlagBits = SparseMemoryBindFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SPARSE_MEMORY_BIND_METADATA_BIT' specifies that the memory being bound
-- is only for the metadata aspect.
pattern SPARSE_MEMORY_BIND_METADATA_BIT = SparseMemoryBindFlagBits 0x00000001

conNameSparseMemoryBindFlagBits :: String
conNameSparseMemoryBindFlagBits = "SparseMemoryBindFlagBits"

enumPrefixSparseMemoryBindFlagBits :: String
enumPrefixSparseMemoryBindFlagBits = "SPARSE_MEMORY_BIND_METADATA_BIT"

showTableSparseMemoryBindFlagBits :: [(SparseMemoryBindFlagBits, String)]
showTableSparseMemoryBindFlagBits = [(SPARSE_MEMORY_BIND_METADATA_BIT, "")]

instance Show SparseMemoryBindFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSparseMemoryBindFlagBits
      showTableSparseMemoryBindFlagBits
      conNameSparseMemoryBindFlagBits
      (\(SparseMemoryBindFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SparseMemoryBindFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSparseMemoryBindFlagBits
      showTableSparseMemoryBindFlagBits
      conNameSparseMemoryBindFlagBits
      SparseMemoryBindFlagBits
