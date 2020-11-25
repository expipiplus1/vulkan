{-# language CPP #-}
-- No documentation found for Chapter "SparseMemoryBindFlagBits"
module Vulkan.Core10.Enums.SparseMemoryBindFlagBits  ( SparseMemoryBindFlags
                                                     , SparseMemoryBindFlagBits( SPARSE_MEMORY_BIND_METADATA_BIT
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
type SparseMemoryBindFlags = SparseMemoryBindFlagBits

-- No documentation found for TopLevel "VkSparseMemoryBindFlagBits"
newtype SparseMemoryBindFlagBits = SparseMemoryBindFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkSparseMemoryBindFlagBits" "VK_SPARSE_MEMORY_BIND_METADATA_BIT"
pattern SPARSE_MEMORY_BIND_METADATA_BIT = SparseMemoryBindFlagBits 0x00000001

conNameSparseMemoryBindFlagBits :: String
conNameSparseMemoryBindFlagBits = "SparseMemoryBindFlagBits"

enumPrefixSparseMemoryBindFlagBits :: String
enumPrefixSparseMemoryBindFlagBits = "SPARSE_MEMORY_BIND_METADATA_BIT"

showTableSparseMemoryBindFlagBits :: [(SparseMemoryBindFlagBits, String)]
showTableSparseMemoryBindFlagBits = [(SPARSE_MEMORY_BIND_METADATA_BIT, "")]


instance Show SparseMemoryBindFlagBits where
showsPrec = enumShowsPrec enumPrefixSparseMemoryBindFlagBits
                          showTableSparseMemoryBindFlagBits
                          conNameSparseMemoryBindFlagBits
                          (\(SparseMemoryBindFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read SparseMemoryBindFlagBits where
  readPrec = enumReadPrec enumPrefixSparseMemoryBindFlagBits
                          showTableSparseMemoryBindFlagBits
                          conNameSparseMemoryBindFlagBits
                          SparseMemoryBindFlagBits

