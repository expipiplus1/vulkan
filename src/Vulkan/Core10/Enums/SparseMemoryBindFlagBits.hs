{-# language CPP #-}
-- No documentation found for Chapter "SparseMemoryBindFlagBits"
module Vulkan.Core10.Enums.SparseMemoryBindFlagBits  ( SparseMemoryBindFlags
                                                     , SparseMemoryBindFlagBits( SPARSE_MEMORY_BIND_METADATA_BIT
                                                                               , ..
                                                                               )
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type SparseMemoryBindFlags = SparseMemoryBindFlagBits

-- | VkSparseMemoryBindFlagBits - Bitmask specifying usage of a sparse memory
-- binding operation
--
-- = See Also
--
-- 'SparseMemoryBindFlags'
newtype SparseMemoryBindFlagBits = SparseMemoryBindFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SPARSE_MEMORY_BIND_METADATA_BIT' specifies that the memory being bound
-- is only for the metadata aspect.
pattern SPARSE_MEMORY_BIND_METADATA_BIT = SparseMemoryBindFlagBits 0x00000001

instance Show SparseMemoryBindFlagBits where
  showsPrec p = \case
    SPARSE_MEMORY_BIND_METADATA_BIT -> showString "SPARSE_MEMORY_BIND_METADATA_BIT"
    SparseMemoryBindFlagBits x -> showParen (p >= 11) (showString "SparseMemoryBindFlagBits 0x" . showHex x)

instance Read SparseMemoryBindFlagBits where
  readPrec = parens (choose [("SPARSE_MEMORY_BIND_METADATA_BIT", pure SPARSE_MEMORY_BIND_METADATA_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SparseMemoryBindFlagBits")
                       v <- step readPrec
                       pure (SparseMemoryBindFlagBits v)))

