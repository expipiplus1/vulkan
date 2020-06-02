{-# language CPP #-}
module Vulkan.Core10.Enums.MemoryMapFlags  (MemoryMapFlags(..)) where

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
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show MemoryMapFlags where
  showsPrec p = \case
    MemoryMapFlags x -> showParen (p >= 11) (showString "MemoryMapFlags 0x" . showHex x)

instance Read MemoryMapFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "MemoryMapFlags")
                       v <- step readPrec
                       pure (MemoryMapFlags v)))

