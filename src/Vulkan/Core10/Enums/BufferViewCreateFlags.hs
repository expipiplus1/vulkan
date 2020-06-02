{-# language CPP #-}
module Vulkan.Core10.Enums.BufferViewCreateFlags  (BufferViewCreateFlags(..)) where

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
-- | VkBufferViewCreateFlags - Reserved for future use
--
-- = Description
--
-- 'BufferViewCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo'
newtype BufferViewCreateFlags = BufferViewCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show BufferViewCreateFlags where
  showsPrec p = \case
    BufferViewCreateFlags x -> showParen (p >= 11) (showString "BufferViewCreateFlags 0x" . showHex x)

instance Read BufferViewCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "BufferViewCreateFlags")
                       v <- step readPrec
                       pure (BufferViewCreateFlags v)))

