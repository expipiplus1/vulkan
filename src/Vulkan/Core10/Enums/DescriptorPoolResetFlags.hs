{-# language CPP #-}
module Vulkan.Core10.Enums.DescriptorPoolResetFlags  (DescriptorPoolResetFlags(..)) where

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
import Vulkan.Core10.BaseType (Flags)
import Vulkan.Zero (Zero)
-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorPoolResetFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.resetDescriptorPool'
newtype DescriptorPoolResetFlags = DescriptorPoolResetFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show DescriptorPoolResetFlags where
  showsPrec p = \case
    DescriptorPoolResetFlags x -> showParen (p >= 11) (showString "DescriptorPoolResetFlags 0x" . showHex x)

instance Read DescriptorPoolResetFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DescriptorPoolResetFlags")
                       v <- step readPrec
                       pure (DescriptorPoolResetFlags v)))

