{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineLayoutCreateFlags  (PipelineLayoutCreateFlags(..)) where

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
-- | VkPipelineLayoutCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineLayoutCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
newtype PipelineLayoutCreateFlags = PipelineLayoutCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineLayoutCreateFlags where
  showsPrec p = \case
    PipelineLayoutCreateFlags x -> showParen (p >= 11) (showString "PipelineLayoutCreateFlags 0x" . showHex x)

instance Read PipelineLayoutCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineLayoutCreateFlags")
                       v <- step readPrec
                       pure (PipelineLayoutCreateFlags v)))

