{-# language CPP #-}
-- No documentation found for Chapter "PipelineColorBlendStateCreateFlags"
module Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags  (PipelineColorBlendStateCreateFlags(..)) where

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
-- | VkPipelineColorBlendStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineColorBlendStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'
newtype PipelineColorBlendStateCreateFlags = PipelineColorBlendStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



instance Show PipelineColorBlendStateCreateFlags where
  showsPrec p = \case
    PipelineColorBlendStateCreateFlags x -> showParen (p >= 11) (showString "PipelineColorBlendStateCreateFlags 0x" . showHex x)

instance Read PipelineColorBlendStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineColorBlendStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineColorBlendStateCreateFlags v)))

