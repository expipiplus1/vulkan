{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags  (PipelineRasterizationStateCreateFlags(..)) where

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
-- | VkPipelineRasterizationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineRasterizationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
newtype PipelineRasterizationStateCreateFlags = PipelineRasterizationStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



instance Show PipelineRasterizationStateCreateFlags where
  showsPrec p = \case
    PipelineRasterizationStateCreateFlags x -> showParen (p >= 11) (showString "PipelineRasterizationStateCreateFlags 0x" . showHex x)

instance Read PipelineRasterizationStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineRasterizationStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineRasterizationStateCreateFlags v)))

