{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineViewportStateCreateFlags  (PipelineViewportStateCreateFlags(..)) where

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
-- | VkPipelineViewportStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineViewportStateCreateFlags' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo'
newtype PipelineViewportStateCreateFlags = PipelineViewportStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineViewportStateCreateFlags where
  showsPrec p = \case
    PipelineViewportStateCreateFlags x -> showParen (p >= 11) (showString "PipelineViewportStateCreateFlags 0x" . showHex x)

instance Read PipelineViewportStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineViewportStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineViewportStateCreateFlags v)))

