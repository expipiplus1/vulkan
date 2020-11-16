{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags  (PipelineDepthStencilStateCreateFlags(..)) where

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
-- | VkPipelineDepthStencilStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineDepthStencilStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo'
newtype PipelineDepthStencilStateCreateFlags = PipelineDepthStencilStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



instance Show PipelineDepthStencilStateCreateFlags where
  showsPrec p = \case
    PipelineDepthStencilStateCreateFlags x -> showParen (p >= 11) (showString "PipelineDepthStencilStateCreateFlags 0x" . showHex x)

instance Read PipelineDepthStencilStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineDepthStencilStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineDepthStencilStateCreateFlags v)))

