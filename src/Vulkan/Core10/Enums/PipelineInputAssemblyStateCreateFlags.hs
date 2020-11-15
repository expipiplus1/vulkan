{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags  (PipelineInputAssemblyStateCreateFlags(..)) where

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
-- | VkPipelineInputAssemblyStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineInputAssemblyStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'
newtype PipelineInputAssemblyStateCreateFlags = PipelineInputAssemblyStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



instance Show PipelineInputAssemblyStateCreateFlags where
  showsPrec p = \case
    PipelineInputAssemblyStateCreateFlags x -> showParen (p >= 11) (showString "PipelineInputAssemblyStateCreateFlags 0x" . showHex x)

instance Read PipelineInputAssemblyStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineInputAssemblyStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineInputAssemblyStateCreateFlags v)))

