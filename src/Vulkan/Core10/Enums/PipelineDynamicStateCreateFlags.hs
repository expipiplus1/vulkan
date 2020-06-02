{-# language CPP #-}
module Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags  (PipelineDynamicStateCreateFlags(..)) where

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
-- | VkPipelineDynamicStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineDynamicStateCreateFlags' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'
newtype PipelineDynamicStateCreateFlags = PipelineDynamicStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineDynamicStateCreateFlags where
  showsPrec p = \case
    PipelineDynamicStateCreateFlags x -> showParen (p >= 11) (showString "PipelineDynamicStateCreateFlags 0x" . showHex x)

instance Read PipelineDynamicStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineDynamicStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineDynamicStateCreateFlags v)))

