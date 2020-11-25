{-# language CPP #-}
-- No documentation found for Chapter "PipelineMultisampleStateCreateFlags"
module Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags  (PipelineMultisampleStateCreateFlags(..)) where

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
-- | VkPipelineMultisampleStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineMultisampleStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo'
newtype PipelineMultisampleStateCreateFlags = PipelineMultisampleStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



instance Show PipelineMultisampleStateCreateFlags where
  showsPrec p = \case
    PipelineMultisampleStateCreateFlags x -> showParen (p >= 11) (showString "PipelineMultisampleStateCreateFlags 0x" . showHex x)

instance Read PipelineMultisampleStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineMultisampleStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineMultisampleStateCreateFlags v)))

