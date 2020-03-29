{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags  (PipelineTessellationStateCreateFlags(..)) where

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
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkPipelineTessellationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineTessellationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo'
newtype PipelineTessellationStateCreateFlags = PipelineTessellationStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineTessellationStateCreateFlags where
  showsPrec p = \case
    PipelineTessellationStateCreateFlags x -> showParen (p >= 11) (showString "PipelineTessellationStateCreateFlags 0x" . showHex x)

instance Read PipelineTessellationStateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineTessellationStateCreateFlags")
                       v <- step readPrec
                       pure (PipelineTessellationStateCreateFlags v)))

