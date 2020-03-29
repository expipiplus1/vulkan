{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.PipelineCacheCreateFlags  (PipelineCacheCreateFlags(..)) where

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
-- | VkPipelineCacheCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineCacheCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.PipelineCache.PipelineCacheCreateInfo'
newtype PipelineCacheCreateFlags = PipelineCacheCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show PipelineCacheCreateFlags where
  showsPrec p = \case
    PipelineCacheCreateFlags x -> showParen (p >= 11) (showString "PipelineCacheCreateFlags 0x" . showHex x)

instance Read PipelineCacheCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "PipelineCacheCreateFlags")
                       v <- step readPrec
                       pure (PipelineCacheCreateFlags v)))

