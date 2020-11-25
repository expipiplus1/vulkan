{-# language CPP #-}
-- No documentation found for Chapter "PipelineVertexInputStateCreateFlags"
module Vulkan.Core10.Enums.PipelineVertexInputStateCreateFlags  (PipelineVertexInputStateCreateFlags(..)) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkPipelineVertexInputStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineVertexInputStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo'
newtype PipelineVertexInputStateCreateFlags = PipelineVertexInputStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineVertexInputStateCreateFlags :: String
conNamePipelineVertexInputStateCreateFlags = "PipelineVertexInputStateCreateFlags"

enumPrefixPipelineVertexInputStateCreateFlags :: String
enumPrefixPipelineVertexInputStateCreateFlags = ""

showTablePipelineVertexInputStateCreateFlags :: [(PipelineVertexInputStateCreateFlags, String)]
showTablePipelineVertexInputStateCreateFlags = []

instance Show PipelineVertexInputStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineVertexInputStateCreateFlags of
    Just s -> showString enumPrefixPipelineVertexInputStateCreateFlags . showString s
    Nothing ->
      let PipelineVertexInputStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineVertexInputStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineVertexInputStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineVertexInputStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineVertexInputStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineVertexInputStateCreateFlags)
            v <- step readPrec
            pure (PipelineVertexInputStateCreateFlags v)
          )
    )

