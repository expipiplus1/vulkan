{-# language CPP #-}
-- No documentation found for Chapter "PipelineDepthStencilStateCreateFlags"
module Vulkan.Core10.Enums.PipelineDepthStencilStateCreateFlags  (PipelineDepthStencilStateCreateFlags(..)) where

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



conNamePipelineDepthStencilStateCreateFlags :: String
conNamePipelineDepthStencilStateCreateFlags = "PipelineDepthStencilStateCreateFlags"

enumPrefixPipelineDepthStencilStateCreateFlags :: String
enumPrefixPipelineDepthStencilStateCreateFlags = ""

showTablePipelineDepthStencilStateCreateFlags :: [(PipelineDepthStencilStateCreateFlags, String)]
showTablePipelineDepthStencilStateCreateFlags = []

instance Show PipelineDepthStencilStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineDepthStencilStateCreateFlags of
    Just s -> showString enumPrefixPipelineDepthStencilStateCreateFlags . showString s
    Nothing ->
      let PipelineDepthStencilStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineDepthStencilStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineDepthStencilStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineDepthStencilStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineDepthStencilStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineDepthStencilStateCreateFlags)
            v <- step readPrec
            pure (PipelineDepthStencilStateCreateFlags v)
          )
    )

