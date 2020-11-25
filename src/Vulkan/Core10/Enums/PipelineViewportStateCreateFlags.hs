{-# language CPP #-}
-- No documentation found for Chapter "PipelineViewportStateCreateFlags"
module Vulkan.Core10.Enums.PipelineViewportStateCreateFlags  (PipelineViewportStateCreateFlags(..)) where

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
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineViewportStateCreateFlags :: String
conNamePipelineViewportStateCreateFlags = "PipelineViewportStateCreateFlags"

enumPrefixPipelineViewportStateCreateFlags :: String
enumPrefixPipelineViewportStateCreateFlags = ""

showTablePipelineViewportStateCreateFlags :: [(PipelineViewportStateCreateFlags, String)]
showTablePipelineViewportStateCreateFlags = []

instance Show PipelineViewportStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineViewportStateCreateFlags of
    Just s -> showString enumPrefixPipelineViewportStateCreateFlags . showString s
    Nothing ->
      let PipelineViewportStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineViewportStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineViewportStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineViewportStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineViewportStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineViewportStateCreateFlags)
            v <- step readPrec
            pure (PipelineViewportStateCreateFlags v)
          )
    )

