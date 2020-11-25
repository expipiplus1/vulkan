{-# language CPP #-}
-- No documentation found for Chapter "PipelineRasterizationStateCreateFlags"
module Vulkan.Core10.Enums.PipelineRasterizationStateCreateFlags  (PipelineRasterizationStateCreateFlags(..)) where

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
-- | VkPipelineRasterizationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineRasterizationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo'
newtype PipelineRasterizationStateCreateFlags = PipelineRasterizationStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineRasterizationStateCreateFlags :: String
conNamePipelineRasterizationStateCreateFlags = "PipelineRasterizationStateCreateFlags"

enumPrefixPipelineRasterizationStateCreateFlags :: String
enumPrefixPipelineRasterizationStateCreateFlags = ""

showTablePipelineRasterizationStateCreateFlags :: [(PipelineRasterizationStateCreateFlags, String)]
showTablePipelineRasterizationStateCreateFlags = []

instance Show PipelineRasterizationStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineRasterizationStateCreateFlags of
    Just s -> showString enumPrefixPipelineRasterizationStateCreateFlags . showString s
    Nothing ->
      let PipelineRasterizationStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineRasterizationStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineRasterizationStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineRasterizationStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineRasterizationStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineRasterizationStateCreateFlags)
            v <- step readPrec
            pure (PipelineRasterizationStateCreateFlags v)
          )
    )

