{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayoutCreateFlags"
module Vulkan.Core10.Enums.PipelineLayoutCreateFlags  (PipelineLayoutCreateFlags(..)) where

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
-- | VkPipelineLayoutCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineLayoutCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo'
newtype PipelineLayoutCreateFlags = PipelineLayoutCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineLayoutCreateFlags :: String
conNamePipelineLayoutCreateFlags = "PipelineLayoutCreateFlags"

enumPrefixPipelineLayoutCreateFlags :: String
enumPrefixPipelineLayoutCreateFlags = ""

showTablePipelineLayoutCreateFlags :: [(PipelineLayoutCreateFlags, String)]
showTablePipelineLayoutCreateFlags = []

instance Show PipelineLayoutCreateFlags where
  showsPrec p e = case lookup e showTablePipelineLayoutCreateFlags of
    Just s -> showString enumPrefixPipelineLayoutCreateFlags . showString s
    Nothing ->
      let PipelineLayoutCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineLayoutCreateFlags . showString " 0x" . showHex x)

instance Read PipelineLayoutCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineLayoutCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineLayoutCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineLayoutCreateFlags)
            v <- step readPrec
            pure (PipelineLayoutCreateFlags v)
          )
    )

