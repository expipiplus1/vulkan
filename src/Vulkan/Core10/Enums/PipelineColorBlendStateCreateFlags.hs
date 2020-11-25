{-# language CPP #-}
-- No documentation found for Chapter "PipelineColorBlendStateCreateFlags"
module Vulkan.Core10.Enums.PipelineColorBlendStateCreateFlags  (PipelineColorBlendStateCreateFlags(..)) where

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
-- | VkPipelineColorBlendStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineColorBlendStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo'
newtype PipelineColorBlendStateCreateFlags = PipelineColorBlendStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineColorBlendStateCreateFlags :: String
conNamePipelineColorBlendStateCreateFlags = "PipelineColorBlendStateCreateFlags"

enumPrefixPipelineColorBlendStateCreateFlags :: String
enumPrefixPipelineColorBlendStateCreateFlags = ""

showTablePipelineColorBlendStateCreateFlags :: [(PipelineColorBlendStateCreateFlags, String)]
showTablePipelineColorBlendStateCreateFlags = []

instance Show PipelineColorBlendStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineColorBlendStateCreateFlags of
    Just s -> showString enumPrefixPipelineColorBlendStateCreateFlags . showString s
    Nothing ->
      let PipelineColorBlendStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineColorBlendStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineColorBlendStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineColorBlendStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineColorBlendStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineColorBlendStateCreateFlags)
            v <- step readPrec
            pure (PipelineColorBlendStateCreateFlags v)
          )
    )

