{-# language CPP #-}
-- No documentation found for Chapter "PipelineTessellationStateCreateFlags"
module Vulkan.Core10.Enums.PipelineTessellationStateCreateFlags  (PipelineTessellationStateCreateFlags(..)) where

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
-- | VkPipelineTessellationStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineTessellationStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo'
newtype PipelineTessellationStateCreateFlags = PipelineTessellationStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineTessellationStateCreateFlags :: String
conNamePipelineTessellationStateCreateFlags = "PipelineTessellationStateCreateFlags"

enumPrefixPipelineTessellationStateCreateFlags :: String
enumPrefixPipelineTessellationStateCreateFlags = ""

showTablePipelineTessellationStateCreateFlags :: [(PipelineTessellationStateCreateFlags, String)]
showTablePipelineTessellationStateCreateFlags = []

instance Show PipelineTessellationStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineTessellationStateCreateFlags of
    Just s -> showString enumPrefixPipelineTessellationStateCreateFlags . showString s
    Nothing ->
      let PipelineTessellationStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineTessellationStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineTessellationStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineTessellationStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineTessellationStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineTessellationStateCreateFlags)
            v <- step readPrec
            pure (PipelineTessellationStateCreateFlags v)
          )
    )

