{-# language CPP #-}
-- No documentation found for Chapter "PipelineMultisampleStateCreateFlags"
module Vulkan.Core10.Enums.PipelineMultisampleStateCreateFlags  (PipelineMultisampleStateCreateFlags(..)) where

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



conNamePipelineMultisampleStateCreateFlags :: String
conNamePipelineMultisampleStateCreateFlags = "PipelineMultisampleStateCreateFlags"

enumPrefixPipelineMultisampleStateCreateFlags :: String
enumPrefixPipelineMultisampleStateCreateFlags = ""

showTablePipelineMultisampleStateCreateFlags :: [(PipelineMultisampleStateCreateFlags, String)]
showTablePipelineMultisampleStateCreateFlags = []

instance Show PipelineMultisampleStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineMultisampleStateCreateFlags of
    Just s -> showString enumPrefixPipelineMultisampleStateCreateFlags . showString s
    Nothing ->
      let PipelineMultisampleStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineMultisampleStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineMultisampleStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineMultisampleStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineMultisampleStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineMultisampleStateCreateFlags)
            v <- step readPrec
            pure (PipelineMultisampleStateCreateFlags v)
          )
    )

