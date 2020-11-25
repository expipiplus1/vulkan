{-# language CPP #-}
-- No documentation found for Chapter "PipelineDynamicStateCreateFlags"
module Vulkan.Core10.Enums.PipelineDynamicStateCreateFlags  (PipelineDynamicStateCreateFlags(..)) where

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
-- | VkPipelineDynamicStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineDynamicStateCreateFlags' is a bitmask type for setting a mask,
-- but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'
newtype PipelineDynamicStateCreateFlags = PipelineDynamicStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineDynamicStateCreateFlags :: String
conNamePipelineDynamicStateCreateFlags = "PipelineDynamicStateCreateFlags"

enumPrefixPipelineDynamicStateCreateFlags :: String
enumPrefixPipelineDynamicStateCreateFlags = ""

showTablePipelineDynamicStateCreateFlags :: [(PipelineDynamicStateCreateFlags, String)]
showTablePipelineDynamicStateCreateFlags = []

instance Show PipelineDynamicStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineDynamicStateCreateFlags of
    Just s -> showString enumPrefixPipelineDynamicStateCreateFlags . showString s
    Nothing ->
      let PipelineDynamicStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineDynamicStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineDynamicStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineDynamicStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineDynamicStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineDynamicStateCreateFlags)
            v <- step readPrec
            pure (PipelineDynamicStateCreateFlags v)
          )
    )

