{-# language CPP #-}
-- No documentation found for Chapter "PipelineInputAssemblyStateCreateFlags"
module Vulkan.Core10.Enums.PipelineInputAssemblyStateCreateFlags  (PipelineInputAssemblyStateCreateFlags(..)) where

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
-- | VkPipelineInputAssemblyStateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'PipelineInputAssemblyStateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo'
newtype PipelineInputAssemblyStateCreateFlags = PipelineInputAssemblyStateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNamePipelineInputAssemblyStateCreateFlags :: String
conNamePipelineInputAssemblyStateCreateFlags = "PipelineInputAssemblyStateCreateFlags"

enumPrefixPipelineInputAssemblyStateCreateFlags :: String
enumPrefixPipelineInputAssemblyStateCreateFlags = ""

showTablePipelineInputAssemblyStateCreateFlags :: [(PipelineInputAssemblyStateCreateFlags, String)]
showTablePipelineInputAssemblyStateCreateFlags = []

instance Show PipelineInputAssemblyStateCreateFlags where
  showsPrec p e = case lookup e showTablePipelineInputAssemblyStateCreateFlags of
    Just s -> showString enumPrefixPipelineInputAssemblyStateCreateFlags . showString s
    Nothing ->
      let PipelineInputAssemblyStateCreateFlags x = e
      in  showParen (p >= 11) (showString conNamePipelineInputAssemblyStateCreateFlags . showString " 0x" . showHex x)

instance Read PipelineInputAssemblyStateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixPipelineInputAssemblyStateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTablePipelineInputAssemblyStateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNamePipelineInputAssemblyStateCreateFlags)
            v <- step readPrec
            pure (PipelineInputAssemblyStateCreateFlags v)
          )
    )

