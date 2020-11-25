{-# language CPP #-}
-- No documentation found for Chapter "DescriptorPoolResetFlags"
module Vulkan.Core10.Enums.DescriptorPoolResetFlags  (DescriptorPoolResetFlags(..)) where

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
-- | VkDescriptorPoolResetFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorPoolResetFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.resetDescriptorPool'
newtype DescriptorPoolResetFlags = DescriptorPoolResetFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDescriptorPoolResetFlags :: String
conNameDescriptorPoolResetFlags = "DescriptorPoolResetFlags"

enumPrefixDescriptorPoolResetFlags :: String
enumPrefixDescriptorPoolResetFlags = ""

showTableDescriptorPoolResetFlags :: [(DescriptorPoolResetFlags, String)]
showTableDescriptorPoolResetFlags = []

instance Show DescriptorPoolResetFlags where
  showsPrec p e = case lookup e showTableDescriptorPoolResetFlags of
    Just s -> showString enumPrefixDescriptorPoolResetFlags . showString s
    Nothing ->
      let DescriptorPoolResetFlags x = e
      in  showParen (p >= 11) (showString conNameDescriptorPoolResetFlags . showString " 0x" . showHex x)

instance Read DescriptorPoolResetFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixDescriptorPoolResetFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableDescriptorPoolResetFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameDescriptorPoolResetFlags)
            v <- step readPrec
            pure (DescriptorPoolResetFlags v)
          )
    )

