{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateCreateFlags"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags  (DescriptorUpdateTemplateCreateFlags(..)) where

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
-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorUpdateTemplateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo'
newtype DescriptorUpdateTemplateCreateFlags = DescriptorUpdateTemplateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameDescriptorUpdateTemplateCreateFlags :: String
conNameDescriptorUpdateTemplateCreateFlags = "DescriptorUpdateTemplateCreateFlags"

enumPrefixDescriptorUpdateTemplateCreateFlags :: String
enumPrefixDescriptorUpdateTemplateCreateFlags = ""

showTableDescriptorUpdateTemplateCreateFlags :: [(DescriptorUpdateTemplateCreateFlags, String)]
showTableDescriptorUpdateTemplateCreateFlags = []

instance Show DescriptorUpdateTemplateCreateFlags where
  showsPrec p e = case lookup e showTableDescriptorUpdateTemplateCreateFlags of
    Just s -> showString enumPrefixDescriptorUpdateTemplateCreateFlags . showString s
    Nothing ->
      let DescriptorUpdateTemplateCreateFlags x = e
      in  showParen (p >= 11) (showString conNameDescriptorUpdateTemplateCreateFlags . showString " 0x" . showHex x)

instance Read DescriptorUpdateTemplateCreateFlags where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixDescriptorUpdateTemplateCreateFlags
          asum ((\(e, s) -> e <$ string s) <$> showTableDescriptorUpdateTemplateCreateFlags)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameDescriptorUpdateTemplateCreateFlags)
            v <- step readPrec
            pure (DescriptorUpdateTemplateCreateFlags v)
          )
    )

