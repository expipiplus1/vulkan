{-# language CPP #-}
module Graphics.Vulkan.Core11.Enums.DescriptorUpdateTemplateCreateFlags  (DescriptorUpdateTemplateCreateFlags(..)) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
-- | VkDescriptorUpdateTemplateCreateFlags - Reserved for future use
--
-- = Description
--
-- 'DescriptorUpdateTemplateCreateFlags' is a bitmask type for setting a
-- mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo'
newtype DescriptorUpdateTemplateCreateFlags = DescriptorUpdateTemplateCreateFlags Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)



instance Show DescriptorUpdateTemplateCreateFlags where
  showsPrec p = \case
    DescriptorUpdateTemplateCreateFlags x -> showParen (p >= 11) (showString "DescriptorUpdateTemplateCreateFlags 0x" . showHex x)

instance Read DescriptorUpdateTemplateCreateFlags where
  readPrec = parens (choose []
                     +++
                     prec 10 (do
                       expectP (Ident "DescriptorUpdateTemplateCreateFlags")
                       v <- step readPrec
                       pure (DescriptorUpdateTemplateCreateFlags v)))

