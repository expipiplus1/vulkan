{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateType"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateType  (DescriptorUpdateTemplateType( DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
                                                                                      , DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
                                                                                      , ..
                                                                                      )) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkDescriptorUpdateTemplateType - Indicates the valid usage of the
-- descriptor update template
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo'
newtype DescriptorUpdateTemplateType = DescriptorUpdateTemplateType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET' specifies that the
-- descriptor update template will be used for descriptor set updates only.
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET       = DescriptorUpdateTemplateType 0
-- | 'DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR' specifies that
-- the descriptor update template will be used for push descriptor updates
-- only.
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = DescriptorUpdateTemplateType 1
{-# complete DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET,
             DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR :: DescriptorUpdateTemplateType #-}

conNameDescriptorUpdateTemplateType :: String
conNameDescriptorUpdateTemplateType = "DescriptorUpdateTemplateType"

enumPrefixDescriptorUpdateTemplateType :: String
enumPrefixDescriptorUpdateTemplateType = "DESCRIPTOR_UPDATE_TEMPLATE_TYPE_"

showTableDescriptorUpdateTemplateType :: [(DescriptorUpdateTemplateType, String)]
showTableDescriptorUpdateTemplateType =
  [ (DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET      , "DESCRIPTOR_SET")
  , (DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR, "PUSH_DESCRIPTORS_KHR")
  ]

instance Show DescriptorUpdateTemplateType where
  showsPrec p e = case lookup e showTableDescriptorUpdateTemplateType of
    Just s -> showString enumPrefixDescriptorUpdateTemplateType . showString s
    Nothing ->
      let DescriptorUpdateTemplateType x = e
      in  showParen (p >= 11) (showString conNameDescriptorUpdateTemplateType . showString " " . showsPrec 11 x)

instance Read DescriptorUpdateTemplateType where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixDescriptorUpdateTemplateType
          asum ((\(e, s) -> e <$ string s) <$> showTableDescriptorUpdateTemplateType)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameDescriptorUpdateTemplateType)
            v <- step readPrec
            pure (DescriptorUpdateTemplateType v)
          )
    )

