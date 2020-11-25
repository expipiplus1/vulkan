{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateType"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateType  (DescriptorUpdateTemplateType( DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
                                                                                      , DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
                                                                                      , ..
                                                                                      )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
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
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET = DescriptorUpdateTemplateType 0
-- | 'DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR' specifies that
-- the descriptor update template will be used for push descriptor updates
-- only.
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR = DescriptorUpdateTemplateType 1
{-# complete DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET,
             DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR :: DescriptorUpdateTemplateType #-}

instance Show DescriptorUpdateTemplateType where
  showsPrec p = \case
    DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET -> showString "DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET"
    DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR -> showString "DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
    DescriptorUpdateTemplateType x -> showParen (p >= 11) (showString "DescriptorUpdateTemplateType " . showsPrec 11 x)

instance Read DescriptorUpdateTemplateType where
  readPrec = parens (choose [("DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET", pure DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET)
                            , ("DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR", pure DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR)]
                     +++
                     prec 10 (do
                       expectP (Ident "DescriptorUpdateTemplateType")
                       v <- step readPrec
                       pure (DescriptorUpdateTemplateType v)))

