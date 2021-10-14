{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateType"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateType  (DescriptorUpdateTemplateType( DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
                                                                                      , DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
                                                                                      , ..
                                                                                      )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkDescriptorUpdateTemplateType - Indicates the valid usage of the
-- descriptor update template
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
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
  showsPrec = enumShowsPrec enumPrefixDescriptorUpdateTemplateType
                            showTableDescriptorUpdateTemplateType
                            conNameDescriptorUpdateTemplateType
                            (\(DescriptorUpdateTemplateType x) -> x)
                            (showsPrec 11)

instance Read DescriptorUpdateTemplateType where
  readPrec = enumReadPrec enumPrefixDescriptorUpdateTemplateType
                          showTableDescriptorUpdateTemplateType
                          conNameDescriptorUpdateTemplateType
                          DescriptorUpdateTemplateType

