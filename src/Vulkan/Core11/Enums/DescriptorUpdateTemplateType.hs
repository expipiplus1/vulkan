{-# language CPP #-}
-- No documentation found for Chapter "DescriptorUpdateTemplateType"
module Vulkan.Core11.Enums.DescriptorUpdateTemplateType  (DescriptorUpdateTemplateType( DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET
                                                                                      , DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR
                                                                                      , ..
                                                                                      )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkDescriptorUpdateTemplateType"
newtype DescriptorUpdateTemplateType = DescriptorUpdateTemplateType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDescriptorUpdateTemplateType" "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET"
pattern DESCRIPTOR_UPDATE_TEMPLATE_TYPE_DESCRIPTOR_SET       = DescriptorUpdateTemplateType 0
-- No documentation found for Nested "VkDescriptorUpdateTemplateType" "VK_DESCRIPTOR_UPDATE_TEMPLATE_TYPE_PUSH_DESCRIPTORS_KHR"
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

