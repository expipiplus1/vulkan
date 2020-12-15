{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_descriptor_update_template"
module Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template  ( DescriptorUpdateTemplateCreateInfo
                                                                      , DescriptorUpdateTemplateEntry
                                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DescriptorUpdateTemplateCreateInfo

instance ToCStruct DescriptorUpdateTemplateCreateInfo
instance Show DescriptorUpdateTemplateCreateInfo

instance FromCStruct DescriptorUpdateTemplateCreateInfo


data DescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateEntry
instance Show DescriptorUpdateTemplateEntry

instance FromCStruct DescriptorUpdateTemplateEntry

