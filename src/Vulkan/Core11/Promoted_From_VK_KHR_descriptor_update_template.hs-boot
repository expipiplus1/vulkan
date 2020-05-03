{-# language CPP #-}
module Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template  ( DescriptorUpdateTemplateCreateInfo
                                                                      , DescriptorUpdateTemplateEntry
                                                                      ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DescriptorUpdateTemplateCreateInfo

instance ToCStruct DescriptorUpdateTemplateCreateInfo
instance Show DescriptorUpdateTemplateCreateInfo

instance FromCStruct DescriptorUpdateTemplateCreateInfo


data DescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateEntry
instance Show DescriptorUpdateTemplateEntry

instance FromCStruct DescriptorUpdateTemplateEntry

