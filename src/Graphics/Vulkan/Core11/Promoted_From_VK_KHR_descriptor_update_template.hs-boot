{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template  ( DescriptorUpdateTemplateCreateInfo
                                                                               , DescriptorUpdateTemplateEntry
                                                                               ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DescriptorUpdateTemplateCreateInfo

instance ToCStruct DescriptorUpdateTemplateCreateInfo
instance Show DescriptorUpdateTemplateCreateInfo

instance FromCStruct DescriptorUpdateTemplateCreateInfo


data DescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateEntry
instance Show DescriptorUpdateTemplateEntry

instance FromCStruct DescriptorUpdateTemplateEntry

