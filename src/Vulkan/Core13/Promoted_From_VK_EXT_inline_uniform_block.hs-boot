{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_inline_uniform_block"
module Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block  ( DescriptorPoolInlineUniformBlockCreateInfo
                                                                , PhysicalDeviceInlineUniformBlockFeatures
                                                                , PhysicalDeviceInlineUniformBlockProperties
                                                                , WriteDescriptorSetInlineUniformBlock
                                                                ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DescriptorPoolInlineUniformBlockCreateInfo

instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfo
instance Show DescriptorPoolInlineUniformBlockCreateInfo

instance FromCStruct DescriptorPoolInlineUniformBlockCreateInfo


data PhysicalDeviceInlineUniformBlockFeatures

instance ToCStruct PhysicalDeviceInlineUniformBlockFeatures
instance Show PhysicalDeviceInlineUniformBlockFeatures

instance FromCStruct PhysicalDeviceInlineUniformBlockFeatures


data PhysicalDeviceInlineUniformBlockProperties

instance ToCStruct PhysicalDeviceInlineUniformBlockProperties
instance Show PhysicalDeviceInlineUniformBlockProperties

instance FromCStruct PhysicalDeviceInlineUniformBlockProperties


data WriteDescriptorSetInlineUniformBlock

instance ToCStruct WriteDescriptorSetInlineUniformBlock
instance Show WriteDescriptorSetInlineUniformBlock

instance FromCStruct WriteDescriptorSetInlineUniformBlock

