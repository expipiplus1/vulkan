{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block  ( DescriptorPoolInlineUniformBlockCreateInfoEXT
                                                               , PhysicalDeviceInlineUniformBlockFeaturesEXT
                                                               , PhysicalDeviceInlineUniformBlockPropertiesEXT
                                                               , WriteDescriptorSetInlineUniformBlockEXT
                                                               ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DescriptorPoolInlineUniformBlockCreateInfoEXT

instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT
instance Show DescriptorPoolInlineUniformBlockCreateInfoEXT

instance FromCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT


data PhysicalDeviceInlineUniformBlockFeaturesEXT

instance ToCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT
instance Show PhysicalDeviceInlineUniformBlockFeaturesEXT

instance FromCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT


data PhysicalDeviceInlineUniformBlockPropertiesEXT

instance ToCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT
instance Show PhysicalDeviceInlineUniformBlockPropertiesEXT

instance FromCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT


data WriteDescriptorSetInlineUniformBlockEXT

instance ToCStruct WriteDescriptorSetInlineUniformBlockEXT
instance Show WriteDescriptorSetInlineUniformBlockEXT

instance FromCStruct WriteDescriptorSetInlineUniformBlockEXT

