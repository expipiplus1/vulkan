{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_texel_buffer_alignment  ( PhysicalDeviceTexelBufferAlignmentFeaturesEXT
                                                        , PhysicalDeviceTexelBufferAlignmentPropertiesEXT
                                                        ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT
instance Show PhysicalDeviceTexelBufferAlignmentFeaturesEXT

instance FromCStruct PhysicalDeviceTexelBufferAlignmentFeaturesEXT


data PhysicalDeviceTexelBufferAlignmentPropertiesEXT

instance ToCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT
instance Show PhysicalDeviceTexelBufferAlignmentPropertiesEXT

instance FromCStruct PhysicalDeviceTexelBufferAlignmentPropertiesEXT

