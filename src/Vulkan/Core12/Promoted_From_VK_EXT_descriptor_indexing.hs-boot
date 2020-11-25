{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_descriptor_indexing"
module Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing  ( DescriptorSetLayoutBindingFlagsCreateInfo
                                                               , DescriptorSetVariableDescriptorCountAllocateInfo
                                                               , DescriptorSetVariableDescriptorCountLayoutSupport
                                                               , PhysicalDeviceDescriptorIndexingFeatures
                                                               , PhysicalDeviceDescriptorIndexingProperties
                                                               ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DescriptorSetLayoutBindingFlagsCreateInfo

instance ToCStruct DescriptorSetLayoutBindingFlagsCreateInfo
instance Show DescriptorSetLayoutBindingFlagsCreateInfo

instance FromCStruct DescriptorSetLayoutBindingFlagsCreateInfo


data DescriptorSetVariableDescriptorCountAllocateInfo

instance ToCStruct DescriptorSetVariableDescriptorCountAllocateInfo
instance Show DescriptorSetVariableDescriptorCountAllocateInfo

instance FromCStruct DescriptorSetVariableDescriptorCountAllocateInfo


data DescriptorSetVariableDescriptorCountLayoutSupport

instance ToCStruct DescriptorSetVariableDescriptorCountLayoutSupport
instance Show DescriptorSetVariableDescriptorCountLayoutSupport

instance FromCStruct DescriptorSetVariableDescriptorCountLayoutSupport


data PhysicalDeviceDescriptorIndexingFeatures

instance ToCStruct PhysicalDeviceDescriptorIndexingFeatures
instance Show PhysicalDeviceDescriptorIndexingFeatures

instance FromCStruct PhysicalDeviceDescriptorIndexingFeatures


data PhysicalDeviceDescriptorIndexingProperties

instance ToCStruct PhysicalDeviceDescriptorIndexingProperties
instance Show PhysicalDeviceDescriptorIndexingProperties

instance FromCStruct PhysicalDeviceDescriptorIndexingProperties

