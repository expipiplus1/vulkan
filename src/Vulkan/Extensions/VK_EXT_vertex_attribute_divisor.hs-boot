{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_vertex_attribute_divisor"
module Vulkan.Extensions.VK_EXT_vertex_attribute_divisor  ( PhysicalDeviceVertexAttributeDivisorFeaturesEXT
                                                          , PhysicalDeviceVertexAttributeDivisorPropertiesEXT
                                                          , PipelineVertexInputDivisorStateCreateInfoEXT
                                                          , VertexInputBindingDivisorDescriptionEXT
                                                          ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceVertexAttributeDivisorFeaturesEXT

instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT
instance Show PhysicalDeviceVertexAttributeDivisorFeaturesEXT

instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT


data PhysicalDeviceVertexAttributeDivisorPropertiesEXT

instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT
instance Show PhysicalDeviceVertexAttributeDivisorPropertiesEXT

instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT


data PipelineVertexInputDivisorStateCreateInfoEXT

instance ToCStruct PipelineVertexInputDivisorStateCreateInfoEXT
instance Show PipelineVertexInputDivisorStateCreateInfoEXT

instance FromCStruct PipelineVertexInputDivisorStateCreateInfoEXT


data VertexInputBindingDivisorDescriptionEXT

instance ToCStruct VertexInputBindingDivisorDescriptionEXT
instance Show VertexInputBindingDivisorDescriptionEXT

instance FromCStruct VertexInputBindingDivisorDescriptionEXT

