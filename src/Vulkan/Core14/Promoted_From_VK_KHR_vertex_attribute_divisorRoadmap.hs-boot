{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_vertex_attribute_divisorRoadmap  ( PhysicalDeviceVertexAttributeDivisorFeatures
                                                                           , PhysicalDeviceVertexAttributeDivisorProperties
                                                                           , PipelineVertexInputDivisorStateCreateInfo
                                                                           , VertexInputBindingDivisorDescription
                                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceVertexAttributeDivisorFeatures

instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeatures
instance Show PhysicalDeviceVertexAttributeDivisorFeatures

instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeatures


data PhysicalDeviceVertexAttributeDivisorProperties

instance ToCStruct PhysicalDeviceVertexAttributeDivisorProperties
instance Show PhysicalDeviceVertexAttributeDivisorProperties

instance FromCStruct PhysicalDeviceVertexAttributeDivisorProperties


data PipelineVertexInputDivisorStateCreateInfo

instance ToCStruct PipelineVertexInputDivisorStateCreateInfo
instance Show PipelineVertexInputDivisorStateCreateInfo

instance FromCStruct PipelineVertexInputDivisorStateCreateInfo


data VertexInputBindingDivisorDescription

instance ToCStruct VertexInputBindingDivisorDescription
instance Show VertexInputBindingDivisorDescription

instance FromCStruct VertexInputBindingDivisorDescription

