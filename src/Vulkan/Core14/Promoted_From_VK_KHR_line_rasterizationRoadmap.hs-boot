{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_line_rasterizationRoadmap"
module Vulkan.Core14.Promoted_From_VK_KHR_line_rasterizationRoadmap  ( PhysicalDeviceLineRasterizationFeatures
                                                                     , PhysicalDeviceLineRasterizationProperties
                                                                     , PipelineRasterizationLineStateCreateInfo
                                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceLineRasterizationFeatures

instance ToCStruct PhysicalDeviceLineRasterizationFeatures
instance Show PhysicalDeviceLineRasterizationFeatures

instance FromCStruct PhysicalDeviceLineRasterizationFeatures


data PhysicalDeviceLineRasterizationProperties

instance ToCStruct PhysicalDeviceLineRasterizationProperties
instance Show PhysicalDeviceLineRasterizationProperties

instance FromCStruct PhysicalDeviceLineRasterizationProperties


data PipelineRasterizationLineStateCreateInfo

instance ToCStruct PipelineRasterizationLineStateCreateInfo
instance Show PipelineRasterizationLineStateCreateInfo

instance FromCStruct PipelineRasterizationLineStateCreateInfo

