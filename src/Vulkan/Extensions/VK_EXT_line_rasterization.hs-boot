{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_line_rasterization  ( PhysicalDeviceLineRasterizationFeaturesEXT
                                                    , PhysicalDeviceLineRasterizationPropertiesEXT
                                                    , PipelineRasterizationLineStateCreateInfoEXT
                                                    ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceLineRasterizationFeaturesEXT

instance ToCStruct PhysicalDeviceLineRasterizationFeaturesEXT
instance Show PhysicalDeviceLineRasterizationFeaturesEXT

instance FromCStruct PhysicalDeviceLineRasterizationFeaturesEXT


data PhysicalDeviceLineRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceLineRasterizationPropertiesEXT
instance Show PhysicalDeviceLineRasterizationPropertiesEXT

instance FromCStruct PhysicalDeviceLineRasterizationPropertiesEXT


data PipelineRasterizationLineStateCreateInfoEXT

instance ToCStruct PipelineRasterizationLineStateCreateInfoEXT
instance Show PipelineRasterizationLineStateCreateInfoEXT

instance FromCStruct PipelineRasterizationLineStateCreateInfoEXT

