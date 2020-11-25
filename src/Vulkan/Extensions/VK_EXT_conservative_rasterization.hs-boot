{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_conservative_rasterization"
module Vulkan.Extensions.VK_EXT_conservative_rasterization  ( PhysicalDeviceConservativeRasterizationPropertiesEXT
                                                            , PipelineRasterizationConservativeStateCreateInfoEXT
                                                            ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceConservativeRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT
instance Show PhysicalDeviceConservativeRasterizationPropertiesEXT

instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT


data PipelineRasterizationConservativeStateCreateInfoEXT

instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT
instance Show PipelineRasterizationConservativeStateCreateInfoEXT

instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT

