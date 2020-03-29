{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization  ( PhysicalDeviceConservativeRasterizationPropertiesEXT
                                                                     , PipelineRasterizationConservativeStateCreateInfoEXT
                                                                     ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceConservativeRasterizationPropertiesEXT

instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT
instance Show PhysicalDeviceConservativeRasterizationPropertiesEXT

instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT


data PipelineRasterizationConservativeStateCreateInfoEXT

instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT
instance Show PipelineRasterizationConservativeStateCreateInfoEXT

instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT

