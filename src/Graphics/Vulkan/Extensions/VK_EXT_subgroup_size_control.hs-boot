{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_subgroup_size_control  ( PhysicalDeviceSubgroupSizeControlFeaturesEXT
                                                                , PhysicalDeviceSubgroupSizeControlPropertiesEXT
                                                                , PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
                                                                ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceSubgroupSizeControlFeaturesEXT

instance ToCStruct PhysicalDeviceSubgroupSizeControlFeaturesEXT
instance Show PhysicalDeviceSubgroupSizeControlFeaturesEXT

instance FromCStruct PhysicalDeviceSubgroupSizeControlFeaturesEXT


data PhysicalDeviceSubgroupSizeControlPropertiesEXT

instance ToCStruct PhysicalDeviceSubgroupSizeControlPropertiesEXT
instance Show PhysicalDeviceSubgroupSizeControlPropertiesEXT

instance FromCStruct PhysicalDeviceSubgroupSizeControlPropertiesEXT


data PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT

instance ToCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT
instance Show PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT

instance FromCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT

