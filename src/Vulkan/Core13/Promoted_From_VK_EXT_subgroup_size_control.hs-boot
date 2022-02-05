{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_subgroup_size_control"
module Vulkan.Core13.Promoted_From_VK_EXT_subgroup_size_control  ( PhysicalDeviceSubgroupSizeControlFeatures
                                                                 , PhysicalDeviceSubgroupSizeControlProperties
                                                                 , PipelineShaderStageRequiredSubgroupSizeCreateInfo
                                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceSubgroupSizeControlFeatures

instance ToCStruct PhysicalDeviceSubgroupSizeControlFeatures
instance Show PhysicalDeviceSubgroupSizeControlFeatures

instance FromCStruct PhysicalDeviceSubgroupSizeControlFeatures


data PhysicalDeviceSubgroupSizeControlProperties

instance ToCStruct PhysicalDeviceSubgroupSizeControlProperties
instance Show PhysicalDeviceSubgroupSizeControlProperties

instance FromCStruct PhysicalDeviceSubgroupSizeControlProperties


data PipelineShaderStageRequiredSubgroupSizeCreateInfo

instance ToCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfo
instance Show PipelineShaderStageRequiredSubgroupSizeCreateInfo

instance FromCStruct PipelineShaderStageRequiredSubgroupSizeCreateInfo

