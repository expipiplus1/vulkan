{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'"
module Vulkan.Core14.Promoted_From_VK_EXT_pipeline_robustnessAdditionalFunctionality'  ( PhysicalDevicePipelineRobustnessFeatures
                                                                                       , PhysicalDevicePipelineRobustnessProperties
                                                                                       , PipelineRobustnessCreateInfo
                                                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePipelineRobustnessFeatures

instance ToCStruct PhysicalDevicePipelineRobustnessFeatures
instance Show PhysicalDevicePipelineRobustnessFeatures

instance FromCStruct PhysicalDevicePipelineRobustnessFeatures


data PhysicalDevicePipelineRobustnessProperties

instance ToCStruct PhysicalDevicePipelineRobustnessProperties
instance Show PhysicalDevicePipelineRobustnessProperties

instance FromCStruct PhysicalDevicePipelineRobustnessProperties


data PipelineRobustnessCreateInfo

instance ToCStruct PipelineRobustnessCreateInfo
instance Show PipelineRobustnessCreateInfo

instance FromCStruct PipelineRobustnessCreateInfo

