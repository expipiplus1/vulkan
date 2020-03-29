{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_transform_feedback  ( PhysicalDeviceTransformFeedbackFeaturesEXT
                                                             , PhysicalDeviceTransformFeedbackPropertiesEXT
                                                             , PipelineRasterizationStateStreamCreateInfoEXT
                                                             ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceTransformFeedbackFeaturesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT
instance Show PhysicalDeviceTransformFeedbackFeaturesEXT

instance FromCStruct PhysicalDeviceTransformFeedbackFeaturesEXT


data PhysicalDeviceTransformFeedbackPropertiesEXT

instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT
instance Show PhysicalDeviceTransformFeedbackPropertiesEXT

instance FromCStruct PhysicalDeviceTransformFeedbackPropertiesEXT


data PipelineRasterizationStateStreamCreateInfoEXT

instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT
instance Show PipelineRasterizationStateStreamCreateInfoEXT

instance FromCStruct PipelineRasterizationStateStreamCreateInfoEXT

