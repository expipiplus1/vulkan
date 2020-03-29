{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties  ( PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
                                                                         , PipelineExecutableInfoKHR
                                                                         , PipelineExecutableInternalRepresentationKHR
                                                                         , PipelineExecutablePropertiesKHR
                                                                         , PipelineExecutableStatisticKHR
                                                                         , PipelineInfoKHR
                                                                         ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDevicePipelineExecutablePropertiesFeaturesKHR

instance ToCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR
instance Show PhysicalDevicePipelineExecutablePropertiesFeaturesKHR

instance FromCStruct PhysicalDevicePipelineExecutablePropertiesFeaturesKHR


data PipelineExecutableInfoKHR

instance ToCStruct PipelineExecutableInfoKHR
instance Show PipelineExecutableInfoKHR

instance FromCStruct PipelineExecutableInfoKHR


data PipelineExecutableInternalRepresentationKHR

instance ToCStruct PipelineExecutableInternalRepresentationKHR
instance Show PipelineExecutableInternalRepresentationKHR

instance FromCStruct PipelineExecutableInternalRepresentationKHR


data PipelineExecutablePropertiesKHR

instance ToCStruct PipelineExecutablePropertiesKHR
instance Show PipelineExecutablePropertiesKHR

instance FromCStruct PipelineExecutablePropertiesKHR


data PipelineExecutableStatisticKHR

instance ToCStruct PipelineExecutableStatisticKHR
instance Show PipelineExecutableStatisticKHR

instance FromCStruct PipelineExecutableStatisticKHR


data PipelineInfoKHR

instance ToCStruct PipelineInfoKHR
instance Show PipelineInfoKHR

instance FromCStruct PipelineInfoKHR

