{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_pipeline_creation_feedback  ( PipelineCreationFeedbackCreateInfoEXT
                                                            , PipelineCreationFeedbackEXT
                                                            ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineCreationFeedbackCreateInfoEXT

instance ToCStruct PipelineCreationFeedbackCreateInfoEXT
instance Show PipelineCreationFeedbackCreateInfoEXT

instance FromCStruct PipelineCreationFeedbackCreateInfoEXT


data PipelineCreationFeedbackEXT

instance ToCStruct PipelineCreationFeedbackEXT
instance Show PipelineCreationFeedbackEXT

instance FromCStruct PipelineCreationFeedbackEXT

