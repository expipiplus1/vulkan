{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_pipeline_creation_feedback"
module Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback  ( PipelineCreationFeedback
                                                                      , PipelineCreationFeedbackCreateInfo
                                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineCreationFeedback

instance ToCStruct PipelineCreationFeedback
instance Show PipelineCreationFeedback

instance FromCStruct PipelineCreationFeedback


data PipelineCreationFeedbackCreateInfo

instance ToCStruct PipelineCreationFeedbackCreateInfo
instance Show PipelineCreationFeedbackCreateInfo

instance FromCStruct PipelineCreationFeedbackCreateInfo

