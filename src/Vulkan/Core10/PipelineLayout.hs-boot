{-# language CPP #-}
-- No documentation found for Chapter "PipelineLayout"
module Vulkan.Core10.PipelineLayout  ( PipelineLayoutCreateInfo
                                     , PushConstantRange
                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineLayoutCreateInfo

instance ToCStruct PipelineLayoutCreateInfo
instance Show PipelineLayoutCreateInfo

instance FromCStruct PipelineLayoutCreateInfo


data PushConstantRange

instance ToCStruct PushConstantRange
instance Show PushConstantRange

instance FromCStruct PushConstantRange

