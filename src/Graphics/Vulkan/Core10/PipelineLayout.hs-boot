{-# language CPP #-}
module Graphics.Vulkan.Core10.PipelineLayout  ( PipelineLayoutCreateInfo
                                              , PushConstantRange
                                              ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PipelineLayoutCreateInfo

instance ToCStruct PipelineLayoutCreateInfo
instance Show PipelineLayoutCreateInfo

instance FromCStruct PipelineLayoutCreateInfo


data PushConstantRange

instance ToCStruct PushConstantRange
instance Show PushConstantRange

instance FromCStruct PushConstantRange

