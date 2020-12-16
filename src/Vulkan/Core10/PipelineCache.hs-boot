{-# language CPP #-}
-- No documentation found for Chapter "PipelineCache"
module Vulkan.Core10.PipelineCache  (PipelineCacheCreateInfo) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PipelineCacheCreateInfo

instance ToCStruct PipelineCacheCreateInfo
instance Show PipelineCacheCreateInfo

instance FromCStruct PipelineCacheCreateInfo

