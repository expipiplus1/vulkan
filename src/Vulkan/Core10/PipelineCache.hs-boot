{-# language CPP #-}
-- No documentation found for Chapter "PipelineCache"
module Vulkan.Core10.PipelineCache  (PipelineCacheCreateInfo) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineCacheCreateInfo

instance ToCStruct PipelineCacheCreateInfo
instance Show PipelineCacheCreateInfo

instance FromCStruct PipelineCacheCreateInfo

