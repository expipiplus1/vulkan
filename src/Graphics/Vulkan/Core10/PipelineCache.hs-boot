{-# language CPP #-}
module Graphics.Vulkan.Core10.PipelineCache  (PipelineCacheCreateInfo) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PipelineCacheCreateInfo

instance ToCStruct PipelineCacheCreateInfo
instance Show PipelineCacheCreateInfo

instance FromCStruct PipelineCacheCreateInfo

