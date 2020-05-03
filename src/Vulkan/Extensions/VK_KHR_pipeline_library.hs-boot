{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_pipeline_library  (PipelineLibraryCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineLibraryCreateInfoKHR

instance ToCStruct PipelineLibraryCreateInfoKHR
instance Show PipelineLibraryCreateInfoKHR

instance FromCStruct PipelineLibraryCreateInfoKHR

