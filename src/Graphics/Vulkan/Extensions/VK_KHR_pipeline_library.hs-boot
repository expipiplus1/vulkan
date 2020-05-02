{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_pipeline_library  (PipelineLibraryCreateInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PipelineLibraryCreateInfoKHR

instance ToCStruct PipelineLibraryCreateInfoKHR
instance Show PipelineLibraryCreateInfoKHR

instance FromCStruct PipelineLibraryCreateInfoKHR

