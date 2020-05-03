{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_pipeline_compiler_control  (PipelineCompilerControlCreateInfoAMD) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineCompilerControlCreateInfoAMD

instance ToCStruct PipelineCompilerControlCreateInfoAMD
instance Show PipelineCompilerControlCreateInfoAMD

instance FromCStruct PipelineCompilerControlCreateInfoAMD

