{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_viewport_swizzle"
module Vulkan.Extensions.VK_NV_viewport_swizzle  ( PipelineViewportSwizzleStateCreateInfoNV
                                                 , ViewportSwizzleNV
                                                 ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PipelineViewportSwizzleStateCreateInfoNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV
instance Show PipelineViewportSwizzleStateCreateInfoNV

instance FromCStruct PipelineViewportSwizzleStateCreateInfoNV


data ViewportSwizzleNV

instance ToCStruct ViewportSwizzleNV
instance Show ViewportSwizzleNV

instance FromCStruct ViewportSwizzleNV

