{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle  ( PipelineViewportSwizzleStateCreateInfoNV
                                                          , ViewportSwizzleNV
                                                          ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PipelineViewportSwizzleStateCreateInfoNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV
instance Show PipelineViewportSwizzleStateCreateInfoNV

instance FromCStruct PipelineViewportSwizzleStateCreateInfoNV


data ViewportSwizzleNV

instance ToCStruct ViewportSwizzleNV
instance Show ViewportSwizzleNV

instance FromCStruct ViewportSwizzleNV

