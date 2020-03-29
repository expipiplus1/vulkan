{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive  ( PhysicalDeviceExclusiveScissorFeaturesNV
                                                           , PipelineViewportExclusiveScissorStateCreateInfoNV
                                                           ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceExclusiveScissorFeaturesNV

instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV
instance Show PhysicalDeviceExclusiveScissorFeaturesNV

instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV


data PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV
instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV

