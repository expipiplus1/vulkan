{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_scissor_exclusive"
module Vulkan.Extensions.VK_NV_scissor_exclusive  ( PhysicalDeviceExclusiveScissorFeaturesNV
                                                  , PipelineViewportExclusiveScissorStateCreateInfoNV
                                                  ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceExclusiveScissorFeaturesNV

instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV
instance Show PhysicalDeviceExclusiveScissorFeaturesNV

instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV


data PipelineViewportExclusiveScissorStateCreateInfoNV

instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV
instance Show PipelineViewportExclusiveScissorStateCreateInfoNV

instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV

