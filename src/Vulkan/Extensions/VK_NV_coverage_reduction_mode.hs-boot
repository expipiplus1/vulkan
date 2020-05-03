{-# language CPP #-}
module Vulkan.Extensions.VK_NV_coverage_reduction_mode  ( FramebufferMixedSamplesCombinationNV
                                                        , PhysicalDeviceCoverageReductionModeFeaturesNV
                                                        , PipelineCoverageReductionStateCreateInfoNV
                                                        ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data FramebufferMixedSamplesCombinationNV

instance ToCStruct FramebufferMixedSamplesCombinationNV
instance Show FramebufferMixedSamplesCombinationNV

instance FromCStruct FramebufferMixedSamplesCombinationNV


data PhysicalDeviceCoverageReductionModeFeaturesNV

instance ToCStruct PhysicalDeviceCoverageReductionModeFeaturesNV
instance Show PhysicalDeviceCoverageReductionModeFeaturesNV

instance FromCStruct PhysicalDeviceCoverageReductionModeFeaturesNV


data PipelineCoverageReductionStateCreateInfoNV

instance ToCStruct PipelineCoverageReductionStateCreateInfoNV
instance Show PipelineCoverageReductionStateCreateInfoNV

instance FromCStruct PipelineCoverageReductionStateCreateInfoNV

