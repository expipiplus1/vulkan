{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test  ( PhysicalDeviceRepresentativeFragmentTestFeaturesNV
                                                                      , PipelineRepresentativeFragmentTestStateCreateInfoNV
                                                                      ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceRepresentativeFragmentTestFeaturesNV

instance ToCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV
instance Show PhysicalDeviceRepresentativeFragmentTestFeaturesNV

instance FromCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV


data PipelineRepresentativeFragmentTestStateCreateInfoNV

instance ToCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV
instance Show PipelineRepresentativeFragmentTestStateCreateInfoNV

instance FromCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV

