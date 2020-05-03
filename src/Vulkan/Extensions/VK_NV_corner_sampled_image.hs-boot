{-# language CPP #-}
module Vulkan.Extensions.VK_NV_corner_sampled_image  (PhysicalDeviceCornerSampledImageFeaturesNV) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceCornerSampledImageFeaturesNV

instance ToCStruct PhysicalDeviceCornerSampledImageFeaturesNV
instance Show PhysicalDeviceCornerSampledImageFeaturesNV

instance FromCStruct PhysicalDeviceCornerSampledImageFeaturesNV

