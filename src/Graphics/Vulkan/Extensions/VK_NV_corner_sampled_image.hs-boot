{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image  (PhysicalDeviceCornerSampledImageFeaturesNV) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceCornerSampledImageFeaturesNV

instance ToCStruct PhysicalDeviceCornerSampledImageFeaturesNV
instance Show PhysicalDeviceCornerSampledImageFeaturesNV

instance FromCStruct PhysicalDeviceCornerSampledImageFeaturesNV

