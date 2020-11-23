{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_ray_query  (PhysicalDeviceRayQueryFeaturesKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceRayQueryFeaturesKHR

instance ToCStruct PhysicalDeviceRayQueryFeaturesKHR
instance Show PhysicalDeviceRayQueryFeaturesKHR

instance FromCStruct PhysicalDeviceRayQueryFeaturesKHR

