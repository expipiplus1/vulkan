{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_shader_clock  (PhysicalDeviceShaderClockFeaturesKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDeviceShaderClockFeaturesKHR

instance ToCStruct PhysicalDeviceShaderClockFeaturesKHR
instance Show PhysicalDeviceShaderClockFeaturesKHR

instance FromCStruct PhysicalDeviceShaderClockFeaturesKHR

