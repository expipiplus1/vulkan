{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shader_clock  (PhysicalDeviceShaderClockFeaturesKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceShaderClockFeaturesKHR

instance ToCStruct PhysicalDeviceShaderClockFeaturesKHR
instance Show PhysicalDeviceShaderClockFeaturesKHR

instance FromCStruct PhysicalDeviceShaderClockFeaturesKHR

