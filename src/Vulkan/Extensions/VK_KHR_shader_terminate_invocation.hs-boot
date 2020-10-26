{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shader_terminate_invocation  (PhysicalDeviceShaderTerminateInvocationFeaturesKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDeviceShaderTerminateInvocationFeaturesKHR

instance ToCStruct PhysicalDeviceShaderTerminateInvocationFeaturesKHR
instance Show PhysicalDeviceShaderTerminateInvocationFeaturesKHR

instance FromCStruct PhysicalDeviceShaderTerminateInvocationFeaturesKHR

