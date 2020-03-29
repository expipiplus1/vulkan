{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_push_descriptor  (PhysicalDevicePushDescriptorPropertiesKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data PhysicalDevicePushDescriptorPropertiesKHR

instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR
instance Show PhysicalDevicePushDescriptorPropertiesKHR

instance FromCStruct PhysicalDevicePushDescriptorPropertiesKHR

