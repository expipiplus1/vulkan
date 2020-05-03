{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_android_surface  (AndroidSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data AndroidSurfaceCreateInfoKHR

instance ToCStruct AndroidSurfaceCreateInfoKHR
instance Show AndroidSurfaceCreateInfoKHR

instance FromCStruct AndroidSurfaceCreateInfoKHR

