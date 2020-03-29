{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_android_surface  (AndroidSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data AndroidSurfaceCreateInfoKHR

instance ToCStruct AndroidSurfaceCreateInfoKHR
instance Show AndroidSurfaceCreateInfoKHR

instance FromCStruct AndroidSurfaceCreateInfoKHR

