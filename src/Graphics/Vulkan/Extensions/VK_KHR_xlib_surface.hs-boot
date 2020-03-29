{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_xlib_surface  (XlibSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR
instance Show XlibSurfaceCreateInfoKHR

instance FromCStruct XlibSurfaceCreateInfoKHR

