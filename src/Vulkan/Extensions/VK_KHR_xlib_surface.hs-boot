{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_xlib_surface  (XlibSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR
instance Show XlibSurfaceCreateInfoKHR

instance FromCStruct XlibSurfaceCreateInfoKHR

