{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_win32_surface  (Win32SurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data Win32SurfaceCreateInfoKHR

instance ToCStruct Win32SurfaceCreateInfoKHR
instance Show Win32SurfaceCreateInfoKHR

instance FromCStruct Win32SurfaceCreateInfoKHR

