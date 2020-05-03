{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_xcb_surface  (XcbSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data XcbSurfaceCreateInfoKHR

instance ToCStruct XcbSurfaceCreateInfoKHR
instance Show XcbSurfaceCreateInfoKHR

instance FromCStruct XcbSurfaceCreateInfoKHR

