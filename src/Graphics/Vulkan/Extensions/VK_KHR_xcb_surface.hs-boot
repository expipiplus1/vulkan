{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_xcb_surface  (XcbSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data XcbSurfaceCreateInfoKHR

instance ToCStruct XcbSurfaceCreateInfoKHR
instance Show XcbSurfaceCreateInfoKHR

instance FromCStruct XcbSurfaceCreateInfoKHR

