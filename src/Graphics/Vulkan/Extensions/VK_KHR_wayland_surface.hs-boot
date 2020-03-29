{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_wayland_surface  (WaylandSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data WaylandSurfaceCreateInfoKHR

instance ToCStruct WaylandSurfaceCreateInfoKHR
instance Show WaylandSurfaceCreateInfoKHR

instance FromCStruct WaylandSurfaceCreateInfoKHR

