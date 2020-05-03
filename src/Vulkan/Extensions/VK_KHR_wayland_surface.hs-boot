{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_wayland_surface  (WaylandSurfaceCreateInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data WaylandSurfaceCreateInfoKHR

instance ToCStruct WaylandSurfaceCreateInfoKHR
instance Show WaylandSurfaceCreateInfoKHR

instance FromCStruct WaylandSurfaceCreateInfoKHR

