{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_wayland_surface  ( WaylandSurfaceCreateInfoKHR
                                                 , Wl_display
                                                 ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data WaylandSurfaceCreateInfoKHR

instance ToCStruct WaylandSurfaceCreateInfoKHR
instance Show WaylandSurfaceCreateInfoKHR

instance FromCStruct WaylandSurfaceCreateInfoKHR


data Wl_display

