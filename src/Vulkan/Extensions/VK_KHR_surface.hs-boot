{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_surface  ( SurfaceCapabilitiesKHR
                                         , SurfaceFormatKHR
                                         , PresentModeKHR
                                         ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data SurfaceCapabilitiesKHR

instance ToCStruct SurfaceCapabilitiesKHR
instance Show SurfaceCapabilitiesKHR

instance FromCStruct SurfaceCapabilitiesKHR


data SurfaceFormatKHR

instance ToCStruct SurfaceFormatKHR
instance Show SurfaceFormatKHR

instance FromCStruct SurfaceFormatKHR


data PresentModeKHR

