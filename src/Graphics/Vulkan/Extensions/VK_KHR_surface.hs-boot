{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_surface  ( SurfaceCapabilitiesKHR
                                                  , SurfaceFormatKHR
                                                  ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data SurfaceCapabilitiesKHR

instance ToCStruct SurfaceCapabilitiesKHR
instance Show SurfaceCapabilitiesKHR

instance FromCStruct SurfaceCapabilitiesKHR


data SurfaceFormatKHR

instance ToCStruct SurfaceFormatKHR
instance Show SurfaceFormatKHR

instance FromCStruct SurfaceFormatKHR

