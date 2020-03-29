{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_MVK_macos_surface  (MacOSSurfaceCreateInfoMVK) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data MacOSSurfaceCreateInfoMVK

instance ToCStruct MacOSSurfaceCreateInfoMVK
instance Show MacOSSurfaceCreateInfoMVK

instance FromCStruct MacOSSurfaceCreateInfoMVK

