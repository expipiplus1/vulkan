{-# language CPP #-}
module Vulkan.Extensions.VK_MVK_macos_surface  (MacOSSurfaceCreateInfoMVK) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data MacOSSurfaceCreateInfoMVK

instance ToCStruct MacOSSurfaceCreateInfoMVK
instance Show MacOSSurfaceCreateInfoMVK

instance FromCStruct MacOSSurfaceCreateInfoMVK

