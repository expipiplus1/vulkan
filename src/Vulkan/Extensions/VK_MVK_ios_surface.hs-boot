{-# language CPP #-}
module Vulkan.Extensions.VK_MVK_ios_surface  (IOSSurfaceCreateInfoMVK) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data IOSSurfaceCreateInfoMVK

instance ToCStruct IOSSurfaceCreateInfoMVK
instance Show IOSSurfaceCreateInfoMVK

instance FromCStruct IOSSurfaceCreateInfoMVK

