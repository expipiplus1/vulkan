{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_metal_surface  (MetalSurfaceCreateInfoEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data MetalSurfaceCreateInfoEXT

instance ToCStruct MetalSurfaceCreateInfoEXT
instance Show MetalSurfaceCreateInfoEXT

instance FromCStruct MetalSurfaceCreateInfoEXT

