{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_metal_surface  (MetalSurfaceCreateInfoEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data MetalSurfaceCreateInfoEXT

instance ToCStruct MetalSurfaceCreateInfoEXT
instance Show MetalSurfaceCreateInfoEXT

instance FromCStruct MetalSurfaceCreateInfoEXT

