{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_headless_surface  (HeadlessSurfaceCreateInfoEXT) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data HeadlessSurfaceCreateInfoEXT

instance ToCStruct HeadlessSurfaceCreateInfoEXT
instance Show HeadlessSurfaceCreateInfoEXT

instance FromCStruct HeadlessSurfaceCreateInfoEXT

