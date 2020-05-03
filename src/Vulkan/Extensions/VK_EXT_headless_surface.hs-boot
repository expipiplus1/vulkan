{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_headless_surface  (HeadlessSurfaceCreateInfoEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data HeadlessSurfaceCreateInfoEXT

instance ToCStruct HeadlessSurfaceCreateInfoEXT
instance Show HeadlessSurfaceCreateInfoEXT

instance FromCStruct HeadlessSurfaceCreateInfoEXT

