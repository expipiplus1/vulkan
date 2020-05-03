{-# language CPP #-}
module Vulkan.Extensions.VK_NN_vi_surface  (ViSurfaceCreateInfoNN) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data ViSurfaceCreateInfoNN

instance ToCStruct ViSurfaceCreateInfoNN
instance Show ViSurfaceCreateInfoNN

instance FromCStruct ViSurfaceCreateInfoNN

