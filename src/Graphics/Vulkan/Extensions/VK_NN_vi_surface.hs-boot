{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NN_vi_surface  (ViSurfaceCreateInfoNN) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data ViSurfaceCreateInfoNN

instance ToCStruct ViSurfaceCreateInfoNN
instance Show ViSurfaceCreateInfoNN

instance FromCStruct ViSurfaceCreateInfoNN

