{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_directfb_surface"
module Vulkan.Extensions.VK_EXT_directfb_surface  ( DirectFBSurfaceCreateInfoEXT
                                                  , IDirectFB
                                                  ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DirectFBSurfaceCreateInfoEXT

instance ToCStruct DirectFBSurfaceCreateInfoEXT
instance Show DirectFBSurfaceCreateInfoEXT

instance FromCStruct DirectFBSurfaceCreateInfoEXT


data IDirectFB

