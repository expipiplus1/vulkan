{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_xlib_surface  ( XlibSurfaceCreateInfoKHR
                                              , Display
                                              , VisualID
                                              ) where

import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data XlibSurfaceCreateInfoKHR

instance ToCStruct XlibSurfaceCreateInfoKHR
instance Show XlibSurfaceCreateInfoKHR

instance FromCStruct XlibSurfaceCreateInfoKHR


type Display = Ptr ()


type VisualID = Word64

