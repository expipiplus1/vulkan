{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_xcb_surface  ( XcbSurfaceCreateInfoKHR
                                             , Xcb_connection_t
                                             , Xcb_visualid_t
                                             ) where

import Data.Word (Word32)
import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data XcbSurfaceCreateInfoKHR

instance ToCStruct XcbSurfaceCreateInfoKHR
instance Show XcbSurfaceCreateInfoKHR

instance FromCStruct XcbSurfaceCreateInfoKHR


data Xcb_connection_t


type Xcb_visualid_t = Word32

