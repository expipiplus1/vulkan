{-# language CPP #-}
module Vulkan.Extensions.WSITypes  ( AHardwareBuffer
                                   , Display
                                   , HANDLE
                                   , RROutput
                                   , VisualID
                                   , Wl_display
                                   , Xcb_connection_t
                                   , Xcb_visualid_t
                                   ) where

import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)

data AHardwareBuffer


type Display = Ptr ()


type HANDLE = Ptr ()


type RROutput = Word64


type VisualID = Word64


data Wl_display


data Xcb_connection_t


type Xcb_visualid_t = Word32

