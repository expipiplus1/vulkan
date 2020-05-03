{-# language CPP #-}
module Vulkan.Extensions.WSITypes  ( HINSTANCE
                                   , HWND
                                   , HMONITOR
                                   , HANDLE
                                   , DWORD
                                   , LPCWSTR
                                   , Display
                                   , VisualID
                                   , Window
                                   , RROutput
                                   , Xcb_visualid_t
                                   , Xcb_window_t
                                   , Zx_handle_t
                                   , GgpStreamDescriptor
                                   , GgpFrameToken
                                   , SECURITY_ATTRIBUTES
                                   , Xcb_connection_t
                                   , Wl_display
                                   , Wl_surface
                                   , CAMetalLayer
                                   , AHardwareBuffer
                                   , ANativeWindow
                                   ) where

import Foreign.C.Types (CWchar)
import Foreign.Ptr (Ptr)
import Data.Word (Word32)
import Data.Word (Word64)

type HINSTANCE = Ptr ()


type HWND = Ptr ()


type HMONITOR = Ptr ()


type HANDLE = Ptr ()


type DWORD = Word32


type LPCWSTR = Ptr CWchar


type Display = Ptr ()


type VisualID = Word64


type Window = Word64


type RROutput = Word64


type Xcb_visualid_t = Word32


type Xcb_window_t = Word32


type Zx_handle_t = Word32


type GgpStreamDescriptor = Word32


type GgpFrameToken = Word32


data SECURITY_ATTRIBUTES


data Xcb_connection_t


data Wl_display


data Wl_surface


data CAMetalLayer


data AHardwareBuffer


data ANativeWindow

