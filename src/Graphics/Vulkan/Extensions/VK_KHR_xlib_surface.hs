{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( XlibSurfaceCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , XlibSurfaceCreateInfoKHR(..)
#endif
  , createXlibSurfaceKHR
  , getPhysicalDeviceXlibPresentationSupportKHR
  , pattern KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  )

#if defined(VK_USE_PLATFORM_GGP)
import Foreign.Ptr
  ( nullPtr
  )
#endif
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateFlagsKHR(..)
  , VisualID
  , vkCreateXlibSurfaceKHR
  , vkGetPhysicalDeviceXlibPresentationSupportKHR
  , pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Window
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "XlibSurfaceCreateFlagsKHR"
type XlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR


-- No complete pragma for XlibSurfaceCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkXlibSurfaceCreateInfoKHR"
data XlibSurfaceCreateInfoKHR = XlibSurfaceCreateInfoKHR
  { -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "flags"
  flags :: XlibSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "dpy"
  dpy :: Ptr Display
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "window"
  window :: Window
  }
  deriving (Show, Eq)

instance Zero XlibSurfaceCreateInfoKHR where
  zero = XlibSurfaceCreateInfoKHR Nothing
                                  zero
                                  nullPtr
                                  zero

#endif


-- No documentation found for TopLevel "vkCreateXlibSurfaceKHR"
createXlibSurfaceKHR :: Instance ->  XlibSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createXlibSurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceXlibPresentationSupportKHR"
getPhysicalDeviceXlibPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Display ->  VisualID ->  IO (VkBool32)
getPhysicalDeviceXlibPresentationSupportKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_EXTENSION_NAME"
pattern KHR_XLIB_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_XLIB_SURFACE_EXTENSION_NAME = VK_KHR_XLIB_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_SPEC_VERSION"
pattern KHR_XLIB_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_XLIB_SURFACE_SPEC_VERSION = VK_KHR_XLIB_SURFACE_SPEC_VERSION
