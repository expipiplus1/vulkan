{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( Win32SurfaceCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , Win32SurfaceCreateInfoKHR(..)
#endif
  , createWin32SurfaceKHR
  , getPhysicalDeviceWin32PresentationSupportKHR
  , pattern KHR_WIN32_SURFACE_EXTENSION_NAME
  , pattern KHR_WIN32_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateFlagsKHR(..)
  , vkCreateWin32SurfaceKHR
  , vkGetPhysicalDeviceWin32PresentationSupportKHR
  , pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( HINSTANCE
  , HWND
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
  ( pattern STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "Win32SurfaceCreateFlagsKHR"
type Win32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR


-- No complete pragma for Win32SurfaceCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWin32SurfaceCreateInfoKHR"
data Win32SurfaceCreateInfoKHR = Win32SurfaceCreateInfoKHR
  { -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "flags"
  flags :: Win32SurfaceCreateFlagsKHR
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "hinstance"
  hinstance :: HINSTANCE
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "hwnd"
  hwnd :: HWND
  }
  deriving (Show, Eq)

instance Zero Win32SurfaceCreateInfoKHR where
  zero = Win32SurfaceCreateInfoKHR Nothing
                                   zero
                                   zero
                                   zero

#endif


-- No documentation found for TopLevel "vkCreateWin32SurfaceKHR"
createWin32SurfaceKHR :: Instance ->  Win32SurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createWin32SurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceWin32PresentationSupportKHR"
getPhysicalDeviceWin32PresentationSupportKHR :: PhysicalDevice ->  Word32 ->  IO (VkBool32)
getPhysicalDeviceWin32PresentationSupportKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_EXTENSION_NAME"
pattern KHR_WIN32_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_WIN32_SURFACE_EXTENSION_NAME = VK_KHR_WIN32_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_WIN32_SURFACE_SPEC_VERSION"
pattern KHR_WIN32_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_WIN32_SURFACE_SPEC_VERSION = VK_KHR_WIN32_SURFACE_SPEC_VERSION
