{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( XcbSurfaceCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , XcbSurfaceCreateInfoKHR(..)
#endif
  , createXcbSurfaceKHR
  , getPhysicalDeviceXcbPresentationSupportKHR
  , pattern KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern KHR_XCB_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  , vkCreateXcbSurfaceKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  , pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( Xcb_window_t
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
  ( pattern STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "XcbSurfaceCreateFlagsKHR"
type XcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR


-- No complete pragma for XcbSurfaceCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkXcbSurfaceCreateInfoKHR"
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "flags"
  flags :: XcbSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "connection"
  connection :: Ptr Xcb_connection_t
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "window"
  window :: Xcb_window_t
  }
  deriving (Show, Eq)

instance Zero XcbSurfaceCreateInfoKHR where
  zero = XcbSurfaceCreateInfoKHR Nothing
                                 zero
                                 nullPtr
                                 zero

#endif


-- No documentation found for TopLevel "vkCreateXcbSurfaceKHR"
createXcbSurfaceKHR :: Instance ->  XcbSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createXcbSurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceXcbPresentationSupportKHR"
getPhysicalDeviceXcbPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Xcb_connection_t ->  Xcb_visualid_t ->  IO (VkBool32)
getPhysicalDeviceXcbPresentationSupportKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_EXTENSION_NAME"
pattern KHR_XCB_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_XCB_SURFACE_EXTENSION_NAME = VK_KHR_XCB_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_SPEC_VERSION"
pattern KHR_XCB_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_XCB_SURFACE_SPEC_VERSION = VK_KHR_XCB_SURFACE_SPEC_VERSION
