{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( WaylandSurfaceCreateFlagsKHR
#if defined(VK_USE_PLATFORM_GGP)
  , WaylandSurfaceCreateInfoKHR(..)
#endif
  , createWaylandSurfaceKHR
  , getPhysicalDeviceWaylandPresentationSupportKHR
  , pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern KHR_WAYLAND_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateFlagsKHR(..)
  , Wl_display
  , vkCreateWaylandSurfaceKHR
  , vkGetPhysicalDeviceWaylandPresentationSupportKHR
  , pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( Wl_surface
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
  ( pattern STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "WaylandSurfaceCreateFlagsKHR"
type WaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR


-- No complete pragma for WaylandSurfaceCreateFlagsKHR as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkWaylandSurfaceCreateInfoKHR"
data WaylandSurfaceCreateInfoKHR = WaylandSurfaceCreateInfoKHR
  { -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "flags"
  flags :: WaylandSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "display"
  display :: Ptr Wl_display
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "surface"
  surface :: Ptr Wl_surface
  }
  deriving (Show, Eq)

instance Zero WaylandSurfaceCreateInfoKHR where
  zero = WaylandSurfaceCreateInfoKHR Nothing
                                     zero
                                     nullPtr
                                     nullPtr

#endif


-- No documentation found for TopLevel "vkCreateWaylandSurfaceKHR"
createWaylandSurfaceKHR :: Instance ->  WaylandSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createWaylandSurfaceKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetPhysicalDeviceWaylandPresentationSupportKHR"
getPhysicalDeviceWaylandPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Wl_display ->  IO (VkBool32)
getPhysicalDeviceWaylandPresentationSupportKHR = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME"
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME = VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_SPEC_VERSION"
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION = VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
