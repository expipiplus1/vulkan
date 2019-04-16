{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( WaylandSurfaceCreateFlagsKHR
  , withCStructWaylandSurfaceCreateInfoKHR
  , fromCStructWaylandSurfaceCreateInfoKHR
  , WaylandSurfaceCreateInfoKHR(..)
  , createWaylandSurfaceKHR
  , getPhysicalDeviceWaylandPresentationSupportKHR
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
  , pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.Word
  ( Word32
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createWaylandSurfaceKHR
  , getPhysicalDeviceWaylandPresentationSupportKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateFlagsKHR(..)
  , VkWaylandSurfaceCreateInfoKHR(..)
  , Wl_display
  , Wl_surface
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  , PhysicalDevice(..)
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "WaylandSurfaceCreateFlagsKHR"
type WaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR
-- No documentation found for TopLevel "WaylandSurfaceCreateInfoKHR"
data WaylandSurfaceCreateInfoKHR = WaylandSurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "flags"
  vkFlags :: WaylandSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "display"
  vkDisplay :: Ptr Wl_display
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "surface"
  vkSurface :: Ptr Wl_surface
  }
  deriving (Show, Eq)
withCStructWaylandSurfaceCreateInfoKHR :: WaylandSurfaceCreateInfoKHR -> (VkWaylandSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructWaylandSurfaceCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: WaylandSurfaceCreateInfoKHR)) (\pPNext -> cont (VkWaylandSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: WaylandSurfaceCreateInfoKHR)) (vkDisplay (from :: WaylandSurfaceCreateInfoKHR)) (vkSurface (from :: WaylandSurfaceCreateInfoKHR))))
fromCStructWaylandSurfaceCreateInfoKHR :: VkWaylandSurfaceCreateInfoKHR -> IO WaylandSurfaceCreateInfoKHR
fromCStructWaylandSurfaceCreateInfoKHR c = WaylandSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWaylandSurfaceCreateInfoKHR)))
                                                                       <*> pure (vkFlags (c :: VkWaylandSurfaceCreateInfoKHR))
                                                                       <*> pure (vkDisplay (c :: VkWaylandSurfaceCreateInfoKHR))
                                                                       <*> pure (vkSurface (c :: VkWaylandSurfaceCreateInfoKHR))

-- | Wrapper for vkCreateWaylandSurfaceKHR
createWaylandSurfaceKHR :: Instance ->  WaylandSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createWaylandSurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructWaylandSurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createWaylandSurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))

-- | Wrapper for vkGetPhysicalDeviceWaylandPresentationSupportKHR
getPhysicalDeviceWaylandPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Wl_display ->  IO (VkBool32)
getPhysicalDeviceWaylandPresentationSupportKHR = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyIndex -> \display -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceWaylandPresentationSupportKHR commandTable physicalDevice queueFamilyIndex display >>= (\r -> pure r)
