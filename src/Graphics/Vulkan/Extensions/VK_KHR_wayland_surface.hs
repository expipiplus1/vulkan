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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateFlagsKHR(..)
  , VkWaylandSurfaceCreateInfoKHR(..)
  , Wl_display
  , Wl_surface
  , vkCreateWaylandSurfaceKHR
  , vkGetPhysicalDeviceWaylandPresentationSupportKHR
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


-- | VkWaylandSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Wayland surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkWaylandSurfaceCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkWaylandSurfaceCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data WaylandSurfaceCreateInfoKHR = WaylandSurfaceCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "flags"
  flags :: WaylandSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "display"
  display :: Ptr Wl_display
  , -- No documentation found for Nested "WaylandSurfaceCreateInfoKHR" "surface"
  surface :: Ptr Wl_surface
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkWaylandSurfaceCreateInfoKHR' and
-- marshal a 'WaylandSurfaceCreateInfoKHR' into it. The 'VkWaylandSurfaceCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructWaylandSurfaceCreateInfoKHR :: WaylandSurfaceCreateInfoKHR -> (VkWaylandSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructWaylandSurfaceCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: WaylandSurfaceCreateInfoKHR)) (\pPNext -> cont (VkWaylandSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR pPNext (flags (marshalled :: WaylandSurfaceCreateInfoKHR)) (display (marshalled :: WaylandSurfaceCreateInfoKHR)) (surface (marshalled :: WaylandSurfaceCreateInfoKHR))))

-- | A function to read a 'VkWaylandSurfaceCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'WaylandSurfaceCreateInfoKHR'.
fromCStructWaylandSurfaceCreateInfoKHR :: VkWaylandSurfaceCreateInfoKHR -> IO WaylandSurfaceCreateInfoKHR
fromCStructWaylandSurfaceCreateInfoKHR c = WaylandSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWaylandSurfaceCreateInfoKHR)))
                                                                       <*> pure (vkFlags (c :: VkWaylandSurfaceCreateInfoKHR))
                                                                       <*> pure (vkDisplay (c :: VkWaylandSurfaceCreateInfoKHR))
                                                                       <*> pure (vkSurface (c :: VkWaylandSurfaceCreateInfoKHR))

instance Zero WaylandSurfaceCreateInfoKHR where
  zero = WaylandSurfaceCreateInfoKHR Nothing
                                     zero
                                     zero
                                     zero



-- | vkCreateWaylandSurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Wayland window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.VkWaylandSurfaceCreateInfoKHR'
--     structure containing parameters affecting the creation of the
--     surface object.
--
-- -   @pAllocator@ is the allocator used for host memory allocated for the
--     surface object when there is no more specific allocator available
--     (see
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>).
--
-- -   @pSurface@ points to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle in
--     which the created surface object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateWaylandSurfaceKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateWaylandSurfaceKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createWaylandSurfaceKHR :: Instance ->  WaylandSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createWaylandSurfaceKHR = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructWaylandSurfaceCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateWaylandSurfaceKHR commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))


-- | vkGetPhysicalDeviceWaylandPresentationSupportKHR - Query physical device
-- for presentation to Wayland
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @display@ is a pointer to the @wl_display@ associated with a Wayland
--     compositor.
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage
--
-- Unresolved directive in
-- vkGetPhysicalDeviceWaylandPresentationSupportKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceWaylandPresentationSupportKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceWaylandPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Wl_display ->  IO (VkBool32)
getPhysicalDeviceWaylandPresentationSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \display' -> vkGetPhysicalDeviceWaylandPresentationSupportKHR commandTable physicalDevice' queueFamilyIndex' display' >>= (\ret -> pure ret)
