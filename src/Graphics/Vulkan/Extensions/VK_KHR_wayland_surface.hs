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
  , pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern KHR_WAYLAND_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
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
  , pattern VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "WaylandSurfaceCreateFlagsKHR"
type WaylandSurfaceCreateFlagsKHR = VkWaylandSurfaceCreateFlagsKHR


-- No complete pragma for WaylandSurfaceCreateFlagsKHR as it has no patterns


-- | VkWaylandSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Wayland surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.VkWaylandSurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR'
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
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.VkWaylandSurfaceCreateInfoKHR'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pSurface@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.VkWaylandSurfaceCreateInfoKHR'
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceWaylandPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Wl_display ->  IO (VkBool32)
getPhysicalDeviceWaylandPresentationSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \display' -> vkGetPhysicalDeviceWaylandPresentationSupportKHR commandTable physicalDevice' queueFamilyIndex' display' >>= (\ret -> pure ret)

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME"
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_WAYLAND_SURFACE_EXTENSION_NAME = VK_KHR_WAYLAND_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_WAYLAND_SURFACE_SPEC_VERSION"
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_WAYLAND_SURFACE_SPEC_VERSION = VK_KHR_WAYLAND_SURFACE_SPEC_VERSION
