{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( XcbSurfaceCreateFlagsKHR
  , withCStructXcbSurfaceCreateInfoKHR
  , fromCStructXcbSurfaceCreateInfoKHR
  , XcbSurfaceCreateInfoKHR(..)
  , createXcbSurfaceKHR
  , getPhysicalDeviceXcbPresentationSupportKHR
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
  , pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR(..)
  , VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  , Xcb_window_t
  , vkCreateXcbSurfaceKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "XcbSurfaceCreateFlagsKHR"
type XcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR


-- | VkXcbSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xcb surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkXcbSurfaceCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkXcbSurfaceCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "flags"
  flags :: XcbSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "connection"
  connection :: Ptr Xcb_connection_t
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "window"
  window :: Xcb_window_t
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkXcbSurfaceCreateInfoKHR' and
-- marshal a 'XcbSurfaceCreateInfoKHR' into it. The 'VkXcbSurfaceCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructXcbSurfaceCreateInfoKHR :: XcbSurfaceCreateInfoKHR -> (VkXcbSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructXcbSurfaceCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: XcbSurfaceCreateInfoKHR)) (\pPNext -> cont (VkXcbSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR pPNext (flags (marshalled :: XcbSurfaceCreateInfoKHR)) (connection (marshalled :: XcbSurfaceCreateInfoKHR)) (window (marshalled :: XcbSurfaceCreateInfoKHR))))

-- | A function to read a 'VkXcbSurfaceCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'XcbSurfaceCreateInfoKHR'.
fromCStructXcbSurfaceCreateInfoKHR :: VkXcbSurfaceCreateInfoKHR -> IO XcbSurfaceCreateInfoKHR
fromCStructXcbSurfaceCreateInfoKHR c = XcbSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkXcbSurfaceCreateInfoKHR)))
                                                               <*> pure (vkFlags (c :: VkXcbSurfaceCreateInfoKHR))
                                                               <*> pure (vkConnection (c :: VkXcbSurfaceCreateInfoKHR))
                                                               <*> pure (vkWindow (c :: VkXcbSurfaceCreateInfoKHR))

instance Zero XcbSurfaceCreateInfoKHR where
  zero = XcbSurfaceCreateInfoKHR Nothing
                                 zero
                                 zero
                                 zero



-- | vkCreateXcbSurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- X11 window, using the XCB client-side library
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.VkXcbSurfaceCreateInfoKHR'
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
-- Unresolved directive in vkCreateXcbSurfaceKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateXcbSurfaceKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createXcbSurfaceKHR :: Instance ->  XcbSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createXcbSurfaceKHR = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructXcbSurfaceCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateXcbSurfaceKHR commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))


-- | vkGetPhysicalDeviceXcbPresentationSupportKHR - Query physical device for
-- presentation to X11 server using XCB
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @connection@ is a pointer to an @xcb_connection_t@ to the X server.
--     @visual_id@ is an X11 visual (@xcb_visualid_t@).
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage
--
-- Unresolved directive in vkGetPhysicalDeviceXcbPresentationSupportKHR.txt
-- -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceXcbPresentationSupportKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceXcbPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Xcb_connection_t ->  Xcb_visualid_t ->  IO (VkBool32)
getPhysicalDeviceXcbPresentationSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \connection' -> \visual_id' -> vkGetPhysicalDeviceXcbPresentationSupportKHR commandTable physicalDevice' queueFamilyIndex' connection' visual_id' >>= (\ret -> pure ret)
