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
  , pattern KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern KHR_XCB_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR(..)
  , VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  , Xcb_window_t
  , vkCreateXcbSurfaceKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  , pattern VK_KHR_XCB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XCB_SURFACE_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "XcbSurfaceCreateFlagsKHR"
type XcbSurfaceCreateFlagsKHR = VkXcbSurfaceCreateFlagsKHR


-- No complete pragma for XcbSurfaceCreateFlagsKHR as it has no patterns


-- | VkXcbSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xcb surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.VkXcbSurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR'
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
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.VkXcbSurfaceCreateInfoKHR'
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.VkXcbSurfaceCreateInfoKHR'
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
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceXcbPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Xcb_connection_t ->  Xcb_visualid_t ->  IO (VkBool32)
getPhysicalDeviceXcbPresentationSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \connection' -> \visual_id' -> vkGetPhysicalDeviceXcbPresentationSupportKHR commandTable physicalDevice' queueFamilyIndex' connection' visual_id' >>= (\ret -> pure ret)

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_EXTENSION_NAME"
pattern KHR_XCB_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_XCB_SURFACE_EXTENSION_NAME = VK_KHR_XCB_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_XCB_SURFACE_SPEC_VERSION"
pattern KHR_XCB_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_XCB_SURFACE_SPEC_VERSION = VK_KHR_XCB_SURFACE_SPEC_VERSION
