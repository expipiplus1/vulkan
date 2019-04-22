{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( XlibSurfaceCreateFlagsKHR
  , withCStructXlibSurfaceCreateInfoKHR
  , fromCStructXlibSurfaceCreateInfoKHR
  , XlibSurfaceCreateInfoKHR(..)
  , createXlibSurfaceKHR
  , getPhysicalDeviceXlibPresentationSupportKHR
  , pattern KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateFlagsKHR(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , VisualID
  , Window
  , vkCreateXlibSurfaceKHR
  , vkGetPhysicalDeviceXlibPresentationSupportKHR
  , pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
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
  ( pattern STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  )


-- No documentation found for TopLevel "XlibSurfaceCreateFlagsKHR"
type XlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR


-- No complete pragma for XlibSurfaceCreateFlagsKHR as it has no patterns


-- | VkXlibSurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Xlib surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.VkXlibSurfaceCreateFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR'
data XlibSurfaceCreateInfoKHR = XlibSurfaceCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "flags"
  flags :: XlibSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "dpy"
  dpy :: Ptr Display
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "window"
  window :: Window
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkXlibSurfaceCreateInfoKHR' and
-- marshal a 'XlibSurfaceCreateInfoKHR' into it. The 'VkXlibSurfaceCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructXlibSurfaceCreateInfoKHR :: XlibSurfaceCreateInfoKHR -> (VkXlibSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructXlibSurfaceCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: XlibSurfaceCreateInfoKHR)) (\pPNext -> cont (VkXlibSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR pPNext (flags (marshalled :: XlibSurfaceCreateInfoKHR)) (dpy (marshalled :: XlibSurfaceCreateInfoKHR)) (window (marshalled :: XlibSurfaceCreateInfoKHR))))

-- | A function to read a 'VkXlibSurfaceCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'XlibSurfaceCreateInfoKHR'.
fromCStructXlibSurfaceCreateInfoKHR :: VkXlibSurfaceCreateInfoKHR -> IO XlibSurfaceCreateInfoKHR
fromCStructXlibSurfaceCreateInfoKHR c = XlibSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkXlibSurfaceCreateInfoKHR)))
                                                                 <*> pure (vkFlags (c :: VkXlibSurfaceCreateInfoKHR))
                                                                 <*> pure (vkDpy (c :: VkXlibSurfaceCreateInfoKHR))
                                                                 <*> pure (vkWindow (c :: VkXlibSurfaceCreateInfoKHR))

instance Zero XlibSurfaceCreateInfoKHR where
  zero = XlibSurfaceCreateInfoKHR Nothing
                                  zero
                                  zero
                                  zero



-- | vkCreateXlibSurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- X11 window, using the Xlib client-side library
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.VkXlibSurfaceCreateInfoKHR'
--     structure containing the parameters affecting the creation of the
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
--     'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.VkXlibSurfaceCreateInfoKHR'
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.VkXlibSurfaceCreateInfoKHR'
createXlibSurfaceKHR :: Instance ->  XlibSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createXlibSurfaceKHR = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructXlibSurfaceCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateXlibSurfaceKHR commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))


-- | vkGetPhysicalDeviceXlibPresentationSupportKHR - Query physical device
-- for presentation to X11 server using Xlib
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- -   @dpy@ is a pointer to an Xlib
--     'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.Display'
--     connection to the server.
--
-- -   @visualId@ is an X11 visual
--     ('Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.VisualID').
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
getPhysicalDeviceXlibPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  VisualID ->  IO (VkBool32, Display)
getPhysicalDeviceXlibPresentationSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> \visualID' -> alloca (\pDpy' -> vkGetPhysicalDeviceXlibPresentationSupportKHR commandTable physicalDevice' queueFamilyIndex' pDpy' visualID' >>= (\ret -> (,) <$> pure ret<*>peek pDpy'))

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_EXTENSION_NAME"
pattern KHR_XLIB_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_XLIB_SURFACE_EXTENSION_NAME = VK_KHR_XLIB_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_XLIB_SURFACE_SPEC_VERSION"
pattern KHR_XLIB_SURFACE_SPEC_VERSION :: Integral a => a
pattern KHR_XLIB_SURFACE_SPEC_VERSION = VK_KHR_XLIB_SURFACE_SPEC_VERSION
