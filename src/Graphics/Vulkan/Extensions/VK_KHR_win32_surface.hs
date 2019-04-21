{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( Win32SurfaceCreateFlagsKHR
  , withCStructWin32SurfaceCreateInfoKHR
  , fromCStructWin32SurfaceCreateInfoKHR
  , Win32SurfaceCreateInfoKHR(..)
  , createWin32SurfaceKHR
  , getPhysicalDeviceWin32PresentationSupportKHR
  , pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION
  , pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
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
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateFlagsKHR(..)
  , VkWin32SurfaceCreateInfoKHR(..)
  , HINSTANCE
  , HWND
  , vkCreateWin32SurfaceKHR
  , vkGetPhysicalDeviceWin32PresentationSupportKHR
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( pattern VK_KHR_WIN32_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_WIN32_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "Win32SurfaceCreateFlagsKHR"
type Win32SurfaceCreateFlagsKHR = VkWin32SurfaceCreateFlagsKHR


-- | VkWin32SurfaceCreateInfoKHR - Structure specifying parameters of a newly
-- created Win32 surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkWin32SurfaceCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkWin32SurfaceCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data Win32SurfaceCreateInfoKHR = Win32SurfaceCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "flags"
  flags :: Win32SurfaceCreateFlagsKHR
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "hinstance"
  hinstance :: HINSTANCE
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "hwnd"
  hwnd :: HWND
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkWin32SurfaceCreateInfoKHR' and
-- marshal a 'Win32SurfaceCreateInfoKHR' into it. The 'VkWin32SurfaceCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructWin32SurfaceCreateInfoKHR :: Win32SurfaceCreateInfoKHR -> (VkWin32SurfaceCreateInfoKHR -> IO a) -> IO a
withCStructWin32SurfaceCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: Win32SurfaceCreateInfoKHR)) (\pPNext -> cont (VkWin32SurfaceCreateInfoKHR VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR pPNext (flags (marshalled :: Win32SurfaceCreateInfoKHR)) (hinstance (marshalled :: Win32SurfaceCreateInfoKHR)) (hwnd (marshalled :: Win32SurfaceCreateInfoKHR))))

-- | A function to read a 'VkWin32SurfaceCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'Win32SurfaceCreateInfoKHR'.
fromCStructWin32SurfaceCreateInfoKHR :: VkWin32SurfaceCreateInfoKHR -> IO Win32SurfaceCreateInfoKHR
fromCStructWin32SurfaceCreateInfoKHR c = Win32SurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWin32SurfaceCreateInfoKHR)))
                                                                   <*> pure (vkFlags (c :: VkWin32SurfaceCreateInfoKHR))
                                                                   <*> pure (vkHinstance (c :: VkWin32SurfaceCreateInfoKHR))
                                                                   <*> pure (vkHwnd (c :: VkWin32SurfaceCreateInfoKHR))

instance Zero Win32SurfaceCreateInfoKHR where
  zero = Win32SurfaceCreateInfoKHR Nothing
                                   zero
                                   zero
                                   zero



-- | vkCreateWin32SurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- Win32 native window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.VkWin32SurfaceCreateInfoKHR'
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
-- Unresolved directive in vkCreateWin32SurfaceKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateWin32SurfaceKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createWin32SurfaceKHR :: Instance ->  Win32SurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createWin32SurfaceKHR = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructWin32SurfaceCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateWin32SurfaceKHR commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))


-- | vkGetPhysicalDeviceWin32PresentationSupportKHR - query queue family
-- support for presentation on a Win32 display
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device.
--
-- -   @queueFamilyIndex@ is the queue family index.
--
-- = Description
--
-- This platform-specific function /can/ be called prior to creating a
-- surface.
--
-- == Valid Usage
--
-- Unresolved directive in
-- vkGetPhysicalDeviceWin32PresentationSupportKHR.txt -
-- include::{generated}\/validity\/protos\/vkGetPhysicalDeviceWin32PresentationSupportKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
getPhysicalDeviceWin32PresentationSupportKHR :: PhysicalDevice ->  Word32 ->  IO (VkBool32)
getPhysicalDeviceWin32PresentationSupportKHR = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyIndex' -> vkGetPhysicalDeviceWin32PresentationSupportKHR commandTable physicalDevice' queueFamilyIndex' >>= (\ret -> pure ret)
