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
import qualified Graphics.Vulkan.C.Dynamic
  ( createWin32SurfaceKHR
  , getPhysicalDeviceWin32PresentationSupportKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateFlagsKHR(..)
  , VkWin32SurfaceCreateInfoKHR(..)
  , HINSTANCE
  , HWND
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
-- No documentation found for TopLevel "Win32SurfaceCreateInfoKHR"
data Win32SurfaceCreateInfoKHR = Win32SurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "flags"
  vkFlags :: Win32SurfaceCreateFlagsKHR
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "hinstance"
  vkHinstance :: HINSTANCE
  , -- No documentation found for Nested "Win32SurfaceCreateInfoKHR" "hwnd"
  vkHwnd :: HWND
  }
  deriving (Show, Eq)
withCStructWin32SurfaceCreateInfoKHR :: Win32SurfaceCreateInfoKHR -> (VkWin32SurfaceCreateInfoKHR -> IO a) -> IO a
withCStructWin32SurfaceCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: Win32SurfaceCreateInfoKHR)) (\pPNext -> cont (VkWin32SurfaceCreateInfoKHR VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: Win32SurfaceCreateInfoKHR)) (vkHinstance (from :: Win32SurfaceCreateInfoKHR)) (vkHwnd (from :: Win32SurfaceCreateInfoKHR))))
fromCStructWin32SurfaceCreateInfoKHR :: VkWin32SurfaceCreateInfoKHR -> IO Win32SurfaceCreateInfoKHR
fromCStructWin32SurfaceCreateInfoKHR c = Win32SurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkWin32SurfaceCreateInfoKHR)))
                                                                   <*> pure (vkFlags (c :: VkWin32SurfaceCreateInfoKHR))
                                                                   <*> pure (vkHinstance (c :: VkWin32SurfaceCreateInfoKHR))
                                                                   <*> pure (vkHwnd (c :: VkWin32SurfaceCreateInfoKHR))

-- | Wrapper for vkCreateWin32SurfaceKHR
createWin32SurfaceKHR :: Instance ->  Win32SurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createWin32SurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructWin32SurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createWin32SurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))

-- | Wrapper for vkGetPhysicalDeviceWin32PresentationSupportKHR
getPhysicalDeviceWin32PresentationSupportKHR :: PhysicalDevice ->  Word32 ->  IO (VkBool32)
getPhysicalDeviceWin32PresentationSupportKHR = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyIndex -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceWin32PresentationSupportKHR commandTable physicalDevice queueFamilyIndex >>= (\r -> pure r)
