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
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  , pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
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
  ( createXlibSurfaceKHR
  , getPhysicalDeviceXlibPresentationSupportKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateFlagsKHR(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , VisualID
  , Window
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
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( pattern VK_KHR_XLIB_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_XLIB_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "XlibSurfaceCreateFlagsKHR"
type XlibSurfaceCreateFlagsKHR = VkXlibSurfaceCreateFlagsKHR
-- No documentation found for TopLevel "XlibSurfaceCreateInfoKHR"
data XlibSurfaceCreateInfoKHR = XlibSurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "flags"
  vkFlags :: XlibSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "dpy"
  vkDpy :: Ptr Display
  , -- No documentation found for Nested "XlibSurfaceCreateInfoKHR" "window"
  vkWindow :: Window
  }
  deriving (Show, Eq)
withCStructXlibSurfaceCreateInfoKHR :: XlibSurfaceCreateInfoKHR -> (VkXlibSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructXlibSurfaceCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: XlibSurfaceCreateInfoKHR)) (\pPNext -> cont (VkXlibSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: XlibSurfaceCreateInfoKHR)) (vkDpy (from :: XlibSurfaceCreateInfoKHR)) (vkWindow (from :: XlibSurfaceCreateInfoKHR))))
fromCStructXlibSurfaceCreateInfoKHR :: VkXlibSurfaceCreateInfoKHR -> IO XlibSurfaceCreateInfoKHR
fromCStructXlibSurfaceCreateInfoKHR c = XlibSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                 maybePeek peekVkStruct (castPtr (vkPNext (c :: VkXlibSurfaceCreateInfoKHR)))
                                                                 <*> pure (vkFlags (c :: VkXlibSurfaceCreateInfoKHR))
                                                                 <*> pure (vkDpy (c :: VkXlibSurfaceCreateInfoKHR))
                                                                 <*> pure (vkWindow (c :: VkXlibSurfaceCreateInfoKHR))

-- | Wrapper for vkCreateXlibSurfaceKHR
createXlibSurfaceKHR :: Instance ->  XlibSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createXlibSurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructXlibSurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createXlibSurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))

-- | Wrapper for vkGetPhysicalDeviceXlibPresentationSupportKHR
getPhysicalDeviceXlibPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  VisualID ->  IO (VkBool32, Display)
getPhysicalDeviceXlibPresentationSupportKHR = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyIndex -> \visualID -> alloca (\pDpy -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceXlibPresentationSupportKHR commandTable physicalDevice queueFamilyIndex pDpy visualID >>= (\r -> (,) <$> pure r<*>peek pDpy))
