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
import qualified Graphics.Vulkan.C.Dynamic
  ( createXcbSurfaceKHR
  , getPhysicalDeviceXcbPresentationSupportKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR(..)
  , VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  , Xcb_window_t
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
-- No documentation found for TopLevel "XcbSurfaceCreateInfoKHR"
data XcbSurfaceCreateInfoKHR = XcbSurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "flags"
  vkFlags :: XcbSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "connection"
  vkConnection :: Ptr Xcb_connection_t
  , -- No documentation found for Nested "XcbSurfaceCreateInfoKHR" "window"
  vkWindow :: Xcb_window_t
  }
  deriving (Show, Eq)
withCStructXcbSurfaceCreateInfoKHR :: XcbSurfaceCreateInfoKHR -> (VkXcbSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructXcbSurfaceCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: XcbSurfaceCreateInfoKHR)) (\pPNext -> cont (VkXcbSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: XcbSurfaceCreateInfoKHR)) (vkConnection (from :: XcbSurfaceCreateInfoKHR)) (vkWindow (from :: XcbSurfaceCreateInfoKHR))))
fromCStructXcbSurfaceCreateInfoKHR :: VkXcbSurfaceCreateInfoKHR -> IO XcbSurfaceCreateInfoKHR
fromCStructXcbSurfaceCreateInfoKHR c = XcbSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkXcbSurfaceCreateInfoKHR)))
                                                               <*> pure (vkFlags (c :: VkXcbSurfaceCreateInfoKHR))
                                                               <*> pure (vkConnection (c :: VkXcbSurfaceCreateInfoKHR))
                                                               <*> pure (vkWindow (c :: VkXcbSurfaceCreateInfoKHR))

-- | Wrapper for 'vkCreateXcbSurfaceKHR'
createXcbSurfaceKHR :: Instance ->  XcbSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO ( SurfaceKHR )
createXcbSurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructXcbSurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createXcbSurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))

-- | Wrapper for 'vkGetPhysicalDeviceXcbPresentationSupportKHR'
getPhysicalDeviceXcbPresentationSupportKHR :: PhysicalDevice ->  Word32 ->  Ptr Xcb_connection_t ->  Xcb_visualid_t ->  IO ( VkBool32 )
getPhysicalDeviceXcbPresentationSupportKHR = \(PhysicalDevice physicalDevice commandTable) -> \queueFamilyIndex -> \connection -> \visual_id -> Graphics.Vulkan.C.Dynamic.getPhysicalDeviceXcbPresentationSupportKHR commandTable physicalDevice queueFamilyIndex connection visual_id >>= (\r -> pure r)
