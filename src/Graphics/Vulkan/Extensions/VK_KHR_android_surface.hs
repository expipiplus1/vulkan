{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( AndroidSurfaceCreateFlagsKHR
  , withCStructAndroidSurfaceCreateInfoKHR
  , fromCStructAndroidSurfaceCreateInfoKHR
  , AndroidSurfaceCreateInfoKHR(..)
  , createAndroidSurfaceKHR
  , pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION
  , pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  , ANativeWindow
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
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
  ( createAndroidSurfaceKHR
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateFlagsKHR(..)
  , VkAndroidSurfaceCreateInfoKHR(..)
  , ANativeWindow
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
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
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( pattern VK_KHR_ANDROID_SURFACE_EXTENSION_NAME
  , pattern VK_KHR_ANDROID_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "AndroidSurfaceCreateFlagsKHR"
type AndroidSurfaceCreateFlagsKHR = VkAndroidSurfaceCreateFlagsKHR
-- No documentation found for TopLevel "AndroidSurfaceCreateInfoKHR"
data AndroidSurfaceCreateInfoKHR = AndroidSurfaceCreateInfoKHR
  { -- Univalued Member elided
  -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "flags"
  vkFlags :: AndroidSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "window"
  vkWindow :: Ptr ANativeWindow
  }
  deriving (Show, Eq)
withCStructAndroidSurfaceCreateInfoKHR :: AndroidSurfaceCreateInfoKHR -> (VkAndroidSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructAndroidSurfaceCreateInfoKHR from cont = maybeWith withSomeVkStruct (vkPNext (from :: AndroidSurfaceCreateInfoKHR)) (\pPNext -> cont (VkAndroidSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR pPNext (vkFlags (from :: AndroidSurfaceCreateInfoKHR)) (vkWindow (from :: AndroidSurfaceCreateInfoKHR))))
fromCStructAndroidSurfaceCreateInfoKHR :: VkAndroidSurfaceCreateInfoKHR -> IO AndroidSurfaceCreateInfoKHR
fromCStructAndroidSurfaceCreateInfoKHR c = AndroidSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidSurfaceCreateInfoKHR)))
                                                                       <*> pure (vkFlags (c :: VkAndroidSurfaceCreateInfoKHR))
                                                                       <*> pure (vkWindow (c :: VkAndroidSurfaceCreateInfoKHR))
instance Zero AndroidSurfaceCreateInfoKHR where
  zero = AndroidSurfaceCreateInfoKHR Nothing
                                     zero
                                     zero

-- | Wrapper for 'vkCreateAndroidSurfaceKHR'
createAndroidSurfaceKHR :: Instance ->  AndroidSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createAndroidSurfaceKHR = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructAndroidSurfaceCreateInfoKHR a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createAndroidSurfaceKHR commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
