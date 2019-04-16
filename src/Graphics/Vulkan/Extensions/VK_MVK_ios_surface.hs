{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( IOSSurfaceCreateFlagsMVK
  , withCStructIOSSurfaceCreateInfoMVK
  , fromCStructIOSSurfaceCreateInfoMVK
  , IOSSurfaceCreateInfoMVK(..)
  , createIOSSurfaceMVK
  , pattern VK_MVK_IOS_SURFACE_SPEC_VERSION
  , pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
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
  ( createIOSSurfaceMVK
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK(..)
  , VkIOSSurfaceCreateInfoMVK(..)
  , pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
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
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_IOS_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "IOSSurfaceCreateFlagsMVK"
type IOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK
-- No documentation found for TopLevel "IOSSurfaceCreateInfoMVK"
data IOSSurfaceCreateInfoMVK = IOSSurfaceCreateInfoMVK
  { -- Univalued Member elided
  -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "flags"
  vkFlags :: IOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "pView"
  vkPView :: Ptr ()
  }
  deriving (Show, Eq)
withCStructIOSSurfaceCreateInfoMVK :: IOSSurfaceCreateInfoMVK -> (VkIOSSurfaceCreateInfoMVK -> IO a) -> IO a
withCStructIOSSurfaceCreateInfoMVK from cont = maybeWith withSomeVkStruct (vkPNext (from :: IOSSurfaceCreateInfoMVK)) (\pPNext -> cont (VkIOSSurfaceCreateInfoMVK VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK pPNext (vkFlags (from :: IOSSurfaceCreateInfoMVK)) (vkPView (from :: IOSSurfaceCreateInfoMVK))))
fromCStructIOSSurfaceCreateInfoMVK :: VkIOSSurfaceCreateInfoMVK -> IO IOSSurfaceCreateInfoMVK
fromCStructIOSSurfaceCreateInfoMVK c = IOSSurfaceCreateInfoMVK <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkIOSSurfaceCreateInfoMVK)))
                                                               <*> pure (vkFlags (c :: VkIOSSurfaceCreateInfoMVK))
                                                               <*> pure (vkPView (c :: VkIOSSurfaceCreateInfoMVK))

-- | Wrapper for vkCreateIOSSurfaceMVK
createIOSSurfaceMVK :: Instance ->  IOSSurfaceCreateInfoMVK ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createIOSSurfaceMVK = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructIOSSurfaceCreateInfoMVK a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createIOSSurfaceMVK commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
