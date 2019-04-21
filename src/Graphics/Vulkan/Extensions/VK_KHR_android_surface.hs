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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateFlagsKHR(..)
  , VkAndroidSurfaceCreateInfoKHR(..)
  , ANativeWindow
  , vkCreateAndroidSurfaceKHR
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


-- | VkAndroidSurfaceCreateInfoKHR - Structure specifying parameters of a
-- newly created Android surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkAndroidSurfaceCreateInfoKHR.txt -
-- include::{generated}\/validity\/structs\/VkAndroidSurfaceCreateInfoKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
data AndroidSurfaceCreateInfoKHR = AndroidSurfaceCreateInfoKHR
  { -- Univalued member elided
  -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "flags"
  flags :: AndroidSurfaceCreateFlagsKHR
  , -- No documentation found for Nested "AndroidSurfaceCreateInfoKHR" "window"
  window :: Ptr ANativeWindow
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAndroidSurfaceCreateInfoKHR' and
-- marshal a 'AndroidSurfaceCreateInfoKHR' into it. The 'VkAndroidSurfaceCreateInfoKHR' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAndroidSurfaceCreateInfoKHR :: AndroidSurfaceCreateInfoKHR -> (VkAndroidSurfaceCreateInfoKHR -> IO a) -> IO a
withCStructAndroidSurfaceCreateInfoKHR marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: AndroidSurfaceCreateInfoKHR)) (\pPNext -> cont (VkAndroidSurfaceCreateInfoKHR VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR pPNext (flags (marshalled :: AndroidSurfaceCreateInfoKHR)) (window (marshalled :: AndroidSurfaceCreateInfoKHR))))

-- | A function to read a 'VkAndroidSurfaceCreateInfoKHR' and all additional
-- structures in the pointer chain into a 'AndroidSurfaceCreateInfoKHR'.
fromCStructAndroidSurfaceCreateInfoKHR :: VkAndroidSurfaceCreateInfoKHR -> IO AndroidSurfaceCreateInfoKHR
fromCStructAndroidSurfaceCreateInfoKHR c = AndroidSurfaceCreateInfoKHR <$> -- Univalued Member elided
                                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkAndroidSurfaceCreateInfoKHR)))
                                                                       <*> pure (vkFlags (c :: VkAndroidSurfaceCreateInfoKHR))
                                                                       <*> pure (vkWindow (c :: VkAndroidSurfaceCreateInfoKHR))

instance Zero AndroidSurfaceCreateInfoKHR where
  zero = AndroidSurfaceCreateInfoKHR Nothing
                                     zero
                                     zero



-- | vkCreateAndroidSurfaceKHR - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for an
-- Android native window
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.VkAndroidSurfaceCreateInfoKHR'
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
-- During the lifetime of a surface created using a particular
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.ANativeWindow'
-- handle any attempts to create another surface for the same
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.ANativeWindow' and
-- any attempts to connect to the same
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.ANativeWindow'
-- through other platform mechanisms will fail.
--
-- __Note__
--
-- In particular, only one
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' /can/ exist
-- at a time for a given window. Similarly, a native window /cannot/ be
-- used by both a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' and
-- @EGLSurface@ simultaneously.
--
-- If successful,
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR'
-- increments the
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.ANativeWindow'’s
-- reference count, and
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkDestroySurfaceKHR' will
-- decrement it.
--
-- On Android, when a swapchain’s @imageExtent@ does not match the
-- surface’s @currentExtent@, the presentable images will be scaled to the
-- surface’s dimensions during presentation. @minImageExtent@ is (1,1), and
-- @maxImageExtent@ is the maximum image size supported by the consumer.
-- For the system compositor, @currentExtent@ is the window size (i.e. the
-- consumer’s preferred size).
--
-- Unresolved directive in vkCreateAndroidSurfaceKHR.txt -
-- include::{generated}\/validity\/protos\/vkCreateAndroidSurfaceKHR.txt[]
--
-- = See Also
--
-- No cross-references are available
createAndroidSurfaceKHR :: Instance ->  AndroidSurfaceCreateInfoKHR ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createAndroidSurfaceKHR = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructAndroidSurfaceCreateInfoKHR marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateAndroidSurfaceKHR commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))
