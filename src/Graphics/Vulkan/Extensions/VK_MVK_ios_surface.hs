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
  , pattern MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern MVK_IOS_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
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
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK(..)
  , VkIOSSurfaceCreateInfoMVK(..)
  , vkCreateIOSSurfaceMVK
  , pattern VK_MVK_IOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_IOS_SURFACE_SPEC_VERSION
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
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  )


-- No documentation found for TopLevel "IOSSurfaceCreateFlagsMVK"
type IOSSurfaceCreateFlagsMVK = VkIOSSurfaceCreateFlagsMVK


-- No complete pragma for IOSSurfaceCreateFlagsMVK as it has no patterns


-- | VkIOSSurfaceCreateInfoMVK - Structure specifying parameters of a newly
-- created iOS surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.VkIOSSurfaceCreateFlagsMVK',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK'
data IOSSurfaceCreateInfoMVK = IOSSurfaceCreateInfoMVK
  { -- Univalued member elided
  -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "flags"
  flags :: IOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "IOSSurfaceCreateInfoMVK" "pView"
  view :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkIOSSurfaceCreateInfoMVK' and
-- marshal a 'IOSSurfaceCreateInfoMVK' into it. The 'VkIOSSurfaceCreateInfoMVK' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructIOSSurfaceCreateInfoMVK :: IOSSurfaceCreateInfoMVK -> (VkIOSSurfaceCreateInfoMVK -> IO a) -> IO a
withCStructIOSSurfaceCreateInfoMVK marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: IOSSurfaceCreateInfoMVK)) (\pPNext -> cont (VkIOSSurfaceCreateInfoMVK VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK pPNext (flags (marshalled :: IOSSurfaceCreateInfoMVK)) (view (marshalled :: IOSSurfaceCreateInfoMVK))))

-- | A function to read a 'VkIOSSurfaceCreateInfoMVK' and all additional
-- structures in the pointer chain into a 'IOSSurfaceCreateInfoMVK'.
fromCStructIOSSurfaceCreateInfoMVK :: VkIOSSurfaceCreateInfoMVK -> IO IOSSurfaceCreateInfoMVK
fromCStructIOSSurfaceCreateInfoMVK c = IOSSurfaceCreateInfoMVK <$> -- Univalued Member elided
                                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkIOSSurfaceCreateInfoMVK)))
                                                               <*> pure (vkFlags (c :: VkIOSSurfaceCreateInfoMVK))
                                                               <*> pure (vkPView (c :: VkIOSSurfaceCreateInfoMVK))

instance Zero IOSSurfaceCreateInfoMVK where
  zero = IOSSurfaceCreateInfoMVK Nothing
                                 zero
                                 zero



-- | vkCreateIOSSurfaceMVK - Create a VkSurfaceKHR object for an iOS UIView
--
-- = Parameters
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.VkIOSSurfaceCreateInfoMVK'
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
--     'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.VkIOSSurfaceCreateInfoMVK'
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
--     -   'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VK_ERROR_NATIVE_WINDOW_IN_USE_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.VkIOSSurfaceCreateInfoMVK',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
createIOSSurfaceMVK :: Instance ->  IOSSurfaceCreateInfoMVK ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createIOSSurfaceMVK = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructIOSSurfaceCreateInfoMVK marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateIOSSurfaceMVK commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_EXTENSION_NAME"
pattern MVK_IOS_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern MVK_IOS_SURFACE_EXTENSION_NAME = VK_MVK_IOS_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_MVK_IOS_SURFACE_SPEC_VERSION"
pattern MVK_IOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern MVK_IOS_SURFACE_SPEC_VERSION = VK_MVK_IOS_SURFACE_SPEC_VERSION
