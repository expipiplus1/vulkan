{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateFlagsMVK
  , withCStructMacOSSurfaceCreateInfoMVK
  , fromCStructMacOSSurfaceCreateInfoMVK
  , MacOSSurfaceCreateInfoMVK(..)
  , createMacOSSurfaceMVK
  , pattern MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern MVK_MACOS_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
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
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateFlagsMVK(..)
  , VkMacOSSurfaceCreateInfoMVK(..)
  , vkCreateMacOSSurfaceMVK
  , pattern VK_MVK_MACOS_SURFACE_EXTENSION_NAME
  , pattern VK_MVK_MACOS_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
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
  ( pattern STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  )


-- No documentation found for TopLevel "MacOSSurfaceCreateFlagsMVK"
type MacOSSurfaceCreateFlagsMVK = VkMacOSSurfaceCreateFlagsMVK


-- No complete pragma for MacOSSurfaceCreateFlagsMVK as it has no patterns


-- | VkMacOSSurfaceCreateInfoMVK - Structure specifying parameters of a newly
-- created macOS surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.VkMacOSSurfaceCreateFlagsMVK',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK'
data MacOSSurfaceCreateInfoMVK = MacOSSurfaceCreateInfoMVK
  { -- Univalued member elided
  -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "flags"
  flags :: MacOSSurfaceCreateFlagsMVK
  , -- No documentation found for Nested "MacOSSurfaceCreateInfoMVK" "pView"
  view :: Ptr ()
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMacOSSurfaceCreateInfoMVK' and
-- marshal a 'MacOSSurfaceCreateInfoMVK' into it. The 'VkMacOSSurfaceCreateInfoMVK' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMacOSSurfaceCreateInfoMVK :: MacOSSurfaceCreateInfoMVK -> (VkMacOSSurfaceCreateInfoMVK -> IO a) -> IO a
withCStructMacOSSurfaceCreateInfoMVK marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MacOSSurfaceCreateInfoMVK)) (\pPNext -> cont (VkMacOSSurfaceCreateInfoMVK VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK pPNext (flags (marshalled :: MacOSSurfaceCreateInfoMVK)) (view (marshalled :: MacOSSurfaceCreateInfoMVK))))

-- | A function to read a 'VkMacOSSurfaceCreateInfoMVK' and all additional
-- structures in the pointer chain into a 'MacOSSurfaceCreateInfoMVK'.
fromCStructMacOSSurfaceCreateInfoMVK :: VkMacOSSurfaceCreateInfoMVK -> IO MacOSSurfaceCreateInfoMVK
fromCStructMacOSSurfaceCreateInfoMVK c = MacOSSurfaceCreateInfoMVK <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMacOSSurfaceCreateInfoMVK)))
                                                                   <*> pure (vkFlags (c :: VkMacOSSurfaceCreateInfoMVK))
                                                                   <*> pure (vkPView (c :: VkMacOSSurfaceCreateInfoMVK))

instance Zero MacOSSurfaceCreateInfoMVK where
  zero = MacOSSurfaceCreateInfoMVK Nothing
                                   zero
                                   zero



-- | vkCreateMacOSSurfaceMVK - Create a VkSurfaceKHR object for a macOS
-- NSView
--
-- = Parameters
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.VkMacOSSurfaceCreateInfoMVK'
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
--     'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.VkMacOSSurfaceCreateInfoMVK'
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.VkMacOSSurfaceCreateInfoMVK',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
createMacOSSurfaceMVK :: Instance ->  MacOSSurfaceCreateInfoMVK ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createMacOSSurfaceMVK = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructMacOSSurfaceCreateInfoMVK marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateMacOSSurfaceMVK commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_EXTENSION_NAME"
pattern MVK_MACOS_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern MVK_MACOS_SURFACE_EXTENSION_NAME = VK_MVK_MACOS_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_MVK_MACOS_SURFACE_SPEC_VERSION"
pattern MVK_MACOS_SURFACE_SPEC_VERSION :: Integral a => a
pattern MVK_MACOS_SURFACE_SPEC_VERSION = VK_MVK_MACOS_SURFACE_SPEC_VERSION
