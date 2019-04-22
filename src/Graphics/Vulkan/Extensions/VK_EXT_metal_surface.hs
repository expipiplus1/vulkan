{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( MetalSurfaceCreateFlagsEXT
  , withCStructMetalSurfaceCreateInfoEXT
  , fromCStructMetalSurfaceCreateInfoEXT
  , MetalSurfaceCreateInfoEXT(..)
  , createMetalSurfaceEXT
  , pattern EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern EXT_METAL_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateFlagsEXT(..)
  , VkMetalSurfaceCreateInfoEXT(..)
  , CAMetalLayer
  , vkCreateMetalSurfaceEXT
  , pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern VK_EXT_METAL_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
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
  ( pattern STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
  )


-- No documentation found for TopLevel "MetalSurfaceCreateFlagsEXT"
type MetalSurfaceCreateFlagsEXT = VkMetalSurfaceCreateFlagsEXT


-- No complete pragma for MetalSurfaceCreateFlagsEXT as it has no patterns


-- | VkMetalSurfaceCreateInfoEXT - Structure specifying parameters of a newly
-- created Metal surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.VkMetalSurfaceCreateFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.vkCreateMetalSurfaceEXT'
data MetalSurfaceCreateInfoEXT = MetalSurfaceCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "flags"
  flags :: MetalSurfaceCreateFlagsEXT
  , -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "pLayer"
  layer :: Ptr CAMetalLayer
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMetalSurfaceCreateInfoEXT' and
-- marshal a 'MetalSurfaceCreateInfoEXT' into it. The 'VkMetalSurfaceCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMetalSurfaceCreateInfoEXT :: MetalSurfaceCreateInfoEXT -> (VkMetalSurfaceCreateInfoEXT -> IO a) -> IO a
withCStructMetalSurfaceCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: MetalSurfaceCreateInfoEXT)) (\pPNext -> cont (VkMetalSurfaceCreateInfoEXT VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT pPNext (flags (marshalled :: MetalSurfaceCreateInfoEXT)) (layer (marshalled :: MetalSurfaceCreateInfoEXT))))

-- | A function to read a 'VkMetalSurfaceCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'MetalSurfaceCreateInfoEXT'.
fromCStructMetalSurfaceCreateInfoEXT :: VkMetalSurfaceCreateInfoEXT -> IO MetalSurfaceCreateInfoEXT
fromCStructMetalSurfaceCreateInfoEXT c = MetalSurfaceCreateInfoEXT <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMetalSurfaceCreateInfoEXT)))
                                                                   <*> pure (vkFlags (c :: VkMetalSurfaceCreateInfoEXT))
                                                                   <*> pure (vkPLayer (c :: VkMetalSurfaceCreateInfoEXT))

instance Zero MetalSurfaceCreateInfoEXT where
  zero = MetalSurfaceCreateInfoEXT Nothing
                                   zero
                                   zero



-- | vkCreateMetalSurfaceEXT - Create a VkSurfaceKHR object for CAMetalLayer
--
-- = Parameters
--
-- -   @instance@ is the instance with which to associate the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.VkMetalSurfaceCreateInfoEXT'
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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.VkMetalSurfaceCreateInfoEXT'
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
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.VkMetalSurfaceCreateInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
createMetalSurfaceEXT :: Instance ->  MetalSurfaceCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createMetalSurfaceEXT = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructMetalSurfaceCreateInfoEXT marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateMetalSurfaceEXT commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))

-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_EXTENSION_NAME"
pattern EXT_METAL_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_METAL_SURFACE_EXTENSION_NAME = VK_EXT_METAL_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_METAL_SURFACE_SPEC_VERSION"
pattern EXT_METAL_SURFACE_SPEC_VERSION :: Integral a => a
pattern EXT_METAL_SURFACE_SPEC_VERSION = VK_EXT_METAL_SURFACE_SPEC_VERSION
