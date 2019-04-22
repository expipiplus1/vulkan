{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_EXT_headless_surface
  ( HeadlessSurfaceCreateFlagsEXT
  , withCStructHeadlessSurfaceCreateInfoEXT
  , fromCStructHeadlessSurfaceCreateInfoEXT
  , HeadlessSurfaceCreateInfoEXT(..)
  , createHeadlessSurfaceEXT
  , pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME
  , pattern EXT_HEADLESS_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
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
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateFlagsEXT(..)
  , VkHeadlessSurfaceCreateInfoEXT(..)
  , vkCreateHeadlessSurfaceEXT
  , pattern VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME
  , pattern VK_EXT_HEADLESS_SURFACE_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
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
  ( pattern STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
  )


-- No documentation found for TopLevel "HeadlessSurfaceCreateFlagsEXT"
type HeadlessSurfaceCreateFlagsEXT = VkHeadlessSurfaceCreateFlagsEXT


-- No complete pragma for HeadlessSurfaceCreateFlagsEXT as it has no patterns


-- | VkHeadlessSurfaceCreateInfoEXT - Structure specifying parameters of a
-- newly created headless surface object
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.VkHeadlessSurfaceCreateFlagsEXT',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.vkCreateHeadlessSurfaceEXT'
data HeadlessSurfaceCreateInfoEXT = HeadlessSurfaceCreateInfoEXT
  { -- Univalued member elided
  -- No documentation found for Nested "HeadlessSurfaceCreateInfoEXT" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "HeadlessSurfaceCreateInfoEXT" "flags"
  flags :: HeadlessSurfaceCreateFlagsEXT
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkHeadlessSurfaceCreateInfoEXT' and
-- marshal a 'HeadlessSurfaceCreateInfoEXT' into it. The 'VkHeadlessSurfaceCreateInfoEXT' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructHeadlessSurfaceCreateInfoEXT :: HeadlessSurfaceCreateInfoEXT -> (VkHeadlessSurfaceCreateInfoEXT -> IO a) -> IO a
withCStructHeadlessSurfaceCreateInfoEXT marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: HeadlessSurfaceCreateInfoEXT)) (\pPNext -> cont (VkHeadlessSurfaceCreateInfoEXT VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT pPNext (flags (marshalled :: HeadlessSurfaceCreateInfoEXT))))

-- | A function to read a 'VkHeadlessSurfaceCreateInfoEXT' and all additional
-- structures in the pointer chain into a 'HeadlessSurfaceCreateInfoEXT'.
fromCStructHeadlessSurfaceCreateInfoEXT :: VkHeadlessSurfaceCreateInfoEXT -> IO HeadlessSurfaceCreateInfoEXT
fromCStructHeadlessSurfaceCreateInfoEXT c = HeadlessSurfaceCreateInfoEXT <$> -- Univalued Member elided
                                                                         maybePeek peekVkStruct (castPtr (vkPNext (c :: VkHeadlessSurfaceCreateInfoEXT)))
                                                                         <*> pure (vkFlags (c :: VkHeadlessSurfaceCreateInfoEXT))

instance Zero HeadlessSurfaceCreateInfoEXT where
  zero = HeadlessSurfaceCreateInfoEXT Nothing
                                      zero



-- | vkCreateHeadlessSurfaceEXT - Create a headless
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object
--
-- = Parameters
--
-- -   @instance@ is the instance to associate the surface with.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.VkHeadlessSurfaceCreateInfoEXT'
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
--     'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.VkHeadlessSurfaceCreateInfoEXT'
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
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.VkHeadlessSurfaceCreateInfoEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR'
createHeadlessSurfaceEXT :: Instance ->  HeadlessSurfaceCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createHeadlessSurfaceEXT = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructHeadlessSurfaceCreateInfoEXT marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateHeadlessSurfaceEXT commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))

-- No documentation found for TopLevel "VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME"
pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_HEADLESS_SURFACE_EXTENSION_NAME = VK_EXT_HEADLESS_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_HEADLESS_SURFACE_SPEC_VERSION"
pattern EXT_HEADLESS_SURFACE_SPEC_VERSION :: Integral a => a
pattern EXT_HEADLESS_SURFACE_SPEC_VERSION = VK_EXT_HEADLESS_SURFACE_SPEC_VERSION
