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
  , pattern VK_EXT_METAL_SURFACE_SPEC_VERSION
  , pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
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
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createMetalSurfaceEXT
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateFlagsEXT(..)
  , VkMetalSurfaceCreateInfoEXT(..)
  , CAMetalLayer
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
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( pattern VK_EXT_METAL_SURFACE_EXTENSION_NAME
  , pattern VK_EXT_METAL_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "MetalSurfaceCreateFlagsEXT"
type MetalSurfaceCreateFlagsEXT = VkMetalSurfaceCreateFlagsEXT
-- No documentation found for TopLevel "MetalSurfaceCreateInfoEXT"
data MetalSurfaceCreateInfoEXT = MetalSurfaceCreateInfoEXT
  { -- Univalued Member elided
  -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "flags"
  vkFlags :: MetalSurfaceCreateFlagsEXT
  , -- No documentation found for Nested "MetalSurfaceCreateInfoEXT" "pLayer"
  vkPLayer :: CAMetalLayer
  }
  deriving (Show, Eq)
withCStructMetalSurfaceCreateInfoEXT :: MetalSurfaceCreateInfoEXT -> (VkMetalSurfaceCreateInfoEXT -> IO a) -> IO a
withCStructMetalSurfaceCreateInfoEXT from cont = with (vkPLayer (from :: MetalSurfaceCreateInfoEXT)) (\pLayer -> maybeWith withSomeVkStruct (vkPNext (from :: MetalSurfaceCreateInfoEXT)) (\pPNext -> cont (VkMetalSurfaceCreateInfoEXT VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT pPNext (vkFlags (from :: MetalSurfaceCreateInfoEXT)) pLayer)))
fromCStructMetalSurfaceCreateInfoEXT :: VkMetalSurfaceCreateInfoEXT -> IO MetalSurfaceCreateInfoEXT
fromCStructMetalSurfaceCreateInfoEXT c = MetalSurfaceCreateInfoEXT <$> -- Univalued Member elided
                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkMetalSurfaceCreateInfoEXT)))
                                                                   <*> pure (vkFlags (c :: VkMetalSurfaceCreateInfoEXT))
                                                                   <*> peek (vkPLayer (c :: VkMetalSurfaceCreateInfoEXT))

-- | Wrapper for vkCreateMetalSurfaceEXT
createMetalSurfaceEXT :: Instance ->  MetalSurfaceCreateInfoEXT ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createMetalSurfaceEXT = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructMetalSurfaceCreateInfoEXT a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createMetalSurfaceEXT commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
