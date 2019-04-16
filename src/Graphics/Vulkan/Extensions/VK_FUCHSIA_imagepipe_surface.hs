{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( ImagePipeSurfaceCreateFlagsFUCHSIA
  , withCStructImagePipeSurfaceCreateInfoFUCHSIA
  , fromCStructImagePipeSurfaceCreateInfoFUCHSIA
  , ImagePipeSurfaceCreateInfoFUCHSIA(..)
  , createImagePipeSurfaceFUCHSIA
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
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
  ( createImagePipeSurfaceFUCHSIA
  )


import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateFlagsFUCHSIA(..)
  , VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  , Zx_handle_t
  , pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
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
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
  )


-- No documentation found for TopLevel "ImagePipeSurfaceCreateFlagsFUCHSIA"
type ImagePipeSurfaceCreateFlagsFUCHSIA = VkImagePipeSurfaceCreateFlagsFUCHSIA
-- No documentation found for TopLevel "ImagePipeSurfaceCreateInfoFUCHSIA"
data ImagePipeSurfaceCreateInfoFUCHSIA = ImagePipeSurfaceCreateInfoFUCHSIA
  { -- Univalued Member elided
  -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "flags"
  vkFlags :: ImagePipeSurfaceCreateFlagsFUCHSIA
  , -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "imagePipeHandle"
  vkImagePipeHandle :: Zx_handle_t
  }
  deriving (Show, Eq)
withCStructImagePipeSurfaceCreateInfoFUCHSIA :: ImagePipeSurfaceCreateInfoFUCHSIA -> (VkImagePipeSurfaceCreateInfoFUCHSIA -> IO a) -> IO a
withCStructImagePipeSurfaceCreateInfoFUCHSIA from cont = maybeWith withSomeVkStruct (vkPNext (from :: ImagePipeSurfaceCreateInfoFUCHSIA)) (\pPNext -> cont (VkImagePipeSurfaceCreateInfoFUCHSIA VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA pPNext (vkFlags (from :: ImagePipeSurfaceCreateInfoFUCHSIA)) (vkImagePipeHandle (from :: ImagePipeSurfaceCreateInfoFUCHSIA))))
fromCStructImagePipeSurfaceCreateInfoFUCHSIA :: VkImagePipeSurfaceCreateInfoFUCHSIA -> IO ImagePipeSurfaceCreateInfoFUCHSIA
fromCStructImagePipeSurfaceCreateInfoFUCHSIA c = ImagePipeSurfaceCreateInfoFUCHSIA <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImagePipeSurfaceCreateInfoFUCHSIA)))
                                                                                   <*> pure (vkFlags (c :: VkImagePipeSurfaceCreateInfoFUCHSIA))
                                                                                   <*> pure (vkImagePipeHandle (c :: VkImagePipeSurfaceCreateInfoFUCHSIA))

-- | Wrapper for 'vkCreateImagePipeSurfaceFUCHSIA'
createImagePipeSurfaceFUCHSIA :: Instance ->  ImagePipeSurfaceCreateInfoFUCHSIA ->  Maybe AllocationCallbacks ->  IO ( SurfaceKHR )
createImagePipeSurfaceFUCHSIA = \(Instance instance' commandTable) -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructImagePipeSurfaceCreateInfoFUCHSIA a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createImagePipeSurfaceFUCHSIA commandTable instance' pCreateInfo pAllocator pSurface >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pSurface)))))
