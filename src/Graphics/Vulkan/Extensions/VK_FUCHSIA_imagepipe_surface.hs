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


import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateFlagsFUCHSIA(..)
  , VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  , Zx_handle_t
  , vkCreateImagePipeSurfaceFUCHSIA
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


-- | VkImagePipeSurfaceCreateInfoFUCHSIA - Structure specifying parameters of
-- a newly created ImagePipe surface object
--
-- == Valid Usage
--
-- Unresolved directive in VkImagePipeSurfaceCreateInfoFUCHSIA.txt -
-- include::{generated}\/validity\/structs\/VkImagePipeSurfaceCreateInfoFUCHSIA.txt[]
--
-- = See Also
--
-- No cross-references are available
data ImagePipeSurfaceCreateInfoFUCHSIA = ImagePipeSurfaceCreateInfoFUCHSIA
  { -- Univalued member elided
  -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "flags"
  flags :: ImagePipeSurfaceCreateFlagsFUCHSIA
  , -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "imagePipeHandle"
  imagePipeHandle :: Zx_handle_t
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImagePipeSurfaceCreateInfoFUCHSIA' and
-- marshal a 'ImagePipeSurfaceCreateInfoFUCHSIA' into it. The 'VkImagePipeSurfaceCreateInfoFUCHSIA' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImagePipeSurfaceCreateInfoFUCHSIA :: ImagePipeSurfaceCreateInfoFUCHSIA -> (VkImagePipeSurfaceCreateInfoFUCHSIA -> IO a) -> IO a
withCStructImagePipeSurfaceCreateInfoFUCHSIA marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: ImagePipeSurfaceCreateInfoFUCHSIA)) (\pPNext -> cont (VkImagePipeSurfaceCreateInfoFUCHSIA VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA pPNext (flags (marshalled :: ImagePipeSurfaceCreateInfoFUCHSIA)) (imagePipeHandle (marshalled :: ImagePipeSurfaceCreateInfoFUCHSIA))))

-- | A function to read a 'VkImagePipeSurfaceCreateInfoFUCHSIA' and all additional
-- structures in the pointer chain into a 'ImagePipeSurfaceCreateInfoFUCHSIA'.
fromCStructImagePipeSurfaceCreateInfoFUCHSIA :: VkImagePipeSurfaceCreateInfoFUCHSIA -> IO ImagePipeSurfaceCreateInfoFUCHSIA
fromCStructImagePipeSurfaceCreateInfoFUCHSIA c = ImagePipeSurfaceCreateInfoFUCHSIA <$> -- Univalued Member elided
                                                                                   maybePeek peekVkStruct (castPtr (vkPNext (c :: VkImagePipeSurfaceCreateInfoFUCHSIA)))
                                                                                   <*> pure (vkFlags (c :: VkImagePipeSurfaceCreateInfoFUCHSIA))
                                                                                   <*> pure (vkImagePipeHandle (c :: VkImagePipeSurfaceCreateInfoFUCHSIA))

instance Zero ImagePipeSurfaceCreateInfoFUCHSIA where
  zero = ImagePipeSurfaceCreateInfoFUCHSIA Nothing
                                           zero
                                           zero



-- | vkCreateImagePipeSurfaceFUCHSIA - Create a
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceKHR' object for a
-- Fuchsia ImagePipe
--
-- = Parameters
--
-- -   @instance@ is the instance to associate with the surface.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface.VkImagePipeSurfaceCreateInfoFUCHSIA'
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
-- Unresolved directive in vkCreateImagePipeSurfaceFUCHSIA.txt -
-- include::{generated}\/validity\/protos\/vkCreateImagePipeSurfaceFUCHSIA.txt[]
--
-- = See Also
--
-- No cross-references are available
createImagePipeSurfaceFUCHSIA :: Instance ->  ImagePipeSurfaceCreateInfoFUCHSIA ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createImagePipeSurfaceFUCHSIA = \(Instance instance' commandTable) -> \createInfo' -> \allocator -> alloca (\pSurface' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructImagePipeSurfaceCreateInfoFUCHSIA marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateImagePipeSurfaceFUCHSIA commandTable instance' pCreateInfo' pAllocator pSurface' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pSurface')))))
