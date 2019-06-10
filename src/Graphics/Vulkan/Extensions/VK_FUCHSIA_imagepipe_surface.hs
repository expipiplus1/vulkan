{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( ImagePipeSurfaceCreateFlagsFUCHSIA
#if defined(VK_USE_PLATFORM_GGP)
  , ImagePipeSurfaceCreateInfoFUCHSIA(..)
#endif
  , createImagePipeSurfaceFUCHSIA
  , pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  , pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
  , pattern STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
  ) where

import Data.String
  ( IsString
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateFlagsFUCHSIA(..)
  , vkCreateImagePipeSurfaceFUCHSIA
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME
  , pattern VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( Zx_handle_t
  )
#endif
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Instance(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceKHR
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
  )


-- No documentation found for TopLevel "ImagePipeSurfaceCreateFlagsFUCHSIA"
type ImagePipeSurfaceCreateFlagsFUCHSIA = VkImagePipeSurfaceCreateFlagsFUCHSIA


-- No complete pragma for ImagePipeSurfaceCreateFlagsFUCHSIA as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkImagePipeSurfaceCreateInfoFUCHSIA"
data ImagePipeSurfaceCreateInfoFUCHSIA = ImagePipeSurfaceCreateInfoFUCHSIA
  { -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "flags"
  flags :: ImagePipeSurfaceCreateFlagsFUCHSIA
  , -- No documentation found for Nested "ImagePipeSurfaceCreateInfoFUCHSIA" "imagePipeHandle"
  imagePipeHandle :: Zx_handle_t
  }
  deriving (Show, Eq)

instance Zero ImagePipeSurfaceCreateInfoFUCHSIA where
  zero = ImagePipeSurfaceCreateInfoFUCHSIA Nothing
                                           zero
                                           zero

#endif


-- No documentation found for TopLevel "vkCreateImagePipeSurfaceFUCHSIA"
createImagePipeSurfaceFUCHSIA :: Instance ->  ImagePipeSurfaceCreateInfoFUCHSIA ->  Maybe AllocationCallbacks ->  IO (SurfaceKHR)
createImagePipeSurfaceFUCHSIA = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- No documentation found for TopLevel "VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME"
pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME = VK_FUCHSIA_IMAGEPIPE_SURFACE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION"
pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION :: Integral a => a
pattern FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION = VK_FUCHSIA_IMAGEPIPE_SURFACE_SPEC_VERSION
