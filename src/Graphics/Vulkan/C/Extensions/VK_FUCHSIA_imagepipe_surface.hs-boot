{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateFlagsFUCHSIA
  , VkImagePipeSurfaceCreateInfoFUCHSIA
  , Zx_handle_t
  , FN_vkCreateImagePipeSurfaceFUCHSIA
  , PFN_vkCreateImagePipeSurfaceFUCHSIA
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data VkImagePipeSurfaceCreateFlagsFUCHSIA

data VkImagePipeSurfaceCreateInfoFUCHSIA

-- No documentation found for TopLevel "Zx_handle_t"
type Zx_handle_t = Word32
  

type FN_vkCreateImagePipeSurfaceFUCHSIA = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateImagePipeSurfaceFUCHSIA = FunPtr FN_vkCreateImagePipeSurfaceFUCHSIA
