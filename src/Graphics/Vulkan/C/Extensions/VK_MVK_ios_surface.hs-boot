{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK
  , VkIOSSurfaceCreateInfoMVK
  , FN_vkCreateIOSSurfaceMVK
  , PFN_vkCreateIOSSurfaceMVK
  ) where

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


data VkIOSSurfaceCreateFlagsMVK

data VkIOSSurfaceCreateInfoMVK

type FN_vkCreateIOSSurfaceMVK = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateIOSSurfaceMVK = FunPtr FN_vkCreateIOSSurfaceMVK
