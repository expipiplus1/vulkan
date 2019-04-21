{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateFlagsMVK
  , VkMacOSSurfaceCreateInfoMVK
  , FN_vkCreateMacOSSurfaceMVK
  , PFN_vkCreateMacOSSurfaceMVK
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


data VkMacOSSurfaceCreateFlagsMVK

data VkMacOSSurfaceCreateInfoMVK

type FN_vkCreateMacOSSurfaceMVK = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateMacOSSurfaceMVK = FunPtr FN_vkCreateMacOSSurfaceMVK
