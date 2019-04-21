{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( ANativeWindow
  , VkAndroidSurfaceCreateFlagsKHR
  , VkAndroidSurfaceCreateInfoKHR
  , FN_vkCreateAndroidSurfaceKHR
  , PFN_vkCreateAndroidSurfaceKHR
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


-- | Opaque data
data ANativeWindow

data VkAndroidSurfaceCreateFlagsKHR

data VkAndroidSurfaceCreateInfoKHR

type FN_vkCreateAndroidSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateAndroidSurfaceKHR = FunPtr FN_vkCreateAndroidSurfaceKHR
