{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateFlagsEXT
  , VkHeadlessSurfaceCreateInfoEXT
  , FN_vkCreateHeadlessSurfaceEXT
  , PFN_vkCreateHeadlessSurfaceEXT
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


data VkHeadlessSurfaceCreateFlagsEXT

data VkHeadlessSurfaceCreateInfoEXT

type FN_vkCreateHeadlessSurfaceEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkHeadlessSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateHeadlessSurfaceEXT = FunPtr FN_vkCreateHeadlessSurfaceEXT
