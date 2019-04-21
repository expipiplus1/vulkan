{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( CAMetalLayer
  , VkMetalSurfaceCreateFlagsEXT
  , VkMetalSurfaceCreateInfoEXT
  , FN_vkCreateMetalSurfaceEXT
  , PFN_vkCreateMetalSurfaceEXT
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
data CAMetalLayer

data VkMetalSurfaceCreateFlagsEXT

data VkMetalSurfaceCreateInfoEXT

type FN_vkCreateMetalSurfaceEXT = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateMetalSurfaceEXT = FunPtr FN_vkCreateMetalSurfaceEXT
