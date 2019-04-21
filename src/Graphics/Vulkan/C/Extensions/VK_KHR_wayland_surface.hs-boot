{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateFlagsKHR
  , VkWaylandSurfaceCreateInfoKHR
  , Wl_display
  , Wl_surface
  , FN_vkCreateWaylandSurfaceKHR
  , PFN_vkCreateWaylandSurfaceKHR
  , FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
  , PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
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
  ( VkBool32
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkInstance
  , VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data VkWaylandSurfaceCreateFlagsKHR

data VkWaylandSurfaceCreateInfoKHR

-- | Opaque data
data Wl_display

-- | Opaque data
data Wl_surface

type FN_vkCreateWaylandSurfaceKHR = ("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult
type PFN_vkCreateWaylandSurfaceKHR = FunPtr FN_vkCreateWaylandSurfaceKHR

type FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32
type PFN_vkGetPhysicalDeviceWaylandPresentationSupportKHR = FunPtr FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
