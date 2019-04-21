{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( HMONITOR
  , VkFullScreenExclusiveEXT
  , VkSurfaceCapabilitiesFullScreenExclusiveEXT
  , VkSurfaceFullScreenExclusiveInfoEXT
  , VkSurfaceFullScreenExclusiveWin32InfoEXT
  , FN_vkAcquireFullScreenExclusiveModeEXT
  , PFN_vkAcquireFullScreenExclusiveModeEXT
  , FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , FN_vkReleaseFullScreenExclusiveModeEXT
  , PFN_vkReleaseFullScreenExclusiveModeEXT
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
  ( VkDevice
  , VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkPresentModeKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


-- No documentation found for TopLevel "HMONITOR"
type HMONITOR = Ptr ()
  

data VkFullScreenExclusiveEXT

data VkSurfaceCapabilitiesFullScreenExclusiveEXT

data VkSurfaceFullScreenExclusiveInfoEXT

data VkSurfaceFullScreenExclusiveWin32InfoEXT

type FN_vkAcquireFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkAcquireFullScreenExclusiveModeEXT = FunPtr FN_vkAcquireFullScreenExclusiveModeEXT

type FN_vkGetPhysicalDeviceSurfacePresentModes2EXT = ("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult
type PFN_vkGetPhysicalDeviceSurfacePresentModes2EXT = FunPtr FN_vkGetPhysicalDeviceSurfacePresentModes2EXT

type FN_vkReleaseFullScreenExclusiveModeEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkReleaseFullScreenExclusiveModeEXT = FunPtr FN_vkReleaseFullScreenExclusiveModeEXT
