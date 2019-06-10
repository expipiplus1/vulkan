{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR
  , VkBindImageMemorySwapchainInfoKHR
  , VkDeviceGroupPresentCapabilitiesKHR
  , VkDeviceGroupPresentInfoKHR
  , VkDeviceGroupPresentModeFlagBitsKHR
  , VkDeviceGroupPresentModeFlagsKHR
  , VkDeviceGroupSwapchainCreateInfoKHR
  , VkImageSwapchainCreateInfoKHR
  , VkPresentInfoKHR
  , VkSwapchainCreateFlagBitsKHR
  , VkSwapchainCreateFlagsKHR
  , VkSwapchainCreateInfoKHR
  , VkSwapchainKHR
  , FN_vkAcquireNextImage2KHR
  , PFN_vkAcquireNextImage2KHR
  , FN_vkAcquireNextImageKHR
  , PFN_vkAcquireNextImageKHR
  , FN_vkCreateSwapchainKHR
  , PFN_vkCreateSwapchainKHR
  , FN_vkDestroySwapchainKHR
  , PFN_vkDestroySwapchainKHR
  , FN_vkGetDeviceGroupPresentCapabilitiesKHR
  , PFN_vkGetDeviceGroupPresentCapabilitiesKHR
  , FN_vkGetDeviceGroupSurfacePresentModesKHR
  , PFN_vkGetDeviceGroupSurfacePresentModesKHR
  , FN_vkGetPhysicalDevicePresentRectanglesKHR
  , PFN_vkGetPhysicalDevicePresentRectanglesKHR
  , FN_vkGetSwapchainImagesKHR
  , PFN_vkGetSwapchainImagesKHR
  , FN_vkQueuePresentKHR
  , PFN_vkQueuePresentKHR
  ) where

import Data.Word
  ( Word32
  , Word64
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
  , VkDevice
  , VkPhysicalDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkImage
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkRect2D
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  , VkQueue
  , VkSemaphore
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceKHR
  )


data VkAcquireNextImageInfoKHR

data VkBindImageMemorySwapchainInfoKHR

data VkDeviceGroupPresentCapabilitiesKHR

data VkDeviceGroupPresentInfoKHR

data VkDeviceGroupPresentModeFlagBitsKHR

-- No documentation found for TopLevel "VkDeviceGroupPresentModeFlagsKHR"
type VkDeviceGroupPresentModeFlagsKHR = VkDeviceGroupPresentModeFlagBitsKHR

data VkDeviceGroupSwapchainCreateInfoKHR

data VkImageSwapchainCreateInfoKHR

data VkPresentInfoKHR

data VkSwapchainCreateFlagBitsKHR

-- No documentation found for TopLevel "VkSwapchainCreateFlagsKHR"
type VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagBitsKHR

data VkSwapchainCreateInfoKHR

-- | Dummy data to tag the 'Ptr' with
data VkSwapchainKHR_T
-- No documentation found for TopLevel "VkSwapchainKHR"
type VkSwapchainKHR = Ptr VkSwapchainKHR_T

type FN_vkAcquireNextImage2KHR = ("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult
type PFN_vkAcquireNextImage2KHR = FunPtr FN_vkAcquireNextImage2KHR

type FN_vkAcquireNextImageKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult
type PFN_vkAcquireNextImageKHR = FunPtr FN_vkAcquireNextImageKHR

type FN_vkCreateSwapchainKHR = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult
type PFN_vkCreateSwapchainKHR = FunPtr FN_vkCreateSwapchainKHR

type FN_vkDestroySwapchainKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroySwapchainKHR = FunPtr FN_vkDestroySwapchainKHR

type FN_vkGetDeviceGroupPresentCapabilitiesKHR = ("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult
type PFN_vkGetDeviceGroupPresentCapabilitiesKHR = FunPtr FN_vkGetDeviceGroupPresentCapabilitiesKHR

type FN_vkGetDeviceGroupSurfacePresentModesKHR = ("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult
type PFN_vkGetDeviceGroupSurfacePresentModesKHR = FunPtr FN_vkGetDeviceGroupSurfacePresentModesKHR

type FN_vkGetPhysicalDevicePresentRectanglesKHR = ("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult
type PFN_vkGetPhysicalDevicePresentRectanglesKHR = FunPtr FN_vkGetPhysicalDevicePresentRectanglesKHR

type FN_vkGetSwapchainImagesKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult
type PFN_vkGetSwapchainImagesKHR = FunPtr FN_vkGetSwapchainImagesKHR

type FN_vkQueuePresentKHR = ("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult
type PFN_vkQueuePresentKHR = FunPtr FN_vkQueuePresentKHR
