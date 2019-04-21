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

-- | VkDeviceGroupPresentModeFlagsKHR - Bitmask of
-- VkDeviceGroupPresentModeFlagBitsKHR
--
-- = Description
--
-- 'VkDeviceGroupPresentModeFlagsKHR' is a bitmask type for setting a mask
-- of zero or more 'VkDeviceGroupPresentModeFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type VkDeviceGroupPresentModeFlagsKHR = VkDeviceGroupPresentModeFlagBitsKHR

data VkDeviceGroupSwapchainCreateInfoKHR

data VkImageSwapchainCreateInfoKHR

data VkPresentInfoKHR

data VkSwapchainCreateFlagBitsKHR

-- | VkSwapchainCreateFlagsKHR - Bitmask of VkSwapchainCreateFlagBitsKHR
--
-- = Description
--
-- 'VkSwapchainCreateFlagsKHR' is a bitmask type for setting a mask of zero
-- or more 'VkSwapchainCreateFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type VkSwapchainCreateFlagsKHR = VkSwapchainCreateFlagBitsKHR

data VkSwapchainCreateInfoKHR

-- | Dummy data to tag the 'Ptr' with
data VkSwapchainKHR_T
-- | VkSwapchainKHR - Opaque handle to a swapchain object
--
-- = Description
--
-- A swapchain is an abstraction for an array of presentable images that
-- are associated with a surface. The presentable images are represented by
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkImage' objects created by
-- the platform. One image (which /can/ be an array image for
-- multiview\/stereoscopic-3D surfaces) is displayed at a time, but
-- multiple images /can/ be queued for presentation. An application renders
-- to the image, and then queues the image for presentation to the surface.
--
-- A native window /cannot/ be associated with more than one non-retired
-- swapchain at a time. Further, swapchains /cannot/ be created for native
-- windows that have a non-Vulkan graphics API surface associated with
-- them.
--
-- __Note__
--
-- The presentation engine is an abstraction for the platform’s compositor
-- or display engine.
--
-- The presentation engine /may/ be synchronous or asynchronous with
-- respect to the application and\/or logical device.
--
-- Some implementations /may/ use the device’s graphics queue or dedicated
-- presentation hardware to perform presentation.
--
-- The presentable images of a swapchain are owned by the presentation
-- engine. An application /can/ acquire use of a presentable image from the
-- presentation engine. Use of a presentable image /must/ occur only after
-- the image is returned by 'vkAcquireNextImageKHR', and before it is
-- presented by 'vkQueuePresentKHR'. This includes transitioning the image
-- layout and rendering commands.
--
-- An application /can/ acquire use of a presentable image with
-- 'vkAcquireNextImageKHR'. After acquiring a presentable image and before
-- modifying it, the application /must/ use a synchronization primitive to
-- ensure that the presentation engine has finished reading from the image.
-- The application /can/ then transition the image’s layout, queue
-- rendering commands to it, etc. Finally, the application presents the
-- image with 'vkQueuePresentKHR', which releases the acquisition of the
-- image.
--
-- The presentation engine controls the order in which presentable images
-- are acquired for use by the application.
--
-- __Note__
--
-- This allows the platform to handle situations which require out-of-order
-- return of images after presentation. At the same time, it allows the
-- application to generate command buffers referencing all of the images in
-- the swapchain at initialization time, rather than in its main loop.
--
-- = See Also
--
-- No cross-references are available
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
