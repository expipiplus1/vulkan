{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( DeviceGroupPresentModeFlagBitsKHR
  , DeviceGroupPresentModeFlagsKHR
  , SwapchainCreateFlagBitsKHR
  , SwapchainCreateFlagsKHR
  , SwapchainKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkDeviceGroupPresentModeFlagBitsKHR
  , VkSwapchainCreateFlagBitsKHR
  , VkSwapchainKHR
  )


-- | VkDeviceGroupPresentModeFlagBitsKHR - Bitmask specifying supported
-- device group present modes
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR'
type DeviceGroupPresentModeFlagBitsKHR = VkDeviceGroupPresentModeFlagBitsKHR

-- | VkDeviceGroupPresentModeFlagsKHR - Bitmask of
-- VkDeviceGroupPresentModeFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagBitsKHR'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentModeFlagBitsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkDeviceGroupSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkGetDeviceGroupSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR'
type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR

-- | VkSwapchainCreateFlagBitsKHR - Bitmask controlling swapchain creation
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateFlagsKHR'
type SwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR

-- | VkSwapchainCreateFlagsKHR - Bitmask of VkSwapchainCreateFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateFlagBitsKHR'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateFlagBitsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR'
type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR

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
-- the image is returned by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- and before it is presented by
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR'. This
-- includes transitioning the image layout and rendering commands.
--
-- An application /can/ acquire use of a presentable image with
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR'.
-- After acquiring a presentable image and before modifying it, the
-- application /must/ use a synchronization primitive to ensure that the
-- presentation engine has finished reading from the image. The application
-- /can/ then transition the image’s layout, queue rendering commands to
-- it, etc. Finally, the application presents the image with
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkQueuePresentKHR', which
-- releases the acquisition of the image.
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkImageSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkPresentInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkAcquireFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain.vkCreateSharedSwapchainsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkDestroySwapchainKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkGetSwapchainCounterEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetSwapchainImagesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.vkGetSwapchainStatusKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkReleaseFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.vkSetHdrMetadataEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr.vkSetLocalDimmingAMD'
type SwapchainKHR = VkSwapchainKHR
