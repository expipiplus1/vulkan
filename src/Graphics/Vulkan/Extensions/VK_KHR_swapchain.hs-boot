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


-- No documentation found for TopLevel "DeviceGroupPresentModeFlagBitsKHR"
type DeviceGroupPresentModeFlagBitsKHR = VkDeviceGroupPresentModeFlagBitsKHR

-- No documentation found for TopLevel "DeviceGroupPresentModeFlagsKHR"
type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR

-- No documentation found for TopLevel "SwapchainCreateFlagBitsKHR"
type SwapchainCreateFlagBitsKHR = VkSwapchainCreateFlagBitsKHR

-- No documentation found for TopLevel "SwapchainCreateFlagsKHR"
type SwapchainCreateFlagsKHR = SwapchainCreateFlagBitsKHR

-- No documentation found for TopLevel "SwapchainKHR"
type SwapchainKHR = VkSwapchainKHR
