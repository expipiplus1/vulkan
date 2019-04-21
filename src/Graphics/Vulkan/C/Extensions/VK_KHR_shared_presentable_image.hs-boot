{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR
  , FN_vkGetSwapchainStatusKHR
  , PFN_vkGetSwapchainStatusKHR
  ) where

import Foreign.Ptr
  ( FunPtr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


data VkSharedPresentSurfaceCapabilitiesKHR

type FN_vkGetSwapchainStatusKHR = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult
type PFN_vkGetSwapchainStatusKHR = FunPtr FN_vkGetSwapchainStatusKHR
