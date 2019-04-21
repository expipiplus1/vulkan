{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR
  , FN_vkCreateSharedSwapchainsKHR
  , PFN_vkCreateSharedSwapchainsKHR
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
  ( VkAllocationCallbacks
  , VkDevice
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainCreateInfoKHR
  , VkSwapchainKHR
  )


data VkDisplayPresentInfoKHR

type FN_vkCreateSharedSwapchainsKHR = ("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult
type PFN_vkCreateSharedSwapchainsKHR = FunPtr FN_vkCreateSharedSwapchainsKHR
