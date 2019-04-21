{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE
  , VkPresentTimeGOOGLE
  , VkPresentTimesInfoGOOGLE
  , VkRefreshCycleDurationGOOGLE
  , FN_vkGetPastPresentationTimingGOOGLE
  , PFN_vkGetPastPresentationTimingGOOGLE
  , FN_vkGetRefreshCycleDurationGOOGLE
  , PFN_vkGetRefreshCycleDurationGOOGLE
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
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


data VkPastPresentationTimingGOOGLE

data VkPresentTimeGOOGLE

data VkPresentTimesInfoGOOGLE

data VkRefreshCycleDurationGOOGLE

type FN_vkGetPastPresentationTimingGOOGLE = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult
type PFN_vkGetPastPresentationTimingGOOGLE = FunPtr FN_vkGetPastPresentationTimingGOOGLE

type FN_vkGetRefreshCycleDurationGOOGLE = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult
type PFN_vkGetRefreshCycleDurationGOOGLE = FunPtr FN_vkGetRefreshCycleDurationGOOGLE
