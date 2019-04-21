{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT
  , VkDeviceEventTypeEXT
  , VkDisplayEventInfoEXT
  , VkDisplayEventTypeEXT
  , VkDisplayPowerInfoEXT
  , VkDisplayPowerStateEXT
  , VkSwapchainCounterCreateInfoEXT
  , FN_vkDisplayPowerControlEXT
  , PFN_vkDisplayPowerControlEXT
  , FN_vkGetSwapchainCounterEXT
  , PFN_vkGetSwapchainCounterEXT
  , FN_vkRegisterDeviceEventEXT
  , PFN_vkRegisterDeviceEventEXT
  , FN_vkRegisterDisplayEventEXT
  , PFN_vkRegisterDisplayEventEXT
  ) where

import Data.Word
  ( Word64
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
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkFence
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagBitsEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkSwapchainKHR
  )


data VkDeviceEventInfoEXT

data VkDeviceEventTypeEXT

data VkDisplayEventInfoEXT

data VkDisplayEventTypeEXT

data VkDisplayPowerInfoEXT

data VkDisplayPowerStateEXT

data VkSwapchainCounterCreateInfoEXT

type FN_vkDisplayPowerControlEXT = ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult
type PFN_vkDisplayPowerControlEXT = FunPtr FN_vkDisplayPowerControlEXT

type FN_vkGetSwapchainCounterEXT = ("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult
type PFN_vkGetSwapchainCounterEXT = FunPtr FN_vkGetSwapchainCounterEXT

type FN_vkRegisterDeviceEventEXT = ("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkRegisterDeviceEventEXT = FunPtr FN_vkRegisterDeviceEventEXT

type FN_vkRegisterDisplayEventEXT = ("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult
type PFN_vkRegisterDisplayEventEXT = FunPtr FN_vkRegisterDisplayEventEXT
