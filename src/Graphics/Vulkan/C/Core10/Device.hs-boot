{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags
  , VkDeviceCreateInfo
  , VkDeviceQueueCreateFlagBits
  , VkDeviceQueueCreateFlags
  , VkDeviceQueueCreateInfo
  , FN_vkCreateDevice
  , PFN_vkCreateDevice
  , FN_vkDestroyDevice
  , PFN_vkDestroyDevice
  ) where

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


data VkDeviceCreateFlags

data VkDeviceCreateInfo

data VkDeviceQueueCreateFlagBits

-- No documentation found for TopLevel "VkDeviceQueueCreateFlags"
type VkDeviceQueueCreateFlags = VkDeviceQueueCreateFlagBits

data VkDeviceQueueCreateInfo

type FN_vkCreateDevice = ("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult
type PFN_vkCreateDevice = FunPtr FN_vkCreateDevice

type FN_vkDestroyDevice = ("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyDevice = FunPtr FN_vkDestroyDevice
