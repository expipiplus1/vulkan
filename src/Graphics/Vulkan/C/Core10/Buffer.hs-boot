{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits
  , VkBufferCreateFlags
  , VkBufferCreateInfo
  , VkBufferUsageFlagBits
  , VkBufferUsageFlags
  , VkSharingMode
  , FN_vkCreateBuffer
  , PFN_vkCreateBuffer
  , FN_vkDestroyBuffer
  , PFN_vkDestroyBuffer
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
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  )


data VkBufferCreateFlagBits

-- No documentation found for TopLevel "VkBufferCreateFlags"
type VkBufferCreateFlags = VkBufferCreateFlagBits

data VkBufferCreateInfo

data VkBufferUsageFlagBits

-- No documentation found for TopLevel "VkBufferUsageFlags"
type VkBufferUsageFlags = VkBufferUsageFlagBits

data VkSharingMode

type FN_vkCreateBuffer = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
type PFN_vkCreateBuffer = FunPtr FN_vkCreateBuffer

type FN_vkDestroyBuffer = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyBuffer = FunPtr FN_vkDestroyBuffer
