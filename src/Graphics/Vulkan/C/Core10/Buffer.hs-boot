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

-- | VkBufferCreateFlags - Bitmask of VkBufferCreateFlagBits
--
-- = Description
--
-- 'VkBufferCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkBufferCreateFlagBits'.
--
-- = See Also
--
-- 'VkBufferCreateFlagBits', 'VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type VkBufferCreateFlags = VkBufferCreateFlagBits

data VkBufferCreateInfo

data VkBufferUsageFlagBits

-- | VkBufferUsageFlags - Bitmask of VkBufferUsageFlagBits
--
-- = Description
--
-- 'VkBufferUsageFlags' is a bitmask type for setting a mask of zero or
-- more 'VkBufferUsageFlagBits'.
--
-- = See Also
--
-- 'VkBufferCreateInfo', 'VkBufferUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo'
type VkBufferUsageFlags = VkBufferUsageFlagBits

data VkSharingMode

type FN_vkCreateBuffer = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult
type PFN_vkCreateBuffer = FunPtr FN_vkCreateBuffer

type FN_vkDestroyBuffer = ("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyBuffer = FunPtr FN_vkDestroyBuffer
