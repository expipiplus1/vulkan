{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPool
  , VkCommandPoolCreateFlagBits
  , VkCommandPoolCreateFlags
  , VkCommandPoolCreateInfo
  , VkCommandPoolResetFlagBits
  , VkCommandPoolResetFlags
  , FN_vkCreateCommandPool
  , PFN_vkCreateCommandPool
  , FN_vkDestroyCommandPool
  , PFN_vkDestroyCommandPool
  , FN_vkResetCommandPool
  , PFN_vkResetCommandPool
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


-- | Dummy data to tag the 'Ptr' with
data VkCommandPool_T
-- | VkCommandPool - Opaque handle to a command pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'vkCreateCommandPool', 'vkDestroyCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'vkResetCommandPool',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_maintenance1.vkTrimCommandPoolKHR'
type VkCommandPool = Ptr VkCommandPool_T

data VkCommandPoolCreateFlagBits

-- | VkCommandPoolCreateFlags - Bitmask of VkCommandPoolCreateFlagBits
--
-- = Description
--
-- 'VkCommandPoolCreateFlags' is a bitmask type for setting a mask of zero
-- or more 'VkCommandPoolCreateFlagBits'.
--
-- = See Also
--
-- 'VkCommandPoolCreateFlagBits', 'VkCommandPoolCreateInfo'
type VkCommandPoolCreateFlags = VkCommandPoolCreateFlagBits

data VkCommandPoolCreateInfo

data VkCommandPoolResetFlagBits

-- | VkCommandPoolResetFlags - Bitmask of VkCommandPoolResetFlagBits
--
-- = Description
--
-- 'VkCommandPoolResetFlags' is a bitmask type for setting a mask of zero
-- or more 'VkCommandPoolResetFlagBits'.
--
-- = See Also
--
-- 'VkCommandPoolResetFlagBits', 'vkResetCommandPool'
type VkCommandPoolResetFlags = VkCommandPoolResetFlagBits

type FN_vkCreateCommandPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult
type PFN_vkCreateCommandPool = FunPtr FN_vkCreateCommandPool

type FN_vkDestroyCommandPool = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyCommandPool = FunPtr FN_vkDestroyCommandPool

type FN_vkResetCommandPool = ("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult
type PFN_vkResetCommandPool = FunPtr FN_vkResetCommandPool
