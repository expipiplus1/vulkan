{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.CommandPool
  ( CommandPool
  , CommandPoolCreateFlagBits
  , CommandPoolCreateFlags
  , CommandPoolResetFlagBits
  , CommandPoolResetFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits
  , VkCommandPoolResetFlagBits
  , VkCommandPool
  )


-- | VkCommandPool - Opaque handle to a command pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool'
type CommandPool = VkCommandPool

-- | VkCommandPoolCreateFlagBits - Bitmask specifying usage behavior for a
-- command pool
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlags'
type CommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits

-- | VkCommandPoolCreateFlags - Bitmask of VkCommandPoolCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo'
type CommandPoolCreateFlags = CommandPoolCreateFlagBits

-- | VkCommandPoolResetFlagBits - Bitmask controlling behavior of a command
-- pool reset
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlags'
type CommandPoolResetFlagBits = VkCommandPoolResetFlagBits

-- | VkCommandPoolResetFlags - Bitmask of VkCommandPoolResetFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolResetFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool'
type CommandPoolResetFlags = CommandPoolResetFlagBits
