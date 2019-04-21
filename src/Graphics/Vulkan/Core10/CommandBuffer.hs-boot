{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.CommandBuffer
  ( CommandBufferLevel
  , CommandBufferResetFlagBits
  , CommandBufferResetFlags
  , CommandBufferUsageFlagBits
  , CommandBufferUsageFlags
  , QueryControlFlagBits
  , QueryControlFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferLevel
  , VkCommandBufferResetFlagBits
  , VkCommandBufferUsageFlagBits
  , VkQueryControlFlagBits
  )


-- | VkCommandBufferLevel - Enumerant specifying a command buffer level
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo'
type CommandBufferLevel = VkCommandBufferLevel

-- | VkCommandBufferResetFlagBits - Bitmask controlling behavior of a command
-- buffer reset
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlags'
type CommandBufferResetFlagBits = VkCommandBufferResetFlagBits

-- | VkCommandBufferResetFlags - Bitmask of VkCommandBufferResetFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferResetFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkResetCommandBuffer'
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- | VkCommandBufferUsageFlagBits - Bitmask specifying usage behavior for
-- command buffer
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlags'
type CommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits

-- | VkCommandBufferUsageFlags - Bitmask of VkCommandBufferUsageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferUsageFlagBits'
type CommandBufferUsageFlags = CommandBufferUsageFlagBits

-- | VkQueryControlFlagBits - Bitmask specifying constraints on a query
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags'
type QueryControlFlagBits = VkQueryControlFlagBits

-- | VkQueryControlFlags - Bitmask of VkQueryControlFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkQueryControlFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'
type QueryControlFlags = QueryControlFlagBits
