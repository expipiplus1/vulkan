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


-- No documentation found for TopLevel "CommandBufferLevel"
type CommandBufferLevel = VkCommandBufferLevel

-- No documentation found for TopLevel "CommandBufferResetFlagBits"
type CommandBufferResetFlagBits = VkCommandBufferResetFlagBits

-- No documentation found for TopLevel "CommandBufferResetFlags"
type CommandBufferResetFlags = CommandBufferResetFlagBits

-- No documentation found for TopLevel "CommandBufferUsageFlagBits"
type CommandBufferUsageFlagBits = VkCommandBufferUsageFlagBits

-- No documentation found for TopLevel "CommandBufferUsageFlags"
type CommandBufferUsageFlags = CommandBufferUsageFlagBits

-- No documentation found for TopLevel "QueryControlFlagBits"
type QueryControlFlagBits = VkQueryControlFlagBits

-- No documentation found for TopLevel "QueryControlFlags"
type QueryControlFlags = QueryControlFlagBits
