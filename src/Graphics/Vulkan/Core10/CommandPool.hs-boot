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
  ( VkCommandPool
  , VkCommandPoolCreateFlagBits
  , VkCommandPoolResetFlagBits
  )


-- No documentation found for TopLevel "CommandPool"
type CommandPool = VkCommandPool

-- No documentation found for TopLevel "CommandPoolCreateFlagBits"
type CommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits

-- No documentation found for TopLevel "CommandPoolCreateFlags"
type CommandPoolCreateFlags = CommandPoolCreateFlagBits

-- No documentation found for TopLevel "CommandPoolResetFlagBits"
type CommandPoolResetFlagBits = VkCommandPoolResetFlagBits

-- No documentation found for TopLevel "CommandPoolResetFlags"
type CommandPoolResetFlags = CommandPoolResetFlagBits
