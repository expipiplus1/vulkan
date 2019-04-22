{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( BufferCreateFlagBits
  , CommandPoolCreateFlagBits
  , DeviceQueueCreateFlagBits
  , MemoryPropertyFlagBits
  , QueueFlagBits
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Device
  ( VkDeviceQueueCreateFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkMemoryPropertyFlagBits
  , VkQueueFlagBits
  )


-- No documentation found for TopLevel "BufferCreateFlagBits"
type BufferCreateFlagBits = VkBufferCreateFlagBits

-- No documentation found for TopLevel "CommandPoolCreateFlagBits"
type CommandPoolCreateFlagBits = VkCommandPoolCreateFlagBits

-- No documentation found for TopLevel "DeviceQueueCreateFlagBits"
type DeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits

-- No documentation found for TopLevel "MemoryPropertyFlagBits"
type MemoryPropertyFlagBits = VkMemoryPropertyFlagBits

-- No documentation found for TopLevel "QueueFlagBits"
type QueueFlagBits = VkQueueFlagBits
