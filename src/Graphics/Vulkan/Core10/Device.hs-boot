{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Device
  ( DeviceCreateFlags
  , DeviceQueueCreateFlagBits
  , DeviceQueueCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags
  , VkDeviceQueueCreateFlagBits
  )


-- | VkDeviceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateFlags' is a bitmask type
-- for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo'
type DeviceCreateFlags = VkDeviceCreateFlags

-- | VkDeviceQueueCreateFlagBits - Bitmask specifying behavior of the queue
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateFlags'
type DeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits

-- | VkDeviceQueueCreateFlags - Bitmask of VkDeviceQueueCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateFlags' is a bitmask
-- type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2'
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits
