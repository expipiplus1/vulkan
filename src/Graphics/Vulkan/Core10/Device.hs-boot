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


-- No documentation found for TopLevel "DeviceCreateFlags"
type DeviceCreateFlags = VkDeviceCreateFlags

-- No documentation found for TopLevel "DeviceQueueCreateFlagBits"
type DeviceQueueCreateFlagBits = VkDeviceQueueCreateFlagBits

-- No documentation found for TopLevel "DeviceQueueCreateFlags"
type DeviceQueueCreateFlags = DeviceQueueCreateFlagBits
