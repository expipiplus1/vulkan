{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( DeviceAddress
  ) where




import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkDeviceAddress
  )


-- | VkDeviceAddress - Vulkan device address type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.VkBufferDeviceAddressCreateInfoEXT'
type DeviceAddress = VkDeviceAddress
  
