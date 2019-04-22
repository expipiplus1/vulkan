{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( DeviceEventTypeEXT
  , DisplayEventTypeEXT
  , DisplayPowerStateEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventTypeEXT
  , VkDisplayEventTypeEXT
  , VkDisplayPowerStateEXT
  )


-- | VkDeviceEventTypeEXT - Events that can occur on a device object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.VkDeviceEventInfoEXT'
type DeviceEventTypeEXT = VkDeviceEventTypeEXT

-- | VkDisplayEventTypeEXT - Events that can occur on a display object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.VkDisplayEventInfoEXT'
type DisplayEventTypeEXT = VkDisplayEventTypeEXT

-- | VkDisplayPowerStateEXT - Possible power states for a display
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.VkDisplayPowerInfoEXT'
type DisplayPowerStateEXT = VkDisplayPowerStateEXT
