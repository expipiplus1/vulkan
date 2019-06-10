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


-- No documentation found for TopLevel "DeviceEventTypeEXT"
type DeviceEventTypeEXT = VkDeviceEventTypeEXT

-- No documentation found for TopLevel "DisplayEventTypeEXT"
type DisplayEventTypeEXT = VkDisplayEventTypeEXT

-- No documentation found for TopLevel "DisplayPowerStateEXT"
type DisplayPowerStateEXT = VkDisplayPowerStateEXT
