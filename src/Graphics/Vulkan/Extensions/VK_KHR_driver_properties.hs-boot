{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( DriverIdKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkDriverIdKHR
  )


-- | VkDriverIdKHR - Khronos driver IDs
--
-- = Description
--
-- __Note__
--
-- Khronos driver IDs may be allocated by vendors at any time. There may be
-- multiple driver IDs for the same vendor, representing different drivers
-- (for e.g. different platforms, proprietary or open source, etc.). Only
-- the latest canonical versions of this Specification, of the
-- corresponding @vk.xml@ API Registry, and of the corresponding
-- @vulkan_core.h@ header file /must/ contain all reserved Khronos driver
-- IDs.
--
-- Only driver IDs registered with Khronos are given symbolic names. There
-- /may/ be unregistered driver IDs returned.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties.VkPhysicalDeviceDriverPropertiesKHR'
type DriverIdKHR = VkDriverIdKHR
