{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayKHR
  , DisplayModeCreateFlagsKHR
  , DisplayModeKHR
  , DisplayPlaneAlphaFlagBitsKHR
  , DisplayPlaneAlphaFlagsKHR
  , DisplaySurfaceCreateFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateFlagsKHR
  , VkDisplayPlaneAlphaFlagBitsKHR
  , VkDisplaySurfaceCreateFlagsKHR
  , VkDisplayKHR
  , VkDisplayModeKHR
  )


-- | VkDisplayKHR - Opaque handle to a display object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display.vkAcquireXlibDisplayEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkDisplayPowerControlEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneSupportedDisplaysKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display.vkGetRandROutputDisplayEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display.vkReleaseDisplayEXT'
type DisplayKHR = VkDisplayKHR

-- No documentation found for TopLevel "DisplayModeCreateFlagsKHR"
type DisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR

-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.VkDisplayPlaneInfo2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR'
type DisplayModeKHR = VkDisplayModeKHR

-- | VkDisplayPlaneAlphaFlagBitsKHR - Alpha blending type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneAlphaFlagsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR'
type DisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR

-- | VkDisplayPlaneAlphaFlagsKHR - Bitmask of VkDisplayPlaneAlphaFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneAlphaFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneAlphaFlagBitsKHR'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneAlphaFlagBitsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.VkDisplayPlaneCapabilitiesKHR'
type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "DisplaySurfaceCreateFlagsKHR"
type DisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR
