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
-- No cross-references are available
type DisplayKHR = VkDisplayKHR

-- No documentation found for TopLevel "DisplayModeCreateFlagsKHR"
type DisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR

-- | VkDisplayModeKHR - Opaque handle to a display mode object
--
-- = See Also
--
-- No cross-references are available
type DisplayModeKHR = VkDisplayModeKHR

-- | VkDisplayPlaneAlphaFlagBitsKHR - Alpha blending type
--
-- = See Also
--
-- No cross-references are available
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
-- No cross-references are available
type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "DisplaySurfaceCreateFlagsKHR"
type DisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR
