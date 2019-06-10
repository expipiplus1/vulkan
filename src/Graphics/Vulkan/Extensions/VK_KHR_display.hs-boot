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
  ( VkDisplayKHR
  , VkDisplayModeCreateFlagsKHR
  , VkDisplayModeKHR
  , VkDisplayPlaneAlphaFlagBitsKHR
  , VkDisplaySurfaceCreateFlagsKHR
  )


-- No documentation found for TopLevel "DisplayKHR"
type DisplayKHR = VkDisplayKHR

-- No documentation found for TopLevel "DisplayModeCreateFlagsKHR"
type DisplayModeCreateFlagsKHR = VkDisplayModeCreateFlagsKHR

-- No documentation found for TopLevel "DisplayModeKHR"
type DisplayModeKHR = VkDisplayModeKHR

-- No documentation found for TopLevel "DisplayPlaneAlphaFlagBitsKHR"
type DisplayPlaneAlphaFlagBitsKHR = VkDisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "DisplayPlaneAlphaFlagsKHR"
type DisplayPlaneAlphaFlagsKHR = DisplayPlaneAlphaFlagBitsKHR

-- No documentation found for TopLevel "DisplaySurfaceCreateFlagsKHR"
type DisplaySurfaceCreateFlagsKHR = VkDisplaySurfaceCreateFlagsKHR
