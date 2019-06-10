{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCounterFlagBitsEXT
  , SurfaceCounterFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagBitsEXT
  )


-- No documentation found for TopLevel "SurfaceCounterFlagBitsEXT"
type SurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT

-- No documentation found for TopLevel "SurfaceCounterFlagsEXT"
type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT
