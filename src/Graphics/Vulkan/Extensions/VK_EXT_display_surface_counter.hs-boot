{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCounterFlagBitsEXT
  , SurfaceCounterFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCounterFlagBitsEXT
  )


-- | VkSurfaceCounterFlagBitsEXT - Surface-relative counter types
--
-- = See Also
--
-- No cross-references are available
type SurfaceCounterFlagBitsEXT = VkSurfaceCounterFlagBitsEXT

-- | VkSurfaceCounterFlagsEXT - Bitmask of VkSurfaceCounterFlagBitsEXT
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagsEXT'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCounterFlagBitsEXT'.
--
-- = See Also
--
-- No cross-references are available
type SurfaceCounterFlagsEXT = SurfaceCounterFlagBitsEXT
