{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( FullScreenExclusiveEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkFullScreenExclusiveEXT
  )


-- | VkFullScreenExclusiveEXT - Hint values an application can specify
-- affecting full-screen transition behavior
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.VkSurfaceFullScreenExclusiveInfoEXT'
type FullScreenExclusiveEXT = VkFullScreenExclusiveEXT
