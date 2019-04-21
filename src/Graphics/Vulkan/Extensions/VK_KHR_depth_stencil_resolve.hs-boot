{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( ResolveModeFlagBitsKHR
  , ResolveModeFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkResolveModeFlagBitsKHR
  )


-- | VkResolveModeFlagBitsKHR - Bitmask indicating supported depth and
-- stencil resolve modes
--
-- = See Also
--
-- No cross-references are available
type ResolveModeFlagBitsKHR = VkResolveModeFlagBitsKHR

-- | VkResolveModeFlagsKHR - Bitmask of VkResolveModeFlagBitsKHR
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkResolveModeFlagsKHR'
-- is a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve.VkResolveModeFlagBitsKHR'.
--
-- = See Also
--
-- No cross-references are available
type ResolveModeFlagsKHR = ResolveModeFlagBitsKHR
