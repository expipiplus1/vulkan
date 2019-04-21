{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( CommandPoolTrimFlags
  , CommandPoolTrimFlagsKHR
  ) where




import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags
  )


-- | VkCommandPoolTrimFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VkCommandPoolTrimFlags'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool'
type CommandPoolTrimFlags = VkCommandPoolTrimFlags

-- No documentation found for TopLevel "CommandPoolTrimFlagsKHR"
type CommandPoolTrimFlagsKHR = CommandPoolTrimFlags
