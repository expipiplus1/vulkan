{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Fence
  ( FenceCreateFlagBits
  , FenceCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateFlagBits
  )


-- | VkFenceCreateFlagBits - Bitmask specifying initial state and behavior of
-- a fence
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlags'
type FenceCreateFlagBits = VkFenceCreateFlagBits

-- | VkFenceCreateFlags - Bitmask of VkFenceCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo'
type FenceCreateFlags = FenceCreateFlagBits
