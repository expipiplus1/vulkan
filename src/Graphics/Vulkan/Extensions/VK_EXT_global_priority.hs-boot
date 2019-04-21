{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( QueueGlobalPriorityEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkQueueGlobalPriorityEXT
  )


-- | VkQueueGlobalPriorityEXT - Values specifying a system-wide queue
-- priority
--
-- = Description
--
-- Priority values are sorted in ascending order. A comparison operation on
-- the enum values can be used to determine the priority order.
--
-- = See Also
--
-- No cross-references are available
type QueueGlobalPriorityEXT = VkQueueGlobalPriorityEXT
