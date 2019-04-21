{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags
  )


-- | VkSemaphoreCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateInfo'
type SemaphoreCreateFlags = VkSemaphoreCreateFlags
