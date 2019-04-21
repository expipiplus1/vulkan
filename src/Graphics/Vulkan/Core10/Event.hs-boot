{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Event
  ( Event
  , EventCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateFlags
  , VkEvent
  )


-- | VkEvent - Opaque handle to an event object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'Graphics.Vulkan.C.Core10.Event.vkCreateEvent',
-- 'Graphics.Vulkan.C.Core10.Event.vkDestroyEvent',
-- 'Graphics.Vulkan.C.Core10.Event.vkGetEventStatus',
-- 'Graphics.Vulkan.C.Core10.Event.vkResetEvent',
-- 'Graphics.Vulkan.C.Core10.Event.vkSetEvent'
type Event = VkEvent

-- | VkEventCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Event.VkEventCreateFlags' is a bitmask type
-- for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Event.VkEventCreateInfo'
type EventCreateFlags = VkEventCreateFlags
