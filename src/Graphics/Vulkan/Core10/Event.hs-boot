{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Event
  ( Event
  , EventCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Event
  ( VkEvent
  , VkEventCreateFlags
  )


-- No documentation found for TopLevel "Event"
type Event = VkEvent

-- No documentation found for TopLevel "EventCreateFlags"
type EventCreateFlags = VkEventCreateFlags
