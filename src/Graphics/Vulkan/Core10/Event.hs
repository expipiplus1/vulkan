{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Event
  ( Event
  , EventCreateFlags
#if defined(VK_USE_PLATFORM_GGP)
  , EventCreateInfo(..)
#endif
  , createEvent
  , destroyEvent
  , getEventStatus
  , resetEvent
  , setEvent
  , withEvent
  ) where

import Control.Exception
  ( bracket
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateFlags(..)
  , VkEvent
  , vkCreateEvent
  , vkDestroyEvent
  , vkGetEventStatus
  , vkResetEvent
  , vkSetEvent
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "Event"
type Event = VkEvent

-- No documentation found for TopLevel "EventCreateFlags"
type EventCreateFlags = VkEventCreateFlags


-- No complete pragma for EventCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkEventCreateInfo"
data EventCreateInfo = EventCreateInfo
  { -- No documentation found for Nested "EventCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "EventCreateInfo" "flags"
  flags :: EventCreateFlags
  }
  deriving (Show, Eq)

instance Zero EventCreateInfo where
  zero = EventCreateInfo Nothing
                         zero

#endif


-- No documentation found for TopLevel "vkCreateEvent"
createEvent :: Device ->  EventCreateInfo ->  Maybe AllocationCallbacks ->  IO (Event)
createEvent = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyEvent"
destroyEvent :: Device ->  Event ->  Maybe AllocationCallbacks ->  IO ()
destroyEvent = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetEventStatus"
getEventStatus :: Device ->  Event ->  IO (VkResult)
getEventStatus = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkResetEvent"
resetEvent :: Device ->  Event ->  IO ()
resetEvent = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkSetEvent"
setEvent :: Device ->  Event ->  IO ()
setEvent = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createEvent' and 'destroyEvent' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withEvent
  :: Device -> EventCreateInfo -> Maybe AllocationCallbacks -> (Event -> IO a) -> IO a
withEvent device eventCreateInfo allocationCallbacks = bracket
  (createEvent device eventCreateInfo allocationCallbacks)
  (\o -> destroyEvent device o allocationCallbacks)
