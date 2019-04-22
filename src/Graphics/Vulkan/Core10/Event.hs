{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Event
  ( Event
  , EventCreateFlags
  , withCStructEventCreateInfo
  , fromCStructEventCreateInfo
  , EventCreateInfo(..)
  , createEvent
  , destroyEvent
  , getEventStatus
  , resetEvent
  , setEvent
  , withEvent
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( when
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateFlags(..)
  , VkEventCreateInfo(..)
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
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
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


-- No complete pragma for EventCreateFlags as it has no patterns


-- | VkEventCreateInfo - Structure specifying parameters of a newly created
-- event
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Event.VkEventCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Event.vkCreateEvent'
data EventCreateInfo = EventCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "EventCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "EventCreateInfo" "flags"
  flags :: EventCreateFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkEventCreateInfo' and
-- marshal a 'EventCreateInfo' into it. The 'VkEventCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructEventCreateInfo :: EventCreateInfo -> (VkEventCreateInfo -> IO a) -> IO a
withCStructEventCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: EventCreateInfo)) (\pPNext -> cont (VkEventCreateInfo VK_STRUCTURE_TYPE_EVENT_CREATE_INFO pPNext (flags (marshalled :: EventCreateInfo))))

-- | A function to read a 'VkEventCreateInfo' and all additional
-- structures in the pointer chain into a 'EventCreateInfo'.
fromCStructEventCreateInfo :: VkEventCreateInfo -> IO EventCreateInfo
fromCStructEventCreateInfo c = EventCreateInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkEventCreateInfo)))
                                               <*> pure (vkFlags (c :: VkEventCreateInfo))

instance Zero EventCreateInfo where
  zero = EventCreateInfo Nothing
                         zero



-- | vkCreateEvent - Create a new event object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the event.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Event.VkEventCreateInfo' structure which
--     contains information about how the event is to be created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pEvent@ points to a handle in which the resulting event object is
--     returned.
--
-- = Description
--
-- When created, the event object is in the unsignaled state.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.Event.VkEventCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pEvent@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.Event.VkEvent' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent',
-- 'Graphics.Vulkan.C.Core10.Event.VkEventCreateInfo'
createEvent :: Device ->  EventCreateInfo ->  Maybe AllocationCallbacks ->  IO (Event)
createEvent = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pEvent' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructEventCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateEvent commandTable device' pCreateInfo' pAllocator pEvent' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pEvent')))))


-- | vkDestroyEvent - Destroy an event object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the event.
--
-- -   @event@ is the handle of the event to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @event@ /must/ have completed
--     execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @event@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @event@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   If @event@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @event@ /must/
--     be a valid 'Graphics.Vulkan.C.Core10.Event.VkEvent' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   If @event@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @event@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent'
destroyEvent :: Device ->  Event ->  Maybe AllocationCallbacks ->  IO ()
destroyEvent = \(Device device' commandTable) -> \event' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyEvent commandTable device' event' pAllocator *> (pure ()))


-- | vkGetEventStatus - Retrieve the status of an event object
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the event.
--
-- -   @event@ is the handle of the event to query.
--
-- = Description
--
-- Upon success, 'Graphics.Vulkan.C.Core10.Event.vkGetEventStatus' returns
-- the state of the event object with the following return codes:
--
-- > +-----------------------------------+-----------------------------------+
-- > | Status                            | Meaning                           |
-- > +===================================+===================================+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK | The event specified by @event@ is |
-- > | _EVENT_SET'                       | signaled.                         |
-- > +-----------------------------------+-----------------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK | The event specified by @event@ is |
-- > | _EVENT_RESET'                     | unsignaled.                       |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Event Object Status Codes
--
-- If a 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent' or
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent' command
-- is in a command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- then the value returned by this command /may/ immediately be out of
-- date.
--
-- The state of an event /can/ be updated by the host. The state of the
-- event is immediately changed, and subsequent calls to
-- 'Graphics.Vulkan.C.Core10.Event.vkGetEventStatus' will return the new
-- state. If an event is already in the requested state, then updating it
-- to the same state has no effect.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_EVENT_SET'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_EVENT_RESET'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent'
getEventStatus :: Device ->  Event ->  IO (VkResult)
getEventStatus = \(Device device' commandTable) -> \event' -> vkGetEventStatus commandTable device' event' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ret))


-- | vkResetEvent - Reset an event to non-signaled state
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the event.
--
-- -   @event@ is the event to reset.
--
-- = Description
--
-- When 'Graphics.Vulkan.C.Core10.Event.vkResetEvent' is executed on the
-- host, it defines an /event unsignal operation/ which resets the event to
-- the unsignaled state.
--
-- If @event@ is already in the unsignaled state when
-- 'Graphics.Vulkan.C.Core10.Event.vkResetEvent' is executed, then
-- 'Graphics.Vulkan.C.Core10.Event.vkResetEvent' has no effect, and no
-- event unsignal operation occurs.
--
-- == Valid Usage
--
-- -   @event@ /must/ not be waited on by a
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents'
--     command that is currently executing
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @event@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Event.VkEvent'
--     handle
--
-- -   @event@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @event@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent'
resetEvent :: Device ->  Event ->  IO ()
resetEvent = \(Device device' commandTable) -> \event' -> vkResetEvent commandTable device' event' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))


-- | vkSetEvent - Set an event to signaled state
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the event.
--
-- -   @event@ is the event to set.
--
-- = Description
--
-- When 'Graphics.Vulkan.C.Core10.Event.vkSetEvent' is executed on the
-- host, it defines an /event signal operation/ which sets the event to the
-- signaled state.
--
-- If @event@ is already in the signaled state when
-- 'Graphics.Vulkan.C.Core10.Event.vkSetEvent' is executed, then
-- 'Graphics.Vulkan.C.Core10.Event.vkSetEvent' has no effect, and no event
-- signal operation occurs.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice' handle
--
-- -   @event@ /must/ be a valid 'Graphics.Vulkan.C.Core10.Event.VkEvent'
--     handle
--
-- -   @event@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @event@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Event.VkEvent'
setEvent :: Device ->  Event ->  IO ()
setEvent = \(Device device' commandTable) -> \event' -> vkSetEvent commandTable device' event' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (pure ()))

-- | A safe wrapper for 'createEvent' and 'destroyEvent' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withEvent
  :: Device -> EventCreateInfo -> Maybe (AllocationCallbacks) -> (Event -> IO a) -> IO a
withEvent device eventCreateInfo allocationCallbacks = bracket
  (createEvent device eventCreateInfo allocationCallbacks)
  (\o -> destroyEvent device o allocationCallbacks)
