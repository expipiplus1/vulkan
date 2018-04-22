{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Event
  ( VkEventCreateFlags(..)
  , VkEvent
  , vkCreateEvent
  , vkDestroyEvent
  , vkGetEventStatus
  , vkSetEvent
  , vkResetEvent
  , VkEventCreateInfo(..)
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )


-- ** VkEventCreateFlags

-- | VkEventCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkEventCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkEventCreateInfo'
newtype VkEventCreateFlags = VkEventCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkEventCreateFlags where
  
  showsPrec p (VkEventCreateFlags x) = showParen (p >= 11) (showString "VkEventCreateFlags " . showsPrec 11 x)

instance Read VkEventCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkEventCreateFlags")
                        v <- step readPrec
                        pure (VkEventCreateFlags v)
                        )
                    )


-- | Dummy data to tag the 'Ptr' with
data VkEvent_T
-- | VkEvent - Opaque handle to a event object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'vkCreateEvent', 'vkDestroyEvent', 'vkGetEventStatus', 'vkResetEvent',
-- 'vkSetEvent'
type VkEvent = Ptr VkEvent_T
-- | vkCreateEvent - Create a new event object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the event.
--
-- -   @pCreateInfo@ is a pointer to an instance of the @VkEventCreateInfo@
--     structure which contains information about how the event is to be
--     created.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
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
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkEventCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pEvent@ /must/ be a valid pointer to a @VkEvent@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkEvent',
-- 'VkEventCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateEvent" vkCreateEvent :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
-- | vkDestroyEvent - Destroy an event object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the event.
--
-- -   @event@ is the handle of the event to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @event@ /must/ have completed
--     execution
--
-- -   If @VkAllocationCallbacks@ were provided when @event@ was created, a
--     compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @event@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @event@ is not 'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE',
--     @event@ /must/ be a valid @VkEvent@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
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
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyEvent" vkDestroyEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
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
-- Upon success, @vkGetEventStatus@ returns the state of the event object
-- with the following return codes:
--
-- +-----------------------------------+-----------------------------------+
-- | Status                            | Meaning                           |
-- +===================================+===================================+
-- | @VK_EVENT_SET@                    | The event specified by @event@ is |
-- |                                   | signaled.                         |
-- +-----------------------------------+-----------------------------------+
-- | @VK_EVENT_RESET@                  | The event specified by @event@ is |
-- |                                   | unsignaled.                       |
-- +-----------------------------------+-----------------------------------+
--
-- Event Object Status Codes
--
-- If a @vkCmdSetEvent@ or @vkCmdResetEvent@ command is in a command buffer
-- that is in the [pending
-- state](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle),
-- then the value returned by this command /may/ immediately be out of
-- date.
--
-- The state of an event /can/ be updated by the host. The state of the
-- event is immediately changed, and subsequent calls to @vkGetEventStatus@
-- will return the new state. If an event is already in the requested
-- state, then updating it to the same state has no effect.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @event@ /must/ be a valid @VkEvent@ handle
--
-- -   @event@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_EVENT_SET@
--
--     -   @VK_EVENT_RESET@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetEventStatus" vkGetEventStatus :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
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
-- When 'vkSetEvent' is executed on the host, it defines an /event signal
-- operation/ which sets the event to the signaled state.
--
-- If @event@ is already in the signaled state when 'vkSetEvent' is
-- executed, then 'vkSetEvent' has no effect, and no event signal operation
-- occurs.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @event@ /must/ be a valid @VkEvent@ handle
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetEvent" vkSetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
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
-- When 'vkResetEvent' is executed on the host, it defines an /event
-- unsignal operation/ which resets the event to the unsignaled state.
--
-- If @event@ is already in the unsignaled state when 'vkResetEvent' is
-- executed, then 'vkResetEvent' has no effect, and no event unsignal
-- operation occurs.
--
-- == Valid Usage
--
-- -   @event@ /must/ not be waited on by a @vkCmdWaitEvents@ command that
--     is currently executing
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @event@ /must/ be a valid @VkEvent@ handle
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
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetEvent" vkResetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
-- | VkEventCreateInfo - Structure specifying parameters of a newly created
-- event
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_EVENT_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- = See Also
--
-- 'VkEventCreateFlags', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateEvent'
data VkEventCreateInfo = VkEventCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkEventCreateFlags
  }
  deriving (Eq, Show)

instance Storable VkEventCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkEventCreateInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkEventCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkEventCreateInfo))
