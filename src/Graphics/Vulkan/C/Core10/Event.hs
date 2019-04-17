{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Event
  ( VkEvent
  , VkEventCreateFlags(..)
  , VkEventCreateInfo(..)
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkCreateEvent
#endif
  , FN_vkCreateEvent
  , PFN_vkCreateEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkDestroyEvent
#endif
  , FN_vkDestroyEvent
  , PFN_vkDestroyEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkGetEventStatus
#endif
  , FN_vkGetEventStatus
  , PFN_vkGetEventStatus
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkResetEvent
#endif
  , FN_vkResetEvent
  , PFN_vkResetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
  , vkSetEvent
#endif
  , FN_vkSetEvent
  , PFN_vkSetEvent
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- | Dummy data to tag the 'Ptr' with
data VkEvent_T
-- | VkEvent - Opaque handle to an event object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetEvent',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWaitEvents',
-- 'vkCreateEvent', 'vkDestroyEvent', 'vkGetEventStatus', 'vkResetEvent',
-- 'vkSetEvent'
type VkEvent = Ptr VkEvent_T
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
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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


-- | VkEventCreateInfo - Structure specifying parameters of a newly created
-- event
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkEventCreateFlags', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'vkCreateEvent'
data VkEventCreateInfo = VkEventCreateInfo
  { -- | @sType@ /must/ be @VK_STRUCTURE_TYPE_EVENT_CREATE_INFO@
  vkSType :: VkStructureType
  , -- | @pNext@ /must/ be @NULL@
  vkPNext :: Ptr ()
  , -- | @flags@ /must/ be @0@
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

instance Zero VkEventCreateInfo where
  zero = VkEventCreateInfo zero
                           zero
                           zero
#if defined(EXPOSE_CORE10_COMMANDS)
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
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkEvent',
-- 'VkEventCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateEvent" vkCreateEvent :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult

#endif
type FN_vkCreateEvent = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult
type PFN_vkCreateEvent = FunPtr FN_vkCreateEvent
#if defined(EXPOSE_CORE10_COMMANDS)
-- | vkDestroyEvent - Destroy an event object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the event.
--
-- -   @event@ is the handle of the event to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation Memory Allocation>
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
-- -   If @event@ is not
--     'Graphics.Vulkan.C.Core10.Constants.VK_NULL_HANDLE', @event@ /must/
--     be a valid @VkEvent@ handle
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyEvent" vkDestroyEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()

#endif
type FN_vkDestroyEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyEvent = FunPtr FN_vkDestroyEvent
#if defined(EXPOSE_CORE10_COMMANDS)
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
-- > +-----------------------------------+-----------------------------------+
-- > | Status                            | Meaning                           |
-- > +===================================+===================================+
-- > | @VK_EVENT_SET@                    | The event specified by @event@ is |
-- > |                                   | signaled.                         |
-- > +-----------------------------------+-----------------------------------+
-- > | @VK_EVENT_RESET@                  | The event specified by @event@ is |
-- > |                                   | unsignaled.                       |
-- > +-----------------------------------+-----------------------------------+
-- >
-- > Event Object Status Codes
--
-- If a @vkCmdSetEvent@ or @vkCmdResetEvent@ command is in a command buffer
-- that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- then the value returned by this command /may/ immediately be out of
-- date.
--
-- The state of an event /can/ be updated by the host. The state of the
-- event is immediately changed, and subsequent calls to @vkGetEventStatus@
-- will return the new state. If an event is already in the requested
-- state, then updating it to the same state has no effect.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_EVENT_SET@
--
--     -   @VK_EVENT_RESET@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetEventStatus" vkGetEventStatus :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult

#endif
type FN_vkGetEventStatus = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkGetEventStatus = FunPtr FN_vkGetEventStatus
#if defined(EXPOSE_CORE10_COMMANDS)
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkResetEvent" vkResetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult

#endif
type FN_vkResetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkResetEvent = FunPtr FN_vkResetEvent
#if defined(EXPOSE_CORE10_COMMANDS)
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
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   @VK_SUCCESS@
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkEvent'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkSetEvent" vkSetEvent :: ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult

#endif
type FN_vkSetEvent = ("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult
type PFN_vkSetEvent = FunPtr FN_vkSetEvent
