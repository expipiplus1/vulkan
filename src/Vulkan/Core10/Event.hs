{-# language CPP #-}
-- No documentation found for Chapter "Event"
module Vulkan.Core10.Event  ( createEvent
                            , withEvent
                            , destroyEvent
                            , getEventStatus
                            , setEvent
                            , resetEvent
                            , EventCreateInfo(..)
                            , Event(..)
                            , EventCreateFlags(..)
                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkCreateEvent))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyEvent))
import Vulkan.Dynamic (DeviceCmds(pVkGetEventStatus))
import Vulkan.Dynamic (DeviceCmds(pVkResetEvent))
import Vulkan.Dynamic (DeviceCmds(pVkSetEvent))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Handles (Event)
import Vulkan.Core10.Handles (Event(..))
import Vulkan.Core10.Enums.EventCreateFlags (EventCreateFlags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EVENT_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Event(..))
import Vulkan.Core10.Enums.EventCreateFlags (EventCreateFlags(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateEvent
  :: FunPtr (Ptr Device_T -> Ptr EventCreateInfo -> Ptr AllocationCallbacks -> Ptr Event -> IO Result) -> Ptr Device_T -> Ptr EventCreateInfo -> Ptr AllocationCallbacks -> Ptr Event -> IO Result

-- | vkCreateEvent - Create a new event object
--
-- = Description
--
-- When created, the event object is in the unsignaled state.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateEvent-events-04468# If the @VK_KHR_portability_subset@
--     extension is enabled, and
--     'Vulkan.Extensions.VK_KHR_portability_subset.PhysicalDevicePortabilitySubsetFeaturesKHR'::@events@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE', then the implementation
--     does not support
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-events events>,
--     and 'createEvent' /must/ not be used.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateEvent-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateEvent-pCreateInfo-parameter# @pCreateInfo@ /must/ be a
--     valid pointer to a valid 'EventCreateInfo' structure
--
-- -   #VUID-vkCreateEvent-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkCreateEvent-pEvent-parameter# @pEvent@ /must/ be a valid
--     pointer to a 'Vulkan.Core10.Handles.Event' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event',
-- 'EventCreateInfo'
createEvent :: forall io
             . (MonadIO io)
            => -- | @device@ is the logical device that creates the event.
               Device
            -> -- | @pCreateInfo@ is a pointer to a 'EventCreateInfo' structure containing
               -- information about how the event is to be created.
               EventCreateInfo
            -> -- | @pAllocator@ controls host memory allocation as described in the
               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
               -- chapter.
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Event)
createEvent device createInfo allocator = liftIO . evalContT $ do
  let vkCreateEventPtr = pVkCreateEvent (deviceCmds (device :: Device))
  lift $ unless (vkCreateEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateEvent is null" Nothing Nothing
  let vkCreateEvent' = mkVkCreateEvent vkCreateEventPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPEvent <- ContT $ bracket (callocBytes @Event 8) free
  r <- lift $ traceAroundEvent "vkCreateEvent" (vkCreateEvent' (deviceHandle (device)) pCreateInfo pAllocator (pPEvent))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pEvent <- lift $ peek @Event pPEvent
  pure $ (pEvent)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createEvent' and 'destroyEvent'
--
-- To ensure that 'destroyEvent' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withEvent :: forall io r . MonadIO io => Device -> EventCreateInfo -> Maybe AllocationCallbacks -> (io Event -> (Event -> io ()) -> r) -> r
withEvent device pCreateInfo pAllocator b =
  b (createEvent device pCreateInfo pAllocator)
    (\(o0) -> destroyEvent device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyEvent
  :: FunPtr (Ptr Device_T -> Event -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> Event -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyEvent - Destroy an event object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyEvent-event-01145# All submitted commands that refer
--     to @event@ /must/ have completed execution
--
-- -   #VUID-vkDestroyEvent-event-01146# If
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @event@ was created, a compatible set of callbacks
--     /must/ be provided here
--
-- -   #VUID-vkDestroyEvent-event-01147# If no
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @event@ was created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyEvent-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyEvent-event-parameter# If @event@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkDestroyEvent-pAllocator-parameter# If @pAllocator@ is not
--     @NULL@, @pAllocator@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' structure
--
-- -   #VUID-vkDestroyEvent-event-parent# If @event@ is a valid handle, it
--     /must/ have been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @event@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
destroyEvent :: forall io
              . (MonadIO io)
             => -- | @device@ is the logical device that destroys the event.
                Device
             -> -- | @event@ is the handle of the event to destroy.
                Event
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyEvent device event allocator = liftIO . evalContT $ do
  let vkDestroyEventPtr = pVkDestroyEvent (deviceCmds (device :: Device))
  lift $ unless (vkDestroyEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyEvent is null" Nothing Nothing
  let vkDestroyEvent' = mkVkDestroyEvent vkDestroyEventPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyEvent" (vkDestroyEvent' (deviceHandle (device)) (event) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetEventStatus
  :: FunPtr (Ptr Device_T -> Event -> IO Result) -> Ptr Device_T -> Event -> IO Result

-- | vkGetEventStatus - Retrieve the status of an event object
--
-- = Description
--
-- Upon success, 'getEventStatus' returns the state of the event object
-- with the following return codes:
--
-- +------------------------------------------+---------------------------+
-- | Status                                   | Meaning                   |
-- +==========================================+===========================+
-- | 'Vulkan.Core10.Enums.Result.EVENT_SET'   | The event specified by    |
-- |                                          | @event@ is signaled.      |
-- +------------------------------------------+---------------------------+
-- | 'Vulkan.Core10.Enums.Result.EVENT_RESET' | The event specified by    |
-- |                                          | @event@ is unsignaled.    |
-- +------------------------------------------+---------------------------+
--
-- Event Object Status Codes
--
-- If a 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent' or
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent' command is in a
-- command buffer that is in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
-- then the value returned by this command /may/ immediately be out of
-- date.
--
-- The state of an event /can/ be updated by the host. The state of the
-- event is immediately changed, and subsequent calls to 'getEventStatus'
-- will return the new state. If an event is already in the requested
-- state, then updating it to the same state has no effect.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.EVENT_SET'
--
--     -   'Vulkan.Core10.Enums.Result.EVENT_RESET'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
getEventStatus :: forall io
                . (MonadIO io)
               => -- | @device@ is the logical device that owns the event.
                  --
                  -- #VUID-vkGetEventStatus-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @event@ is the handle of the event to query.
                  --
                  -- #VUID-vkGetEventStatus-event-parameter# @event@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Event' handle
                  --
                  -- #VUID-vkGetEventStatus-event-parent# @event@ /must/ have been created,
                  -- allocated, or retrieved from @device@
                  Event
               -> io (Result)
getEventStatus device event = liftIO $ do
  let vkGetEventStatusPtr = pVkGetEventStatus (deviceCmds (device :: Device))
  unless (vkGetEventStatusPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetEventStatus is null" Nothing Nothing
  let vkGetEventStatus' = mkVkGetEventStatus vkGetEventStatusPtr
  r <- traceAroundEvent "vkGetEventStatus" (vkGetEventStatus' (deviceHandle (device)) (event))
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetEvent
  :: FunPtr (Ptr Device_T -> Event -> IO Result) -> Ptr Device_T -> Event -> IO Result

-- | vkSetEvent - Set an event to signaled state
--
-- = Description
--
-- When 'setEvent' is executed on the host, it defines an /event signal
-- operation/ which sets the event to the signaled state.
--
-- If @event@ is already in the signaled state when 'setEvent' is executed,
-- then 'setEvent' has no effect, and no event signal operation occurs.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSetEvent-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetEvent-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkSetEvent-event-parent# @event@ /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @event@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
setEvent :: forall io
          . (MonadIO io)
         => -- | @device@ is the logical device that owns the event.
            Device
         -> -- | @event@ is the event to set.
            Event
         -> io ()
setEvent device event = liftIO $ do
  let vkSetEventPtr = pVkSetEvent (deviceCmds (device :: Device))
  unless (vkSetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetEvent is null" Nothing Nothing
  let vkSetEvent' = mkVkSetEvent vkSetEventPtr
  r <- traceAroundEvent "vkSetEvent" (vkSetEvent' (deviceHandle (device)) (event))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetEvent
  :: FunPtr (Ptr Device_T -> Event -> IO Result) -> Ptr Device_T -> Event -> IO Result

-- | vkResetEvent - Reset an event to non-signaled state
--
-- = Description
--
-- When 'resetEvent' is executed on the host, it defines an /event unsignal
-- operation/ which resets the event to the unsignaled state.
--
-- If @event@ is already in the unsignaled state when 'resetEvent' is
-- executed, then 'resetEvent' has no effect, and no event unsignal
-- operation occurs.
--
-- == Valid Usage
--
-- -   #VUID-vkResetEvent-event-01148# @event@ /must/ not be waited on by a
--     'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' command that is
--     currently executing
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkResetEvent-device-parameter# @device@ /must/ be a valid
--     'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkResetEvent-event-parameter# @event@ /must/ be a valid
--     'Vulkan.Core10.Handles.Event' handle
--
-- -   #VUID-vkResetEvent-event-parent# @event@ /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @event@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
resetEvent :: forall io
            . (MonadIO io)
           => -- | @device@ is the logical device that owns the event.
              Device
           -> -- | @event@ is the event to reset.
              Event
           -> io ()
resetEvent device event = liftIO $ do
  let vkResetEventPtr = pVkResetEvent (deviceCmds (device :: Device))
  unless (vkResetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetEvent is null" Nothing Nothing
  let vkResetEvent' = mkVkResetEvent vkResetEventPtr
  r <- traceAroundEvent "vkResetEvent" (vkResetEvent' (deviceHandle (device)) (event))
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkEventCreateInfo - Structure specifying parameters of a newly created
-- event
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.EventCreateFlags.EventCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createEvent'
data EventCreateInfo = EventCreateInfo
  { -- | @flags@ is reserved for future use.
    --
    -- #VUID-VkEventCreateInfo-flags-zerobitmask# @flags@ /must/ be @0@
    flags :: EventCreateFlags }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventCreateInfo)
#endif
deriving instance Show EventCreateInfo

instance ToCStruct EventCreateInfo where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventCreateInfo{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EVENT_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr EventCreateFlags)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EVENT_CREATE_INFO)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct EventCreateInfo where
  peekCStruct p = do
    flags <- peek @EventCreateFlags ((p `plusPtr` 16 :: Ptr EventCreateFlags))
    pure $ EventCreateInfo
             flags

instance Storable EventCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero EventCreateInfo where
  zero = EventCreateInfo
           zero

