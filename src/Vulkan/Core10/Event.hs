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
                            , EventCreateFlagBits(..)
                            , EventCreateFlags
                            ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
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
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCreateEvent))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyEvent))
import Vulkan.Dynamic (DeviceCmds(pVkGetEventStatus))
import Vulkan.Dynamic (DeviceCmds(pVkResetEvent))
import Vulkan.Dynamic (DeviceCmds(pVkSetEvent))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Handles (Event)
import Vulkan.Core10.Handles (Event(..))
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ExportMetalObjectCreateInfoEXT)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_metal_objects (ImportMetalSharedEventInfoEXT)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_EVENT_CREATE_INFO))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Core10.Handles (Event(..))
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlagBits(..))
import Vulkan.Core10.Enums.EventCreateFlagBits (EventCreateFlags)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateEvent
  :: FunPtr (Ptr Device_T -> Ptr (SomeStruct EventCreateInfo) -> Ptr AllocationCallbacks -> Ptr Event -> IO Result) -> Ptr Device_T -> Ptr (SomeStruct EventCreateInfo) -> Ptr AllocationCallbacks -> Ptr Event -> IO Result

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
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#synchronization-events events>,
--     and 'createEvent' /must/ not be used
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event',
-- 'EventCreateInfo'
createEvent :: forall a io
             . (Extendss EventCreateInfo a, PokeChain a, MonadIO io)
            => -- | @device@ is the logical device that creates the event.
               Device
            -> -- | @pCreateInfo@ is a pointer to a 'EventCreateInfo' structure containing
               -- information about how the event is to be created.
               (EventCreateInfo a)
            -> -- | @pAllocator@ controls host memory allocation as described in the
               -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
               -- chapter.
               ("allocator" ::: Maybe AllocationCallbacks)
            -> io (Event)
createEvent device createInfo allocator = liftIO . evalContT $ do
  let vkCreateEventPtr = pVkCreateEvent (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateEvent is null" Nothing Nothing
  let vkCreateEvent' = mkVkCreateEvent vkCreateEventPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPEvent <- ContT $ bracket (callocBytes @Event 8) free
  r <- lift $ traceAroundEvent "vkCreateEvent" (vkCreateEvent'
                                                  (deviceHandle (device))
                                                  (forgetExtensions pCreateInfo)
                                                  pAllocator
                                                  (pPEvent))
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
withEvent :: forall a io r . (Extendss EventCreateInfo a, PokeChain a, MonadIO io) => Device -> EventCreateInfo a -> Maybe AllocationCallbacks -> (io Event -> (Event -> io ()) -> r) -> r
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
destroyEvent :: forall io
              . (MonadIO io)
             => -- | @device@ is the logical device that destroys the event.
                Device
             -> -- | @event@ is the handle of the event to destroy.
                Event
             -> -- | @pAllocator@ controls host memory allocation as described in the
                -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                -- chapter.
                ("allocator" ::: Maybe AllocationCallbacks)
             -> io ()
destroyEvent device event allocator = liftIO . evalContT $ do
  let vkDestroyEventPtr = pVkDestroyEvent (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkDestroyEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyEvent is null" Nothing Nothing
  let vkDestroyEvent' = mkVkDestroyEvent vkDestroyEventPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyEvent" (vkDestroyEvent'
                                              (deviceHandle (device))
                                              (event)
                                              pAllocator)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-lifecycle pending state>,
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
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
                  -- #VUID-vkGetEventStatus-event-03940# @event@ /must/ not have been created
                  -- with
                  -- 'Vulkan.Core10.Enums.EventCreateFlagBits.EVENT_CREATE_DEVICE_ONLY_BIT'
                  --
                  -- #VUID-vkGetEventStatus-event-parameter# @event@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Event' handle
                  --
                  -- #VUID-vkGetEventStatus-event-parent# @event@ /must/ have been created,
                  -- allocated, or retrieved from @device@
                  Event
               -> io (Result)
getEventStatus device event = liftIO $ do
  let vkGetEventStatusPtr = pVkGetEventStatus (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetEventStatusPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetEventStatus is null" Nothing Nothing
  let vkGetEventStatus' = mkVkGetEventStatus vkGetEventStatusPtr
  r <- traceAroundEvent "vkGetEventStatus" (vkGetEventStatus'
                                              (deviceHandle (device))
                                              (event))
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
-- Note
--
-- If a command buffer is waiting for an event to be signaled from the
-- host, the application must signal the event before submitting the
-- command buffer, as described in the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#commandbuffers-submission-progress queue forward progress>
-- section.
--
-- == Valid Usage
--
-- -   #VUID-vkSetEvent-event-03941# @event@ /must/ not have been created
--     with
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EVENT_CREATE_DEVICE_ONLY_BIT'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
setEvent :: forall io
          . (MonadIO io)
         => -- | @device@ is the logical device that owns the event.
            Device
         -> -- | @event@ is the event to set.
            Event
         -> io ()
setEvent device event = liftIO $ do
  let vkSetEventPtr = pVkSetEvent (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkSetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetEvent is null" Nothing Nothing
  let vkSetEvent' = mkVkSetEvent vkSetEventPtr
  r <- traceAroundEvent "vkSetEvent" (vkSetEvent'
                                        (deviceHandle (device))
                                        (event))
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
-- -   #VUID-vkResetEvent-event-03821# There /must/ be an execution
--     dependency between 'resetEvent' and the execution of any
--     'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents' that includes
--     @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkResetEvent-event-03822# There /must/ be an execution
--     dependency between 'resetEvent' and the execution of any
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWaitEvents2'
--     that includes @event@ in its @pEvents@ parameter
--
-- -   #VUID-vkResetEvent-event-03823# @event@ /must/ not have been created
--     with
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EVENT_CREATE_DEVICE_ONLY_BIT'
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Event'
resetEvent :: forall io
            . (MonadIO io)
           => -- | @device@ is the logical device that owns the event.
              Device
           -> -- | @event@ is the event to reset.
              Event
           -> io ()
resetEvent device event = liftIO $ do
  let vkResetEventPtr = pVkResetEvent (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkResetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetEvent is null" Nothing Nothing
  let vkResetEvent' = mkVkResetEvent vkResetEventPtr
  r <- traceAroundEvent "vkResetEvent" (vkResetEvent'
                                          (deviceHandle (device))
                                          (event))
  when (r < SUCCESS) (throwIO (VulkanException r))


-- | VkEventCreateInfo - Structure specifying parameters of a newly created
-- event
--
-- == Valid Usage
--
-- -   #VUID-VkEventCreateInfo-pNext-06790# If the @pNext@ chain includes a
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--     structure, its @exportObjectType@ member /must/ be
--     'Vulkan.Extensions.VK_EXT_metal_objects.EXPORT_METAL_OBJECT_TYPE_METAL_SHARED_EVENT_BIT_EXT'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkEventCreateInfo-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_EVENT_CREATE_INFO'
--
-- -   #VUID-VkEventCreateInfo-pNext-pNext# Each @pNext@ member of any
--     structure (including this one) in the @pNext@ chain /must/ be either
--     @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalSharedEventInfoEXT'
--
-- -   #VUID-VkEventCreateInfo-sType-unique# The @sType@ value of each
--     struct in the @pNext@ chain /must/ be unique, with the exception of
--     structures of type
--     'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalObjectCreateInfoEXT'
--
-- -   #VUID-VkEventCreateInfo-flags-parameter# @flags@ /must/ be a valid
--     combination of
--     'Vulkan.Core10.Enums.EventCreateFlagBits.EventCreateFlagBits' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Enums.EventCreateFlagBits.EventCreateFlags',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'createEvent'
data EventCreateInfo (es :: [Type]) = EventCreateInfo
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.EventCreateFlagBits.EventCreateFlagBits' defining
    -- additional creation parameters.
    flags :: EventCreateFlags
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (EventCreateInfo (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (EventCreateInfo es)

instance Extensible EventCreateInfo where
  extensibleTypeName = "EventCreateInfo"
  setNext EventCreateInfo{..} next' = EventCreateInfo{next = next', ..}
  getNext EventCreateInfo{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends EventCreateInfo e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImportMetalSharedEventInfoEXT = Just f
    | Just Refl <- eqT @e @ExportMetalObjectCreateInfoEXT = Just f
    | otherwise = Nothing

instance ( Extendss EventCreateInfo es
         , PokeChain es ) => ToCStruct (EventCreateInfo es) where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p EventCreateInfo{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EVENT_CREATE_INFO)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr EventCreateFlags)) (flags)
    lift $ f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_EVENT_CREATE_INFO)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ f

instance ( Extendss EventCreateInfo es
         , PeekChain es ) => FromCStruct (EventCreateInfo es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @EventCreateFlags ((p `plusPtr` 16 :: Ptr EventCreateFlags))
    pure $ EventCreateInfo
             next flags

instance es ~ '[] => Zero (EventCreateInfo es) where
  zero = EventCreateInfo
           ()
           zero

