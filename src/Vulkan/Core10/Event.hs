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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero(..))
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

-- No documentation found for TopLevel "vkCreateEvent"
createEvent :: forall io
             . (MonadIO io)
            => -- No documentation found for Nested "vkCreateEvent" "device"
               Device
            -> -- No documentation found for Nested "vkCreateEvent" "pCreateInfo"
               EventCreateInfo
            -> -- No documentation found for Nested "vkCreateEvent" "pAllocator"
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
  r <- lift $ vkCreateEvent' (deviceHandle (device)) pCreateInfo pAllocator (pPEvent)
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

-- No documentation found for TopLevel "vkDestroyEvent"
destroyEvent :: forall io
              . (MonadIO io)
             => -- No documentation found for Nested "vkDestroyEvent" "device"
                Device
             -> -- No documentation found for Nested "vkDestroyEvent" "event"
                Event
             -> -- No documentation found for Nested "vkDestroyEvent" "pAllocator"
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
  lift $ vkDestroyEvent' (deviceHandle (device)) (event) pAllocator
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetEventStatus
  :: FunPtr (Ptr Device_T -> Event -> IO Result) -> Ptr Device_T -> Event -> IO Result

-- No documentation found for TopLevel "vkGetEventStatus"
getEventStatus :: forall io
                . (MonadIO io)
               => -- No documentation found for Nested "vkGetEventStatus" "device"
                  Device
               -> -- No documentation found for Nested "vkGetEventStatus" "event"
                  Event
               -> io (Result)
getEventStatus device event = liftIO $ do
  let vkGetEventStatusPtr = pVkGetEventStatus (deviceCmds (device :: Device))
  unless (vkGetEventStatusPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetEventStatus is null" Nothing Nothing
  let vkGetEventStatus' = mkVkGetEventStatus vkGetEventStatusPtr
  r <- vkGetEventStatus' (deviceHandle (device)) (event)
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetEvent
  :: FunPtr (Ptr Device_T -> Event -> IO Result) -> Ptr Device_T -> Event -> IO Result

-- No documentation found for TopLevel "vkSetEvent"
setEvent :: forall io
          . (MonadIO io)
         => -- No documentation found for Nested "vkSetEvent" "device"
            Device
         -> -- No documentation found for Nested "vkSetEvent" "event"
            Event
         -> io ()
setEvent device event = liftIO $ do
  let vkSetEventPtr = pVkSetEvent (deviceCmds (device :: Device))
  unless (vkSetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetEvent is null" Nothing Nothing
  let vkSetEvent' = mkVkSetEvent vkSetEventPtr
  r <- vkSetEvent' (deviceHandle (device)) (event)
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetEvent
  :: FunPtr (Ptr Device_T -> Event -> IO Result) -> Ptr Device_T -> Event -> IO Result

-- No documentation found for TopLevel "vkResetEvent"
resetEvent :: forall io
            . (MonadIO io)
           => -- No documentation found for Nested "vkResetEvent" "device"
              Device
           -> -- No documentation found for Nested "vkResetEvent" "event"
              Event
           -> io ()
resetEvent device event = liftIO $ do
  let vkResetEventPtr = pVkResetEvent (deviceCmds (device :: Device))
  unless (vkResetEventPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkResetEvent is null" Nothing Nothing
  let vkResetEvent' = mkVkResetEvent vkResetEventPtr
  r <- vkResetEvent' (deviceHandle (device)) (event)
  when (r < SUCCESS) (throwIO (VulkanException r))



-- No documentation found for TopLevel "VkEventCreateInfo"
data EventCreateInfo = EventCreateInfo
  { -- No documentation found for Nested "VkEventCreateInfo" "flags"
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

