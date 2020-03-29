{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_EXT_display_control  ( displayPowerControlEXT
                                                          , registerDeviceEventEXT
                                                          , registerDisplayEventEXT
                                                          , getSwapchainCounterEXT
                                                          , DisplayPowerInfoEXT(..)
                                                          , DeviceEventInfoEXT(..)
                                                          , DisplayEventInfoEXT(..)
                                                          , SwapchainCounterCreateInfoEXT(..)
                                                          , DisplayPowerStateEXT( DISPLAY_POWER_STATE_OFF_EXT
                                                                                , DISPLAY_POWER_STATE_SUSPEND_EXT
                                                                                , DISPLAY_POWER_STATE_ON_EXT
                                                                                , ..
                                                                                )
                                                          , DeviceEventTypeEXT( DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT
                                                                              , ..
                                                                              )
                                                          , DisplayEventTypeEXT( DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT
                                                                               , ..
                                                                               )
                                                          , EXT_DISPLAY_CONTROL_SPEC_VERSION
                                                          , pattern EXT_DISPLAY_CONTROL_SPEC_VERSION
                                                          , EXT_DISPLAY_CONTROL_EXTENSION_NAME
                                                          , pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME
                                                          , DisplayKHR(..)
                                                          , SwapchainKHR(..)
                                                          , SurfaceCounterFlagBitsEXT(..)
                                                          , SurfaceCounterFlagsEXT
                                                          ) where

import Control.Exception.Base (bracket)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import Data.Word (Word64)
import Text.Read.Lex (Lexeme(Ident))
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Graphics.Vulkan.NamedType ((:::))
import Graphics.Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Graphics.Vulkan.Core10.Handles (Device)
import Graphics.Vulkan.Core10.Handles (Device(..))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkDisplayPowerControlEXT))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainCounterEXT))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkRegisterDeviceEventEXT))
import Graphics.Vulkan.Dynamic (DeviceCmds(pVkRegisterDisplayEventEXT))
import Graphics.Vulkan.Core10.Handles (Device_T)
import Graphics.Vulkan.Extensions.Handles (DisplayKHR)
import Graphics.Vulkan.Extensions.Handles (DisplayKHR(..))
import Graphics.Vulkan.Core10.Handles (Fence)
import Graphics.Vulkan.Core10.Handles (Fence(..))
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (FromCStruct(..))
import Graphics.Vulkan.Core10.Enums.Result (Result)
import Graphics.Vulkan.Core10.Enums.Result (Result(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType)
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT)
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT(..))
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagsEXT)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
import Graphics.Vulkan.CStruct (ToCStruct)
import Graphics.Vulkan.CStruct (ToCStruct(..))
import Graphics.Vulkan.Exception (VulkanException(..))
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT))
import Graphics.Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Graphics.Vulkan.Extensions.Handles (DisplayKHR(..))
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT(..))
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagsEXT)
import Graphics.Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDisplayPowerControlEXT
  :: FunPtr (Ptr Device_T -> DisplayKHR -> Ptr DisplayPowerInfoEXT -> IO Result) -> Ptr Device_T -> DisplayKHR -> Ptr DisplayPowerInfoEXT -> IO Result

-- | vkDisplayPowerControlEXT - Set the power state of a display
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is a logical device
--     associated with 'Graphics.Vulkan.Extensions.WSITypes.Display'.
--
-- -   'Graphics.Vulkan.Extensions.WSITypes.Display' is the display whose
--     power state is modified.
--
-- -   @pDisplayPowerInfo@ is a 'DisplayPowerInfoEXT' structure specifying
--     the new power state of
--     'Graphics.Vulkan.Extensions.WSITypes.Display'.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Extensions.WSITypes.Display' /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- -   @pDisplayPowerInfo@ /must/ be a valid pointer to a valid
--     'DisplayPowerInfoEXT' structure
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Device', and
--     'Graphics.Vulkan.Extensions.WSITypes.Display' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Extensions.Handles.DisplayKHR', 'DisplayPowerInfoEXT'
displayPowerControlEXT :: Device -> DisplayKHR -> DisplayPowerInfoEXT -> IO ()
displayPowerControlEXT device display displayPowerInfo = evalContT $ do
  let vkDisplayPowerControlEXT' = mkVkDisplayPowerControlEXT (pVkDisplayPowerControlEXT (deviceCmds (device :: Device)))
  pDisplayPowerInfo <- ContT $ withCStruct (displayPowerInfo)
  _ <- lift $ vkDisplayPowerControlEXT' (deviceHandle (device)) (display) pDisplayPowerInfo
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDeviceEventEXT
  :: FunPtr (Ptr Device_T -> Ptr DeviceEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result) -> Ptr Device_T -> Ptr DeviceEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result

-- | vkRegisterDeviceEventEXT - Signal a fence when a device event occurs
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is a logical device on which
--     the event /may/ occur.
--
-- -   @pDeviceEventInfo@ is a pointer to a 'DeviceEventInfoEXT' structure
--     describing the event of interest to the application.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFence@ is a pointer to a handle in which the resulting fence
--     object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @pDeviceEventInfo@ /must/ be a valid pointer to a valid
--     'DeviceEventInfoEXT' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pFence@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.Fence' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'DeviceEventInfoEXT',
-- 'Graphics.Vulkan.Core10.Handles.Fence'
registerDeviceEventEXT :: Device -> DeviceEventInfoEXT -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (Fence)
registerDeviceEventEXT device deviceEventInfo allocator = evalContT $ do
  let vkRegisterDeviceEventEXT' = mkVkRegisterDeviceEventEXT (pVkRegisterDeviceEventEXT (deviceCmds (device :: Device)))
  pDeviceEventInfo <- ContT $ withCStruct (deviceEventInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFence <- ContT $ bracket (callocBytes @Fence 8) free
  _ <- lift $ vkRegisterDeviceEventEXT' (deviceHandle (device)) pDeviceEventInfo pAllocator (pPFence)
  pFence <- lift $ peek @Fence pPFence
  pure $ (pFence)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDisplayEventEXT
  :: FunPtr (Ptr Device_T -> DisplayKHR -> Ptr DisplayEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result) -> Ptr Device_T -> DisplayKHR -> Ptr DisplayEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result

-- | vkRegisterDisplayEventEXT - Signal a fence when a display event occurs
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is a logical device
--     associated with 'Graphics.Vulkan.Extensions.WSITypes.Display'
--
-- -   'Graphics.Vulkan.Extensions.WSITypes.Display' is the display on
--     which the event /may/ occur.
--
-- -   @pDisplayEventInfo@ is a pointer to a 'DisplayEventInfoEXT'
--     structure describing the event of interest to the application.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pFence@ is a pointer to a handle in which the resulting fence
--     object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   'Graphics.Vulkan.Extensions.WSITypes.Display' /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.DisplayKHR' handle
--
-- -   @pDisplayEventInfo@ /must/ be a valid pointer to a valid
--     'DisplayEventInfoEXT' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   @pFence@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.Core10.Handles.Fence' handle
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Device', and
--     'Graphics.Vulkan.Extensions.WSITypes.Display' /must/ have been
--     created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.PhysicalDevice'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Graphics.Vulkan.Core10.Handles.Device', 'DisplayEventInfoEXT',
-- 'Graphics.Vulkan.Extensions.Handles.DisplayKHR',
-- 'Graphics.Vulkan.Core10.Handles.Fence'
registerDisplayEventEXT :: Device -> DisplayKHR -> DisplayEventInfoEXT -> ("allocator" ::: Maybe AllocationCallbacks) -> IO (Fence)
registerDisplayEventEXT device display displayEventInfo allocator = evalContT $ do
  let vkRegisterDisplayEventEXT' = mkVkRegisterDisplayEventEXT (pVkRegisterDisplayEventEXT (deviceCmds (device :: Device)))
  pDisplayEventInfo <- ContT $ withCStruct (displayEventInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFence <- ContT $ bracket (callocBytes @Fence 8) free
  _ <- lift $ vkRegisterDisplayEventEXT' (deviceHandle (device)) (display) pDisplayEventInfo pAllocator (pPFence)
  pFence <- lift $ peek @Fence pPFence
  pure $ (pFence)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainCounterEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO Result

-- | vkGetSwapchainCounterEXT - Query the current value of a surface counter
--
-- = Parameters
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' is the
--     'Graphics.Vulkan.Core10.Handles.Device' associated with @swapchain@.
--
-- -   @swapchain@ is the swapchain from which to query the counter value.
--
-- -   @counter@ is the counter to query.
--
-- -   @pCounterValue@ will return the current value of the counter.
--
-- = Description
--
-- If a counter is not available because the swapchain is out of date, the
-- implementation /may/ return
-- 'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'.
--
-- == Valid Usage
--
-- -   One or more present commands on @swapchain@ /must/ have been
--     processed by the presentation engine.
--
-- == Valid Usage (Implicit)
--
-- -   'Graphics.Vulkan.Core10.Handles.Device' /must/ be a valid
--     'Graphics.Vulkan.Core10.Handles.Device' handle
--
-- -   @swapchain@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   @counter@ /must/ be a valid
--     'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCounterFlagBitsEXT'
--     value
--
-- -   @pCounterValue@ /must/ be a valid pointer to a @uint64_t@ value
--
-- -   Both of 'Graphics.Vulkan.Core10.Handles.Device', and @swapchain@
--     /must/ have been created, allocated, or retrieved from the same
--     'Graphics.Vulkan.Core10.Handles.Instance'
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Graphics.Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Handles.Device',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCounterFlagBitsEXT',
-- 'Graphics.Vulkan.Extensions.Handles.SwapchainKHR'
getSwapchainCounterEXT :: Device -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> IO (("counterValue" ::: Word64))
getSwapchainCounterEXT device swapchain counter = evalContT $ do
  let vkGetSwapchainCounterEXT' = mkVkGetSwapchainCounterEXT (pVkGetSwapchainCounterEXT (deviceCmds (device :: Device)))
  pPCounterValue <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ vkGetSwapchainCounterEXT' (deviceHandle (device)) (swapchain) (counter) (pPCounterValue)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCounterValue <- lift $ peek @Word64 pPCounterValue
  pure $ (pCounterValue)


-- | VkDisplayPowerInfoEXT - Describe the power state of a display
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DisplayPowerStateEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'displayPowerControlEXT'
data DisplayPowerInfoEXT = DisplayPowerInfoEXT
  { -- | @powerState@ /must/ be a valid 'DisplayPowerStateEXT' value
    powerState :: DisplayPowerStateEXT }
  deriving (Typeable)
deriving instance Show DisplayPowerInfoEXT

instance ToCStruct DisplayPowerInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayPowerInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPowerStateEXT)) (powerState)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayPowerStateEXT)) (zero)
    f

instance FromCStruct DisplayPowerInfoEXT where
  peekCStruct p = do
    powerState <- peek @DisplayPowerStateEXT ((p `plusPtr` 16 :: Ptr DisplayPowerStateEXT))
    pure $ DisplayPowerInfoEXT
             powerState

instance Storable DisplayPowerInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayPowerInfoEXT where
  zero = DisplayPowerInfoEXT
           zero


-- | VkDeviceEventInfoEXT - Describe a device event to create
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DeviceEventTypeEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'registerDeviceEventEXT'
data DeviceEventInfoEXT = DeviceEventInfoEXT
  { -- | @deviceEvent@ /must/ be a valid 'DeviceEventTypeEXT' value
    deviceEvent :: DeviceEventTypeEXT }
  deriving (Typeable)
deriving instance Show DeviceEventInfoEXT

instance ToCStruct DeviceEventInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceEventInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceEventTypeEXT)) (deviceEvent)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DeviceEventTypeEXT)) (zero)
    f

instance FromCStruct DeviceEventInfoEXT where
  peekCStruct p = do
    deviceEvent <- peek @DeviceEventTypeEXT ((p `plusPtr` 16 :: Ptr DeviceEventTypeEXT))
    pure $ DeviceEventInfoEXT
             deviceEvent

instance Storable DeviceEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DeviceEventInfoEXT where
  zero = DeviceEventInfoEXT
           zero


-- | VkDisplayEventInfoEXT - Describe a display event to create
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'DisplayEventTypeEXT',
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'registerDisplayEventEXT'
data DisplayEventInfoEXT = DisplayEventInfoEXT
  { -- | @displayEvent@ /must/ be a valid 'DisplayEventTypeEXT' value
    displayEvent :: DisplayEventTypeEXT }
  deriving (Typeable)
deriving instance Show DisplayEventInfoEXT

instance ToCStruct DisplayEventInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DisplayEventInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayEventTypeEXT)) (displayEvent)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr DisplayEventTypeEXT)) (zero)
    f

instance FromCStruct DisplayEventInfoEXT where
  peekCStruct p = do
    displayEvent <- peek @DisplayEventTypeEXT ((p `plusPtr` 16 :: Ptr DisplayEventTypeEXT))
    pure $ DisplayEventInfoEXT
             displayEvent

instance Storable DisplayEventInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero DisplayEventInfoEXT where
  zero = DisplayEventInfoEXT
           zero


-- | VkSwapchainCounterCreateInfoEXT - Specify the surface counters desired
--
-- == Valid Usage
--
-- -   The bits in @surfaceCounters@ /must/ be supported by
--     'Graphics.Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@surface@,
--     as reported by
--     'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.getPhysicalDeviceSurfaceCapabilities2EXT'.
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT'
--
-- -   @surfaceCounters@ /must/ be a valid combination of
--     'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCounterFlagBitsEXT'
--     values
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCounterFlagsEXT'
data SwapchainCounterCreateInfoEXT = SwapchainCounterCreateInfoEXT
  { -- | @surfaceCounters@ is a bitmask of
    -- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCounterFlagBitsEXT'
    -- specifying surface counters to enable for the swapchain.
    surfaceCounters :: SurfaceCounterFlagsEXT }
  deriving (Typeable)
deriving instance Show SwapchainCounterCreateInfoEXT

instance ToCStruct SwapchainCounterCreateInfoEXT where
  withCStruct x f = allocaBytesAligned 24 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainCounterCreateInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SurfaceCounterFlagsEXT)) (surfaceCounters)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainCounterCreateInfoEXT where
  peekCStruct p = do
    surfaceCounters <- peek @SurfaceCounterFlagsEXT ((p `plusPtr` 16 :: Ptr SurfaceCounterFlagsEXT))
    pure $ SwapchainCounterCreateInfoEXT
             surfaceCounters

instance Storable SwapchainCounterCreateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainCounterCreateInfoEXT where
  zero = SwapchainCounterCreateInfoEXT
           zero


-- | VkDisplayPowerStateEXT - Possible power states for a display
--
-- = See Also
--
-- 'DisplayPowerInfoEXT'
newtype DisplayPowerStateEXT = DisplayPowerStateEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DISPLAY_POWER_STATE_OFF_EXT' specifies that the display is powered
-- down.
pattern DISPLAY_POWER_STATE_OFF_EXT = DisplayPowerStateEXT 0
-- | 'DISPLAY_POWER_STATE_SUSPEND_EXT' specifies that the display is put into
-- a low power mode, from which it /may/ be able to transition back to
-- 'DISPLAY_POWER_STATE_ON_EXT' more quickly than if it were in
-- 'DISPLAY_POWER_STATE_OFF_EXT'. This state /may/ be the same as
-- 'DISPLAY_POWER_STATE_OFF_EXT'.
pattern DISPLAY_POWER_STATE_SUSPEND_EXT = DisplayPowerStateEXT 1
-- | 'DISPLAY_POWER_STATE_ON_EXT' specifies that the display is powered on.
pattern DISPLAY_POWER_STATE_ON_EXT = DisplayPowerStateEXT 2
{-# complete DISPLAY_POWER_STATE_OFF_EXT,
             DISPLAY_POWER_STATE_SUSPEND_EXT,
             DISPLAY_POWER_STATE_ON_EXT :: DisplayPowerStateEXT #-}

instance Show DisplayPowerStateEXT where
  showsPrec p = \case
    DISPLAY_POWER_STATE_OFF_EXT -> showString "DISPLAY_POWER_STATE_OFF_EXT"
    DISPLAY_POWER_STATE_SUSPEND_EXT -> showString "DISPLAY_POWER_STATE_SUSPEND_EXT"
    DISPLAY_POWER_STATE_ON_EXT -> showString "DISPLAY_POWER_STATE_ON_EXT"
    DisplayPowerStateEXT x -> showParen (p >= 11) (showString "DisplayPowerStateEXT " . showsPrec 11 x)

instance Read DisplayPowerStateEXT where
  readPrec = parens (choose [("DISPLAY_POWER_STATE_OFF_EXT", pure DISPLAY_POWER_STATE_OFF_EXT)
                            , ("DISPLAY_POWER_STATE_SUSPEND_EXT", pure DISPLAY_POWER_STATE_SUSPEND_EXT)
                            , ("DISPLAY_POWER_STATE_ON_EXT", pure DISPLAY_POWER_STATE_ON_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DisplayPowerStateEXT")
                       v <- step readPrec
                       pure (DisplayPowerStateEXT v)))


-- | VkDeviceEventTypeEXT - Events that can occur on a device object
--
-- = See Also
--
-- 'DeviceEventInfoEXT'
newtype DeviceEventTypeEXT = DeviceEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT' specifies that the fence is
-- signaled when a display is plugged into or unplugged from the specified
-- device. Applications /can/ use this notification to determine when they
-- need to re-enumerate the available displays on a device.
pattern DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = DeviceEventTypeEXT 0
{-# complete DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT :: DeviceEventTypeEXT #-}

instance Show DeviceEventTypeEXT where
  showsPrec p = \case
    DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT -> showString "DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
    DeviceEventTypeEXT x -> showParen (p >= 11) (showString "DeviceEventTypeEXT " . showsPrec 11 x)

instance Read DeviceEventTypeEXT where
  readPrec = parens (choose [("DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT", pure DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DeviceEventTypeEXT")
                       v <- step readPrec
                       pure (DeviceEventTypeEXT v)))


-- | VkDisplayEventTypeEXT - Events that can occur on a display object
--
-- = See Also
--
-- 'DisplayEventInfoEXT'
newtype DisplayEventTypeEXT = DisplayEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT' specifies that the fence is
-- signaled when the first pixel of the next display refresh cycle leaves
-- the display engine for the display.
pattern DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = DisplayEventTypeEXT 0
{-# complete DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT :: DisplayEventTypeEXT #-}

instance Show DisplayEventTypeEXT where
  showsPrec p = \case
    DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT -> showString "DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
    DisplayEventTypeEXT x -> showParen (p >= 11) (showString "DisplayEventTypeEXT " . showsPrec 11 x)

instance Read DisplayEventTypeEXT where
  readPrec = parens (choose [("DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT", pure DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DisplayEventTypeEXT")
                       v <- step readPrec
                       pure (DisplayEventTypeEXT v)))


type EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_SPEC_VERSION"
pattern EXT_DISPLAY_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISPLAY_CONTROL_SPEC_VERSION = 1


type EXT_DISPLAY_CONTROL_EXTENSION_NAME = "VK_EXT_display_control"

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME"
pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME = "VK_EXT_display_control"

