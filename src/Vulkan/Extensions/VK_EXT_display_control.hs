{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_display_control"
module Vulkan.Extensions.VK_EXT_display_control  ( displayPowerControlEXT
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

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
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
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkDisplayPowerControlEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainCounterEXT))
import Vulkan.Dynamic (DeviceCmds(pVkRegisterDeviceEventEXT))
import Vulkan.Dynamic (DeviceCmds(pVkRegisterDisplayEventEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Extensions.Handles (DisplayKHR)
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Core10.Handles (Fence)
import Vulkan.Core10.Handles (Fence(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT)
import Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagsEXT)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DisplayKHR(..))
import Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagBitsEXT(..))
import Vulkan.Extensions.VK_EXT_display_surface_counter (SurfaceCounterFlagsEXT)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDisplayPowerControlEXT
  :: FunPtr (Ptr Device_T -> DisplayKHR -> Ptr DisplayPowerInfoEXT -> IO Result) -> Ptr Device_T -> DisplayKHR -> Ptr DisplayPowerInfoEXT -> IO Result

-- No documentation found for TopLevel "vkDisplayPowerControlEXT"
displayPowerControlEXT :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkDisplayPowerControlEXT" "device"
                          Device
                       -> -- No documentation found for Nested "vkDisplayPowerControlEXT" "display"
                          DisplayKHR
                       -> -- No documentation found for Nested "vkDisplayPowerControlEXT" "pDisplayPowerInfo"
                          DisplayPowerInfoEXT
                       -> io ()
displayPowerControlEXT device display displayPowerInfo = liftIO . evalContT $ do
  let vkDisplayPowerControlEXTPtr = pVkDisplayPowerControlEXT (deviceCmds (device :: Device))
  lift $ unless (vkDisplayPowerControlEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDisplayPowerControlEXT is null" Nothing Nothing
  let vkDisplayPowerControlEXT' = mkVkDisplayPowerControlEXT vkDisplayPowerControlEXTPtr
  pDisplayPowerInfo <- ContT $ withCStruct (displayPowerInfo)
  r <- lift $ vkDisplayPowerControlEXT' (deviceHandle (device)) (display) pDisplayPowerInfo
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDeviceEventEXT
  :: FunPtr (Ptr Device_T -> Ptr DeviceEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result) -> Ptr Device_T -> Ptr DeviceEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result

-- No documentation found for TopLevel "vkRegisterDeviceEventEXT"
registerDeviceEventEXT :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkRegisterDeviceEventEXT" "device"
                          Device
                       -> -- No documentation found for Nested "vkRegisterDeviceEventEXT" "pDeviceEventInfo"
                          DeviceEventInfoEXT
                       -> -- No documentation found for Nested "vkRegisterDeviceEventEXT" "pAllocator"
                          ("allocator" ::: Maybe AllocationCallbacks)
                       -> io (Fence)
registerDeviceEventEXT device deviceEventInfo allocator = liftIO . evalContT $ do
  let vkRegisterDeviceEventEXTPtr = pVkRegisterDeviceEventEXT (deviceCmds (device :: Device))
  lift $ unless (vkRegisterDeviceEventEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkRegisterDeviceEventEXT is null" Nothing Nothing
  let vkRegisterDeviceEventEXT' = mkVkRegisterDeviceEventEXT vkRegisterDeviceEventEXTPtr
  pDeviceEventInfo <- ContT $ withCStruct (deviceEventInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFence <- ContT $ bracket (callocBytes @Fence 8) free
  r <- lift $ vkRegisterDeviceEventEXT' (deviceHandle (device)) pDeviceEventInfo pAllocator (pPFence)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFence <- lift $ peek @Fence pPFence
  pure $ (pFence)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDisplayEventEXT
  :: FunPtr (Ptr Device_T -> DisplayKHR -> Ptr DisplayEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result) -> Ptr Device_T -> DisplayKHR -> Ptr DisplayEventInfoEXT -> Ptr AllocationCallbacks -> Ptr Fence -> IO Result

-- No documentation found for TopLevel "vkRegisterDisplayEventEXT"
registerDisplayEventEXT :: forall io
                         . (MonadIO io)
                        => -- No documentation found for Nested "vkRegisterDisplayEventEXT" "device"
                           Device
                        -> -- No documentation found for Nested "vkRegisterDisplayEventEXT" "display"
                           DisplayKHR
                        -> -- No documentation found for Nested "vkRegisterDisplayEventEXT" "pDisplayEventInfo"
                           DisplayEventInfoEXT
                        -> -- No documentation found for Nested "vkRegisterDisplayEventEXT" "pAllocator"
                           ("allocator" ::: Maybe AllocationCallbacks)
                        -> io (Fence)
registerDisplayEventEXT device display displayEventInfo allocator = liftIO . evalContT $ do
  let vkRegisterDisplayEventEXTPtr = pVkRegisterDisplayEventEXT (deviceCmds (device :: Device))
  lift $ unless (vkRegisterDisplayEventEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkRegisterDisplayEventEXT is null" Nothing Nothing
  let vkRegisterDisplayEventEXT' = mkVkRegisterDisplayEventEXT vkRegisterDisplayEventEXTPtr
  pDisplayEventInfo <- ContT $ withCStruct (displayEventInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPFence <- ContT $ bracket (callocBytes @Fence 8) free
  r <- lift $ vkRegisterDisplayEventEXT' (deviceHandle (device)) (display) pDisplayEventInfo pAllocator (pPFence)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pFence <- lift $ peek @Fence pPFence
  pure $ (pFence)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainCounterEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> SurfaceCounterFlagBitsEXT -> Ptr Word64 -> IO Result

-- No documentation found for TopLevel "vkGetSwapchainCounterEXT"
getSwapchainCounterEXT :: forall io
                        . (MonadIO io)
                       => -- No documentation found for Nested "vkGetSwapchainCounterEXT" "device"
                          Device
                       -> -- No documentation found for Nested "vkGetSwapchainCounterEXT" "swapchain"
                          SwapchainKHR
                       -> -- No documentation found for Nested "vkGetSwapchainCounterEXT" "counter"
                          SurfaceCounterFlagBitsEXT
                       -> io (("counterValue" ::: Word64))
getSwapchainCounterEXT device swapchain counter = liftIO . evalContT $ do
  let vkGetSwapchainCounterEXTPtr = pVkGetSwapchainCounterEXT (deviceCmds (device :: Device))
  lift $ unless (vkGetSwapchainCounterEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSwapchainCounterEXT is null" Nothing Nothing
  let vkGetSwapchainCounterEXT' = mkVkGetSwapchainCounterEXT vkGetSwapchainCounterEXTPtr
  pPCounterValue <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ vkGetSwapchainCounterEXT' (deviceHandle (device)) (swapchain) (counter) (pPCounterValue)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pCounterValue <- lift $ peek @Word64 pPCounterValue
  pure $ (pCounterValue)



-- No documentation found for TopLevel "VkDisplayPowerInfoEXT"
data DisplayPowerInfoEXT = DisplayPowerInfoEXT
  { -- No documentation found for Nested "VkDisplayPowerInfoEXT" "powerState"
    powerState :: DisplayPowerStateEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayPowerInfoEXT)
#endif
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



-- No documentation found for TopLevel "VkDeviceEventInfoEXT"
data DeviceEventInfoEXT = DeviceEventInfoEXT
  { -- No documentation found for Nested "VkDeviceEventInfoEXT" "deviceEvent"
    deviceEvent :: DeviceEventTypeEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceEventInfoEXT)
#endif
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



-- No documentation found for TopLevel "VkDisplayEventInfoEXT"
data DisplayEventInfoEXT = DisplayEventInfoEXT
  { -- No documentation found for Nested "VkDisplayEventInfoEXT" "displayEvent"
    displayEvent :: DisplayEventTypeEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DisplayEventInfoEXT)
#endif
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



-- No documentation found for TopLevel "VkSwapchainCounterCreateInfoEXT"
data SwapchainCounterCreateInfoEXT = SwapchainCounterCreateInfoEXT
  { -- No documentation found for Nested "VkSwapchainCounterCreateInfoEXT" "surfaceCounters"
    surfaceCounters :: SurfaceCounterFlagsEXT }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainCounterCreateInfoEXT)
#endif
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


-- No documentation found for TopLevel "VkDisplayPowerStateEXT"
newtype DisplayPowerStateEXT = DisplayPowerStateEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDisplayPowerStateEXT" "VK_DISPLAY_POWER_STATE_OFF_EXT"
pattern DISPLAY_POWER_STATE_OFF_EXT     = DisplayPowerStateEXT 0
-- No documentation found for Nested "VkDisplayPowerStateEXT" "VK_DISPLAY_POWER_STATE_SUSPEND_EXT"
pattern DISPLAY_POWER_STATE_SUSPEND_EXT = DisplayPowerStateEXT 1
-- No documentation found for Nested "VkDisplayPowerStateEXT" "VK_DISPLAY_POWER_STATE_ON_EXT"
pattern DISPLAY_POWER_STATE_ON_EXT      = DisplayPowerStateEXT 2
{-# complete DISPLAY_POWER_STATE_OFF_EXT,
             DISPLAY_POWER_STATE_SUSPEND_EXT,
             DISPLAY_POWER_STATE_ON_EXT :: DisplayPowerStateEXT #-}

conNameDisplayPowerStateEXT :: String
conNameDisplayPowerStateEXT = "DisplayPowerStateEXT"

enumPrefixDisplayPowerStateEXT :: String
enumPrefixDisplayPowerStateEXT = "DISPLAY_POWER_STATE_"

showTableDisplayPowerStateEXT :: [(DisplayPowerStateEXT, String)]
showTableDisplayPowerStateEXT =
  [ (DISPLAY_POWER_STATE_OFF_EXT    , "OFF_EXT")
  , (DISPLAY_POWER_STATE_SUSPEND_EXT, "SUSPEND_EXT")
  , (DISPLAY_POWER_STATE_ON_EXT     , "ON_EXT")
  ]


instance Show DisplayPowerStateEXT where
showsPrec = enumShowsPrec enumPrefixDisplayPowerStateEXT
                          showTableDisplayPowerStateEXT
                          conNameDisplayPowerStateEXT
                          (\(DisplayPowerStateEXT x) -> x)
                          (showsPrec 11)


instance Read DisplayPowerStateEXT where
  readPrec = enumReadPrec enumPrefixDisplayPowerStateEXT
                          showTableDisplayPowerStateEXT
                          conNameDisplayPowerStateEXT
                          DisplayPowerStateEXT


-- No documentation found for TopLevel "VkDeviceEventTypeEXT"
newtype DeviceEventTypeEXT = DeviceEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDeviceEventTypeEXT" "VK_DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"
pattern DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT = DeviceEventTypeEXT 0
{-# complete DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT :: DeviceEventTypeEXT #-}

conNameDeviceEventTypeEXT :: String
conNameDeviceEventTypeEXT = "DeviceEventTypeEXT"

enumPrefixDeviceEventTypeEXT :: String
enumPrefixDeviceEventTypeEXT = "DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT"

showTableDeviceEventTypeEXT :: [(DeviceEventTypeEXT, String)]
showTableDeviceEventTypeEXT = [(DEVICE_EVENT_TYPE_DISPLAY_HOTPLUG_EXT, "")]


instance Show DeviceEventTypeEXT where
showsPrec = enumShowsPrec enumPrefixDeviceEventTypeEXT
                          showTableDeviceEventTypeEXT
                          conNameDeviceEventTypeEXT
                          (\(DeviceEventTypeEXT x) -> x)
                          (showsPrec 11)


instance Read DeviceEventTypeEXT where
  readPrec =
    enumReadPrec enumPrefixDeviceEventTypeEXT showTableDeviceEventTypeEXT conNameDeviceEventTypeEXT DeviceEventTypeEXT


-- No documentation found for TopLevel "VkDisplayEventTypeEXT"
newtype DisplayEventTypeEXT = DisplayEventTypeEXT Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDisplayEventTypeEXT" "VK_DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"
pattern DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT = DisplayEventTypeEXT 0
{-# complete DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT :: DisplayEventTypeEXT #-}

conNameDisplayEventTypeEXT :: String
conNameDisplayEventTypeEXT = "DisplayEventTypeEXT"

enumPrefixDisplayEventTypeEXT :: String
enumPrefixDisplayEventTypeEXT = "DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT"

showTableDisplayEventTypeEXT :: [(DisplayEventTypeEXT, String)]
showTableDisplayEventTypeEXT = [(DISPLAY_EVENT_TYPE_FIRST_PIXEL_OUT_EXT, "")]


instance Show DisplayEventTypeEXT where
showsPrec = enumShowsPrec enumPrefixDisplayEventTypeEXT
                          showTableDisplayEventTypeEXT
                          conNameDisplayEventTypeEXT
                          (\(DisplayEventTypeEXT x) -> x)
                          (showsPrec 11)


instance Read DisplayEventTypeEXT where
  readPrec = enumReadPrec enumPrefixDisplayEventTypeEXT
                          showTableDisplayEventTypeEXT
                          conNameDisplayEventTypeEXT
                          DisplayEventTypeEXT


type EXT_DISPLAY_CONTROL_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_SPEC_VERSION"
pattern EXT_DISPLAY_CONTROL_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DISPLAY_CONTROL_SPEC_VERSION = 1


type EXT_DISPLAY_CONTROL_EXTENSION_NAME = "VK_EXT_display_control"

-- No documentation found for TopLevel "VK_EXT_DISPLAY_CONTROL_EXTENSION_NAME"
pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DISPLAY_CONTROL_EXTENSION_NAME = "VK_EXT_display_control"

