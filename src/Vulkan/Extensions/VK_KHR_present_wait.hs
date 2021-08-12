{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_wait - device extension
--
-- == VK_KHR_present_wait
--
-- [__Name String__]
--     @VK_KHR_present_wait@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     249
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_swapchain@
--
--     -   Requires @VK_KHR_present_id@
--
-- [__Contact__]
--
--     -   Keith Packard
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_present_wait:%20&body=@keithp%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-05-15
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Keith Packard, Valve
--
--     -   Ian Elliott, Google
--
--     -   Tobias Hector, AMD
--
--     -   Daniel Stone, Collabora
--
-- == Description
--
-- This device extension allows an application that uses the
-- @VK_KHR_swapchain@ extension to wait for present operations to complete.
-- An application can use this to monitor and control the pacing of the
-- application by managing the number of outstanding images yet to be
-- presented.
--
-- == New Commands
--
-- -   'waitForPresentKHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentWaitFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_WAIT_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_WAIT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR'
--
-- == Issues
--
-- 1) When does the wait finish?
--
-- __RESOLVED__. The wait will finish when the present is visible to the
-- user. There is no requirement for any precise timing relationship
-- between the presentation of the image to the user, but implementations
-- /should/ signal the wait as close as possible to the presentation of the
-- first pixel in the new image to the user.
--
-- 2) Should this use fences or other existing synchronization mechanism.
--
-- __RESOLVED__. Because display and rendering are often implemented in
-- separate drivers, this extension will provide a separate synchronization
-- API.
--
-- 3) Should this extension share present identification with other
-- extensions?
--
-- __RESOLVED__. Yes. A new extension, VK_KHR_present_id, should be created
-- to provide a shared structure for presentation identifiers.
--
-- 4) What happens when presentations complete out of order wrt calls to
-- vkQueuePresent? This could happen if the semaphores for the
-- presentations were ready out of order.
--
-- __OPTION A__: Require that when a PresentId is set that the driver
-- ensure that images are always presented in the order of calls to
-- vkQueuePresent.
--
-- __OPTION B__: Finish both waits when the earliest present completes.
-- This will complete the later present wait earlier than the actual
-- presentation. This should be the easiest to implement as the driver need
-- only track the largest present ID completed. This is also the
-- \'natural\' consequence of interpreting the existing vkWaitForPresentKHR
-- specificationn.
--
-- __OPTION C__: Finish both waits when both have completed. This will
-- complete the earlier presentation later than the actual presentation
-- time. This is allowed by the current specification as there is no
-- precise timing requirement for when the presentId value is updated. This
-- requires slightly more complexity in the driver as it will need to track
-- all outstanding presentId values.
--
-- == Version History
--
-- -   Revision 1, 2019-02-19 (Keith Packard)
--
--     -   Initial version
--
-- = See Also
--
-- 'PhysicalDevicePresentWaitFeaturesKHR', 'waitForPresentKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_wait  ( waitForPresentKHR
                                              , waitForPresentKHRSafe
                                              , PhysicalDevicePresentWaitFeaturesKHR(..)
                                              , KHR_PRESENT_WAIT_SPEC_VERSION
                                              , pattern KHR_PRESENT_WAIT_SPEC_VERSION
                                              , KHR_PRESENT_WAIT_EXTENSION_NAME
                                              , pattern KHR_PRESENT_WAIT_EXTENSION_NAME
                                              , SwapchainKHR(..)
                                              ) where

import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero(..))
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
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Data.Kind (Type)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkWaitForPresentKHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitForPresentKHRUnsafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Word64 -> IO Result

foreign import ccall
  "dynamic" mkVkWaitForPresentKHRSafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Word64 -> IO Result

-- | waitForPresentKHR with selectable safeness
waitForPresentKHRSafeOrUnsafe :: forall io
                               . (MonadIO io)
                              => (FunPtr (Ptr Device_T -> SwapchainKHR -> Word64 -> Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word64 -> Word64 -> IO Result)
                              -> -- | @device@ is the device associated with @swapchain@.
                                 Device
                              -> -- | @swapchain@ is the non-retired swapchain on which an image was queued
                                 -- for presentation.
                                 SwapchainKHR
                              -> -- | @presentId@ is the presentation presentId to wait for.
                                 ("presentId" ::: Word64)
                              -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                                 -- adjusted to the closest value allowed by the implementation-dependent
                                 -- timeout accuracy, which /may/ be substantially longer than one
                                 -- nanosecond, and /may/ be longer than the requested period.
                                 ("timeout" ::: Word64)
                              -> io (Result)
waitForPresentKHRSafeOrUnsafe mkVkWaitForPresentKHR device swapchain presentId timeout = liftIO $ do
  let vkWaitForPresentKHRPtr = pVkWaitForPresentKHR (deviceCmds (device :: Device))
  unless (vkWaitForPresentKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWaitForPresentKHR is null" Nothing Nothing
  let vkWaitForPresentKHR' = mkVkWaitForPresentKHR vkWaitForPresentKHRPtr
  r <- traceAroundEvent "vkWaitForPresentKHR" (vkWaitForPresentKHR' (deviceHandle (device)) (swapchain) (presentId) (timeout))
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)

-- | vkWaitForPresentKHR - Wait for presentation
--
-- = Description
--
-- 'waitForPresentKHR' waits for the presentId associated with @swapchain@
-- to be increased in value so that it is at least equal to @presentId@.
--
-- For 'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_MAILBOX_KHR' (or
-- other present mode where images may be replaced in the presentation
-- queue) any wait of this type associated with such an image /must/ be
-- signaled no later than a wait associated with the replacing image would
-- be signaled.
--
-- When the presentation has completed, the presentId associated with the
-- related @pSwapChains@ entry will be increased in value so that it is at
-- least equal to the value provided in the
-- 'Vulkan.Extensions.VK_KHR_present_id.PresentIdKHR' structure.
--
-- There is no requirement for any precise timing relationship between the
-- presentation of the image to the user and the update of the presentId
-- value, but implementations /should/ make this as close as possible to
-- the presentation of the first pixel in the new image to the user.
--
-- The call to 'waitForPresentKHR' will block until either the presentId
-- associated with @swapchain@ is greater than or equal to @presentId@, or
-- @timeout@ nanoseconds passes. When the swapchain becomes OUT_OF_DATE,
-- the call will either return 'Vulkan.Core10.Enums.Result.SUCCESS' (if the
-- image was delivered to the presentation engine and may have been
-- presented to the user) or will return early with status
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' (if the image was not
-- presented to the user).
--
-- As an exception to the normal rules for objects which are externally
-- synchronized, the @swapchain@ passed to 'waitForPresentKHR' /may/ be
-- simultaneously used by other threads in calls to functions other than
-- 'Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR'. Access to the
-- swapchain data associated with this extension /must/ be atomic within
-- the implementation.
--
-- == Valid Usage
--
-- -   #VUID-vkWaitForPresentKHR-swapchain-04997# @swapchain@ /must/ not be
--     in the retired state
--
-- -   #VUID-vkWaitForPresentKHR-presentWait-06234# The
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-presentWait presentWait>
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWaitForPresentKHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWaitForPresentKHR-swapchain-parameter# @swapchain@ /must/ be
--     a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkWaitForPresentKHR-commonparent# Both of @device@, and
--     @swapchain@ /must/ have been created, allocated, or retrieved from
--     the same 'Vulkan.Core10.Handles.Instance'
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
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
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
waitForPresentKHR :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the device associated with @swapchain@.
                     Device
                  -> -- | @swapchain@ is the non-retired swapchain on which an image was queued
                     -- for presentation.
                     SwapchainKHR
                  -> -- | @presentId@ is the presentation presentId to wait for.
                     ("presentId" ::: Word64)
                  -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                     -- adjusted to the closest value allowed by the implementation-dependent
                     -- timeout accuracy, which /may/ be substantially longer than one
                     -- nanosecond, and /may/ be longer than the requested period.
                     ("timeout" ::: Word64)
                  -> io (Result)
waitForPresentKHR = waitForPresentKHRSafeOrUnsafe mkVkWaitForPresentKHRUnsafe

-- | A variant of 'waitForPresentKHR' which makes a *safe* FFI call
waitForPresentKHRSafe :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the device associated with @swapchain@.
                         Device
                      -> -- | @swapchain@ is the non-retired swapchain on which an image was queued
                         -- for presentation.
                         SwapchainKHR
                      -> -- | @presentId@ is the presentation presentId to wait for.
                         ("presentId" ::: Word64)
                      -> -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
                         -- adjusted to the closest value allowed by the implementation-dependent
                         -- timeout accuracy, which /may/ be substantially longer than one
                         -- nanosecond, and /may/ be longer than the requested period.
                         ("timeout" ::: Word64)
                      -> io (Result)
waitForPresentKHRSafe = waitForPresentKHRSafeOrUnsafe mkVkWaitForPresentKHRSafe


-- | VkPhysicalDevicePresentWaitFeaturesKHR - Structure indicating support
-- for present wait
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePresentWaitFeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDevicePresentWaitFeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentWaitFeaturesKHR = PhysicalDevicePresentWaitFeaturesKHR
  { -- | #features-presentWait# @presentWait@ indicates that the implementation
    -- supports 'waitForPresentKHR'.
    presentWait :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentWaitFeaturesKHR)
#endif
deriving instance Show PhysicalDevicePresentWaitFeaturesKHR

instance ToCStruct PhysicalDevicePresentWaitFeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentWaitFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentWait))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentWaitFeaturesKHR where
  peekCStruct p = do
    presentWait <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentWaitFeaturesKHR
             (bool32ToBool presentWait)

instance Storable PhysicalDevicePresentWaitFeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentWaitFeaturesKHR where
  zero = PhysicalDevicePresentWaitFeaturesKHR
           zero


type KHR_PRESENT_WAIT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PRESENT_WAIT_SPEC_VERSION"
pattern KHR_PRESENT_WAIT_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PRESENT_WAIT_SPEC_VERSION = 1


type KHR_PRESENT_WAIT_EXTENSION_NAME = "VK_KHR_present_wait"

-- No documentation found for TopLevel "VK_KHR_PRESENT_WAIT_EXTENSION_NAME"
pattern KHR_PRESENT_WAIT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PRESENT_WAIT_EXTENSION_NAME = "VK_KHR_present_wait"

