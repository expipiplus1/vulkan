{-# language CPP #-}
-- | = Name
--
-- VK_KHR_present_wait2 - device extension
--
-- = VK_KHR_present_wait2
--
-- [__Name String__]
--     @VK_KHR_present_wait2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     481
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_surface VK_KHR_surface>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id2 VK_KHR_present_id2>
--
-- [__Contact__]
--
--     -   Daniel Stone
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_present_wait2.adoc VK_KHR_present_wait2>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-05-30
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   James Jones, NVIDIA
--
--     -   Daniel Stone, Collabora
--
--     -   Derek Foreman, Collabora
--
--     -   /contributors to
--         \`<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait VK_KHR_present_wait>\`/
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
-- -   'waitForPresent2KHR'
--
-- == New Structures
--
-- -   'PresentWait2InfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentWait2FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'SurfaceCapabilitiesPresentWait2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_PRESENT_WAIT_2_EXTENSION_NAME'
--
-- -   'KHR_PRESENT_WAIT_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_WAIT_2_BIT_KHR'
--
-- == Issues
--
-- 1) When does the wait finish?
--
-- __RESOLVED__. The wait request will complete when the timeout expires,
-- or after the corresponding presentation request has either taken effect
-- within the presentation engine or has been replaced without
-- presentation. Additionally, a wait may complete immediately if the
-- swapchain becomes out of date.
--
-- In circumstances outside the application’s control, this wait may be
-- particularly long. For example, a user session may have the display
-- locked and switched off for multiple days. During this time, the latest
-- image presented through the WSI will never be presented to the user
-- (because nothing is being presented), or replaced (because nothing newer
-- has been queued by the application). Each operating system may have a
-- separate mechanism to inform the application of states such as these,
-- however it is out of scope of the Vulkan WSI.
--
-- There is no requirement for any precise timing relationship between the
-- presentation of the image to the user and the end of the wait.
--
-- This extension is not intended for time-to-light estimation, which is
-- better solved by a separate extension dedicated to present-timing
-- feedback for audio\/visual\/input synchronization.
--
-- 2) Should this use fences or other existing synchronization mechanism?
--
-- __RESOLVED__. VkFence is a legacy primitive. Building a new API around a
-- legacy primitive is undesirable.
--
-- Other existing synchronization mechanisms may lack a platform-provided
-- framework for sharing synchronization objects between display and render
-- drivers.
--
-- For these reasons, this extension will provide a separate
-- synchronization API.
--
-- 3) Should this extension share present identification with other
-- extensions?
--
-- __RESOLVED__. Yes. A new extension, @VK_KHR_present_id2@, should be
-- created to provide a shared structure for presentation identifiers.
--
-- 4) What happens when presentations complete out of order with respect to
-- calls to vkQueuePresent? This could happen if the semaphores for the
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
-- specification.
--
-- __OPTION C__: Finish both waits when both have completed. This will
-- complete the earlier presentation later than the actual presentation
-- time. This is allowed by the current specification as there is no
-- precise timing requirement for when the presentId value is updated. This
-- requires slightly more complexity in the driver as it will need to track
-- all outstanding presentId values.
--
-- __OPTION D__: The order of completion between outstanding
-- 'waitForPresent2KHR' calls is always undefined. However, a
-- 'Vulkan.Core10.Enums.Result.SUCCESS' return value in
-- 'PresentWait2InfoKHR'::@presentId@ implies that future calls to
-- 'waitForPresent2KHR' where 'PresentWait2InfoKHR'::@presentId@ is less
-- than or equal to N will complete immediately.
--
-- __RESOLVED__. __OPTION D__: This option ensures implementations do not
-- need to create complex internal queues to generate signals in the right
-- order.
--
-- 5) Should this extension deviate from @VK_KHR_present_wait@ and require
-- the presentation engine to provide the presentId values?
--
-- __RESOLVED__. No. This extension is intended to be a bugfix of
-- @VK_KHR_present_wait@, and existing low-latency apis require an
-- application provided id. At least on some platforms, a mapping mechanism
-- would be required to translate between presentation engine and
-- application ids. This exceeds the intended scope of this extension.
--
-- When needed in the future, we can introduce an independent presentation
-- engine driven id and a mechanism for mapping presentation engine ids to
-- application provided ids.
--
-- == Version History
--
-- -   Revision 1, 2022-10-05 (Daniel Stone)
--
--     -   Repurposed from VK_KHR_present_wait to be based on surface
--         capabilities
--
--     -   Reworded wait finish section to avoid time-to-light
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_present_wait2 Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_present_wait2  ( waitForPresent2KHR
                                               , waitForPresent2KHRSafe
                                               , PresentWait2InfoKHR(..)
                                               , PhysicalDevicePresentWait2FeaturesKHR(..)
                                               , SurfaceCapabilitiesPresentWait2KHR(..)
                                               , KHR_PRESENT_WAIT_2_SPEC_VERSION
                                               , pattern KHR_PRESENT_WAIT_2_SPEC_VERSION
                                               , KHR_PRESENT_WAIT_2_EXTENSION_NAME
                                               , pattern KHR_PRESENT_WAIT_2_EXTENSION_NAME
                                               , SwapchainKHR(..)
                                               , SwapchainCreateFlagBitsKHR(..)
                                               , SwapchainCreateFlagsKHR
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
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
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkWaitForPresent2KHR))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitForPresent2KHRUnsafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr PresentWait2InfoKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr PresentWait2InfoKHR -> IO Result

foreign import ccall
  "dynamic" mkVkWaitForPresent2KHRSafe
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr PresentWait2InfoKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr PresentWait2InfoKHR -> IO Result

-- | waitForPresent2KHR with selectable safeness
waitForPresent2KHRSafeOrUnsafe :: forall io
                                . (MonadIO io)
                               => (FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr PresentWait2InfoKHR -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr PresentWait2InfoKHR -> IO Result)
                               -> -- | @device@ is the device associated with @swapchain@.
                                  Device
                               -> -- | @swapchain@ is the non-retired swapchain on which an image was queued
                                  -- for presentation.
                                  SwapchainKHR
                               -> -- | @pPresentWait2Info@ is a pointer to a 'PresentWait2InfoKHR' structure
                                  -- specifying the parameters of the wait.
                                  PresentWait2InfoKHR
                               -> io (Result)
waitForPresent2KHRSafeOrUnsafe mkVkWaitForPresent2KHR device
                                                        swapchain
                                                        presentWait2Info = liftIO . evalContT $ do
  let vkWaitForPresent2KHRPtr = pVkWaitForPresent2KHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkWaitForPresent2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkWaitForPresent2KHR is null" Nothing Nothing
  let vkWaitForPresent2KHR' = mkVkWaitForPresent2KHR vkWaitForPresent2KHRPtr
  pPresentWait2Info <- ContT $ withCStruct (presentWait2Info)
  r <- lift $ traceAroundEvent "vkWaitForPresent2KHR" (vkWaitForPresent2KHR'
                                                         (deviceHandle (device))
                                                         (swapchain)
                                                         pPresentWait2Info)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)

-- | vkWaitForPresent2KHR - Wait for presentation
--
-- = Description
--
-- 'waitForPresent2KHR' waits for the presentation engine to have begun
-- presentation of the presentation request associated with the
-- 'PresentWait2InfoKHR'::@presentId@ on @swapchain@, or for
-- 'PresentWait2InfoKHR'::@timeout@ to have expired.
--
-- The wait request will complete when the timeout expires, or after the
-- corresponding presentation request has either taken effect within the
-- presentation engine or has been replaced without presentation.
--
-- The timing relationship between the presentation of the image to the
-- user and the wait request completing is implementation-dependent due to
-- variations in window system implementations.
--
-- If the @swapchain@ becomes
-- 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' either before or
-- during this call, the call /may/ either return
-- 'Vulkan.Core10.Enums.Result.SUCCESS' (if the image was delivered to the
-- presentation engine and /may/ have been presented to the user) or return
-- early with status 'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR' (if
-- the image could not be presented to the user).
--
-- As an exception to the normal rules for objects which are externally
-- synchronized, the @swapchain@ passed to 'waitForPresent2KHR' /may/ be
-- simultaneously used by other threads in calls to functions other than
-- 'Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR'. Access to the
-- swapchain data associated with this extension /must/ be atomic within
-- the implementation.
--
-- == Valid Usage
--
-- -   #VUID-vkWaitForPresent2KHR-presentWait2-10814# The
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-presentWait2 presentWait2>
--     feature /must/ be enabled
--
-- -   #VUID-vkWaitForPresent2KHR-None-10815# The
--     'SurfaceCapabilitiesPresentWait2KHR' surface capability /must/ be
--     present for the underlying surface
--
-- -   #VUID-vkWaitForPresent2KHR-None-10816# The swapchain must have been
--     created with
--     'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_WAIT_2_BIT_KHR'
--     bit set in the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR'
--     field
--
-- -   #VUID-vkWaitForPresent2KHR-presentId-10817# The @presentId@ value
--     /must/ have been associated with a
--     'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' request on the
--     @swapchain@ which returned a non-error value
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkWaitForPresent2KHR-device-parameter# @device@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkWaitForPresent2KHR-swapchain-parameter# @swapchain@ /must/
--     be a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkWaitForPresent2KHR-pPresentWait2Info-parameter#
--     @pPresentWait2Info@ /must/ be a valid pointer to a valid
--     'PresentWait2InfoKHR' structure
--
-- -   #VUID-vkWaitForPresent2KHR-swapchain-parent# @swapchain@ /must/ have
--     been created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUBOPTIMAL_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.TIMEOUT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_FULL_SCREEN_EXCLUSIVE_MODE_LOST_EXT'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait2 VK_KHR_present_wait2>,
-- 'Vulkan.Core10.Handles.Device', 'PresentWait2InfoKHR',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
waitForPresent2KHR :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the device associated with @swapchain@.
                      Device
                   -> -- | @swapchain@ is the non-retired swapchain on which an image was queued
                      -- for presentation.
                      SwapchainKHR
                   -> -- | @pPresentWait2Info@ is a pointer to a 'PresentWait2InfoKHR' structure
                      -- specifying the parameters of the wait.
                      PresentWait2InfoKHR
                   -> io (Result)
waitForPresent2KHR = waitForPresent2KHRSafeOrUnsafe mkVkWaitForPresent2KHRUnsafe

-- | A variant of 'waitForPresent2KHR' which makes a *safe* FFI call
waitForPresent2KHRSafe :: forall io
                        . (MonadIO io)
                       => -- | @device@ is the device associated with @swapchain@.
                          Device
                       -> -- | @swapchain@ is the non-retired swapchain on which an image was queued
                          -- for presentation.
                          SwapchainKHR
                       -> -- | @pPresentWait2Info@ is a pointer to a 'PresentWait2InfoKHR' structure
                          -- specifying the parameters of the wait.
                          PresentWait2InfoKHR
                       -> io (Result)
waitForPresent2KHRSafe = waitForPresent2KHRSafeOrUnsafe mkVkWaitForPresent2KHRSafe


-- | VkPresentWait2InfoKHR - Structure describing parameters of a
-- presentation wait
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait2 VK_KHR_present_wait2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'waitForPresent2KHR'
data PresentWait2InfoKHR = PresentWait2InfoKHR
  { -- | @presentId@ is the presentation presentId to wait for.
    presentId :: Word64
  , -- | @timeout@ is the timeout period in units of nanoseconds. @timeout@ is
    -- adjusted to the closest value allowed by the implementation-dependent
    -- timeout accuracy, which /may/ be substantially longer than one
    -- nanosecond, and /may/ be longer than the requested period.
    timeout :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentWait2InfoKHR)
#endif
deriving instance Show PresentWait2InfoKHR

instance ToCStruct PresentWait2InfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentWait2InfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (presentId)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (timeout)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_WAIT_2_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct PresentWait2InfoKHR where
  peekCStruct p = do
    presentId <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    timeout <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ PresentWait2InfoKHR
             presentId timeout

instance Storable PresentWait2InfoKHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PresentWait2InfoKHR where
  zero = PresentWait2InfoKHR
           zero
           zero


-- | VkPhysicalDevicePresentWait2FeaturesKHR - Structure indicating support
-- for present wait 2
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePresentWait2FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePresentWait2FeaturesKHR', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait2 VK_KHR_present_wait2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentWait2FeaturesKHR = PhysicalDevicePresentWait2FeaturesKHR
  { -- | #features-presentWait2# @presentWait2@ indicates that the implementation
    -- supports 'waitForPresent2KHR'.
    presentWait2 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentWait2FeaturesKHR)
#endif
deriving instance Show PhysicalDevicePresentWait2FeaturesKHR

instance ToCStruct PhysicalDevicePresentWait2FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentWait2FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentWait2))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_WAIT_2_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentWait2FeaturesKHR where
  peekCStruct p = do
    presentWait2 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDevicePresentWait2FeaturesKHR
             (bool32ToBool presentWait2)

instance Storable PhysicalDevicePresentWait2FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentWait2FeaturesKHR where
  zero = PhysicalDevicePresentWait2FeaturesKHR
           zero


-- | VkSurfaceCapabilitiesPresentWait2KHR - Structure describing
-- presentation-wait capabilities of a surface
--
-- = Description
--
-- This structure /can/ be included in the @pNext@ chain of
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR'
-- to determine support for present-wait. If @presentWait2Supported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE', it indicates that waiting for
-- presentation is not possible for this surface.
--
-- Applications /must/ not attempt to call 'waitForPresent2KHR' on a
-- swapchain if @presentWait2Supported@ is
-- 'Vulkan.Core10.FundamentalTypes.FALSE'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_wait2 VK_KHR_present_wait2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SurfaceCapabilitiesPresentWait2KHR = SurfaceCapabilitiesPresentWait2KHR
  { -- | @presentWait2Supported@ is a boolean describing whether the surface is
    -- able to support the present-wait extension
    presentWait2Supported :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SurfaceCapabilitiesPresentWait2KHR)
#endif
deriving instance Show SurfaceCapabilitiesPresentWait2KHR

instance ToCStruct SurfaceCapabilitiesPresentWait2KHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SurfaceCapabilitiesPresentWait2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentWait2Supported))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_PRESENT_WAIT_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct SurfaceCapabilitiesPresentWait2KHR where
  peekCStruct p = do
    presentWait2Supported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SurfaceCapabilitiesPresentWait2KHR
             (bool32ToBool presentWait2Supported)

instance Storable SurfaceCapabilitiesPresentWait2KHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SurfaceCapabilitiesPresentWait2KHR where
  zero = SurfaceCapabilitiesPresentWait2KHR
           zero


type KHR_PRESENT_WAIT_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_PRESENT_WAIT_2_SPEC_VERSION"
pattern KHR_PRESENT_WAIT_2_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_PRESENT_WAIT_2_SPEC_VERSION = 1


type KHR_PRESENT_WAIT_2_EXTENSION_NAME = "VK_KHR_present_wait2"

-- No documentation found for TopLevel "VK_KHR_PRESENT_WAIT_2_EXTENSION_NAME"
pattern KHR_PRESENT_WAIT_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_PRESENT_WAIT_2_EXTENSION_NAME = "VK_KHR_present_wait2"

