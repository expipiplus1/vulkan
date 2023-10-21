{-# language CPP #-}
-- | = Name
--
-- VK_NV_low_latency2 - device extension
--
-- == VK_NV_low_latency2
--
-- [__Name String__]
--     @VK_NV_low_latency2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     506
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__; __Contact__]
--
--     -   Charles Hansen
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_low_latency2] @cshansen%0A*Here describe the issue or question you have about the VK_NV_low_latency2 extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-09-25
--
-- [__Contributors__]
--
--     -   Charles Hansen, NVIDIA
--
--     -   Liam Middlebrook, NVIDIA
--
--     -   Lionel Duc, NVIDIA
--
--     -   James Jones, NVIDIA
--
--     -   Eric Sullivan, NVIDIA
--
-- == New Commands
--
-- -   'getLatencyTimingsNV'
--
-- -   'latencySleepNV'
--
-- -   'queueNotifyOutOfBandNV'
--
-- -   'setLatencyMarkerNV'
--
-- -   'setLatencySleepModeNV'
--
-- == New Structures
--
-- -   'GetLatencyMarkerInfoNV'
--
-- -   'LatencySleepInfoNV'
--
-- -   'LatencySleepModeInfoNV'
--
-- -   'LatencyTimingsFrameReportNV'
--
-- -   'OutOfBandQueueTypeInfoNV'
--
-- -   'SetLatencyMarkerInfoNV'
--
-- -   Extending 'Vulkan.Core10.Queue.SubmitInfo',
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.SubmitInfo2':
--
--     -   'LatencySubmissionPresentIdNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'LatencySurfaceCapabilitiesNV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR':
--
--     -   'SwapchainLatencyCreateInfoNV'
--
-- == New Enums
--
-- -   'LatencyMarkerNV'
--
-- -   'OutOfBandQueueTypeNV'
--
-- == New Enum Constants
--
-- -   'NV_LOW_LATENCY_2_EXTENSION_NAME'
--
-- -   'NV_LOW_LATENCY_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GET_LATENCY_MARKER_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LATENCY_SLEEP_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LATENCY_SLEEP_MODE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LATENCY_TIMINGS_FRAME_REPORT_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_OUT_OF_BAND_QUEUE_TYPE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SET_LATENCY_MARKER_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV'
--
-- == Description
--
-- This extension gives applications timing suggestions on when to start
-- the recording of new frames to reduce the latency between input sampling
-- and frame presentation. Applications can accomplish this through the
-- extension by calling 'setLatencySleepModeNV' to allow the driver to pace
-- a given swapchain, then calling 'latencySleepNV' before input sampling
-- to delay the start of the CPU side work. Additional methods and
-- structures are provided to give insight into the latency pipeline of an
-- application through the latency markers. @VK_NV_low_latency@ provides
-- legacy support for applications that make use of the NVIDIA Reflex SDK
-- whereas new implementations should use the @VK_NV_low_latency2@
-- extension.
--
-- == Issues
--
-- 1) How does Low Latency 2 work with applications that utilize device
-- groups?
--
-- Low Latency 2 does not support device groups.
--
-- == Version History
--
-- -   Revision 1, 2023-09-25 (Charles Hansen)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'GetLatencyMarkerInfoNV', 'LatencyMarkerNV', 'LatencySleepInfoNV',
-- 'LatencySleepModeInfoNV', 'LatencySubmissionPresentIdNV',
-- 'LatencySurfaceCapabilitiesNV', 'LatencyTimingsFrameReportNV',
-- 'OutOfBandQueueTypeInfoNV', 'OutOfBandQueueTypeNV',
-- 'SetLatencyMarkerInfoNV', 'SwapchainLatencyCreateInfoNV',
-- 'getLatencyTimingsNV', 'latencySleepNV', 'queueNotifyOutOfBandNV',
-- 'setLatencyMarkerNV', 'setLatencySleepModeNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_low_latency2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_low_latency2  ( setLatencySleepModeNV
                                             , latencySleepNV
                                             , setLatencyMarkerNV
                                             , getLatencyTimingsNV
                                             , queueNotifyOutOfBandNV
                                             , LatencySleepModeInfoNV(..)
                                             , LatencySleepInfoNV(..)
                                             , SetLatencyMarkerInfoNV(..)
                                             , GetLatencyMarkerInfoNV(..)
                                             , LatencyTimingsFrameReportNV(..)
                                             , OutOfBandQueueTypeInfoNV(..)
                                             , LatencySubmissionPresentIdNV(..)
                                             , SwapchainLatencyCreateInfoNV(..)
                                             , LatencySurfaceCapabilitiesNV(..)
                                             , LatencyMarkerNV( LATENCY_MARKER_SIMULATION_START_NV
                                                              , LATENCY_MARKER_SIMULATION_END_NV
                                                              , LATENCY_MARKER_RENDERSUBMIT_START_NV
                                                              , LATENCY_MARKER_RENDERSUBMIT_END_NV
                                                              , LATENCY_MARKER_PRESENT_START_NV
                                                              , LATENCY_MARKER_PRESENT_END_NV
                                                              , LATENCY_MARKER_INPUT_SAMPLE_NV
                                                              , LATENCY_MARKER_TRIGGER_FLASH_NV
                                                              , LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_START_NV
                                                              , LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_END_NV
                                                              , LATENCY_MARKER_OUT_OF_BAND_PRESENT_START_NV
                                                              , LATENCY_MARKER_OUT_OF_BAND_PRESENT_END_NV
                                                              , ..
                                                              )
                                             , OutOfBandQueueTypeNV( OUT_OF_BAND_QUEUE_TYPE_RENDER_NV
                                                                   , OUT_OF_BAND_QUEUE_TYPE_PRESENT_NV
                                                                   , ..
                                                                   )
                                             , NV_LOW_LATENCY_2_SPEC_VERSION
                                             , pattern NV_LOW_LATENCY_2_SPEC_VERSION
                                             , NV_LOW_LATENCY_2_EXTENSION_NAME
                                             , pattern NV_LOW_LATENCY_2_EXTENSION_NAME
                                             , SwapchainKHR(..)
                                             , PresentModeKHR(..)
                                             ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Foreign.Marshal.Alloc (allocaBytes)
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
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
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
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetLatencyTimingsNV))
import Vulkan.Dynamic (DeviceCmds(pVkLatencySleepNV))
import Vulkan.Dynamic (DeviceCmds(pVkQueueNotifyOutOfBandNV))
import Vulkan.Dynamic (DeviceCmds(pVkSetLatencyMarkerNV))
import Vulkan.Dynamic (DeviceCmds(pVkSetLatencySleepModeNV))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR)
import Vulkan.Core10.Handles (Queue)
import Vulkan.Core10.Handles (Queue(..))
import Vulkan.Core10.Handles (Queue(Queue))
import Vulkan.Core10.Handles (Queue_T)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Handles (Semaphore)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GET_LATENCY_MARKER_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_LATENCY_SLEEP_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_LATENCY_SLEEP_MODE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_LATENCY_TIMINGS_FRAME_REPORT_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_OUT_OF_BAND_QUEUE_TYPE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SET_LATENCY_MARKER_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_surface (PresentModeKHR(..))
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetLatencySleepModeNV
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr LatencySleepModeInfoNV -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr LatencySleepModeInfoNV -> IO Result

-- | vkSetLatencySleepModeNV - Enable or Disable low latency mode on a
-- swapchain
--
-- = Description
--
-- If @pSleepModeInfo@ is @NULL@, 'setLatencySleepModeNV' will disable low
-- latency mode, low latency boost, and set the minimum present interval
-- previously specified by 'LatencySleepModeInfoNV' to zero on @swapchain@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.Handles.Device', 'LatencySleepModeInfoNV',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
setLatencySleepModeNV :: forall io
                       . (MonadIO io)
                      => -- | @device@ is the device associated with @swapchain@.
                         --
                         -- #VUID-vkSetLatencySleepModeNV-device-parameter# @device@ /must/ be a
                         -- valid 'Vulkan.Core10.Handles.Device' handle
                         Device
                      -> -- | @swapchain@ is the swapchain to enable or disable low latency mode on.
                         --
                         -- #VUID-vkSetLatencySleepModeNV-swapchain-parameter# @swapchain@ /must/ be
                         -- a valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
                         --
                         -- #VUID-vkSetLatencySleepModeNV-swapchain-parent# @swapchain@ /must/ have
                         -- been created, allocated, or retrieved from @device@
                         SwapchainKHR
                      -> -- | @pSleepModeInfo@ is @NULL@ or a pointer to a 'LatencySleepModeInfoNV'
                         -- structure specifying the parameters of the latency sleep mode.
                         --
                         -- #VUID-vkSetLatencySleepModeNV-pSleepModeInfo-parameter# @pSleepModeInfo@
                         -- /must/ be a valid pointer to a valid 'LatencySleepModeInfoNV' structure
                         LatencySleepModeInfoNV
                      -> io ()
setLatencySleepModeNV device swapchain sleepModeInfo = liftIO . evalContT $ do
  let vkSetLatencySleepModeNVPtr = pVkSetLatencySleepModeNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkSetLatencySleepModeNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetLatencySleepModeNV is null" Nothing Nothing
  let vkSetLatencySleepModeNV' = mkVkSetLatencySleepModeNV vkSetLatencySleepModeNVPtr
  pSleepModeInfo <- ContT $ withCStruct (sleepModeInfo)
  r <- lift $ traceAroundEvent "vkSetLatencySleepModeNV" (vkSetLatencySleepModeNV'
                                                            (deviceHandle (device))
                                                            (swapchain)
                                                            pSleepModeInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkLatencySleepNV
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr LatencySleepInfoNV -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr LatencySleepInfoNV -> IO Result

-- | vkLatencySleepNV - Trigger low latency mode Sleep
--
-- = Description
--
-- 'latencySleepNV' returns immediately. Applications /should/ use
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.waitSemaphores'
-- with
-- @pSleepInfo@::'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore'
-- to delay host CPU work. CPU work refers to application work done before
-- presenting which includes but is not limited to: input sampling,
-- simulation, command buffer recording, command buffer submission, and
-- present submission. It is recommended to call this function before input
-- sampling. When using this function, it /should/ be called exactly once
-- between presents.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.Handles.Device', 'LatencySleepInfoNV',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
latencySleepNV :: forall io
                . (MonadIO io)
               => -- | @device@ is the device associated with @swapchain@.
                  --
                  -- #VUID-vkLatencySleepNV-device-parameter# @device@ /must/ be a valid
                  -- 'Vulkan.Core10.Handles.Device' handle
                  Device
               -> -- | @swapchain@ is the swapchain to delay associated CPU work based on
                  -- 'LatencySubmissionPresentIdNV' submissions.
                  --
                  -- #VUID-vkLatencySleepNV-swapchain-parameter# @swapchain@ /must/ be a
                  -- valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
                  --
                  -- #VUID-vkLatencySleepNV-swapchain-parent# @swapchain@ /must/ have been
                  -- created, allocated, or retrieved from @device@
                  SwapchainKHR
               -> -- | @pSleepInfo@ is a pointer to a 'LatencySleepInfoNV' structure specifying
                  -- the parameters of the latency sleep.
                  --
                  -- #VUID-vkLatencySleepNV-pSleepInfo-parameter# @pSleepInfo@ /must/ be a
                  -- valid pointer to a valid 'LatencySleepInfoNV' structure
                  LatencySleepInfoNV
               -> io ()
latencySleepNV device swapchain sleepInfo = liftIO . evalContT $ do
  let vkLatencySleepNVPtr = pVkLatencySleepNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkLatencySleepNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkLatencySleepNV is null" Nothing Nothing
  let vkLatencySleepNV' = mkVkLatencySleepNV vkLatencySleepNVPtr
  pSleepInfo <- ContT $ withCStruct (sleepInfo)
  r <- lift $ traceAroundEvent "vkLatencySleepNV" (vkLatencySleepNV'
                                                     (deviceHandle (device))
                                                     (swapchain)
                                                     pSleepInfo)
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetLatencyMarkerNV
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr SetLatencyMarkerInfoNV -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Ptr SetLatencyMarkerInfoNV -> IO ()

-- | vkSetLatencyMarkerNV - Pass in marker for timing info
--
-- = Description
--
-- At the beginning and end of simulation and render threads and beginning
-- and end of 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' calls,
-- 'setLatencyMarkerNV' /can/ be called to provide timestamps for the
-- application’s reference. These timestamps are returned with a call to
-- 'getLatencyTimingsNV' alongside driver provided timestamps at various
-- points of interest with regards to latency within the application.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.Handles.Device', 'SetLatencyMarkerInfoNV',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
setLatencyMarkerNV :: forall io
                    . (MonadIO io)
                   => -- | @device@ is the device associated with @swapchain@.
                      --
                      -- #VUID-vkSetLatencyMarkerNV-device-parameter# @device@ /must/ be a valid
                      -- 'Vulkan.Core10.Handles.Device' handle
                      Device
                   -> -- | @swapchain@ is the swapchain to capture timestamps on.
                      --
                      -- #VUID-vkSetLatencyMarkerNV-swapchain-parameter# @swapchain@ /must/ be a
                      -- valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
                      --
                      -- #VUID-vkSetLatencyMarkerNV-swapchain-parent# @swapchain@ /must/ have
                      -- been created, allocated, or retrieved from @device@
                      SwapchainKHR
                   -> -- | #VUID-vkSetLatencyMarkerNV-pLatencyMarkerInfo-parameter#
                      -- @pLatencyMarkerInfo@ /must/ be a valid pointer to a valid
                      -- 'SetLatencyMarkerInfoNV' structure
                      SetLatencyMarkerInfoNV
                   -> io ()
setLatencyMarkerNV device swapchain latencyMarkerInfo = liftIO . evalContT $ do
  let vkSetLatencyMarkerNVPtr = pVkSetLatencyMarkerNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkSetLatencyMarkerNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetLatencyMarkerNV is null" Nothing Nothing
  let vkSetLatencyMarkerNV' = mkVkSetLatencyMarkerNV vkSetLatencyMarkerNVPtr
  pLatencyMarkerInfo <- ContT $ withCStruct (latencyMarkerInfo)
  lift $ traceAroundEvent "vkSetLatencyMarkerNV" (vkSetLatencyMarkerNV'
                                                    (deviceHandle (device))
                                                    (swapchain)
                                                    pLatencyMarkerInfo)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetLatencyTimingsNV
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr GetLatencyMarkerInfoNV -> IO ()) -> Ptr Device_T -> SwapchainKHR -> Ptr Word32 -> Ptr GetLatencyMarkerInfoNV -> IO ()

-- | vkGetLatencyTimingsNV - Get latency marker results
--
-- = Description
--
-- The timings returned by 'getLatencyTimingsNV' contain the timestamps
-- requested from 'setLatencyMarkerNV' and additional
-- implementation-specific markers defined in
-- 'LatencyTimingsFrameReportNV'. If @pTimings@ is @NULL@, then the maximum
-- number of queryable frame data is returned in @pTimingCount@. Otherwise,
-- @pTimingCount@ /must/ point to a variable set by the user to the number
-- of elements in the @pTimings@ array in @pGetLatencyMarkerInfo@, and on
-- return the variable is overwritten with the number of values actually
-- written to @pTimings@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.Handles.Device', 'GetLatencyMarkerInfoNV',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
getLatencyTimingsNV :: forall io
                     . (MonadIO io)
                    => -- | @device@ is the device associated with @swapchain@.
                       --
                       -- #VUID-vkGetLatencyTimingsNV-device-parameter# @device@ /must/ be a valid
                       -- 'Vulkan.Core10.Handles.Device' handle
                       Device
                    -> -- | @swapchain@ is the swapchain to return data from.
                       --
                       -- #VUID-vkGetLatencyTimingsNV-swapchain-parameter# @swapchain@ /must/ be a
                       -- valid 'Vulkan.Extensions.Handles.SwapchainKHR' handle
                       --
                       -- #VUID-vkGetLatencyTimingsNV-swapchain-parent# @swapchain@ /must/ have
                       -- been created, allocated, or retrieved from @device@
                       SwapchainKHR
                    -> io (("timingCount" ::: Word32), GetLatencyMarkerInfoNV)
getLatencyTimingsNV device swapchain = liftIO . evalContT $ do
  let vkGetLatencyTimingsNVPtr = pVkGetLatencyTimingsNV (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetLatencyTimingsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetLatencyTimingsNV is null" Nothing Nothing
  let vkGetLatencyTimingsNV' = mkVkGetLatencyTimingsNV vkGetLatencyTimingsNVPtr
  pPTimingCount <- ContT $ bracket (callocBytes @Word32 4) free
  pPLatencyMarkerInfo <- ContT (withZeroCStruct @GetLatencyMarkerInfoNV)
  lift $ traceAroundEvent "vkGetLatencyTimingsNV" (vkGetLatencyTimingsNV'
                                                     (deviceHandle (device))
                                                     (swapchain)
                                                     (pPTimingCount)
                                                     (pPLatencyMarkerInfo))
  pTimingCount <- lift $ peek @Word32 pPTimingCount
  pLatencyMarkerInfo <- lift $ peekCStruct @GetLatencyMarkerInfoNV pPLatencyMarkerInfo
  pure $ (pTimingCount, pLatencyMarkerInfo)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueNotifyOutOfBandNV
  :: FunPtr (Ptr Queue_T -> Ptr OutOfBandQueueTypeInfoNV -> IO ()) -> Ptr Queue_T -> Ptr OutOfBandQueueTypeInfoNV -> IO ()

-- | vkQueueNotifyOutOfBandNV - Notify out of band queue
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | -                                                                                                                          | -                                                                                                                      | -                                                                                                                           | Any                                                                                                                   | -                                                                                                                                      |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'OutOfBandQueueTypeInfoNV', 'Vulkan.Core10.Handles.Queue'
queueNotifyOutOfBandNV :: forall io
                        . (MonadIO io)
                       => -- | @queue@ is the VkQueue to be marked as out of band.
                          --
                          -- #VUID-vkQueueNotifyOutOfBandNV-queue-parameter# @queue@ /must/ be a
                          -- valid 'Vulkan.Core10.Handles.Queue' handle
                          Queue
                       -> -- | @pQueueTypeInfo@ is a pointer to a 'OutOfBandQueueTypeInfoNV' structure
                          -- specifying the queue type.
                          --
                          -- #VUID-vkQueueNotifyOutOfBandNV-pQueueTypeInfo-parameter#
                          -- @pQueueTypeInfo@ /must/ be a valid pointer to a valid
                          -- 'OutOfBandQueueTypeInfoNV' structure
                          OutOfBandQueueTypeInfoNV
                       -> io ()
queueNotifyOutOfBandNV queue queueTypeInfo = liftIO . evalContT $ do
  let vkQueueNotifyOutOfBandNVPtr = pVkQueueNotifyOutOfBandNV (case queue of Queue{deviceCmds} -> deviceCmds)
  lift $ unless (vkQueueNotifyOutOfBandNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkQueueNotifyOutOfBandNV is null" Nothing Nothing
  let vkQueueNotifyOutOfBandNV' = mkVkQueueNotifyOutOfBandNV vkQueueNotifyOutOfBandNVPtr
  pQueueTypeInfo <- ContT $ withCStruct (queueTypeInfo)
  lift $ traceAroundEvent "vkQueueNotifyOutOfBandNV" (vkQueueNotifyOutOfBandNV'
                                                        (queueHandle (queue))
                                                        pQueueTypeInfo)
  pure $ ()


-- | VkLatencySleepModeInfoNV - Structure to set low latency mode
--
-- = Description
--
-- If @lowLatencyMode@ is set to 'Vulkan.Core10.FundamentalTypes.FALSE',
-- @lowLatencyBoost@ will still hint to the GPU to increase its power state
-- and 'latencySleepNV' will still enforce @minimumIntervalUs@ between
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' calls.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setLatencySleepModeNV'
data LatencySleepModeInfoNV = LatencySleepModeInfoNV
  { -- | @lowLatencyMode@ is the toggle to enable or disable low latency mode.
    lowLatencyMode :: Bool
  , -- | @lowLatencyBoost@ allows an application to hint to the GPU to increase
    -- performance to provide additional latency savings at a cost of increased
    -- power consumption.
    lowLatencyBoost :: Bool
  , -- No documentation found for Nested "VkLatencySleepModeInfoNV" "minimumIntervalUs"
    minimumIntervalUs :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LatencySleepModeInfoNV)
#endif
deriving instance Show LatencySleepModeInfoNV

instance ToCStruct LatencySleepModeInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LatencySleepModeInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SLEEP_MODE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (lowLatencyMode))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (lowLatencyBoost))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (minimumIntervalUs)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SLEEP_MODE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    f

instance FromCStruct LatencySleepModeInfoNV where
  peekCStruct p = do
    lowLatencyMode <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    lowLatencyBoost <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    minimumIntervalUs <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    pure $ LatencySleepModeInfoNV
             (bool32ToBool lowLatencyMode)
             (bool32ToBool lowLatencyBoost)
             minimumIntervalUs

instance Storable LatencySleepModeInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LatencySleepModeInfoNV where
  zero = LatencySleepModeInfoNV
           zero
           zero
           zero


-- | VkLatencySleepInfoNV - Structure specifying the parameters of
-- vkLatencySleepNV
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.Handles.Semaphore',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'latencySleepNV'
data LatencySleepInfoNV = LatencySleepInfoNV
  { -- | 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore'
    -- is a semaphore that is signaled to indicate that the application
    -- /should/ resume input sampling work.
    --
    -- #VUID-VkLatencySleepInfoNV-signalSemaphore-09361#
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore'
    -- /must/ be a timeline semaphore
    --
    -- #VUID-VkLatencySleepInfoNV-signalSemaphore-parameter#
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore'
    -- /must/ be a valid 'Vulkan.Core10.Handles.Semaphore' handle
    signalSemaphore :: Semaphore
  , -- | @value@ is the value that
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore'
    -- is set to for resuming sampling work.
    value :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LatencySleepInfoNV)
#endif
deriving instance Show LatencySleepInfoNV

instance ToCStruct LatencySleepInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LatencySleepInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SLEEP_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (signalSemaphore)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (value)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SLEEP_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Semaphore)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct LatencySleepInfoNV where
  peekCStruct p = do
    signalSemaphore <- peek @Semaphore ((p `plusPtr` 16 :: Ptr Semaphore))
    value <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ LatencySleepInfoNV
             signalSemaphore value

instance Storable LatencySleepInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LatencySleepInfoNV where
  zero = LatencySleepInfoNV
           zero
           zero


-- | VkSetLatencyMarkerInfoNV - Structure specifying the parameters of
-- vkSetLatencyMarkerNV
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'LatencyMarkerNV', 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'setLatencyMarkerNV'
data SetLatencyMarkerInfoNV = SetLatencyMarkerInfoNV
  { -- No documentation found for Nested "VkSetLatencyMarkerInfoNV" "presentID"
    presentID :: Word64
  , -- | @marker@ is the type of timestamp to be recorded.
    --
    -- #VUID-VkSetLatencyMarkerInfoNV-marker-parameter# @marker@ /must/ be a
    -- valid 'LatencyMarkerNV' value
    marker :: LatencyMarkerNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SetLatencyMarkerInfoNV)
#endif
deriving instance Show SetLatencyMarkerInfoNV

instance ToCStruct SetLatencyMarkerInfoNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SetLatencyMarkerInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SET_LATENCY_MARKER_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (presentID)
    poke ((p `plusPtr` 24 :: Ptr LatencyMarkerNV)) (marker)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SET_LATENCY_MARKER_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr LatencyMarkerNV)) (zero)
    f

instance FromCStruct SetLatencyMarkerInfoNV where
  peekCStruct p = do
    presentID <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    marker <- peek @LatencyMarkerNV ((p `plusPtr` 24 :: Ptr LatencyMarkerNV))
    pure $ SetLatencyMarkerInfoNV
             presentID marker

instance Storable SetLatencyMarkerInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SetLatencyMarkerInfoNV where
  zero = SetLatencyMarkerInfoNV
           zero
           zero


-- | VkGetLatencyMarkerInfoNV - Structure specifying the parameters of
-- vkGetLatencyTimingsNV
--
-- = Description
--
-- The elements of @pTimings@ are arranged in the order they were requested
-- in, with the oldest data in the first entry.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'LatencyTimingsFrameReportNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'getLatencyTimingsNV'
data GetLatencyMarkerInfoNV = GetLatencyMarkerInfoNV
  { -- | @pTimings@ is either @NULL@ or a pointer to an array of
    -- 'LatencyTimingsFrameReportNV' structures.
    --
    -- #VUID-VkGetLatencyMarkerInfoNV-pTimings-parameter# @pTimings@ /must/ be
    -- a valid pointer to a 'LatencyTimingsFrameReportNV' structure
    timings :: Ptr LatencyTimingsFrameReportNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GetLatencyMarkerInfoNV)
#endif
deriving instance Show GetLatencyMarkerInfoNV

instance ToCStruct GetLatencyMarkerInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GetLatencyMarkerInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GET_LATENCY_MARKER_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr LatencyTimingsFrameReportNV))) (timings)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GET_LATENCY_MARKER_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr (Ptr LatencyTimingsFrameReportNV))) (zero)
    f

instance FromCStruct GetLatencyMarkerInfoNV where
  peekCStruct p = do
    pTimings <- peek @(Ptr LatencyTimingsFrameReportNV) ((p `plusPtr` 16 :: Ptr (Ptr LatencyTimingsFrameReportNV)))
    pure $ GetLatencyMarkerInfoNV
             pTimings

instance Storable GetLatencyMarkerInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GetLatencyMarkerInfoNV where
  zero = GetLatencyMarkerInfoNV
           zero


-- | VkLatencyTimingsFrameReportNV - Structure containing latency data
--
-- = Members
--
-- The members of the 'LatencyTimingsFrameReportNV' structure describe the
-- following:
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'GetLatencyMarkerInfoNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data LatencyTimingsFrameReportNV = LatencyTimingsFrameReportNV
  { -- No documentation found for Nested "VkLatencyTimingsFrameReportNV" "presentID"
    presentID :: Word64
  , -- No documentation found for Nested "VkLatencyTimingsFrameReportNV" "inputSampleTimeUs"
    inputSampleTimeUs :: Word64
  , -- | @simStartTimeUs@ is the timestamp written when 'setLatencyMarkerNV' is
    -- called with the 'LatencyMarkerNV' enum
    -- 'LATENCY_MARKER_SIMULATION_START_NV'.
    simStartTimeUs :: Word64
  , -- | @simEndTimeUs@ is the timestamp written when 'setLatencyMarkerNV' is
    -- called with the 'LatencyMarkerNV' enum
    -- 'LATENCY_MARKER_SIMULATION_END_NV'
    simEndTimeUs :: Word64
  , -- No documentation found for Nested "VkLatencyTimingsFrameReportNV" "renderSubmitStartTimeUs"
    renderSubmitStartTimeUs :: Word64
  , -- No documentation found for Nested "VkLatencyTimingsFrameReportNV" "renderSubmitEndTimeUs"
    renderSubmitEndTimeUs :: Word64
  , -- | @presentStartTimeUs@ is the timestamp written when 'setLatencyMarkerNV'
    -- is called with the 'LatencyMarkerNV' enum
    -- 'LATENCY_MARKER_PRESENT_START_NV'.
    presentStartTimeUs :: Word64
  , -- | @presentEndTimeUs@ is the timestamp written when 'setLatencyMarkerNV' is
    -- called with the 'LatencyMarkerNV' enum 'LATENCY_MARKER_PRESENT_END_NV'.
    presentEndTimeUs :: Word64
  , -- | @driverStartTimeUs@ is the timestamp written when the first
    -- 'Vulkan.Core10.Queue.queueSubmit' for the frame is called.
    driverStartTimeUs :: Word64
  , -- | @driverEndTimeUs@ is the timestamp written when the final
    -- 'Vulkan.Core10.Queue.queueSubmit' hands off from the Vulkan Driver.
    driverEndTimeUs :: Word64
  , -- | @osRenderQueueStartTimeUs@ is the timestamp written when the final
    -- 'Vulkan.Core10.Queue.queueSubmit' hands off from the Vulkan Driver.
    osRenderQueueStartTimeUs :: Word64
  , -- | @osRenderQueueEndTimeUs@ is the timestamp written when the first
    -- submission reaches the GPU.
    osRenderQueueEndTimeUs :: Word64
  , -- | @gpuRenderStartTimeUs@ is the timestamp written when the first
    -- submission reaches the GPU.
    gpuRenderStartTimeUs :: Word64
  , -- | @gpuRenderEndTimeUs@ is the timestamp written when the final submission
    -- finishes on the GPU for the frame.
    gpuRenderEndTimeUs :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LatencyTimingsFrameReportNV)
#endif
deriving instance Show LatencyTimingsFrameReportNV

instance ToCStruct LatencyTimingsFrameReportNV where
  withCStruct x f = allocaBytes 128 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LatencyTimingsFrameReportNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_TIMINGS_FRAME_REPORT_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (presentID)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (inputSampleTimeUs)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (simStartTimeUs)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (simEndTimeUs)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (renderSubmitStartTimeUs)
    poke ((p `plusPtr` 56 :: Ptr Word64)) (renderSubmitEndTimeUs)
    poke ((p `plusPtr` 64 :: Ptr Word64)) (presentStartTimeUs)
    poke ((p `plusPtr` 72 :: Ptr Word64)) (presentEndTimeUs)
    poke ((p `plusPtr` 80 :: Ptr Word64)) (driverStartTimeUs)
    poke ((p `plusPtr` 88 :: Ptr Word64)) (driverEndTimeUs)
    poke ((p `plusPtr` 96 :: Ptr Word64)) (osRenderQueueStartTimeUs)
    poke ((p `plusPtr` 104 :: Ptr Word64)) (osRenderQueueEndTimeUs)
    poke ((p `plusPtr` 112 :: Ptr Word64)) (gpuRenderStartTimeUs)
    poke ((p `plusPtr` 120 :: Ptr Word64)) (gpuRenderEndTimeUs)
    f
  cStructSize = 128
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_TIMINGS_FRAME_REPORT_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 80 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 88 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 96 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 104 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 112 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 120 :: Ptr Word64)) (zero)
    f

instance FromCStruct LatencyTimingsFrameReportNV where
  peekCStruct p = do
    presentID <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    inputSampleTimeUs <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    simStartTimeUs <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    simEndTimeUs <- peek @Word64 ((p `plusPtr` 40 :: Ptr Word64))
    renderSubmitStartTimeUs <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    renderSubmitEndTimeUs <- peek @Word64 ((p `plusPtr` 56 :: Ptr Word64))
    presentStartTimeUs <- peek @Word64 ((p `plusPtr` 64 :: Ptr Word64))
    presentEndTimeUs <- peek @Word64 ((p `plusPtr` 72 :: Ptr Word64))
    driverStartTimeUs <- peek @Word64 ((p `plusPtr` 80 :: Ptr Word64))
    driverEndTimeUs <- peek @Word64 ((p `plusPtr` 88 :: Ptr Word64))
    osRenderQueueStartTimeUs <- peek @Word64 ((p `plusPtr` 96 :: Ptr Word64))
    osRenderQueueEndTimeUs <- peek @Word64 ((p `plusPtr` 104 :: Ptr Word64))
    gpuRenderStartTimeUs <- peek @Word64 ((p `plusPtr` 112 :: Ptr Word64))
    gpuRenderEndTimeUs <- peek @Word64 ((p `plusPtr` 120 :: Ptr Word64))
    pure $ LatencyTimingsFrameReportNV
             presentID
             inputSampleTimeUs
             simStartTimeUs
             simEndTimeUs
             renderSubmitStartTimeUs
             renderSubmitEndTimeUs
             presentStartTimeUs
             presentEndTimeUs
             driverStartTimeUs
             driverEndTimeUs
             osRenderQueueStartTimeUs
             osRenderQueueEndTimeUs
             gpuRenderStartTimeUs
             gpuRenderEndTimeUs

instance Storable LatencyTimingsFrameReportNV where
  sizeOf ~_ = 128
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LatencyTimingsFrameReportNV where
  zero = LatencyTimingsFrameReportNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkOutOfBandQueueTypeInfoNV - Structure used to describe the queue that
-- is being marked as Out of Band
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'OutOfBandQueueTypeNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'queueNotifyOutOfBandNV'
data OutOfBandQueueTypeInfoNV = OutOfBandQueueTypeInfoNV
  { -- | @queueType@ describes the usage of the queue to be marked as out of
    -- band.
    --
    -- #VUID-VkOutOfBandQueueTypeInfoNV-queueType-parameter# @queueType@ /must/
    -- be a valid 'OutOfBandQueueTypeNV' value
    queueType :: OutOfBandQueueTypeNV }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (OutOfBandQueueTypeInfoNV)
#endif
deriving instance Show OutOfBandQueueTypeInfoNV

instance ToCStruct OutOfBandQueueTypeInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p OutOfBandQueueTypeInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OUT_OF_BAND_QUEUE_TYPE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OutOfBandQueueTypeNV)) (queueType)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_OUT_OF_BAND_QUEUE_TYPE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr OutOfBandQueueTypeNV)) (zero)
    f

instance FromCStruct OutOfBandQueueTypeInfoNV where
  peekCStruct p = do
    queueType <- peek @OutOfBandQueueTypeNV ((p `plusPtr` 16 :: Ptr OutOfBandQueueTypeNV))
    pure $ OutOfBandQueueTypeInfoNV
             queueType

instance Storable OutOfBandQueueTypeInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero OutOfBandQueueTypeInfoNV where
  zero = OutOfBandQueueTypeInfoNV
           zero


-- | VkLatencySubmissionPresentIdNV - Structure used to associate a
-- queueSubmit with a presentId
--
-- = Description
--
-- For any submission to be tracked with low latency mode pacing, it needs
-- to be associated with other submissions in a given present. Applications
-- :must include the VkLatencySubmissionPresentIdNV in the pNext chain of
-- 'Vulkan.Core10.Queue.queueSubmit' to associate that submission with the
-- @presentId@ present for low latency mode.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data LatencySubmissionPresentIdNV = LatencySubmissionPresentIdNV
  { -- No documentation found for Nested "VkLatencySubmissionPresentIdNV" "presentID"
    presentID :: Word64 }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LatencySubmissionPresentIdNV)
#endif
deriving instance Show LatencySubmissionPresentIdNV

instance ToCStruct LatencySubmissionPresentIdNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LatencySubmissionPresentIdNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (presentID)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SUBMISSION_PRESENT_ID_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    f

instance FromCStruct LatencySubmissionPresentIdNV where
  peekCStruct p = do
    presentID <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    pure $ LatencySubmissionPresentIdNV
             presentID

instance Storable LatencySubmissionPresentIdNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LatencySubmissionPresentIdNV where
  zero = LatencySubmissionPresentIdNV
           zero


-- | VkSwapchainLatencyCreateInfoNV - Specify that a swapchain will use low
-- latency mode
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data SwapchainLatencyCreateInfoNV = SwapchainLatencyCreateInfoNV
  { -- No documentation found for Nested "VkSwapchainLatencyCreateInfoNV" "latencyModeEnable"
    latencyModeEnable :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainLatencyCreateInfoNV)
#endif
deriving instance Show SwapchainLatencyCreateInfoNV

instance ToCStruct SwapchainLatencyCreateInfoNV where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainLatencyCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (latencyModeEnable))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_LATENCY_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct SwapchainLatencyCreateInfoNV where
  peekCStruct p = do
    latencyModeEnable <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ SwapchainLatencyCreateInfoNV
             (bool32ToBool latencyModeEnable)

instance Storable SwapchainLatencyCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainLatencyCreateInfoNV where
  zero = SwapchainLatencyCreateInfoNV
           zero


-- | VkLatencySurfaceCapabilitiesNV - Structure describing surface optimized
-- presentation modes for use with low latency mode
--
-- = Description
--
-- If @pPresentModes@ is @NULL@, then the number of present modes that are
-- optimized for use with low latency mode returned in @presentModeCount@.
-- Otherwise, @presentModeCount@ must be set by the user to the number of
-- elements in the @pPresentModes@ array, and on return the variable is
-- overwritten with the number of values actually written to
-- @pPresentModes@. If the value of @presentModeCount@ is less than the
-- number of optimized present modes, at most @presentModeCount@ values
-- will be written to @pPresentModes@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkLatencySurfaceCapabilitiesNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV'
--
-- -   #VUID-VkLatencySurfaceCapabilitiesNV-pPresentModes-parameter# If
--     @presentModeCount@ is not @0@, and @pPresentModes@ is not @NULL@,
--     @pPresentModes@ /must/ be a valid pointer to an array of
--     @presentModeCount@ 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR'
--     values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'Vulkan.Extensions.VK_KHR_surface.PresentModeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data LatencySurfaceCapabilitiesNV = LatencySurfaceCapabilitiesNV
  { -- | @presentModeCount@ is the number of presentation modes provided.
    presentModeCount :: Word32
  , -- | @pPresentModes@ is list of presentation modes optimized for use with low
    -- latency mode with @presentModeCount@ entries.
    presentModes :: Ptr PresentModeKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (LatencySurfaceCapabilitiesNV)
#endif
deriving instance Show LatencySurfaceCapabilitiesNV

instance ToCStruct LatencySurfaceCapabilitiesNV where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p LatencySurfaceCapabilitiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (presentModeCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR))) (presentModes)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_LATENCY_SURFACE_CAPABILITIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct LatencySurfaceCapabilitiesNV where
  peekCStruct p = do
    presentModeCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pPresentModes <- peek @(Ptr PresentModeKHR) ((p `plusPtr` 24 :: Ptr (Ptr PresentModeKHR)))
    pure $ LatencySurfaceCapabilitiesNV
             presentModeCount pPresentModes

instance Storable LatencySurfaceCapabilitiesNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero LatencySurfaceCapabilitiesNV where
  zero = LatencySurfaceCapabilitiesNV
           zero
           zero


-- | VkLatencyMarkerNV - Structure used to mark different points in latency
--
-- = Description
--
-- The members of the 'LatencyMarkerNV' are used as arguments for
-- 'setLatencyMarkerNV' in the use cases described below:
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'SetLatencyMarkerInfoNV'
newtype LatencyMarkerNV = LatencyMarkerNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'LATENCY_MARKER_SIMULATION_START_NV' /should/ be called at the start of
-- the simulation execution each frame, but after the call to
-- 'latencySleepNV'.
pattern LATENCY_MARKER_SIMULATION_START_NV = LatencyMarkerNV 0

-- | 'LATENCY_MARKER_SIMULATION_END_NV' /should/ be called at the end of the
-- simulation execution each frame.
pattern LATENCY_MARKER_SIMULATION_END_NV = LatencyMarkerNV 1

-- | 'LATENCY_MARKER_RENDERSUBMIT_START_NV' /should/ be called at the
-- beginning of the render submission execution each frame. This /should/
-- be wherever Vulkan API calls are made and /must/ not span into
-- asynchronous rendering.
pattern LATENCY_MARKER_RENDERSUBMIT_START_NV = LatencyMarkerNV 2

-- | 'LATENCY_MARKER_RENDERSUBMIT_END_NV' /should/ be called at the end of
-- the render submission execution each frame.
pattern LATENCY_MARKER_RENDERSUBMIT_END_NV = LatencyMarkerNV 3

-- | 'LATENCY_MARKER_PRESENT_START_NV' /should/ be called just before
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'.
pattern LATENCY_MARKER_PRESENT_START_NV = LatencyMarkerNV 4

-- | 'LATENCY_MARKER_PRESENT_END_NV' /should/ be called when
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' returns.
pattern LATENCY_MARKER_PRESENT_END_NV = LatencyMarkerNV 5

-- | 'LATENCY_MARKER_INPUT_SAMPLE_NV' /should/ be called just before the
-- application gathers input data.
pattern LATENCY_MARKER_INPUT_SAMPLE_NV = LatencyMarkerNV 6

-- | 'LATENCY_MARKER_TRIGGER_FLASH_NV' /should/ be called anywhere between
-- 'LATENCY_MARKER_SIMULATION_START_NV' and
-- 'LATENCY_MARKER_SIMULATION_END_NV' whenever a left mouse click occurs.
pattern LATENCY_MARKER_TRIGGER_FLASH_NV = LatencyMarkerNV 7

-- No documentation found for Nested "VkLatencyMarkerNV" "VK_LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_START_NV"
pattern LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_START_NV = LatencyMarkerNV 8

-- No documentation found for Nested "VkLatencyMarkerNV" "VK_LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_END_NV"
pattern LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_END_NV = LatencyMarkerNV 9

-- No documentation found for Nested "VkLatencyMarkerNV" "VK_LATENCY_MARKER_OUT_OF_BAND_PRESENT_START_NV"
pattern LATENCY_MARKER_OUT_OF_BAND_PRESENT_START_NV = LatencyMarkerNV 10

-- No documentation found for Nested "VkLatencyMarkerNV" "VK_LATENCY_MARKER_OUT_OF_BAND_PRESENT_END_NV"
pattern LATENCY_MARKER_OUT_OF_BAND_PRESENT_END_NV = LatencyMarkerNV 11

{-# COMPLETE
  LATENCY_MARKER_SIMULATION_START_NV
  , LATENCY_MARKER_SIMULATION_END_NV
  , LATENCY_MARKER_RENDERSUBMIT_START_NV
  , LATENCY_MARKER_RENDERSUBMIT_END_NV
  , LATENCY_MARKER_PRESENT_START_NV
  , LATENCY_MARKER_PRESENT_END_NV
  , LATENCY_MARKER_INPUT_SAMPLE_NV
  , LATENCY_MARKER_TRIGGER_FLASH_NV
  , LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_START_NV
  , LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_END_NV
  , LATENCY_MARKER_OUT_OF_BAND_PRESENT_START_NV
  , LATENCY_MARKER_OUT_OF_BAND_PRESENT_END_NV ::
    LatencyMarkerNV
  #-}

conNameLatencyMarkerNV :: String
conNameLatencyMarkerNV = "LatencyMarkerNV"

enumPrefixLatencyMarkerNV :: String
enumPrefixLatencyMarkerNV = "LATENCY_MARKER_"

showTableLatencyMarkerNV :: [(LatencyMarkerNV, String)]
showTableLatencyMarkerNV =
  [
    ( LATENCY_MARKER_SIMULATION_START_NV
    , "SIMULATION_START_NV"
    )
  ,
    ( LATENCY_MARKER_SIMULATION_END_NV
    , "SIMULATION_END_NV"
    )
  ,
    ( LATENCY_MARKER_RENDERSUBMIT_START_NV
    , "RENDERSUBMIT_START_NV"
    )
  ,
    ( LATENCY_MARKER_RENDERSUBMIT_END_NV
    , "RENDERSUBMIT_END_NV"
    )
  ,
    ( LATENCY_MARKER_PRESENT_START_NV
    , "PRESENT_START_NV"
    )
  , (LATENCY_MARKER_PRESENT_END_NV, "PRESENT_END_NV")
  , (LATENCY_MARKER_INPUT_SAMPLE_NV, "INPUT_SAMPLE_NV")
  ,
    ( LATENCY_MARKER_TRIGGER_FLASH_NV
    , "TRIGGER_FLASH_NV"
    )
  ,
    ( LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_START_NV
    , "OUT_OF_BAND_RENDERSUBMIT_START_NV"
    )
  ,
    ( LATENCY_MARKER_OUT_OF_BAND_RENDERSUBMIT_END_NV
    , "OUT_OF_BAND_RENDERSUBMIT_END_NV"
    )
  ,
    ( LATENCY_MARKER_OUT_OF_BAND_PRESENT_START_NV
    , "OUT_OF_BAND_PRESENT_START_NV"
    )
  ,
    ( LATENCY_MARKER_OUT_OF_BAND_PRESENT_END_NV
    , "OUT_OF_BAND_PRESENT_END_NV"
    )
  ]

instance Show LatencyMarkerNV where
  showsPrec =
    enumShowsPrec
      enumPrefixLatencyMarkerNV
      showTableLatencyMarkerNV
      conNameLatencyMarkerNV
      (\(LatencyMarkerNV x) -> x)
      (showsPrec 11)

instance Read LatencyMarkerNV where
  readPrec =
    enumReadPrec
      enumPrefixLatencyMarkerNV
      showTableLatencyMarkerNV
      conNameLatencyMarkerNV
      LatencyMarkerNV

-- | VkOutOfBandQueueTypeNV - Type of out of band queue
--
-- = Description
--
-- The members of the 'OutOfBandQueueTypeNV' are used to describe the queue
-- type in 'OutOfBandQueueTypeInfoNV' as described below:
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_low_latency2 VK_NV_low_latency2>,
-- 'OutOfBandQueueTypeInfoNV'
newtype OutOfBandQueueTypeNV = OutOfBandQueueTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'OUT_OF_BAND_QUEUE_TYPE_RENDER_NV' indicates that work will be submitted
-- to this queue.
pattern OUT_OF_BAND_QUEUE_TYPE_RENDER_NV = OutOfBandQueueTypeNV 0

-- | 'OUT_OF_BAND_QUEUE_TYPE_PRESENT_NV' indicates that this queue will be
-- presented from.
pattern OUT_OF_BAND_QUEUE_TYPE_PRESENT_NV = OutOfBandQueueTypeNV 1

{-# COMPLETE
  OUT_OF_BAND_QUEUE_TYPE_RENDER_NV
  , OUT_OF_BAND_QUEUE_TYPE_PRESENT_NV ::
    OutOfBandQueueTypeNV
  #-}

conNameOutOfBandQueueTypeNV :: String
conNameOutOfBandQueueTypeNV = "OutOfBandQueueTypeNV"

enumPrefixOutOfBandQueueTypeNV :: String
enumPrefixOutOfBandQueueTypeNV = "OUT_OF_BAND_QUEUE_TYPE_"

showTableOutOfBandQueueTypeNV :: [(OutOfBandQueueTypeNV, String)]
showTableOutOfBandQueueTypeNV =
  [
    ( OUT_OF_BAND_QUEUE_TYPE_RENDER_NV
    , "RENDER_NV"
    )
  ,
    ( OUT_OF_BAND_QUEUE_TYPE_PRESENT_NV
    , "PRESENT_NV"
    )
  ]

instance Show OutOfBandQueueTypeNV where
  showsPrec =
    enumShowsPrec
      enumPrefixOutOfBandQueueTypeNV
      showTableOutOfBandQueueTypeNV
      conNameOutOfBandQueueTypeNV
      (\(OutOfBandQueueTypeNV x) -> x)
      (showsPrec 11)

instance Read OutOfBandQueueTypeNV where
  readPrec =
    enumReadPrec
      enumPrefixOutOfBandQueueTypeNV
      showTableOutOfBandQueueTypeNV
      conNameOutOfBandQueueTypeNV
      OutOfBandQueueTypeNV

type NV_LOW_LATENCY_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_LOW_LATENCY_2_SPEC_VERSION"
pattern NV_LOW_LATENCY_2_SPEC_VERSION :: forall a . Integral a => a
pattern NV_LOW_LATENCY_2_SPEC_VERSION = 1


type NV_LOW_LATENCY_2_EXTENSION_NAME = "VK_NV_low_latency2"

-- No documentation found for TopLevel "VK_NV_LOW_LATENCY_2_EXTENSION_NAME"
pattern NV_LOW_LATENCY_2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_LOW_LATENCY_2_EXTENSION_NAME = "VK_NV_low_latency2"

