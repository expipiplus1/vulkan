{-# language CPP #-}
-- | = Name
--
-- VK_EXT_present_timing - device extension
--
-- = VK_EXT_present_timing
--
-- [__Name String__]
--     @VK_EXT_present_timing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     209
--
-- [__Revision__]
--     3
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_swapchain VK_KHR_swapchain>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_present_id2 VK_KHR_present_id2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_surface_capabilities2 VK_KHR_get_surface_capabilities2>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>
--
-- [__Contact__]
--
--     -   Lionel Duc
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_present_timing] @nvlduc%0A*Here describe the issue or question you have about the VK_EXT_present_timing extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_present_timing.adoc VK_EXT_present_timing>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-10-09
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Ian Elliott, Google
--
--     -   James Jones, NVIDIA
--
--     -   Jeff Juliano, NVIDIA
--
--     -   Daniel Rakos, AMD
--
--     -   Daniel Stone, Collabora
--
--     -   Daniel Vetter, Intel
--
--     -   Aric Cyr, AMD
--
--     -   Faith Ekstrand, Intel
--
--     -   Nicolai Hähnle, AMD
--
--     -   Alon Or-Bach, Samsung
--
--     -   Niklas Smedberg, Unity Technologies
--
--     -   Tobias Hector, AMD
--
--     -   Lionel Duc, NVIDIA
--
--     -   Lina Versace, Google
--
--     -   Sebastian Wick, Red Hat
--
--     -   Jakob Bornecrantz, Collabora
--
--     -   David Kvasnica, NVIDIA
--
-- == Description
--
-- This device extension allows an application that uses the
-- @VK_KHR_swapchain@ extension to obtain information about the
-- presentation engine’s display, to obtain timing information about each
-- present, and to schedule a present to happen no earlier than a desired
-- time. An application can use this to minimize various visual anomalies
-- (e.g. stuttering).
--
-- Traditional game and real-time animation applications need to correctly
-- position their geometry for when the presentable image will be presented
-- to the user. To accomplish this, applications need various timing
-- information about the presentation engine’s display. They need to know
-- when presentable images were actually presented, and when they could
-- have been presented. Applications also need to tell the presentation
-- engine to display an image no sooner than a given time. This allows the
-- application to avoid stuttering, so the animation looks smooth to the
-- user.
--
-- == New Commands
--
-- -   'getPastPresentationTimingEXT'
--
-- -   'getSwapchainTimeDomainPropertiesEXT'
--
-- -   'getSwapchainTimingPropertiesEXT'
--
-- -   'setSwapchainPresentTimingQueueSizeEXT'
--
-- == New Structures
--
-- -   'PastPresentationTimingEXT'
--
-- -   'PastPresentationTimingInfoEXT'
--
-- -   'PastPresentationTimingPropertiesEXT'
--
-- -   'PresentStageTimeEXT'
--
-- -   'PresentTimingInfoEXT'
--
-- -   'SwapchainTimeDomainPropertiesEXT'
--
-- -   'SwapchainTimingPropertiesEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_calibrated_timestamps.CalibratedTimestampInfoKHR':
--
--     -   'SwapchainCalibratedTimestampInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePresentTimingFeaturesEXT'
--
-- -   Extending 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR':
--
--     -   'PresentTimingsInfoEXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR':
--
--     -   'PresentTimingSurfaceCapabilitiesEXT'
--
-- == New Enums
--
-- -   'PastPresentationTimingFlagBitsEXT'
--
-- -   'PresentStageFlagBitsEXT'
--
-- -   'PresentTimingInfoFlagBitsEXT'
--
-- == New Bitmasks
--
-- -   'PastPresentationTimingFlagsEXT'
--
-- -   'PresentStageFlagsEXT'
--
-- -   'PresentTimingInfoFlagsEXT'
--
-- == New Enum Constants
--
-- -   'EXT_PRESENT_TIMING_EXTENSION_NAME'
--
-- -   'EXT_PRESENT_TIMING_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_PRESENT_TIMING_QUEUE_FULL_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_TIMING_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_TIMING_PROPERTIES_EXT'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateFlagBitsKHR':
--
--     -   'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_TIMING_BIT_EXT'
--
-- -   Extending 'TimeDomainKHR':
--
--     -   'TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT'
--
--     -   'TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT'
--
-- == Issues
--
-- 1) How does the application determine refresh duration, quanta for
-- change, whether FRR vs. VRR, etc.
--
-- The query returns two values: 1) a refresh-cycle duration
-- (@refreshDuration@), and 2) an indication whether the timing is
-- currently fixed (FRR) or variable (VRR). If @refreshDuration@ is zero,
-- the platform cannot supply these values until after at least one
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' has been done, from
-- this time (e.g. if 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'
-- has been previously called for this swapchain, at least one additional
-- call must be made). After calling
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', the query can be
-- repeated until @refreshDuration@ is non-zero, at which point the FRR vs.
-- VRR indication will also be valid.
--
-- If the presentation engine’s @refreshDuration@ is a fixed value, the
-- application’s image present duration (IPD) should be a multiple of
-- @refreshDuration@. That is, the quanta for changing the IPD is
-- @refreshDuration@. For example, if @refreshDuration@ is 16.67ms, the IPD
-- can be 16.67ms, 33.33ms, 50.0ms, etc.
--
-- If the presentation engine’s @refreshDuration@ is variable,
-- @refreshDuration@ is the minimum value of the application’s IPD, and the
-- IPD can be larger by any quanta that is meaningful to the application.
-- For example, if the @refreshDuration@ is 10ms (i.e. the maximum refresh
-- rate is 100Hz), the application can choose an IPD of 11ms, 13.33ms,
-- 13.5ms, or 66.0ms; any value greater than or equal to 10ms is valid.
-- There may be negative consequences for choosing an IPD that is too high,
-- as the presentation engine may actually have a practical maximum
-- @refreshDuration@, where it needs to display the previous image again,
-- and during this time the presentation engine might delay displaying a
-- newly-presented image.
--
-- FRR displays on at least one platform (Wayland) are not necessarily
-- fixed; but can change over time. For example, if a full-screen video
-- player application is visible, the display may operate at a 24Hz refresh
-- cycle; and then later switch to 60Hz when multiple windows are visible.
--
-- VRR displays on some platforms can also be seen as having different
-- characteristics over time. For example, if an application’s window is
-- full-screen-exclusive (i.e. no other window or window system component
-- is visible), the display can look like a VRR display (however that is
-- defined). If the application’s window is not full-screen-exclusive (e.g.
-- a normal multi-window case), the display can look like an FRR display
-- (i.e. because the compositor is trying to treat all windows in a
-- consistent manner). A different issue will deal with how the timing
-- characteristics can change over time.
--
-- 2) Do we return min\/max values for refresh duration for VRR?
--
-- Return only the minimum value of refreshDuration for a VRR.
--
-- VRR displays have a minimum and maximum refresh rate, and therefore a
-- minimum and maximum refreshDuration. It has been asserted that the
-- display effectively does not have a minimum refresh rate. That is
-- because if an application does not present soon enough, the display
-- hardware will automatically re-display the previous image. However, when
-- the display does that, an application cannot present a new image for a
-- certain period of time. It is unclear about whether that period is large
-- enough to cause visual artifacts.
--
-- 3) How to deal with changes in timing properties?
--
-- __RESOLVED__: The 'PastPresentationTimingPropertiesEXT' structure that
-- is returned by 'getPastPresentationTimingEXT' contains
-- @timeDomainsCounter@, which is incremented if the time domain enabled
-- for the swapchain is not currently available.
--
-- An example of why display timing properties can change is if a surface
-- changes from being a window that’s a subset of the display size, to
-- becoming full-screen-exclusive. While the surface was a subset of the
-- display, a compositor might enforce fixed timings on the surface (e.g.
-- FRR of 60Hz), where the presentation engine might be free to allow VRR
-- behavior of a full-screen-exclusive surface.
--
-- It is possible that a full-screen-exclusive window can become
-- temporarily obscured (e.g. when a short-term dialog pops up). In this
-- case, the surface might use FRR timings while the dialog is visible and
-- VRR otherwise.
--
-- 4) One Query for all Timing info vs. an initial query to determine FRR
-- vs. VRR, and then FRR-specific vs VRR-specific queries?
--
-- __RESOLVED__: Have one query, as described in issue 1, that can be
-- called whenever the application needs to obtain the timing properties of
-- the surface.
--
-- 5) Query to determine time domain?
--
-- __RESOLVED__: Have a query to determine the time domain. This extension
-- defines a basic swapchain-local time domain. Other extensions can add
-- other platform-specific time domains.
--
-- 6) What time to use for targetPresentTime for early images?
--
-- __RESOLVED__: Have no query for determining the current time in the PE’s
-- time domain; and do allow the special value of zero for
-- targetPresentTime, meaning that there is no target.
--
-- On some platforms, there is no way to determine the current time, nor to
-- determine surface timing properties until after at least one image has
-- been presented.
--
-- In such cases, the special value of zero allows the application to
-- indicate that timing feedback is desired, but that no targetPresentTime
-- is requested. Later, once the application has obtained feedback, it can
-- specify targetPresentTime by using the result’s actualPresentTime.
--
-- 7) How long before an application’s request for new image duration is
-- honored?
--
-- Apparently, changes to some vendors\' display hardware settings do not
-- take effect immediately. It is not clear what settings, and therefore,
-- it is not clear how to address this issue.
--
-- 8) Do we have a query for the anticipated latency from present to
-- feedback?
--
-- __RESOLVED__: Do not provide a query for the feedback latency.
--
-- There is some amount of latency from when an application calls
-- vkQueuePresentKHR to when the image is displayed to the user, to when
-- feedback is available to the application on when the image was actually
-- displayed to the user. The first time (from the call till the image is
-- presented) generally doesn’t matter, because the application will likely
-- be providing a targetPresentTime (i.e. the application may have some
-- indication for how long this will be). However, the latency between
-- targetPresentTime until feedback is available may be much longer. For
-- example, on Android on the 1st-generation Pixel phone (60Hz FRR
-- display), the latency was approximately 5 refresh cycles (83.33ms). For
-- higher-frequency displays, the latency may have a larger number of
-- refresh cycles.
--
-- 9) Do we have a query(s) about the number of VkPastPresentationTimingEXT
-- structs to keep?
--
-- __RESOLVED__: Do not provide a query for the number of results the
-- swapchain is allowed to store before querying them with
-- vkGetPastPresentationTimingEXT. Let the application specify that value
-- with a dedicated API.
--
-- 10) How is the SWAPCHAIN_LOCAL and STAGE_LOCAL time domain used with the
-- calibrated timestamps extension?
--
-- __RESOLVED__: Define a struct to chain into
-- VkCalibratedTimestampInfoEXT::pNext that specifies a swapchain and
-- present stage.
--
-- 11) Should VK_PRESENT_MODE_FIFO_LATEST_READY_EXT be part of this
-- extension, or split out into its own extension?
--
-- __RESOLVED__: It is only tangentially related. Split it out into its own
-- extension and define the interaction here.
--
-- == Version History
--
-- -   Revision 1, 2018-05-11 (Ian Elliott)
--
--     -   Internal revisions.
--
-- -   Revision 2, 2022-11-30 (Lionel Duc)
--
--     -   Rebase for public discussions.
--
-- -   Revision 3, 2024-10-09 (Lionel Duc)
--
--     -   Public revisions.
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_EXT_present_timing Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_present_timing  ( setSwapchainPresentTimingQueueSizeEXT
                                                , getSwapchainTimingPropertiesEXT
                                                , getSwapchainTimeDomainPropertiesEXT
                                                , getPastPresentationTimingEXT
                                                , PhysicalDevicePresentTimingFeaturesEXT(..)
                                                , PresentTimingSurfaceCapabilitiesEXT(..)
                                                , SwapchainTimingPropertiesEXT(..)
                                                , SwapchainTimeDomainPropertiesEXT(..)
                                                , PresentStageTimeEXT(..)
                                                , PastPresentationTimingInfoEXT(..)
                                                , PastPresentationTimingPropertiesEXT(..)
                                                , PastPresentationTimingEXT(..)
                                                , PresentTimingsInfoEXT(..)
                                                , PresentTimingInfoEXT(..)
                                                , SwapchainCalibratedTimestampInfoEXT(..)
                                                , TimeDomainKHR( TIME_DOMAIN_DEVICE_KHR
                                                               , TIME_DOMAIN_CLOCK_MONOTONIC_KHR
                                                               , TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR
                                                               , TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR
                                                               , TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT
                                                               , TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT
                                                               , ..
                                                               )
                                                , PresentStageFlagsEXT
                                                , PresentStageFlagBitsEXT( PRESENT_STAGE_QUEUE_OPERATIONS_END_BIT_EXT
                                                                         , PRESENT_STAGE_REQUEST_DEQUEUED_BIT_EXT
                                                                         , PRESENT_STAGE_IMAGE_FIRST_PIXEL_OUT_BIT_EXT
                                                                         , PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT
                                                                         , ..
                                                                         )
                                                , PastPresentationTimingFlagsEXT
                                                , PastPresentationTimingFlagBitsEXT( PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT
                                                                                   , PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT
                                                                                   , ..
                                                                                   )
                                                , PresentTimingInfoFlagsEXT
                                                , PresentTimingInfoFlagBitsEXT( PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT
                                                                              , PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT
                                                                              , ..
                                                                              )
                                                , EXT_PRESENT_TIMING_SPEC_VERSION
                                                , pattern EXT_PRESENT_TIMING_SPEC_VERSION
                                                , EXT_PRESENT_TIMING_EXTENSION_NAME
                                                , pattern EXT_PRESENT_TIMING_EXTENSION_NAME
                                                , SwapchainKHR(..)
                                                , SwapchainCreateFlagBitsKHR(..)
                                                , SwapchainCreateFlagsKHR
                                                ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
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
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import qualified Data.Vector (null)
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
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkGetPastPresentationTimingEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainTimeDomainPropertiesEXT))
import Vulkan.Dynamic (DeviceCmds(pVkGetSwapchainTimingPropertiesEXT))
import Vulkan.Dynamic (DeviceCmds(pVkSetSwapchainPresentTimingQueueSizeEXT))
import Vulkan.Core10.Handles (Device_T)
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.Handles (SwapchainKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_TIMING_FEATURES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SWAPCHAIN_TIMING_PROPERTIES_EXT))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_swapchain (SwapchainCreateFlagsKHR)
import Vulkan.Extensions.Handles (SwapchainKHR(..))
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetSwapchainPresentTimingQueueSizeEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Word32 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Word32 -> IO Result

-- | vkSetSwapchainPresentTimingQueueSizeEXT - Allocate memory for the
-- swapchain-internal timing results queue
--
-- = Description
--
-- If this function is called multiple times, the internal queue is
-- reallocated to fit the new @size@. If the new @size@ is less than the
-- current number of outstanding results,
-- 'Vulkan.Core10.Enums.Result.NOT_READY' is returned and no allocation is
-- performed.
--
-- == Valid Usage
--
-- -   #VUID-vkSetSwapchainPresentTimingQueueSizeEXT-swapchain-12229#
--     @swapchain@ /must/ have been created with
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@flags@
--     containing
--     'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_TIMING_BIT_EXT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkSetSwapchainPresentTimingQueueSizeEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkSetSwapchainPresentTimingQueueSizeEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkSetSwapchainPresentTimingQueueSizeEXT-swapchain-parent#
--     @swapchain@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Extensions.Handles.SwapchainKHR'
setSwapchainPresentTimingQueueSizeEXT :: forall io
                                       . (MonadIO io)
                                      => -- | @device@ is the device associated with @swapchain@.
                                         Device
                                      -> -- | @swapchain@ is the swapchain to allocate a results queue for.
                                         SwapchainKHR
                                      -> -- | @size@ is the requested number of slots in the internal results queue.
                                         ("size" ::: Word32)
                                      -> io (Result)
setSwapchainPresentTimingQueueSizeEXT device swapchain size = liftIO $ do
  let vkSetSwapchainPresentTimingQueueSizeEXTPtr = pVkSetSwapchainPresentTimingQueueSizeEXT (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkSetSwapchainPresentTimingQueueSizeEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkSetSwapchainPresentTimingQueueSizeEXT is null" Nothing Nothing
  let vkSetSwapchainPresentTimingQueueSizeEXT' = mkVkSetSwapchainPresentTimingQueueSizeEXT vkSetSwapchainPresentTimingQueueSizeEXTPtr
  r <- traceAroundEvent "vkSetSwapchainPresentTimingQueueSizeEXT" (vkSetSwapchainPresentTimingQueueSizeEXT'
                                                                     (deviceHandle (device))
                                                                     (swapchain)
                                                                     (size))
  when (r < SUCCESS) (throwIO (VulkanException r))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainTimingPropertiesEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr SwapchainTimingPropertiesEXT -> Ptr Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr SwapchainTimingPropertiesEXT -> Ptr Word64 -> IO Result

-- | vkGetSwapchainTimingPropertiesEXT - Obtain the display timing properties
-- of the PE’s display
--
-- = Description
--
-- If 'getSwapchainTimingPropertiesEXT' returns
-- 'Vulkan.Core10.Enums.Result.NOT_READY', the implementation was not able
-- to determine the current refresh cycle duration. Some platforms /may/
-- not provide timing properties until after at least one image has been
-- presented to the @swapchain@. If timing properties change for the
-- @swapchain@, these platforms /may/ not provide updated results until
-- after at least one additional image has been presented to the
-- @swapchain@.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetSwapchainTimingPropertiesEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetSwapchainTimingPropertiesEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkGetSwapchainTimingPropertiesEXT-pSwapchainTimingProperties-parameter#
--     @pSwapchainTimingProperties@ /must/ be a valid pointer to a
--     'SwapchainTimingPropertiesEXT' structure
--
-- -   #VUID-vkGetSwapchainTimingPropertiesEXT-pSwapchainTimingPropertiesCounter-parameter#
--     If @pSwapchainTimingPropertiesCounter@ is not @NULL@,
--     @pSwapchainTimingPropertiesCounter@ /must/ be a valid pointer to a
--     @uint64_t@ value
--
-- -   #VUID-vkGetSwapchainTimingPropertiesEXT-swapchain-parent#
--     @swapchain@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.NOT_READY'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'SwapchainTimingPropertiesEXT'
getSwapchainTimingPropertiesEXT :: forall io
                                 . (MonadIO io)
                                => -- | @device@ is the device associated with @swapchain@.
                                   Device
                                -> -- | @swapchain@ is the swapchain to obtain timing properties for.
                                   SwapchainKHR
                                -> io (Result, SwapchainTimingPropertiesEXT, ("swapchainTimingPropertiesCounter" ::: Word64))
getSwapchainTimingPropertiesEXT device swapchain = liftIO . evalContT $ do
  let vkGetSwapchainTimingPropertiesEXTPtr = pVkGetSwapchainTimingPropertiesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetSwapchainTimingPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSwapchainTimingPropertiesEXT is null" Nothing Nothing
  let vkGetSwapchainTimingPropertiesEXT' = mkVkGetSwapchainTimingPropertiesEXT vkGetSwapchainTimingPropertiesEXTPtr
  pPSwapchainTimingProperties <- ContT (withZeroCStruct @SwapchainTimingPropertiesEXT)
  pPSwapchainTimingPropertiesCounter <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ traceAroundEvent "vkGetSwapchainTimingPropertiesEXT" (vkGetSwapchainTimingPropertiesEXT'
                                                                      (deviceHandle (device))
                                                                      (swapchain)
                                                                      (pPSwapchainTimingProperties)
                                                                      (pPSwapchainTimingPropertiesCounter))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchainTimingProperties <- lift $ peekCStruct @SwapchainTimingPropertiesEXT pPSwapchainTimingProperties
  pSwapchainTimingPropertiesCounter <- lift $ peek @Word64 pPSwapchainTimingPropertiesCounter
  pure $ (r, pSwapchainTimingProperties, pSwapchainTimingPropertiesCounter)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainTimeDomainPropertiesEXT
  :: FunPtr (Ptr Device_T -> SwapchainKHR -> Ptr SwapchainTimeDomainPropertiesEXT -> Ptr Word64 -> IO Result) -> Ptr Device_T -> SwapchainKHR -> Ptr SwapchainTimeDomainPropertiesEXT -> Ptr Word64 -> IO Result

-- | vkGetSwapchainTimeDomainPropertiesEXT - Obtain the time domains
-- supported by the PE for the swapchain
--
-- = Description
--
-- If upon return 'SwapchainTimeDomainPropertiesEXT'::@timeDomainCount@ is
-- smaller than the number of time domains supported for the given
-- @swapchain@, 'Vulkan.Core10.Enums.Result.INCOMPLETE' will be returned
-- instead of 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate that not all
-- the available values were returned.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetSwapchainTimeDomainPropertiesEXT-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetSwapchainTimeDomainPropertiesEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-vkGetSwapchainTimeDomainPropertiesEXT-pSwapchainTimeDomainProperties-parameter#
--     @pSwapchainTimeDomainProperties@ /must/ be a valid pointer to a
--     'SwapchainTimeDomainPropertiesEXT' structure
--
-- -   #VUID-vkGetSwapchainTimeDomainPropertiesEXT-pTimeDomainsCounter-parameter#
--     If @pTimeDomainsCounter@ is not @NULL@, @pTimeDomainsCounter@ /must/
--     be a valid pointer to a @uint64_t@ value
--
-- -   #VUID-vkGetSwapchainTimeDomainPropertiesEXT-swapchain-parent#
--     @swapchain@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.Handles.SwapchainKHR',
-- 'SwapchainTimeDomainPropertiesEXT'
getSwapchainTimeDomainPropertiesEXT :: forall io
                                     . (MonadIO io)
                                    => -- | @device@ is the device associated with @swapchain@.
                                       Device
                                    -> -- | @swapchain@ is the swapchain to obtain time domain properties for.
                                       SwapchainKHR
                                    -> io (Result, SwapchainTimeDomainPropertiesEXT, ("timeDomainsCounter" ::: Word64))
getSwapchainTimeDomainPropertiesEXT device swapchain = liftIO . evalContT $ do
  let vkGetSwapchainTimeDomainPropertiesEXTPtr = pVkGetSwapchainTimeDomainPropertiesEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetSwapchainTimeDomainPropertiesEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetSwapchainTimeDomainPropertiesEXT is null" Nothing Nothing
  let vkGetSwapchainTimeDomainPropertiesEXT' = mkVkGetSwapchainTimeDomainPropertiesEXT vkGetSwapchainTimeDomainPropertiesEXTPtr
  pPSwapchainTimeDomainProperties <- ContT (withZeroCStruct @SwapchainTimeDomainPropertiesEXT)
  pPTimeDomainsCounter <- ContT $ bracket (callocBytes @Word64 8) free
  r <- lift $ traceAroundEvent "vkGetSwapchainTimeDomainPropertiesEXT" (vkGetSwapchainTimeDomainPropertiesEXT'
                                                                          (deviceHandle (device))
                                                                          (swapchain)
                                                                          (pPSwapchainTimeDomainProperties)
                                                                          (pPTimeDomainsCounter))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pSwapchainTimeDomainProperties <- lift $ peekCStruct @SwapchainTimeDomainPropertiesEXT pPSwapchainTimeDomainProperties
  pTimeDomainsCounter <- lift $ peek @Word64 pPTimeDomainsCounter
  pure $ (r, pSwapchainTimeDomainProperties, pTimeDomainsCounter)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPastPresentationTimingEXT
  :: FunPtr (Ptr Device_T -> Ptr PastPresentationTimingInfoEXT -> Ptr PastPresentationTimingPropertiesEXT -> IO Result) -> Ptr Device_T -> Ptr PastPresentationTimingInfoEXT -> Ptr PastPresentationTimingPropertiesEXT -> IO Result

-- | vkGetPastPresentationTimingEXT - Obtain timing of previously-presented
-- images
--
-- = Description
--
-- If upon return the value of
-- 'PastPresentationTimingPropertiesEXT'::@presentationTimingCount@ is less
-- than the number of available timing records for the given
-- 'PastPresentationTimingInfoEXT'::@swapchain@,
-- 'Vulkan.Core10.Enums.Result.INCOMPLETE' is returned instead of
-- 'Vulkan.Core10.Enums.Result.SUCCESS' to indicate that not all the
-- available values were returned.
--
-- Upon return, zero or more slots of the @swapchain@ internal timing
-- results queue, equal to the number of entries written to
-- 'PastPresentationTimingPropertiesEXT'::@pPresentationTimings@ for which
-- @reportComplete@ is 'Vulkan.Core10.FundamentalTypes.TRUE', are made
-- available for future
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' calls. Elements of
-- @pPresentationTimings@ are arranged in ascending order of present ids.
--
-- Timing information /may/ become available out of order with regards to
-- their associated 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR'
-- order. 'PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT'
-- /can/ be set in 'PastPresentationTimingInfoEXT'::@flags@ to allow
-- 'getPastPresentationTimingEXT' to return results in that same order.
-- Otherwise, results are returned in the order of their associated
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' calls.
--
-- There is no requirement for any precise timing relationship between the
-- completion of a present stage and the availability of any associated
-- timing information. However, results /must/ be made available in finite
-- time.
--
-- As an exception to the normal rules for objects which are externally
-- synchronized, @swapchain@ /may/ be simultaneously used by other threads
-- in calls to functions other than
-- 'Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR' and
-- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR' with @swapchain@
-- used as an @oldSwapchain@. Access to the swapchain timing information
-- /must/ be atomic within the implementation.
--
-- == Valid Usage
--
-- -   #VUID-vkGetPastPresentationTimingEXT-flags-12230# If
--     'PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT' is set
--     in 'PastPresentationTimingInfoEXT'::@flags@, the @presentStageCount@
--     value of each element of
--     'PastPresentationTimingPropertiesEXT'::@pPresentationTimings@ /must/
--     be at least the maximum number of present stages set in
--     'PresentTimingInfoEXT'::@presentStageQueries@ among all
--     'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' calls, with a
--     non-zero @presentStageQueries@, for which complete results have not
--     been returned yet by a previous call
--
-- -   #VUID-vkGetPastPresentationTimingEXT-flags-12231# If
--     'PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT' is not
--     set in 'PastPresentationTimingInfoEXT'::@flags@, the
--     @presentStageCount@ value of each element of
--     'PastPresentationTimingPropertiesEXT'::@pPresentationTimings@ /must/
--     be at least the number of present stages set in
--     'PresentTimingInfoEXT'::@presentStageQueries@ for the earliest call
--     to 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR', with a
--     non-zero @presentStageQueries@, that corresponds to that element’s
--     index and for which complete results have not been returned yet by a
--     previous call
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetPastPresentationTimingEXT-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetPastPresentationTimingEXT-pPastPresentationTimingInfo-parameter#
--     @pPastPresentationTimingInfo@ /must/ be a valid pointer to a valid
--     'PastPresentationTimingInfoEXT' structure
--
-- -   #VUID-vkGetPastPresentationTimingEXT-pPastPresentationTimingProperties-parameter#
--     @pPastPresentationTimingProperties@ /must/ be a valid pointer to a
--     'PastPresentationTimingPropertiesEXT' structure
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.INCOMPLETE'
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DATE_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_SURFACE_LOST_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_UNKNOWN'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_VALIDATION_FAILED'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.Handles.Device', 'PastPresentationTimingInfoEXT',
-- 'PastPresentationTimingPropertiesEXT'
getPastPresentationTimingEXT :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the device associated with @swapchain@.
                                Device
                             -> -- | @pPastPresentationTimingInfo@ is a pointer to an instance of the
                                -- 'PastPresentationTimingInfoEXT' structure.
                                PastPresentationTimingInfoEXT
                             -> io (Result, PastPresentationTimingPropertiesEXT)
getPastPresentationTimingEXT device
                               pastPresentationTimingInfo = liftIO . evalContT $ do
  let vkGetPastPresentationTimingEXTPtr = pVkGetPastPresentationTimingEXT (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetPastPresentationTimingEXTPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetPastPresentationTimingEXT is null" Nothing Nothing
  let vkGetPastPresentationTimingEXT' = mkVkGetPastPresentationTimingEXT vkGetPastPresentationTimingEXTPtr
  pPastPresentationTimingInfo <- ContT $ withCStruct (pastPresentationTimingInfo)
  pPPastPresentationTimingProperties <- ContT (withZeroCStruct @PastPresentationTimingPropertiesEXT)
  r <- lift $ traceAroundEvent "vkGetPastPresentationTimingEXT" (vkGetPastPresentationTimingEXT'
                                                                   (deviceHandle (device))
                                                                   pPastPresentationTimingInfo
                                                                   (pPPastPresentationTimingProperties))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPastPresentationTimingProperties <- lift $ peekCStruct @PastPresentationTimingPropertiesEXT pPPastPresentationTimingProperties
  pure $ (r, pPastPresentationTimingProperties)


-- | VkPhysicalDevicePresentTimingFeaturesEXT - Structure indicating support
-- for present timing
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDevicePresentTimingFeaturesEXT' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. If the application wishes to use a
-- 'Vulkan.Core10.Handles.Device' with any features described by
-- 'PhysicalDevicePresentTimingFeaturesEXT', it /must/ add an instance of
-- the structure, with the desired feature members set to
-- 'Vulkan.Core10.FundamentalTypes.TRUE', to the @pNext@ chain of
-- 'Vulkan.Core10.Device.DeviceCreateInfo' when creating the
-- 'Vulkan.Core10.Handles.Device'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDevicePresentTimingFeaturesEXT = PhysicalDevicePresentTimingFeaturesEXT
  { -- | #features-presentTiming# @presentTiming@ indicates that the
    -- implementation supports 'getPastPresentationTimingEXT'.
    presentTiming :: Bool
  , -- | #features-presentAtAbsoluteTime# @presentAtAbsoluteTime@ indicates that
    -- the implementation supports specifying absolute target present times.
    presentAtAbsoluteTime :: Bool
  , -- | #features-presentAtRelativeTime# @presentAtRelativeTime@ indicates that
    -- the implementation supports specifying relative target present times.
    presentAtRelativeTime :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDevicePresentTimingFeaturesEXT)
#endif
deriving instance Show PhysicalDevicePresentTimingFeaturesEXT

instance ToCStruct PhysicalDevicePresentTimingFeaturesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDevicePresentTimingFeaturesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_TIMING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentTiming))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (presentAtAbsoluteTime))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (presentAtRelativeTime))
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PRESENT_TIMING_FEATURES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDevicePresentTimingFeaturesEXT where
  peekCStruct p = do
    presentTiming <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    presentAtAbsoluteTime <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    presentAtRelativeTime <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    pure $ PhysicalDevicePresentTimingFeaturesEXT
             (bool32ToBool presentTiming)
             (bool32ToBool presentAtAbsoluteTime)
             (bool32ToBool presentAtRelativeTime)

instance Storable PhysicalDevicePresentTimingFeaturesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDevicePresentTimingFeaturesEXT where
  zero = PhysicalDevicePresentTimingFeaturesEXT
           zero
           zero
           zero


-- | VkPresentTimingSurfaceCapabilitiesEXT - Structure describing present
-- timing capabilities of a surface
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'PresentStageFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentTimingSurfaceCapabilitiesEXT = PresentTimingSurfaceCapabilitiesEXT
  { -- | @presentTimingSupported@ indicates whether querying presentation
    -- timestamps is supported for a swapchain created from
    -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@.
    presentTimingSupported :: Bool
  , -- | @presentAtAbsoluteTimeSupported@ indicates whether a swapchain created
    -- from
    -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@
    -- supports presenting images with absolute times.
    presentAtAbsoluteTimeSupported :: Bool
  , -- | @presentAtRelativeTimeSupported@ indicates whether a swapchain created
    -- from
    -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@
    -- supports presenting images with relative times.
    presentAtRelativeTimeSupported :: Bool
  , -- | @presentStageQueries@ is a bitmask of 'PresentStageFlagBitsEXT'
    -- indicating which present stages a swapchain created from
    -- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR'::@surface@
    -- is able to provide timing information for.
    presentStageQueries :: PresentStageFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentTimingSurfaceCapabilitiesEXT)
#endif
deriving instance Show PresentTimingSurfaceCapabilitiesEXT

instance ToCStruct PresentTimingSurfaceCapabilitiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentTimingSurfaceCapabilitiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (presentTimingSupported))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (presentAtAbsoluteTimeSupported))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (presentAtRelativeTimeSupported))
    poke ((p `plusPtr` 28 :: Ptr PresentStageFlagsEXT)) (presentStageQueries)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMING_SURFACE_CAPABILITIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr PresentStageFlagsEXT)) (zero)
    f

instance FromCStruct PresentTimingSurfaceCapabilitiesEXT where
  peekCStruct p = do
    presentTimingSupported <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    presentAtAbsoluteTimeSupported <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    presentAtRelativeTimeSupported <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    presentStageQueries <- peek @PresentStageFlagsEXT ((p `plusPtr` 28 :: Ptr PresentStageFlagsEXT))
    pure $ PresentTimingSurfaceCapabilitiesEXT
             (bool32ToBool presentTimingSupported)
             (bool32ToBool presentAtAbsoluteTimeSupported)
             (bool32ToBool presentAtRelativeTimeSupported)
             presentStageQueries

instance Storable PresentTimingSurfaceCapabilitiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PresentTimingSurfaceCapabilitiesEXT where
  zero = PresentTimingSurfaceCapabilitiesEXT
           zero
           zero
           zero
           zero


-- | VkSwapchainTimingPropertiesEXT - Structure containing the RC duration of
-- a display
--
-- = Description
--
-- If @refreshDuration@ is zero, the presentation engine is not able to
-- determine the duration of the refresh cycle. Similarly, if
-- @refreshInterval@ is zero, the presentation engine is not able to
-- determine whether it is operating in VRR mode.
--
-- Otherwise, if @refreshInterval@ is the same as @refreshDuration@, the
-- presentation engine is operating in FRR mode. In this case,
-- @refreshDuration@ is the number of nanoseconds from the start of one
-- refresh cycle to the start of the next refresh cycle.
--
-- If @refreshInterval@ is @UINT64_MAX@, the presentation engine is
-- operating in VRR mode, and @refreshDuration@ is the minimum number of
-- nanoseconds from the start of one refresh cycle to the start of the next
-- refresh cycle.
--
-- If @refreshDuration@ and @refreshInterval@ are not zero,
-- @refreshInterval@ is a factor of @refreshDuration@.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getSwapchainTimingPropertiesEXT'
data SwapchainTimingPropertiesEXT = SwapchainTimingPropertiesEXT
  { -- | @refreshDuration@ is zero or an indication of the duration of a refresh
    -- cycle.
    refreshDuration :: Word64
  , -- | @refreshInterval@ is zero or a duration in nanoseconds indicating the
    -- interval between refresh cycle durations.
    refreshInterval :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainTimingPropertiesEXT)
#endif
deriving instance Show SwapchainTimingPropertiesEXT

instance ToCStruct SwapchainTimingPropertiesEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainTimingPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_TIMING_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (refreshDuration)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (refreshInterval)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_TIMING_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    f

instance FromCStruct SwapchainTimingPropertiesEXT where
  peekCStruct p = do
    refreshDuration <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    refreshInterval <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    pure $ SwapchainTimingPropertiesEXT
             refreshDuration refreshInterval

instance Storable SwapchainTimingPropertiesEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainTimingPropertiesEXT where
  zero = SwapchainTimingPropertiesEXT
           zero
           zero


-- | VkSwapchainTimeDomainPropertiesEXT - List of available time domains for
-- a swapchain
--
-- = Description
--
-- When calling 'getSwapchainTimeDomainPropertiesEXT', if @pTimeDomains@ is
-- @NULL@ and @pTimeDomainIds@ is @NULL@, then the number of time domains
-- supported for the given @swapchain@ is returned in @timeDomainCount@.
-- Otherwise, @timeDomainCount@ /must/ specify the number of elements in
-- @pTimeDomains@ and @pTimeDomainIds@, and on return is overwritten with
-- the number of values actually written to each array.
--
-- Due to the dynamic nature of their underlying
-- 'Vulkan.Extensions.Handles.SurfaceKHR' properties, swapchains may need
-- to expose multiple swapchain-local opaque time domains using the same
-- 'TimeDomainKHR' value over time, for example when a surface is moved
-- from one display hardware to another. Arbitrary identifiers, provided in
-- @timeDomainIds@, are used by the implementation to differentiate opaque
-- time domains of identical scopes.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainTimeDomainPropertiesEXT-pTimeDomains-12370#
--     @pTimeDomains@ and @pTimeDomainIds@ /must/ both be @NULL@ or both
--     not be @NULL@
--
-- -   #VUID-VkSwapchainTimeDomainPropertiesEXT-pTimeDomains-12371# If
--     @pTimeDomains@ and @pTimeDomainIds@ are not @NULL@, then
--     @timeDomainCount@ /must/ not be zero
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainTimeDomainPropertiesEXT-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT'
--
-- -   #VUID-VkSwapchainTimeDomainPropertiesEXT-pNext-pNext# @pNext@ /must/
--     be @NULL@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'TimeDomainKHR',
-- 'getSwapchainTimeDomainPropertiesEXT'
data SwapchainTimeDomainPropertiesEXT = SwapchainTimeDomainPropertiesEXT
  { -- | @timeDomainCount@ is an integer related to the number of time domains
    -- available or queried, as described below.
    timeDomainCount :: Word32
  , -- | @pTimeDomains@ is a pointer to an array of 'TimeDomainKHR' values
    -- representing time domains that are available for the swapchain.
    timeDomains :: Ptr TimeDomainKHR
  , -- | @pTimeDomainIds@ is a pointer to an array of unique identifiers for each
    -- time domain.
    timeDomainIds :: Ptr Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainTimeDomainPropertiesEXT)
#endif
deriving instance Show SwapchainTimeDomainPropertiesEXT

instance ToCStruct SwapchainTimeDomainPropertiesEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainTimeDomainPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (timeDomainCount)
    poke ((p `plusPtr` 24 :: Ptr (Ptr TimeDomainKHR))) (timeDomains)
    poke ((p `plusPtr` 32 :: Ptr (Ptr Word64))) (timeDomainIds)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_TIME_DOMAIN_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    f

instance FromCStruct SwapchainTimeDomainPropertiesEXT where
  peekCStruct p = do
    timeDomainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pTimeDomains <- peek @(Ptr TimeDomainKHR) ((p `plusPtr` 24 :: Ptr (Ptr TimeDomainKHR)))
    pTimeDomainIds <- peek @(Ptr Word64) ((p `plusPtr` 32 :: Ptr (Ptr Word64)))
    pure $ SwapchainTimeDomainPropertiesEXT
             timeDomainCount pTimeDomains pTimeDomainIds

instance Storable SwapchainTimeDomainPropertiesEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainTimeDomainPropertiesEXT where
  zero = SwapchainTimeDomainPropertiesEXT
           zero
           zero
           zero


-- | VkPresentStageTimeEXT - Associate a present stage with a timestamp
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PastPresentationTimingEXT', 'PresentStageFlagsEXT'
data PresentStageTimeEXT = PresentStageTimeEXT
  { -- | @stage@ is a 'PresentStageFlagsEXT' value specifying a present stage.
    stage :: PresentStageFlagsEXT
  , -- | @time@ is a time in nanoseconds associated with the @stage@.
    time :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentStageTimeEXT)
#endif
deriving instance Show PresentStageTimeEXT

instance ToCStruct PresentStageTimeEXT where
  withCStruct x f = allocaBytes 16 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentStageTimeEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr PresentStageFlagsEXT)) (stage)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (time)
    f
  cStructSize = 16
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr PresentStageFlagsEXT)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word64)) (zero)
    f

instance FromCStruct PresentStageTimeEXT where
  peekCStruct p = do
    stage <- peek @PresentStageFlagsEXT ((p `plusPtr` 0 :: Ptr PresentStageFlagsEXT))
    time <- peek @Word64 ((p `plusPtr` 8 :: Ptr Word64))
    pure $ PresentStageTimeEXT
             stage time

instance Storable PresentStageTimeEXT where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PresentStageTimeEXT where
  zero = PresentStageTimeEXT
           zero
           zero


-- | VkPastPresentationTimingInfoEXT - Structure specifying swapchain present
-- timing query parameters
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPastPresentationTimingInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT'
--
-- -   #VUID-VkPastPresentationTimingInfoEXT-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkPastPresentationTimingInfoEXT-flags-parameter# @flags@
--     /must/ be a valid combination of 'PastPresentationTimingFlagBitsEXT'
--     values
--
-- -   #VUID-VkPastPresentationTimingInfoEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PastPresentationTimingFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR', 'getPastPresentationTimingEXT'
data PastPresentationTimingInfoEXT = PastPresentationTimingInfoEXT
  { -- | @flags@ is a bitmask of 'PastPresentationTimingFlagBitsEXT' specifying
    -- options for queries of past presentation timing information.
    flags :: PastPresentationTimingFlagsEXT
  , -- | @swapchain@ is the swapchain to obtain presentation timing information
    -- for.
    swapchain :: SwapchainKHR
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PastPresentationTimingInfoEXT)
#endif
deriving instance Show PastPresentationTimingInfoEXT

instance ToCStruct PastPresentationTimingInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PastPresentationTimingInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PastPresentationTimingFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr SwapchainKHR)) (swapchain)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr SwapchainKHR)) (zero)
    f

instance FromCStruct PastPresentationTimingInfoEXT where
  peekCStruct p = do
    flags <- peek @PastPresentationTimingFlagsEXT ((p `plusPtr` 16 :: Ptr PastPresentationTimingFlagsEXT))
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 24 :: Ptr SwapchainKHR))
    pure $ PastPresentationTimingInfoEXT
             flags swapchain

instance Storable PastPresentationTimingInfoEXT where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PastPresentationTimingInfoEXT where
  zero = PastPresentationTimingInfoEXT
           zero
           zero


-- | VkPastPresentationTimingPropertiesEXT - Structure containing details
-- about a swapchain past presentation activity
--
-- = Description
--
-- When calling 'getPastPresentationTimingEXT', if @pPresentationTimings@
-- is @NULL@, then the number of available timing records for the given
-- @swapchain@ is returned in @presentationTimingCount@. Otherwise,
-- @presentationTimingCount@ /must/ specify the number of elements in the
-- @pPresentationTimings@ array, and on return is overwritten with the
-- number of structures actually written to @pPresentationTimings@.
--
-- if 'PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT' is specified
-- in 'PastPresentationTimingInfoEXT'::@flags@,
-- 'getPastPresentationTimingEXT' /may/ return incomplete results,
-- containing only information for a subset of the requested present
-- stages. Further calls to 'getPastPresentationTimingEXT' will keep
-- providing all available results for a previously incomplete entry until
-- it is complete.
--
-- The implementation /must/ return a 'PastPresentationTimingEXT' for every
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' referencing
-- @swapchain@ where a non-zero
-- 'PresentTimingInfoEXT'::@presentStageQueries@ was specified and at least
-- one present stage has available results.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PastPresentationTimingEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getPastPresentationTimingEXT'
data PastPresentationTimingPropertiesEXT = PastPresentationTimingPropertiesEXT
  { -- | @timingPropertiesCounter@ is a 64-bit unsigned integer set by the
    -- implementation to the current value of the swapchain’s internal timing
    -- properties counter.
    timingPropertiesCounter :: Word64
  , -- | @timeDomainsCounter@ is a 64-bit unsigned integer set by the
    -- implementation to the current value of the swapchain’s internal time
    -- domains list counter.
    timeDomainsCounter :: Word64
  , -- | @presentationTimingCount@ is an integer related to the number of
    -- 'PastPresentationTimingEXT' structures available or queried, as
    -- described below.
    presentationTimingCount :: Word32
  , -- | @pPresentationTimings@ is @NULL@ or a pointer to an array of
    -- 'PastPresentationTimingEXT' structures.
    presentationTimings :: Ptr PastPresentationTimingEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PastPresentationTimingPropertiesEXT)
#endif
deriving instance Show PastPresentationTimingPropertiesEXT

instance ToCStruct PastPresentationTimingPropertiesEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PastPresentationTimingPropertiesEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (timingPropertiesCounter)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (timeDomainsCounter)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (presentationTimingCount)
    poke ((p `plusPtr` 40 :: Ptr (Ptr PastPresentationTimingEXT))) (presentationTimings)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_PROPERTIES_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr (Ptr PastPresentationTimingEXT))) (zero)
    f

instance FromCStruct PastPresentationTimingPropertiesEXT where
  peekCStruct p = do
    timingPropertiesCounter <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    timeDomainsCounter <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    presentationTimingCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPresentationTimings <- peek @(Ptr PastPresentationTimingEXT) ((p `plusPtr` 40 :: Ptr (Ptr PastPresentationTimingEXT)))
    pure $ PastPresentationTimingPropertiesEXT
             timingPropertiesCounter
             timeDomainsCounter
             presentationTimingCount
             pPresentationTimings

instance Storable PastPresentationTimingPropertiesEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PastPresentationTimingPropertiesEXT where
  zero = PastPresentationTimingPropertiesEXT
           zero
           zero
           zero
           zero


-- | VkPastPresentationTimingEXT - Structure containing timing information
-- about a previously-presented image
--
-- = Description
--
-- When calling 'getPastPresentationTimingEXT', the implementation sets
-- @presentStageCount@ to the number of present stages it has written
-- results for. If 'PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT'
-- was specified in 'PastPresentationTimingInfoEXT'::@flags@, the
-- implementation /may/ return an incomplete report containing fewer
-- present stage results than were queried by the associated presentation
-- request. Otherwise, results for all the present stages queried by the
-- presentation request are written by the implementation.
--
-- Timing information for some present stages /may/ have a time value of 0,
-- indicating that results for that present stage are not available.
--
-- For systems with multiple entities operating within the presentation
-- engine, such as multiple displays, @pPresentStages@ will return timing
-- results for one entity which has been affected by the presentation.
--
-- @timeDomainId@ /may/ be different than the time domain that was
-- specified in 'PresentTimingInfoEXT'::@timeDomainId@ if the requirements
-- for using this time domain could not be met at the time the presentation
-- engine processed the presentation request. In such a case, the
-- presentation engine /may/ pick a time domain to fall back to, if one is
-- available, and report results in that domain. Applications /can/
-- continue to use this fallback time domain in future
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' calls, or they
-- /can/ call 'getSwapchainTimeDomainPropertiesEXT' to choose from the
-- currently supported time domains.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'PastPresentationTimingPropertiesEXT', 'PresentStageTimeEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType', 'TimeDomainKHR'
data PastPresentationTimingEXT = PastPresentationTimingEXT
  { -- | @presentId@ is zero or a value that was given to a previous
    -- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' command via
    -- 'Vulkan.Extensions.VK_KHR_present_id2.PresentId2KHR'::@pPresentIds@.
    presentId :: Word64
  , -- | @targetTime@ is the application-provided target absolute time or
    -- duration of the associated presentation request in
    -- 'PresentTimingInfoEXT'::@targetTime@.
    targetTime :: Word64
  , -- | @presentStageCount@ is a count of items contained in @pPresentStages@.
    presentStageCount :: Word32
  , -- | @pPresentStages@ a pointer to an array of 'PresentStageTimeEXT'
    -- providing timing information for the presentation request associated
    -- with @presentId@.
    presentStages :: Ptr PresentStageTimeEXT
  , -- | @timeDomain@ is the time domain used by the presentation engine to
    -- report times in @pPresentStages@.
    timeDomain :: TimeDomainKHR
  , -- | @timeDomainId@ is the id associated with @timeDomain@.
    timeDomainId :: Word64
  , -- | @reportComplete@ is 'Vulkan.Core10.FundamentalTypes.TRUE' if the
    -- presentation engine has reported all the requested results in
    -- @pPresentStages@.
    reportComplete :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PastPresentationTimingEXT)
#endif
deriving instance Show PastPresentationTimingEXT

instance ToCStruct PastPresentationTimingEXT where
  withCStruct x f = allocaBytes 72 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PastPresentationTimingEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (presentId)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (targetTime)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (presentStageCount)
    poke ((p `plusPtr` 40 :: Ptr (Ptr PresentStageTimeEXT))) (presentStages)
    poke ((p `plusPtr` 48 :: Ptr TimeDomainKHR)) (timeDomain)
    poke ((p `plusPtr` 56 :: Ptr Word64)) (timeDomainId)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (reportComplete))
    f
  cStructSize = 72
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PAST_PRESENTATION_TIMING_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr (Ptr PresentStageTimeEXT))) (zero)
    poke ((p `plusPtr` 48 :: Ptr TimeDomainKHR)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 64 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PastPresentationTimingEXT where
  peekCStruct p = do
    presentId <- peek @Word64 ((p `plusPtr` 16 :: Ptr Word64))
    targetTime <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    presentStageCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pPresentStages <- peek @(Ptr PresentStageTimeEXT) ((p `plusPtr` 40 :: Ptr (Ptr PresentStageTimeEXT)))
    timeDomain <- peek @TimeDomainKHR ((p `plusPtr` 48 :: Ptr TimeDomainKHR))
    timeDomainId <- peek @Word64 ((p `plusPtr` 56 :: Ptr Word64))
    reportComplete <- peek @Bool32 ((p `plusPtr` 64 :: Ptr Bool32))
    pure $ PastPresentationTimingEXT
             presentId
             targetTime
             presentStageCount
             pPresentStages
             timeDomain
             timeDomainId
             (bool32ToBool reportComplete)

instance Storable PastPresentationTimingEXT where
  sizeOf ~_ = 72
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PastPresentationTimingEXT where
  zero = PastPresentationTimingEXT
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkPresentTimingsInfoEXT - Array of VkPresentTimingInfoEXT to chain with
-- VkPresentInfoKHR
--
-- == Valid Usage
--
-- -   #VUID-VkPresentTimingsInfoEXT-swapchainCount-12233# @swapchainCount@
--     /must/ be equal to
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@swapchainCount@
--
-- -   #VUID-VkPresentTimingsInfoEXT-pSwapchains-12234# All swapchains in
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pSwapchains@
--     /must/ have been created with the
--     'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR'::@flags@
--     field containing
--     'Vulkan.Extensions.VK_KHR_swapchain.SWAPCHAIN_CREATE_PRESENT_TIMING_BIT_EXT'
--
-- -   #VUID-VkPresentTimingsInfoEXT-pSwapchains-12235# For each member of
--     'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pSwapchains@,
--     if the associated 'PresentTimingInfoEXT'::@targetTime@ is not zero,
--     the swapchain’s current present mode /must/ be
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_LATEST_READY_KHR',
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_KHR' or
--     'Vulkan.Extensions.VK_KHR_surface.PRESENT_MODE_FIFO_RELAXED_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentTimingsInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT'
--
-- -   #VUID-VkPresentTimingsInfoEXT-pTimingInfos-parameter# If
--     @pTimingInfos@ is not @NULL@, @pTimingInfos@ /must/ be a valid
--     pointer to an array of @swapchainCount@ valid 'PresentTimingInfoEXT'
--     structures
--
-- -   #VUID-VkPresentTimingsInfoEXT-swapchainCount-arraylength#
--     @swapchainCount@ /must/ be greater than @0@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PresentTimingInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentTimingsInfoEXT = PresentTimingsInfoEXT
  { -- | @swapchainCount@ is the number of swapchains being presented to by this
    -- command.
    swapchainCount :: Word32
  , -- | @pTimingInfos@ is @NULL@ or a pointer to an array of
    -- 'PresentTimingInfoEXT' elements with @swapchainCount@ entries. If not
    -- @NULL@, each element of @pTimingInfos@ contains timing information for
    -- the presentation of the image corresponding to the entry in the
    -- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pImageIndices@
    -- array.
    timingInfos :: Vector PresentTimingInfoEXT
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentTimingsInfoEXT)
#endif
deriving instance Show PresentTimingsInfoEXT

instance ToCStruct PresentTimingsInfoEXT where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentTimingsInfoEXT{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    let pTimingInfosLength = Data.Vector.length $ (timingInfos)
    swapchainCount'' <- lift $ if (swapchainCount) == 0
      then pure $ fromIntegral pTimingInfosLength
      else do
        unless (fromIntegral pTimingInfosLength == (swapchainCount) || pTimingInfosLength == 0) $
          throwIO $ IOError Nothing InvalidArgument "" "pTimingInfos must be empty or have 'swapchainCount' elements" Nothing Nothing
        pure (swapchainCount)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (swapchainCount'')
    pTimingInfos'' <- if Data.Vector.null (timingInfos)
      then pure nullPtr
      else do
        pPTimingInfos <- ContT $ allocaBytes @PresentTimingInfoEXT (((Data.Vector.length (timingInfos))) * 48)
        lift $ Data.Vector.imapM_ (\i e -> poke (pPTimingInfos `plusPtr` (48 * (i)) :: Ptr PresentTimingInfoEXT) (e)) ((timingInfos))
        pure $ pPTimingInfos
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr PresentTimingInfoEXT))) pTimingInfos''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMINGS_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct PresentTimingsInfoEXT where
  peekCStruct p = do
    swapchainCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pTimingInfos <- peek @(Ptr PresentTimingInfoEXT) ((p `plusPtr` 24 :: Ptr (Ptr PresentTimingInfoEXT)))
    let pTimingInfosLength = if pTimingInfos == nullPtr then 0 else (fromIntegral swapchainCount)
    pTimingInfos' <- generateM pTimingInfosLength (\i -> peekCStruct @PresentTimingInfoEXT ((pTimingInfos `advancePtrBytes` (48 * (i)) :: Ptr PresentTimingInfoEXT)))
    pure $ PresentTimingsInfoEXT
             swapchainCount pTimingInfos'

instance Zero PresentTimingsInfoEXT where
  zero = PresentTimingsInfoEXT
           zero
           mempty


-- | VkPresentTimingInfoEXT - Specifies per-present timing information
--
-- = Description
--
-- If @targetTime@ is not zero, the implementation attempts to align the
-- 'PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT' present stage of that
-- presentation request with the time specified in @targetTime@ according
-- to the time domain used. If
-- 'PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT' is not
-- set in @flags@, it indicates that the application would strictly prefer
-- the image to not be visible before @targetTime@ has lapsed.
--
-- If @targetTime@ is not zero and @timeDomainId@ is associated with a
-- 'TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT' time domain,
-- @targetTimeDomainPresentStage@ is used to specify which present stage’s
-- time domain @targetTime@ is specified for. Otherwise,
-- @targetTimeDomainPresentStage@ is ignored.
--
-- Some platforms, due to hardware or system limitations, /may/ not be able
-- to accurately time @targetTime@ with the actual physical event of the
-- image becoming visible on the display. However, those timing
-- capabilities /may/ still be useful and result in improved animation
-- quality.
--
-- As such, the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-presentAtAbsoluteTime presentAtAbsoluteTime>
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-presentAtRelativeTime presentAtRelativeTime>
-- features do not provide a strict guarantee regarding the completion of
-- the 'PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT' present stage
-- relative to the @targetTime@, and implementations /must/ strive to make
-- it as consistent and accurate as possible.
--
-- Applications that specify an absolute present time /should/ regularly
-- rebase their calculations for their next target time on the feedback
-- from 'getPastPresentationTimingEXT' to compensate for accumulated
-- precision errors or potential clock drift. It is recommended that when
-- targeting the time of a vertical blanking period, applications set
-- 'PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT' to allow
-- the implementation to compensate for small precision errors that may
-- cause an image to be displayed one refresh cycle later than intended.
--
-- == Valid Usage
--
-- -   #VUID-VkPresentTimingInfoEXT-targetTime-12236# If @targetTime@ is
--     not zero and @flags@ does not contain
--     'PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT', the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-presentAtAbsoluteTime presentAtAbsoluteTime>
--     feature /must/ be enabled and the @presentAtAbsoluteTimeSupported@
--     member of the 'PresentTimingSurfaceCapabilitiesEXT' returned by
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     for the surface associated with the swapchain /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPresentTimingInfoEXT-targetTime-12237# If @targetTime@ is
--     not zero and @flags@ contains
--     'PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT', the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-presentAtRelativeTime presentAtRelativeTime>
--     feature /must/ be enabled and the @presentAtRelativeTimeSupported@
--     member of the 'PresentTimingSurfaceCapabilitiesEXT' returned by
--     'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR'
--     for the surface associated with the swapchain /must/ be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- -   #VUID-VkPresentTimingInfoEXT-timeDomainId-12238# If @timeDomainId@
--     is associated with a 'TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT' time
--     domain, and @targetTime@ is not zero, @targetTimeDomainPresentStage@
--     /must/ be a single 'PresentStageFlagsEXT' value
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPresentTimingInfoEXT-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT'
--
-- -   #VUID-VkPresentTimingInfoEXT-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkPresentTimingInfoEXT-flags-parameter# @flags@ /must/ be a
--     valid combination of 'PresentTimingInfoFlagBitsEXT' values
--
-- -   #VUID-VkPresentTimingInfoEXT-presentStageQueries-parameter#
--     @presentStageQueries@ /must/ be a valid combination of
--     'PresentStageFlagBitsEXT' values
--
-- -   #VUID-VkPresentTimingInfoEXT-targetTimeDomainPresentStage-parameter#
--     @targetTimeDomainPresentStage@ /must/ be a valid combination of
--     'PresentStageFlagBitsEXT' values
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PresentStageFlagsEXT', 'PresentTimingInfoFlagsEXT',
-- 'PresentTimingsInfoEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PresentTimingInfoEXT = PresentTimingInfoEXT
  { -- | @flags@ is a bitmask of 'PresentTimingInfoFlagBitsEXT' specifying
    -- options for how to interpret the timing information.
    flags :: PresentTimingInfoFlagsEXT
  , -- | @targetTime@ is zero or a value specifying the target present time or
    -- duration, in nanoseconds, of the presentation request.
    targetTime :: Word64
  , -- | @timeDomainId@ is the id of the time domain used to specify the absolute
    -- target present time and the timing results obtained in a subsequent
    -- 'getPastPresentationTimingEXT' call for the current presentation
    -- request.
    timeDomainId :: Word64
  , -- | @presentStageQueries@ is a valid 'PresentStageFlagsEXT' value indicating
    -- which present stages the presentation engine will collect timing
    -- information for.
    presentStageQueries :: PresentStageFlagsEXT
  , -- | @targetTimeDomainPresentStage@ is a valid 'PresentStageFlagsEXT'
    -- specifying a single present stage used to interpret @targetTime@.
    targetTimeDomainPresentStage :: PresentStageFlagsEXT
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PresentTimingInfoEXT)
#endif
deriving instance Show PresentTimingInfoEXT

instance ToCStruct PresentTimingInfoEXT where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PresentTimingInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PresentTimingInfoFlagsEXT)) (flags)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (targetTime)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (timeDomainId)
    poke ((p `plusPtr` 40 :: Ptr PresentStageFlagsEXT)) (presentStageQueries)
    poke ((p `plusPtr` 44 :: Ptr PresentStageFlagsEXT)) (targetTimeDomainPresentStage)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PRESENT_TIMING_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    f

instance FromCStruct PresentTimingInfoEXT where
  peekCStruct p = do
    flags <- peek @PresentTimingInfoFlagsEXT ((p `plusPtr` 16 :: Ptr PresentTimingInfoFlagsEXT))
    targetTime <- peek @Word64 ((p `plusPtr` 24 :: Ptr Word64))
    timeDomainId <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    presentStageQueries <- peek @PresentStageFlagsEXT ((p `plusPtr` 40 :: Ptr PresentStageFlagsEXT))
    targetTimeDomainPresentStage <- peek @PresentStageFlagsEXT ((p `plusPtr` 44 :: Ptr PresentStageFlagsEXT))
    pure $ PresentTimingInfoEXT
             flags
             targetTime
             timeDomainId
             presentStageQueries
             targetTimeDomainPresentStage

instance Storable PresentTimingInfoEXT where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PresentTimingInfoEXT where
  zero = PresentTimingInfoEXT
           zero
           zero
           zero
           zero
           zero


-- | VkSwapchainCalibratedTimestampInfoEXT - Structure specifying the
-- swapchain to calibrate a swapchain-local timestamp query
--
-- = Description
--
-- @timeDomainId@ /must/ be an id previously reported by
-- 'getSwapchainTimeDomainPropertiesEXT' for @swapchain@. If the
-- @timeDomainId@ is no longer supported by the @swapchain@,
-- implementations /may/ report zero as the calibrated timestamp value.
--
-- == Valid Usage
--
-- -   #VUID-VkSwapchainCalibratedTimestampInfoEXT-timeDomain-12228# If the
--     @timeDomain@ member of the
--     'Vulkan.Extensions.VK_KHR_calibrated_timestamps.CalibratedTimestampInfoKHR'
--     structure in this structure’s @pNext@ chain is
--     'TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT', @presentStage@ /must/ specify
--     one and only one present stage
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSwapchainCalibratedTimestampInfoEXT-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT'
--
-- -   #VUID-VkSwapchainCalibratedTimestampInfoEXT-swapchain-parameter#
--     @swapchain@ /must/ be a valid
--     'Vulkan.Extensions.Handles.SwapchainKHR' handle
--
-- -   #VUID-VkSwapchainCalibratedTimestampInfoEXT-presentStage-parameter#
--     @presentStage@ /must/ be a valid combination of
--     'PresentStageFlagBitsEXT' values
--
-- -   #VUID-VkSwapchainCalibratedTimestampInfoEXT-presentStage-requiredbitmask#
--     @presentStage@ /must/ not be @0@
--
-- == Host Synchronization
--
-- -   Host access to @swapchain@ /must/ be externally synchronized
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PresentStageFlagsEXT',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.Handles.SwapchainKHR'
data SwapchainCalibratedTimestampInfoEXT = SwapchainCalibratedTimestampInfoEXT
  { -- | @swapchain@ is the swapchain to retrieve the swapchain-local timestamp
    -- from.
    swapchain :: SwapchainKHR
  , -- | @presentStage@ is zero or a 'PresentStageFlagsEXT' value used to
    -- identify a single present stage when calibrating a timestamp in the
    -- 'TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT' time domain.
    presentStage :: PresentStageFlagsEXT
  , -- | @timeDomainId@ is the id for the opaque time domain being calibrated.
    timeDomainId :: Word64
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SwapchainCalibratedTimestampInfoEXT)
#endif
deriving instance Show SwapchainCalibratedTimestampInfoEXT

instance ToCStruct SwapchainCalibratedTimestampInfoEXT where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SwapchainCalibratedTimestampInfoEXT{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (swapchain)
    poke ((p `plusPtr` 24 :: Ptr PresentStageFlagsEXT)) (presentStage)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (timeDomainId)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SWAPCHAIN_CALIBRATED_TIMESTAMP_INFO_EXT)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr SwapchainKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr PresentStageFlagsEXT)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    f

instance FromCStruct SwapchainCalibratedTimestampInfoEXT where
  peekCStruct p = do
    swapchain <- peek @SwapchainKHR ((p `plusPtr` 16 :: Ptr SwapchainKHR))
    presentStage <- peek @PresentStageFlagsEXT ((p `plusPtr` 24 :: Ptr PresentStageFlagsEXT))
    timeDomainId <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    pure $ SwapchainCalibratedTimestampInfoEXT
             swapchain presentStage timeDomainId

instance Storable SwapchainCalibratedTimestampInfoEXT where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero SwapchainCalibratedTimestampInfoEXT where
  zero = SwapchainCalibratedTimestampInfoEXT
           zero
           zero
           zero


-- | VkTimeDomainKHR - Supported time domains
--
-- = Description
--
-- -   'TIME_DOMAIN_DEVICE_KHR' specifies the device time domain. Timestamp
--     values in this time domain use the same units and are comparable
--     with device timestamp values captured using
--     'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' or
--     'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2'
--     and are defined to be incrementing according to the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-timestampPeriod timestampPeriod>
--     of the device.
--
-- -   'TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT' specifies a time domain unique
--     to a particular swapchain and a specific present stage. Timestamp
--     values in this time domain are in units of nanosecond and are
--     comparable only with other values from the same swapchain and
--     present stage.
--
-- -   'TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT' specifies a time domain unique to
--     a particular swapchain. Timestamp values in this time domain are in
--     units of nanosecond and are comparable only with other values from
--     the same swapchain.
--
-- -   'TIME_DOMAIN_CLOCK_MONOTONIC_KHR' specifies the CLOCK_MONOTONIC time
--     domain available on POSIX platforms. Timestamp values in this time
--     domain are in units of nanoseconds and are comparable with platform
--     timestamp values captured using the POSIX clock_gettime API as
--     computed by this example:
--
-- An implementation supporting @VK_KHR_calibrated_timestamps@ or
-- @VK_EXT_calibrated_timestamps@ will use the same time domain for all its
-- 'Vulkan.Core10.Handles.Queue' so that timestamp values reported for
-- 'TIME_DOMAIN_DEVICE_KHR' can be matched to any timestamp captured
-- through 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp' or
-- 'Vulkan.Core13.Promoted_From_VK_KHR_synchronization2.cmdWriteTimestamp2'
-- .
--
-- > struct timespec tv;
-- > clock_gettime(CLOCK_MONOTONIC, &tv);
-- > return tv.tv_nsec + tv.tv_sec*1000000000ull;
--
-- -   'TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR' specifies the
--     CLOCK_MONOTONIC_RAW time domain available on POSIX platforms.
--     Timestamp values in this time domain are in units of nanoseconds and
--     are comparable with platform timestamp values captured using the
--     POSIX clock_gettime API as computed by this example:
--
-- > struct timespec tv;
-- > clock_gettime(CLOCK_MONOTONIC_RAW, &tv);
-- > return tv.tv_nsec + tv.tv_sec*1000000000ull;
--
-- -   'TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR' specifies the
--     performance counter (QPC) time domain available on Windows.
--     Timestamp values in this time domain are in the same units as those
--     provided by the Windows QueryPerformanceCounter API and are
--     comparable with platform timestamp values captured using that API as
--     computed by this example:
--
-- > LARGE_INTEGER counter;
-- > QueryPerformanceCounter(&counter);
-- > return counter.QuadPart;
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_calibrated_timestamps VK_EXT_calibrated_timestamps>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_calibrated_timestamps VK_KHR_calibrated_timestamps>,
-- 'Vulkan.Extensions.VK_KHR_calibrated_timestamps.CalibratedTimestampInfoKHR',
-- 'PastPresentationTimingEXT', 'SwapchainTimeDomainPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_calibrated_timestamps.getPhysicalDeviceCalibrateableTimeDomainsKHR',
-- 'Vulkan.Extensions.VK_KHR_calibrated_timestamps.getPhysicalDeviceCalibrateableTimeDomainsKHR'
newtype TimeDomainKHR = TimeDomainKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkTimeDomainKHR" "VK_TIME_DOMAIN_DEVICE_KHR"
pattern TIME_DOMAIN_DEVICE_KHR = TimeDomainKHR 0

-- No documentation found for Nested "VkTimeDomainKHR" "VK_TIME_DOMAIN_CLOCK_MONOTONIC_KHR"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_KHR = TimeDomainKHR 1

-- No documentation found for Nested "VkTimeDomainKHR" "VK_TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR"
pattern TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR = TimeDomainKHR 2

-- No documentation found for Nested "VkTimeDomainKHR" "VK_TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR"
pattern TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR = TimeDomainKHR 3

-- No documentation found for Nested "VkTimeDomainKHR" "VK_TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT"
pattern TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT = TimeDomainKHR 1000208001

-- No documentation found for Nested "VkTimeDomainKHR" "VK_TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT"
pattern TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT = TimeDomainKHR 1000208000

{-# COMPLETE
  TIME_DOMAIN_DEVICE_KHR
  , TIME_DOMAIN_CLOCK_MONOTONIC_KHR
  , TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR
  , TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR
  , TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT
  , TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT ::
    TimeDomainKHR
  #-}

conNameTimeDomainKHR :: String
conNameTimeDomainKHR = "TimeDomainKHR"

enumPrefixTimeDomainKHR :: String
enumPrefixTimeDomainKHR = "TIME_DOMAIN_"

showTableTimeDomainKHR :: [(TimeDomainKHR, String)]
showTableTimeDomainKHR =
  [ (TIME_DOMAIN_DEVICE_KHR, "DEVICE_KHR")
  ,
    ( TIME_DOMAIN_CLOCK_MONOTONIC_KHR
    , "CLOCK_MONOTONIC_KHR"
    )
  ,
    ( TIME_DOMAIN_CLOCK_MONOTONIC_RAW_KHR
    , "CLOCK_MONOTONIC_RAW_KHR"
    )
  ,
    ( TIME_DOMAIN_QUERY_PERFORMANCE_COUNTER_KHR
    , "QUERY_PERFORMANCE_COUNTER_KHR"
    )
  ,
    ( TIME_DOMAIN_SWAPCHAIN_LOCAL_EXT
    , "SWAPCHAIN_LOCAL_EXT"
    )
  ,
    ( TIME_DOMAIN_PRESENT_STAGE_LOCAL_EXT
    , "PRESENT_STAGE_LOCAL_EXT"
    )
  ]

instance Show TimeDomainKHR where
  showsPrec =
    enumShowsPrec
      enumPrefixTimeDomainKHR
      showTableTimeDomainKHR
      conNameTimeDomainKHR
      (\(TimeDomainKHR x) -> x)
      (showsPrec 11)

instance Read TimeDomainKHR where
  readPrec =
    enumReadPrec
      enumPrefixTimeDomainKHR
      showTableTimeDomainKHR
      conNameTimeDomainKHR
      TimeDomainKHR

type PresentStageFlagsEXT = PresentStageFlagBitsEXT

-- | VkPresentStageFlagBitsEXT - Bitmask specifying stages of the image
-- presentation process
--
-- = Description
--
-- -   'PRESENT_STAGE_QUEUE_OPERATIONS_END_BIT_EXT' marks the end of the
--     set of queue operations enqueued by
--     'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR' on the provided
--     'Vulkan.Core10.Handles.Queue' for a presentation request.
--
-- -   'PRESENT_STAGE_REQUEST_DEQUEUED_BIT_EXT' is the stage after which
--     the presentation request has been dequeued from the swapchain’s
--     internal presentation request queue, if any, as specified by the
--     present mode associated with that request.
--
-- -   'PRESENT_STAGE_IMAGE_FIRST_PIXEL_OUT_BIT_EXT' is the stage after
--     which data for the first pixel of the presentation request
--     associated with the image has left the presentation engine for a
--     display hardware.
--
-- -   'PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT' is the stage after
--     which a display hardware has made the first pixel visible for the
--     presentation request associated with the image to be presented.
--
-- The set of queue operations delimited by
-- 'PRESENT_STAGE_QUEUE_OPERATIONS_END_BIT_EXT' includes the wait for the
-- semaphores specified in
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR'::@pWaitSemaphores@,
-- if any, and any work implicitly enqueued by the implementation.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PresentStageFlagsEXT'
newtype PresentStageFlagBitsEXT = PresentStageFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPresentStageFlagBitsEXT" "VK_PRESENT_STAGE_QUEUE_OPERATIONS_END_BIT_EXT"
pattern PRESENT_STAGE_QUEUE_OPERATIONS_END_BIT_EXT = PresentStageFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkPresentStageFlagBitsEXT" "VK_PRESENT_STAGE_REQUEST_DEQUEUED_BIT_EXT"
pattern PRESENT_STAGE_REQUEST_DEQUEUED_BIT_EXT = PresentStageFlagBitsEXT 0x00000002

-- No documentation found for Nested "VkPresentStageFlagBitsEXT" "VK_PRESENT_STAGE_IMAGE_FIRST_PIXEL_OUT_BIT_EXT"
pattern PRESENT_STAGE_IMAGE_FIRST_PIXEL_OUT_BIT_EXT = PresentStageFlagBitsEXT 0x00000004

-- No documentation found for Nested "VkPresentStageFlagBitsEXT" "VK_PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT"
pattern PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT = PresentStageFlagBitsEXT 0x00000008

conNamePresentStageFlagBitsEXT :: String
conNamePresentStageFlagBitsEXT = "PresentStageFlagBitsEXT"

enumPrefixPresentStageFlagBitsEXT :: String
enumPrefixPresentStageFlagBitsEXT = "PRESENT_STAGE_"

showTablePresentStageFlagBitsEXT :: [(PresentStageFlagBitsEXT, String)]
showTablePresentStageFlagBitsEXT =
  [
    ( PRESENT_STAGE_QUEUE_OPERATIONS_END_BIT_EXT
    , "QUEUE_OPERATIONS_END_BIT_EXT"
    )
  ,
    ( PRESENT_STAGE_REQUEST_DEQUEUED_BIT_EXT
    , "REQUEST_DEQUEUED_BIT_EXT"
    )
  ,
    ( PRESENT_STAGE_IMAGE_FIRST_PIXEL_OUT_BIT_EXT
    , "IMAGE_FIRST_PIXEL_OUT_BIT_EXT"
    )
  ,
    ( PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT
    , "IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT"
    )
  ]

instance Show PresentStageFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPresentStageFlagBitsEXT
      showTablePresentStageFlagBitsEXT
      conNamePresentStageFlagBitsEXT
      (\(PresentStageFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PresentStageFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixPresentStageFlagBitsEXT
      showTablePresentStageFlagBitsEXT
      conNamePresentStageFlagBitsEXT
      PresentStageFlagBitsEXT

type PastPresentationTimingFlagsEXT = PastPresentationTimingFlagBitsEXT

-- | VkPastPresentationTimingFlagBitsEXT - Bitmask specifying past
-- presentation timing query flags
--
-- = Description
--
-- -   'PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT' specifies
--     that 'getPastPresentationTimingEXT' /may/ return partial results for
--     presentation requests that have not completed all requested present
--     stages.
--
-- -   'PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT'
--     specifies that 'getPastPresentationTimingEXT' /may/ return results
--     out of order with respect to the presentation order.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PastPresentationTimingFlagsEXT'
newtype PastPresentationTimingFlagBitsEXT = PastPresentationTimingFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPastPresentationTimingFlagBitsEXT" "VK_PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT"
pattern PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT = PastPresentationTimingFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkPastPresentationTimingFlagBitsEXT" "VK_PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT"
pattern PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT = PastPresentationTimingFlagBitsEXT 0x00000002

conNamePastPresentationTimingFlagBitsEXT :: String
conNamePastPresentationTimingFlagBitsEXT = "PastPresentationTimingFlagBitsEXT"

enumPrefixPastPresentationTimingFlagBitsEXT :: String
enumPrefixPastPresentationTimingFlagBitsEXT = "PAST_PRESENTATION_TIMING_ALLOW_"

showTablePastPresentationTimingFlagBitsEXT :: [(PastPresentationTimingFlagBitsEXT, String)]
showTablePastPresentationTimingFlagBitsEXT =
  [
    ( PAST_PRESENTATION_TIMING_ALLOW_PARTIAL_RESULTS_BIT_EXT
    , "PARTIAL_RESULTS_BIT_EXT"
    )
  ,
    ( PAST_PRESENTATION_TIMING_ALLOW_OUT_OF_ORDER_RESULTS_BIT_EXT
    , "OUT_OF_ORDER_RESULTS_BIT_EXT"
    )
  ]

instance Show PastPresentationTimingFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPastPresentationTimingFlagBitsEXT
      showTablePastPresentationTimingFlagBitsEXT
      conNamePastPresentationTimingFlagBitsEXT
      (\(PastPresentationTimingFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PastPresentationTimingFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixPastPresentationTimingFlagBitsEXT
      showTablePastPresentationTimingFlagBitsEXT
      conNamePastPresentationTimingFlagBitsEXT
      PastPresentationTimingFlagBitsEXT

type PresentTimingInfoFlagsEXT = PresentTimingInfoFlagBitsEXT

-- | VkPresentTimingInfoFlagBitsEXT - Bitmask specifying present timing info
-- flags
--
-- = Description
--
-- -   'PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT' specifies
--     that 'PresentTimingInfoEXT'::@targetTime@ is to be interpreted as a
--     relative time from the previous presentation’s
--     'PRESENT_STAGE_IMAGE_FIRST_PIXEL_VISIBLE_BIT_EXT' stage. If the
--     @swapchain@ has never been used to present an image, the provided
--     @targetTime@ is ignored.
--
-- -   'PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT'
--     specifies that the application would prefer the image to be
--     presented earlier than the time specified in
--     'PresentTimingInfoEXT'::@targetTime@ if that time falls within the
--     first half of a refresh cycle. In that case, the presentation engine
--     /may/ choose to display the image at the start of that refresh
--     cycle.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_present_timing VK_EXT_present_timing>,
-- 'PresentTimingInfoFlagsEXT'
newtype PresentTimingInfoFlagBitsEXT = PresentTimingInfoFlagBitsEXT Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPresentTimingInfoFlagBitsEXT" "VK_PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT"
pattern PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT = PresentTimingInfoFlagBitsEXT 0x00000001

-- No documentation found for Nested "VkPresentTimingInfoFlagBitsEXT" "VK_PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT"
pattern PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT = PresentTimingInfoFlagBitsEXT 0x00000002

conNamePresentTimingInfoFlagBitsEXT :: String
conNamePresentTimingInfoFlagBitsEXT = "PresentTimingInfoFlagBitsEXT"

enumPrefixPresentTimingInfoFlagBitsEXT :: String
enumPrefixPresentTimingInfoFlagBitsEXT = "PRESENT_TIMING_INFO_PRESENT_AT_"

showTablePresentTimingInfoFlagBitsEXT :: [(PresentTimingInfoFlagBitsEXT, String)]
showTablePresentTimingInfoFlagBitsEXT =
  [
    ( PRESENT_TIMING_INFO_PRESENT_AT_RELATIVE_TIME_BIT_EXT
    , "RELATIVE_TIME_BIT_EXT"
    )
  ,
    ( PRESENT_TIMING_INFO_PRESENT_AT_NEAREST_REFRESH_CYCLE_BIT_EXT
    , "NEAREST_REFRESH_CYCLE_BIT_EXT"
    )
  ]

instance Show PresentTimingInfoFlagBitsEXT where
  showsPrec =
    enumShowsPrec
      enumPrefixPresentTimingInfoFlagBitsEXT
      showTablePresentTimingInfoFlagBitsEXT
      conNamePresentTimingInfoFlagBitsEXT
      (\(PresentTimingInfoFlagBitsEXT x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PresentTimingInfoFlagBitsEXT where
  readPrec =
    enumReadPrec
      enumPrefixPresentTimingInfoFlagBitsEXT
      showTablePresentTimingInfoFlagBitsEXT
      conNamePresentTimingInfoFlagBitsEXT
      PresentTimingInfoFlagBitsEXT

type EXT_PRESENT_TIMING_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_EXT_PRESENT_TIMING_SPEC_VERSION"
pattern EXT_PRESENT_TIMING_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_PRESENT_TIMING_SPEC_VERSION = 3


type EXT_PRESENT_TIMING_EXTENSION_NAME = "VK_EXT_present_timing"

-- No documentation found for TopLevel "VK_EXT_PRESENT_TIMING_EXTENSION_NAME"
pattern EXT_PRESENT_TIMING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_PRESENT_TIMING_EXTENSION_NAME = "VK_EXT_present_timing"

