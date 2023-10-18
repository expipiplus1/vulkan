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
module Vulkan.Extensions.VK_NV_low_latency2  ( GetLatencyMarkerInfoNV
                                             , LatencySleepInfoNV
                                             , LatencySleepModeInfoNV
                                             , LatencySubmissionPresentIdNV
                                             , LatencySurfaceCapabilitiesNV
                                             , LatencyTimingsFrameReportNV
                                             , OutOfBandQueueTypeInfoNV
                                             , SetLatencyMarkerInfoNV
                                             , SwapchainLatencyCreateInfoNV
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data GetLatencyMarkerInfoNV

instance ToCStruct GetLatencyMarkerInfoNV
instance Show GetLatencyMarkerInfoNV

instance FromCStruct GetLatencyMarkerInfoNV


data LatencySleepInfoNV

instance ToCStruct LatencySleepInfoNV
instance Show LatencySleepInfoNV

instance FromCStruct LatencySleepInfoNV


data LatencySleepModeInfoNV

instance ToCStruct LatencySleepModeInfoNV
instance Show LatencySleepModeInfoNV

instance FromCStruct LatencySleepModeInfoNV


data LatencySubmissionPresentIdNV

instance ToCStruct LatencySubmissionPresentIdNV
instance Show LatencySubmissionPresentIdNV

instance FromCStruct LatencySubmissionPresentIdNV


data LatencySurfaceCapabilitiesNV

instance ToCStruct LatencySurfaceCapabilitiesNV
instance Show LatencySurfaceCapabilitiesNV

instance FromCStruct LatencySurfaceCapabilitiesNV


data LatencyTimingsFrameReportNV

instance ToCStruct LatencyTimingsFrameReportNV
instance Show LatencyTimingsFrameReportNV

instance FromCStruct LatencyTimingsFrameReportNV


data OutOfBandQueueTypeInfoNV

instance ToCStruct OutOfBandQueueTypeInfoNV
instance Show OutOfBandQueueTypeInfoNV

instance FromCStruct OutOfBandQueueTypeInfoNV


data SetLatencyMarkerInfoNV

instance ToCStruct SetLatencyMarkerInfoNV
instance Show SetLatencyMarkerInfoNV

instance FromCStruct SetLatencyMarkerInfoNV


data SwapchainLatencyCreateInfoNV

instance ToCStruct SwapchainLatencyCreateInfoNV
instance Show SwapchainLatencyCreateInfoNV

instance FromCStruct SwapchainLatencyCreateInfoNV

