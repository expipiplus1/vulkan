{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_elapsed_timer_query - device extension
--
-- = VK_QCOM_elapsed_timer_query
--
-- [__Name String__]
--     @VK_QCOM_elapsed_timer_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     174
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__Contact__]
--
--     -   Matthew Netsch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_elapsed_timer_query] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_elapsed_timer_query extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_elapsed_timer_query.adoc VK_QCOM_elapsed_timer_query>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-05-08
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @apiExt:VK_KHR_calibrated_timestamps@
--
--     -   Interacts with @apiExt:VK_KHR_maintenance7@
--
--     -   Interacts with @apiExt:VK_EXT_transform_feedback@
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
--     -   Jacob Garcia, Qualcomm Technologies, Inc
--
--     -   Patrick Boyle, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension adds a new query type to write out the elapsed time
-- between a set of commands, providing similar functionality to
-- @GL_ARB_timer_query@.
--
-- There is no existing solution on tilers for measuring commands inside
-- render pass instances, as comparing query timestamps will not give valid
-- results.
--
-- This extension, solves the issue by adding a new query type that can be
-- used to measure time between a begin and end query and works inside
-- render pass instances. The implementation is responsible for
-- accumulating the correct elapsed time for the commands across all tiles.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceElapsedTimerQueryFeaturesQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_ELAPSED_TIMER_QUERY_EXTENSION_NAME'
--
-- -   'QCOM_ELAPSED_TIMER_QUERY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_TIME_ELAPSED_QCOM'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_ELAPSED_TIMER_QUERY_FEATURES_QCOM'
--
-- == Issues
--
-- (1) How to handle overflows?
--
-- -   Resolved: While
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryResultFlagBits VK_QUERY_RESULT_WITH_STATUS_BIT_KHR>
--     could have been used to indicate an overflow, it is difficult for an
--     implementation to detect this. The accumulation occurs on the
--     device, and overflow detection requires calculations prior to
--     accumulation which may not be supported by the device. Instead, if
--     the application rejects outliers as part of the profiling process,
--     this issue should be exceedingly rare. If an application is
--     sensitive to this in production, it will need to implement some
--     mechanism to reject bad results. For example, measuring the
--     timestamp before and after the render pass could give a clue about
--     an overflow.
--
-- == Version History
--
-- -   Revision 1, 2026-05-08 (Matthew Netsch)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_elapsed_timer_query Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_elapsed_timer_query  (PhysicalDeviceElapsedTimerQueryFeaturesQCOM) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceElapsedTimerQueryFeaturesQCOM

instance ToCStruct PhysicalDeviceElapsedTimerQueryFeaturesQCOM
instance Show PhysicalDeviceElapsedTimerQueryFeaturesQCOM

instance FromCStruct PhysicalDeviceElapsedTimerQueryFeaturesQCOM

