{-# language CPP #-}
-- | = Name
--
-- VK_QCOM_queue_perf_hint - device extension
--
-- = VK_QCOM_queue_perf_hint
--
-- [__Name String__]
--     @VK_QCOM_queue_perf_hint@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     303
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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_QCOM_queue_perf_hint] @mnetsch%0A*Here describe the issue or question you have about the VK_QCOM_queue_perf_hint extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_QCOM_queue_perf_hint.adoc VK_QCOM_queue_perf_hint>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-02-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Interacts with @VK_KHR_internally_synchronized_queues@
--
-- [__Contributors__]
--
--     -   Matthew Netsch, Qualcomm Technologies, Inc
--
-- == Description
--
-- This extension exposes power constraints to the application, allowing it
-- to provide hints for influencing the device’s clock frequency.
--
-- These hints are 'Vulkan.Core10.Handles.Queue' state and are persistent
-- across the life of the queue until the app updates or removes the
-- constraint. The kernel combines the constraints across the active queues
-- from all processes to determine the actual clock frequency levels.
--
-- == New Commands
--
-- -   'queueSetPerfHintQCOM'
--
-- == New Structures
--
-- -   'PerfHintInfoQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceQueuePerfHintFeaturesQCOM'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceQueuePerfHintPropertiesQCOM'
--
-- == New Enums
--
-- -   'PerfHintTypeQCOM'
--
-- == New Enum Constants
--
-- -   'QCOM_QUEUE_PERF_HINT_EXTENSION_NAME'
--
-- -   'QCOM_QUEUE_PERF_HINT_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PERF_HINT_INFO_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_FEATURES_QCOM'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_QUEUE_PERF_HINT_PROPERTIES_QCOM'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2026-02-26 (Matthew Netsch)
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_QCOM_queue_perf_hint Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_QCOM_queue_perf_hint  ( PerfHintInfoQCOM
                                                  , PhysicalDeviceQueuePerfHintFeaturesQCOM
                                                  , PhysicalDeviceQueuePerfHintPropertiesQCOM
                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PerfHintInfoQCOM

instance ToCStruct PerfHintInfoQCOM
instance Show PerfHintInfoQCOM

instance FromCStruct PerfHintInfoQCOM


data PhysicalDeviceQueuePerfHintFeaturesQCOM

instance ToCStruct PhysicalDeviceQueuePerfHintFeaturesQCOM
instance Show PhysicalDeviceQueuePerfHintFeaturesQCOM

instance FromCStruct PhysicalDeviceQueuePerfHintFeaturesQCOM


data PhysicalDeviceQueuePerfHintPropertiesQCOM

instance ToCStruct PhysicalDeviceQueuePerfHintPropertiesQCOM
instance Show PhysicalDeviceQueuePerfHintPropertiesQCOM

instance FromCStruct PhysicalDeviceQueuePerfHintPropertiesQCOM

