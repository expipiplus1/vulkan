{-# language CPP #-}
-- | = Name
--
-- VK_KHR_global_priority - device extension
--
-- == VK_KHR_global_priority
--
-- [__Name String__]
--     @VK_KHR_global_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     189
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_global_priority] @tobski%0A*Here describe the issue or question you have about the VK_KHR_global_priority extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-10-22
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Contributors to @VK_EXT_global_priority@
--
--     -   Contributors to @VK_EXT_global_priority_query@
--
-- == Description
--
-- In Vulkan, users can specify device-scope queue priorities. In some
-- cases it may be useful to extend this concept to a system-wide scope.
-- This device extension allows applications to query the global queue
-- priorities supported by a queue family, and then set a priority when
-- creating queues. The default queue priority is
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
--
-- Implementations can report which global priority levels are treated
-- differently by the implementation. It is intended primarily for use in
-- system integration along with certain platform-specific priority
-- enforcement rules.
--
-- The driver implementation will attempt to skew hardware resource
-- allocation in favour of the higher-priority task. Therefore,
-- higher-priority work may retain similar latency and throughput
-- characteristics even if the system is congested with lower priority
-- work.
--
-- The global priority level of a queue shall take precedence over the
-- per-process queue priority
-- ('Vulkan.Core10.Device.DeviceQueueCreateInfo'::@pQueuePriorities@).
--
-- Abuse of this feature may result in starving the rest of the system from
-- hardware resources. Therefore, the driver implementation may deny
-- requests to acquire a priority above the default priority
-- ('QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT') if the caller does not have
-- sufficient privileges. In this scenario
-- 'Vulkan.Extensions.VK_EXT_global_priority.ERROR_NOT_PERMITTED_EXT' is
-- returned.
--
-- The driver implementation may fail the queue allocation request if
-- resources required to complete the operation have been exhausted (either
-- by the same process or a different process). In this scenario
-- 'Vulkan.Core10.Enums.Result.ERROR_INITIALIZATION_FAILED' is returned.
--
-- == New Structures
--
-- -   Extending 'Vulkan.Core10.Device.DeviceQueueCreateInfo':
--
--     -   'DeviceQueueGlobalPriorityCreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGlobalPriorityQueryFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyGlobalPriorityPropertiesKHR'
--
-- == New Enums
--
-- -   'QueueGlobalPriorityKHR'
--
-- == New Enum Constants
--
-- -   'KHR_GLOBAL_PRIORITY_EXTENSION_NAME'
--
-- -   'KHR_GLOBAL_PRIORITY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_PERMITTED_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR'
--
-- == Issues
--
-- 1) Can we additionally query whether a caller is permitted to acquire a
-- specific global queue priority in this extension?
--
-- __RESOLVED__: No. Whether a caller has enough privilege goes with the
-- OS, and the Vulkan driver cannot really guarantee that the privilege
-- will not change in between this query and the actual queue creation
-- call.
--
-- 2) If more than 1 queue using global priority is requested, is there a
-- good way to know which queue is failing the device creation?
--
-- __RESOLVED__: No. There is not a good way at this moment, and it is also
-- not quite actionable for the applications to know that because the
-- information may not be accurate. Queue creation can fail because of
-- runtime constraints like insufficient privilege or lack of resource, and
-- the failure is not necessarily tied to that particular queue
-- configuration requested.
--
-- == Version History
--
-- -   Revision 1, 2021-10-22 (Tobias Hector)
--
--     -   Initial draft
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_KHR',
-- 'DeviceQueueGlobalPriorityCreateInfoKHR',
-- 'PhysicalDeviceGlobalPriorityQueryFeaturesKHR',
-- 'QueueFamilyGlobalPriorityPropertiesKHR', 'QueueGlobalPriorityKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_global_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_global_priority  ( DeviceQueueGlobalPriorityCreateInfoKHR
                                                 , PhysicalDeviceGlobalPriorityQueryFeaturesKHR
                                                 , QueueFamilyGlobalPriorityPropertiesKHR
                                                 , QueueGlobalPriorityKHR
                                                 ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceQueueGlobalPriorityCreateInfoKHR

instance ToCStruct DeviceQueueGlobalPriorityCreateInfoKHR
instance Show DeviceQueueGlobalPriorityCreateInfoKHR

instance FromCStruct DeviceQueueGlobalPriorityCreateInfoKHR


data PhysicalDeviceGlobalPriorityQueryFeaturesKHR

instance ToCStruct PhysicalDeviceGlobalPriorityQueryFeaturesKHR
instance Show PhysicalDeviceGlobalPriorityQueryFeaturesKHR

instance FromCStruct PhysicalDeviceGlobalPriorityQueryFeaturesKHR


data QueueFamilyGlobalPriorityPropertiesKHR

instance ToCStruct QueueFamilyGlobalPriorityPropertiesKHR
instance Show QueueFamilyGlobalPriorityPropertiesKHR

instance FromCStruct QueueFamilyGlobalPriorityPropertiesKHR


data QueueGlobalPriorityKHR

