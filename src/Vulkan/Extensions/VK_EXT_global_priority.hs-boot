{-# language CPP #-}
-- | = Name
--
-- VK_EXT_global_priority - device extension
--
-- == VK_EXT_global_priority
--
-- [__Name String__]
--     @VK_EXT_global_priority@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     175
--
-- [__Revision__]
--     2
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
-- [__Contact__]
--
--     -   Andres Rodriguez
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_global_priority] @lostgoat%0A<<Here describe the issue or question you have about the VK_EXT_global_priority extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2017-10-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Andres Rodriguez, Valve
--
--     -   Pierre-Loup Griffais, Valve
--
--     -   Dan Ginsburg, Valve
--
--     -   Mitch Singer, AMD
--
-- == Description
--
-- In Vulkan, users can specify device-scope queue priorities. In some
-- cases it may be useful to extend this concept to a system-wide scope.
-- This extension provides a mechanism for callers to set their system-wide
-- priority. The default queue priority is
-- 'QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
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
-- 'Vulkan.Core10.Enums.Result.ERROR_NOT_PERMITTED_EXT' is returned.
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
--     -   'DeviceQueueGlobalPriorityCreateInfoEXT'
--
-- == New Enums
--
-- -   'QueueGlobalPriorityEXT'
--
-- == New Enum Constants
--
-- -   'EXT_GLOBAL_PRIORITY_EXTENSION_NAME'
--
-- -   'EXT_GLOBAL_PRIORITY_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_NOT_PERMITTED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT'
--
-- == Version History
--
-- -   Revision 2, 2017-11-03 (Andres Rodriguez)
--
--     -   Fixed VkQueueGlobalPriorityEXT missing _EXT suffix
--
-- -   Revision 1, 2017-10-06 (Andres Rodriguez)
--
--     -   First version.
--
-- == See Also
--
-- 'DeviceQueueGlobalPriorityCreateInfoEXT', 'QueueGlobalPriorityEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_global_priority  (DeviceQueueGlobalPriorityCreateInfoEXT) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceQueueGlobalPriorityCreateInfoEXT

instance ToCStruct DeviceQueueGlobalPriorityCreateInfoEXT
instance Show DeviceQueueGlobalPriorityCreateInfoEXT

instance FromCStruct DeviceQueueGlobalPriorityCreateInfoEXT

