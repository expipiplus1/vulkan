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
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__; __Deprecation State__]
--
--     -   /Promoted/ to @VK_KHR_global_priority@ extension
--
-- [__Contact__]
--
--     -   Andres Rodriguez
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_global_priority] @lostgoat%0A*Here describe the issue or question you have about the VK_EXT_global_priority extension* >
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
-- 'Vulkan.Extensions.VK_KHR_global_priority.QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT'.
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
-- ('Vulkan.Extensions.VK_KHR_global_priority.QUEUE_GLOBAL_PRIORITY_MEDIUM_EXT')
-- if the caller does not have sufficient privileges. In this scenario
-- 'ERROR_NOT_PERMITTED_EXT' is returned.
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
--     -   'ERROR_NOT_PERMITTED_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT'
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_global_priority Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_global_priority  ( pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
                                                 , pattern ERROR_NOT_PERMITTED_EXT
                                                 , QueueGlobalPriorityEXT
                                                 , DeviceQueueGlobalPriorityCreateInfoEXT
                                                 , EXT_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION
                                                 , EXT_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 , pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME
                                                 , DeviceQueueGlobalPriorityCreateInfoKHR(..)
                                                 , QueueGlobalPriorityKHR(..)
                                                 ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_global_priority (DeviceQueueGlobalPriorityCreateInfoKHR)
import Vulkan.Extensions.VK_KHR_global_priority (QueueGlobalPriorityKHR)
import Vulkan.Core10.Enums.Result (Result(ERROR_NOT_PERMITTED_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR))
import Vulkan.Extensions.VK_KHR_global_priority (DeviceQueueGlobalPriorityCreateInfoKHR(..))
import Vulkan.Extensions.VK_KHR_global_priority (QueueGlobalPriorityKHR(..))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT = STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_KHR


-- No documentation found for TopLevel "VK_ERROR_NOT_PERMITTED_EXT"
pattern ERROR_NOT_PERMITTED_EXT = ERROR_NOT_PERMITTED_KHR


-- No documentation found for TopLevel "VkQueueGlobalPriorityEXT"
type QueueGlobalPriorityEXT = QueueGlobalPriorityKHR


-- No documentation found for TopLevel "VkDeviceQueueGlobalPriorityCreateInfoEXT"
type DeviceQueueGlobalPriorityCreateInfoEXT = DeviceQueueGlobalPriorityCreateInfoKHR


type EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GLOBAL_PRIORITY_SPEC_VERSION = 2


type EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_EXTENSION_NAME = "VK_EXT_global_priority"

