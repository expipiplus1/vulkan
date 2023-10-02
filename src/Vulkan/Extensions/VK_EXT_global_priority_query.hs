{-# language CPP #-}
-- | = Name
--
-- VK_EXT_global_priority_query - device extension
--
-- == VK_EXT_global_priority_query
--
-- [__Name String__]
--     @VK_EXT_global_priority_query@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     389
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_global_priority VK_EXT_global_priority>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to @VK_KHR_global_priority@ extension
--
-- [__Contact__]
--
--     -   Yiwei Zhang
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_global_priority_query] @zhangyiwei%0A*Here describe the issue or question you have about the VK_EXT_global_priority_query extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-03-29
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Yiwei Zhang, Google
--
-- == Description
--
-- This device extension allows applications to query the global queue
-- priorities supported by a queue family. It allows implementations to
-- report which global priority levels are treated differently by the
-- implementation, instead of silently mapping multiple requested global
-- priority levels to the same internal priority, or using device creation
-- failure to signal that a requested priority is not supported. It is
-- intended primarily for use by system integration along with certain
-- platform-specific priority enforcement rules.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceGlobalPriorityQueryFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2':
--
--     -   'QueueFamilyGlobalPriorityPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME'
--
-- -   'EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT'
--
--     -   'STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT'
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
-- -   Revision 1, 2021-03-29 (Yiwei Zhang)
--
-- == See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_GLOBAL_PRIORITY_SIZE_EXT',
-- 'PhysicalDeviceGlobalPriorityQueryFeaturesEXT',
-- 'QueueFamilyGlobalPriorityPropertiesEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_global_priority_query Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_global_priority_query  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT
                                                       , pattern STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT
                                                       , pattern MAX_GLOBAL_PRIORITY_SIZE_EXT
                                                       , PhysicalDeviceGlobalPriorityQueryFeaturesEXT
                                                       , QueueFamilyGlobalPriorityPropertiesEXT
                                                       , EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION
                                                       , pattern EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION
                                                       , EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME
                                                       , pattern EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME
                                                       , PhysicalDeviceGlobalPriorityQueryFeaturesKHR(..)
                                                       , QueueFamilyGlobalPriorityPropertiesKHR(..)
                                                       , QueueGlobalPriorityKHR(..)
                                                       , MAX_GLOBAL_PRIORITY_SIZE_KHR
                                                       , pattern MAX_GLOBAL_PRIORITY_SIZE_KHR
                                                       ) where

import Data.String (IsString)
import Vulkan.Extensions.VK_KHR_global_priority (PhysicalDeviceGlobalPriorityQueryFeaturesKHR)
import Vulkan.Extensions.VK_KHR_global_priority (QueueFamilyGlobalPriorityPropertiesKHR)
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE_KHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR))
import Vulkan.Core10.APIConstants (MAX_GLOBAL_PRIORITY_SIZE_KHR)
import Vulkan.Extensions.VK_KHR_global_priority (PhysicalDeviceGlobalPriorityQueryFeaturesKHR(..))
import Vulkan.Extensions.VK_KHR_global_priority (QueueFamilyGlobalPriorityPropertiesKHR(..))
import Vulkan.Extensions.VK_KHR_global_priority (QueueGlobalPriorityKHR(..))
import Vulkan.Core10.APIConstants (pattern MAX_GLOBAL_PRIORITY_SIZE_KHR)
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_EXT = STRUCTURE_TYPE_PHYSICAL_DEVICE_GLOBAL_PRIORITY_QUERY_FEATURES_KHR


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_EXT = STRUCTURE_TYPE_QUEUE_FAMILY_GLOBAL_PRIORITY_PROPERTIES_KHR


-- No documentation found for TopLevel "VK_MAX_GLOBAL_PRIORITY_SIZE_EXT"
pattern MAX_GLOBAL_PRIORITY_SIZE_EXT = MAX_GLOBAL_PRIORITY_SIZE_KHR


-- No documentation found for TopLevel "VkPhysicalDeviceGlobalPriorityQueryFeaturesEXT"
type PhysicalDeviceGlobalPriorityQueryFeaturesEXT = PhysicalDeviceGlobalPriorityQueryFeaturesKHR


-- No documentation found for TopLevel "VkQueueFamilyGlobalPriorityPropertiesEXT"
type QueueFamilyGlobalPriorityPropertiesEXT = QueueFamilyGlobalPriorityPropertiesKHR


type EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION"
pattern EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_GLOBAL_PRIORITY_QUERY_SPEC_VERSION = 1


type EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME = "VK_EXT_global_priority_query"

-- No documentation found for TopLevel "VK_EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME"
pattern EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_GLOBAL_PRIORITY_QUERY_EXTENSION_NAME = "VK_EXT_global_priority_query"

