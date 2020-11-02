{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_device_group_creation  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR
                                                       , pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR
                                                       , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR
                                                       , pattern MAX_DEVICE_GROUP_SIZE_KHR
                                                       , enumeratePhysicalDeviceGroupsKHR
                                                       , PhysicalDeviceGroupPropertiesKHR
                                                       , DeviceGroupDeviceCreateInfoKHR
                                                       , KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
                                                       , pattern KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
                                                       , KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
                                                       , pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (enumeratePhysicalDeviceGroups)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (DeviceGroupDeviceCreateInfo)
import Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation (PhysicalDeviceGroupProperties)
import Vulkan.Core10.APIConstants (pattern MAX_DEVICE_GROUP_SIZE)
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlags)
import Vulkan.Core10.Enums.MemoryHeapFlagBits (MemoryHeapFlagBits(MEMORY_HEAP_MULTI_INSTANCE_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO


-- No documentation found for TopLevel "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR"
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR = MEMORY_HEAP_MULTI_INSTANCE_BIT


-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE_KHR"
pattern MAX_DEVICE_GROUP_SIZE_KHR = MAX_DEVICE_GROUP_SIZE


-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceGroupsKHR"
enumeratePhysicalDeviceGroupsKHR = enumeratePhysicalDeviceGroups


-- No documentation found for TopLevel "VkPhysicalDeviceGroupPropertiesKHR"
type PhysicalDeviceGroupPropertiesKHR = PhysicalDeviceGroupProperties


-- No documentation found for TopLevel "VkDeviceGroupDeviceCreateInfoKHR"
type DeviceGroupDeviceCreateInfoKHR = DeviceGroupDeviceCreateInfo


type KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION"
pattern KHR_DEVICE_GROUP_CREATION_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1


type KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME = "VK_KHR_device_group_creation"

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME"
pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME = "VK_KHR_device_group_creation"

