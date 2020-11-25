{-# language CPP #-}
-- | = Name
--
-- VK_KHR_device_group_creation - instance extension
--
-- = Registered Extension Number
--
-- 71
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- = Deprecation state
--
-- -   /Promoted/ to
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1-promotions Vulkan 1.1>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2016-10-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.1 Core
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension provides instance-level commands to enumerate groups of
-- physical devices, and to create a logical device from a subset of one of
-- those groups. Such a logical device can then be used with new features
-- in the @VK_KHR_device_group@ extension.
--
-- == Promotion to Vulkan 1.1
--
-- All functionality in this extension is included in core Vulkan 1.1, with
-- the KHR suffix omitted. The original type, enum and command names are
-- still available as aliases of the core functionality.
--
-- == New Commands
--
-- -   'enumeratePhysicalDeviceGroupsKHR'
--
-- == New Structures
--
-- -   'PhysicalDeviceGroupPropertiesKHR'
--
-- -   Extending 'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'DeviceGroupDeviceCreateInfoKHR'
--
-- == New Enum Constants
--
-- -   'KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME'
--
-- -   'KHR_DEVICE_GROUP_CREATION_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.MAX_DEVICE_GROUP_SIZE_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MemoryHeapFlagBits':
--
--     -   'MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR'
--
-- == Examples
--
-- >     VkDeviceCreateInfo devCreateInfo = { VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO };
-- >     // (not shown) fill out devCreateInfo as usual.
-- >     uint32_t deviceGroupCount = 0;
-- >     VkPhysicalDeviceGroupPropertiesKHR *props = NULL;
-- >
-- >     // Query the number of device groups
-- >     vkEnumeratePhysicalDeviceGroupsKHR(g_vkInstance, &deviceGroupCount, NULL);
-- >
-- >     // Allocate and initialize structures to query the device groups
-- >     props = (VkPhysicalDeviceGroupPropertiesKHR *)malloc(deviceGroupCount*sizeof(VkPhysicalDeviceGroupPropertiesKHR));
-- >     for (i = 0; i < deviceGroupCount; ++i) {
-- >         props[i].sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR;
-- >         props[i].pNext = NULL;
-- >     }
-- >     vkEnumeratePhysicalDeviceGroupsKHR(g_vkInstance, &deviceGroupCount, props);
-- >
-- >     // If the first device group has more than one physical device. create
-- >     // a logical device using all of the physical devices.
-- >     VkDeviceGroupDeviceCreateInfoKHR deviceGroupInfo = { VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR };
-- >     if (props[0].physicalDeviceCount > 1) {
-- >         deviceGroupInfo.physicalDeviceCount = props[0].physicalDeviceCount;
-- >         deviceGroupInfo.pPhysicalDevices = props[0].physicalDevices;
-- >         devCreateInfo.pNext = &deviceGroupInfo;
-- >     }
-- >
-- >     vkCreateDevice(props[0].physicalDevices[0], &devCreateInfo, NULL, &g_vkDevice);
-- >     free(props);
--
-- == Version History
--
-- -   Revision 1, 2016-10-19 (Jeff Bolz)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'Vulkan.Core10.APIConstants.MAX_DEVICE_GROUP_SIZE_KHR',
-- 'DeviceGroupDeviceCreateInfoKHR', 'PhysicalDeviceGroupPropertiesKHR',
-- 'enumeratePhysicalDeviceGroupsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group_creation Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

