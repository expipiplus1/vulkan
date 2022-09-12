{-# language CPP #-}
-- | = Name
--
-- VK_NV_external_memory_rdma - device extension
--
-- == VK_NV_external_memory_rdma
--
-- [__Name String__]
--     @VK_NV_external_memory_rdma@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     372
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_external_memory@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Carsten Rohde
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_external_memory_rdma] @crohde%0A*Here describe the issue or question you have about the VK_NV_external_memory_rdma extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2021-04-19
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Carsten Rohde, NVIDIA
--
-- == Description
--
-- This extension adds support for allocating memory which can be used for
-- remote direct memory access (RDMA) from other devices.
--
-- == New Base Types
--
-- -   'RemoteAddressNV'
--
-- == New Commands
--
-- -   'getMemoryRemoteAddressNV'
--
-- == New Structures
--
-- -   'MemoryGetRemoteAddressInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceExternalMemoryRDMAFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_EXTERNAL_MEMORY_RDMA_EXTENSION_NAME'
--
-- -   'NV_EXTERNAL_MEMORY_RDMA_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.ExternalMemoryHandleTypeFlagBits':
--
--     -   'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MemoryPropertyFlagBits':
--
--     -   'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_RDMA_FEATURES_NV'
--
-- == Examples
--
-- > VkPhysicalDeviceMemoryBudgetPropertiesEXT memoryBudgetProperties = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT };
-- > VkPhysicalDeviceMemoryProperties2 memoryProperties2 = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2, &memoryBudgetProperties };
-- > vkGetPhysicalDeviceMemoryProperties2(physicalDevice, &memoryProperties2);
-- > uint32_t heapIndex = (uint32_t)-1;
-- > for (uint32_t memoryType = 0; memoryType < memoryProperties2.memoryProperties.memoryTypeCount; memoryType++) {
-- >     if (memoryProperties2.memoryProperties.memoryTypes[memoryType].propertyFlags & VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV) {
-- >         heapIndex = memoryProperties2.memoryProperties.memoryTypes[memoryType].heapIndex;
-- >         break;
-- >     }
-- > }
-- > if ((heapIndex == (uint32_t)-1) ||
-- >     (memoryBudgetProperties.heapBudget[heapIndex] < size)) {
-- >     return;
-- > }
-- >
-- > VkPhysicalDeviceExternalBufferInfo externalBufferInfo = { VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO };
-- > externalBufferInfo.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
-- > externalBufferInfo.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > VkExternalBufferProperties externalBufferProperties = { VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES };
-- > vkGetPhysicalDeviceExternalBufferProperties(physicalDevice, &externalBufferInfo, &externalBufferProperties);
-- >
-- > if (!(externalBufferProperties.externalMemoryProperties.externalMemoryFeatures & VK_EXTERNAL_MEMORY_FEATURE_EXPORTABLE_BIT)) {
-- >     return;
-- > }
-- >
-- > VkExternalMemoryBufferCreateInfo externalMemoryBufferCreateInfo = { VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO };
-- > externalMemoryBufferCreateInfo.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > VkBufferCreateInfo bufferCreateInfo = { VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO, &externalMemoryBufferCreateInfo };
-- > bufferCreateInfo.size = size;
-- > bufferCreateInfo.usage = VK_BUFFER_USAGE_TRANSFER_SRC_BIT | VK_BUFFER_USAGE_TRANSFER_DST_BIT;
-- >
-- > VkMemoryRequirements mem_reqs;
-- > vkCreateBuffer(device, &bufferCreateInfo, NULL, &buffer);
-- > vkGetBufferMemoryRequirements(device, buffer, &mem_reqs);
-- >
-- > VkExportMemoryAllocateInfo exportMemoryAllocateInfo = { VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO };
-- > exportMemoryAllocateInfo.handleTypes = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > // Find memory type index
-- > uint32_t i = 0;
-- > for (; i < VK_MAX_MEMORY_TYPES; i++) {
-- >     if ((mem_reqs.memoryTypeBits & (1 << i)) &&
-- >         (memoryProperties.memoryTypes[i].propertyFlags & VK_MEMORY_PROPERTY_RDMA_CAPABLE_BIT_NV)) {
-- >         break;
-- >     }
-- > }
-- >
-- > VkMemoryAllocateInfo memAllocInfo = { VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO, &exportMemoryAllocateInfo };
-- > memAllocInfo.allocationSize = mem_reqs.size;
-- > memAllocInfo.memoryTypeIndex = i;
-- >
-- > vkAllocateMemory(device, &memAllocInfo, NULL, &mem);
-- > vkBindBufferMemory(device, buffer, mem, 0);
-- >
-- > VkMemoryGetRemoteAddressInfoNV getMemoryRemoteAddressInfo = { VK_STRUCTURE_TYPE_MEMORY_GET_REMOTE_ADDRESS_INFO_NV };
-- > getMemoryRemoteAddressInfo.memory = mem;
-- > getMemoryRemoteAddressInfo.handleType = VK_EXTERNAL_MEMORY_HANDLE_TYPE_RDMA_ADDRESS_BIT_NV;
-- >
-- > VkRemoteAddressNV rdmaAddress;
-- > vkGetMemoryRemoteAddressNV(device, &getMemoryRemoteAddressInfo, &rdmaAddress);
-- > // address returned in 'rdmaAddress' can be used by external devices to initiate RDMA transfers
--
-- == Version History
--
-- -   Revision 1, 2020-12-15 (Carsten Rohde)
--
--     -   Internal revisions
--
-- == See Also
--
-- 'MemoryGetRemoteAddressInfoNV',
-- 'PhysicalDeviceExternalMemoryRDMAFeaturesNV', 'RemoteAddressNV',
-- 'getMemoryRemoteAddressNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_external_memory_rdma Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_external_memory_rdma  ( MemoryGetRemoteAddressInfoNV
                                                     , PhysicalDeviceExternalMemoryRDMAFeaturesNV
                                                     , RemoteAddressNV
                                                     ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Foreign.Ptr (Ptr)
import Data.Kind (Type)

data MemoryGetRemoteAddressInfoNV

instance ToCStruct MemoryGetRemoteAddressInfoNV
instance Show MemoryGetRemoteAddressInfoNV

instance FromCStruct MemoryGetRemoteAddressInfoNV


data PhysicalDeviceExternalMemoryRDMAFeaturesNV

instance ToCStruct PhysicalDeviceExternalMemoryRDMAFeaturesNV
instance Show PhysicalDeviceExternalMemoryRDMAFeaturesNV

instance FromCStruct PhysicalDeviceExternalMemoryRDMAFeaturesNV


-- No documentation found for TopLevel "VkRemoteAddressNV"
type RemoteAddressNV = Ptr ()

