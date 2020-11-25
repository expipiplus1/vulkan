{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_buffer_device_address"
module Vulkan.Extensions.VK_KHR_buffer_device_address  ( pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR
                                                       , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR
                                                       , pattern STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR
                                                       , pattern STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR
                                                       , pattern STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR
                                                       , pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR
                                                       , pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR
                                                       , pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR
                                                       , pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR
                                                       , pattern ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR
                                                       , getBufferOpaqueCaptureAddressKHR
                                                       , getBufferDeviceAddressKHR
                                                       , getDeviceMemoryOpaqueCaptureAddressKHR
                                                       , PhysicalDeviceBufferDeviceAddressFeaturesKHR
                                                       , BufferDeviceAddressInfoKHR
                                                       , BufferOpaqueCaptureAddressCreateInfoKHR
                                                       , MemoryOpaqueCaptureAddressAllocateInfoKHR
                                                       , DeviceMemoryOpaqueCaptureAddressInfoKHR
                                                       , KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
                                                       , pattern KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION
                                                       , KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                                                       , pattern KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (getBufferDeviceAddress)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (getBufferOpaqueCaptureAddress)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (getDeviceMemoryOpaqueCaptureAddress)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferDeviceAddressInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (BufferOpaqueCaptureAddressCreateInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (DeviceMemoryOpaqueCaptureAddressInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (MemoryOpaqueCaptureAddressAllocateInfo)
import Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address (PhysicalDeviceBufferDeviceAddressFeatures)
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlags)
import Vulkan.Core10.Enums.BufferCreateFlagBits (BufferCreateFlagBits(BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT))
import Vulkan.Core10.Enums.Result (Result(ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT))
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlags)
import Vulkan.Core11.Enums.MemoryAllocateFlagBits (MemoryAllocateFlagBits(MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES))
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR"
pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR = STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR = STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR = STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO


-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR = STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO


-- No documentation found for TopLevel "VK_BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR"
pattern BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR = BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT


-- No documentation found for TopLevel "VK_BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR"
pattern BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR = BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT


-- No documentation found for TopLevel "VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR"
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR = MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT


-- No documentation found for TopLevel "VK_MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR"
pattern MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR = MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT


-- No documentation found for TopLevel "VK_ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR"
pattern ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR = ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS


-- No documentation found for TopLevel "vkGetBufferOpaqueCaptureAddressKHR"
getBufferOpaqueCaptureAddressKHR = getBufferOpaqueCaptureAddress


-- No documentation found for TopLevel "vkGetBufferDeviceAddressKHR"
getBufferDeviceAddressKHR = getBufferDeviceAddress


-- No documentation found for TopLevel "vkGetDeviceMemoryOpaqueCaptureAddressKHR"
getDeviceMemoryOpaqueCaptureAddressKHR = getDeviceMemoryOpaqueCaptureAddress


-- No documentation found for TopLevel "VkPhysicalDeviceBufferDeviceAddressFeaturesKHR"
type PhysicalDeviceBufferDeviceAddressFeaturesKHR = PhysicalDeviceBufferDeviceAddressFeatures


-- No documentation found for TopLevel "VkBufferDeviceAddressInfoKHR"
type BufferDeviceAddressInfoKHR = BufferDeviceAddressInfo


-- No documentation found for TopLevel "VkBufferOpaqueCaptureAddressCreateInfoKHR"
type BufferOpaqueCaptureAddressCreateInfoKHR = BufferOpaqueCaptureAddressCreateInfo


-- No documentation found for TopLevel "VkMemoryOpaqueCaptureAddressAllocateInfoKHR"
type MemoryOpaqueCaptureAddressAllocateInfoKHR = MemoryOpaqueCaptureAddressAllocateInfo


-- No documentation found for TopLevel "VkDeviceMemoryOpaqueCaptureAddressInfoKHR"
type DeviceMemoryOpaqueCaptureAddressInfoKHR = DeviceMemoryOpaqueCaptureAddressInfo


type KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION"
pattern KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION = 1


type KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = "VK_KHR_buffer_device_address"

-- No documentation found for TopLevel "VK_KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME"
pattern KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME = "VK_KHR_buffer_device_address"

