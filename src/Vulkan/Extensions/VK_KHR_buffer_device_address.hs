{-# language CPP #-}
-- | = Name
--
-- VK_KHR_buffer_device_address - device extension
--
-- == VK_KHR_buffer_device_address
--
-- [__Name String__]
--     @VK_KHR_buffer_device_address@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     258
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Deprecation state__]
--
--     -   /Promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_buffer_device_address:%20&body=@jeffbolznv%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-06-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Promoted to Vulkan 1.2 Core
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_physical_storage_buffer.html SPV_KHR_physical_storage_buffer>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference.txt GL_EXT_buffer_reference>
--         and
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference2.txt GL_EXT_buffer_reference2>
--         and
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference_uvec2.txt GL_EXT_buffer_reference_uvec2>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Neil Henning, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Jason Ekstrand, Intel
--
--     -   Baldur Karlsson, Valve
--
--     -   Jan-Harald Fredriksen, Arm
--
-- == Description
--
-- This extension allows the application to query a 64-bit buffer device
-- address value for a buffer, which can be used to access the buffer
-- memory via the @PhysicalStorageBuffer@ storage class in the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference.txt GL_EXT_buffer_reference>
-- GLSL extension and
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_physical_storage_buffer.html SPV_KHR_physical_storage_buffer>
-- SPIR-V extension.
--
-- Another way to describe this extension is that it adds \"pointers to
-- buffer memory in shaders\". By calling
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
-- with a 'Vulkan.Core10.Handles.Buffer', it will return a
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress' value which represents
-- the address of the start of the buffer.
--
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddress'
-- and
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddress'
-- allow opaque addresses for buffers and memory objects to be queried for
-- the current process. A trace capture and replay tool can then supply
-- these addresses to be used at replay time to match the addresses used
-- when the trace was captured. To enable tools to insert these queries,
-- new memory allocation flags must be specified for memory objects that
-- will be bound to buffers accessed via the @PhysicalStorageBuffer@
-- storage class. Note that this mechanism is intended only to support
-- capture\/replay tools, and is not recommended for use in other
-- applications.
--
-- There are various use cases this extension is designed for. It is
-- required for ray tracing, useful for DX12 portability, and by allowing
-- buffer addresses to be stored in memory it enables more complex data
-- structures to be created.
--
-- This extension can also be used to hardcode a dedicated debug channel
-- into all shaders by querying a pointer at startup and pushing that into
-- shaders as a run-time constant (e.g. specialization constant) that
-- avoids impacting other descriptor limits.
--
-- There are examples of usage in the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference.txt GL_EXT_buffer_reference>
-- spec for how to use this in a high-level shading language such as GLSL.
-- The
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference2.txt GL_EXT_buffer_reference2>
-- and
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference_uvec2.txt GL_EXT_buffer_reference_uvec2>
-- extensions were also added to help cover a few additional edge cases.
--
-- == Promotion to Vulkan 1.2
--
-- All functionality in this extension is included in core Vulkan 1.2, with
-- the KHR suffix omitted. However, if Vulkan 1.2 is supported and this
-- extension is not, the @bufferDeviceAddress@ capability is optional. The
-- original type, enum and command names are still available as aliases of
-- the core functionality.
--
-- == New Commands
--
-- -   'getBufferDeviceAddressKHR'
--
-- -   'getBufferOpaqueCaptureAddressKHR'
--
-- -   'getDeviceMemoryOpaqueCaptureAddressKHR'
--
-- == New Structures
--
-- -   'BufferDeviceAddressInfoKHR'
--
-- -   'DeviceMemoryOpaqueCaptureAddressInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo':
--
--     -   'BufferOpaqueCaptureAddressCreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Memory.MemoryAllocateInfo':
--
--     -   'MemoryOpaqueCaptureAddressAllocateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceBufferDeviceAddressFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME'
--
-- -   'KHR_BUFFER_DEVICE_ADDRESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits':
--
--     -   'BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core11.Enums.MemoryAllocateFlagBits.MemoryAllocateFlagBits':
--
--     -   'MEMORY_ALLOCATE_DEVICE_ADDRESS_BIT_KHR'
--
--     -   'MEMORY_ALLOCATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO_KHR'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-PhysicalStorageBufferAddresses PhysicalStorageBufferAddresses>
--
-- == Version History
--
-- -   Revision 1, 2019-06-24 (Jan-Harald Fredriksen)
--
--     -   Internal revisions based on VK_EXT_buffer_device_address
--
-- = See Also
--
-- 'BufferDeviceAddressInfoKHR', 'BufferOpaqueCaptureAddressCreateInfoKHR',
-- 'DeviceMemoryOpaqueCaptureAddressInfoKHR',
-- 'MemoryOpaqueCaptureAddressAllocateInfoKHR',
-- 'PhysicalDeviceBufferDeviceAddressFeaturesKHR',
-- 'getBufferDeviceAddressKHR', 'getBufferOpaqueCaptureAddressKHR',
-- 'getDeviceMemoryOpaqueCaptureAddressKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
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

