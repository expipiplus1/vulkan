{-# language CPP #-}
-- | = Name
--
-- VK_EXT_buffer_device_address - device extension
--
-- == VK_EXT_buffer_device_address
--
-- [__Name String__]
--     @VK_EXT_buffer_device_address@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     245
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_physical_storage_buffer.html SPV_EXT_physical_storage_buffer>
--
-- [__Deprecation State__]
--
--     -   /Deprecated/ by
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
--         extension
--
--         -   Which in turn was /promoted/ to
--             <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
--
-- [__Contact__]
--
--     -   Jeff Bolz
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_buffer_device_address] @jeffbolznv%0A*Here describe the issue or question you have about the VK_EXT_buffer_device_address extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-01-06
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference.txt GLSL_EXT_buffer_reference>
--         and
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference_uvec2.txt GLSL_EXT_buffer_reference_uvec2>
--
-- [__Contributors__]
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Neil Henning, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Faith Ekstrand, Intel
--
--     -   Baldur Karlsson, Valve
--
-- == Description
--
-- This extension allows the application to query a 64-bit buffer device
-- address value for a buffer, which can be used to access the buffer
-- memory via the @PhysicalStorageBufferEXT@ storage class in the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference.txt GL_EXT_buffer_reference>
-- GLSL extension and
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_physical_storage_buffer.html SPV_EXT_physical_storage_buffer>
-- SPIR-V extension.
--
-- It also allows buffer device addresses to be provided by a trace replay
-- tool, so that it matches the address used when the trace was captured.
--
-- == New Commands
--
-- -   'getBufferDeviceAddressEXT'
--
-- == New Structures
--
-- -   'BufferDeviceAddressInfoEXT'
--
-- -   Extending 'Vulkan.Core10.Buffer.BufferCreateInfo':
--
--     -   'BufferDeviceAddressCreateInfoEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceBufferAddressFeaturesEXT'
--
--     -   'PhysicalDeviceBufferDeviceAddressFeaturesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_BUFFER_DEVICE_ADDRESS_EXTENSION_NAME'
--
-- -   'EXT_BUFFER_DEVICE_ADDRESS_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BufferCreateFlagBits':
--
--     -   'BUFFER_CREATE_DEVICE_ADDRESS_CAPTURE_REPLAY_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'BUFFER_USAGE_SHADER_DEVICE_ADDRESS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.Result.Result':
--
--     -   'ERROR_INVALID_DEVICE_ADDRESS_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT'
--
--     -   'STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-PhysicalStorageBufferAddresses PhysicalStorageBufferAddressesEXT>
--
-- == Issues
--
-- 1) Where is
-- VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_ADDRESS_FEATURES_EXT and
-- VkPhysicalDeviceBufferAddressFeaturesEXT?
--
-- __RESOLVED__: They were renamed as
-- 'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT'
-- and 'PhysicalDeviceBufferDeviceAddressFeaturesEXT' accordingly for
-- consistency. Even though, the old names can still be found in the
-- generated header files for compatibility.
--
-- == Version History
--
-- -   Revision 1, 2018-11-01 (Jeff Bolz)
--
--     -   Internal revisions
--
-- -   Revision 2, 2019-01-06 (Jon Leech)
--
--     -   Minor updates to appendix for publication
--
-- == See Also
--
-- 'BufferDeviceAddressCreateInfoEXT', 'BufferDeviceAddressInfoEXT',
-- 'PhysicalDeviceBufferAddressFeaturesEXT',
-- 'PhysicalDeviceBufferDeviceAddressFeaturesEXT',
-- 'getBufferDeviceAddressEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_buffer_device_address Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_buffer_device_address  ( BufferDeviceAddressCreateInfoEXT
                                                       , PhysicalDeviceBufferDeviceAddressFeaturesEXT
                                                       ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BufferDeviceAddressCreateInfoEXT

instance ToCStruct BufferDeviceAddressCreateInfoEXT
instance Show BufferDeviceAddressCreateInfoEXT

instance FromCStruct BufferDeviceAddressCreateInfoEXT


data PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT
instance Show PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance FromCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT

