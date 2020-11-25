{-# language CPP #-}
-- | = Name
--
-- VK_EXT_buffer_device_address - device extension
--
-- = Registered Extension Number
--
-- 245
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires Vulkan 1.0
--
-- -   Requires @VK_KHR_get_physical_device_properties2@
--
-- = Deprecation state
--
-- -   /Deprecated/ by @VK_KHR_buffer_device_address@ extension
--
--     -   Which in turn was /promoted/ to
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2-promotions Vulkan 1.2>
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
--     -   This extension requires
--         {spirv}\/EXT\/SPV_EXT_physical_storage_buffer.html[@SPV_EXT_physical_storage_buffer@]
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
-- == Description
--
-- This extension allows the application to query a 64-bit buffer device
-- address value for a buffer, which can be used to access the buffer
-- memory via the @PhysicalStorageBufferEXT@ storage class in the
-- <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_buffer_reference.txt GL_EXT_buffer_reference>
-- GLSL extension and
-- {spirv}\/EXT\/SPV_EXT_physical_storage_buffer.html[@SPV_EXT_physical_storage_buffer@]
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
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT'
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-physicalstoragebufferaddresses PhysicalStorageBufferAddressesEXT>
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
-- = See Also
--
-- 'BufferDeviceAddressCreateInfoEXT', 'BufferDeviceAddressInfoEXT',
-- 'PhysicalDeviceBufferAddressFeaturesEXT',
-- 'PhysicalDeviceBufferDeviceAddressFeaturesEXT',
-- 'getBufferDeviceAddressEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_buffer_device_address Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_buffer_device_address  ( BufferDeviceAddressCreateInfoEXT
                                                       , PhysicalDeviceBufferDeviceAddressFeaturesEXT
                                                       ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data BufferDeviceAddressCreateInfoEXT

instance ToCStruct BufferDeviceAddressCreateInfoEXT
instance Show BufferDeviceAddressCreateInfoEXT

instance FromCStruct BufferDeviceAddressCreateInfoEXT


data PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT
instance Show PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance FromCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT

