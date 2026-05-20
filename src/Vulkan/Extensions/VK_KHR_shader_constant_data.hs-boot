{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_constant_data - device extension
--
-- = VK_KHR_shader_constant_data
--
-- [__Name String__]
--     @VK_KHR_shader_constant_data@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     232
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_get_physical_device_properties2 VK_KHR_get_physical_device_properties2>
--     or
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Vulkan Version 1.1>
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_constant_data.html SPV_KHR_constant_data>
--
-- [__Contact__]
--
--     -   Tobias Hector
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_constant_data] @tobski%0A*Here describe the issue or question you have about the VK_KHR_shader_constant_data extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_constant_data.adoc VK_KHR_shader_constant_data>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2026-03-18
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Tobias Hector, AMD
--
--     -   Piers Daniell, Nvidia
--
--     -   Craig Graham, Samsung
--
--     -   Vikram Tarikere, IMG
--
-- == Description
--
-- This extension allows the use of the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_constant_data.html SPV_KHR_constant_data>
-- extension in SPIR-V shader modules which enables the specification and
-- specialization of arrays of constant data.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderConstantDataFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_CONSTANT_DATA_EXTENSION_NAME'
--
-- -   'KHR_SHADER_CONSTANT_DATA_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CONSTANT_DATA_FEATURES_KHR'
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-ConstantDataKHR ConstantDataKHR>
--
-- == Version History
--
-- -   Revision 1, 2024-10-30 (Tobias Hector)
--
--     -   Initial revision
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_KHR_shader_constant_data Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_constant_data  (PhysicalDeviceShaderConstantDataFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderConstantDataFeaturesKHR

instance ToCStruct PhysicalDeviceShaderConstantDataFeaturesKHR
instance Show PhysicalDeviceShaderConstantDataFeaturesKHR

instance FromCStruct PhysicalDeviceShaderConstantDataFeaturesKHR

