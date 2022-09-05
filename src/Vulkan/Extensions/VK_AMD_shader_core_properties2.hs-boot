{-# language CPP #-}
-- | = Name
--
-- VK_AMD_shader_core_properties2 - device extension
--
-- == VK_AMD_shader_core_properties2
--
-- [__Name String__]
--     @VK_AMD_shader_core_properties2@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     228
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_AMD_shader_core_properties@ to be enabled for any
--         device-level functionality
--
-- [__Contact__]
--
--     -   Matthaeus G. Chajdas
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_AMD_shader_core_properties2] @anteru%0A<<Here describe the issue or question you have about the VK_AMD_shader_core_properties2 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2019-07-26
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Matthaeus G. Chajdas, AMD
--
--     -   Tobias Hector, AMD
--
-- == Description
--
-- This extension exposes additional shader core properties for a target
-- physical device through the @VK_KHR_get_physical_device_properties2@
-- extension.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceShaderCoreProperties2AMD'
--
-- == New Enums
--
-- -   'ShaderCorePropertiesFlagBitsAMD'
--
-- == New Bitmasks
--
-- -   'ShaderCorePropertiesFlagsAMD'
--
-- == New Enum Constants
--
-- -   'AMD_SHADER_CORE_PROPERTIES_2_EXTENSION_NAME'
--
-- -   'AMD_SHADER_CORE_PROPERTIES_2_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD'
--
-- == Examples
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2019-07-26 (Matthaeus G. Chajdas)
--
--     -   Initial draft.
--
-- == See Also
--
-- 'PhysicalDeviceShaderCoreProperties2AMD',
-- 'ShaderCorePropertiesFlagBitsAMD', 'ShaderCorePropertiesFlagsAMD'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_AMD_shader_core_properties2 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_AMD_shader_core_properties2  (PhysicalDeviceShaderCoreProperties2AMD) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderCoreProperties2AMD

instance ToCStruct PhysicalDeviceShaderCoreProperties2AMD
instance Show PhysicalDeviceShaderCoreProperties2AMD

instance FromCStruct PhysicalDeviceShaderCoreProperties2AMD

