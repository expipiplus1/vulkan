{-# language CPP #-}
-- | = Name
--
-- VK_NV_push_constant_bank - device extension
--
-- = VK_NV_push_constant_bank
--
-- [__Name String__]
--     @VK_NV_push_constant_bank@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     581
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_push_constant_bank.html SPV_NV_push_constant_bank>
--
-- [__Contact__]
--
--     -   Vassili Nikolaev
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_push_constant_bank] @vasnik1%0A*Here describe the issue or question you have about the VK_NV_push_constant_bank extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_NV_push_constant_bank.adoc VK_NV_push_constant_bank>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2025-09-15
--
-- [__Contributors__]
--
--     -   Pat Brown, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Rodrigo Locatti, NVIDIA
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_push_constant_bank VK_NV_push_constant_bank>
-- extension allows applications to specify a bank and offset for push
-- constants, enabling more flexible push constant management in descriptor
-- heap scenarios where shaders are able to access different root
-- descriptors.
--
-- Traditional push constants are placed in a default location, but this
-- extension allows applications to specify which hardware constant bank to
-- use and at what offset within that bank. This provides greater control
-- over memory layout and enables more efficient use of hardware resources
-- in advanced descriptor heap configurations.
--
-- The extension integrates with
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_heap VK_EXT_descriptor_heap>
-- by allowing 'PushConstantBankInfoNV' structures to be chained to
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT',
-- 'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT',
-- 'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
-- or
-- 'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutTokenEXT'
-- structures, specifying the hardware bank where push constants should be
-- placed as part of the descriptor heap mapping configuration or push data
-- operations.
--
-- Key features include:
--
-- -   Bank and offset specification for push constant placement
--
-- -   Integration with descriptor heap mapping through structure chaining
--
-- -   Support for GLSL shader qualifiers for bank and offset specification
--     in SPIR-V
--
-- -   Validation of bank bounds and alignment requirements
--
-- -   Compatibility with existing push constant API
--
-- The number of available push constant banks is implementation-dependent
-- and can be queried through separate limits in
-- 'PhysicalDevicePushConstantBankPropertiesNV':
-- @maxGraphicsPushConstantBanks@ and @maxComputePushConstantBanks@ for
-- non-descriptor heap usage, and @maxGraphicsPushDataBanks@ and
-- @maxComputePushDataBanks@ for descriptor heap scenarios. Applications
-- must ensure bank indices remain within the appropriate
-- implementation-defined range based on the shader type and usage context.
--
-- Shader support for banks and member offsets are defined by the
-- <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_push_constant_bank.html SPV_NV_push_constant_bank>
-- SPIR-V extension, which can be used with the
-- <https://github.com/KhronosGroup/GLSL/blob/main/extensions/nv/GLSL_NV_push_constant_bank.txt GLSL_NV_push_constant_bank>
-- GLSL extension.
--
-- == New SPIR-V Capabilities
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#spirvenv-capabilities-table-PushConstantBanksNV PushConstantBanksNV>
--
-- == New SPIR-V Decorations
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-pushconstant-decorations-banknv BankNV>
--
-- -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#shaders-pushconstant-decorations-memberoffsetnv MemberOffsetNV>
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.DescriptorSetAndBindingMappingEXT',
--     'Vulkan.Extensions.VK_EXT_descriptor_heap.PushDataInfoEXT',
--     'Vulkan.Core14.Promoted_From_VK_KHR_maintenance6AdditionalFunctionality'.PushConstantsInfo',
--     'Vulkan.Extensions.VK_EXT_device_generated_commands.IndirectCommandsLayoutTokenEXT':
--
--     -   'PushConstantBankInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePushConstantBankFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDevicePushConstantBankPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_PUSH_CONSTANT_BANK_EXTENSION_NAME'
--
-- -   'NV_PUSH_CONSTANT_BANK_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_CONSTANT_BANK_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PUSH_CONSTANT_BANK_INFO_NV'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2025-09-15 (NVIDIA Vassili Nikolaev)
--
--     -   Initial draft
--
-- == See Also
--
-- No cross-references are available
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#VK_NV_push_constant_bank Vulkan Specification>.
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_push_constant_bank  ( PhysicalDevicePushConstantBankFeaturesNV
                                                   , PhysicalDevicePushConstantBankPropertiesNV
                                                   , PushConstantBankInfoNV
                                                   ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePushConstantBankFeaturesNV

instance ToCStruct PhysicalDevicePushConstantBankFeaturesNV
instance Show PhysicalDevicePushConstantBankFeaturesNV

instance FromCStruct PhysicalDevicePushConstantBankFeaturesNV


data PhysicalDevicePushConstantBankPropertiesNV

instance ToCStruct PhysicalDevicePushConstantBankPropertiesNV
instance Show PhysicalDevicePushConstantBankPropertiesNV

instance FromCStruct PhysicalDevicePushConstantBankPropertiesNV


data PushConstantBankInfoNV

instance ToCStruct PushConstantBankInfoNV
instance Show PushConstantBankInfoNV

instance FromCStruct PushConstantBankInfoNV

