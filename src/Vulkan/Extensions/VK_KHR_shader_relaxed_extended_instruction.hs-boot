{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_relaxed_extended_instruction - device extension
--
-- = VK_KHR_shader_relaxed_extended_instruction
--
-- [__Name String__]
--     @VK_KHR_shader_relaxed_extended_instruction@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     559
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     None
--
-- [__SPIR-V Dependencies__]
--
--     -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_relaxed_extended_instruction.html SPV_KHR_relaxed_extended_instruction>
--
-- [__Contact__]
--
--     -   Nathan Gauër
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_relaxed_extended_instruction] @Keenuts%0A*Here describe the issue or question you have about the VK_KHR_shader_relaxed_extended_instruction extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_shader_relaxed_extended_instruction.adoc VK_KHR_shader_relaxed_extended_instruction>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2024-01-24
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Alan Baker, Google LLC
--
--     -   Nathan Gauër, Google LLC
--
-- == Description
--
-- This extension allows the use of the
-- @SPV_KHR_relaxed_extended_instruction@ extension in SPIR-V shader
-- modules.
--
-- It adds a new SPIR-V instruction, which allows some usage of forward
-- references in non-semantic instruction sets. This extensions interacts
-- with the @SPV_KHR_non_semantic_info@ extension, hence with
-- @VK_KHR_shader_non_semantic_info@.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_EXTENSION_NAME'
--
-- -   'KHR_SHADER_RELAXED_EXTENDED_INSTRUCTION_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_RELAXED_EXTENDED_INSTRUCTION_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2024-01-24 (Nathan Gauër)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_shader_relaxed_extended_instruction Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_relaxed_extended_instruction  (PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR

instance ToCStruct PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR
instance Show PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR

instance FromCStruct PhysicalDeviceShaderRelaxedExtendedInstructionFeaturesKHR

