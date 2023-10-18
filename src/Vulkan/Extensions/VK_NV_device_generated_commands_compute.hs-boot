{-# language CPP #-}
-- | = Name
--
-- VK_NV_device_generated_commands_compute - device extension
--
-- == VK_NV_device_generated_commands_compute
--
-- [__Name String__]
--     @VK_NV_device_generated_commands_compute@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     429
--
-- [__Revision__]
--     2
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
--
-- [__Contact__]
--
--     -   Vikram Kushwaha
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_device_generated_commands_compute] @vkushwaha-nv%0A*Here describe the issue or question you have about the VK_NV_device_generated_commands_compute extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-07-21
--
-- [__Contributors__]
--
--     -   Vikram Kushwaha, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Mike Blumenkrantz, VALVE
--
-- == Description
--
-- This extension allows the device to generate commands for binding
-- compute pipelines, setting push constants and launching compute
-- dispatches.
--
-- == New Commands
--
-- -   'cmdUpdatePipelineIndirectBufferNV'
--
-- -   'getPipelineIndirectDeviceAddressNV'
--
-- -   'getPipelineIndirectMemoryRequirementsNV'
--
-- == New Structures
--
-- -   'BindPipelineIndirectCommandNV'
--
-- -   'ComputePipelineIndirectBufferInfoNV'
--
-- -   'PipelineIndirectDeviceAddressInfoNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_DEVICE_GENERATED_COMMANDS_COMPUTE_EXTENSION_NAME'
--
-- -   'NV_DEVICE_GENERATED_COMMANDS_COMPUTE_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsTokenTypeNV':
--
--     -   'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_DISPATCH_NV'
--
--     -   'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_PIPELINE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_COMPUTE_PIPELINE_INDIRECT_BUFFER_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_COMPUTE_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_INDIRECT_DEVICE_ADDRESS_INFO_NV'
--
-- == Version History
--
-- -   Revision 2, 2023-07-21 (Vikram Kushwaha)
--
--     -   Rename vkCmdUpdatePipelineIndirectBuffer to
--         vkCmdUpdatePipelineIndirectBufferNV
--
-- -   Revision 1, 2023-06-09 (Vikram Kushwaha)
--
--     -   First Revision
--
-- == See Also
--
-- 'BindPipelineIndirectCommandNV', 'ComputePipelineIndirectBufferInfoNV',
-- 'PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV',
-- 'PipelineIndirectDeviceAddressInfoNV',
-- 'cmdUpdatePipelineIndirectBufferNV',
-- 'getPipelineIndirectDeviceAddressNV',
-- 'getPipelineIndirectMemoryRequirementsNV'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_device_generated_commands_compute Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_device_generated_commands_compute  ( BindPipelineIndirectCommandNV
                                                                  , ComputePipelineIndirectBufferInfoNV
                                                                  , PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV
                                                                  , PipelineIndirectDeviceAddressInfoNV
                                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BindPipelineIndirectCommandNV

instance ToCStruct BindPipelineIndirectCommandNV
instance Show BindPipelineIndirectCommandNV

instance FromCStruct BindPipelineIndirectCommandNV


data ComputePipelineIndirectBufferInfoNV

instance ToCStruct ComputePipelineIndirectBufferInfoNV
instance Show ComputePipelineIndirectBufferInfoNV

instance FromCStruct ComputePipelineIndirectBufferInfoNV


data PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV

instance ToCStruct PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV
instance Show PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV

instance FromCStruct PhysicalDeviceDeviceGeneratedCommandsComputeFeaturesNV


data PipelineIndirectDeviceAddressInfoNV

instance ToCStruct PipelineIndirectDeviceAddressInfoNV
instance Show PipelineIndirectDeviceAddressInfoNV

instance FromCStruct PipelineIndirectDeviceAddressInfoNV

