{-# language CPP #-}
-- | = Name
--
-- VK_NV_per_stage_descriptor_set - device extension
--
-- = VK_NV_per_stage_descriptor_set
--
-- [__Name String__]
--     @VK_NV_per_stage_descriptor_set@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     517
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Not ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance6 VK_KHR_maintenance6>
--
-- [__Contact__]
--
--     -   Piers Daniell
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_NV_per_stage_descriptor_set] @pdaniell-nv%0A*Here describe the issue or question you have about the VK_NV_per_stage_descriptor_set extension* >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-10-16
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Contributors__]
--
--     -   Daniel Story, Nintendo
--
-- == Description
--
-- This extension introduces a new descriptor set layout creation flag that
-- allows bindings in a descriptor set to be scoped to each shader stage.
-- This means that shaders bound at the same time /may/ use completely
-- different descriptor set layouts without any restrictions on
-- compatibility, and that the descriptor limits that would otherwise apply
-- to the union of all stages together instead apply to each stage
-- individually. It also means that descriptors shared by multiple stages
-- /must/ be bound to each stage or set of stages that use a unique
-- descriptor set layout using their specific per stage descriptor set
-- layout(s).
--
-- This extension also allows each of the new descriptor binding functions
-- from VK_KHR_maintenance6 to have their
-- 'Vulkan.Core10.Handles.PipelineLayout' member be optionally set to
-- 'Vulkan.Core10.APIConstants.NULL_HANDLE', in which case the pipeline
-- layout information is taken from a
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo' structure in the
-- @pNext@ chain. This enables descriptors to be directly bound using
-- descriptor set layouts without applications needing to create and manage
-- 'Vulkan.Core10.Handles.PipelineLayout' objects at command recording
-- time.
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDevicePerStageDescriptorSetFeaturesNV'
--
-- == New Enum Constants
--
-- -   'NV_PER_STAGE_DESCRIPTOR_SET_EXTENSION_NAME'
--
-- -   'NV_PER_STAGE_DESCRIPTOR_SET_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DescriptorSetLayoutCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.DescriptorSetLayoutCreateFlagBits.DESCRIPTOR_SET_LAYOUT_CREATE_PER_STAGE_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_PER_STAGE_DESCRIPTOR_SET_FEATURES_NV'
--
-- == Issues
--
-- None
--
-- == Version History
--
-- -   Revision 1, 2023-10-16 (Piers Daniell)
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
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_NV_per_stage_descriptor_set Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_per_stage_descriptor_set  (PhysicalDevicePerStageDescriptorSetFeaturesNV) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDevicePerStageDescriptorSetFeaturesNV

instance ToCStruct PhysicalDevicePerStageDescriptorSetFeaturesNV
instance Show PhysicalDevicePerStageDescriptorSetFeaturesNV

instance FromCStruct PhysicalDevicePerStageDescriptorSetFeaturesNV

