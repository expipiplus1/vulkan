{-# language CPP #-}
-- | = Name
--
-- VK_KHR_shader_subgroup_uniform_control_flow - device extension
--
-- == VK_KHR_shader_subgroup_uniform_control_flow
--
-- [__Name String__]
--     @VK_KHR_shader_subgroup_uniform_control_flow@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     324
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
-- [__Contact__]
--
--     -   Alan Baker
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_shader_subgroup_uniform_control_flow] @alan-baker%0A<<Here describe the issue or question you have about the VK_KHR_shader_subgroup_uniform_control_flow extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-08-27
--
-- [__IP Status__]
--     No known IP claims.
--
-- [__Interactions and External Dependencies__]
--
--     -   Requires SPIR-V 1.3.
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_subgroup_uniform_control_flow.html SPV_KHR_subgroup_uniform_control_flow>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GL_EXT_subgroupuniform_qualifier.txt GL_EXT_subgroupuniform_qualifier>
--
-- [__Contributors__]
--
--     -   Alan Baker, Google
--
--     -   Jeff Bolz, NVIDIA
--
-- == Description
--
-- This extension allows the use of the
-- @SPV_KHR_subgroup_uniform_control_flow@ SPIR-V extension in shader
-- modules. @SPV_KHR_subgroup_uniform_control_flow@ provides stronger
-- guarantees that diverged subgroups will reconverge.
--
-- Developers should utilize this extension if they use subgroup operations
-- to reduce the work performed by a uniform subgroup. This extension will
-- guarantee that uniform subgroup will reconverge in the same manner as
-- invocation groups (see “Uniform Control Flow” in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirv-spec Khronos SPIR-V Specification>).
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR'
--
-- == New Enum Constants
--
-- -   'KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_EXTENSION_NAME'
--
-- -   'KHR_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_UNIFORM_CONTROL_FLOW_FEATURES_KHR'
--
-- == Version History
--
-- -   Revision 1, 2020-08-27 (Alan Baker)
--
--     -   Internal draft version
--
-- == See Also
--
-- 'PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_shader_subgroup_uniform_control_flow Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_shader_subgroup_uniform_control_flow  (PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR

instance ToCStruct PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR
instance Show PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR

instance FromCStruct PhysicalDeviceShaderSubgroupUniformControlFlowFeaturesKHR

