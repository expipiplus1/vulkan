{-# language CPP #-}
-- | = Name
--
-- VK_EXT_mesh_shader - device extension
--
-- == VK_EXT_mesh_shader
--
-- [__Name String__]
--     @VK_EXT_mesh_shader@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     329
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires support for Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@ to be enabled
--         for any device-level functionality
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_EXT_mesh_shader] @pixeljetstream%0A<<Here describe the issue or question you have about the VK_EXT_mesh_shader extension>> >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_EXT_mesh_shader.adoc VK_EXT_mesh_shader>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-01-20
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_mesh_shader.html SPV_EXT_mesh_shader>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_mesh_shader.txt GLSL_EXT_mesh_shader>
--
--     -   Interacts with Vulkan 1.1
--
--     -   Interacts with @VK_KHR_multiview@
--
--     -   Interacts with @VK_KHR_fragment_shading_rate@
--
-- [__Contributors__]
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Pat Brown, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Piers Daniell, NVIDIA
--
--     -   Pierre Boudier, NVIDIA
--
--     -   Patrick Mours, NVIDIA
--
--     -   David Zhao Akeley, NVIDIA
--
--     -   Kedarnath Thangudu, NVIDIA
--
--     -   Timur Kristóf, Valve
--
--     -   Hans-Kristian Arntzen, Valve
--
--     -   Philip Rebohle, Valve
--
--     -   Mike Blumenkrantz, Valve
--
--     -   Slawomir Grajewski, Intel
--
--     -   Michal Pietrasiuk, Intel
--
--     -   Mariusz Merecki, Intel
--
--     -   Tom Olson, ARM
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Sandeep Kakarlapudi, ARM
--
--     -   Ruihao Zhang, QUALCOMM
--
--     -   Ricardo Garcia, Igalia, S.L.
--
--     -   Tobias Hector, AMD
--
--     -   Stu Smith, AMD
--
-- == Description
--
-- This extension provides a new mechanism allowing applications to
-- generate collections of geometric primitives via programmable mesh
-- shading. It is an alternative to the existing programmable primitive
-- shading pipeline, which relied on generating input primitives by a fixed
-- function assembler as well as fixed function vertex fetch.
--
-- This extension also adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/EXT/SPV_EXT_mesh_shader.html SPV_EXT_mesh_shader>
--
-- == New Commands
--
-- -   'cmdDrawMeshTasksEXT'
--
-- -   'cmdDrawMeshTasksIndirectCountEXT'
--
-- -   'cmdDrawMeshTasksIndirectEXT'
--
-- == New Structures
--
-- -   'DrawMeshTasksIndirectCommandEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMeshShaderFeaturesEXT'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMeshShaderPropertiesEXT'
--
-- == New Enum Constants
--
-- -   'EXT_MESH_SHADER_EXTENSION_NAME'
--
-- -   'EXT_MESH_SHADER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QueryPipelineStatisticFlagBits':
--
--     -   'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QUERY_PIPELINE_STATISTIC_MESH_SHADER_INVOCATIONS_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits.QUERY_PIPELINE_STATISTIC_TASK_SHADER_INVOCATIONS_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_EXT'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_EXT'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsTokenTypeNV':
--
--     -   'Vulkan.Extensions.VK_NV_device_generated_commands.INDIRECT_COMMANDS_TOKEN_TYPE_DRAW_MESH_TASKS_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-cullprimitive CullPrimitiveEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-primitivepointindices PrimitivePointIndicesEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-primitivelineindices PrimitiveLineIndicesEXT>
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-primitivetriangleindices PrimitiveTriangleIndicesEXT>
--
-- -   (modified)@Position@
--
-- -   (modified)@PointSize@
--
-- -   (modified)@ClipDistance@
--
-- -   (modified)@CullDistance@
--
-- -   (modified)@PrimitiveId@
--
-- -   (modified)@Layer@
--
-- -   (modified)@ViewportIndex@
--
-- -   (modified)@NumWorkgroups@
--
-- -   (modified)@WorkgroupSize@
--
-- -   (modified)@WorkgroupId@
--
-- -   (modified)@LocalInvocationId@
--
-- -   (modified)@GlobalInvocationId@
--
-- -   (modified)@LocalInvocationIndex@
--
-- -   (modified)@NumSubgroups@
--
-- -   (modified)@SubgroupId@
--
-- -   (modified)@DrawIndex@
--
-- -   (modified)@PrimitiveShadingRateKHR@
--
-- -   (modified)@ViewIndex@
--
-- == New SPIR-V Capability
--
-- -   <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-meshshading MeshShadingEXT>
--
-- == Version History
--
-- -   Revision 1, 2022-03-08 (Christoph Kubisch, Daniel Koch, Patrick
--     Mours)
--
--     -   Initial revision
--
-- == See Also
--
-- 'DrawMeshTasksIndirectCommandEXT',
-- 'PhysicalDeviceMeshShaderFeaturesEXT',
-- 'PhysicalDeviceMeshShaderPropertiesEXT', 'cmdDrawMeshTasksEXT',
-- 'cmdDrawMeshTasksIndirectCountEXT', 'cmdDrawMeshTasksIndirectEXT'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_EXT_mesh_shader Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_EXT_mesh_shader  ( DrawMeshTasksIndirectCommandEXT
                                             , PhysicalDeviceMeshShaderFeaturesEXT
                                             , PhysicalDeviceMeshShaderPropertiesEXT
                                             ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DrawMeshTasksIndirectCommandEXT

instance ToCStruct DrawMeshTasksIndirectCommandEXT
instance Show DrawMeshTasksIndirectCommandEXT

instance FromCStruct DrawMeshTasksIndirectCommandEXT


data PhysicalDeviceMeshShaderFeaturesEXT

instance ToCStruct PhysicalDeviceMeshShaderFeaturesEXT
instance Show PhysicalDeviceMeshShaderFeaturesEXT

instance FromCStruct PhysicalDeviceMeshShaderFeaturesEXT


data PhysicalDeviceMeshShaderPropertiesEXT

instance ToCStruct PhysicalDeviceMeshShaderPropertiesEXT
instance Show PhysicalDeviceMeshShaderPropertiesEXT

instance FromCStruct PhysicalDeviceMeshShaderPropertiesEXT

