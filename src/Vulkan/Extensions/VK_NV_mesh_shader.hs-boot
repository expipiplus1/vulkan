{-# language CPP #-}
-- | = Name
--
-- VK_NV_mesh_shader - device extension
--
-- == VK_NV_mesh_shader
--
-- [__Name String__]
--     @VK_NV_mesh_shader@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     203
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
-- [__Contact__]
--
--     -   Christoph Kubisch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_mesh_shader:%20&body=@pixeljetstream%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-07-19
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_mesh_shader.html SPV_NV_mesh_shader>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_mesh_shader.txt GLSL_NV_mesh_shader>
--
-- [__Contributors__]
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
-- == Description
--
-- This extension provides a new mechanism allowing applications to
-- generate collections of geometric primitives via programmable mesh
-- shading. It is an alternative to the existing programmable primitive
-- shading pipeline, which relied on generating input primitives by a fixed
-- function assembler as well as fixed function vertex fetch.
--
-- There are new programmable shader types — the task and mesh shader — to
-- generate these collections to be processed by fixed-function primitive
-- assembly and rasterization logic. When the task and mesh shaders are
-- dispatched, they replace the standard programmable vertex processing
-- pipeline, including vertex array attribute fetching, vertex shader
-- processing, tessellation, and the geometry shader processing.
--
-- This extension also adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_mesh_shader.html SPV_NV_mesh_shader>
--
-- == New Commands
--
-- -   'cmdDrawMeshTasksIndirectCountNV'
--
-- -   'cmdDrawMeshTasksIndirectNV'
--
-- -   'cmdDrawMeshTasksNV'
--
-- == New Structures
--
-- -   'DrawMeshTasksIndirectCommandNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMeshShaderFeaturesNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMeshShaderPropertiesNV'
--
-- == New Enum Constants
--
-- -   'NV_MESH_SHADER_EXTENSION_NAME'
--
-- -   'NV_MESH_SHADER_SPEC_VERSION'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_MESH_SHADER_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_TASK_SHADER_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MESH_BIT_NV'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_TASK_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-taskcount TaskCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-primitivecount PrimitiveCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-primitiveindices PrimitiveIndicesNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-clipdistancepv ClipDistancePerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-culldistancepv CullDistancePerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-layerpv LayerPerViewNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-meshviewcount MeshViewCountNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-meshviewindices MeshViewIndicesNV>
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
-- -   (modified)@DrawIndex@
--
-- -   (modified)@ViewportMaskNV@
--
-- -   (modified)@PositionPerViewNV@
--
-- -   (modified)@ViewportMaskPerViewNV@
--
-- == New SPIR-V Capability
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-MeshShadingNV MeshShadingNV>
--
-- == Issues
--
-- 1.  How to name this extension?
--
--     RESOLVED: VK_NV_mesh_shader
--
--     Other options considered:
--
--     -   VK_NV_mesh_shading
--
--     -   VK_NV_programmable_mesh_shading
--
--     -   VK_NV_primitive_group_shading
--
--     -   VK_NV_grouped_drawing
--
-- 2.  Do we need a new VkPrimitiveTopology?
--
--     RESOLVED: NO, we skip the InputAssembler stage
--
-- 3.  Should we allow Instancing?
--
--     RESOLVED: NO, there is no fixed function input, other than the IDs.
--     However, allow offsetting with a \"first\" value.
--
-- 4.  Should we use existing vkCmdDraw or introduce new functions?
--
--     RESOLVED: Introduce new functions.
--
--     New functions make it easier to separate from \"programmable
--     primitive shading\" chapter, less \"dual use\" language about
--     existing functions having alternative behavior. The text around the
--     existing \"draws\" is heavily based around emitting vertices.
--
-- 5.  If new functions, how to name?
--
--     RESOLVED: CmdDrawMeshTasks*
--
--     Other options considered:
--
--     -   CmdDrawMeshed
--
--     -   CmdDrawTasked
--
--     -   CmdDrawGrouped
--
-- 6.  Should VK_SHADER_STAGE_ALL_GRAPHICS be updated to include the new
--     stages?
--
--     RESOLVED: No. If an application were to be recompiled with headers
--     that include additional shader stage bits in
--     VK_SHADER_STAGE_ALL_GRAPHICS, then the previously valid application
--     would no longer be valid on implementations that don’t support mesh
--     or task shaders. This means the change would not be backwards
--     compatible. It’s too bad VkShaderStageFlagBits doesn’t have a
--     dedicated \"all supported graphics stages\" bit like
--     VK_PIPELINE_STAGE_ALL_GRAPHICS_BIT, which would have avoided this
--     problem.
--
-- == Version History
--
-- -   Revision 1, 2018-07-19 (Christoph Kubisch, Daniel Koch)
--
--     -   Internal revisions
--
-- = See Also
--
-- 'DrawMeshTasksIndirectCommandNV', 'PhysicalDeviceMeshShaderFeaturesNV',
-- 'PhysicalDeviceMeshShaderPropertiesNV',
-- 'cmdDrawMeshTasksIndirectCountNV', 'cmdDrawMeshTasksIndirectNV',
-- 'cmdDrawMeshTasksNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_mesh_shader Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_mesh_shader  ( DrawMeshTasksIndirectCommandNV
                                            , PhysicalDeviceMeshShaderFeaturesNV
                                            , PhysicalDeviceMeshShaderPropertiesNV
                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DrawMeshTasksIndirectCommandNV

instance ToCStruct DrawMeshTasksIndirectCommandNV
instance Show DrawMeshTasksIndirectCommandNV

instance FromCStruct DrawMeshTasksIndirectCommandNV


data PhysicalDeviceMeshShaderFeaturesNV

instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV
instance Show PhysicalDeviceMeshShaderFeaturesNV

instance FromCStruct PhysicalDeviceMeshShaderFeaturesNV


data PhysicalDeviceMeshShaderPropertiesNV

instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV
instance Show PhysicalDeviceMeshShaderPropertiesNV

instance FromCStruct PhysicalDeviceMeshShaderPropertiesNV

