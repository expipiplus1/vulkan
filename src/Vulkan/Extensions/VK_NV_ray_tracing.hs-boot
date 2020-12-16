{-# language CPP #-}
-- | = Name
--
-- VK_NV_ray_tracing - device extension
--
-- == VK_NV_ray_tracing
--
-- [__Name String__]
--     @VK_NV_ray_tracing@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     166
--
-- [__Revision__]
--     3
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.0
--
--     -   Requires @VK_KHR_get_physical_device_properties2@
--
--     -   Requires @VK_KHR_get_memory_requirements2@
--
-- [__Contact__]
--
--     -   Eric Werness
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_NV_ray_tracing:%20&body=@ewerness%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2018-11-20
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/NV/SPV_NV_ray_tracing.html SPV_NV_ray_tracing>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/nv/GLSL_NV_ray_tracing.txt GL_NV_ray_tracing>
--
-- [__Contributors__]
--
--     -   Eric Werness, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Robert Stepinski, NVIDIA
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Martin Stich, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Joshua Barczak, Intel
--
--     -   Tobias Hector, AMD
--
--     -   Henrik Rydgard, NVIDIA
--
--     -   Pascal Gautron, NVIDIA
--
-- == Description
--
-- Rasterization has been the dominant method to produce interactive
-- graphics, but increasing performance of graphics hardware has made ray
-- tracing a viable option for interactive rendering. Being able to
-- integrate ray tracing with traditional rasterization makes it easier for
-- applications to incrementally add ray traced effects to existing
-- applications or to do hybrid approaches with rasterization for primary
-- visibility and ray tracing for secondary queries.
--
-- To enable ray tracing, this extension adds a few different categories of
-- new functionality:
--
-- -   Acceleration structure objects and build commands
--
-- -   A new pipeline type with new shader domains
--
-- -   An indirection table to link shader groups with acceleration
--     structure items
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_NV_ray_tracing@
--
-- == New Object Types
--
-- -   'Vulkan.Extensions.Handles.AccelerationStructureNV'
--
-- == New Commands
--
-- -   'bindAccelerationStructureMemoryNV'
--
-- -   'cmdBuildAccelerationStructureNV'
--
-- -   'cmdCopyAccelerationStructureNV'
--
-- -   'cmdTraceRaysNV'
--
-- -   'cmdWriteAccelerationStructuresPropertiesNV'
--
-- -   'compileDeferredNV'
--
-- -   'createAccelerationStructureNV'
--
-- -   'createRayTracingPipelinesNV'
--
-- -   'destroyAccelerationStructureNV'
--
-- -   'getAccelerationStructureHandleNV'
--
-- -   'getAccelerationStructureMemoryRequirementsNV'
--
-- -   'getRayTracingShaderGroupHandlesNV'
--
-- == New Structures
--
-- -   'AabbPositionsNV'
--
-- -   'AccelerationStructureCreateInfoNV'
--
-- -   'AccelerationStructureInfoNV'
--
-- -   'AccelerationStructureInstanceNV'
--
-- -   'AccelerationStructureMemoryRequirementsInfoNV'
--
-- -   'BindAccelerationStructureMemoryInfoNV'
--
-- -   'GeometryAABBNV'
--
-- -   'GeometryDataNV'
--
-- -   'GeometryNV'
--
-- -   'GeometryTrianglesNV'
--
-- -   'Vulkan.Extensions.VK_KHR_get_memory_requirements2.MemoryRequirements2KHR'
--
-- -   'RayTracingPipelineCreateInfoNV'
--
-- -   'RayTracingShaderGroupCreateInfoNV'
--
-- -   'TransformMatrixNV'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRayTracingPropertiesNV'
--
-- -   Extending 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet':
--
--     -   'WriteDescriptorSetAccelerationStructureNV'
--
-- == New Enums
--
-- -   'AccelerationStructureMemoryRequirementsTypeNV'
--
-- -   'AccelerationStructureTypeNV'
--
-- -   'BuildAccelerationStructureFlagBitsNV'
--
-- -   'CopyAccelerationStructureModeNV'
--
-- -   'GeometryFlagBitsNV'
--
-- -   'GeometryInstanceFlagBitsNV'
--
-- -   'GeometryTypeNV'
--
-- -   'RayTracingShaderGroupTypeNV'
--
-- == New Bitmasks
--
-- -   'BuildAccelerationStructureFlagsNV'
--
-- -   'GeometryFlagsNV'
--
-- -   'GeometryInstanceFlagsNV'
--
-- == New Enum Constants
--
-- -   'NV_RAY_TRACING_EXTENSION_NAME'
--
-- -   'NV_RAY_TRACING_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureTypeKHR':
--
--     -   'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV'
--
--     -   'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.AccessFlagBits.AccessFlagBits':
--
--     -   'ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV'
--
--     -   'ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'BUFFER_USAGE_RAY_TRACING_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BuildAccelerationStructureFlagBitsKHR':
--
--     -   'BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV'
--
--     -   'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV'
--
--     -   'BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV'
--
--     -   'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV'
--
--     -   'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyAccelerationStructureModeKHR':
--
--     -   'COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV'
--
--     -   'COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_EXT_debug_report.DebugReportObjectTypeEXT':
--
--     -   'Vulkan.Extensions.VK_EXT_debug_report.DEBUG_REPORT_OBJECT_TYPE_ACCELERATION_STRUCTURE_NV_EXT'
--
-- -   Extending 'Vulkan.Core10.Enums.DescriptorType.DescriptorType':
--
--     -   'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryFlagBitsKHR':
--
--     -   'GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV'
--
--     -   'GEOMETRY_OPAQUE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryInstanceFlagBitsKHR':
--
--     -   'GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV'
--
--     -   'GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV'
--
--     -   'GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV'
--
--     -   'GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR':
--
--     -   'GEOMETRY_TYPE_AABBS_NV'
--
--     -   'GEOMETRY_TYPE_TRIANGLES_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.IndexType.IndexType':
--
--     -   'INDEX_TYPE_NONE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.ObjectType.ObjectType':
--
--     -   'Vulkan.Core10.Enums.ObjectType.OBJECT_TYPE_ACCELERATION_STRUCTURE_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint':
--
--     -   'PIPELINE_BIND_POINT_RAY_TRACING_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEFER_COMPILE_BIT_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV'
--
--     -   'PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV'
--
-- -   Extending
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingShaderGroupTypeKHR':
--
--     -   'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV'
--
--     -   'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV'
--
--     -   'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits':
--
--     -   'SHADER_STAGE_ANY_HIT_BIT_NV'
--
--     -   'SHADER_STAGE_CALLABLE_BIT_NV'
--
--     -   'SHADER_STAGE_CLOSEST_HIT_BIT_NV'
--
--     -   'SHADER_STAGE_INTERSECTION_BIT_NV'
--
--     -   'SHADER_STAGE_MISS_BIT_NV'
--
--     -   'SHADER_STAGE_RAYGEN_BIT_NV'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_AABB_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV'
--
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-launchid LaunchIdNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-launchsize LaunchSizeNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-worldrayorigin WorldRayOriginNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-worldraydirection WorldRayDirectionNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-objectrayorigin ObjectRayOriginNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-objectraydirection ObjectRayDirectionNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-raytmin RayTminNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-raytmax RayTmaxNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-instancecustomindex InstanceCustomIndexNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-instanceid InstanceId>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-objecttoworld ObjectToWorldNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-worldtoobject WorldToObjectNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-hitt HitTNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-hitkind HitKindNV>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-incomingrayflags IncomingRayFlagsNV>
--
-- -   (modified)@PrimitiveId@
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTracingNV RayTracingNV>
--
-- == Issues
--
-- 1) Are there issues?
--
-- __RESOLVED__: Yes.
--
-- == Sample Code
--
-- Example ray generation GLSL shader
--
-- > #version 450 core
-- > #extension GL_NV_ray_tracing : require
-- > layout(set = 0, binding = 0, rgba8) uniform image2D image;
-- > layout(set = 0, binding = 1) uniform accelerationStructureNV as;
-- > layout(location = 0) rayPayloadNV float payload;
-- >
-- > void main()
-- > {
-- >    vec4 col = vec4(0, 0, 0, 1);
-- >
-- >    vec3 origin = vec3(float(gl_LaunchIDNV.x)/float(gl_LaunchSizeNV.x), float(gl_LaunchIDNV.y)/float(gl_LaunchSizeNV.y), 1.0);
-- >    vec3 dir = vec3(0.0, 0.0, -1.0);
-- >
-- >    traceNV(as, 0, 0xff, 0, 1, 0, origin, 0.0, dir, 1000.0, 0);
-- >
-- >    col.y = payload;
-- >
-- >    imageStore(image, ivec2(gl_LaunchIDNV.xy), col);
-- > }
--
-- == Version History
--
-- -   Revision 1, 2018-09-11 (Robert Stepinski, Nuno Subtil, Eric Werness)
--
--     -   Internal revisions
--
-- -   Revision 2, 2018-10-19 (Eric Werness)
--
--     -   rename to VK_NV_ray_tracing, add support for callables.
--
--     -   too many updates to list
--
-- -   Revision 3, 2018-11-20 (Daniel Koch)
--
--     -   update to use InstanceId instead of InstanceIndex as
--         implemented.
--
-- = See Also
--
-- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV', 'AabbPositionsNV',
-- 'AccelerationStructureCreateInfoNV', 'AccelerationStructureInfoNV',
-- 'AccelerationStructureInstanceNV',
-- 'AccelerationStructureMemoryRequirementsInfoNV',
-- 'AccelerationStructureMemoryRequirementsTypeNV',
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'AccelerationStructureTypeNV', 'BindAccelerationStructureMemoryInfoNV',
-- 'BuildAccelerationStructureFlagBitsNV',
-- 'BuildAccelerationStructureFlagsNV', 'CopyAccelerationStructureModeNV',
-- 'GeometryAABBNV', 'GeometryDataNV', 'GeometryFlagBitsNV',
-- 'GeometryFlagsNV', 'GeometryInstanceFlagBitsNV',
-- 'GeometryInstanceFlagsNV', 'GeometryNV', 'GeometryTrianglesNV',
-- 'GeometryTypeNV',
-- 'Vulkan.Extensions.VK_KHR_get_memory_requirements2.MemoryRequirements2KHR',
-- 'PhysicalDeviceRayTracingPropertiesNV',
-- 'RayTracingPipelineCreateInfoNV', 'RayTracingShaderGroupCreateInfoNV',
-- 'RayTracingShaderGroupTypeNV', 'TransformMatrixNV',
-- 'WriteDescriptorSetAccelerationStructureNV',
-- 'bindAccelerationStructureMemoryNV', 'cmdBuildAccelerationStructureNV',
-- 'cmdCopyAccelerationStructureNV', 'cmdTraceRaysNV',
-- 'cmdWriteAccelerationStructuresPropertiesNV', 'compileDeferredNV',
-- 'createAccelerationStructureNV', 'createRayTracingPipelinesNV',
-- 'destroyAccelerationStructureNV', 'getAccelerationStructureHandleNV',
-- 'getAccelerationStructureMemoryRequirementsNV',
-- 'getRayTracingShaderGroupHandlesNV'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_NV_ray_tracing  ( AccelerationStructureCreateInfoNV
                                            , AccelerationStructureInfoNV
                                            , AccelerationStructureMemoryRequirementsInfoNV
                                            , BindAccelerationStructureMemoryInfoNV
                                            , GeometryAABBNV
                                            , GeometryDataNV
                                            , GeometryNV
                                            , GeometryTrianglesNV
                                            , PhysicalDeviceRayTracingPropertiesNV
                                            , RayTracingPipelineCreateInfoNV
                                            , RayTracingShaderGroupCreateInfoNV
                                            , WriteDescriptorSetAccelerationStructureNV
                                            ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data AccelerationStructureCreateInfoNV

instance ToCStruct AccelerationStructureCreateInfoNV
instance Show AccelerationStructureCreateInfoNV

instance FromCStruct AccelerationStructureCreateInfoNV


data AccelerationStructureInfoNV

instance ToCStruct AccelerationStructureInfoNV
instance Show AccelerationStructureInfoNV

instance FromCStruct AccelerationStructureInfoNV


data AccelerationStructureMemoryRequirementsInfoNV

instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV
instance Show AccelerationStructureMemoryRequirementsInfoNV

instance FromCStruct AccelerationStructureMemoryRequirementsInfoNV


data BindAccelerationStructureMemoryInfoNV

instance ToCStruct BindAccelerationStructureMemoryInfoNV
instance Show BindAccelerationStructureMemoryInfoNV

instance FromCStruct BindAccelerationStructureMemoryInfoNV


data GeometryAABBNV

instance ToCStruct GeometryAABBNV
instance Show GeometryAABBNV

instance FromCStruct GeometryAABBNV


data GeometryDataNV

instance ToCStruct GeometryDataNV
instance Show GeometryDataNV

instance FromCStruct GeometryDataNV


data GeometryNV

instance ToCStruct GeometryNV
instance Show GeometryNV

instance FromCStruct GeometryNV


data GeometryTrianglesNV

instance ToCStruct GeometryTrianglesNV
instance Show GeometryTrianglesNV

instance FromCStruct GeometryTrianglesNV


data PhysicalDeviceRayTracingPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingPropertiesNV
instance Show PhysicalDeviceRayTracingPropertiesNV

instance FromCStruct PhysicalDeviceRayTracingPropertiesNV


type role RayTracingPipelineCreateInfoNV nominal
data RayTracingPipelineCreateInfoNV (es :: [Type])

instance (Extendss RayTracingPipelineCreateInfoNV es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoNV es)
instance Show (Chain es) => Show (RayTracingPipelineCreateInfoNV es)

instance (Extendss RayTracingPipelineCreateInfoNV es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoNV es)


data RayTracingShaderGroupCreateInfoNV

instance ToCStruct RayTracingShaderGroupCreateInfoNV
instance Show RayTracingShaderGroupCreateInfoNV

instance FromCStruct RayTracingShaderGroupCreateInfoNV


data WriteDescriptorSetAccelerationStructureNV

instance ToCStruct WriteDescriptorSetAccelerationStructureNV
instance Show WriteDescriptorSetAccelerationStructureNV

instance FromCStruct WriteDescriptorSetAccelerationStructureNV

