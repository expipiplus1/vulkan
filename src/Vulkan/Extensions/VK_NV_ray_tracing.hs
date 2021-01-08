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
module Vulkan.Extensions.VK_NV_ray_tracing  ( compileDeferredNV
                                            , createAccelerationStructureNV
                                            , withAccelerationStructureNV
                                            , destroyAccelerationStructureNV
                                            , getAccelerationStructureMemoryRequirementsNV
                                            , bindAccelerationStructureMemoryNV
                                            , cmdCopyAccelerationStructureNV
                                            , cmdWriteAccelerationStructuresPropertiesNV
                                            , cmdBuildAccelerationStructureNV
                                            , cmdTraceRaysNV
                                            , getAccelerationStructureHandleNV
                                            , createRayTracingPipelinesNV
                                            , withRayTracingPipelinesNV
                                            , pattern SHADER_STAGE_RAYGEN_BIT_NV
                                            , pattern SHADER_STAGE_ANY_HIT_BIT_NV
                                            , pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV
                                            , pattern SHADER_STAGE_MISS_BIT_NV
                                            , pattern SHADER_STAGE_INTERSECTION_BIT_NV
                                            , pattern SHADER_STAGE_CALLABLE_BIT_NV
                                            , pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV
                                            , pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV
                                            , pattern BUFFER_USAGE_RAY_TRACING_BIT_NV
                                            , pattern PIPELINE_BIND_POINT_RAY_TRACING_NV
                                            , pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV
                                            , pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV
                                            , pattern INDEX_TYPE_NONE_NV
                                            , pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV
                                            , pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV
                                            , pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV
                                            , pattern GEOMETRY_TYPE_TRIANGLES_NV
                                            , pattern GEOMETRY_TYPE_AABBS_NV
                                            , pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV
                                            , pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV
                                            , pattern GEOMETRY_OPAQUE_BIT_NV
                                            , pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV
                                            , pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV
                                            , pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV
                                            , pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV
                                            , pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV
                                            , pattern SHADER_UNUSED_NV
                                            , getRayTracingShaderGroupHandlesNV
                                            , RayTracingShaderGroupCreateInfoNV(..)
                                            , RayTracingPipelineCreateInfoNV(..)
                                            , GeometryTrianglesNV(..)
                                            , GeometryAABBNV(..)
                                            , GeometryDataNV(..)
                                            , GeometryNV(..)
                                            , AccelerationStructureInfoNV(..)
                                            , AccelerationStructureCreateInfoNV(..)
                                            , BindAccelerationStructureMemoryInfoNV(..)
                                            , WriteDescriptorSetAccelerationStructureNV(..)
                                            , AccelerationStructureMemoryRequirementsInfoNV(..)
                                            , PhysicalDeviceRayTracingPropertiesNV(..)
                                            , AccelerationStructureMemoryRequirementsTypeNV( ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV
                                                                                           , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV
                                                                                           , ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV
                                                                                           , ..
                                                                                           )
                                            , GeometryFlagsNV
                                            , GeometryInstanceFlagsNV
                                            , BuildAccelerationStructureFlagsNV
                                            , GeometryFlagBitsNV
                                            , GeometryInstanceFlagBitsNV
                                            , BuildAccelerationStructureFlagBitsNV
                                            , CopyAccelerationStructureModeNV
                                            , AccelerationStructureTypeNV
                                            , GeometryTypeNV
                                            , RayTracingShaderGroupTypeNV
                                            , AabbPositionsNV
                                            , TransformMatrixNV
                                            , AccelerationStructureInstanceNV
                                            , NV_RAY_TRACING_SPEC_VERSION
                                            , pattern NV_RAY_TRACING_SPEC_VERSION
                                            , NV_RAY_TRACING_EXTENSION_NAME
                                            , pattern NV_RAY_TRACING_EXTENSION_NAME
                                            , AccelerationStructureNV(..)
                                            , AabbPositionsKHR(..)
                                            , TransformMatrixKHR(..)
                                            , AccelerationStructureInstanceKHR(..)
                                            , getRayTracingShaderGroupHandlesKHR
                                            , DebugReportObjectTypeEXT(..)
                                            , GeometryInstanceFlagBitsKHR(..)
                                            , GeometryInstanceFlagsKHR
                                            , GeometryFlagBitsKHR(..)
                                            , GeometryFlagsKHR
                                            , BuildAccelerationStructureFlagBitsKHR(..)
                                            , BuildAccelerationStructureFlagsKHR
                                            , CopyAccelerationStructureModeKHR(..)
                                            , AccelerationStructureTypeKHR(..)
                                            , GeometryTypeKHR(..)
                                            , RayTracingShaderGroupTypeKHR(..)
                                            , MemoryRequirements2KHR
                                            , SHADER_UNUSED_KHR
                                            , pattern SHADER_UNUSED_KHR
                                            ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Exception.Base (bracket)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable (traverse_)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytesAligned)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import GHC.Base (when)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showsPrec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
import Foreign.C.Types (CSize(..))
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (FromCStruct(..))
import Vulkan.CStruct (ToCStruct)
import Vulkan.CStruct (ToCStruct(..))
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Control.Monad.IO.Class (MonadIO)
import Data.String (IsString)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Foreign.C.Types (CSize)
import Foreign.C.Types (CSize(CSize))
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Data.Int (Int32)
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Word (Word64)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.Pipeline (destroyPipeline)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (getRayTracingShaderGroupHandlesKHR)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AabbPositionsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR)
import Vulkan.Extensions.Handles (AccelerationStructureNV)
import Vulkan.Extensions.Handles (AccelerationStructureNV(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR)
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.FundamentalTypes (Bool32(..))
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Dynamic (DeviceCmds(pVkBindAccelerationStructureMemoryNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBuildAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdCopyAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysNV))
import Vulkan.Dynamic (DeviceCmds(pVkCmdWriteAccelerationStructuresPropertiesNV))
import Vulkan.Dynamic (DeviceCmds(pVkCompileDeferredNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesNV))
import Vulkan.Dynamic (DeviceCmds(pVkDestroyAccelerationStructureNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureHandleNV))
import Vulkan.Dynamic (DeviceCmds(pVkGetAccelerationStructureMemoryRequirementsNV))
import Vulkan.Core10.Handles (DeviceMemory)
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfoEXT)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Handles (QueryPool)
import Vulkan.Core10.Handles (QueryPool(..))
import Vulkan.Core10.Enums.QueryType (QueryType)
import Vulkan.Core10.Enums.QueryType (QueryType(..))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR)
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR))
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlags)
import Vulkan.Core10.Enums.AccessFlagBits (AccessFlagBits(ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR))
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlags)
import Vulkan.Core10.Enums.BufferUsageFlagBits (BufferUsageFlagBits(BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR(GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR(GEOMETRY_OPAQUE_BIT_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(GEOMETRY_TYPE_AABBS_KHR))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(GEOMETRY_TYPE_TRIANGLES_KHR))
import Vulkan.Core10.Enums.IndexType (IndexType(INDEX_TYPE_NONE_KHR))
import Vulkan.Core10.Enums.PipelineBindPoint (PipelineBindPoint(PIPELINE_BIND_POINT_RAY_TRACING_KHR))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR))
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlags)
import Vulkan.Core10.Enums.PipelineStageFlagBits (PipelineStageFlagBits(PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_ANY_HIT_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_CALLABLE_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_CLOSEST_HIT_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_INTERSECTION_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_MISS_BIT_KHR))
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlags)
import Vulkan.Core10.Enums.ShaderStageFlagBits (ShaderStageFlagBits(SHADER_STAGE_RAYGEN_BIT_KHR))
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_AABB_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (getRayTracingShaderGroupHandlesKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (AabbPositionsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureInstanceKHR(..))
import Vulkan.Extensions.Handles (AccelerationStructureNV(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (AccelerationStructureTypeKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (BuildAccelerationStructureFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (CopyAccelerationStructureModeKHR(..))
import Vulkan.Extensions.VK_EXT_debug_report (DebugReportObjectTypeEXT(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagBitsKHR(..))
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryInstanceFlagsKHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (GeometryTypeKHR(..))
import Vulkan.Extensions.VK_KHR_get_memory_requirements2 (MemoryRequirements2KHR)
import Vulkan.Extensions.VK_KHR_ray_tracing_pipeline (RayTracingShaderGroupTypeKHR(..))
import Vulkan.Core10.APIConstants (SHADER_UNUSED_KHR)
import Vulkan.Extensions.VK_KHR_acceleration_structure (TransformMatrixKHR(..))
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCompileDeferredNV
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> IO Result

-- | vkCompileDeferredNV - Deferred compilation of shaders
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
compileDeferredNV :: forall io
                   . (MonadIO io)
                  => -- | @device@ is the logical device containing the ray tracing pipeline.
                     --
                     -- #VUID-vkCompileDeferredNV-device-parameter# @device@ /must/ be a valid
                     -- 'Vulkan.Core10.Handles.Device' handle
                     Device
                  -> -- | @pipeline@ is the ray tracing pipeline object containing the shaders.
                     --
                     -- #VUID-vkCompileDeferredNV-pipeline-04621# @pipeline@ /must/ be a ray
                     -- tracing pipeline
                     --
                     -- #VUID-vkCompileDeferredNV-pipeline-02237# @pipeline@ /must/ have been
                     -- created with
                     -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEFER_COMPILE_BIT_NV'
                     --
                     -- #VUID-vkCompileDeferredNV-pipeline-parameter# @pipeline@ /must/ be a
                     -- valid 'Vulkan.Core10.Handles.Pipeline' handle
                     --
                     -- #VUID-vkCompileDeferredNV-pipeline-parent# @pipeline@ /must/ have been
                     -- created, allocated, or retrieved from @device@
                     Pipeline
                  -> -- | @shader@ is the index of the shader to compile.
                     --
                     -- #VUID-vkCompileDeferredNV-shader-02238# @shader@ /must/ not have been
                     -- called as a deferred compile before
                     ("shader" ::: Word32)
                  -> io ()
compileDeferredNV device pipeline shader = liftIO $ do
  let vkCompileDeferredNVPtr = pVkCompileDeferredNV (deviceCmds (device :: Device))
  unless (vkCompileDeferredNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCompileDeferredNV is null" Nothing Nothing
  let vkCompileDeferredNV' = mkVkCompileDeferredNV vkCompileDeferredNVPtr
  r <- traceAroundEvent "vkCompileDeferredNV" (vkCompileDeferredNV' (deviceHandle (device)) (pipeline) (shader))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureCreateInfoNV -> Ptr AllocationCallbacks -> Ptr AccelerationStructureNV -> IO Result) -> Ptr Device_T -> Ptr AccelerationStructureCreateInfoNV -> Ptr AllocationCallbacks -> Ptr AccelerationStructureNV -> IO Result

-- | vkCreateAccelerationStructureNV - Create a new acceleration structure
-- object
--
-- = Description
--
-- Similar to other objects in Vulkan, the acceleration structure creation
-- merely creates an object with a specific “shape” as specified by the
-- information in 'AccelerationStructureInfoNV' and @compactedSize@ in
-- @pCreateInfo@. Populating the data in the object after allocating and
-- binding memory is done with 'cmdBuildAccelerationStructureNV' and
-- 'cmdCopyAccelerationStructureNV'.
--
-- Acceleration structure creation uses the count and type information from
-- the geometries, but does not use the data references in the structures.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateAccelerationStructureNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateAccelerationStructureNV-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'AccelerationStructureCreateInfoNV' structure
--
-- -   #VUID-vkCreateAccelerationStructureNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateAccelerationStructureNV-pAccelerationStructure-parameter#
--     @pAccelerationStructure@ /must/ be a valid pointer to a
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoNV',
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
createAccelerationStructureNV :: forall io
                               . (MonadIO io)
                              => -- | @device@ is the logical device that creates the buffer object.
                                 Device
                              -> -- | @pCreateInfo@ is a pointer to a 'AccelerationStructureCreateInfoNV'
                                 -- structure containing parameters affecting creation of the acceleration
                                 -- structure.
                                 AccelerationStructureCreateInfoNV
                              -> -- | @pAllocator@ controls host memory allocation as described in the
                                 -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                 -- chapter.
                                 ("allocator" ::: Maybe AllocationCallbacks)
                              -> io (AccelerationStructureNV)
createAccelerationStructureNV device createInfo allocator = liftIO . evalContT $ do
  let vkCreateAccelerationStructureNVPtr = pVkCreateAccelerationStructureNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateAccelerationStructureNV is null" Nothing Nothing
  let vkCreateAccelerationStructureNV' = mkVkCreateAccelerationStructureNV vkCreateAccelerationStructureNVPtr
  pCreateInfo <- ContT $ withCStruct (createInfo)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPAccelerationStructure <- ContT $ bracket (callocBytes @AccelerationStructureNV 8) free
  r <- lift $ traceAroundEvent "vkCreateAccelerationStructureNV" (vkCreateAccelerationStructureNV' (deviceHandle (device)) pCreateInfo pAllocator (pPAccelerationStructure))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pAccelerationStructure <- lift $ peek @AccelerationStructureNV pPAccelerationStructure
  pure $ (pAccelerationStructure)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createAccelerationStructureNV' and 'destroyAccelerationStructureNV'
--
-- To ensure that 'destroyAccelerationStructureNV' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withAccelerationStructureNV :: forall io r . MonadIO io => Device -> AccelerationStructureCreateInfoNV -> Maybe AllocationCallbacks -> (io AccelerationStructureNV -> (AccelerationStructureNV -> io ()) -> r) -> r
withAccelerationStructureNV device pCreateInfo pAllocator b =
  b (createAccelerationStructureNV device pCreateInfo pAllocator)
    (\(o0) -> destroyAccelerationStructureNV device o0 pAllocator)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureNV
  :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> Ptr AllocationCallbacks -> IO ()) -> Ptr Device_T -> AccelerationStructureNV -> Ptr AllocationCallbacks -> IO ()

-- | vkDestroyAccelerationStructureNV - Destroy an acceleration structure
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkDestroyAccelerationStructureNV-accelerationStructure-03752#
--     All submitted commands that refer to @accelerationStructure@ /must/
--     have completed execution
--
-- -   #VUID-vkDestroyAccelerationStructureNV-accelerationStructure-03753#
--     If 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @accelerationStructure@ was created, a compatible set
--     of callbacks /must/ be provided here
--
-- -   #VUID-vkDestroyAccelerationStructureNV-accelerationStructure-03754#
--     If no 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks' were
--     provided when @accelerationStructure@ was created, @pAllocator@
--     /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkDestroyAccelerationStructureNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkDestroyAccelerationStructureNV-accelerationStructure-parameter#
--     If @accelerationStructure@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @accelerationStructure@
--     /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   #VUID-vkDestroyAccelerationStructureNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkDestroyAccelerationStructureNV-accelerationStructure-parent#
--     If @accelerationStructure@ is a valid handle, it /must/ have been
--     created, allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @accelerationStructure@ /must/ be externally
--     synchronized
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device'
destroyAccelerationStructureNV :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that destroys the buffer.
                                  Device
                               -> -- | @accelerationStructure@ is the acceleration structure to destroy.
                                  AccelerationStructureNV
                               -> -- | @pAllocator@ controls host memory allocation as described in the
                                  -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                  -- chapter.
                                  ("allocator" ::: Maybe AllocationCallbacks)
                               -> io ()
destroyAccelerationStructureNV device accelerationStructure allocator = liftIO . evalContT $ do
  let vkDestroyAccelerationStructureNVPtr = pVkDestroyAccelerationStructureNV (deviceCmds (device :: Device))
  lift $ unless (vkDestroyAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkDestroyAccelerationStructureNV is null" Nothing Nothing
  let vkDestroyAccelerationStructureNV' = mkVkDestroyAccelerationStructureNV vkDestroyAccelerationStructureNVPtr
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  lift $ traceAroundEvent "vkDestroyAccelerationStructureNV" (vkDestroyAccelerationStructureNV' (deviceHandle (device)) (accelerationStructure) pAllocator)
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsNV
  :: FunPtr (Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2KHR) -> IO ()) -> Ptr Device_T -> Ptr AccelerationStructureMemoryRequirementsInfoNV -> Ptr (SomeStruct MemoryRequirements2KHR) -> IO ()

-- | vkGetAccelerationStructureMemoryRequirementsNV - Get acceleration
-- structure memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoNV',
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Extensions.VK_KHR_get_memory_requirements2.MemoryRequirements2KHR'
getAccelerationStructureMemoryRequirementsNV :: forall a io
                                              . (Extendss MemoryRequirements2KHR a, PokeChain a, PeekChain a, MonadIO io)
                                             => -- | @device@ is the logical device on which the acceleration structure was
                                                -- created.
                                                --
                                                -- #VUID-vkGetAccelerationStructureMemoryRequirementsNV-device-parameter#
                                                -- @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                                Device
                                             -> -- | @pInfo@ specifies the acceleration structure to get memory requirements
                                                -- for.
                                                --
                                                -- #VUID-vkGetAccelerationStructureMemoryRequirementsNV-pInfo-parameter#
                                                -- @pInfo@ /must/ be a valid pointer to a valid
                                                -- 'AccelerationStructureMemoryRequirementsInfoNV' structure
                                                AccelerationStructureMemoryRequirementsInfoNV
                                             -> io (MemoryRequirements2KHR a)
getAccelerationStructureMemoryRequirementsNV device info = liftIO . evalContT $ do
  let vkGetAccelerationStructureMemoryRequirementsNVPtr = pVkGetAccelerationStructureMemoryRequirementsNV (deviceCmds (device :: Device))
  lift $ unless (vkGetAccelerationStructureMemoryRequirementsNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureMemoryRequirementsNV is null" Nothing Nothing
  let vkGetAccelerationStructureMemoryRequirementsNV' = mkVkGetAccelerationStructureMemoryRequirementsNV vkGetAccelerationStructureMemoryRequirementsNVPtr
  pInfo <- ContT $ withCStruct (info)
  pPMemoryRequirements <- ContT (withZeroCStruct @(MemoryRequirements2KHR _))
  lift $ traceAroundEvent "vkGetAccelerationStructureMemoryRequirementsNV" (vkGetAccelerationStructureMemoryRequirementsNV' (deviceHandle (device)) pInfo (forgetExtensions (pPMemoryRequirements)))
  pMemoryRequirements <- lift $ peekCStruct @(MemoryRequirements2KHR _) pPMemoryRequirements
  pure $ (pMemoryRequirements)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindAccelerationStructureMemoryNV
  :: FunPtr (Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoNV -> IO Result) -> Ptr Device_T -> Word32 -> Ptr BindAccelerationStructureMemoryInfoNV -> IO Result

-- | vkBindAccelerationStructureMemoryNV - Bind acceleration structure memory
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'BindAccelerationStructureMemoryInfoNV', 'Vulkan.Core10.Handles.Device'
bindAccelerationStructureMemoryNV :: forall io
                                   . (MonadIO io)
                                  => -- | @device@ is the logical device that owns the acceleration structures and
                                     -- memory.
                                     --
                                     -- #VUID-vkBindAccelerationStructureMemoryNV-device-parameter# @device@
                                     -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                     Device
                                  -> -- | @pBindInfos@ is a pointer to an array of
                                     -- 'BindAccelerationStructureMemoryInfoNV' structures describing
                                     -- acceleration structures and memory to bind.
                                     --
                                     -- #VUID-vkBindAccelerationStructureMemoryNV-pBindInfos-parameter#
                                     -- @pBindInfos@ /must/ be a valid pointer to an array of @bindInfoCount@
                                     -- valid 'BindAccelerationStructureMemoryInfoNV' structures
                                     ("bindInfos" ::: Vector BindAccelerationStructureMemoryInfoNV)
                                  -> io ()
bindAccelerationStructureMemoryNV device bindInfos = liftIO . evalContT $ do
  let vkBindAccelerationStructureMemoryNVPtr = pVkBindAccelerationStructureMemoryNV (deviceCmds (device :: Device))
  lift $ unless (vkBindAccelerationStructureMemoryNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkBindAccelerationStructureMemoryNV is null" Nothing Nothing
  let vkBindAccelerationStructureMemoryNV' = mkVkBindAccelerationStructureMemoryNV vkBindAccelerationStructureMemoryNVPtr
  pPBindInfos <- ContT $ allocaBytesAligned @BindAccelerationStructureMemoryInfoNV ((Data.Vector.length (bindInfos)) * 56) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeCStruct (pPBindInfos `plusPtr` (56 * (i)) :: Ptr BindAccelerationStructureMemoryInfoNV) (e) . ($ ())) (bindInfos)
  r <- lift $ traceAroundEvent "vkBindAccelerationStructureMemoryNV" (vkBindAccelerationStructureMemoryNV' (deviceHandle (device)) ((fromIntegral (Data.Vector.length $ (bindInfos)) :: Word32)) (pPBindInfos))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> AccelerationStructureNV -> AccelerationStructureNV -> CopyAccelerationStructureModeKHR -> IO ()) -> Ptr CommandBuffer_T -> AccelerationStructureNV -> AccelerationStructureNV -> CopyAccelerationStructureModeKHR -> IO ()

-- | vkCmdCopyAccelerationStructureNV - Copy an acceleration structure
--
-- = Description
--
-- Accesses to @src@ and @dst@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'
-- or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'
-- as appropriate.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-mode-03410# @mode@ /must/ be
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR'
--     or
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR'
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-src-03411# If @mode@ is
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR',
--     @src@ /must/ have been built with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR'
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-buffer-03718# The @buffer@
--     used to create @src@ /must/ be bound to device memory
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-buffer-03719# The @buffer@
--     used to create @dst@ /must/ be bound to device memory
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-dst-parameter# @dst@ /must/
--     be a valid 'Vulkan.Extensions.Handles.AccelerationStructureNV'
--     handle
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-src-parameter# @src@ /must/
--     be a valid 'Vulkan.Extensions.Handles.AccelerationStructureNV'
--     handle
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-mode-parameter# @mode@ /must/
--     be a valid
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyAccelerationStructureModeKHR'
--     value
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdCopyAccelerationStructureNV-commonparent# Each of
--     @commandBuffer@, @dst@, and @src@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyAccelerationStructureModeKHR'
cmdCopyAccelerationStructureNV :: forall io
                                . (MonadIO io)
                               => -- | @commandBuffer@ is the command buffer into which the command will be
                                  -- recorded.
                                  CommandBuffer
                               -> -- | @dst@ is the target acceleration structure for the copy.
                                  ("dst" ::: AccelerationStructureNV)
                               -> -- | @src@ is the source acceleration structure for the copy.
                                  ("src" ::: AccelerationStructureNV)
                               -> -- | @mode@ is a
                                  -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.CopyAccelerationStructureModeKHR'
                                  -- value specifying additional operations to perform during the copy.
                                  CopyAccelerationStructureModeKHR
                               -> io ()
cmdCopyAccelerationStructureNV commandBuffer dst src mode = liftIO $ do
  let vkCmdCopyAccelerationStructureNVPtr = pVkCmdCopyAccelerationStructureNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdCopyAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdCopyAccelerationStructureNV is null" Nothing Nothing
  let vkCmdCopyAccelerationStructureNV' = mkVkCmdCopyAccelerationStructureNV vkCmdCopyAccelerationStructureNVPtr
  traceAroundEvent "vkCmdCopyAccelerationStructureNV" (vkCmdCopyAccelerationStructureNV' (commandBufferHandle (commandBuffer)) (dst) (src) (mode))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesNV
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureNV -> QueryType -> QueryPool -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> Ptr AccelerationStructureNV -> QueryType -> QueryPool -> Word32 -> IO ()

-- | vkCmdWriteAccelerationStructuresPropertiesNV - Write acceleration
-- structure result parameters to query results.
--
-- = Description
--
-- Accesses to any of the acceleration structures listed in
-- @pAccelerationStructures@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-queryPool-03755#
--     @queryPool@ /must/ have been created with a @queryType@ matching
--     @queryType@
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-queryPool-03756#
--     The queries identified by @queryPool@ and @firstQuery@ /must/ be
--     /unavailable/
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-accelerationStructure-03757#
--     @accelerationStructure@ /must/ be bound completely and contiguously
--     to a single 'Vulkan.Core10.Handles.DeviceMemory' object via
--     'bindAccelerationStructureMemoryNV'
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-accelerationStructures-03431#
--     All acceleration structures in @pAccelerationStructures@ /must/ have
--     been built with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR'
--     if @queryType@ is
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-queryType-03432#
--     @queryType@ /must/ be
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR'
--     or
--     'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-pAccelerationStructures-parameter#
--     @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' handles
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-queryType-parameter#
--     @queryType@ /must/ be a valid
--     'Vulkan.Core10.Enums.QueryType.QueryType' value
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-queryPool-parameter#
--     @queryPool@ /must/ be a valid 'Vulkan.Core10.Handles.QueryPool'
--     handle
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-renderpass# This
--     command /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-accelerationStructureCount-arraylength#
--     @accelerationStructureCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCmdWriteAccelerationStructuresPropertiesNV-commonparent#
--     Each of @commandBuffer@, @queryPool@, and the elements of
--     @pAccelerationStructures@ /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.Handles.QueryPool',
-- 'Vulkan.Core10.Enums.QueryType.QueryType'
cmdWriteAccelerationStructuresPropertiesNV :: forall io
                                            . (MonadIO io)
                                           => -- | @commandBuffer@ is the command buffer into which the command will be
                                              -- recorded.
                                              CommandBuffer
                                           -> -- | @pAccelerationStructures@ is a pointer to an array of existing
                                              -- previously built acceleration structures.
                                              ("accelerationStructures" ::: Vector AccelerationStructureNV)
                                           -> -- | @queryType@ is a 'Vulkan.Core10.Enums.QueryType.QueryType' value
                                              -- specifying the type of queries managed by the pool.
                                              QueryType
                                           -> -- | @queryPool@ is the query pool that will manage the results of the query.
                                              QueryPool
                                           -> -- | @firstQuery@ is the first query index within the query pool that will
                                              -- contain the @accelerationStructureCount@ number of results.
                                              ("firstQuery" ::: Word32)
                                           -> io ()
cmdWriteAccelerationStructuresPropertiesNV commandBuffer accelerationStructures queryType queryPool firstQuery = liftIO . evalContT $ do
  let vkCmdWriteAccelerationStructuresPropertiesNVPtr = pVkCmdWriteAccelerationStructuresPropertiesNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdWriteAccelerationStructuresPropertiesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdWriteAccelerationStructuresPropertiesNV is null" Nothing Nothing
  let vkCmdWriteAccelerationStructuresPropertiesNV' = mkVkCmdWriteAccelerationStructuresPropertiesNV vkCmdWriteAccelerationStructuresPropertiesNVPtr
  pPAccelerationStructures <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (accelerationStructures)) * 8) 8
  lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (accelerationStructures)
  lift $ traceAroundEvent "vkCmdWriteAccelerationStructuresPropertiesNV" (vkCmdWriteAccelerationStructuresPropertiesNV' (commandBufferHandle (commandBuffer)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32)) (pPAccelerationStructures) (queryType) (queryPool) (firstQuery))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureNV
  :: FunPtr (Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureNV -> AccelerationStructureNV -> Buffer -> DeviceSize -> IO ()) -> Ptr CommandBuffer_T -> Ptr AccelerationStructureInfoNV -> Buffer -> DeviceSize -> Bool32 -> AccelerationStructureNV -> AccelerationStructureNV -> Buffer -> DeviceSize -> IO ()

-- | vkCmdBuildAccelerationStructureNV - Build an acceleration structure
--
-- = Description
--
-- Accesses to @dst@, @src@, and @scratch@ /must/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-dependencies synchronized>
-- with the
-- 'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR'
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages pipeline stage>
-- and an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-access-types access type>
-- of
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR'
-- or
-- 'Vulkan.Core10.Enums.AccessFlagBits.ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR'.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-geometryCount-02241#
--     @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-dst-02488# @dst@ /must/ have
--     been created with compatible 'AccelerationStructureInfoNV' where
--     'AccelerationStructureInfoNV'::@type@ and
--     'AccelerationStructureInfoNV'::@flags@ are identical,
--     'AccelerationStructureInfoNV'::@instanceCount@ and
--     'AccelerationStructureInfoNV'::@geometryCount@ for @dst@ are greater
--     than or equal to the build size and each geometry in
--     'AccelerationStructureInfoNV'::@pGeometries@ for @dst@ has greater
--     than or equal to the number of vertices, indices, and AABBs
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-02489# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', @src@ /must/ not be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-02490# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', @src@ /must/ have been built
--     before with 'BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV' set
--     in 'AccelerationStructureInfoNV'::@flags@
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-02491# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.FALSE', the @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     'AccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'AccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-02492# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @size@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     'AccelerationStructureMemoryRequirementsInfoNV'::@accelerationStructure@
--     set to @dst@ and
--     'AccelerationStructureMemoryRequirementsInfoNV'::@type@ set to
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
--     /must/ be less than or equal to the size of @scratch@ minus
--     @scratchOffset@
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-scratch-03522# @scratch@
--     /must/ have been created with 'BUFFER_USAGE_RAY_TRACING_BIT_NV'
--     usage flag
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-instanceData-03523# If
--     @instanceData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @instanceData@ /must/ have been created with
--     'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-accelerationStructureReference-03786#
--     Each
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureInstanceKHR'::@accelerationStructureReference@
--     value in @instanceData@ /must/ be a valid device address containing
--     a value obtained from 'getAccelerationStructureHandleNV'
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-03524# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then objects that were
--     previously active /must/ not be made inactive as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims>
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-03525# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', then objects that were
--     previously inactive /must/ not be made active as per
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-inactive-prims>
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-update-03526# If @update@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE', the @src@ and @dst@ objects
--     /must/ either be the same object or not have any
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing memory aliasing>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-pInfo-parameter# @pInfo@
--     /must/ be a valid pointer to a valid 'AccelerationStructureInfoNV'
--     structure
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-instanceData-parameter# If
--     @instanceData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @instanceData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-dst-parameter# @dst@ /must/
--     be a valid 'Vulkan.Extensions.Handles.AccelerationStructureNV'
--     handle
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-src-parameter# If @src@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @src@ /must/ be a
--     valid 'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-scratch-parameter# @scratch@
--     /must/ be a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-renderpass# This command
--     /must/ only be called outside of a render pass instance
--
-- -   #VUID-vkCmdBuildAccelerationStructureNV-commonparent# Each of
--     @commandBuffer@, @dst@, @instanceData@, @scratch@, and @src@ that
--     are valid handles of non-ignored parameters /must/ have been
--     created, allocated, or retrieved from the same
--     'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'AccelerationStructureInfoNV',
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.FundamentalTypes.Bool32', 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdBuildAccelerationStructureNV :: forall io
                                 . (MonadIO io)
                                => -- | @commandBuffer@ is the command buffer into which the command will be
                                   -- recorded.
                                   CommandBuffer
                                -> -- | @pInfo@ contains the shared information for the acceleration structure’s
                                   -- structure.
                                   AccelerationStructureInfoNV
                                -> -- | @instanceData@ is the buffer containing an array of
                                   -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureInstanceKHR'
                                   -- structures defining acceleration structures. This parameter /must/ be
                                   -- @NULL@ for bottom level acceleration structures.
                                   ("instanceData" ::: Buffer)
                                -> -- | @instanceOffset@ is the offset in bytes (relative to the start of
                                   -- @instanceData@) at which the instance data is located.
                                   ("instanceOffset" ::: DeviceSize)
                                -> -- | @update@ specifies whether to update the @dst@ acceleration structure
                                   -- with the data in @src@.
                                   ("update" ::: Bool)
                                -> -- | @dst@ is a pointer to the target acceleration structure for the build.
                                   ("dst" ::: AccelerationStructureNV)
                                -> -- | @src@ is a pointer to an existing acceleration structure that is to be
                                   -- used to update the @dst@ acceleration structure.
                                   ("src" ::: AccelerationStructureNV)
                                -> -- | @scratch@ is the 'Vulkan.Core10.Handles.Buffer' that will be used as
                                   -- scratch memory for the build.
                                   ("scratch" ::: Buffer)
                                -> -- | @scratchOffset@ is the offset in bytes relative to the start of
                                   -- @scratch@ that will be used as a scratch memory.
                                   ("scratchOffset" ::: DeviceSize)
                                -> io ()
cmdBuildAccelerationStructureNV commandBuffer info instanceData instanceOffset update dst src scratch scratchOffset = liftIO . evalContT $ do
  let vkCmdBuildAccelerationStructureNVPtr = pVkCmdBuildAccelerationStructureNV (deviceCmds (commandBuffer :: CommandBuffer))
  lift $ unless (vkCmdBuildAccelerationStructureNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBuildAccelerationStructureNV is null" Nothing Nothing
  let vkCmdBuildAccelerationStructureNV' = mkVkCmdBuildAccelerationStructureNV vkCmdBuildAccelerationStructureNVPtr
  pInfo <- ContT $ withCStruct (info)
  lift $ traceAroundEvent "vkCmdBuildAccelerationStructureNV" (vkCmdBuildAccelerationStructureNV' (commandBufferHandle (commandBuffer)) pInfo (instanceData) (instanceOffset) (boolToBool32 (update)) (dst) (src) (scratch) (scratchOffset))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysNV
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Buffer -> DeviceSize -> DeviceSize -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdTraceRaysNV - Initialize a ray tracing dispatch
--
-- = Description
--
-- When the command is executed, a ray generation group of @width@ ×
-- @height@ × @depth@ rays is assembled.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdTraceRaysNV-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysNV-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdTraceRaysNV-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdTraceRaysNV-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdTraceRaysNV-filterCubicMinmax-02695# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' with a
--     reduction mode of either
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MIN'
--     or
--     'Vulkan.Core12.Enums.SamplerReductionMode.SAMPLER_REDUCTION_MODE_MAX'
--     as a result of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering together with minmax filtering, as
--     specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubicMinmax@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdTraceRaysNV-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdTraceRaysNV-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysNV-None-02698# For each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysNV-None-02699# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdTraceRaysNV-None-02700# A valid pipeline /must/ be bound
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdTraceRaysNV-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set for @commandBuffer@, and done so after any
--     previously bound pipeline with the corresponding state not specified
--     as dynamic
--
-- -   #VUID-vkCmdTraceRaysNV-None-02859# There /must/ not have been any
--     calls to dynamic state setting commands for any state not specified
--     as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command, since that pipeline
--     was bound
--
-- -   #VUID-vkCmdTraceRaysNV-None-02702# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used to sample from any
--     'Vulkan.Core10.Handles.Image' with a
--     'Vulkan.Core10.Handles.ImageView' of the type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_3D',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_1D_ARRAY',
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY', in
--     any shader stage
--
-- -   #VUID-vkCmdTraceRaysNV-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysNV-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysNV-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysNV-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysNV-commandBuffer-02707# If @commandBuffer@ is an
--     unprotected command buffer, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdTraceRaysNV-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdTraceRaysNV-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format.
--
-- -   #VUID-vkCmdTraceRaysNV-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdTraceRaysNV-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdTraceRaysNV-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit channel width
--     is accessed as a result of this command, the @SampledType@ of the
--     @OpTypeImage@ operand of that instruction /must/ have a @Width@ of
--     64.
--
-- -   #VUID-vkCmdTraceRaysNV-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a channel width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32.
--
-- -   #VUID-vkCmdTraceRaysNV-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdTraceRaysNV-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command.
--
-- -   #VUID-vkCmdTraceRaysNV-None-03429# Any shader group handle
--     referenced by this call /must/ have been queried from the currently
--     bound ray tracing shader pipeline
--
-- -   #VUID-vkCmdTraceRaysNV-commandBuffer-04624# @commandBuffer@ /must/
--     not be a protected command buffer
--
-- -   #VUID-vkCmdTraceRaysNV-maxRecursionDepth-03625# This command /must/
--     not cause a trace ray instruction to be executed from a shader
--     invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxRecursionDepth@ used to create the
--     bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysNV-raygenShaderBindingTableBuffer-04042# If
--     @raygenShaderBindingTableBuffer@ is non-sparse then it /must/ be
--     bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysNV-raygenShaderBindingOffset-02455#
--     @raygenShaderBindingOffset@ /must/ be less than the size of
--     @raygenShaderBindingTableBuffer@
--
-- -   #VUID-vkCmdTraceRaysNV-raygenShaderBindingOffset-02456#
--     @raygenShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysNV-missShaderBindingTableBuffer-04043# If
--     @missShaderBindingTableBuffer@ is non-sparse then it /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysNV-missShaderBindingOffset-02457#
--     @missShaderBindingOffset@ /must/ be less than the size of
--     @missShaderBindingTableBuffer@
--
-- -   #VUID-vkCmdTraceRaysNV-missShaderBindingOffset-02458#
--     @missShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysNV-hitShaderBindingTableBuffer-04044# If
--     @hitShaderBindingTableBuffer@ is non-sparse then it /must/ be bound
--     completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysNV-hitShaderBindingOffset-02459#
--     @hitShaderBindingOffset@ /must/ be less than the size of
--     @hitShaderBindingTableBuffer@
--
-- -   #VUID-vkCmdTraceRaysNV-hitShaderBindingOffset-02460#
--     @hitShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysNV-callableShaderBindingTableBuffer-04045# If
--     @callableShaderBindingTableBuffer@ is non-sparse then it /must/ be
--     bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysNV-callableShaderBindingOffset-02461#
--     @callableShaderBindingOffset@ /must/ be less than the size of
--     @callableShaderBindingTableBuffer@
--
-- -   #VUID-vkCmdTraceRaysNV-callableShaderBindingOffset-02462#
--     @callableShaderBindingOffset@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysNV-missShaderBindingStride-02463#
--     @missShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   #VUID-vkCmdTraceRaysNV-hitShaderBindingStride-02464#
--     @hitShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   #VUID-vkCmdTraceRaysNV-callableShaderBindingStride-02465#
--     @callableShaderBindingStride@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPropertiesNV'::@shaderGroupHandleSize@
--
-- -   #VUID-vkCmdTraceRaysNV-missShaderBindingStride-02466#
--     @missShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysNV-hitShaderBindingStride-02467#
--     @hitShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysNV-callableShaderBindingStride-02468#
--     @callableShaderBindingStride@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysNV-width-02469# @width@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--
-- -   #VUID-vkCmdTraceRaysNV-height-02470# @height@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--
-- -   #VUID-vkCmdTraceRaysNV-depth-02471# @depth@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdTraceRaysNV-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdTraceRaysNV-raygenShaderBindingTableBuffer-parameter#
--     @raygenShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdTraceRaysNV-missShaderBindingTableBuffer-parameter# If
--     @missShaderBindingTableBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @missShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdTraceRaysNV-hitShaderBindingTableBuffer-parameter# If
--     @hitShaderBindingTableBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @hitShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdTraceRaysNV-callableShaderBindingTableBuffer-parameter#
--     If @callableShaderBindingTableBuffer@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @callableShaderBindingTableBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdTraceRaysNV-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdTraceRaysNV-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdTraceRaysNV-renderpass# This command /must/ only be
--     called outside of a render pass instance
--
-- -   #VUID-vkCmdTraceRaysNV-commonparent# Each of
--     @callableShaderBindingTableBuffer@, @commandBuffer@,
--     @hitShaderBindingTableBuffer@, @missShaderBindingTableBuffer@, and
--     @raygenShaderBindingTableBuffer@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- == Host Synchronization
--
-- -   Host access to @commandBuffer@ /must/ be externally synchronized
--
-- -   Host access to the 'Vulkan.Core10.Handles.CommandPool' that
--     @commandBuffer@ was allocated from /must/ be externally synchronized
--
-- == Command Properties
--
-- \'
--
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#synchronization-pipeline-stages-types Pipeline Type> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+=====================================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |                                                                                                                                     |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |                                                                                                                                     |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+-------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize'
cmdTraceRaysNV :: forall io
                . (MonadIO io)
               => -- | @commandBuffer@ is the command buffer into which the command will be
                  -- recorded.
                  CommandBuffer
               -> -- | @raygenShaderBindingTableBuffer@ is the buffer object that holds the
                  -- shader binding table data for the ray generation shader stage.
                  ("raygenShaderBindingTableBuffer" ::: Buffer)
               -> -- | @raygenShaderBindingOffset@ is the offset in bytes (relative to
                  -- @raygenShaderBindingTableBuffer@) of the ray generation shader being
                  -- used for the trace.
                  ("raygenShaderBindingOffset" ::: DeviceSize)
               -> -- | @missShaderBindingTableBuffer@ is the buffer object that holds the
                  -- shader binding table data for the miss shader stage.
                  ("missShaderBindingTableBuffer" ::: Buffer)
               -> -- | @missShaderBindingOffset@ is the offset in bytes (relative to
                  -- @missShaderBindingTableBuffer@) of the miss shader being used for the
                  -- trace.
                  ("missShaderBindingOffset" ::: DeviceSize)
               -> -- | @missShaderBindingStride@ is the size in bytes of each shader binding
                  -- table record in @missShaderBindingTableBuffer@.
                  ("missShaderBindingStride" ::: DeviceSize)
               -> -- | @hitShaderBindingTableBuffer@ is the buffer object that holds the shader
                  -- binding table data for the hit shader stages.
                  ("hitShaderBindingTableBuffer" ::: Buffer)
               -> -- | @hitShaderBindingOffset@ is the offset in bytes (relative to
                  -- @hitShaderBindingTableBuffer@) of the hit shader group being used for
                  -- the trace.
                  ("hitShaderBindingOffset" ::: DeviceSize)
               -> -- | @hitShaderBindingStride@ is the size in bytes of each shader binding
                  -- table record in @hitShaderBindingTableBuffer@.
                  ("hitShaderBindingStride" ::: DeviceSize)
               -> -- | @callableShaderBindingTableBuffer@ is the buffer object that holds the
                  -- shader binding table data for the callable shader stage.
                  ("callableShaderBindingTableBuffer" ::: Buffer)
               -> -- | @callableShaderBindingOffset@ is the offset in bytes (relative to
                  -- @callableShaderBindingTableBuffer@) of the callable shader being used
                  -- for the trace.
                  ("callableShaderBindingOffset" ::: DeviceSize)
               -> -- | @callableShaderBindingStride@ is the size in bytes of each shader
                  -- binding table record in @callableShaderBindingTableBuffer@.
                  ("callableShaderBindingStride" ::: DeviceSize)
               -> -- | @width@ is the width of the ray trace query dimensions.
                  ("width" ::: Word32)
               -> -- | @height@ is height of the ray trace query dimensions.
                  ("height" ::: Word32)
               -> -- | @depth@ is depth of the ray trace query dimensions.
                  ("depth" ::: Word32)
               -> io ()
cmdTraceRaysNV commandBuffer raygenShaderBindingTableBuffer raygenShaderBindingOffset missShaderBindingTableBuffer missShaderBindingOffset missShaderBindingStride hitShaderBindingTableBuffer hitShaderBindingOffset hitShaderBindingStride callableShaderBindingTableBuffer callableShaderBindingOffset callableShaderBindingStride width height depth = liftIO $ do
  let vkCmdTraceRaysNVPtr = pVkCmdTraceRaysNV (deviceCmds (commandBuffer :: CommandBuffer))
  unless (vkCmdTraceRaysNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysNV is null" Nothing Nothing
  let vkCmdTraceRaysNV' = mkVkCmdTraceRaysNV vkCmdTraceRaysNVPtr
  traceAroundEvent "vkCmdTraceRaysNV" (vkCmdTraceRaysNV' (commandBufferHandle (commandBuffer)) (raygenShaderBindingTableBuffer) (raygenShaderBindingOffset) (missShaderBindingTableBuffer) (missShaderBindingOffset) (missShaderBindingStride) (hitShaderBindingTableBuffer) (hitShaderBindingOffset) (hitShaderBindingStride) (callableShaderBindingTableBuffer) (callableShaderBindingOffset) (callableShaderBindingStride) (width) (height) (depth))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureHandleNV
  :: FunPtr (Ptr Device_T -> AccelerationStructureNV -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> AccelerationStructureNV -> CSize -> Ptr () -> IO Result

-- | vkGetAccelerationStructureHandleNV - Get opaque acceleration structure
-- handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Handles.Device'
getAccelerationStructureHandleNV :: forall io
                                  . (MonadIO io)
                                 => -- | @device@ is the logical device that owns the acceleration structures.
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-device-parameter# @device@
                                    -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                    Device
                                 -> -- | @accelerationStructure@ is the acceleration structure.
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-accelerationStructure-02787#
                                    -- @accelerationStructure@ /must/ be bound completely and contiguously to a
                                    -- single 'Vulkan.Core10.Handles.DeviceMemory' object via
                                    -- 'bindAccelerationStructureMemoryNV'
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-accelerationStructure-parameter#
                                    -- @accelerationStructure@ /must/ be a valid
                                    -- 'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-accelerationStructure-parent#
                                    -- @accelerationStructure@ /must/ have been created, allocated, or
                                    -- retrieved from @device@
                                    AccelerationStructureNV
                                 -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-dataSize-02240# @dataSize@
                                    -- /must/ be large enough to contain the result of the query, as described
                                    -- above
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-dataSize-arraylength#
                                    -- @dataSize@ /must/ be greater than @0@
                                    ("dataSize" ::: Word64)
                                 -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                                    -- be written.
                                    --
                                    -- #VUID-vkGetAccelerationStructureHandleNV-pData-parameter# @pData@ /must/
                                    -- be a valid pointer to an array of @dataSize@ bytes
                                    ("data" ::: Ptr ())
                                 -> io ()
getAccelerationStructureHandleNV device accelerationStructure dataSize data' = liftIO $ do
  let vkGetAccelerationStructureHandleNVPtr = pVkGetAccelerationStructureHandleNV (deviceCmds (device :: Device))
  unless (vkGetAccelerationStructureHandleNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetAccelerationStructureHandleNV is null" Nothing Nothing
  let vkGetAccelerationStructureHandleNV' = mkVkGetAccelerationStructureHandleNV vkGetAccelerationStructureHandleNVPtr
  r <- traceAroundEvent "vkGetAccelerationStructureHandleNV" (vkGetAccelerationStructureHandleNV' (deviceHandle (device)) (accelerationStructure) (CSize (dataSize)) (data'))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesNV
  :: FunPtr (Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoNV) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoNV) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateRayTracingPipelinesNV - Creates a new ray tracing pipeline
-- object
--
-- == Valid Usage
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-flags-03415# If the @flags@
--     member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-flags-03416# If the @flags@
--     member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-flags-03816# @flags@ /must/ not
--     contain the
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.PIPELINE_CREATE_DISPATCH_BASE'
--     flag
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-pipelineCache-02903# If
--     @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT_EXT',
--     host access to @pipelineCache@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-pipelineCache-parameter# If
--     @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'RayTracingPipelineCreateInfoNV' structures
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-pPipelines-parameter#
--     @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCreateRayTracingPipelinesNV-pipelineCache-parent# If
--     @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_SHADER_NV'
--
-- = See Also
--
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Handles.PipelineCache', 'RayTracingPipelineCreateInfoNV'
createRayTracingPipelinesNV :: forall io
                             . (MonadIO io)
                            => -- | @device@ is the logical device that creates the ray tracing pipelines.
                               Device
                            -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                               -- indicating that pipeline caching is disabled, or the handle of a valid
                               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-cache pipeline cache>
                               -- object, in which case use of that cache is enabled for the duration of
                               -- the command.
                               PipelineCache
                            -> -- | @pCreateInfos@ is a pointer to an array of
                               -- 'RayTracingPipelineCreateInfoNV' structures.
                               ("createInfos" ::: Vector (SomeStruct RayTracingPipelineCreateInfoNV))
                            -> -- | @pAllocator@ controls host memory allocation as described in the
                               -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                               -- chapter.
                               ("allocator" ::: Maybe AllocationCallbacks)
                            -> io (Result, ("pipelines" ::: Vector Pipeline))
createRayTracingPipelinesNV device pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateRayTracingPipelinesNVPtr = pVkCreateRayTracingPipelinesNV (deviceCmds (device :: Device))
  lift $ unless (vkCreateRayTracingPipelinesNVPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRayTracingPipelinesNV is null" Nothing Nothing
  let vkCreateRayTracingPipelinesNV' = mkVkCreateRayTracingPipelinesNV vkCreateRayTracingPipelinesNVPtr
  pPCreateInfos <- ContT $ allocaBytesAligned @(RayTracingPipelineCreateInfoNV _) ((Data.Vector.length (createInfos)) * 80) 8
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (80 * (i)) :: Ptr (RayTracingPipelineCreateInfoNV _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateRayTracingPipelinesNV" (vkCreateRayTracingPipelinesNV' (deviceHandle (device)) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPPipelines))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createRayTracingPipelinesNV' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withRayTracingPipelinesNV :: forall io r . MonadIO io => Device -> PipelineCache -> Vector (SomeStruct RayTracingPipelineCreateInfoNV) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withRayTracingPipelinesNV device pipelineCache pCreateInfos pAllocator b =
  b (createRayTracingPipelinesNV device pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device o1Elem pAllocator) o1)


-- No documentation found for TopLevel "VK_SHADER_STAGE_RAYGEN_BIT_NV"
pattern SHADER_STAGE_RAYGEN_BIT_NV = SHADER_STAGE_RAYGEN_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_ANY_HIT_BIT_NV"
pattern SHADER_STAGE_ANY_HIT_BIT_NV = SHADER_STAGE_ANY_HIT_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_CLOSEST_HIT_BIT_NV"
pattern SHADER_STAGE_CLOSEST_HIT_BIT_NV = SHADER_STAGE_CLOSEST_HIT_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_MISS_BIT_NV"
pattern SHADER_STAGE_MISS_BIT_NV = SHADER_STAGE_MISS_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_INTERSECTION_BIT_NV"
pattern SHADER_STAGE_INTERSECTION_BIT_NV = SHADER_STAGE_INTERSECTION_BIT_KHR


-- No documentation found for TopLevel "VK_SHADER_STAGE_CALLABLE_BIT_NV"
pattern SHADER_STAGE_CALLABLE_BIT_NV = SHADER_STAGE_CALLABLE_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV"
pattern PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_NV = PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV"
pattern PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_NV = PIPELINE_STAGE_ACCELERATION_STRUCTURE_BUILD_BIT_KHR


-- No documentation found for TopLevel "VK_BUFFER_USAGE_RAY_TRACING_BIT_NV"
pattern BUFFER_USAGE_RAY_TRACING_BIT_NV = BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR


-- No documentation found for TopLevel "VK_PIPELINE_BIND_POINT_RAY_TRACING_NV"
pattern PIPELINE_BIND_POINT_RAY_TRACING_NV = PIPELINE_BIND_POINT_RAY_TRACING_KHR


-- No documentation found for TopLevel "VK_ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_READ_BIT_NV = ACCESS_ACCELERATION_STRUCTURE_READ_BIT_KHR


-- No documentation found for TopLevel "VK_ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV"
pattern ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_NV = ACCESS_ACCELERATION_STRUCTURE_WRITE_BIT_KHR


-- No documentation found for TopLevel "VK_INDEX_TYPE_NONE_NV"
pattern INDEX_TYPE_NONE_NV = INDEX_TYPE_NONE_KHR


-- No documentation found for TopLevel "VK_RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV = RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR


-- No documentation found for TopLevel "VK_RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV = RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR


-- No documentation found for TopLevel "VK_RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV"
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV = RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_TYPE_TRIANGLES_NV"
pattern GEOMETRY_TYPE_TRIANGLES_NV = GEOMETRY_TYPE_TRIANGLES_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_TYPE_AABBS_NV"
pattern GEOMETRY_TYPE_AABBS_NV = GEOMETRY_TYPE_AABBS_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV"
pattern ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV = ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR


-- No documentation found for TopLevel "VK_ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV"
pattern ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV = ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_OPAQUE_BIT_NV"
pattern GEOMETRY_OPAQUE_BIT_NV = GEOMETRY_OPAQUE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV"
pattern GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_NV = GEOMETRY_NO_DUPLICATE_ANY_HIT_INVOCATION_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV"
pattern GEOMETRY_INSTANCE_TRIANGLE_CULL_DISABLE_BIT_NV = GEOMETRY_INSTANCE_TRIANGLE_FACING_CULL_DISABLE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV"
pattern GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_NV = GEOMETRY_INSTANCE_TRIANGLE_FRONT_COUNTERCLOCKWISE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV"
pattern GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_NV = GEOMETRY_INSTANCE_FORCE_OPAQUE_BIT_KHR


-- No documentation found for TopLevel "VK_GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV"
pattern GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_NV = GEOMETRY_INSTANCE_FORCE_NO_OPAQUE_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_NV = BUILD_ACCELERATION_STRUCTURE_ALLOW_UPDATE_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_NV = BUILD_ACCELERATION_STRUCTURE_ALLOW_COMPACTION_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV = BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_KHR


-- No documentation found for TopLevel "VK_BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV"
pattern BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_NV = BUILD_ACCELERATION_STRUCTURE_LOW_MEMORY_BIT_KHR


-- No documentation found for TopLevel "VK_COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV"
pattern COPY_ACCELERATION_STRUCTURE_MODE_CLONE_NV = COPY_ACCELERATION_STRUCTURE_MODE_CLONE_KHR


-- No documentation found for TopLevel "VK_COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV"
pattern COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_NV = COPY_ACCELERATION_STRUCTURE_MODE_COMPACT_KHR


-- No documentation found for TopLevel "VK_SHADER_UNUSED_NV"
pattern SHADER_UNUSED_NV = SHADER_UNUSED_KHR


-- No documentation found for TopLevel "vkGetRayTracingShaderGroupHandlesNV"
getRayTracingShaderGroupHandlesNV = getRayTracingShaderGroupHandlesKHR


-- | VkRayTracingShaderGroupCreateInfoNV - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-type-02413# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then @generalShader@
--     /must/ be a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_RAYGEN_BIT_NV', 'SHADER_STAGE_MISS_BIT_NV', or
--     'SHADER_STAGE_CALLABLE_BIT_NV'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-type-02414# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV' then @closestHitShader@,
--     @anyHitShader@, and @intersectionShader@ /must/ be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-type-02415# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV' then
--     @intersectionShader@ /must/ be a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_INTERSECTION_BIT_NV'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-type-02416# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' then
--     @intersectionShader@ /must/ be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-closestHitShader-02417#
--     @closestHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' or a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_CLOSEST_HIT_BIT_NV'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-anyHitShader-02418#
--     @anyHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' or a valid index into
--     'RayTracingPipelineCreateInfoNV'::@pStages@ referring to a shader of
--     'SHADER_STAGE_ANY_HIT_BIT_NV'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoNV-type-parameter# @type@
--     /must/ be a valid
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingShaderGroupTypeKHR'
--     value
--
-- = See Also
--
-- 'RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingShaderGroupTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingShaderGroupCreateInfoNV = RayTracingShaderGroupCreateInfoNV
  { -- | @type@ is the type of hit group specified in this structure.
    type' :: RayTracingShaderGroupTypeKHR
  , -- | @generalShader@ is the index of the ray generation, miss, or callable
    -- shader from 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if
    -- the shader group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    generalShader :: Word32
  , -- | @closestHitShader@ is the optional index of the closest hit shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    closestHitShader :: Word32
  , -- | @anyHitShader@ is the optional index of the any-hit shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_NV' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    anyHitShader :: Word32
  , -- | @intersectionShader@ is the index of the intersection shader from
    -- 'RayTracingPipelineCreateInfoNV'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_NV', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_NV' otherwise.
    intersectionShader :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingShaderGroupCreateInfoNV)
#endif
deriving instance Show RayTracingShaderGroupCreateInfoNV

instance ToCStruct RayTracingShaderGroupCreateInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingShaderGroupCreateInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (type')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (generalShader)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (closestHitShader)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (anyHitShader)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (intersectionShader)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingShaderGroupCreateInfoNV where
  peekCStruct p = do
    type' <- peek @RayTracingShaderGroupTypeKHR ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR))
    generalShader <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    closestHitShader <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    anyHitShader <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    intersectionShader <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pure $ RayTracingShaderGroupCreateInfoNV
             type' generalShader closestHitShader anyHitShader intersectionShader

instance Storable RayTracingShaderGroupCreateInfoNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingShaderGroupCreateInfoNV where
  zero = RayTracingShaderGroupCreateInfoNV
           zero
           zero
           zero
           zero
           zero


-- | VkRayTracingPipelineCreateInfoNV - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- == Valid Usage
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03421# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing 'Vulkan.Core10.Handles.Pipeline'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03422# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03423# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03424# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be @-1@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-pStages-03426# The shader
--     code for the entry points identified by @pStages@, and the rest of
--     the state identified by this structure /must/ adhere to the pipeline
--     linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-layout-03427# @layout@ /must/
--     be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-layout-03428# The number of
--     resources in @layout@ accessible to each shader stage that is used
--     by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-02904# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-pipelineCreationCacheControl-02905#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT_EXT'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-stage-03425# The @stage@
--     member of at least one element of @pStages@ /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03456# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-maxRecursionDepth-03457#
--     @maxRecursionDepth@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxRecursionDepth@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03458# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03459# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03460# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03461# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03462# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03463# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-03588# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-02957# @flags@ /must/
--     not include both
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEFER_COMPILE_BIT_NV'
--     and
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_EXT'
--     at the same time
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-pNext-pNext# @pNext@ /must/
--     be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-pStages-parameter# @pStages@
--     /must/ be a valid pointer to an array of @stageCount@ valid
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-pGroups-parameter# @pGroups@
--     /must/ be a valid pointer to an array of @groupCount@ valid
--     'RayTracingShaderGroupCreateInfoNV' structures
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-stageCount-arraylength#
--     @stageCount@ /must/ be greater than @0@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-groupCount-arraylength#
--     @groupCount@ /must/ be greater than @0@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoNV-commonparent# Both of
--     @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'RayTracingShaderGroupCreateInfoNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createRayTracingPipelinesNV'
data RayTracingPipelineCreateInfoNV (es :: [Type]) = RayTracingPipelineCreateInfoNV
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @pStages@ is an array of size @stageCount@ structures of type
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' describing the
    -- set of the shader stages to be included in the ray tracing pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pGroups@ is an array of size @groupCount@ structures of type
    -- 'RayTracingShaderGroupCreateInfoNV' describing the set of the shader
    -- stages to be included in each shader group in the ray tracing pipeline.
    groups :: Vector RayTracingShaderGroupCreateInfoNV
  , -- | @maxRecursionDepth@ is the
    -- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth maximum recursion depth>
    -- of shaders executed by this pipeline.
    maxRecursionDepth :: Word32
  , -- | @layout@ is the description of binding locations used by both the
    -- pipeline and descriptor sets used with the pipeline.
    layout :: PipelineLayout
  , -- | @basePipelineHandle@ is a pipeline to derive from.
    basePipelineHandle :: Pipeline
  , -- | @basePipelineIndex@ is an index into the @pCreateInfos@ parameter to use
    -- as a pipeline to derive from.
    basePipelineIndex :: Int32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingPipelineCreateInfoNV (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RayTracingPipelineCreateInfoNV es)

instance Extensible RayTracingPipelineCreateInfoNV where
  extensibleTypeName = "RayTracingPipelineCreateInfoNV"
  setNext x next = x{next = next}
  getNext RayTracingPipelineCreateInfoNV{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RayTracingPipelineCreateInfoNV e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfoEXT = Just f
    | otherwise = Nothing

instance (Extendss RayTracingPipelineCreateInfoNV es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoNV es) where
  withCStruct x f = allocaBytesAligned 80 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytesAligned @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48) 8
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytesAligned @RayTracingShaderGroupCreateInfoNV ((Data.Vector.length (groups)) * 40) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGroups' `plusPtr` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV) (e)) (groups)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxRecursionDepth)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 64 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 72 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 80
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 56 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 72 :: Ptr Int32)) (zero)
    lift $ f

instance (Extendss RayTracingPipelineCreateInfoNV es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoNV es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    groupCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pGroups <- peek @(Ptr RayTracingShaderGroupCreateInfoNV) ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoNV)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @RayTracingShaderGroupCreateInfoNV ((pGroups `advancePtrBytes` (40 * (i)) :: Ptr RayTracingShaderGroupCreateInfoNV)))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    layout <- peek @PipelineLayout ((p `plusPtr` 56 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 64 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 72 :: Ptr Int32))
    pure $ RayTracingPipelineCreateInfoNV
             next flags pStages' pGroups' maxRecursionDepth layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (RayTracingPipelineCreateInfoNV es) where
  zero = RayTracingPipelineCreateInfoNV
           ()
           zero
           mempty
           mempty
           zero
           zero
           zero
           zero


-- | VkGeometryTrianglesNV - Structure specifying a triangle geometry in a
-- bottom-level acceleration structure
--
-- = Description
--
-- If @indexType@ is 'INDEX_TYPE_NONE_NV', then this structure describes a
-- set of triangles determined by @vertexCount@. Otherwise, this structure
-- describes a set of indexed triangles determined by @indexCount@.
--
-- == Valid Usage
--
-- -   #VUID-VkGeometryTrianglesNV-vertexOffset-02428# @vertexOffset@
--     /must/ be less than the size of @vertexData@
--
-- -   #VUID-VkGeometryTrianglesNV-vertexOffset-02429# @vertexOffset@
--     /must/ be a multiple of the component size of @vertexFormat@
--
-- -   #VUID-VkGeometryTrianglesNV-vertexFormat-02430# @vertexFormat@
--     /must/ be one of
--     'Vulkan.Core10.Enums.Format.FORMAT_R32G32B32_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R32G32_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SFLOAT',
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16_SNORM', or
--     'Vulkan.Core10.Enums.Format.FORMAT_R16G16B16_SNORM'
--
-- -   #VUID-VkGeometryTrianglesNV-vertexStride-03818# @vertexStride@
--     /must/ be less than or equal to 232-1
--
-- -   #VUID-VkGeometryTrianglesNV-indexOffset-02431# @indexOffset@ /must/
--     be less than the size of @indexData@
--
-- -   #VUID-VkGeometryTrianglesNV-indexOffset-02432# @indexOffset@ /must/
--     be a multiple of the element size of @indexType@
--
-- -   #VUID-VkGeometryTrianglesNV-indexType-02433# @indexType@ /must/ be
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT16',
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT32', or
--     'INDEX_TYPE_NONE_NV'
--
-- -   #VUID-VkGeometryTrianglesNV-indexData-02434# @indexData@ /must/ be
--     'Vulkan.Core10.APIConstants.NULL_HANDLE' if @indexType@ is
--     'INDEX_TYPE_NONE_NV'
--
-- -   #VUID-VkGeometryTrianglesNV-indexData-02435# @indexData@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle if @indexType@ is not
--     'INDEX_TYPE_NONE_NV'
--
-- -   #VUID-VkGeometryTrianglesNV-indexCount-02436# @indexCount@ /must/ be
--     @0@ if @indexType@ is 'INDEX_TYPE_NONE_NV'
--
-- -   #VUID-VkGeometryTrianglesNV-transformOffset-02437# @transformOffset@
--     /must/ be less than the size of @transformData@
--
-- -   #VUID-VkGeometryTrianglesNV-transformOffset-02438# @transformOffset@
--     /must/ be a multiple of @16@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGeometryTrianglesNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV'
--
-- -   #VUID-VkGeometryTrianglesNV-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkGeometryTrianglesNV-vertexData-parameter# If @vertexData@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @vertexData@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkGeometryTrianglesNV-vertexFormat-parameter# @vertexFormat@
--     /must/ be a valid 'Vulkan.Core10.Enums.Format.Format' value
--
-- -   #VUID-VkGeometryTrianglesNV-indexData-parameter# If @indexData@ is
--     not 'Vulkan.Core10.APIConstants.NULL_HANDLE', @indexData@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-VkGeometryTrianglesNV-indexType-parameter# @indexType@ /must/
--     be a valid 'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-VkGeometryTrianglesNV-transformData-parameter# If
--     @transformData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @transformData@ /must/ be a valid 'Vulkan.Core10.Handles.Buffer'
--     handle
--
-- -   #VUID-VkGeometryTrianglesNV-commonparent# Each of @indexData@,
--     @transformData@, and @vertexData@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.Format.Format', 'GeometryDataNV',
-- 'Vulkan.Core10.Enums.IndexType.IndexType',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryTrianglesNV = GeometryTrianglesNV
  { -- | @vertexData@ is the buffer containing vertex data for this geometry.
    vertexData :: Buffer
  , -- | @vertexOffset@ is the offset in bytes within @vertexData@ containing
    -- vertex data for this geometry.
    vertexOffset :: DeviceSize
  , -- | @vertexCount@ is the number of valid vertices.
    vertexCount :: Word32
  , -- | @vertexStride@ is the stride in bytes between each vertex.
    vertexStride :: DeviceSize
  , -- | @vertexFormat@ is a 'Vulkan.Core10.Enums.Format.Format' describing the
    -- format of each vertex element.
    vertexFormat :: Format
  , -- | @indexData@ is the buffer containing index data for this geometry.
    indexData :: Buffer
  , -- | @indexOffset@ is the offset in bytes within @indexData@ containing index
    -- data for this geometry.
    indexOffset :: DeviceSize
  , -- | @indexCount@ is the number of indices to include in this geometry.
    indexCount :: Word32
  , -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' describing
    -- the format of each index.
    indexType :: IndexType
  , -- | @transformData@ is an optional buffer containing an 'TransformMatrixNV'
    -- structure defining a transformation to be applied to this geometry.
    transformData :: Buffer
  , -- | @transformOffset@ is the offset in bytes in @transformData@ of the
    -- transform information described above.
    transformOffset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryTrianglesNV)
#endif
deriving instance Show GeometryTrianglesNV

instance ToCStruct GeometryTrianglesNV where
  withCStruct x f = allocaBytesAligned 96 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryTrianglesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (vertexData)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (vertexOffset)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (vertexCount)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (vertexStride)
    poke ((p `plusPtr` 48 :: Ptr Format)) (vertexFormat)
    poke ((p `plusPtr` 56 :: Ptr Buffer)) (indexData)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (indexOffset)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (indexCount)
    poke ((p `plusPtr` 76 :: Ptr IndexType)) (indexType)
    poke ((p `plusPtr` 80 :: Ptr Buffer)) (transformData)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (transformOffset)
    f
  cStructSize = 96
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 64 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 72 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 76 :: Ptr IndexType)) (zero)
    poke ((p `plusPtr` 88 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeometryTrianglesNV where
  peekCStruct p = do
    vertexData <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    vertexOffset <- peek @DeviceSize ((p `plusPtr` 24 :: Ptr DeviceSize))
    vertexCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    vertexStride <- peek @DeviceSize ((p `plusPtr` 40 :: Ptr DeviceSize))
    vertexFormat <- peek @Format ((p `plusPtr` 48 :: Ptr Format))
    indexData <- peek @Buffer ((p `plusPtr` 56 :: Ptr Buffer))
    indexOffset <- peek @DeviceSize ((p `plusPtr` 64 :: Ptr DeviceSize))
    indexCount <- peek @Word32 ((p `plusPtr` 72 :: Ptr Word32))
    indexType <- peek @IndexType ((p `plusPtr` 76 :: Ptr IndexType))
    transformData <- peek @Buffer ((p `plusPtr` 80 :: Ptr Buffer))
    transformOffset <- peek @DeviceSize ((p `plusPtr` 88 :: Ptr DeviceSize))
    pure $ GeometryTrianglesNV
             vertexData vertexOffset vertexCount vertexStride vertexFormat indexData indexOffset indexCount indexType transformData transformOffset

instance Storable GeometryTrianglesNV where
  sizeOf ~_ = 96
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryTrianglesNV where
  zero = GeometryTrianglesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkGeometryAABBNV - Structure specifying axis-aligned bounding box
-- geometry in a bottom-level acceleration structure
--
-- = Description
--
-- The AABB data in memory is six 32-bit floats consisting of the minimum
-- x, y, and z values followed by the maximum x, y, and z values.
--
-- == Valid Usage
--
-- -   #VUID-VkGeometryAABBNV-offset-02439# @offset@ /must/ be less than
--     the size of @aabbData@
--
-- -   #VUID-VkGeometryAABBNV-offset-02440# @offset@ /must/ be a multiple
--     of @8@
--
-- -   #VUID-VkGeometryAABBNV-stride-02441# @stride@ /must/ be a multiple
--     of @8@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkGeometryAABBNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_GEOMETRY_AABB_NV'
--
-- -   #VUID-VkGeometryAABBNV-pNext-pNext# @pNext@ /must/ be @NULL@
--
-- -   #VUID-VkGeometryAABBNV-aabbData-parameter# If @aabbData@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @aabbData@ /must/ be a
--     valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- = See Also
--
-- 'Vulkan.Core10.Handles.Buffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'GeometryDataNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryAABBNV = GeometryAABBNV
  { -- | @aabbData@ is the buffer containing axis-aligned bounding box data.
    aabbData :: Buffer
  , -- | @numAABBs@ is the number of AABBs in this geometry.
    numAABBs :: Word32
  , -- | @stride@ is the stride in bytes between AABBs in @aabbData@.
    stride :: Word32
  , -- | @offset@ is the offset in bytes of the first AABB in @aabbData@.
    offset :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryAABBNV)
#endif
deriving instance Show GeometryAABBNV

instance ToCStruct GeometryAABBNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryAABBNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_AABB_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Buffer)) (aabbData)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (numAABBs)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (stride)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (offset)
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_AABB_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct GeometryAABBNV where
  peekCStruct p = do
    aabbData <- peek @Buffer ((p `plusPtr` 16 :: Ptr Buffer))
    numAABBs <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    stride <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    offset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    pure $ GeometryAABBNV
             aabbData numAABBs stride offset

instance Storable GeometryAABBNV where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryAABBNV where
  zero = GeometryAABBNV
           zero
           zero
           zero
           zero


-- | VkGeometryDataNV - Structure specifying geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'GeometryAABBNV', 'GeometryNV', 'GeometryTrianglesNV'
data GeometryDataNV = GeometryDataNV
  { -- | @triangles@ contains triangle data if 'GeometryNV'::@geometryType@ is
    -- 'GEOMETRY_TYPE_TRIANGLES_NV'.
    --
    -- #VUID-VkGeometryDataNV-triangles-parameter# @triangles@ /must/ be a
    -- valid 'GeometryTrianglesNV' structure
    triangles :: GeometryTrianglesNV
  , -- | @aabbs@ contains axis-aligned bounding box data if
    -- 'GeometryNV'::@geometryType@ is 'GEOMETRY_TYPE_AABBS_NV'.
    --
    -- #VUID-VkGeometryDataNV-aabbs-parameter# @aabbs@ /must/ be a valid
    -- 'GeometryAABBNV' structure
    aabbs :: GeometryAABBNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryDataNV)
#endif
deriving instance Show GeometryDataNV

instance ToCStruct GeometryDataNV where
  withCStruct x f = allocaBytesAligned 136 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryDataNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (triangles)
    poke ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (aabbs)
    f
  cStructSize = 136
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV)) (zero)
    poke ((p `plusPtr` 96 :: Ptr GeometryAABBNV)) (zero)
    f

instance FromCStruct GeometryDataNV where
  peekCStruct p = do
    triangles <- peekCStruct @GeometryTrianglesNV ((p `plusPtr` 0 :: Ptr GeometryTrianglesNV))
    aabbs <- peekCStruct @GeometryAABBNV ((p `plusPtr` 96 :: Ptr GeometryAABBNV))
    pure $ GeometryDataNV
             triangles aabbs

instance Storable GeometryDataNV where
  sizeOf ~_ = 136
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryDataNV where
  zero = GeometryDataNV
           zero
           zero


-- | VkGeometryNV - Structure specifying a geometry in a bottom-level
-- acceleration structure
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureInfoNV', 'GeometryDataNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryFlagsKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data GeometryNV = GeometryNV
  { -- | @geometryType@ specifies the
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR' which
    -- this geometry refers to.
    --
    -- #VUID-VkGeometryNV-geometryType-03503# @geometryType@ /must/ be
    -- 'GEOMETRY_TYPE_TRIANGLES_NV' or 'GEOMETRY_TYPE_AABBS_NV'
    --
    -- #VUID-VkGeometryNV-geometryType-parameter# @geometryType@ /must/ be a
    -- valid 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryTypeKHR'
    -- value
    geometryType :: GeometryTypeKHR
  , -- | @geometry@ contains the geometry data as described in 'GeometryDataNV'.
    --
    -- #VUID-VkGeometryNV-geometry-parameter# @geometry@ /must/ be a valid
    -- 'GeometryDataNV' structure
    geometry :: GeometryDataNV
  , -- | @flags@ has
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryFlagBitsKHR'
    -- describing options for this geometry.
    --
    -- #VUID-VkGeometryNV-flags-parameter# @flags@ /must/ be a valid
    -- combination of
    -- 'Vulkan.Extensions.VK_KHR_acceleration_structure.GeometryFlagBitsKHR'
    -- values
    flags :: GeometryFlagsKHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (GeometryNV)
#endif
deriving instance Show GeometryNV

instance ToCStruct GeometryNV where
  withCStruct x f = allocaBytesAligned 168 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p GeometryNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (geometryType)
    poke ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (geometry)
    poke ((p `plusPtr` 160 :: Ptr GeometryFlagsKHR)) (flags)
    f
  cStructSize = 168
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_GEOMETRY_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr GeometryTypeKHR)) (zero)
    poke ((p `plusPtr` 24 :: Ptr GeometryDataNV)) (zero)
    f

instance FromCStruct GeometryNV where
  peekCStruct p = do
    geometryType <- peek @GeometryTypeKHR ((p `plusPtr` 16 :: Ptr GeometryTypeKHR))
    geometry <- peekCStruct @GeometryDataNV ((p `plusPtr` 24 :: Ptr GeometryDataNV))
    flags <- peek @GeometryFlagsKHR ((p `plusPtr` 160 :: Ptr GeometryFlagsKHR))
    pure $ GeometryNV
             geometryType geometry flags

instance Storable GeometryNV where
  sizeOf ~_ = 168
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero GeometryNV where
  zero = GeometryNV
           zero
           zero
           zero


-- | VkAccelerationStructureInfoNV - Structure specifying the parameters of
-- acceleration structure object
--
-- = Description
--
-- 'AccelerationStructureInfoNV' contains information that is used both for
-- acceleration structure creation with 'createAccelerationStructureNV' and
-- in combination with the actual geometric data to build the acceleration
-- structure with 'cmdBuildAccelerationStructureNV'.
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureInfoNV-geometryCount-02422#
--     @geometryCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxGeometryCount@
--
-- -   #VUID-VkAccelerationStructureInfoNV-instanceCount-02423#
--     @instanceCount@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPropertiesNV'::@maxInstanceCount@
--
-- -   #VUID-VkAccelerationStructureInfoNV-maxTriangleCount-02424# The
--     total number of triangles in all geometries /must/ be less than or
--     equal to 'PhysicalDeviceRayTracingPropertiesNV'::@maxTriangleCount@
--
-- -   #VUID-VkAccelerationStructureInfoNV-type-02425# If @type@ is
--     'ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_NV' then @geometryCount@
--     /must/ be @0@
--
-- -   #VUID-VkAccelerationStructureInfoNV-type-02426# If @type@ is
--     'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then @instanceCount@
--     /must/ be @0@
--
-- -   #VUID-VkAccelerationStructureInfoNV-type-02786# If @type@ is
--     'ACCELERATION_STRUCTURE_TYPE_BOTTOM_LEVEL_NV' then the
--     @geometryType@ member of each geometry in @pGeometries@ /must/ be
--     the same
--
-- -   #VUID-VkAccelerationStructureInfoNV-type-04623# @type@ /must/ not be
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_TYPE_GENERIC_KHR'
--
-- -   #VUID-VkAccelerationStructureInfoNV-flags-02592# If @flags@ has the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_TRACE_BIT_NV' bit set,
--     then it /must/ not have the
--     'BUILD_ACCELERATION_STRUCTURE_PREFER_FAST_BUILD_BIT_NV' bit set
--
-- -   #VUID-VkAccelerationStructureInfoNV-scratch-02781# @scratch@ /must/
--     have been created with 'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- -   #VUID-VkAccelerationStructureInfoNV-instanceData-02782# If
--     @instanceData@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @instanceData@ /must/ have been created with
--     'BUFFER_USAGE_RAY_TRACING_BIT_NV' usage flag
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureInfoNV-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV'
--
-- -   #VUID-VkAccelerationStructureInfoNV-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkAccelerationStructureInfoNV-type-parameter# @type@ /must/ be
--     a valid 'AccelerationStructureTypeNV' value
--
-- -   #VUID-VkAccelerationStructureInfoNV-flags-parameter# @flags@ /must/
--     be a valid combination of 'BuildAccelerationStructureFlagBitsNV'
--     values
--
-- -   #VUID-VkAccelerationStructureInfoNV-pGeometries-parameter# If
--     @geometryCount@ is not @0@, @pGeometries@ /must/ be a valid pointer
--     to an array of @geometryCount@ valid 'GeometryNV' structures
--
-- = See Also
--
-- 'AccelerationStructureCreateInfoNV', 'AccelerationStructureTypeNV',
-- 'BuildAccelerationStructureFlagsNV', 'GeometryNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'cmdBuildAccelerationStructureNV'
data AccelerationStructureInfoNV = AccelerationStructureInfoNV
  { -- | @type@ is a 'AccelerationStructureTypeNV' value specifying the type of
    -- acceleration structure that will be created.
    type' :: AccelerationStructureTypeNV
  , -- | @flags@ is a bitmask of 'BuildAccelerationStructureFlagBitsNV'
    -- specifying additional parameters of the acceleration structure.
    flags :: BuildAccelerationStructureFlagsNV
  , -- | @instanceCount@ specifies the number of instances that will be in the
    -- new acceleration structure.
    instanceCount :: Word32
  , -- | @pGeometries@ is a pointer to an array of @geometryCount@ 'GeometryNV'
    -- structures containing the scene data being passed into the acceleration
    -- structure.
    geometries :: Vector GeometryNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureInfoNV)
#endif
deriving instance Show AccelerationStructureInfoNV

instance ToCStruct AccelerationStructureInfoNV where
  withCStruct x f = allocaBytesAligned 40 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (type')
    lift $ poke ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsNV)) (flags)
    lift $ poke ((p `plusPtr` 24 :: Ptr Word32)) (instanceCount)
    lift $ poke ((p `plusPtr` 28 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (geometries)) :: Word32))
    pPGeometries' <- ContT $ allocaBytesAligned @GeometryNV ((Data.Vector.length (geometries)) * 168) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGeometries' `plusPtr` (168 * (i)) :: Ptr GeometryNV) (e)) (geometries)
    lift $ poke ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV))) (pPGeometries')
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV)) (zero)
    f

instance FromCStruct AccelerationStructureInfoNV where
  peekCStruct p = do
    type' <- peek @AccelerationStructureTypeNV ((p `plusPtr` 16 :: Ptr AccelerationStructureTypeNV))
    flags <- peek @BuildAccelerationStructureFlagsNV ((p `plusPtr` 20 :: Ptr BuildAccelerationStructureFlagsNV))
    instanceCount <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    geometryCount <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    pGeometries <- peek @(Ptr GeometryNV) ((p `plusPtr` 32 :: Ptr (Ptr GeometryNV)))
    pGeometries' <- generateM (fromIntegral geometryCount) (\i -> peekCStruct @GeometryNV ((pGeometries `advancePtrBytes` (168 * (i)) :: Ptr GeometryNV)))
    pure $ AccelerationStructureInfoNV
             type' flags instanceCount pGeometries'

instance Zero AccelerationStructureInfoNV where
  zero = AccelerationStructureInfoNV
           zero
           zero
           zero
           mempty


-- | VkAccelerationStructureCreateInfoNV - Structure specifying the
-- parameters of a newly created acceleration structure object
--
-- == Valid Usage
--
-- -   #VUID-VkAccelerationStructureCreateInfoNV-compactedSize-02421# If
--     @compactedSize@ is not @0@ then both @info.geometryCount@ and
--     @info.instanceCount@ /must/ be @0@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkAccelerationStructureCreateInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV'
--
-- -   #VUID-VkAccelerationStructureCreateInfoNV-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkAccelerationStructureCreateInfoNV-info-parameter# @info@
--     /must/ be a valid 'AccelerationStructureInfoNV' structure
--
-- = See Also
--
-- 'AccelerationStructureInfoNV',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createAccelerationStructureNV'
data AccelerationStructureCreateInfoNV = AccelerationStructureCreateInfoNV
  { -- | @compactedSize@ is the size from the result of
    -- 'cmdWriteAccelerationStructuresPropertiesNV' if this acceleration
    -- structure is going to be the target of a compacting copy.
    compactedSize :: DeviceSize
  , -- | @info@ is the 'AccelerationStructureInfoNV' structure specifying further
    -- parameters of the created acceleration structure.
    info :: AccelerationStructureInfoNV
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureCreateInfoNV)
#endif
deriving instance Show AccelerationStructureCreateInfoNV

instance ToCStruct AccelerationStructureCreateInfoNV where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureCreateInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (compactedSize)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV)) (info) . ($ ())
    lift $ f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    ContT $ pokeCStruct ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV)) (zero) . ($ ())
    lift $ f

instance FromCStruct AccelerationStructureCreateInfoNV where
  peekCStruct p = do
    compactedSize <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    info <- peekCStruct @AccelerationStructureInfoNV ((p `plusPtr` 24 :: Ptr AccelerationStructureInfoNV))
    pure $ AccelerationStructureCreateInfoNV
             compactedSize info

instance Zero AccelerationStructureCreateInfoNV where
  zero = AccelerationStructureCreateInfoNV
           zero
           zero


-- | VkBindAccelerationStructureMemoryInfoNV - Structure specifying
-- acceleration structure memory binding
--
-- == Valid Usage
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-accelerationStructure-03620#
--     @accelerationStructure@ /must/ not already be backed by a memory
--     object
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-memoryOffset-03621#
--     @memoryOffset@ /must/ be less than the size of @memory@
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-memory-03622# @memory@
--     /must/ have been allocated using one of the memory types allowed in
--     the @memoryTypeBits@ member of the
--     'Vulkan.Core10.MemoryManagement.MemoryRequirements' structure
--     returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-memoryOffset-03623#
--     @memoryOffset@ /must/ be an integer multiple of the @alignment@
--     member of the 'Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV'
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-size-03624# The @size@
--     member of the 'Vulkan.Core10.MemoryManagement.MemoryRequirements'
--     structure returned from a call to
--     'getAccelerationStructureMemoryRequirementsNV' with
--     @accelerationStructure@ and @type@ of
--     'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' /must/
--     be less than or equal to the size of @memory@ minus @memoryOffset@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV'
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-accelerationStructure-parameter#
--     @accelerationStructure@ /must/ be a valid
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-memory-parameter#
--     @memory@ /must/ be a valid 'Vulkan.Core10.Handles.DeviceMemory'
--     handle
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-pDeviceIndices-parameter#
--     If @deviceIndexCount@ is not @0@, @pDeviceIndices@ /must/ be a valid
--     pointer to an array of @deviceIndexCount@ @uint32_t@ values
--
-- -   #VUID-VkBindAccelerationStructureMemoryInfoNV-commonparent# Both of
--     @accelerationStructure@, and @memory@ /must/ have been created,
--     allocated, or retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Handles.DeviceMemory',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'bindAccelerationStructureMemoryNV'
data BindAccelerationStructureMemoryInfoNV = BindAccelerationStructureMemoryInfoNV
  { -- | @accelerationStructure@ is the acceleration structure to be attached to
    -- memory.
    accelerationStructure :: AccelerationStructureNV
  , -- | @memory@ is a 'Vulkan.Core10.Handles.DeviceMemory' object describing the
    -- device memory to attach.
    memory :: DeviceMemory
  , -- | @memoryOffset@ is the start offset of the region of memory that is to be
    -- bound to the acceleration structure. The number of bytes returned in the
    -- 'Vulkan.Core10.MemoryManagement.MemoryRequirements'::@size@ member in
    -- @memory@, starting from @memoryOffset@ bytes, will be bound to the
    -- specified acceleration structure.
    memoryOffset :: DeviceSize
  , -- | @pDeviceIndices@ is a pointer to an array of device indices.
    deviceIndices :: Vector Word32
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BindAccelerationStructureMemoryInfoNV)
#endif
deriving instance Show BindAccelerationStructureMemoryInfoNV

instance ToCStruct BindAccelerationStructureMemoryInfoNV where
  withCStruct x f = allocaBytesAligned 56 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BindAccelerationStructureMemoryInfoNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr AccelerationStructureNV)) (accelerationStructure)
    lift $ poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (memory)
    lift $ poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (memoryOffset)
    lift $ poke ((p `plusPtr` 40 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (deviceIndices)) :: Word32))
    pPDeviceIndices' <- ContT $ allocaBytesAligned @Word32 ((Data.Vector.length (deviceIndices)) * 4) 4
    lift $ Data.Vector.imapM_ (\i e -> poke (pPDeviceIndices' `plusPtr` (4 * (i)) :: Ptr Word32) (e)) (deviceIndices)
    lift $ poke ((p `plusPtr` 48 :: Ptr (Ptr Word32))) (pPDeviceIndices')
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr DeviceMemory)) (zero)
    poke ((p `plusPtr` 32 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct BindAccelerationStructureMemoryInfoNV where
  peekCStruct p = do
    accelerationStructure <- peek @AccelerationStructureNV ((p `plusPtr` 16 :: Ptr AccelerationStructureNV))
    memory <- peek @DeviceMemory ((p `plusPtr` 24 :: Ptr DeviceMemory))
    memoryOffset <- peek @DeviceSize ((p `plusPtr` 32 :: Ptr DeviceSize))
    deviceIndexCount <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    pDeviceIndices <- peek @(Ptr Word32) ((p `plusPtr` 48 :: Ptr (Ptr Word32)))
    pDeviceIndices' <- generateM (fromIntegral deviceIndexCount) (\i -> peek @Word32 ((pDeviceIndices `advancePtrBytes` (4 * (i)) :: Ptr Word32)))
    pure $ BindAccelerationStructureMemoryInfoNV
             accelerationStructure memory memoryOffset pDeviceIndices'

instance Zero BindAccelerationStructureMemoryInfoNV where
  zero = BindAccelerationStructureMemoryInfoNV
           zero
           zero
           zero
           mempty


-- | VkWriteDescriptorSetAccelerationStructureNV - Structure specifying
-- acceleration structure descriptor info
--
-- == Valid Usage
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureNV-accelerationStructureCount-03747#
--     @accelerationStructureCount@ /must/ be equal to @descriptorCount@ in
--     the extended structure
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureNV-pAccelerationStructures-03748#
--     Each acceleration structure in @pAccelerationStructures@ /must/ have
--     been created with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.ACCELERATION_STRUCTURE_TYPE_TOP_LEVEL_KHR'
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureNV-pAccelerationStructures-03749#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-nullDescriptor nullDescriptor>
--     feature is not enabled, each member of @pAccelerationStructures@
--     /must/ not be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureNV-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV'
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureNV-pAccelerationStructures-parameter#
--     @pAccelerationStructures@ /must/ be a valid pointer to an array of
--     @accelerationStructureCount@ valid or
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'
--     'Vulkan.Extensions.Handles.AccelerationStructureNV' handles
--
-- -   #VUID-VkWriteDescriptorSetAccelerationStructureNV-accelerationStructureCount-arraylength#
--     @accelerationStructureCount@ /must/ be greater than @0@
--
-- = See Also
--
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data WriteDescriptorSetAccelerationStructureNV = WriteDescriptorSetAccelerationStructureNV
  { -- | @pAccelerationStructures@ are the acceleration structures to update.
    accelerationStructures :: Vector AccelerationStructureNV }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (WriteDescriptorSetAccelerationStructureNV)
#endif
deriving instance Show WriteDescriptorSetAccelerationStructureNV

instance ToCStruct WriteDescriptorSetAccelerationStructureNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p WriteDescriptorSetAccelerationStructureNV{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (accelerationStructures)) :: Word32))
    pPAccelerationStructures' <- ContT $ allocaBytesAligned @AccelerationStructureNV ((Data.Vector.length (accelerationStructures)) * 8) 8
    lift $ Data.Vector.imapM_ (\i e -> poke (pPAccelerationStructures' `plusPtr` (8 * (i)) :: Ptr AccelerationStructureNV) (e)) (accelerationStructures)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV))) (pPAccelerationStructures')
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    f

instance FromCStruct WriteDescriptorSetAccelerationStructureNV where
  peekCStruct p = do
    accelerationStructureCount <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    pAccelerationStructures <- peek @(Ptr AccelerationStructureNV) ((p `plusPtr` 24 :: Ptr (Ptr AccelerationStructureNV)))
    pAccelerationStructures' <- generateM (fromIntegral accelerationStructureCount) (\i -> peek @AccelerationStructureNV ((pAccelerationStructures `advancePtrBytes` (8 * (i)) :: Ptr AccelerationStructureNV)))
    pure $ WriteDescriptorSetAccelerationStructureNV
             pAccelerationStructures'

instance Zero WriteDescriptorSetAccelerationStructureNV where
  zero = WriteDescriptorSetAccelerationStructureNV
           mempty


-- | VkAccelerationStructureMemoryRequirementsInfoNV - Structure specifying
-- acceleration to query for memory requirements
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsTypeNV',
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getAccelerationStructureMemoryRequirementsNV'
data AccelerationStructureMemoryRequirementsInfoNV = AccelerationStructureMemoryRequirementsInfoNV
  { -- | @type@ selects the type of memory requirement being queried.
    -- 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' returns the
    -- memory requirements for the object itself.
    -- 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
    -- returns the memory requirements for the scratch memory when doing a
    -- build.
    -- 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
    -- returns the memory requirements for the scratch memory when doing an
    -- update.
    --
    -- #VUID-VkAccelerationStructureMemoryRequirementsInfoNV-type-parameter#
    -- @type@ /must/ be a valid 'AccelerationStructureMemoryRequirementsTypeNV'
    -- value
    type' :: AccelerationStructureMemoryRequirementsTypeNV
  , -- | @accelerationStructure@ is the acceleration structure to be queried for
    -- memory requirements.
    --
    -- #VUID-VkAccelerationStructureMemoryRequirementsInfoNV-accelerationStructure-parameter#
    -- @accelerationStructure@ /must/ be a valid
    -- 'Vulkan.Extensions.Handles.AccelerationStructureNV' handle
    accelerationStructure :: AccelerationStructureNV
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (AccelerationStructureMemoryRequirementsInfoNV)
#endif
deriving instance Show AccelerationStructureMemoryRequirementsInfoNV

instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV where
  withCStruct x f = allocaBytesAligned 32 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p AccelerationStructureMemoryRequirementsInfoNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV)) (type')
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (accelerationStructure)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV)) (zero)
    poke ((p `plusPtr` 24 :: Ptr AccelerationStructureNV)) (zero)
    f

instance FromCStruct AccelerationStructureMemoryRequirementsInfoNV where
  peekCStruct p = do
    type' <- peek @AccelerationStructureMemoryRequirementsTypeNV ((p `plusPtr` 16 :: Ptr AccelerationStructureMemoryRequirementsTypeNV))
    accelerationStructure <- peek @AccelerationStructureNV ((p `plusPtr` 24 :: Ptr AccelerationStructureNV))
    pure $ AccelerationStructureMemoryRequirementsInfoNV
             type' accelerationStructure

instance Storable AccelerationStructureMemoryRequirementsInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero AccelerationStructureMemoryRequirementsInfoNV where
  zero = AccelerationStructureMemoryRequirementsInfoNV
           zero
           zero


-- | VkPhysicalDeviceRayTracingPropertiesNV - Properties of the physical
-- device for ray tracing
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingPropertiesNV' structure is included in
-- the @pNext@ chain of
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- it is filled with the implementation-dependent limits.
--
-- Limits specified by this structure /must/ match those specified with the
-- same name in
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.PhysicalDeviceAccelerationStructurePropertiesKHR'
-- and
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.PhysicalDeviceRayTracingPipelinePropertiesKHR'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPropertiesNV = PhysicalDeviceRayTracingPropertiesNV
  { -- | @shaderGroupHandleSize@ size in bytes of the shader header.
    shaderGroupHandleSize :: Word32
  , -- | #limits-maxRecursionDepth# @maxRecursionDepth@ is the maximum number of
    -- levels of recursion allowed in a trace command.
    maxRecursionDepth :: Word32
  , -- | @maxShaderGroupStride@ is the maximum stride in bytes allowed between
    -- shader groups in the shader binding table.
    maxShaderGroupStride :: Word32
  , -- | @shaderGroupBaseAlignment@ is the /required/ alignment in bytes for the
    -- base of the shader binding table.
    shaderGroupBaseAlignment :: Word32
  , -- | @maxGeometryCount@ is the maximum number of geometries in the bottom
    -- level acceleration structure.
    maxGeometryCount :: Word64
  , -- | @maxInstanceCount@ is the maximum number of instances in the top level
    -- acceleration structure.
    maxInstanceCount :: Word64
  , -- | @maxTriangleCount@ is the maximum number of triangles in all geometries
    -- in the bottom level acceleration structure.
    maxTriangleCount :: Word64
  , -- | @maxDescriptorSetAccelerationStructures@ is the maximum number of
    -- acceleration structure descriptors that are allowed in a descriptor set.
    maxDescriptorSetAccelerationStructures :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPropertiesNV)
#endif
deriving instance Show PhysicalDeviceRayTracingPropertiesNV

instance ToCStruct PhysicalDeviceRayTracingPropertiesNV where
  withCStruct x f = allocaBytesAligned 64 8 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPropertiesNV{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderGroupHandleSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxRecursionDepth)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxShaderGroupStride)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderGroupBaseAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (maxGeometryCount)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (maxInstanceCount)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (maxTriangleCount)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (maxDescriptorSetAccelerationStructures)
    f
  cStructSize = 64
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 48 :: Ptr Word64)) (zero)
    poke ((p `plusPtr` 56 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingPropertiesNV where
  peekCStruct p = do
    shaderGroupHandleSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxRecursionDepth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxShaderGroupStride <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderGroupBaseAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    maxGeometryCount <- peek @Word64 ((p `plusPtr` 32 :: Ptr Word64))
    maxInstanceCount <- peek @Word64 ((p `plusPtr` 40 :: Ptr Word64))
    maxTriangleCount <- peek @Word64 ((p `plusPtr` 48 :: Ptr Word64))
    maxDescriptorSetAccelerationStructures <- peek @Word32 ((p `plusPtr` 56 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingPropertiesNV
             shaderGroupHandleSize maxRecursionDepth maxShaderGroupStride shaderGroupBaseAlignment maxGeometryCount maxInstanceCount maxTriangleCount maxDescriptorSetAccelerationStructures

instance Storable PhysicalDeviceRayTracingPropertiesNV where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPropertiesNV where
  zero = PhysicalDeviceRayTracingPropertiesNV
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkAccelerationStructureMemoryRequirementsTypeNV - Acceleration structure
-- memory requirement type
--
-- = See Also
--
-- 'AccelerationStructureMemoryRequirementsInfoNV'
newtype AccelerationStructureMemoryRequirementsTypeNV = AccelerationStructureMemoryRequirementsTypeNV Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV' requests the
-- memory requirement for the
-- 'Vulkan.Extensions.Handles.AccelerationStructureNV' backing store.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV = AccelerationStructureMemoryRequirementsTypeNV 0
-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV'
-- requests the memory requirement for scratch space during the initial
-- build.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV =
  AccelerationStructureMemoryRequirementsTypeNV 1
-- | 'ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV'
-- requests the memory requirement for scratch space during an update.
pattern ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV =
  AccelerationStructureMemoryRequirementsTypeNV 2
{-# complete ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV,
             ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV :: AccelerationStructureMemoryRequirementsTypeNV #-}

conNameAccelerationStructureMemoryRequirementsTypeNV :: String
conNameAccelerationStructureMemoryRequirementsTypeNV = "AccelerationStructureMemoryRequirementsTypeNV"

enumPrefixAccelerationStructureMemoryRequirementsTypeNV :: String
enumPrefixAccelerationStructureMemoryRequirementsTypeNV = "ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_"

showTableAccelerationStructureMemoryRequirementsTypeNV :: [(AccelerationStructureMemoryRequirementsTypeNV, String)]
showTableAccelerationStructureMemoryRequirementsTypeNV =
  [ (ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_OBJECT_NV        , "OBJECT_NV")
  , (ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_BUILD_SCRATCH_NV , "BUILD_SCRATCH_NV")
  , (ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_TYPE_UPDATE_SCRATCH_NV, "UPDATE_SCRATCH_NV")
  ]

instance Show AccelerationStructureMemoryRequirementsTypeNV where
  showsPrec = enumShowsPrec enumPrefixAccelerationStructureMemoryRequirementsTypeNV
                            showTableAccelerationStructureMemoryRequirementsTypeNV
                            conNameAccelerationStructureMemoryRequirementsTypeNV
                            (\(AccelerationStructureMemoryRequirementsTypeNV x) -> x)
                            (showsPrec 11)

instance Read AccelerationStructureMemoryRequirementsTypeNV where
  readPrec = enumReadPrec enumPrefixAccelerationStructureMemoryRequirementsTypeNV
                          showTableAccelerationStructureMemoryRequirementsTypeNV
                          conNameAccelerationStructureMemoryRequirementsTypeNV
                          AccelerationStructureMemoryRequirementsTypeNV


-- No documentation found for TopLevel "VkGeometryFlagsNV"
type GeometryFlagsNV = GeometryFlagsKHR


-- No documentation found for TopLevel "VkGeometryInstanceFlagsNV"
type GeometryInstanceFlagsNV = GeometryInstanceFlagsKHR


-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagsNV"
type BuildAccelerationStructureFlagsNV = BuildAccelerationStructureFlagsKHR


-- No documentation found for TopLevel "VkGeometryFlagBitsNV"
type GeometryFlagBitsNV = GeometryFlagBitsKHR


-- No documentation found for TopLevel "VkGeometryInstanceFlagBitsNV"
type GeometryInstanceFlagBitsNV = GeometryInstanceFlagBitsKHR


-- No documentation found for TopLevel "VkBuildAccelerationStructureFlagBitsNV"
type BuildAccelerationStructureFlagBitsNV = BuildAccelerationStructureFlagBitsKHR


-- No documentation found for TopLevel "VkCopyAccelerationStructureModeNV"
type CopyAccelerationStructureModeNV = CopyAccelerationStructureModeKHR


-- No documentation found for TopLevel "VkAccelerationStructureTypeNV"
type AccelerationStructureTypeNV = AccelerationStructureTypeKHR


-- No documentation found for TopLevel "VkGeometryTypeNV"
type GeometryTypeNV = GeometryTypeKHR


-- No documentation found for TopLevel "VkRayTracingShaderGroupTypeNV"
type RayTracingShaderGroupTypeNV = RayTracingShaderGroupTypeKHR


-- No documentation found for TopLevel "VkAabbPositionsNV"
type AabbPositionsNV = AabbPositionsKHR


-- No documentation found for TopLevel "VkTransformMatrixNV"
type TransformMatrixNV = TransformMatrixKHR


-- No documentation found for TopLevel "VkAccelerationStructureInstanceNV"
type AccelerationStructureInstanceNV = AccelerationStructureInstanceKHR


type NV_RAY_TRACING_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_SPEC_VERSION"
pattern NV_RAY_TRACING_SPEC_VERSION :: forall a . Integral a => a
pattern NV_RAY_TRACING_SPEC_VERSION = 3


type NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

-- No documentation found for TopLevel "VK_NV_RAY_TRACING_EXTENSION_NAME"
pattern NV_RAY_TRACING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_RAY_TRACING_EXTENSION_NAME = "VK_NV_ray_tracing"

