{-# language CPP #-}
-- | = Name
--
-- VK_KHR_ray_tracing_maintenance1 - device extension
--
-- == VK_KHR_ray_tracing_maintenance1
--
-- [__Name String__]
--     @VK_KHR_ray_tracing_maintenance1@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     387
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
--     -   Requires @VK_KHR_acceleration_structure@
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_ray_tracing_maintenance1] @dgkoch%0A<<Here describe the issue or question you have about the VK_KHR_ray_tracing_maintenance1 extension>> >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2022-02-21
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_ray_cull_mask.html SPV_KHR_ray_cull_mask>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_ray_cull_mask.txt GLSL_EXT_ray_cull_mask>
--
--     -   Interacts with @VK_KHR_ray_tracing_pipeline@
--
--     -   Interacts with @VK_KHR_synchronization2@
--
-- [__Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Marius Bjorge, Arm
--
--     -   Tom Olson, Arm
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Yunpeng Zhu, Huawei
--
--     -   Andrew Garrard, Imagination
--
--     -   Dae Kim, Imagination
--
--     -   Joshua Barczak, Intel
--
--     -   Lionel Landwerlin, Intel
--
--     -   Daniel Koch, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Spencer Fricke, Samsung
--
-- == Description
--
-- @VK_KHR_ray_tracing_maintenance1@ adds a collection of minor ray tracing
-- features, none of which would warrant an entire extension of their own.
--
-- The new features are as follows:
--
-- -   Adds support for the @SPV_KHR_ray_cull_mask@ SPIR-V extension in
--     Vulkan. This extension provides access to built-in @CullMaskKHR@
--     shader variable which contains the value of the @OpTrace*@
--     @Cull Mask@ parameter. This new shader variable is accessible in the
--     intersection, any-hit, closest-hit and miss shader stages.
--
-- -   Adds support for a new pipeline stage and access mask built on top
--     of @VK_KHR_synchronization2@:
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
--         to specify execution of
--         <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure-copying acceleration structure copy commands>
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR'
--         to specify read access to a
--         <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shader-binding-table shader binding table>
--         in any shader pipeline stage
--
-- -   Adds two new acceleration structure query parameters:
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'
--         to query the acceleration structure size on the device timeline
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--         to query the number of bottom level acceleration structure
--         pointers for serialization
--
-- -   Adds an optional new indirect ray tracing dispatch command,
--     'cmdTraceRaysIndirect2KHR', which sources the shader binding table
--     parameters as well as the dispatch dimensions from the device. The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTracingPipelineTraceRaysIndirect2 rayTracingPipelineTraceRaysIndirect2>
--     feature indicates whether this functionality is supported.
--
-- == New Commands
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   'cmdTraceRaysIndirect2KHR'
--
-- == New Structures
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingMaintenance1FeaturesKHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   'TraceRaysIndirectCommand2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_RAY_TRACING_MAINTENANCE_1_EXTENSION_NAME'
--
-- -   'KHR_RAY_TRACING_MAINTENANCE_1_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.QueryType.QueryType':
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--
--     -   'Vulkan.Core10.Enums.QueryType.QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_MAINTENANCE_1_FEATURES_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>
-- is supported:
--
-- -   Extending
--     'Vulkan.Core13.Enums.PipelineStageFlags2.PipelineStageFlagBits2':
--
--     -   'Vulkan.Core13.Enums.PipelineStageFlags2.PIPELINE_STAGE_2_ACCELERATION_STRUCTURE_COPY_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_synchronization2 VK_KHR_synchronization2>,VK_KHR_ray_tracing_pipeline
-- is supported:
--
-- -   Extending 'Vulkan.Core13.Enums.AccessFlags2.AccessFlagBits2':
--
--     -   'Vulkan.Core13.Enums.AccessFlags2.ACCESS_2_SHADER_BINDING_TABLE_READ_BIT_KHR'
--
-- == New Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-cullmask CullMaskKHR>
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-RayCullMaskKHR RayCullMaskKHR>
--
-- == Issues
--
-- None Yet!
--
-- == Version History
--
-- -   Revision 1, 2022-02-21 (Members of the Vulkan Ray Tracing TSG)
--
--     -   internal revisions
--
-- == See Also
--
-- 'PhysicalDeviceRayTracingMaintenance1FeaturesKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_ray_tracing_maintenance1 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_ray_tracing_maintenance1  ( PhysicalDeviceRayTracingMaintenance1FeaturesKHR
                                                          , TraceRaysIndirectCommand2KHR
                                                          ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data PhysicalDeviceRayTracingMaintenance1FeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingMaintenance1FeaturesKHR
instance Show PhysicalDeviceRayTracingMaintenance1FeaturesKHR

instance FromCStruct PhysicalDeviceRayTracingMaintenance1FeaturesKHR


data TraceRaysIndirectCommand2KHR

instance ToCStruct TraceRaysIndirectCommand2KHR
instance Show TraceRaysIndirectCommand2KHR

instance FromCStruct TraceRaysIndirectCommand2KHR

