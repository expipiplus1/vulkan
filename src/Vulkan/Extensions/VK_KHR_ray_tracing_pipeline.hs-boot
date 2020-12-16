{-# language CPP #-}
-- | = Name
--
-- VK_KHR_ray_tracing_pipeline - device extension
--
-- == VK_KHR_ray_tracing_pipeline
--
-- [__Name String__]
--     @VK_KHR_ray_tracing_pipeline@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     348
--
-- [__Revision__]
--     1
--
-- [__Extension and Version Dependencies__]
--
--     -   Requires Vulkan 1.1
--
--     -   Requires @VK_KHR_spirv_1_4@
--
--     -   Requires @VK_KHR_acceleration_structure@
--
-- [__Contact__]
--
--     -   Daniel Koch
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?title=VK_KHR_ray_tracing_pipeline:%20&body=@dgkoch%20 >
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2020-11-12
--
-- [__Interactions and External Dependencies__]
--
--     -   This extension requires
--         <https://htmlpreview.github.io/?https://github.com/KhronosGroup/SPIRV-Registry/blob/master/extensions/KHR/SPV_KHR_ray_tracing.html SPV_KHR_ray_tracing>
--
--     -   This extension provides API support for
--         <https://github.com/KhronosGroup/GLSL/blob/master/extensions/ext/GLSL_EXT_ray_tracing.txt GLSL_EXT_ray_tracing>
--
--     -   This extension interacts with
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#version-1.2 Vulkan 1.2>
--         and
--         <VK_KHR_vulkan_memory_model.html VK_KHR_vulkan_memory_model>,
--         adding the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-call-related shader-call-related>
--         relation of invocations,
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-call-order shader-call-order>
--         partial order of dynamic instances of instructions, and the
--         <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shader-scope-shadercall ShaderCallKHR>
--         scope.
--
--     -   This extension interacts with
--         <VK_KHR_pipeline_library.html VK_KHR_pipeline_library>, enabling
--         pipeline libraries to be used with ray tracing pipelines and
--         enabling usage of 'RayTracingPipelineInterfaceCreateInfoKHR'.
--
-- [__Contributors__]
--
--     -   Matthäus Chajdas, AMD
--
--     -   Greg Grebe, AMD
--
--     -   Nicolai Hähnle, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Dave Oldcorn, AMD
--
--     -   Skyler Saleh, AMD
--
--     -   Mathieu Robart, Arm
--
--     -   Marius Bjorge, Arm
--
--     -   Tom Olson, Arm
--
--     -   Sebastian Tafuri, EA
--
--     -   Henrik Rydgard, Embark
--
--     -   Juan Cañada, Epic Games
--
--     -   Patrick Kelly, Epic Games
--
--     -   Yuriy O’Donnell, Epic Games
--
--     -   Michael Doggett, Facebook\/Oculus
--
--     -   Andrew Garrard, Imagination
--
--     -   Don Scorgie, Imagination
--
--     -   Dae Kim, Imagination
--
--     -   Joshua Barczak, Intel
--
--     -   Slawek Grajewski, Intel
--
--     -   Jeff Bolz, NVIDIA
--
--     -   Pascal Gautron, NVIDIA
--
--     -   Daniel Koch, NVIDIA
--
--     -   Christoph Kubisch, NVIDIA
--
--     -   Ashwin Lele, NVIDIA
--
--     -   Robert Stepinski, NVIDIA
--
--     -   Martin Stich, NVIDIA
--
--     -   Nuno Subtil, NVIDIA
--
--     -   Eric Werness, NVIDIA
--
--     -   Jon Leech, Khronos
--
--     -   Jeroen van Schijndel, OTOY
--
--     -   Juul Joosten, OTOY
--
--     -   Alex Bourd, Qualcomm
--
--     -   Roman Larionov, Qualcomm
--
--     -   David McAllister, Qualcomm
--
--     -   Spencer Fricke, Samsung
--
--     -   Lewis Gordon, Samsung
--
--     -   Ralph Potter, Samsung
--
--     -   Jasper Bekkers, Traverse Research
--
--     -   Jesse Barker, Unity
--
--     -   Baldur Karlsson, Valve
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
-- -   A new ray tracing pipeline type with new shader domains: ray
--     generation, intersection, any-hit, closest hit, miss, and callable
--
-- -   A shader binding indirection table to link shader groups with
--     acceleration structure items
--
-- -   Trace ray commands which initiates the ray pipeline traversal and
--     invocation of the various new shader domains depending on which
--     traversal conditions are met
--
-- This extension adds support for the following SPIR-V extension in
-- Vulkan:
--
-- -   @SPV_KHR_ray_tracing@
--
-- == New Commands
--
-- -   'cmdSetRayTracingPipelineStackSizeKHR'
--
-- -   'cmdTraceRaysIndirectKHR'
--
-- -   'cmdTraceRaysKHR'
--
-- -   'createRayTracingPipelinesKHR'
--
-- -   'getRayTracingCaptureReplayShaderGroupHandlesKHR'
--
-- -   'getRayTracingShaderGroupHandlesKHR'
--
-- -   'getRayTracingShaderGroupStackSizeKHR'
--
-- == New Structures
--
-- -   'RayTracingPipelineCreateInfoKHR'
--
-- -   'RayTracingPipelineInterfaceCreateInfoKHR'
--
-- -   'RayTracingShaderGroupCreateInfoKHR'
--
-- -   'StridedDeviceAddressRegionKHR'
--
-- -   'TraceRaysIndirectCommandKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceRayTracingPipelineFeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceRayTracingPipelinePropertiesKHR'
--
-- == New Enums
--
-- -   'RayTracingShaderGroupTypeKHR'
--
-- -   'ShaderGroupShaderKHR'
--
-- == New Enum Constants
--
-- -   'KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME'
--
-- -   'KHR_RAY_TRACING_PIPELINE_SPEC_VERSION'
--
-- -   'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BufferUsageFlagBits':
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.DynamicState.DynamicState':
--
--     -   'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.PipelineBindPoint.PipelineBindPoint':
--
--     -   'Vulkan.Core10.Enums.PipelineBindPoint.PIPELINE_BIND_POINT_RAY_TRACING_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PipelineStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_RAY_TRACING_SHADER_BIT_KHR'
--
-- -   Extending
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.ShaderStageFlagBits':
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ANY_HIT_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLOSEST_HIT_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_INTERSECTION_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_KHR'
--
--     -   'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR'
--
-- == New or Modified Built-In Variables
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-launchid LaunchIdKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-launchsize LaunchSizeKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-worldrayorigin WorldRayOriginKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-worldraydirection WorldRayDirectionKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-objectrayorigin ObjectRayOriginKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-objectraydirection ObjectRayDirectionKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-raytmin RayTminKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-raytmax RayTmaxKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-instancecustomindex InstanceCustomIndexKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-instanceid InstanceId>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-objecttoworld ObjectToWorldKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-worldtoobject WorldToObjectKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-hitkind HitKindKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-incomingrayflags IncomingRayFlagsKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces-builtin-variables-raygeometryindex RayGeometryIndexKHR>
--
-- -   (modified)@PrimitiveId@
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTracingKHR RayTracingKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTraversalPrimitiveCullingKHR RayTraversalPrimitiveCullingKHR>
--
-- == Issues
--
-- (1) How does this extension differ from VK_NV_ray_tracing?
--
-- __DISCUSSION__:
--
-- The following is a summary of the main functional differences between
-- VK_KHR_ray_tracing_pipeline and VK_NV_ray_tracing:
--
-- -   added support for indirect ray tracing ('cmdTraceRaysIndirectKHR')
--
-- -   uses SPV_KHR_ray_tracing instead of SPV_NV_ray_tracing
--
--     -   refer to KHR SPIR-V enums instead of NV SPIR-V enums (which are
--         functionally equivalent and aliased to the same values).
--
--     -   added @RayGeometryIndexKHR@ built-in
--
-- -   removed vkCompileDeferredNV compilation functionality and replaced
--     with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#deferred-host-operations deferred host operations>
--     interactions for ray tracing
--
-- -   added 'PhysicalDeviceRayTracingPipelineFeaturesKHR' structure
--
-- -   extended 'PhysicalDeviceRayTracingPipelinePropertiesKHR' structure
--
--     -   renamed @maxRecursionDepth@ to @maxRayRecursionDepth@ and it has
--         a minimum of 1 instead of 31
--
--     -   require @shaderGroupHandleSize@ to be 32 bytes
--
--     -   added @maxRayDispatchInvocationCount@,
--         @shaderGroupHandleAlignment@ and @maxRayHitAttributeSize@
--
-- -   reworked geometry structures so they could be better shared between
--     device, host, and indirect builds
--
-- -   changed SBT parameters to a structure and added size
--     ('StridedDeviceAddressRegionKHR')
--
-- -   add parameter for requesting memory requirements for host and\/or
--     device build
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-library pipeline library>
--     support for ray tracing
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-traversal-watertight watertightness guarantees>
--
-- -   added no-null-shader pipeline flags
--     (@VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_*_SHADERS_BIT_KHR@)
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-shader-call memory model interactions>
--     with ray tracing and define how subgroups work and can be repacked
--
-- (2) Can you give a more detailed comparision of differences and
-- similarities between VK_NV_ray_tracing and VK_KHR_ray_tracing_pipeline?
--
-- __DISCUSSION__:
--
-- The following is a more detailed comparision of which commands,
-- structures, and enums are aliased, changed, or removed.
--
-- -   Aliased functionality — enums, structures, and commands that are
--     considered equivalent:
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingShaderGroupTypeNV'
--         ↔ 'RayTracingShaderGroupTypeKHR'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.getRayTracingShaderGroupHandlesNV'
--         ↔ 'getRayTracingShaderGroupHandlesKHR'
--
-- -   Changed enums, structures, and commands:
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingShaderGroupCreateInfoNV'
--         → 'RayTracingShaderGroupCreateInfoKHR' (added
--         @pShaderGroupCaptureReplayHandle@)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV'
--         → 'RayTracingPipelineCreateInfoKHR' (changed type of @pGroups@,
--         added @libraries@, @pLibraryInterface@, and @pDynamicState@)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV'
--         → VkPhysicalDeviceRayTracingPropertiesKHR (renamed
--         @maxTriangleCount@ to @maxPrimitiveCount@, added
--         @shaderGroupHandleCaptureReplaySize@)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV' →
--         'cmdTraceRaysKHR' (params to struct)
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV'
--         → 'createRayTracingPipelinesKHR' (different struct, changed
--         functionality)
--
-- -   Added enums, structures and commands:
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR',
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR',
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--         to
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--
--     -   'PhysicalDeviceRayTracingPipelineFeaturesKHR' structure
--
--     -   'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressKHR'
--         and
--         'Vulkan.Extensions.VK_KHR_acceleration_structure.DeviceOrHostAddressConstKHR'
--         unions
--
--     -   'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--         struct
--
--     -   'RayTracingPipelineInterfaceCreateInfoKHR' struct
--
--     -   'StridedDeviceAddressRegionKHR' struct
--
--     -   'cmdTraceRaysIndirectKHR' command and
--         'TraceRaysIndirectCommandKHR' struct
--
--     -   'getRayTracingCaptureReplayShaderGroupHandlesKHR' (shader group
--         capture\/replay)
--
--     -   'cmdSetRayTracingPipelineStackSizeKHR' and
--         'getRayTracingShaderGroupStackSizeKHR' commands for stack size
--         control
--
-- -   Functionality removed:
--
--     -   'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DEFER_COMPILE_BIT_NV'
--
--     -   'Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV' command
--         (replaced with
--         <VK_KHR_deferred_host_operations.html VK_KHR_deferred_host_operations>)
--
-- (3) What are the changes between the public provisional
-- (VK_KHR_ray_tracing v8) release and the internal provisional
-- (VK_KHR_ray_tracing v9) release?
--
-- -   Require Vulkan 1.1 and SPIR-V 1.4
--
-- -   Added interactions with Vulkan 1.2 and
--     <VK_KHR_vulkan_memory_model.html VK_KHR_vulkan_memory_model>
--
-- -   added creation time capture and replay flags
--
--     -   added
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--         to
--         'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--
-- -   replace @VkStridedBufferRegionKHR@ with
--     'StridedDeviceAddressRegionKHR' and change 'cmdTraceRaysKHR',
--     'cmdTraceRaysIndirectKHR', to take these for the shader binding
--     table and use device addresses instead of buffers.
--
-- -   require the shader binding table buffers to have the
--     @VK_BUFFER_USAGE_RAY_TRACING_BIT_KHR@ set
--
-- -   make <VK_KHR_pipeline_library.html VK_KHR_pipeline_library> an
--     interaction instead of required extension
--
-- -   rename the @libraries@ member of 'RayTracingPipelineCreateInfoKHR'
--     to @pLibraryInfo@ and make it a pointer
--
-- -   make
--     <VK_KHR_deferred_host_operations.html VK_KHR_deferred_host_operations>
--     an interaction instead of a required extension (later went back on
--     this)
--
-- -   added explicit stack size management for ray tracing pipelines
--
--     -   removed the @maxCallableSize@ member of
--         'RayTracingPipelineInterfaceCreateInfoKHR'
--
--     -   added the @pDynamicState@ member to
--         'RayTracingPipelineCreateInfoKHR'
--
--     -   added
--         'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR'
--         dynamic state for ray tracing pipelines
--
--     -   added 'getRayTracingShaderGroupStackSizeKHR' and
--         'cmdSetRayTracingPipelineStackSizeKHR' commands
--
--     -   added 'ShaderGroupShaderKHR' enum
--
-- -   Added @maxRayDispatchInvocationCount@ limit to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'
--
-- -   Added @shaderGroupHandleAlignment@ property to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'
--
-- -   Added @maxRayHitAttributeSize@ property to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'
--
-- -   Clarify deferred host ops for pipeline creation
--
--     -   'Vulkan.Extensions.Handles.DeferredOperationKHR' is now a
--         top-level parameter for 'createRayTracingPipelinesKHR'
--
--     -   removed @VkDeferredOperationInfoKHR@ structure
--
--     -   change deferred host creation\/return parameter behavior such
--         that the implementation can modify such parameters until the
--         deferred host operation completes
--
--     -   <VK_KHR_deferred_host_operations.html VK_KHR_deferred_host_operations>
--         is required again
--
-- (4) What are the changes between the internal provisional
-- (VK_KHR_ray_tracing v9) release and the final
-- (VK_KHR_acceleration_structure v11 \/ VK_KHR_ray_tracing_pipeline v1)
-- release?
--
-- -   refactor VK_KHR_ray_tracing into 3 extensions, enabling
--     implementation flexibility and decoupling ray query support from ray
--     pipelines:
--
--     -   <VK_KHR_acceleration_structure.html VK_KHR_acceleration_structure>
--         (for acceleration structure operations)
--
--     -   <VK_KHR_ray_tracing_pipeline.html VK_KHR_ray_tracing_pipeline>
--         (for ray tracing pipeline and shader stages)
--
--     -   <VK_KHR_ray_query.html VK_KHR_ray_query> (for ray queries in
--         existing shader stages)
--
-- -   Require @Volatile@ for the following builtins in the ray generation,
--     closest hit, miss, intersection, and callable shader stages:
--
--     -   @SubgroupSize@, @SubgroupLocalInvocationId@, @SubgroupEqMask@,
--         @SubgroupGeMask@, @SubgroupGtMask@, @SubgroupLeMask@,
--         @SubgroupLtMask@
--
--     -   @SMIDNV@, @WarpIDNV@
--
-- -   clarify buffer usage flags for ray tracing
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--         is added as an alias of
--         'Vulkan.Extensions.VK_NV_ray_tracing.BUFFER_USAGE_RAY_TRACING_BIT_NV'
--         and is required on shader binding table buffers
--
--     -   'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_STORAGE_BUFFER_BIT'
--         is used in
--         <VK_KHR_acceleration_structure.html VK_KHR_acceleration_structure>
--         for @scratchData@
--
-- -   rename @maxRecursionDepth@ to @maxRayPipelineRecursionDepth@
--     (pipeline creation) and @maxRayRecursionDepth@ (limit) to reduce
--     confusion
--
-- -   Add queryable @maxRayHitAttributeSize@ limit and rename members of
--     'RayTracingPipelineInterfaceCreateInfoKHR' to
--     @maxPipelineRayPayloadSize@ and @maxPipelineRayHitAttributeSize@ for
--     clarity
--
-- -   Update SPIRV capabilities to use @RayTracingKHR@
--
-- -   extension is no longer provisional
--
-- -   define synchronization requirements for indirect trace rays and
--     indirect buffer
--
-- (5) This extension adds gl_InstanceID for the intersection, any-hit, and
-- closest hit shaders, but in KHR_vulkan_glsl, gl_InstanceID is replaced
-- with gl_InstanceIndex. Which should be used for Vulkan in this
-- extension?
--
-- RESOLVED: This extension uses gl_InstanceID and maps it to @InstanceId@
-- in SPIR-V. It is acknowledged that this is different than other shader
-- stages in Vulkan. There are two main reasons for the difference here:
--
-- -   symmetry with gl_PrimitiveID which is also available in these
--     shaders
--
-- -   there is no \"baseInstance\" relevant for these shaders, and so ID
--     makes it more obvious that this is zero-based.
--
-- == Sample Code
--
-- Example ray generation GLSL shader
--
-- > #version 450 core
-- > #extension GL_EXT_ray_tracing : require
-- > layout(set = 0, binding = 0, rgba8) uniform image2D image;
-- > layout(set = 0, binding = 1) uniform accelerationStructureEXT as;
-- > layout(location = 0) rayPayloadEXT float payload;
-- >
-- > void main()
-- > {
-- >    vec4 col = vec4(0, 0, 0, 1);
-- >
-- >    vec3 origin = vec3(float(gl_LaunchIDEXT.x)/float(gl_LaunchSizeEXT.x), float(gl_LaunchIDEXT.y)/float(gl_LaunchSizeEXT.y), 1.0);
-- >    vec3 dir = vec3(0.0, 0.0, -1.0);
-- >
-- >    traceRayEXT(as, 0, 0xff, 0, 1, 0, origin, 0.0, dir, 1000.0, 0);
-- >
-- >    col.y = payload;
-- >
-- >    imageStore(image, ivec2(gl_LaunchIDEXT.xy), col);
-- > }
--
-- == Version History
--
-- -   Revision 1, 2020-11-12 (Mathieu Robart, Daniel Koch, Eric Werness,
--     Tobias Hector)
--
--     -   Decomposition of the specification, from VK_KHR_ray_tracing to
--         VK_KHR_ray_tracing_pipeline (#1918,!3912)
--
--     -   require certain subgroup and sm_shader_builtin shader builtins
--         to be decorated as volatile in the ray generation, closest hit,
--         miss, intersection, and callable stages (#1924,!3903,!3954)
--
--     -   clarify buffer usage flags for ray tracing (#2181,!3939)
--
--     -   rename maxRecursionDepth to maxRayPipelineRecursionDepth and
--         maxRayRecursionDepth (#2203,!3937)
--
--     -   add queriable maxRayHitAttributeSize and rename members of
--         VkRayTracingPipelineInterfaceCreateInfoKHR (#2102,!3966)
--
--     -   update to use @RayTracingKHR@ SPIR-V capability
--
--     -   add VUs for matching hit group type against geometry type
--         (#2245,!3994)
--
--     -   require @RayTMaxKHR@ be volatile in intersection shaders
--         (#2268,!4030)
--
--     -   add numerical limits for ray parameters (#2235,!3960)
--
--     -   fix SBT indexing rules for device addresses (#2308,!4079)
--
--     -   relax formula for ray intersection candidate determination
--         (#2322,!4080)
--
--     -   add more details on @ShaderRecordBufferKHR@ variables
--         (#2230,!4083)
--
--     -   clarify valid bits for @InstanceCustomIndexKHR@
--         (GLSL\/GLSL#19,!4128)
--
--     -   allow at most one @IncomingRayPayloadKHR@,
--         @IncomingCallableDataKHR@, and @HitAttributeKHR@ (!4129)
--
--     -   add minimum for maxShaderGroupStride (#2353,!4131)
--
--     -   require VK_KHR_pipeline_library extension to be supported
--         (#2348,!4135)
--
--     -   clarify meaning of \'geometry index\' (#2272,!4137)
--
--     -   restrict traces to TLAS (#2239,!4141)
--
--     -   add note about maxPipelineRayPayloadSize (#2383,!4172)
--
--     -   do not require raygen shader in pipeline libraries (!4185)
--
--     -   define sync for indirect trace rays and indirect buffer
--         (#2407,!4208)
--
-- = See Also
--
-- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR',
-- 'PhysicalDeviceRayTracingPipelineFeaturesKHR',
-- 'PhysicalDeviceRayTracingPipelinePropertiesKHR',
-- 'RayTracingPipelineCreateInfoKHR',
-- 'RayTracingPipelineInterfaceCreateInfoKHR',
-- 'RayTracingShaderGroupCreateInfoKHR', 'RayTracingShaderGroupTypeKHR',
-- 'ShaderGroupShaderKHR', 'StridedDeviceAddressRegionKHR',
-- 'TraceRaysIndirectCommandKHR', 'cmdSetRayTracingPipelineStackSizeKHR',
-- 'cmdTraceRaysIndirectKHR', 'cmdTraceRaysKHR',
-- 'createRayTracingPipelinesKHR',
-- 'getRayTracingCaptureReplayShaderGroupHandlesKHR',
-- 'getRayTracingShaderGroupHandlesKHR',
-- 'getRayTracingShaderGroupStackSizeKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_ray_tracing_pipeline  ( PhysicalDeviceRayTracingPipelineFeaturesKHR
                                                      , PhysicalDeviceRayTracingPipelinePropertiesKHR
                                                      , RayTracingPipelineCreateInfoKHR
                                                      , RayTracingPipelineInterfaceCreateInfoKHR
                                                      , RayTracingShaderGroupCreateInfoKHR
                                                      , StridedDeviceAddressRegionKHR
                                                      , TraceRaysIndirectCommandKHR
                                                      , ShaderGroupShaderKHR
                                                      , RayTracingShaderGroupTypeKHR
                                                      ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data PhysicalDeviceRayTracingPipelineFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR
instance Show PhysicalDeviceRayTracingPipelineFeaturesKHR

instance FromCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR


data PhysicalDeviceRayTracingPipelinePropertiesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR
instance Show PhysicalDeviceRayTracingPipelinePropertiesKHR

instance FromCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR


type role RayTracingPipelineCreateInfoKHR nominal
data RayTracingPipelineCreateInfoKHR (es :: [Type])

instance (Extendss RayTracingPipelineCreateInfoKHR es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoKHR es)
instance Show (Chain es) => Show (RayTracingPipelineCreateInfoKHR es)

instance (Extendss RayTracingPipelineCreateInfoKHR es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoKHR es)


data RayTracingPipelineInterfaceCreateInfoKHR

instance ToCStruct RayTracingPipelineInterfaceCreateInfoKHR
instance Show RayTracingPipelineInterfaceCreateInfoKHR

instance FromCStruct RayTracingPipelineInterfaceCreateInfoKHR


data RayTracingShaderGroupCreateInfoKHR

instance ToCStruct RayTracingShaderGroupCreateInfoKHR
instance Show RayTracingShaderGroupCreateInfoKHR

instance FromCStruct RayTracingShaderGroupCreateInfoKHR


data StridedDeviceAddressRegionKHR

instance ToCStruct StridedDeviceAddressRegionKHR
instance Show StridedDeviceAddressRegionKHR

instance FromCStruct StridedDeviceAddressRegionKHR


data TraceRaysIndirectCommandKHR

instance ToCStruct TraceRaysIndirectCommandKHR
instance Show TraceRaysIndirectCommandKHR

instance FromCStruct TraceRaysIndirectCommandKHR


data ShaderGroupShaderKHR


data RayTracingShaderGroupTypeKHR

