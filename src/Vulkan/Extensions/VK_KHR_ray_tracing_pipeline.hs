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
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_ray_tracing_pipeline] @dgkoch%0A<<Here describe the issue or question you have about the VK_KHR_ray_tracing_pipeline extension>> >
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
--         <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#versions-1.2 Vulkan 1.2>
--         and @VK_KHR_vulkan_memory_model@, adding the
--         <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shader-call-related shader-call-related>
--         relation of invocations,
--         <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shader-call-order shader-call-order>
--         partial order of dynamic instances of instructions, and the
--         <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#shaders-scope-shadercall ShaderCallKHR>
--         scope.
--
--     -   This extension interacts with @VK_KHR_pipeline_library@,
--         enabling pipeline libraries to be used with ray tracing
--         pipelines and enabling usage of
--         'RayTracingPipelineInterfaceCreateInfoKHR'.
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
-- -   Ray tracing commands which initiate the ray pipeline traversal and
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
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-launchid LaunchIdKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-launchsize LaunchSizeKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-worldrayorigin WorldRayOriginKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-worldraydirection WorldRayDirectionKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-objectrayorigin ObjectRayOriginKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-objectraydirection ObjectRayDirectionKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-raytmin RayTminKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-raytmax RayTmaxKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-instancecustomindex InstanceCustomIndexKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-instanceid InstanceId>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-objecttoworld ObjectToWorldKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-worldtoobject WorldToObjectKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-hitkind HitKindKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-incomingrayflags IncomingRayFlagsKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-builtin-variables-raygeometryindex RayGeometryIndexKHR>
--
-- -   (modified)@PrimitiveId@
--
-- == New SPIR-V Capabilities
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTracingKHR RayTracingKHR>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#spirvenv-capabilities-table-RayTraversalPrimitiveCullingKHR RayTraversalPrimitiveCullingKHR>
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
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#deferred-host-operations deferred host operations>
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
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipeline-library pipeline library>
--     support for ray tracing
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-traversal-watertight watertightness guarantees>
--
-- -   added no-null-shader pipeline flags
--     (@VK_PIPELINE_CREATE_RAY_TRACING_NO_NULL_*_SHADERS_BIT_KHR@)
--
-- -   added
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-tracing-shader-call memory model interactions>
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
--         (replaced with @VK_KHR_deferred_host_operations@)
--
-- (3) What are the changes between the public provisional
-- (VK_KHR_ray_tracing v8) release and the internal provisional
-- (VK_KHR_ray_tracing v9) release?
--
-- -   Require Vulkan 1.1 and SPIR-V 1.4
--
-- -   Added interactions with Vulkan 1.2 and @VK_KHR_vulkan_memory_model@
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
-- -   make @VK_KHR_pipeline_library@ an interaction instead of required
--     extension
--
-- -   rename the @libraries@ member of 'RayTracingPipelineCreateInfoKHR'
--     to @pLibraryInfo@ and make it a pointer
--
-- -   make @VK_KHR_deferred_host_operations@ an interaction instead of a
--     required extension (later went back on this)
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
--     -   @VK_KHR_deferred_host_operations@ is required again
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
--     -   @VK_KHR_acceleration_structure@ (for acceleration structure
--         operations)
--
--     -   @VK_KHR_ray_tracing_pipeline@ (for ray tracing pipeline and
--         shader stages)
--
--     -   @VK_KHR_ray_query@ (for ray queries in existing shader stages)
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
--         is used in @VK_KHR_acceleration_structure@ for @scratchData@
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
-- __RESOLVED__: This extension uses gl_InstanceID and maps it to
-- @InstanceId@ in SPIR-V. It is acknowledged that this is different than
-- other shader stages in Vulkan. There are two main reasons for the
-- difference here:
--
-- -   symmetry with gl_PrimitiveID which is also available in these
--     shaders
--
-- -   there is no “baseInstance” relevant for these shaders, and so ID
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
-- == See Also
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
-- == Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_ray_tracing_pipeline  ( cmdTraceRaysKHR
                                                      , getRayTracingShaderGroupHandlesKHR
                                                      , getRayTracingCaptureReplayShaderGroupHandlesKHR
                                                      , createRayTracingPipelinesKHR
                                                      , withRayTracingPipelinesKHR
                                                      , cmdTraceRaysIndirectKHR
                                                      , getRayTracingShaderGroupStackSizeKHR
                                                      , cmdSetRayTracingPipelineStackSizeKHR
                                                      , RayTracingShaderGroupCreateInfoKHR(..)
                                                      , RayTracingPipelineCreateInfoKHR(..)
                                                      , PhysicalDeviceRayTracingPipelineFeaturesKHR(..)
                                                      , PhysicalDeviceRayTracingPipelinePropertiesKHR(..)
                                                      , StridedDeviceAddressRegionKHR(..)
                                                      , TraceRaysIndirectCommandKHR(..)
                                                      , RayTracingPipelineInterfaceCreateInfoKHR(..)
                                                      , RayTracingShaderGroupTypeKHR( RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR
                                                                                    , RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR
                                                                                    , RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR
                                                                                    , ..
                                                                                    )
                                                      , ShaderGroupShaderKHR( SHADER_GROUP_SHADER_GENERAL_KHR
                                                                            , SHADER_GROUP_SHADER_CLOSEST_HIT_KHR
                                                                            , SHADER_GROUP_SHADER_ANY_HIT_KHR
                                                                            , SHADER_GROUP_SHADER_INTERSECTION_KHR
                                                                            , ..
                                                                            )
                                                      , KHR_RAY_TRACING_PIPELINE_SPEC_VERSION
                                                      , pattern KHR_RAY_TRACING_PIPELINE_SPEC_VERSION
                                                      , KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                                                      , pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME
                                                      , DeferredOperationKHR(..)
                                                      , PipelineLibraryCreateInfoKHR(..)
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
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Alloc (callocBytes)
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Utils (maybePeek)
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
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.Core10.Pipeline (destroyPipeline)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (pokeSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.AllocationCallbacks (AllocationCallbacks)
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Extensions.Handles (DeferredOperationKHR)
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Core10.FundamentalTypes (DeviceAddress)
import Vulkan.Dynamic (DeviceCmds(pVkCmdSetRayTracingPipelineStackSizeKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysIndirectKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCmdTraceRaysKHR))
import Vulkan.Dynamic (DeviceCmds(pVkCreateRayTracingPipelinesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingShaderGroupHandlesKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRayTracingShaderGroupStackSizeKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.Core10.Handles (Pipeline)
import Vulkan.Core10.Handles (Pipeline(..))
import Vulkan.Core10.Handles (PipelineCache)
import Vulkan.Core10.Handles (PipelineCache(..))
import Vulkan.Core10.Enums.PipelineCreateFlagBits (PipelineCreateFlags)
import {-# SOURCE #-} Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback (PipelineCreationFeedbackCreateInfo)
import Vulkan.Core10.Pipeline (PipelineDynamicStateCreateInfo)
import Vulkan.Core10.Handles (PipelineLayout)
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR)
import Vulkan.Core10.Pipeline (PipelineShaderStageCreateInfo)
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.Core10.Enums.Result (Result)
import Vulkan.Core10.Enums.Result (Result(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.Core10.Enums.StructureType (StructureType)
import Vulkan.Exception (VulkanException(..))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.Result (Result(SUCCESS))
import Vulkan.Extensions.Handles (DeferredOperationKHR(..))
import Vulkan.Extensions.VK_KHR_pipeline_library (PipelineLibraryCreateInfoKHR(..))
import Vulkan.Core10.APIConstants (SHADER_UNUSED_KHR)
import Vulkan.Core10.APIConstants (pattern SHADER_UNUSED_KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Word32 -> Word32 -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Word32 -> Word32 -> Word32 -> IO ()

-- | vkCmdTraceRaysKHR - Initialize a ray tracing dispatch
--
-- = Description
--
-- When the command is executed, a ray generation group of @width@ ×
-- @height@ × @depth@ rays is assembled.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdTraceRaysKHR-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysKHR-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysKHR-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdTraceRaysKHR-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdTraceRaysKHR-filterCubicMinmax-02695# Any
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
-- -   #VUID-vkCmdTraceRaysKHR-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdTraceRaysKHR-OpTypeImage-06423# Any
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' being written as a storage image
--     or storage texel buffer where the image format field of the
--     @OpTypeImage@ is @Unknown@ /must/ have image format features that
--     support
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysKHR-OpTypeImage-06424# Any
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' being read as a storage image or
--     storage texel buffer where the image format field of the
--     @OpTypeImage@ is @Unknown@ /must/ have image format features that
--     support
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysKHR-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02699# Descriptors in each bound
--     descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02700# A valid pipeline /must/ be bound
--     to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdTraceRaysKHR-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02859# There /must/ not have been any
--     calls to dynamic state setting commands for any state not specified
--     as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object bound to
--     the pipeline bind point used by this command, since that pipeline
--     was bound
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02702# If the
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
-- -   #VUID-vkCmdTraceRaysKHR-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysKHR-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysKHR-commandBuffer-02707# If @commandBuffer@ is
--     an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdTraceRaysKHR-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdTraceRaysKHR-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdTraceRaysKHR-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdTraceRaysKHR-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdTraceRaysKHR-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdTraceRaysKHR-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdTraceRaysKHR-sparseImageInt64Atomics-04474# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdTraceRaysKHR-sparseImageInt64Atomics-04475# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdTraceRaysKHR-None-03429# Any shader group handle
--     referenced by this call /must/ have been queried from the currently
--     bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysKHR-maxPipelineRayRecursionDepth-03679# This
--     command /must/ not cause a shader call instruction to be executed
--     from a shader invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxPipelineRayRecursionDepth@ used to
--     create the bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysKHR-pRayGenShaderBindingTable-03680# If the
--     buffer from which @pRayGenShaderBindingTable->deviceAddress@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysKHR-pRayGenShaderBindingTable-03681# The buffer
--     from which the @pRayGenShaderBindingTable->deviceAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysKHR-pRayGenShaderBindingTable-03682#
--     @pRayGenShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-size-04023# The @size@ member of
--     @pRayGenShaderBindingTable@ /must/ be equal to its @stride@ member
--
-- -   #VUID-vkCmdTraceRaysKHR-pMissShaderBindingTable-03683# If the buffer
--     from which @pMissShaderBindingTable->deviceAddress@ was queried is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysKHR-pMissShaderBindingTable-03684# The buffer
--     from which the @pMissShaderBindingTable->deviceAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysKHR-pMissShaderBindingTable-03685#
--     @pMissShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-stride-03686# The @stride@ member of
--     @pMissShaderBindingTable@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-stride-04029# The @stride@ member of
--     @pMissShaderBindingTable@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysKHR-pHitShaderBindingTable-03687# If the buffer
--     from which @pHitShaderBindingTable->deviceAddress@ was queried is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysKHR-pHitShaderBindingTable-03688# The buffer
--     from which the @pHitShaderBindingTable->deviceAddress@ is queried
--     /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysKHR-pHitShaderBindingTable-03689#
--     @pHitShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-stride-03690# The @stride@ member of
--     @pHitShaderBindingTable@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-stride-04035# The @stride@ member of
--     @pHitShaderBindingTable@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysKHR-pCallableShaderBindingTable-03691# If the
--     buffer from which @pCallableShaderBindingTable->deviceAddress@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysKHR-pCallableShaderBindingTable-03692# The
--     buffer from which the @pCallableShaderBindingTable->deviceAddress@
--     is queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysKHR-pCallableShaderBindingTable-03693#
--     @pCallableShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-stride-03694# The @stride@ member of
--     @pCallableShaderBindingTable@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-vkCmdTraceRaysKHR-stride-04041# The @stride@ member of
--     @pCallableShaderBindingTable@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysKHR-flags-03696# If the currently bound ray
--     tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     the @deviceAddress@ member of @pHitShaderBindingTable@ /must/ not be
--     zero
--
-- -   #VUID-vkCmdTraceRaysKHR-flags-03697# If the currently bound ray
--     tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     the @deviceAddress@ member of @pHitShaderBindingTable@ /must/ not be
--     zero
--
-- -   #VUID-vkCmdTraceRaysKHR-flags-03511# If the currently bound ray
--     tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR',
--     the shader group handle identified by @pMissShaderBindingTable@
--     /must/ not be set to zero
--
-- -   #VUID-vkCmdTraceRaysKHR-flags-03512# If the currently bound ray
--     tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     entries in @pHitShaderBindingTable@ accessed as a result of this
--     command in order to execute an any-hit shader /must/ not be set to
--     zero
--
-- -   #VUID-vkCmdTraceRaysKHR-flags-03513# If the currently bound ray
--     tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     entries in @pHitShaderBindingTable@ accessed as a result of this
--     command in order to execute a closest hit shader /must/ not be set
--     to zero
--
-- -   #VUID-vkCmdTraceRaysKHR-flags-03514# If the currently bound ray
--     tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     entries in @pHitShaderBindingTable@ accessed as a result of this
--     command in order to execute an intersection shader /must/ not be set
--     to zero
--
-- -   #VUID-vkCmdTraceRaysKHR-pHitShaderBindingTable-04735# Any non-zero
--     hit shader group entries in @pHitShaderBindingTable@ accessed by
--     this call from a geometry with a @geometryType@ of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_TRIANGLES_KHR'
--     /must/ have been created with
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR'
--
-- -   #VUID-vkCmdTraceRaysKHR-pHitShaderBindingTable-04736# Any non-zero
--     hit shader group entries in @pHitShaderBindingTable@ accessed by
--     this call from a geometry with a @geometryType@ of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_AABBS_KHR'
--     /must/ have been created with
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR'
--
-- -   #VUID-vkCmdTraceRaysKHR-commandBuffer-04625# @commandBuffer@ /must/
--     not be a protected command buffer
--
-- -   #VUID-vkCmdTraceRaysKHR-width-03626# @width@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[0]
--
-- -   #VUID-vkCmdTraceRaysKHR-height-03627# @height@ /must/ be less than
--     or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[1]
--
-- -   #VUID-vkCmdTraceRaysKHR-depth-03628# @depth@ /must/ be less than or
--     equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[2]
--
-- -   #VUID-vkCmdTraceRaysKHR-width-03629# @width@ × @height@ × @depth@
--     /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxRayDispatchInvocationCount@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdTraceRaysKHR-commandBuffer-parameter# @commandBuffer@
--     /must/ be a valid 'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdTraceRaysKHR-pRaygenShaderBindingTable-parameter#
--     @pRaygenShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysKHR-pMissShaderBindingTable-parameter#
--     @pMissShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysKHR-pHitShaderBindingTable-parameter#
--     @pHitShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysKHR-pCallableShaderBindingTable-parameter#
--     @pCallableShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysKHR-commandBuffer-recording# @commandBuffer@
--     /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdTraceRaysKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdTraceRaysKHR-renderpass# This command /must/ only be
--     called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.CommandBuffer', 'StridedDeviceAddressRegionKHR'
cmdTraceRaysKHR :: forall io
                 . (MonadIO io)
                => -- | @commandBuffer@ is the command buffer into which the command will be
                   -- recorded.
                   CommandBuffer
                -> -- | @pRaygenShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that
                   -- holds the shader binding table data for the ray generation shader stage.
                   ("raygenShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- | @pMissShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that
                   -- holds the shader binding table data for the miss shader stage.
                   ("missShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- | @pHitShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that holds
                   -- the shader binding table data for the hit shader stage.
                   ("hitShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- | @pCallableShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that
                   -- holds the shader binding table data for the callable shader stage.
                   ("callableShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                -> -- | @width@ is the width of the ray trace query dimensions.
                   ("width" ::: Word32)
                -> -- | @height@ is height of the ray trace query dimensions.
                   ("height" ::: Word32)
                -> -- | @depth@ is depth of the ray trace query dimensions.
                   ("depth" ::: Word32)
                -> io ()
cmdTraceRaysKHR commandBuffer raygenShaderBindingTable missShaderBindingTable hitShaderBindingTable callableShaderBindingTable width height depth = liftIO . evalContT $ do
  let vkCmdTraceRaysKHRPtr = pVkCmdTraceRaysKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdTraceRaysKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysKHR is null" Nothing Nothing
  let vkCmdTraceRaysKHR' = mkVkCmdTraceRaysKHR vkCmdTraceRaysKHRPtr
  pRaygenShaderBindingTable <- ContT $ withCStruct (raygenShaderBindingTable)
  pMissShaderBindingTable <- ContT $ withCStruct (missShaderBindingTable)
  pHitShaderBindingTable <- ContT $ withCStruct (hitShaderBindingTable)
  pCallableShaderBindingTable <- ContT $ withCStruct (callableShaderBindingTable)
  lift $ traceAroundEvent "vkCmdTraceRaysKHR" (vkCmdTraceRaysKHR' (commandBufferHandle (commandBuffer)) pRaygenShaderBindingTable pMissShaderBindingTable pHitShaderBindingTable pCallableShaderBindingTable (width) (height) (depth))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupHandlesKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result

-- | vkGetRayTracingShaderGroupHandlesKHR - Query ray tracing pipeline shader
-- group handles
--
-- == Valid Usage
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-pipeline-04619#
--     @pipeline@ /must/ be a ray tracing pipeline
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-firstGroup-04050#
--     @firstGroup@ /must/ be less than the number of shader groups in
--     @pipeline@
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-firstGroup-02419# The sum
--     of @firstGroup@ and @groupCount@ /must/ be less than or equal to the
--     number of shader groups in @pipeline@
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-dataSize-02420#
--     @dataSize@ /must/ be at least
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleSize@
--     × @groupCount@
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-pipeline-03482#
--     @pipeline@ /must/ have not been created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-pipeline-parameter#
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-pData-parameter# @pData@
--     /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-dataSize-arraylength#
--     @dataSize@ /must/ be greater than @0@
--
-- -   #VUID-vkGetRayTracingShaderGroupHandlesKHR-pipeline-parent#
--     @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
getRayTracingShaderGroupHandlesKHR :: forall io
                                    . (MonadIO io)
                                   => -- | @device@ is the logical device containing the ray tracing pipeline.
                                      Device
                                   -> -- | @pipeline@ is the ray tracing pipeline object containing the shaders.
                                      Pipeline
                                   -> -- | @firstGroup@ is the index of the first group to retrieve a handle for
                                      -- from the 'RayTracingPipelineCreateInfoKHR'::@pGroups@ or
                                      -- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV'::@pGroups@
                                      -- array.
                                      ("firstGroup" ::: Word32)
                                   -> -- | @groupCount@ is the number of shader handles to retrieve.
                                      ("groupCount" ::: Word32)
                                   -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                                      ("dataSize" ::: Word64)
                                   -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                                      -- be written.
                                      ("data" ::: Ptr ())
                                   -> io ()
getRayTracingShaderGroupHandlesKHR device pipeline firstGroup groupCount dataSize data' = liftIO $ do
  let vkGetRayTracingShaderGroupHandlesKHRPtr = pVkGetRayTracingShaderGroupHandlesKHR (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetRayTracingShaderGroupHandlesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRayTracingShaderGroupHandlesKHR is null" Nothing Nothing
  let vkGetRayTracingShaderGroupHandlesKHR' = mkVkGetRayTracingShaderGroupHandlesKHR vkGetRayTracingShaderGroupHandlesKHRPtr
  r <- traceAroundEvent "vkGetRayTracingShaderGroupHandlesKHR" (vkGetRayTracingShaderGroupHandlesKHR' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data'))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingCaptureReplayShaderGroupHandlesKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result) -> Ptr Device_T -> Pipeline -> Word32 -> Word32 -> CSize -> Ptr () -> IO Result

-- | vkGetRayTracingCaptureReplayShaderGroupHandlesKHR - Query ray tracing
-- capture replay pipeline shader group handles
--
-- == Valid Usage
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-pipeline-04620#
--     @pipeline@ /must/ be a ray tracing pipeline
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-firstGroup-04051#
--     @firstGroup@ /must/ be less than the number of shader groups in
--     @pipeline@
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-firstGroup-03483#
--     The sum of @firstGroup@ and @groupCount@ /must/ be less than or
--     equal to the number of shader groups in @pipeline@
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-dataSize-03484#
--     @dataSize@ /must/ be at least
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleCaptureReplaySize@
--     × @groupCount@
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-rayTracingPipelineShaderGroupHandleCaptureReplay-03606#
--     'PhysicalDeviceRayTracingPipelineFeaturesKHR'::@rayTracingPipelineShaderGroupHandleCaptureReplay@
--     /must/ be enabled to call this function
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-pipeline-03607#
--     @pipeline@ /must/ have been created with a @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-pipeline-parameter#
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-pData-parameter#
--     @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-dataSize-arraylength#
--     @dataSize@ /must/ be greater than @0@
--
-- -   #VUID-vkGetRayTracingCaptureReplayShaderGroupHandlesKHR-pipeline-parent#
--     @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
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
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline'
getRayTracingCaptureReplayShaderGroupHandlesKHR :: forall io
                                                 . (MonadIO io)
                                                => -- | @device@ is the logical device containing the ray tracing pipeline.
                                                   Device
                                                -> -- | @pipeline@ is the ray tracing pipeline object containing the shaders.
                                                   Pipeline
                                                -> -- | @firstGroup@ is the index of the first group to retrieve a handle for
                                                   -- from the 'RayTracingPipelineCreateInfoKHR'::@pGroups@ array.
                                                   ("firstGroup" ::: Word32)
                                                -> -- | @groupCount@ is the number of shader handles to retrieve.
                                                   ("groupCount" ::: Word32)
                                                -> -- | @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
                                                   ("dataSize" ::: Word64)
                                                -> -- | @pData@ is a pointer to a user-allocated buffer where the results will
                                                   -- be written.
                                                   ("data" ::: Ptr ())
                                                -> io ()
getRayTracingCaptureReplayShaderGroupHandlesKHR device pipeline firstGroup groupCount dataSize data' = liftIO $ do
  let vkGetRayTracingCaptureReplayShaderGroupHandlesKHRPtr = pVkGetRayTracingCaptureReplayShaderGroupHandlesKHR (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetRayTracingCaptureReplayShaderGroupHandlesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRayTracingCaptureReplayShaderGroupHandlesKHR is null" Nothing Nothing
  let vkGetRayTracingCaptureReplayShaderGroupHandlesKHR' = mkVkGetRayTracingCaptureReplayShaderGroupHandlesKHR vkGetRayTracingCaptureReplayShaderGroupHandlesKHRPtr
  r <- traceAroundEvent "vkGetRayTracingCaptureReplayShaderGroupHandlesKHR" (vkGetRayTracingCaptureReplayShaderGroupHandlesKHR' (deviceHandle (device)) (pipeline) (firstGroup) (groupCount) (CSize (dataSize)) (data'))
  when (r < SUCCESS) (throwIO (VulkanException r))


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesKHR
  :: FunPtr (Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result) -> Ptr Device_T -> DeferredOperationKHR -> PipelineCache -> Word32 -> Ptr (SomeStruct RayTracingPipelineCreateInfoKHR) -> Ptr AllocationCallbacks -> Ptr Pipeline -> IO Result

-- | vkCreateRayTracingPipelinesKHR - Creates a new ray tracing pipeline
-- object
--
-- = Description
--
-- The 'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
-- error is returned if the implementation is unable to re-use the shader
-- group handles provided in
-- 'RayTracingShaderGroupCreateInfoKHR'::@pShaderGroupCaptureReplayHandle@
-- when
-- 'PhysicalDeviceRayTracingPipelineFeaturesKHR'::@rayTracingPipelineShaderGroupHandleCaptureReplay@
-- is enabled.
--
-- == Valid Usage
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-flags-03415# If the @flags@
--     member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and the @basePipelineIndex@ member of that same element is not
--     @-1@, @basePipelineIndex@ /must/ be less than the index into
--     @pCreateInfos@ that corresponds to that element
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-flags-03416# If the @flags@
--     member of any element of @pCreateInfos@ contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, the base pipeline /must/ have been created with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_ALLOW_DERIVATIVES_BIT'
--     flag set
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-flags-03816# @flags@ /must/ not
--     contain the
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.PIPELINE_CREATE_DISPATCH_BASE'
--     flag
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-pipelineCache-02903# If
--     @pipelineCache@ was created with
--     'Vulkan.Core10.Enums.PipelineCacheCreateFlagBits.PIPELINE_CACHE_CREATE_EXTERNALLY_SYNCHRONIZED_BIT',
--     host access to @pipelineCache@ /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-threadingbehavior externally synchronized>
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-deferredOperation-03677# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     it /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' object
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-deferredOperation-03678# Any
--     previous deferred operation that was associated with
--     @deferredOperation@ /must/ be complete
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-rayTracingPipeline-03586# The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTracingPipeline rayTracingPipeline>
--     feature /must/ be enabled
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-deferredOperation-03587# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     the @flags@ member of elements of @pCreateInfos@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-deferredOperation-parameter# If
--     @deferredOperation@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @deferredOperation@ /must/ be a valid
--     'Vulkan.Extensions.Handles.DeferredOperationKHR' handle
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-pipelineCache-parameter# If
--     @pipelineCache@ is not 'Vulkan.Core10.APIConstants.NULL_HANDLE',
--     @pipelineCache@ /must/ be a valid
--     'Vulkan.Core10.Handles.PipelineCache' handle
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-pCreateInfos-parameter#
--     @pCreateInfos@ /must/ be a valid pointer to an array of
--     @createInfoCount@ valid 'RayTracingPipelineCreateInfoKHR' structures
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-pAllocator-parameter# If
--     @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid pointer
--     to a valid 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks'
--     structure
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-pPipelines-parameter#
--     @pPipelines@ /must/ be a valid pointer to an array of
--     @createInfoCount@ 'Vulkan.Core10.Handles.Pipeline' handles
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-createInfoCount-arraylength#
--     @createInfoCount@ /must/ be greater than @0@
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-deferredOperation-parent# If
--     @deferredOperation@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- -   #VUID-vkCreateRayTracingPipelinesKHR-pipelineCache-parent# If
--     @pipelineCache@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--
--     -   'Vulkan.Core10.Enums.Result.SUCCESS'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_DEFERRED_KHR'
--
--     -   'Vulkan.Core10.Enums.Result.OPERATION_NOT_DEFERRED_KHR'
--
--     -   'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PIPELINE_COMPILE_REQUIRED_EXT'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Vulkan.Core10.Enums.Result.ERROR_INVALID_OPAQUE_CAPTURE_ADDRESS'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.AllocationCallbacks.AllocationCallbacks',
-- 'Vulkan.Extensions.Handles.DeferredOperationKHR',
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Handles.PipelineCache', 'RayTracingPipelineCreateInfoKHR'
createRayTracingPipelinesKHR :: forall io
                              . (MonadIO io)
                             => -- | @device@ is the logical device that creates the ray tracing pipelines.
                                Device
                             -> -- | @deferredOperation@ is 'Vulkan.Core10.APIConstants.NULL_HANDLE' or the
                                -- handle of a valid 'Vulkan.Extensions.Handles.DeferredOperationKHR'
                                -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#deferred-host-operations-requesting request deferral>
                                -- object for this command.
                                DeferredOperationKHR
                             -> -- | @pipelineCache@ is either 'Vulkan.Core10.APIConstants.NULL_HANDLE',
                                -- indicating that pipeline caching is disabled, or the handle of a valid
                                -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-cache pipeline cache>
                                -- object, in which case use of that cache is enabled for the duration of
                                -- the command.
                                PipelineCache
                             -> -- | @pCreateInfos@ is a pointer to an array of
                                -- 'RayTracingPipelineCreateInfoKHR' structures.
                                ("createInfos" ::: Vector (SomeStruct RayTracingPipelineCreateInfoKHR))
                             -> -- | @pAllocator@ controls host memory allocation as described in the
                                -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#memory-allocation Memory Allocation>
                                -- chapter.
                                ("allocator" ::: Maybe AllocationCallbacks)
                             -> io (Result, ("pipelines" ::: Vector Pipeline))
createRayTracingPipelinesKHR device deferredOperation pipelineCache createInfos allocator = liftIO . evalContT $ do
  let vkCreateRayTracingPipelinesKHRPtr = pVkCreateRayTracingPipelinesKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkCreateRayTracingPipelinesKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCreateRayTracingPipelinesKHR is null" Nothing Nothing
  let vkCreateRayTracingPipelinesKHR' = mkVkCreateRayTracingPipelinesKHR vkCreateRayTracingPipelinesKHRPtr
  pPCreateInfos <- ContT $ allocaBytes @(RayTracingPipelineCreateInfoKHR _) ((Data.Vector.length (createInfos)) * 104)
  Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPCreateInfos `plusPtr` (104 * (i)) :: Ptr (RayTracingPipelineCreateInfoKHR _))) (e) . ($ ())) (createInfos)
  pAllocator <- case (allocator) of
    Nothing -> pure nullPtr
    Just j -> ContT $ withCStruct (j)
  pPPipelines <- ContT $ bracket (callocBytes @Pipeline ((fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) * 8)) free
  r <- lift $ traceAroundEvent "vkCreateRayTracingPipelinesKHR" (vkCreateRayTracingPipelinesKHR' (deviceHandle (device)) (deferredOperation) (pipelineCache) ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32)) (forgetExtensions (pPCreateInfos)) pAllocator (pPPipelines))
  lift $ when (r < SUCCESS) (throwIO (VulkanException r))
  pPipelines <- lift $ generateM (fromIntegral ((fromIntegral (Data.Vector.length $ (createInfos)) :: Word32))) (\i -> peek @Pipeline ((pPPipelines `advancePtrBytes` (8 * (i)) :: Ptr Pipeline)))
  pure $ (r, pPipelines)

-- | A convenience wrapper to make a compatible pair of calls to
-- 'createRayTracingPipelinesKHR' and 'destroyPipeline'
--
-- To ensure that 'destroyPipeline' is always called: pass
-- 'Control.Exception.bracket' (or the allocate function from your
-- favourite resource management library) as the last argument.
-- To just extract the pair pass '(,)' as the last argument.
--
withRayTracingPipelinesKHR :: forall io r . MonadIO io => Device -> DeferredOperationKHR -> PipelineCache -> Vector (SomeStruct RayTracingPipelineCreateInfoKHR) -> Maybe AllocationCallbacks -> (io (Result, Vector Pipeline) -> ((Result, Vector Pipeline) -> io ()) -> r) -> r
withRayTracingPipelinesKHR device deferredOperation pipelineCache pCreateInfos pAllocator b =
  b (createRayTracingPipelinesKHR device deferredOperation pipelineCache pCreateInfos pAllocator)
    (\(_, o1) -> traverse_ (\o1Elem -> destroyPipeline device o1Elem pAllocator) o1)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysIndirectKHR
  :: FunPtr (Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> DeviceAddress -> IO ()) -> Ptr CommandBuffer_T -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> Ptr StridedDeviceAddressRegionKHR -> DeviceAddress -> IO ()

-- | vkCmdTraceRaysIndirectKHR - Initialize an indirect ray tracing dispatch
--
-- = Description
--
-- 'cmdTraceRaysIndirectKHR' behaves similarly to 'cmdTraceRaysKHR' except
-- that the ray trace query dimensions are read by the device from
-- @indirectDeviceAddress@ during execution.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-magFilter-04553# If a
--     'Vulkan.Core10.Handles.Sampler' created with @magFilter@ or
--     @minFilter@ equal to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR' and
--     @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE' is
--     used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-mipmapMode-04770# If a
--     'Vulkan.Core10.Handles.Sampler' created with @mipmapMode@ equal to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'
--     and @compareEnable@ equal to 'Vulkan.Core10.FundamentalTypes.FALSE'
--     is used to sample a 'Vulkan.Core10.Handles.ImageView' as a result of
--     this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-06479# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-depth-compare-operation depth comparison>,
--     the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02691# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using atomic
--     operations as a result of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Core10.Enums.FormatFeatureFlagBits.FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02692# If a
--     'Vulkan.Core10.Handles.ImageView' is sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command, then the image view’s
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-view-format-features format features>
--     /must/ contain
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-filterCubic-02694# Any
--     'Vulkan.Core10.Handles.ImageView' being sampled with
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FILTER_CUBIC_EXT' as a result
--     of this command /must/ have a
--     'Vulkan.Core10.Enums.ImageViewType.ImageViewType' and format that
--     supports cubic filtering, as specified by
--     'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT'::@filterCubic@
--     returned by
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-filterCubicMinmax-02695# Any
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
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-02696# Any
--     'Vulkan.Core10.Handles.Image' created with a
--     'Vulkan.Core10.Image.ImageCreateInfo'::@flags@ containing
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_CORNER_SAMPLED_BIT_NV'
--     sampled as a result of this command /must/ only be sampled using a
--     'Vulkan.Core10.Enums.SamplerAddressMode.SamplerAddressMode' of
--     'Vulkan.Core10.Enums.SamplerAddressMode.SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-OpTypeImage-06423# Any
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' being written as a storage image
--     or storage texel buffer where the image format field of the
--     @OpTypeImage@ is @Unknown@ /must/ have image format features that
--     support
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-OpTypeImage-06424# Any
--     'Vulkan.Core10.Handles.ImageView' or
--     'Vulkan.Core10.Handles.BufferView' being read as a storage image or
--     storage texel buffer where the image format field of the
--     @OpTypeImage@ is @Unknown@ /must/ have image format features that
--     support
--     'Vulkan.Core13.Enums.FormatFeatureFlags2.FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02697# For each set /n/ that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a descriptor set /must/
--     have been bound to /n/ at the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for set
--     /n/, with the 'Vulkan.Core10.Handles.PipelineLayout' used to create
--     the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-maintenance4-06425# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-maintenance4 maintenance4>
--     feature is not enabled, then for each push constant that is
--     statically used by the 'Vulkan.Core10.Handles.Pipeline' bound to the
--     pipeline bind point used by this command, a push constant value
--     /must/ have been set for the same pipeline bind point, with a
--     'Vulkan.Core10.Handles.PipelineLayout' that is compatible for push
--     constants, with the 'Vulkan.Core10.Handles.PipelineLayout' used to
--     create the current 'Vulkan.Core10.Handles.Pipeline', as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-compatibility ???>
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02699# Descriptors in each
--     bound descriptor set, specified via
--     'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets', /must/
--     be valid if they are statically used by the
--     'Vulkan.Core10.Handles.Pipeline' bound to the pipeline bind point
--     used by this command
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02700# A valid pipeline /must/
--     be bound to the pipeline bind point used by this command
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-commandBuffer-02701# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command requires any dynamic state, that state
--     /must/ have been set or inherited (if the
--     @VK_NV_inherited_viewport_scissor@ extension is enabled) for
--     @commandBuffer@, and done so after any previously bound pipeline
--     with the corresponding state not specified as dynamic
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02859# There /must/ not have
--     been any calls to dynamic state setting commands for any state not
--     specified as dynamic in the 'Vulkan.Core10.Handles.Pipeline' object
--     bound to the pipeline bind point used by this command, since that
--     pipeline was bound
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02702# If the
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
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02703# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions with
--     @ImplicitLod@, @Dref@ or @Proj@ in their name, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02704# If the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command accesses a
--     'Vulkan.Core10.Handles.Sampler' object that uses unnormalized
--     coordinates, that sampler /must/ not be used with any of the SPIR-V
--     @OpImageSample*@ or @OpImageSparseSample*@ instructions that
--     includes a LOD bias or any offset values, in any shader stage
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02705# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a uniform buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-02706# If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-robustBufferAccess robust buffer access>
--     feature is not enabled, and if the 'Vulkan.Core10.Handles.Pipeline'
--     object bound to the pipeline bind point used by this command
--     accesses a storage buffer, it /must/ not access values outside of
--     the range of the buffer as specified in the descriptor set bound to
--     the same pipeline bind point
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-commandBuffer-02707# If
--     @commandBuffer@ is an unprotected command buffer and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-protectedNoFault protectedNoFault>
--     is not supported, any resource accessed by the
--     'Vulkan.Core10.Handles.Pipeline' object bound to the pipeline bind
--     point used by this command /must/ not be a protected resource
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-04115# If a
--     'Vulkan.Core10.Handles.ImageView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     image view’s format
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-OpImageWrite-04469# If a
--     'Vulkan.Core10.Handles.BufferView' is accessed using @OpImageWrite@
--     as a result of this command, then the @Type@ of the @Texel@ operand
--     of that instruction /must/ have at least as many components as the
--     buffer view’s format
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-SampledType-04470# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-SampledType-04471# If a
--     'Vulkan.Core10.Handles.ImageView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-SampledType-04472# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a 64-bit component
--     width is accessed as a result of this command, the @SampledType@ of
--     the @OpTypeImage@ operand of that instruction /must/ have a @Width@
--     of 64
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-SampledType-04473# If a
--     'Vulkan.Core10.Handles.BufferView' with a
--     'Vulkan.Core10.Enums.Format.Format' that has a component width less
--     than 64-bit is accessed as a result of this command, the
--     @SampledType@ of the @OpTypeImage@ operand of that instruction
--     /must/ have a @Width@ of 32
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-sparseImageInt64Atomics-04474# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Image' objects
--     created with the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-sparseImageInt64Atomics-04475# If
--     the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-sparseImageInt64Atomics sparseImageInt64Atomics>
--     feature is not enabled, 'Vulkan.Core10.Handles.Buffer' objects
--     created with the
--     'Vulkan.Core10.Enums.BufferCreateFlagBits.BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
--     flag /must/ not be accessed by atomic instructions through an
--     @OpTypeImage@ with a @SampledType@ with a @Width@ of 64 by this
--     command
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-None-03429# Any shader group handle
--     referenced by this call /must/ have been queried from the currently
--     bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-maxPipelineRayRecursionDepth-03679#
--     This command /must/ not cause a shader call instruction to be
--     executed from a shader invocation with a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#ray-tracing-recursion-depth recursion depth>
--     greater than the value of @maxPipelineRayRecursionDepth@ used to
--     create the bound ray tracing pipeline
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pRayGenShaderBindingTable-03680# If
--     the buffer from which @pRayGenShaderBindingTable->deviceAddress@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pRayGenShaderBindingTable-03681# The
--     buffer from which the @pRayGenShaderBindingTable->deviceAddress@ is
--     queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pRayGenShaderBindingTable-03682#
--     @pRayGenShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-size-04023# The @size@ member of
--     @pRayGenShaderBindingTable@ /must/ be equal to its @stride@ member
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pMissShaderBindingTable-03683# If
--     the buffer from which @pMissShaderBindingTable->deviceAddress@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pMissShaderBindingTable-03684# The
--     buffer from which the @pMissShaderBindingTable->deviceAddress@ is
--     queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pMissShaderBindingTable-03685#
--     @pMissShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-stride-03686# The @stride@ member of
--     @pMissShaderBindingTable@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-stride-04029# The @stride@ member of
--     @pMissShaderBindingTable@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pHitShaderBindingTable-03687# If the
--     buffer from which @pHitShaderBindingTable->deviceAddress@ was
--     queried is non-sparse then it /must/ be bound completely and
--     contiguously to a single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pHitShaderBindingTable-03688# The
--     buffer from which the @pHitShaderBindingTable->deviceAddress@ is
--     queried /must/ have been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pHitShaderBindingTable-03689#
--     @pHitShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-stride-03690# The @stride@ member of
--     @pHitShaderBindingTable@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-stride-04035# The @stride@ member of
--     @pHitShaderBindingTable@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pCallableShaderBindingTable-03691#
--     If the buffer from which
--     @pCallableShaderBindingTable->deviceAddress@ was queried is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pCallableShaderBindingTable-03692#
--     The buffer from which the
--     @pCallableShaderBindingTable->deviceAddress@ is queried /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_SHADER_BINDING_TABLE_BIT_KHR'
--     usage flag
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pCallableShaderBindingTable-03693#
--     @pCallableShaderBindingTable->deviceAddress@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupBaseAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-stride-03694# The @stride@ member of
--     @pCallableShaderBindingTable@ /must/ be a multiple of
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@shaderGroupHandleAlignment@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-stride-04041# The @stride@ member of
--     @pCallableShaderBindingTable@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxShaderGroupStride@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-03696# If the currently bound
--     ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     the @deviceAddress@ member of @pHitShaderBindingTable@ /must/ not be
--     zero
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-03697# If the currently bound
--     ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     the @deviceAddress@ member of @pHitShaderBindingTable@ /must/ not be
--     zero
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-03511# If the currently bound
--     ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR',
--     the shader group handle identified by @pMissShaderBindingTable@
--     /must/ not be set to zero
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-03512# If the currently bound
--     ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     entries in @pHitShaderBindingTable@ accessed as a result of this
--     command in order to execute an any-hit shader /must/ not be set to
--     zero
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-03513# If the currently bound
--     ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     entries in @pHitShaderBindingTable@ accessed as a result of this
--     command in order to execute a closest hit shader /must/ not be set
--     to zero
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-flags-03514# If the currently bound
--     ray tracing pipeline was created with @flags@ that included
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     entries in @pHitShaderBindingTable@ accessed as a result of this
--     command in order to execute an intersection shader /must/ not be set
--     to zero
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pHitShaderBindingTable-04735# Any
--     non-zero hit shader group entries in @pHitShaderBindingTable@
--     accessed by this call from a geometry with a @geometryType@ of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_TRIANGLES_KHR'
--     /must/ have been created with
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pHitShaderBindingTable-04736# Any
--     non-zero hit shader group entries in @pHitShaderBindingTable@
--     accessed by this call from a geometry with a @geometryType@ of
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.GEOMETRY_TYPE_AABBS_KHR'
--     /must/ have been created with
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR'
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-indirectDeviceAddress-03632# If the
--     buffer from which @indirectDeviceAddress@ was queried is non-sparse
--     then it /must/ be bound completely and contiguously to a single
--     'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-indirectDeviceAddress-03633# The
--     buffer from which @indirectDeviceAddress@ was queried /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDIRECT_BUFFER_BIT'
--     bit set
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-indirectDeviceAddress-03634#
--     @indirectDeviceAddress@ /must/ be a multiple of @4@
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-commandBuffer-03635# @commandBuffer@
--     /must/ not be a protected command buffer
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-indirectDeviceAddress-03636# All
--     device addresses between @indirectDeviceAddress@ and
--     @indirectDeviceAddress@ + @sizeof@('TraceRaysIndirectCommandKHR') -
--     1 /must/ be in the buffer device address range of the same buffer
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-rayTracingPipelineTraceRaysIndirect-03637#
--     The
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTracingPipelineTraceRaysIndirect ::rayTracingPipelineTraceRaysIndirect>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-rayTracingMotionBlurPipelineTraceRaysIndirect-04951#
--     If the bound ray tracing pipeline was created with
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--     'Vulkan.Extensions.VK_NV_ray_tracing_motion_blur.PhysicalDeviceRayTracingMotionBlurFeaturesNV'::@rayTracingMotionBlurPipelineTraceRaysIndirect@
--     feature /must/ be enabled
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pRaygenShaderBindingTable-parameter#
--     @pRaygenShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pMissShaderBindingTable-parameter#
--     @pMissShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pHitShaderBindingTable-parameter#
--     @pHitShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-pCallableShaderBindingTable-parameter#
--     @pCallableShaderBindingTable@ /must/ be a valid pointer to a valid
--     'StridedDeviceAddressRegionKHR' structure
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdTraceRaysIndirectKHR-renderpass# This command /must/ only
--     be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'StridedDeviceAddressRegionKHR'
cmdTraceRaysIndirectKHR :: forall io
                         . (MonadIO io)
                        => -- | @commandBuffer@ is the command buffer into which the command will be
                           -- recorded.
                           CommandBuffer
                        -> -- | @pRaygenShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that
                           -- holds the shader binding table data for the ray generation shader stage.
                           ("raygenShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- | @pMissShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that
                           -- holds the shader binding table data for the miss shader stage.
                           ("missShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- | @pHitShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that holds
                           -- the shader binding table data for the hit shader stage.
                           ("hitShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- | @pCallableShaderBindingTable@ is a 'StridedDeviceAddressRegionKHR' that
                           -- holds the shader binding table data for the callable shader stage.
                           ("callableShaderBindingTable" ::: StridedDeviceAddressRegionKHR)
                        -> -- | @indirectDeviceAddress@ is a buffer device address which is a pointer to
                           -- a 'TraceRaysIndirectCommandKHR' structure containing the trace ray
                           -- parameters.
                           ("indirectDeviceAddress" ::: DeviceAddress)
                        -> io ()
cmdTraceRaysIndirectKHR commandBuffer raygenShaderBindingTable missShaderBindingTable hitShaderBindingTable callableShaderBindingTable indirectDeviceAddress = liftIO . evalContT $ do
  let vkCmdTraceRaysIndirectKHRPtr = pVkCmdTraceRaysIndirectKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  lift $ unless (vkCmdTraceRaysIndirectKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdTraceRaysIndirectKHR is null" Nothing Nothing
  let vkCmdTraceRaysIndirectKHR' = mkVkCmdTraceRaysIndirectKHR vkCmdTraceRaysIndirectKHRPtr
  pRaygenShaderBindingTable <- ContT $ withCStruct (raygenShaderBindingTable)
  pMissShaderBindingTable <- ContT $ withCStruct (missShaderBindingTable)
  pHitShaderBindingTable <- ContT $ withCStruct (hitShaderBindingTable)
  pCallableShaderBindingTable <- ContT $ withCStruct (callableShaderBindingTable)
  lift $ traceAroundEvent "vkCmdTraceRaysIndirectKHR" (vkCmdTraceRaysIndirectKHR' (commandBufferHandle (commandBuffer)) pRaygenShaderBindingTable pMissShaderBindingTable pHitShaderBindingTable pCallableShaderBindingTable (indirectDeviceAddress))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupStackSizeKHR
  :: FunPtr (Ptr Device_T -> Pipeline -> Word32 -> ShaderGroupShaderKHR -> IO DeviceSize) -> Ptr Device_T -> Pipeline -> Word32 -> ShaderGroupShaderKHR -> IO DeviceSize

-- | vkGetRayTracingShaderGroupStackSizeKHR - Query ray tracing pipeline
-- shader group shader stack size
--
-- = Description
--
-- The return value is the ray tracing pipeline stack size in bytes for the
-- specified shader as called from the specified shader group.
--
-- == Valid Usage
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-pipeline-04622#
--     @pipeline@ /must/ be a ray tracing pipeline
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-group-03608# The value
--     of @group@ must be less than the number of shader groups in
--     @pipeline@
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-groupShader-03609# The
--     shader identified by @groupShader@ in @group@ /must/ not be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-device-parameter#
--     @device@ /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-pipeline-parameter#
--     @pipeline@ /must/ be a valid 'Vulkan.Core10.Handles.Pipeline' handle
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-groupShader-parameter#
--     @groupShader@ /must/ be a valid 'ShaderGroupShaderKHR' value
--
-- -   #VUID-vkGetRayTracingShaderGroupStackSizeKHR-pipeline-parent#
--     @pipeline@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Pipeline',
-- 'ShaderGroupShaderKHR'
getRayTracingShaderGroupStackSizeKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @device@ is the logical device containing the ray tracing pipeline.
                                        Device
                                     -> -- | @pipeline@ is the ray tracing pipeline object containing the shaders
                                        -- groups.
                                        Pipeline
                                     -> -- | @group@ is the index of the shader group to query.
                                        ("group" ::: Word32)
                                     -> -- | @groupShader@ is the type of shader from the group to query.
                                        ShaderGroupShaderKHR
                                     -> io (DeviceSize)
getRayTracingShaderGroupStackSizeKHR device pipeline group groupShader = liftIO $ do
  let vkGetRayTracingShaderGroupStackSizeKHRPtr = pVkGetRayTracingShaderGroupStackSizeKHR (case device of Device{deviceCmds} -> deviceCmds)
  unless (vkGetRayTracingShaderGroupStackSizeKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRayTracingShaderGroupStackSizeKHR is null" Nothing Nothing
  let vkGetRayTracingShaderGroupStackSizeKHR' = mkVkGetRayTracingShaderGroupStackSizeKHR vkGetRayTracingShaderGroupStackSizeKHRPtr
  r <- traceAroundEvent "vkGetRayTracingShaderGroupStackSizeKHR" (vkGetRayTracingShaderGroupStackSizeKHR' (deviceHandle (device)) (pipeline) (group) (groupShader))
  pure $ (r)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetRayTracingPipelineStackSizeKHR
  :: FunPtr (Ptr CommandBuffer_T -> Word32 -> IO ()) -> Ptr CommandBuffer_T -> Word32 -> IO ()

-- | vkCmdSetRayTracingPipelineStackSizeKHR - Set the stack size dynamically
-- for a ray tracing pipeline
--
-- = Description
--
-- This command sets the stack size for subsequent ray tracing commands
-- when the ray tracing pipeline is created with
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR'
-- set in
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo'::@pDynamicStates@.
-- Otherwise, the stack size is computed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-tracing-pipeline-stack Ray Tracing Pipeline Stack>.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdSetRayTracingPipelineStackSizeKHR-pipelineStackSize-03610#
--     @pipelineStackSize@ /must/ be large enough for any dynamic execution
--     through the shaders in the ray tracing pipeline used by a subsequent
--     trace call
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdSetRayTracingPipelineStackSizeKHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdSetRayTracingPipelineStackSizeKHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdSetRayTracingPipelineStackSizeKHR-commandBuffer-cmdpool#
--     The 'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support compute operations
--
-- -   #VUID-vkCmdSetRayTracingPipelineStackSizeKHR-renderpass# This
--     command /must/ only be called outside of a render pass instance
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> |
-- +============================================================================================================================+========================================================================================================================+=======================================================================================================================+
-- | Primary                                                                                                                    | Outside                                                                                                                | Compute                                                                                                               |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                       |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.CommandBuffer'
cmdSetRayTracingPipelineStackSizeKHR :: forall io
                                      . (MonadIO io)
                                     => -- | @commandBuffer@ is the command buffer into which the command will be
                                        -- recorded.
                                        CommandBuffer
                                     -> -- | @pipelineStackSize@ is the stack size to use for subsequent ray tracing
                                        -- trace commands.
                                        ("pipelineStackSize" ::: Word32)
                                     -> io ()
cmdSetRayTracingPipelineStackSizeKHR commandBuffer pipelineStackSize = liftIO $ do
  let vkCmdSetRayTracingPipelineStackSizeKHRPtr = pVkCmdSetRayTracingPipelineStackSizeKHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdSetRayTracingPipelineStackSizeKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdSetRayTracingPipelineStackSizeKHR is null" Nothing Nothing
  let vkCmdSetRayTracingPipelineStackSizeKHR' = mkVkCmdSetRayTracingPipelineStackSizeKHR vkCmdSetRayTracingPipelineStackSizeKHRPtr
  traceAroundEvent "vkCmdSetRayTracingPipelineStackSizeKHR" (vkCmdSetRayTracingPipelineStackSizeKHR' (commandBufferHandle (commandBuffer)) (pipelineStackSize))
  pure $ ()


-- | VkRayTracingShaderGroupCreateInfoKHR - Structure specifying shaders in a
-- shader group
--
-- == Valid Usage
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-type-03474# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR' then @generalShader@
--     /must/ be a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR',
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_KHR',
--     or
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-type-03475# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR' then @closestHitShader@,
--     @anyHitShader@, and @intersectionShader@ /must/ be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-type-03476# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR' then
--     @intersectionShader@ /must/ be a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_INTERSECTION_BIT_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-type-03477# If @type@ is
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' then
--     @intersectionShader@ /must/ be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-closestHitShader-03478#
--     @closestHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' or a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CLOSEST_HIT_BIT_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-anyHitShader-03479#
--     @anyHitShader@ /must/ be either
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' or a valid index into
--     'RayTracingPipelineCreateInfoKHR'::@pStages@ referring to a shader
--     of
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_ANY_HIT_BIT_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-rayTracingPipelineShaderGroupHandleCaptureReplayMixed-03603#
--     If
--     'PhysicalDeviceRayTracingPipelineFeaturesKHR'::@rayTracingPipelineShaderGroupHandleCaptureReplayMixed@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' then
--     @pShaderGroupCaptureReplayHandle@ /must/ not be provided if it has
--     not been provided on a previous call to ray tracing pipeline
--     creation
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-rayTracingPipelineShaderGroupHandleCaptureReplayMixed-03604#
--     If
--     'PhysicalDeviceRayTracingPipelineFeaturesKHR'::@rayTracingPipelineShaderGroupHandleCaptureReplayMixed@
--     is 'Vulkan.Core10.FundamentalTypes.FALSE' then the caller /must/
--     guarantee that no ray tracing pipeline creation commands with
--     @pShaderGroupCaptureReplayHandle@ provided execute simultaneously
--     with ray tracing pipeline creation commands without
--     @pShaderGroupCaptureReplayHandle@ provided
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-sType-sType# @sType@
--     /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR'
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-pNext-pNext# @pNext@
--     /must/ be @NULL@
--
-- -   #VUID-VkRayTracingShaderGroupCreateInfoKHR-type-parameter# @type@
--     /must/ be a valid 'RayTracingShaderGroupTypeKHR' value
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'RayTracingPipelineCreateInfoKHR', 'RayTracingShaderGroupTypeKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingShaderGroupCreateInfoKHR = RayTracingShaderGroupCreateInfoKHR
  { -- | @type@ is the type of hit group specified in this structure.
    type' :: RayTracingShaderGroupTypeKHR
  , -- | @generalShader@ is the index of the ray generation, miss, or callable
    -- shader from 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if
    -- the shader group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    generalShader :: Word32
  , -- | @closestHitShader@ is the optional index of the closest hit shader from
    -- 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    closestHitShader :: Word32
  , -- | @anyHitShader@ is the optional index of the any-hit shader from
    -- 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    anyHitShader :: Word32
  , -- | @intersectionShader@ is the index of the intersection shader from
    -- 'RayTracingPipelineCreateInfoKHR'::@pStages@ in the group if the shader
    -- group has @type@ of
    -- 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', and
    -- 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR' otherwise.
    intersectionShader :: Word32
  , -- | @pShaderGroupCaptureReplayHandle@ is @NULL@ or a pointer to replay
    -- information for this shader group. Ignored if
    -- 'PhysicalDeviceRayTracingPipelineFeaturesKHR'::@rayTracingPipelineShaderGroupHandleCaptureReplay@
    -- is 'Vulkan.Core10.FundamentalTypes.FALSE'.
    shaderGroupCaptureReplayHandle :: Ptr ()
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingShaderGroupCreateInfoKHR)
#endif
deriving instance Show RayTracingShaderGroupCreateInfoKHR

instance ToCStruct RayTracingShaderGroupCreateInfoKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingShaderGroupCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (type')
    poke ((p `plusPtr` 20 :: Ptr Word32)) (generalShader)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (closestHitShader)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (anyHitShader)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (intersectionShader)
    poke ((p `plusPtr` 40 :: Ptr (Ptr ()))) (shaderGroupCaptureReplayHandle)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingShaderGroupCreateInfoKHR where
  peekCStruct p = do
    type' <- peek @RayTracingShaderGroupTypeKHR ((p `plusPtr` 16 :: Ptr RayTracingShaderGroupTypeKHR))
    generalShader <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    closestHitShader <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    anyHitShader <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    intersectionShader <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pShaderGroupCaptureReplayHandle <- peek @(Ptr ()) ((p `plusPtr` 40 :: Ptr (Ptr ())))
    pure $ RayTracingShaderGroupCreateInfoKHR
             type' generalShader closestHitShader anyHitShader intersectionShader pShaderGroupCaptureReplayHandle

instance Storable RayTracingShaderGroupCreateInfoKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingShaderGroupCreateInfoKHR where
  zero = RayTracingShaderGroupCreateInfoKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRayTracingPipelineCreateInfoKHR - Structure specifying parameters of a
-- newly created ray tracing pipeline
--
-- = Description
--
-- The parameters @basePipelineHandle@ and @basePipelineIndex@ are
-- described in more detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>.
--
-- When
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR'
-- is specified, this pipeline defines a /pipeline library/ which /cannot/
-- be bound as a ray tracing pipeline directly. Instead, pipeline libraries
-- define common shaders and shader groups which /can/ be included in
-- future pipeline creation.
--
-- If pipeline libraries are included in @pLibraryInfo@, shaders defined in
-- those libraries are treated as if they were defined as additional
-- entries in @pStages@, appended in the order they appear in the
-- @pLibraries@ array and in the @pStages@ array when those libraries were
-- defined.
--
-- When referencing shader groups in order to obtain a shader group handle,
-- groups defined in those libraries are treated as if they were defined as
-- additional entries in @pGroups@, appended in the order they appear in
-- the @pLibraries@ array and in the @pGroups@ array when those libraries
-- were defined. The shaders these groups reference are set when the
-- pipeline library is created, referencing those specified in the pipeline
-- library, not in the pipeline that includes it.
--
-- The default stack size for a pipeline if
-- 'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR'
-- is not provided is computed as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-tracing-pipeline-stack Ray Tracing Pipeline Stack>.
--
-- == Valid Usage
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03421# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is @-1@, @basePipelineHandle@ /must/
--     be a valid handle to a ray tracing 'Vulkan.Core10.Handles.Pipeline'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03422# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be a valid index into the calling command’s @pCreateInfos@ parameter
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03423# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineIndex@ is not @-1@, @basePipelineHandle@
--     /must/ be 'Vulkan.Core10.APIConstants.NULL_HANDLE'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03424# If @flags@
--     contains the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_DERIVATIVE_BIT'
--     flag, and @basePipelineHandle@ is not
--     'Vulkan.Core10.APIConstants.NULL_HANDLE', @basePipelineIndex@ /must/
--     be @-1@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pStages-03426# The shader
--     code for the entry points identified by @pStages@, and the rest of
--     the state identified by this structure /must/ adhere to the pipeline
--     linking rules described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#interfaces Shader Interfaces>
--     chapter
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-layout-03427# @layout@
--     /must/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-pipelinelayout-consistency consistent>
--     with all shaders specified in @pStages@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-layout-03428# The number of
--     resources in @layout@ accessible to each shader stage that is used
--     by the pipeline /must/ be less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxPerStageResources@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-02904# @flags@ /must/
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_INDIRECT_BINDABLE_BIT_NV'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pipelineCreationCacheControl-02905#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-pipelineCreationCacheControl pipelineCreationCacheControl>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT'
--     or
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_EARLY_RETURN_ON_FAILURE_BIT'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-stage-03425# If @flags@ does
--     not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     the @stage@ member of at least one element of @pStages@, including
--     those implicitly added by @pLibraryInfo@, /must/ be
--     'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-maxPipelineRayRecursionDepth-03589#
--     @maxPipelineRayRecursionDepth@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxRayRecursionDepth@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03465# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_LIBRARY_BIT_KHR',
--     @pLibraryInterface@ /must/ not be @NULL@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-03590# If
--     @pLibraryInfo@ is not @NULL@ and its @libraryCount@ member is
--     greater than @0@, its @pLibraryInterface@ member /must/ not be
--     @NULL@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraries-03591# Each
--     element of @pLibraryInfo->pLibraries@ /must/ have been created with
--     the value of @maxPipelineRayRecursionDepth@ equal to that in this
--     pipeline
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-03592# If
--     @pLibraryInfo@ is not @NULL@, each element of its @pLibraries@
--     member /must/ have been created with a @layout@ that is compatible
--     with the @layout@ in this pipeline
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-03593# If
--     @pLibraryInfo@ is not @NULL@, each element of its @pLibraries@
--     member /must/ have been created with values of the
--     @maxPipelineRayPayloadSize@ and @maxPipelineRayHitAttributeSize@
--     members of @pLibraryInterface@ equal to those in this pipeline
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03594# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-04718# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-04719# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-04720# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-04721# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-04722# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-04723# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR',
--     each element of @pLibraryInfo->pLibraries@ /must/ have been created
--     with the
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--     bit set
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-03595# If the
--     @VK_KHR_pipeline_library@ extension is not enabled, @pLibraryInfo@
--     and @pLibraryInterface@ /must/ be @NULL@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03470# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR',
--     for any element of @pGroups@ with a @type@ of
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', the
--     @anyHitShader@ of that element /must/ not be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03471# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR',
--     for any element of @pGroups@ with a @type@ of
--     'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' or
--     'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR', the
--     @closestHitShader@ of that element /must/ not be
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-rayTraversalPrimitiveCulling-03596#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTraversalPrimitiveCulling rayTraversalPrimitiveCulling>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-rayTraversalPrimitiveCulling-03597#
--     If the
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTraversalPrimitiveCulling rayTraversalPrimitiveCulling>
--     feature is not enabled, @flags@ /must/ not include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-03598# If @flags@
--     includes
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR',
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#features-rayTracingPipelineShaderGroupHandleCaptureReplay rayTracingPipelineShaderGroupHandleCaptureReplay>
--     /must/ be enabled
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-rayTracingPipelineShaderGroupHandleCaptureReplay-03599#
--     If
--     'PhysicalDeviceRayTracingPipelineFeaturesKHR'::@rayTracingPipelineShaderGroupHandleCaptureReplay@
--     is 'Vulkan.Core10.FundamentalTypes.TRUE' and the
--     @pShaderGroupCaptureReplayHandle@ member of any element of @pGroups@
--     is not @NULL@, @flags@ /must/ include
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PIPELINE_CREATE_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-03600# If
--     @pLibraryInfo@ is not @NULL@ and its @libraryCount@ is @0@,
--     @stageCount@ /must/ not be @0@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-03601# If
--     @pLibraryInfo@ is not @NULL@ and its @libraryCount@ is @0@,
--     @groupCount@ /must/ not be @0@
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pDynamicStates-03602# Any
--     element of the @pDynamicStates@ member of @pDynamicState@ /must/ be
--     'Vulkan.Core10.Enums.DynamicState.DYNAMIC_STATE_RAY_TRACING_PIPELINE_STACK_SIZE_KHR'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-sType-sType# @sType@ /must/
--     be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pNext-pNext# @pNext@ /must/
--     be @NULL@ or a pointer to a valid instance of
--     'Vulkan.Core13.Promoted_From_VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfo'
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-sType-unique# The @sType@
--     value of each struct in the @pNext@ chain /must/ be unique
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-flags-parameter# @flags@
--     /must/ be a valid combination of
--     'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
--     values
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pStages-parameter# If
--     @stageCount@ is not @0@, @pStages@ /must/ be a valid pointer to an
--     array of @stageCount@ valid
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pGroups-parameter# If
--     @groupCount@ is not @0@, @pGroups@ /must/ be a valid pointer to an
--     array of @groupCount@ valid 'RayTracingShaderGroupCreateInfoKHR'
--     structures
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInfo-parameter# If
--     @pLibraryInfo@ is not @NULL@, @pLibraryInfo@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pLibraryInterface-parameter#
--     If @pLibraryInterface@ is not @NULL@, @pLibraryInterface@ /must/ be
--     a valid pointer to a valid
--     'RayTracingPipelineInterfaceCreateInfoKHR' structure
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-pDynamicState-parameter# If
--     @pDynamicState@ is not @NULL@, @pDynamicState@ /must/ be a valid
--     pointer to a valid
--     'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo' structure
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-layout-parameter# @layout@
--     /must/ be a valid 'Vulkan.Core10.Handles.PipelineLayout' handle
--
-- -   #VUID-VkRayTracingPipelineCreateInfoKHR-commonparent# Both of
--     @basePipelineHandle@, and @layout@ that are valid handles of
--     non-ignored parameters /must/ have been created, allocated, or
--     retrieved from the same 'Vulkan.Core10.Handles.Device'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Handles.Pipeline',
-- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlags',
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo',
-- 'Vulkan.Core10.Handles.PipelineLayout',
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'RayTracingPipelineInterfaceCreateInfoKHR',
-- 'RayTracingShaderGroupCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'createRayTracingPipelinesKHR'
data RayTracingPipelineCreateInfoKHR (es :: [Type]) = RayTracingPipelineCreateInfoKHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @flags@ is a bitmask of
    -- 'Vulkan.Core10.Enums.PipelineCreateFlagBits.PipelineCreateFlagBits'
    -- specifying how the pipeline will be generated.
    flags :: PipelineCreateFlags
  , -- | @pStages@ is a pointer to an array of @stageCount@
    -- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo' structures
    -- describing the set of the shader stages to be included in the ray
    -- tracing pipeline.
    stages :: Vector (SomeStruct PipelineShaderStageCreateInfo)
  , -- | @pGroups@ is a pointer to an array of @groupCount@
    -- 'RayTracingShaderGroupCreateInfoKHR' structures describing the set of
    -- the shader stages to be included in each shader group in the ray tracing
    -- pipeline.
    groups :: Vector RayTracingShaderGroupCreateInfoKHR
  , -- | @maxPipelineRayRecursionDepth@ is the
    -- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-tracing-recursion-depth maximum recursion depth>
    -- of shaders executed by this pipeline.
    maxPipelineRayRecursionDepth :: Word32
  , -- | @pLibraryInfo@ is a pointer to a
    -- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
    -- structure defining pipeline libraries to include.
    libraryInfo :: Maybe PipelineLibraryCreateInfoKHR
  , -- | @pLibraryInterface@ is a pointer to a
    -- 'RayTracingPipelineInterfaceCreateInfoKHR' structure defining additional
    -- information when using pipeline libraries.
    libraryInterface :: Maybe RayTracingPipelineInterfaceCreateInfoKHR
  , -- | @pDynamicState@ is a pointer to a
    -- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo' structure, and
    -- is used to indicate which properties of the pipeline state object are
    -- dynamic and /can/ be changed independently of the pipeline state. This
    -- /can/ be @NULL@, which means no state in the pipeline is considered
    -- dynamic.
    dynamicState :: Maybe PipelineDynamicStateCreateInfo
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
deriving instance Generic (RayTracingPipelineCreateInfoKHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (RayTracingPipelineCreateInfoKHR es)

instance Extensible RayTracingPipelineCreateInfoKHR where
  extensibleTypeName = "RayTracingPipelineCreateInfoKHR"
  setNext RayTracingPipelineCreateInfoKHR{..} next' = RayTracingPipelineCreateInfoKHR{next = next', ..}
  getNext RayTracingPipelineCreateInfoKHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends RayTracingPipelineCreateInfoKHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @PipelineCreationFeedbackCreateInfo = Just f
    | otherwise = Nothing

instance (Extendss RayTracingPipelineCreateInfoKHR es, PokeChain es) => ToCStruct (RayTracingPipelineCreateInfoKHR es) where
  withCStruct x f = allocaBytes 104 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineCreateInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags)) (flags)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (stages)) :: Word32))
    pPStages' <- ContT $ allocaBytes @(PipelineShaderStageCreateInfo _) ((Data.Vector.length (stages)) * 48)
    Data.Vector.imapM_ (\i e -> ContT $ pokeSomeCStruct (forgetExtensions (pPStages' `plusPtr` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _))) (e) . ($ ())) (stages)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _)))) (pPStages')
    lift $ poke ((p `plusPtr` 32 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (groups)) :: Word32))
    pPGroups' <- ContT $ allocaBytes @RayTracingShaderGroupCreateInfoKHR ((Data.Vector.length (groups)) * 48)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPGroups' `plusPtr` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR) (e)) (groups)
    lift $ poke ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR))) (pPGroups')
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (maxPipelineRayRecursionDepth)
    pLibraryInfo'' <- case (libraryInfo) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 56 :: Ptr (Ptr PipelineLibraryCreateInfoKHR))) pLibraryInfo''
    pLibraryInterface'' <- case (libraryInterface) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 64 :: Ptr (Ptr RayTracingPipelineInterfaceCreateInfoKHR))) pLibraryInterface''
    pDynamicState'' <- case (dynamicState) of
      Nothing -> pure nullPtr
      Just j -> ContT $ withCStruct (j)
    lift $ poke ((p `plusPtr` 72 :: Ptr (Ptr PipelineDynamicStateCreateInfo))) pDynamicState''
    lift $ poke ((p `plusPtr` 80 :: Ptr PipelineLayout)) (layout)
    lift $ poke ((p `plusPtr` 88 :: Ptr Pipeline)) (basePipelineHandle)
    lift $ poke ((p `plusPtr` 96 :: Ptr Int32)) (basePipelineIndex)
    lift $ f
  cStructSize = 104
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 48 :: Ptr Word32)) (zero)
    lift $ poke ((p `plusPtr` 80 :: Ptr PipelineLayout)) (zero)
    lift $ poke ((p `plusPtr` 96 :: Ptr Int32)) (zero)
    lift $ f

instance (Extendss RayTracingPipelineCreateInfoKHR es, PeekChain es) => FromCStruct (RayTracingPipelineCreateInfoKHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    flags <- peek @PipelineCreateFlags ((p `plusPtr` 16 :: Ptr PipelineCreateFlags))
    stageCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pStages <- peek @(Ptr (PipelineShaderStageCreateInfo _)) ((p `plusPtr` 24 :: Ptr (Ptr (PipelineShaderStageCreateInfo _))))
    pStages' <- generateM (fromIntegral stageCount) (\i -> peekSomeCStruct (forgetExtensions ((pStages `advancePtrBytes` (48 * (i)) :: Ptr (PipelineShaderStageCreateInfo _)))))
    groupCount <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    pGroups <- peek @(Ptr RayTracingShaderGroupCreateInfoKHR) ((p `plusPtr` 40 :: Ptr (Ptr RayTracingShaderGroupCreateInfoKHR)))
    pGroups' <- generateM (fromIntegral groupCount) (\i -> peekCStruct @RayTracingShaderGroupCreateInfoKHR ((pGroups `advancePtrBytes` (48 * (i)) :: Ptr RayTracingShaderGroupCreateInfoKHR)))
    maxPipelineRayRecursionDepth <- peek @Word32 ((p `plusPtr` 48 :: Ptr Word32))
    pLibraryInfo <- peek @(Ptr PipelineLibraryCreateInfoKHR) ((p `plusPtr` 56 :: Ptr (Ptr PipelineLibraryCreateInfoKHR)))
    pLibraryInfo' <- maybePeek (\j -> peekCStruct @PipelineLibraryCreateInfoKHR (j)) pLibraryInfo
    pLibraryInterface <- peek @(Ptr RayTracingPipelineInterfaceCreateInfoKHR) ((p `plusPtr` 64 :: Ptr (Ptr RayTracingPipelineInterfaceCreateInfoKHR)))
    pLibraryInterface' <- maybePeek (\j -> peekCStruct @RayTracingPipelineInterfaceCreateInfoKHR (j)) pLibraryInterface
    pDynamicState <- peek @(Ptr PipelineDynamicStateCreateInfo) ((p `plusPtr` 72 :: Ptr (Ptr PipelineDynamicStateCreateInfo)))
    pDynamicState' <- maybePeek (\j -> peekCStruct @PipelineDynamicStateCreateInfo (j)) pDynamicState
    layout <- peek @PipelineLayout ((p `plusPtr` 80 :: Ptr PipelineLayout))
    basePipelineHandle <- peek @Pipeline ((p `plusPtr` 88 :: Ptr Pipeline))
    basePipelineIndex <- peek @Int32 ((p `plusPtr` 96 :: Ptr Int32))
    pure $ RayTracingPipelineCreateInfoKHR
             next flags pStages' pGroups' maxPipelineRayRecursionDepth pLibraryInfo' pLibraryInterface' pDynamicState' layout basePipelineHandle basePipelineIndex

instance es ~ '[] => Zero (RayTracingPipelineCreateInfoKHR es) where
  zero = RayTracingPipelineCreateInfoKHR
           ()
           zero
           mempty
           mempty
           zero
           Nothing
           Nothing
           Nothing
           zero
           zero
           zero


-- | VkPhysicalDeviceRayTracingPipelineFeaturesKHR - Structure describing the
-- ray tracing features that can be supported by an implementation
--
-- = Members
--
-- This structure describes the following features:
--
-- = Description
--
-- -   @sType@ is the type of this structure.
--
-- -   @pNext@ is @NULL@ or a pointer to a structure extending this
--     structure.
--
-- -   #features-rayTracingPipeline# @rayTracingPipeline@ indicates whether
--     the implementation supports the ray tracing pipeline functionality.
--     See
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-tracing Ray Tracing>.
--
-- -   #features-rayTracingPipelineShaderGroupHandleCaptureReplay#
--     @rayTracingPipelineShaderGroupHandleCaptureReplay@ indicates whether
--     the implementation supports saving and reusing shader group handles,
--     e.g. for trace capture and replay.
--
-- -   #features-rayTracingPipelineShaderGroupHandleCaptureReplayMixed#
--     @rayTracingPipelineShaderGroupHandleCaptureReplayMixed@ indicates
--     whether the implementation supports reuse of shader group handles
--     being arbitrarily mixed with creation of non-reused shader group
--     handles. If this is 'Vulkan.Core10.FundamentalTypes.FALSE', all
--     reused shader group handles /must/ be specified before any
--     non-reused handles /may/ be created.
--
-- -   #features-rayTracingPipelineTraceRaysIndirect#
--     @rayTracingPipelineTraceRaysIndirect@ indicates whether the
--     implementation supports indirect ray tracing commands, e.g.
--     'cmdTraceRaysIndirectKHR'.
--
-- -   #features-rayTraversalPrimitiveCulling#
--     @rayTraversalPrimitiveCulling@ indicates whether the implementation
--     supports
--     <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#ray-traversal-culling-primitive primitive culling during ray traversal>.
--
-- If the 'PhysicalDeviceRayTracingPipelineFeaturesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceRayTracingPipelineFeaturesKHR' /can/ also be
-- used in the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage
--
-- -   #VUID-VkPhysicalDeviceRayTracingPipelineFeaturesKHR-rayTracingPipelineShaderGroupHandleCaptureReplayMixed-03575#
--     If @rayTracingPipelineShaderGroupHandleCaptureReplayMixed@ is
--     'Vulkan.Core10.FundamentalTypes.TRUE',
--     @rayTracingPipelineShaderGroupHandleCaptureReplay@ /must/ also be
--     'Vulkan.Core10.FundamentalTypes.TRUE'
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkPhysicalDeviceRayTracingPipelineFeaturesKHR-sType-sType#
--     @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR'
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPipelineFeaturesKHR = PhysicalDeviceRayTracingPipelineFeaturesKHR
  { -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipeline"
    rayTracingPipeline :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipelineShaderGroupHandleCaptureReplay"
    rayTracingPipelineShaderGroupHandleCaptureReplay :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipelineShaderGroupHandleCaptureReplayMixed"
    rayTracingPipelineShaderGroupHandleCaptureReplayMixed :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTracingPipelineTraceRaysIndirect"
    rayTracingPipelineTraceRaysIndirect :: Bool
  , -- No documentation found for Nested "VkPhysicalDeviceRayTracingPipelineFeaturesKHR" "rayTraversalPrimitiveCulling"
    rayTraversalPrimitiveCulling :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPipelineFeaturesKHR)
#endif
deriving instance Show PhysicalDeviceRayTracingPipelineFeaturesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPipelineFeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (rayTracingPipeline))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineShaderGroupHandleCaptureReplay))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineShaderGroupHandleCaptureReplayMixed))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (rayTracingPipelineTraceRaysIndirect))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (rayTraversalPrimitiveCulling))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceRayTracingPipelineFeaturesKHR where
  peekCStruct p = do
    rayTracingPipeline <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    rayTracingPipelineShaderGroupHandleCaptureReplay <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    rayTracingPipelineShaderGroupHandleCaptureReplayMixed <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    rayTracingPipelineTraceRaysIndirect <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    rayTraversalPrimitiveCulling <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    pure $ PhysicalDeviceRayTracingPipelineFeaturesKHR
             (bool32ToBool rayTracingPipeline) (bool32ToBool rayTracingPipelineShaderGroupHandleCaptureReplay) (bool32ToBool rayTracingPipelineShaderGroupHandleCaptureReplayMixed) (bool32ToBool rayTracingPipelineTraceRaysIndirect) (bool32ToBool rayTraversalPrimitiveCulling)

instance Storable PhysicalDeviceRayTracingPipelineFeaturesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPipelineFeaturesKHR where
  zero = PhysicalDeviceRayTracingPipelineFeaturesKHR
           zero
           zero
           zero
           zero
           zero


-- | VkPhysicalDeviceRayTracingPipelinePropertiesKHR - Properties of the
-- physical device for ray tracing
--
-- = Description
--
-- If the 'PhysicalDeviceRayTracingPipelinePropertiesKHR' structure is
-- included in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- Limits specified by this structure /must/ match those specified with the
-- same name in
-- 'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV'.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceRayTracingPipelinePropertiesKHR = PhysicalDeviceRayTracingPipelinePropertiesKHR
  { -- | @shaderGroupHandleSize@ is the size in bytes of the shader header.
    shaderGroupHandleSize :: Word32
  , -- | #limits-maxRayRecursionDepth# @maxRayRecursionDepth@ is the maximum
    -- number of levels of ray recursion allowed in a trace command.
    maxRayRecursionDepth :: Word32
  , -- | @maxShaderGroupStride@ is the maximum stride in bytes allowed between
    -- shader groups in the shader binding table.
    maxShaderGroupStride :: Word32
  , -- | @shaderGroupBaseAlignment@ is the /required/ alignment in bytes for the
    -- base of the shader binding table.
    shaderGroupBaseAlignment :: Word32
  , -- | @shaderGroupHandleCaptureReplaySize@ is the number of bytes for the
    -- information required to do capture and replay for shader group handles.
    shaderGroupHandleCaptureReplaySize :: Word32
  , -- | @maxRayDispatchInvocationCount@ is the maximum number of ray generation
    -- shader invocations which /may/ be produced by a single
    -- 'cmdTraceRaysIndirectKHR' or 'cmdTraceRaysKHR' command.
    maxRayDispatchInvocationCount :: Word32
  , -- | @shaderGroupHandleAlignment@ is the /required/ alignment in bytes for
    -- each shader binding table entry. The value /must/ be a power of two.
    shaderGroupHandleAlignment :: Word32
  , -- | @maxRayHitAttributeSize@ is the maximum size in bytes for a ray
    -- attribute structure
    maxRayHitAttributeSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceRayTracingPipelinePropertiesKHR)
#endif
deriving instance Show PhysicalDeviceRayTracingPipelinePropertiesKHR

instance ToCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR where
  withCStruct x f = allocaBytes 48 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceRayTracingPipelinePropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (shaderGroupHandleSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxRayRecursionDepth)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (maxShaderGroupStride)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (shaderGroupBaseAlignment)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (shaderGroupHandleCaptureReplaySize)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (maxRayDispatchInvocationCount)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (shaderGroupHandleAlignment)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (maxRayHitAttributeSize)
    f
  cStructSize = 48
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PIPELINE_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 24 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 28 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 40 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 44 :: Ptr Word32)) (zero)
    f

instance FromCStruct PhysicalDeviceRayTracingPipelinePropertiesKHR where
  peekCStruct p = do
    shaderGroupHandleSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxRayRecursionDepth <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    maxShaderGroupStride <- peek @Word32 ((p `plusPtr` 24 :: Ptr Word32))
    shaderGroupBaseAlignment <- peek @Word32 ((p `plusPtr` 28 :: Ptr Word32))
    shaderGroupHandleCaptureReplaySize <- peek @Word32 ((p `plusPtr` 32 :: Ptr Word32))
    maxRayDispatchInvocationCount <- peek @Word32 ((p `plusPtr` 36 :: Ptr Word32))
    shaderGroupHandleAlignment <- peek @Word32 ((p `plusPtr` 40 :: Ptr Word32))
    maxRayHitAttributeSize <- peek @Word32 ((p `plusPtr` 44 :: Ptr Word32))
    pure $ PhysicalDeviceRayTracingPipelinePropertiesKHR
             shaderGroupHandleSize maxRayRecursionDepth maxShaderGroupStride shaderGroupBaseAlignment shaderGroupHandleCaptureReplaySize maxRayDispatchInvocationCount shaderGroupHandleAlignment maxRayHitAttributeSize

instance Storable PhysicalDeviceRayTracingPipelinePropertiesKHR where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceRayTracingPipelinePropertiesKHR where
  zero = PhysicalDeviceRayTracingPipelinePropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkStridedDeviceAddressRegionKHR - Structure specifying a region of
-- device addresses with a stride
--
-- == Valid Usage
--
-- -   #VUID-VkStridedDeviceAddressRegionKHR-size-04631# If @size@ is not
--     zero, all addresses between @deviceAddress@ and @deviceAddress@ +
--     @size@ - 1 /must/ be in the buffer device address range of the same
--     buffer
--
-- -   #VUID-VkStridedDeviceAddressRegionKHR-size-04632# If @size@ is not
--     zero, @stride@ /must/ be less than or equal to the size of the
--     buffer from which @deviceAddress@ was queried
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'Vulkan.Core10.FundamentalTypes.DeviceAddress',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize', 'cmdTraceRaysIndirectKHR',
-- 'cmdTraceRaysKHR'
data StridedDeviceAddressRegionKHR = StridedDeviceAddressRegionKHR
  { -- | @deviceAddress@ is the device address (as returned by the
    -- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress'
    -- command) at which the region starts, or zero if the region is unused.
    deviceAddress :: DeviceAddress
  , -- | @stride@ is the byte stride between consecutive elements.
    stride :: DeviceSize
  , -- | @size@ is the size in bytes of the region starting at @deviceAddress@.
    size :: DeviceSize
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (StridedDeviceAddressRegionKHR)
#endif
deriving instance Show StridedDeviceAddressRegionKHR

instance ToCStruct StridedDeviceAddressRegionKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p StridedDeviceAddressRegionKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr DeviceAddress)) (deviceAddress)
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (stride)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (size)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 8 :: Ptr DeviceSize)) (zero)
    poke ((p `plusPtr` 16 :: Ptr DeviceSize)) (zero)
    f

instance FromCStruct StridedDeviceAddressRegionKHR where
  peekCStruct p = do
    deviceAddress <- peek @DeviceAddress ((p `plusPtr` 0 :: Ptr DeviceAddress))
    stride <- peek @DeviceSize ((p `plusPtr` 8 :: Ptr DeviceSize))
    size <- peek @DeviceSize ((p `plusPtr` 16 :: Ptr DeviceSize))
    pure $ StridedDeviceAddressRegionKHR
             deviceAddress stride size

instance Storable StridedDeviceAddressRegionKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero StridedDeviceAddressRegionKHR where
  zero = StridedDeviceAddressRegionKHR
           zero
           zero
           zero


-- | VkTraceRaysIndirectCommandKHR - Structure specifying the parameters of
-- an indirect ray tracing command
--
-- = Description
--
-- The members of 'TraceRaysIndirectCommandKHR' have the same meaning as
-- the similarly named parameters of 'cmdTraceRaysKHR'.
--
-- == Valid Usage
--
-- -   #VUID-VkTraceRaysIndirectCommandKHR-width-03638# @width@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[0]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[0]
--
-- -   #VUID-VkTraceRaysIndirectCommandKHR-height-03639# @height@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[1]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[1]
--
-- -   #VUID-VkTraceRaysIndirectCommandKHR-depth-03640# @depth@ /must/ be
--     less than or equal to
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupCount@[2]
--     ×
--     'Vulkan.Core10.DeviceInitialization.PhysicalDeviceLimits'::@maxComputeWorkGroupSize@[2]
--
-- -   #VUID-VkTraceRaysIndirectCommandKHR-width-03641# @width@ × @height@
--     × @depth@ /must/ be less than or equal to
--     'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxRayDispatchInvocationCount@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
data TraceRaysIndirectCommandKHR = TraceRaysIndirectCommandKHR
  { -- | @width@ is the width of the ray trace query dimensions.
    width :: Word32
  , -- | @height@ is height of the ray trace query dimensions.
    height :: Word32
  , -- | @depth@ is depth of the ray trace query dimensions.
    depth :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (TraceRaysIndirectCommandKHR)
#endif
deriving instance Show TraceRaysIndirectCommandKHR

instance ToCStruct TraceRaysIndirectCommandKHR where
  withCStruct x f = allocaBytes 12 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p TraceRaysIndirectCommandKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (width)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (height)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (depth)
    f
  cStructSize = 12
  cStructAlignment = 4
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 4 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 8 :: Ptr Word32)) (zero)
    f

instance FromCStruct TraceRaysIndirectCommandKHR where
  peekCStruct p = do
    width <- peek @Word32 ((p `plusPtr` 0 :: Ptr Word32))
    height <- peek @Word32 ((p `plusPtr` 4 :: Ptr Word32))
    depth <- peek @Word32 ((p `plusPtr` 8 :: Ptr Word32))
    pure $ TraceRaysIndirectCommandKHR
             width height depth

instance Storable TraceRaysIndirectCommandKHR where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero TraceRaysIndirectCommandKHR where
  zero = TraceRaysIndirectCommandKHR
           zero
           zero
           zero


-- | VkRayTracingPipelineInterfaceCreateInfoKHR - Structure specifying
-- additional interface information when using libraries
--
-- = Description
--
-- @maxPipelineRayPayloadSize@ is calculated as the maximum number of bytes
-- used by any block declared in the @RayPayloadKHR@ or
-- @IncomingRayPayloadKHR@ storage classes.
-- @maxPipelineRayHitAttributeSize@ is calculated as the maximum number of
-- bytes used by any block declared in the @HitAttributeKHR@ storage class.
-- As variables in these storage classes do not have explicit offsets, the
-- size should be calculated as if each variable has a
-- <https://www.khronos.org/registry/vulkan/specs/1.3-extensions/html/vkspec.html#interfaces-alignment-requirements scalar alignment>
-- equal to the largest scalar alignment of any of the block’s members.
--
-- Note
--
-- There is no explicit upper limit for @maxPipelineRayPayloadSize@, but in
-- practice it should be kept as small as possible. Similar to invocation
-- local memory, it must be allocated for each shader invocation and for
-- devices which support many simultaneous invocations, this storage can
-- rapidly be exhausted, resulting in failure.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data RayTracingPipelineInterfaceCreateInfoKHR = RayTracingPipelineInterfaceCreateInfoKHR
  { -- | @maxPipelineRayPayloadSize@ is the maximum payload size in bytes used by
    -- any shader in the pipeline.
    maxPipelineRayPayloadSize :: Word32
  , -- | @maxPipelineRayHitAttributeSize@ is the maximum attribute structure size
    -- in bytes used by any shader in the pipeline.
    --
    -- #VUID-VkRayTracingPipelineInterfaceCreateInfoKHR-maxPipelineRayHitAttributeSize-03605#
    -- @maxPipelineRayHitAttributeSize@ /must/ be less than or equal to
    -- 'PhysicalDeviceRayTracingPipelinePropertiesKHR'::@maxRayHitAttributeSize@
    maxPipelineRayHitAttributeSize :: Word32
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RayTracingPipelineInterfaceCreateInfoKHR)
#endif
deriving instance Show RayTracingPipelineInterfaceCreateInfoKHR

instance ToCStruct RayTracingPipelineInterfaceCreateInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RayTracingPipelineInterfaceCreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (maxPipelineRayPayloadSize)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (maxPipelineRayHitAttributeSize)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 20 :: Ptr Word32)) (zero)
    f

instance FromCStruct RayTracingPipelineInterfaceCreateInfoKHR where
  peekCStruct p = do
    maxPipelineRayPayloadSize <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    maxPipelineRayHitAttributeSize <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pure $ RayTracingPipelineInterfaceCreateInfoKHR
             maxPipelineRayPayloadSize maxPipelineRayHitAttributeSize

instance Storable RayTracingPipelineInterfaceCreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero RayTracingPipelineInterfaceCreateInfoKHR where
  zero = RayTracingPipelineInterfaceCreateInfoKHR
           zero
           zero


-- | VkRayTracingShaderGroupTypeKHR - Shader group types
--
-- = Description
--
-- Note
--
-- For current group types, the hit group type could be inferred from the
-- presence or absence of the intersection shader, but we provide the type
-- explicitly for future hit groups that do not have that property.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>,
-- 'RayTracingShaderGroupCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingShaderGroupCreateInfoNV'
newtype RayTracingShaderGroupTypeKHR = RayTracingShaderGroupTypeKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR' indicates a shader group
-- with a single
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_RAYGEN_BIT_KHR',
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_MISS_BIT_KHR', or
-- 'Vulkan.Core10.Enums.ShaderStageFlagBits.SHADER_STAGE_CALLABLE_BIT_KHR'
-- shader in it.
pattern RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR              = RayTracingShaderGroupTypeKHR 0
-- | 'RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR' specifies a
-- shader group that only hits triangles and /must/ not contain an
-- intersection shader, only closest hit and any-hit shaders.
pattern RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR  = RayTracingShaderGroupTypeKHR 1
-- | 'RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR' specifies a
-- shader group that only intersects with custom geometry and /must/
-- contain an intersection shader and /may/ contain closest hit and any-hit
-- shaders.
pattern RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR = RayTracingShaderGroupTypeKHR 2
{-# complete RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR,
             RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR,
             RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR :: RayTracingShaderGroupTypeKHR #-}

conNameRayTracingShaderGroupTypeKHR :: String
conNameRayTracingShaderGroupTypeKHR = "RayTracingShaderGroupTypeKHR"

enumPrefixRayTracingShaderGroupTypeKHR :: String
enumPrefixRayTracingShaderGroupTypeKHR = "RAY_TRACING_SHADER_GROUP_TYPE_"

showTableRayTracingShaderGroupTypeKHR :: [(RayTracingShaderGroupTypeKHR, String)]
showTableRayTracingShaderGroupTypeKHR =
  [ (RAY_TRACING_SHADER_GROUP_TYPE_GENERAL_KHR             , "GENERAL_KHR")
  , (RAY_TRACING_SHADER_GROUP_TYPE_TRIANGLES_HIT_GROUP_KHR , "TRIANGLES_HIT_GROUP_KHR")
  , (RAY_TRACING_SHADER_GROUP_TYPE_PROCEDURAL_HIT_GROUP_KHR, "PROCEDURAL_HIT_GROUP_KHR")
  ]

instance Show RayTracingShaderGroupTypeKHR where
  showsPrec = enumShowsPrec enumPrefixRayTracingShaderGroupTypeKHR
                            showTableRayTracingShaderGroupTypeKHR
                            conNameRayTracingShaderGroupTypeKHR
                            (\(RayTracingShaderGroupTypeKHR x) -> x)
                            (showsPrec 11)

instance Read RayTracingShaderGroupTypeKHR where
  readPrec = enumReadPrec enumPrefixRayTracingShaderGroupTypeKHR
                          showTableRayTracingShaderGroupTypeKHR
                          conNameRayTracingShaderGroupTypeKHR
                          RayTracingShaderGroupTypeKHR


-- | VkShaderGroupShaderKHR - Shader group shaders
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>,
-- 'getRayTracingShaderGroupStackSizeKHR'
newtype ShaderGroupShaderKHR = ShaderGroupShaderKHR Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SHADER_GROUP_SHADER_GENERAL_KHR' uses the shader specified in the group
-- with 'RayTracingShaderGroupCreateInfoKHR'::@generalShader@
pattern SHADER_GROUP_SHADER_GENERAL_KHR      = ShaderGroupShaderKHR 0
-- | 'SHADER_GROUP_SHADER_CLOSEST_HIT_KHR' uses the shader specified in the
-- group with 'RayTracingShaderGroupCreateInfoKHR'::@closestHitShader@
pattern SHADER_GROUP_SHADER_CLOSEST_HIT_KHR  = ShaderGroupShaderKHR 1
-- | 'SHADER_GROUP_SHADER_ANY_HIT_KHR' uses the shader specified in the group
-- with 'RayTracingShaderGroupCreateInfoKHR'::@anyHitShader@
pattern SHADER_GROUP_SHADER_ANY_HIT_KHR      = ShaderGroupShaderKHR 2
-- | 'SHADER_GROUP_SHADER_INTERSECTION_KHR' uses the shader specified in the
-- group with 'RayTracingShaderGroupCreateInfoKHR'::@intersectionShader@
pattern SHADER_GROUP_SHADER_INTERSECTION_KHR = ShaderGroupShaderKHR 3
{-# complete SHADER_GROUP_SHADER_GENERAL_KHR,
             SHADER_GROUP_SHADER_CLOSEST_HIT_KHR,
             SHADER_GROUP_SHADER_ANY_HIT_KHR,
             SHADER_GROUP_SHADER_INTERSECTION_KHR :: ShaderGroupShaderKHR #-}

conNameShaderGroupShaderKHR :: String
conNameShaderGroupShaderKHR = "ShaderGroupShaderKHR"

enumPrefixShaderGroupShaderKHR :: String
enumPrefixShaderGroupShaderKHR = "SHADER_GROUP_SHADER_"

showTableShaderGroupShaderKHR :: [(ShaderGroupShaderKHR, String)]
showTableShaderGroupShaderKHR =
  [ (SHADER_GROUP_SHADER_GENERAL_KHR     , "GENERAL_KHR")
  , (SHADER_GROUP_SHADER_CLOSEST_HIT_KHR , "CLOSEST_HIT_KHR")
  , (SHADER_GROUP_SHADER_ANY_HIT_KHR     , "ANY_HIT_KHR")
  , (SHADER_GROUP_SHADER_INTERSECTION_KHR, "INTERSECTION_KHR")
  ]

instance Show ShaderGroupShaderKHR where
  showsPrec = enumShowsPrec enumPrefixShaderGroupShaderKHR
                            showTableShaderGroupShaderKHR
                            conNameShaderGroupShaderKHR
                            (\(ShaderGroupShaderKHR x) -> x)
                            (showsPrec 11)

instance Read ShaderGroupShaderKHR where
  readPrec = enumReadPrec enumPrefixShaderGroupShaderKHR
                          showTableShaderGroupShaderKHR
                          conNameShaderGroupShaderKHR
                          ShaderGroupShaderKHR


type KHR_RAY_TRACING_PIPELINE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_PIPELINE_SPEC_VERSION"
pattern KHR_RAY_TRACING_PIPELINE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_RAY_TRACING_PIPELINE_SPEC_VERSION = 1


type KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME = "VK_KHR_ray_tracing_pipeline"

-- No documentation found for TopLevel "VK_KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME"
pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_RAY_TRACING_PIPELINE_EXTENSION_NAME = "VK_KHR_ray_tracing_pipeline"

