{-# language CPP #-}
-- | = Name
--
-- VK_KHR_maintenance5 - device extension
--
-- == VK_KHR_maintenance5
--
-- [__Name String__]
--     @VK_KHR_maintenance5@
--
-- [__Extension Type__]
--     Device extension
--
-- [__Registered Extension Number__]
--     471
--
-- [__Revision__]
--     1
--
-- [__Ratification Status__]
--     Ratified
--
-- [__Extension and Version Dependencies__]
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
--
-- [__API Interactions__]
--
--     -   Interacts with VK_VERSION_1_1
--
--     -   Interacts with VK_VERSION_1_2
--
--     -   Interacts with VK_VERSION_1_3
--
--     -   Interacts with VK_EXT_attachment_feedback_loop_layout
--
--     -   Interacts with VK_EXT_buffer_device_address
--
--     -   Interacts with VK_EXT_conditional_rendering
--
--     -   Interacts with VK_EXT_descriptor_buffer
--
--     -   Interacts with VK_EXT_fragment_density_map
--
--     -   Interacts with VK_EXT_graphics_pipeline_library
--
--     -   Interacts with VK_EXT_opacity_micromap
--
--     -   Interacts with VK_EXT_pipeline_creation_cache_control
--
--     -   Interacts with VK_EXT_pipeline_protected_access
--
--     -   Interacts with VK_EXT_transform_feedback
--
--     -   Interacts with VK_KHR_acceleration_structure
--
--     -   Interacts with VK_KHR_buffer_device_address
--
--     -   Interacts with VK_KHR_device_group
--
--     -   Interacts with VK_KHR_dynamic_rendering
--
--     -   Interacts with VK_KHR_fragment_shading_rate
--
--     -   Interacts with VK_KHR_pipeline_executable_properties
--
--     -   Interacts with VK_KHR_pipeline_library
--
--     -   Interacts with VK_KHR_ray_tracing_pipeline
--
--     -   Interacts with VK_KHR_video_decode_queue
--
--     -   Interacts with VK_KHR_video_encode_queue
--
--     -   Interacts with VK_NV_device_generated_commands
--
--     -   Interacts with VK_NV_displacement_micromap
--
--     -   Interacts with VK_NV_ray_tracing
--
--     -   Interacts with VK_NV_ray_tracing_motion_blur
--
-- [__Contact__]
--
--     -   Stu Smith
--         <https://github.com/KhronosGroup/Vulkan-Docs/issues/new?body=[VK_KHR_maintenance5] @stu-s%0A*Here describe the issue or question you have about the VK_KHR_maintenance5 extension* >
--
-- [__Extension Proposal__]
--     <https://github.com/KhronosGroup/Vulkan-Docs/tree/main/proposals/VK_KHR_maintenance5.adoc VK_KHR_maintenance5>
--
-- == Other Extension Metadata
--
-- [__Last Modified Date__]
--     2023-05-02
--
-- [__Interactions and External Dependencies__; __Contributors__]
--
--     -   Stu Smith, AMD
--
--     -   Tobias Hector, AMD
--
--     -   Shahbaz Youssefi, Google
--
--     -   Slawomir Cygan, Intel
--
--     -   Lionel Landwerlin, Intel
--
--     -   James Fitzpatrick, Imagination Technologies
--
--     -   Andrew Garrard, Imagination Technologies
--
--     -   Ralph Potter, Samsung
--
--     -   Pan Gao, Huawei
--
--     -   Jan-Harald Fredriksen, ARM
--
--     -   Jon Leech, Khronos
--
--     -   Mike Blumenkrantz, Valve
--
-- == Description
--
-- @VK_KHR_maintenance5@ adds a collection of minor features, none of which
-- would warrant an entire extension of their own.
--
-- The new features are as follows:
--
-- -   A new 'Vulkan.Core10.Enums.Format.FORMAT_A1B5G5R5_UNORM_PACK16_KHR'
--     format
--
-- -   A new 'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR' format
--
-- -   A property to indicate that multisample coverage operations are
--     performed after sample counting in EarlyFragmentTests mode
--
-- -   Relax VkBufferView creation requirements by allowing subsets of the
--     associated VkBuffer usage using 'BufferUsageFlags2CreateInfoKHR'
--
-- -   A new entry point 'cmdBindIndexBuffer2KHR', allowing a range of
--     memory to be bound as an index buffer
--
-- -   'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr' must return
--     @NULL@ for supported core functions beyond the version requested by
--     the application.
--
-- -   A property to indicate that the sample mask test is performed after
--     sample counting in EarlyFragmentTests mode
--
-- -   'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
--     now supports using 'Vulkan.Core10.APIConstants.WHOLE_SIZE' in the
--     @pSizes@ parameter.
--
-- -   A default size of 1.0 is used if @PointSize@ is not written
--
-- -   Shader modules are deprecated - applications can now pass
--     'Vulkan.Core10.Shader.ShaderModuleCreateInfo' as a chained struct to
--     pipeline creation via
--     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo'
--
-- -   A function 'getRenderingAreaGranularityKHR' to query the optimal
--     render area for a dynamic rendering instance.
--
-- -   A property to indicate that depth\/stencil texturing operations with
--     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
--     defined behavior
--
-- -   Add 'getImageSubresourceLayout2KHR' and a new function
--     'getDeviceImageSubresourceLayoutKHR' to allow the application to
--     query the image memory layout without having to create an image
--     object and query it.
--
-- -   Allow 'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' as the
--     @layerCount@ member of
--     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'
--
-- -   Adds stronger guarantees for propagation of
--     'Vulkan.Core10.Enums.Result.ERROR_DEVICE_LOST' return values
--
-- -   A property to indicate whether @PointSize@ controls the final
--     rasterization of polygons if
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-polygonmode polygon mode>
--     is 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT'
--
-- -   Two properties to indicate the non-strict line rasterization
--     algorithm used
--
-- -   Two new flags words 'PipelineCreateFlagBits2KHR' and
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR'
--
-- -   Physical-device-level functions can now be called with any value in
--     the valid range for a type beyond the defined enumerants, such that
--     applications can avoid checking individual features, extensions, or
--     versions before querying supported properties of a particular
--     enumerant.
--
-- -   Clarification that copies between images of any type are allowed,
--     treating 1D images as 2D images with a height of 1.
--
-- == New Commands
--
-- -   'cmdBindIndexBuffer2KHR'
--
-- -   'getDeviceImageSubresourceLayoutKHR'
--
-- -   'getImageSubresourceLayout2KHR'
--
-- -   'getRenderingAreaGranularityKHR'
--
-- == New Structures
--
-- -   'DeviceImageSubresourceInfoKHR'
--
-- -   'ImageSubresource2KHR'
--
-- -   'RenderingAreaInfoKHR'
--
-- -   'SubresourceLayout2KHR'
--
-- -   Extending 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
--     'Vulkan.Core10.Buffer.BufferCreateInfo',
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalBufferInfo',
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.DescriptorBufferBindingInfoEXT':
--
--     -   'BufferUsageFlags2CreateInfoKHR'
--
-- -   Extending 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
--     'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
--     'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
--     'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR':
--
--     -   'PipelineCreateFlags2CreateInfoKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
--     'Vulkan.Core10.Device.DeviceCreateInfo':
--
--     -   'PhysicalDeviceMaintenance5FeaturesKHR'
--
-- -   Extending
--     'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2':
--
--     -   'PhysicalDeviceMaintenance5PropertiesKHR'
--
-- == New Enums
--
-- -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR'
--
-- -   'PipelineCreateFlagBits2KHR'
--
-- == New Bitmasks
--
-- -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlags2KHR'
--
-- -   'PipelineCreateFlags2KHR'
--
-- == New Enum Constants
--
-- -   'KHR_MAINTENANCE_5_EXTENSION_NAME'
--
-- -   'KHR_MAINTENANCE_5_SPEC_VERSION'
--
-- -   Extending 'Vulkan.Core10.Enums.Format.Format':
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_A1B5G5R5_UNORM_PACK16_KHR'
--
--     -   'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR'
--
-- -   Extending 'Vulkan.Core10.Enums.StructureType.StructureType':
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_RENDERING_AREA_INFO_KHR'
--
--     -   'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_attachment_feedback_loop_layout VK_EXT_attachment_feedback_loop_layout>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
--     -   'PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_conditional_rendering VK_EXT_conditional_rendering>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_CONDITIONAL_RENDERING_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_descriptor_buffer VK_EXT_descriptor_buffer>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_PUSH_DESCRIPTORS_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_RESOURCE_DESCRIPTOR_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_SAMPLER_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_graphics_pipeline_library VK_EXT_graphics_pipeline_library>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT'
--
--     -   'PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_opacity_micromap VK_EXT_opacity_micromap>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_MICROMAP_BUILD_INPUT_READ_ONLY_BIT_EXT'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_MICROMAP_STORAGE_BIT_EXT'
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_protected_access VK_EXT_pipeline_protected_access>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT'
--
--     -   'PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_transform_feedback VK_EXT_transform_feedback>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_TRANSFORM_FEEDBACK_BUFFER_BIT_EXT'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_TRANSFORM_FEEDBACK_COUNTER_BUFFER_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_acceleration_structure VK_KHR_acceleration_structure>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_ACCELERATION_STRUCTURE_BUILD_INPUT_READ_ONLY_BIT_KHR'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_ACCELERATION_STRUCTURE_STORAGE_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_fragment_density_map VK_EXT_fragment_density_map>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_dynamic_rendering VK_KHR_dynamic_rendering>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_fragment_shading_rate VK_KHR_fragment_shading_rate>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_executable_properties VK_KHR_pipeline_executable_properties>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_pipeline_library VK_KHR_pipeline_library>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_LIBRARY_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_ray_tracing_pipeline VK_KHR_ray_tracing_pipeline>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR'
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_video_decode_queue VK_KHR_video_decode_queue>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_VIDEO_DECODE_DST_BIT_KHR'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_VIDEO_DECODE_SRC_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_video_encode_queue VK_KHR_video_encode_queue>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_VIDEO_ENCODE_DST_BIT_KHR'
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_VIDEO_ENCODE_SRC_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_device_generated_commands VK_NV_device_generated_commands>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_displacement_micromap VK_NV_displacement_micromap>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing VK_NV_ray_tracing>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'BUFFER_USAGE_2_RAY_TRACING_BIT_NV'
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_NV_ray_tracing_motion_blur VK_NV_ray_tracing_motion_blur>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.1 Version 1.1>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_device_group VK_KHR_device_group>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_DISPATCH_BASE_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.2 Version 1.2>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_buffer_device_address VK_KHR_buffer_device_address>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_buffer_device_address VK_EXT_buffer_device_address>
-- is supported:
--
-- -   Extending
--     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR':
--
--     -   'Vulkan.Extensions.VK_AMDX_shader_enqueue.BUFFER_USAGE_2_SHADER_DEVICE_ADDRESS_BIT_KHR'
--
-- If
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#versions-1.3 Version 1.3>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_pipeline_creation_cache_control VK_EXT_pipeline_creation_cache_control>
-- is supported:
--
-- -   Extending 'PipelineCreateFlagBits2KHR':
--
--     -   'PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR'
--
--     -   'PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR'
--
-- == Issues
--
-- None.
--
-- == Version History
--
-- -   Revision 1, 2022-12-12 (Stu Smith)
--
--     -   Initial revision
--
-- == See Also
--
-- 'DeviceImageSubresourceInfoKHR', 'ImageSubresource2KHR',
-- 'PhysicalDeviceMaintenance5FeaturesKHR',
-- 'PhysicalDeviceMaintenance5PropertiesKHR', 'RenderingAreaInfoKHR',
-- 'SubresourceLayout2KHR', 'cmdBindIndexBuffer2KHR',
-- 'getDeviceImageSubresourceLayoutKHR', 'getImageSubresourceLayout2KHR',
-- 'getRenderingAreaGranularityKHR'
--
-- == Document Notes
--
-- For more information, see the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#VK_KHR_maintenance5 Vulkan Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module Vulkan.Extensions.VK_KHR_maintenance5  ( getRenderingAreaGranularityKHR
                                              , cmdBindIndexBuffer2KHR
                                              , getImageSubresourceLayout2KHR
                                              , getDeviceImageSubresourceLayoutKHR
                                              , pattern BUFFER_USAGE_2_RAY_TRACING_BIT_NV
                                              , BufferUsageFlags2CreateInfoKHR(..)
                                              , PipelineCreateFlags2CreateInfoKHR(..)
                                              , PhysicalDeviceMaintenance5FeaturesKHR(..)
                                              , PhysicalDeviceMaintenance5PropertiesKHR(..)
                                              , RenderingAreaInfoKHR(..)
                                              , ImageSubresource2KHR(..)
                                              , SubresourceLayout2KHR(..)
                                              , DeviceImageSubresourceInfoKHR(..)
                                              , PipelineCreateFlags2KHR
                                              , PipelineCreateFlagBits2KHR( PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT_KHR
                                                                          , PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT_KHR
                                                                          , PIPELINE_CREATE_2_DERIVATIVE_BIT_KHR
                                                                          , PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV
                                                                          , PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT
                                                                          , PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT
                                                                          , PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
                                                                          , PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV
                                                                          , PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
                                                                          , PIPELINE_CREATE_2_LIBRARY_BIT_KHR
                                                                          , PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT
                                                                          , PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT
                                                                          , PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR
                                                                          , PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR
                                                                          , PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR
                                                                          , PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV
                                                                          , PIPELINE_CREATE_2_DISPATCH_BASE_BIT_KHR
                                                                          , PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
                                                                          , ..
                                                                          )
                                              , KHR_MAINTENANCE_5_SPEC_VERSION
                                              , pattern KHR_MAINTENANCE_5_SPEC_VERSION
                                              , KHR_MAINTENANCE_5_EXTENSION_NAME
                                              , pattern KHR_MAINTENANCE_5_EXTENSION_NAME
                                              , BufferUsageFlagBits2KHR(..)
                                              , BufferUsageFlags2KHR
                                              ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import Vulkan.Internal.Utils (traceAroundEvent)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Data.Typeable (eqT)
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.IO (throwIO)
import GHC.Ptr (castPtr)
import GHC.Ptr (nullFunPtr)
import Foreign.Ptr (nullPtr)
import Foreign.Ptr (plusPtr)
import GHC.Show (showString)
import Numeric (showHex)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Cont (evalContT)
import Data.Vector (generateM)
import qualified Data.Vector (imapM_)
import qualified Data.Vector (length)
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
import Foreign.Storable (Storable)
import Foreign.Storable (Storable(peek))
import Foreign.Storable (Storable(poke))
import qualified Foreign.Storable (Storable(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..))
import GHC.IO.Exception (IOException(..))
import Foreign.Ptr (FunPtr)
import Foreign.Ptr (Ptr)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Data.Word (Word32)
import Data.Kind (Type)
import Control.Monad.Trans.Cont (ContT(..))
import Data.Vector (Vector)
import Vulkan.CStruct.Utils (advancePtrBytes)
import Vulkan.Core10.FundamentalTypes (bool32ToBool)
import Vulkan.Core10.FundamentalTypes (boolToBool32)
import Vulkan.CStruct.Extends (forgetExtensions)
import Vulkan.CStruct.Extends (peekSomeCStruct)
import Vulkan.CStruct.Extends (withSomeCStruct)
import Vulkan.NamedType ((:::))
import Vulkan.Core10.FundamentalTypes (Bool32)
import Vulkan.Core10.Handles (Buffer)
import Vulkan.Core10.Handles (Buffer(..))
import Vulkan.Extensions.VK_AMDX_shader_enqueue (BufferUsageFlags2KHR)
import Vulkan.CStruct.Extends (Chain)
import Vulkan.Core10.Handles (CommandBuffer)
import Vulkan.Core10.Handles (CommandBuffer(..))
import Vulkan.Core10.Handles (CommandBuffer(CommandBuffer))
import Vulkan.Core10.Handles (CommandBuffer_T)
import Vulkan.Core10.Handles (Device)
import Vulkan.Core10.Handles (Device(..))
import Vulkan.Core10.Handles (Device(Device))
import Vulkan.Dynamic (DeviceCmds(pVkCmdBindIndexBuffer2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetDeviceImageSubresourceLayoutKHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetImageSubresourceLayout2KHR))
import Vulkan.Dynamic (DeviceCmds(pVkGetRenderingAreaGranularityKHR))
import Vulkan.Core10.FundamentalTypes (DeviceSize)
import Vulkan.Core10.Handles (Device_T)
import Vulkan.CStruct.Extends (Extends)
import Vulkan.CStruct.Extends (Extendss)
import Vulkan.CStruct.Extends (Extensible(..))
import Vulkan.Core10.FundamentalTypes (Extent2D)
import Vulkan.Core10.FundamentalTypes (Flags64)
import Vulkan.Core10.Enums.Format (Format)
import Vulkan.Core10.Handles (Image)
import Vulkan.Core10.Handles (Image(..))
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_image_compression_control (ImageCompressionPropertiesEXT)
import Vulkan.Core10.Image (ImageCreateInfo)
import Vulkan.Core10.SparseResourceMemoryManagement (ImageSubresource)
import Vulkan.Core10.Enums.IndexType (IndexType)
import Vulkan.Core10.Enums.IndexType (IndexType(..))
import Vulkan.CStruct.Extends (PeekChain)
import Vulkan.CStruct.Extends (PeekChain(..))
import Vulkan.CStruct.Extends (PokeChain)
import Vulkan.CStruct.Extends (PokeChain(..))
import Vulkan.CStruct.Extends (SomeStruct)
import Vulkan.CStruct.Extends (SomeStruct(..))
import Vulkan.Core10.Enums.StructureType (StructureType)
import {-# SOURCE #-} Vulkan.Extensions.VK_EXT_host_image_copy (SubresourceHostMemcpySizeEXT)
import Vulkan.Core10.Image (SubresourceLayout)
import Vulkan.Extensions.VK_AMDX_shader_enqueue (BufferUsageFlags2KHR)
import Vulkan.Extensions.VK_AMDX_shader_enqueue (BufferUsageFlagBits2KHR(BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_RENDERING_AREA_INFO_KHR))
import Vulkan.Core10.Enums.StructureType (StructureType(STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR))
import Vulkan.Extensions.VK_AMDX_shader_enqueue (BufferUsageFlagBits2KHR(..))
import Vulkan.Extensions.VK_AMDX_shader_enqueue (BufferUsageFlags2KHR)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderingAreaGranularityKHR
  :: FunPtr (Ptr Device_T -> Ptr RenderingAreaInfoKHR -> Ptr Extent2D -> IO ()) -> Ptr Device_T -> Ptr RenderingAreaInfoKHR -> Ptr Extent2D -> IO ()

-- | vkGetRenderingAreaGranularityKHR - Returns the granularity for dynamic
-- rendering optimal render area
--
-- = Description
--
-- The conditions leading to an optimal @renderArea@ are:
--
-- -   the @offset.x@ member in @renderArea@ is a multiple of the @width@
--     member of the returned 'Vulkan.Core10.FundamentalTypes.Extent2D'
--     (the horizontal granularity).
--
-- -   the @offset.y@ member in @renderArea@ is a multiple of the @height@
--     member of the returned 'Vulkan.Core10.FundamentalTypes.Extent2D'
--     (the vertical granularity).
--
-- -   either the @extent.width@ member in @renderArea@ is a multiple of
--     the horizontal granularity or @offset.x@+@extent.width@ is equal to
--     the @width@ of the @framebuffer@ in the
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'.
--
-- -   either the @extent.height@ member in @renderArea@ is a multiple of
--     the vertical granularity or @offset.y@+@extent.height@ is equal to
--     the @height@ of the @framebuffer@ in the
--     'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo'.
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetRenderingAreaGranularityKHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetRenderingAreaGranularityKHR-pRenderingAreaInfo-parameter#
--     @pRenderingAreaInfo@ /must/ be a valid pointer to a valid
--     'RenderingAreaInfoKHR' structure
--
-- -   #VUID-vkGetRenderingAreaGranularityKHR-pGranularity-parameter#
--     @pGranularity@ /must/ be a valid pointer to a
--     'Vulkan.Core10.FundamentalTypes.Extent2D' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Handles.Device',
-- 'Vulkan.Core10.FundamentalTypes.Extent2D', 'RenderingAreaInfoKHR'
getRenderingAreaGranularityKHR :: forall io
                                . (MonadIO io)
                               => -- | @device@ is the logical device that owns the render pass instance.
                                  Device
                               -> -- | @pRenderingAreaInfo@ is a pointer to a 'RenderingAreaInfoKHR' structure
                                  -- specifying details of the render pass instance to query the render area
                                  -- granularity for.
                                  RenderingAreaInfoKHR
                               -> io (("granularity" ::: Extent2D))
getRenderingAreaGranularityKHR device
                                 renderingAreaInfo = liftIO . evalContT $ do
  let vkGetRenderingAreaGranularityKHRPtr = pVkGetRenderingAreaGranularityKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetRenderingAreaGranularityKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetRenderingAreaGranularityKHR is null" Nothing Nothing
  let vkGetRenderingAreaGranularityKHR' = mkVkGetRenderingAreaGranularityKHR vkGetRenderingAreaGranularityKHRPtr
  pRenderingAreaInfo <- ContT $ withCStruct (renderingAreaInfo)
  pPGranularity <- ContT (withZeroCStruct @Extent2D)
  lift $ traceAroundEvent "vkGetRenderingAreaGranularityKHR" (vkGetRenderingAreaGranularityKHR'
                                                                (deviceHandle (device))
                                                                pRenderingAreaInfo
                                                                (pPGranularity))
  pGranularity <- lift $ peekCStruct @Extent2D pPGranularity
  pure $ (pGranularity)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindIndexBuffer2KHR
  :: FunPtr (Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> IndexType -> IO ()) -> Ptr CommandBuffer_T -> Buffer -> DeviceSize -> DeviceSize -> IndexType -> IO ()

-- | vkCmdBindIndexBuffer2KHR - Bind an index buffer to a command buffer
--
-- = Description
--
-- @size@ specifies the bound size of the index buffer starting from
-- @offset@. If @size@ is 'Vulkan.Core10.APIConstants.WHOLE_SIZE' then the
-- bound size is from @offset@ to the end of the @buffer@.
--
-- == Valid Usage
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-offset-08782# @offset@ /must/ be less
--     than the size of @buffer@
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-offset-08783# The sum of @offset@ and
--     the base address of the range of
--     'Vulkan.Core10.Handles.DeviceMemory' object that is backing
--     @buffer@, /must/ be a multiple of the size of the type indicated by
--     @indexType@
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-buffer-08784# @buffer@ /must/ have
--     been created with the
--     'Vulkan.Core10.Enums.BufferUsageFlagBits.BUFFER_USAGE_INDEX_BUFFER_BIT'
--     flag
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-buffer-08785# If @buffer@ is
--     non-sparse then it /must/ be bound completely and contiguously to a
--     single 'Vulkan.Core10.Handles.DeviceMemory' object
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-indexType-08786# @indexType@ /must/
--     not be 'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_NONE_KHR'
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-indexType-08787# If @indexType@ is
--     'Vulkan.Core10.Enums.IndexType.INDEX_TYPE_UINT8_EXT', the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-indexTypeUint8 indexTypeUint8>
--     feature /must/ be enabled
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-size-08767# If @size@ is not
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', @size@ /must/ be a multiple
--     of the size of the type indicated by @indexType@
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-size-08768# If @size@ is not
--     'Vulkan.Core10.APIConstants.WHOLE_SIZE', the sum of @offset@ and
--     @size@ /must/ be less than or equal to the size of @buffer@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-commandBuffer-parameter#
--     @commandBuffer@ /must/ be a valid
--     'Vulkan.Core10.Handles.CommandBuffer' handle
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-buffer-parameter# @buffer@ /must/ be
--     a valid 'Vulkan.Core10.Handles.Buffer' handle
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-indexType-parameter# @indexType@
--     /must/ be a valid 'Vulkan.Core10.Enums.IndexType.IndexType' value
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-commandBuffer-recording#
--     @commandBuffer@ /must/ be in the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#commandbuffers-lifecycle recording state>
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-commandBuffer-cmdpool# The
--     'Vulkan.Core10.Handles.CommandPool' that @commandBuffer@ was
--     allocated from /must/ support graphics operations
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-videocoding# This command /must/ only
--     be called outside of a video coding scope
--
-- -   #VUID-vkCmdBindIndexBuffer2KHR-commonparent# Both of @buffer@, and
--     @commandBuffer@ /must/ have been created, allocated, or retrieved
--     from the same 'Vulkan.Core10.Handles.Device'
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
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
-- | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkCommandBufferLevel Command Buffer Levels> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginRenderPass Render Pass Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkCmdBeginVideoCodingKHR Video Coding Scope> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueueFlagBits Supported Queue Types> | <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-queueoperation-command-types Command Type> |
-- +============================================================================================================================+========================================================================================================================+=============================================================================================================================+=======================================================================================================================+========================================================================================================================================+
-- | Primary                                                                                                                    | Both                                                                                                                   | Outside                                                                                                                     | Graphics                                                                                                              | State                                                                                                                                  |
-- | Secondary                                                                                                                  |                                                                                                                        |                                                                                                                             |                                                                                                                       |                                                                                                                                        |
-- +----------------------------------------------------------------------------------------------------------------------------+------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------------+-----------------------------------------------------------------------------------------------------------------------+----------------------------------------------------------------------------------------------------------------------------------------+
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Handles.Buffer', 'Vulkan.Core10.Handles.CommandBuffer',
-- 'Vulkan.Core10.FundamentalTypes.DeviceSize',
-- 'Vulkan.Core10.Enums.IndexType.IndexType'
cmdBindIndexBuffer2KHR :: forall io
                        . (MonadIO io)
                       => -- | @commandBuffer@ is the command buffer into which the command is
                          -- recorded.
                          CommandBuffer
                       -> -- | @buffer@ is the buffer being bound.
                          Buffer
                       -> -- | @offset@ is the starting offset in bytes within @buffer@ used in index
                          -- buffer address calculations.
                          ("offset" ::: DeviceSize)
                       -> -- | @size@ is the size in bytes of index data bound from @buffer@.
                          DeviceSize
                       -> -- | @indexType@ is a 'Vulkan.Core10.Enums.IndexType.IndexType' value
                          -- specifying the size of the indices.
                          IndexType
                       -> io ()
cmdBindIndexBuffer2KHR commandBuffer buffer offset size indexType = liftIO $ do
  let vkCmdBindIndexBuffer2KHRPtr = pVkCmdBindIndexBuffer2KHR (case commandBuffer of CommandBuffer{deviceCmds} -> deviceCmds)
  unless (vkCmdBindIndexBuffer2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkCmdBindIndexBuffer2KHR is null" Nothing Nothing
  let vkCmdBindIndexBuffer2KHR' = mkVkCmdBindIndexBuffer2KHR vkCmdBindIndexBuffer2KHRPtr
  traceAroundEvent "vkCmdBindIndexBuffer2KHR" (vkCmdBindIndexBuffer2KHR'
                                                 (commandBufferHandle (commandBuffer))
                                                 (buffer)
                                                 (offset)
                                                 (size)
                                                 (indexType))
  pure $ ()


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSubresourceLayout2KHR
  :: FunPtr (Ptr Device_T -> Image -> Ptr ImageSubresource2KHR -> Ptr (SomeStruct SubresourceLayout2KHR) -> IO ()) -> Ptr Device_T -> Image -> Ptr ImageSubresource2KHR -> Ptr (SomeStruct SubresourceLayout2KHR) -> IO ()

-- | vkGetImageSubresourceLayout2KHR - Retrieve information about an image
-- subresource
--
-- = Description
--
-- 'getImageSubresourceLayout2KHR' behaves similarly to
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', with the ability to
-- specify extended inputs via chained input structures, and to return
-- extended information via chained output structures.
--
-- It is legal to call 'getImageSubresourceLayout2KHR' with a @image@
-- created with @tiling@ equal to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', but the members
-- of 'SubresourceLayout2KHR'::@subresourceLayout@ will have undefined
-- values in this case.
--
-- Note
--
-- Structures chained from 'ImageSubresource2KHR'::@pNext@ will also be
-- updated when @tiling@ is equal to
-- 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL'.
--
-- == Valid Usage
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-aspectMask-00997# The
--     @aspectMask@ member of @pSubresource@ /must/ only have a single bit
--     set
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-mipLevel-01716# The @mipLevel@
--     member of @pSubresource@ /must/ be less than the @mipLevels@
--     specified in @image@
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-arrayLayer-01717# The
--     @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in @image@
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-format-08886# If @format@ of
--     the @image@ is a color format, @tiling@ of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and does not
--     have a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-format-04462# If @format@ of
--     the @image@ has a depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-format-04463# If @format@ of
--     the @image@ has a stencil component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-format-04464# If @format@ of
--     the @image@ does not contain a stencil or depth component, the
--     @aspectMask@ member of @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-tiling-08717# If the @tiling@
--     of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then the @aspectMask@ member of @pSubresource@ /must/ be a single
--     valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-image-09434# If @image@ was
--     created with the
--     'Vulkan.Core11.Enums.ExternalMemoryHandleTypeFlagBits.EXTERNAL_MEMORY_HANDLE_TYPE_ANDROID_HARDWARE_BUFFER_BIT_ANDROID'
--     external memory handle type, then @image@ /must/ be bound to memory
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-tiling-09435# If the @tiling@
--     of the @image@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT',
--     then the @aspectMask@ member of @pSubresource@ /must/ be
--     @VK_IMAGE_ASPECT_MEMORY_PLANE_i_BIT_EXT@ and the index /i/ /must/ be
--     less than the
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesEXT'::@drmFormatModifierPlaneCount@
--     associated with the image’s @format@ and
--     'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT'::@drmFormatModifier@
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-device-parameter# @device@
--     /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-image-parameter# @image@
--     /must/ be a valid 'Vulkan.Core10.Handles.Image' handle
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-pSubresource-parameter#
--     @pSubresource@ /must/ be a valid pointer to a valid
--     'ImageSubresource2KHR' structure
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-pLayout-parameter# @pLayout@
--     /must/ be a valid pointer to a 'SubresourceLayout2KHR' structure
--
-- -   #VUID-vkGetImageSubresourceLayout2KHR-image-parent# @image@ /must/
--     have been created, allocated, or retrieved from @device@
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Handles.Device', 'Vulkan.Core10.Handles.Image',
-- 'ImageSubresource2KHR', 'SubresourceLayout2KHR'
getImageSubresourceLayout2KHR :: forall a io
                               . ( Extendss SubresourceLayout2KHR a
                                 , PokeChain a
                                 , PeekChain a
                                 , MonadIO io )
                              => -- | @device@ is the logical device that owns the image.
                                 Device
                              -> -- | @image@ is the image whose layout is being queried.
                                 Image
                              -> -- | @pSubresource@ is a pointer to a 'ImageSubresource2KHR' structure
                                 -- selecting a specific image for the image subresource.
                                 ImageSubresource2KHR
                              -> io (SubresourceLayout2KHR a)
getImageSubresourceLayout2KHR device image subresource = liftIO . evalContT $ do
  let vkGetImageSubresourceLayout2KHRPtr = pVkGetImageSubresourceLayout2KHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetImageSubresourceLayout2KHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetImageSubresourceLayout2KHR is null" Nothing Nothing
  let vkGetImageSubresourceLayout2KHR' = mkVkGetImageSubresourceLayout2KHR vkGetImageSubresourceLayout2KHRPtr
  pSubresource <- ContT $ withCStruct (subresource)
  pPLayout <- ContT (withZeroCStruct @(SubresourceLayout2KHR _))
  lift $ traceAroundEvent "vkGetImageSubresourceLayout2KHR" (vkGetImageSubresourceLayout2KHR'
                                                               (deviceHandle (device))
                                                               (image)
                                                               pSubresource
                                                               (forgetExtensions (pPLayout)))
  pLayout <- lift $ peekCStruct @(SubresourceLayout2KHR _) pPLayout
  pure $ (pLayout)


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceImageSubresourceLayoutKHR
  :: FunPtr (Ptr Device_T -> Ptr DeviceImageSubresourceInfoKHR -> Ptr (SomeStruct SubresourceLayout2KHR) -> IO ()) -> Ptr Device_T -> Ptr DeviceImageSubresourceInfoKHR -> Ptr (SomeStruct SubresourceLayout2KHR) -> IO ()

-- | vkGetDeviceImageSubresourceLayoutKHR - Retrieve information about an
-- image subresource without an image object
--
-- = Description
--
-- 'getDeviceImageSubresourceLayoutKHR' behaves similarly to
-- 'getImageSubresourceLayout2KHR', but uses a
-- 'Vulkan.Core10.Image.ImageCreateInfo' structure to specify the image
-- rather than a 'Vulkan.Core10.Handles.Image' object.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Handles.Device', 'DeviceImageSubresourceInfoKHR',
-- 'SubresourceLayout2KHR'
getDeviceImageSubresourceLayoutKHR :: forall a io
                                    . ( Extendss SubresourceLayout2KHR a
                                      , PokeChain a
                                      , PeekChain a
                                      , MonadIO io )
                                   => -- | @device@ is the logical device that owns the image.
                                      --
                                      -- #VUID-vkGetDeviceImageSubresourceLayoutKHR-device-parameter# @device@
                                      -- /must/ be a valid 'Vulkan.Core10.Handles.Device' handle
                                      Device
                                   -> -- | @pInfo@ is a pointer to a 'DeviceImageSubresourceInfoKHR' structure
                                      -- containing parameters required for the subresource layout query.
                                      --
                                      -- #VUID-vkGetDeviceImageSubresourceLayoutKHR-pInfo-parameter# @pInfo@
                                      -- /must/ be a valid pointer to a valid 'DeviceImageSubresourceInfoKHR'
                                      -- structure
                                      DeviceImageSubresourceInfoKHR
                                   -> io (SubresourceLayout2KHR a)
getDeviceImageSubresourceLayoutKHR device info = liftIO . evalContT $ do
  let vkGetDeviceImageSubresourceLayoutKHRPtr = pVkGetDeviceImageSubresourceLayoutKHR (case device of Device{deviceCmds} -> deviceCmds)
  lift $ unless (vkGetDeviceImageSubresourceLayoutKHRPtr /= nullFunPtr) $
    throwIO $ IOError Nothing InvalidArgument "" "The function pointer for vkGetDeviceImageSubresourceLayoutKHR is null" Nothing Nothing
  let vkGetDeviceImageSubresourceLayoutKHR' = mkVkGetDeviceImageSubresourceLayoutKHR vkGetDeviceImageSubresourceLayoutKHRPtr
  pInfo <- ContT $ withCStruct (info)
  pPLayout <- ContT (withZeroCStruct @(SubresourceLayout2KHR _))
  lift $ traceAroundEvent "vkGetDeviceImageSubresourceLayoutKHR" (vkGetDeviceImageSubresourceLayoutKHR'
                                                                    (deviceHandle (device))
                                                                    pInfo
                                                                    (forgetExtensions (pPLayout)))
  pLayout <- lift $ peekCStruct @(SubresourceLayout2KHR _) pPLayout
  pure $ (pLayout)


-- No documentation found for TopLevel "VK_BUFFER_USAGE_2_RAY_TRACING_BIT_NV"
pattern BUFFER_USAGE_2_RAY_TRACING_BIT_NV = BUFFER_USAGE_2_SHADER_BINDING_TABLE_BIT_KHR


-- | VkBufferUsageFlags2CreateInfoKHR - Extended buffer usage flags
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of a buffer creation
-- structure, @usage@ is used instead of the corresponding @usage@ value
-- passed in that creation structure, allowing additional usage flags to be
-- specified. If this structure is included in the @pNext@ chain of a
-- buffer query structure, the usage flags of the buffer are returned in
-- @usage@ of this structure, and the usage flags representable in @usage@
-- of the buffer query structure are also returned in that field.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data BufferUsageFlags2CreateInfoKHR = BufferUsageFlags2CreateInfoKHR
  { -- | @usage@ is a bitmask of
    -- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR'
    -- specifying allowed usages of the buffer.
    --
    -- #VUID-VkBufferUsageFlags2CreateInfoKHR-usage-parameter# @usage@ /must/
    -- be a valid combination of
    -- 'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR'
    -- values
    --
    -- #VUID-VkBufferUsageFlags2CreateInfoKHR-usage-requiredbitmask# @usage@
    -- /must/ not be @0@
    usage :: BufferUsageFlags2KHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (BufferUsageFlags2CreateInfoKHR)
#endif
deriving instance Show BufferUsageFlags2CreateInfoKHR

instance ToCStruct BufferUsageFlags2CreateInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p BufferUsageFlags2CreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferUsageFlags2KHR)) (usage)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_BUFFER_USAGE_FLAGS_2_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr BufferUsageFlags2KHR)) (zero)
    f

instance FromCStruct BufferUsageFlags2CreateInfoKHR where
  peekCStruct p = do
    usage <- peek @BufferUsageFlags2KHR ((p `plusPtr` 16 :: Ptr BufferUsageFlags2KHR))
    pure $ BufferUsageFlags2CreateInfoKHR
             usage

instance Storable BufferUsageFlags2CreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero BufferUsageFlags2CreateInfoKHR where
  zero = BufferUsageFlags2CreateInfoKHR
           zero


-- | VkPipelineCreateFlags2CreateInfoKHR - Extended pipeline create flags
--
-- = Description
--
-- If this structure is included in the @pNext@ chain of a pipeline
-- creation structure, @flags@ is used instead of the corresponding @flags@
-- value passed in that creation structure, allowing additional creation
-- flags to be specified.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'PipelineCreateFlags2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PipelineCreateFlags2CreateInfoKHR = PipelineCreateFlags2CreateInfoKHR
  { -- | @flags@ is a bitmask of 'PipelineCreateFlagBits2KHR' specifying how a
    -- pipeline will be generated.
    --
    -- #VUID-VkPipelineCreateFlags2CreateInfoKHR-flags-parameter# @flags@
    -- /must/ be a valid combination of 'PipelineCreateFlagBits2KHR' values
    --
    -- #VUID-VkPipelineCreateFlags2CreateInfoKHR-flags-requiredbitmask# @flags@
    -- /must/ not be @0@
    flags :: PipelineCreateFlags2KHR }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PipelineCreateFlags2CreateInfoKHR)
#endif
deriving instance Show PipelineCreateFlags2CreateInfoKHR

instance ToCStruct PipelineCreateFlags2CreateInfoKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PipelineCreateFlags2CreateInfoKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2KHR)) (flags)
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PIPELINE_CREATE_FLAGS_2_CREATE_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2KHR)) (zero)
    f

instance FromCStruct PipelineCreateFlags2CreateInfoKHR where
  peekCStruct p = do
    flags <- peek @PipelineCreateFlags2KHR ((p `plusPtr` 16 :: Ptr PipelineCreateFlags2KHR))
    pure $ PipelineCreateFlags2CreateInfoKHR
             flags

instance Storable PipelineCreateFlags2CreateInfoKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PipelineCreateFlags2CreateInfoKHR where
  zero = PipelineCreateFlags2CreateInfoKHR
           zero


-- | VkPhysicalDeviceMaintenance5FeaturesKHR - Structure describing whether
-- the implementation supports maintenance5 functionality
--
-- = Members
--
-- This structure describes the following feature:
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance5FeaturesKHR' structure is included in
-- the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- it is filled in to indicate whether each corresponding feature is
-- supported. 'PhysicalDeviceMaintenance5FeaturesKHR' /can/ also be used in
-- the @pNext@ chain of 'Vulkan.Core10.Device.DeviceCreateInfo' to
-- selectively enable these features.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance5FeaturesKHR = PhysicalDeviceMaintenance5FeaturesKHR
  { -- | #features-maintenance5# @maintenance5@ indicates that the implementation
    -- supports the following:
    --
    -- -   The ability to expose support for the optional format
    --     'Vulkan.Core10.Enums.Format.FORMAT_A1B5G5R5_UNORM_PACK16_KHR'.
    --
    -- -   The ability to expose support for the optional format
    --     'Vulkan.Core10.Enums.Format.FORMAT_A8_UNORM_KHR'.
    --
    -- -   A property to indicate that multisample coverage operations are
    --     performed after sample counting in EarlyFragmentTests mode.
    --
    -- -   Creating a 'Vulkan.Core10.Handles.BufferView' with a subset of the
    --     associated 'Vulkan.Core10.Handles.Buffer' usage using
    --     'BufferUsageFlags2CreateInfoKHR'.
    --
    -- -   A new function 'cmdBindIndexBuffer2KHR', allowing a range of memory
    --     to be bound as an index buffer.
    --
    -- -   'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr' will return
    --     @NULL@ for function pointers of core functions for versions higher
    --     than the version requested by the application.
    --
    -- -   'Vulkan.Core13.Promoted_From_VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2'
    --     supports using 'Vulkan.Core10.APIConstants.WHOLE_SIZE' in the
    --     @pSizes@ parameter.
    --
    -- -   If @PointSize@ is not written, a default value of @1.0@ is used for
    --     the size of points.
    --
    -- -   'Vulkan.Core10.Shader.ShaderModuleCreateInfo' /can/ be added as a
    --     chained structure to pipeline creation via
    --     'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo', rather than
    --     having to create a shader module.
    --
    -- -   A function 'getRenderingAreaGranularityKHR' to query the optimal
    --     render area for a dynamic rendering instance.
    --
    -- -   A property to indicate that depth\/stencil texturing operations with
    --     'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
    --     defined behavior.
    --
    -- -   'getDeviceImageSubresourceLayoutKHR' allows an application to
    --     perform a 'Vulkan.Core10.Image.getImageSubresourceLayout' query
    --     without having to create an image.
    --
    -- -   'Vulkan.Core10.APIConstants.REMAINING_ARRAY_LAYERS' as the
    --     @layerCount@ member of
    --     'Vulkan.Core10.CommandBufferBuilding.ImageSubresourceLayers'.
    --
    -- -   A property to indicate whether @PointSize@ controls the final
    --     rasterization of polygons if
    --     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-polygonmode polygon mode>
    --     is 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT'.
    --
    -- -   Two properties to indicate the non-strict line rasterization
    --     algorithm used.
    --
    -- -   Two new flags words 'PipelineCreateFlagBits2KHR' and
    --     'Vulkan.Extensions.VK_AMDX_shader_enqueue.BufferUsageFlagBits2KHR'.
    --
    -- -   Physical-device-level functions /can/ now be called with any value
    --     in the valid range for a type beyond the defined enumerants, such
    --     that applications can avoid checking individual features,
    --     extensions, or versions before querying supported properties of a
    --     particular enumerant.
    --
    -- -   Copies between images of any type are allowed, with 1D images
    --     treated as 2D images with a height of @1@.
    maintenance5 :: Bool }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance5FeaturesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance5FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance5FeaturesKHR where
  withCStruct x f = allocaBytes 24 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance5FeaturesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (maintenance5))
    f
  cStructSize = 24
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_FEATURES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance5FeaturesKHR where
  peekCStruct p = do
    maintenance5 <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance5FeaturesKHR
             (bool32ToBool maintenance5)

instance Storable PhysicalDeviceMaintenance5FeaturesKHR where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance5FeaturesKHR where
  zero = PhysicalDeviceMaintenance5FeaturesKHR
           zero


-- | VkPhysicalDeviceMaintenance5PropertiesKHR - Structure describing various
-- implementation-defined properties introduced with VK_KHR_maintenance5
--
-- = Description
--
-- If the 'PhysicalDeviceMaintenance5PropertiesKHR' structure is included
-- in the @pNext@ chain of the
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2'
-- structure passed to
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- it is filled in with each corresponding implementation-dependent
-- property.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.FundamentalTypes.Bool32',
-- 'Vulkan.Core10.Enums.StructureType.StructureType'
data PhysicalDeviceMaintenance5PropertiesKHR = PhysicalDeviceMaintenance5PropertiesKHR
  { -- | @earlyFragmentMultisampleCoverageAfterSampleCounting@ is a boolean value
    -- indicating whether the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-shader fragment shading>
    -- and
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-covg multisample coverage>
    -- operations are performed after
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-samplecount sample counting>
    -- for
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-shader fragment shaders>
    -- with @EarlyFragmentTests@ execution mode.
    earlyFragmentMultisampleCoverageAfterSampleCounting :: Bool
  , -- | @earlyFragmentSampleMaskTestBeforeSampleCounting@ is a boolean value
    -- indicating whether the
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-samplemask sample mask test>
    -- operation is performed before
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-samplecount sample counting>
    -- for
    -- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#fragops-shader fragment shaders>
    -- using the @EarlyFragmentTests@ execution mode.
    earlyFragmentSampleMaskTestBeforeSampleCounting :: Bool
  , -- | @depthStencilSwizzleOneSupport@ is a boolean indicating that
    -- depth\/stencil texturing operations with
    -- 'Vulkan.Core10.Enums.ComponentSwizzle.COMPONENT_SWIZZLE_ONE' have
    -- defined behavior.
    depthStencilSwizzleOneSupport :: Bool
  , -- | @polygonModePointSize@ is a boolean value indicating whether the point
    -- size of the final rasterization of polygons with
    -- 'Vulkan.Core10.Enums.PolygonMode.POLYGON_MODE_POINT' is controlled by
    -- @PointSize@.
    polygonModePointSize :: Bool
  , -- | @nonStrictSinglePixelWideLinesUseParallelogram@ is a boolean value
    -- indicating whether non-strict lines with a width of 1.0 are rasterized
    -- as parallelograms or using Bresenham’s algorithm.
    nonStrictSinglePixelWideLinesUseParallelogram :: Bool
  , -- | @nonStrictWideLinesUseParallelogram@ is a boolean value indicating
    -- whether non-strict lines with a width greater than 1.0 are rasterized as
    -- parallelograms or using Bresenham’s algorithm.
    nonStrictWideLinesUseParallelogram :: Bool
  }
  deriving (Typeable, Eq)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (PhysicalDeviceMaintenance5PropertiesKHR)
#endif
deriving instance Show PhysicalDeviceMaintenance5PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance5PropertiesKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p PhysicalDeviceMaintenance5PropertiesKHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (earlyFragmentMultisampleCoverageAfterSampleCounting))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (earlyFragmentSampleMaskTestBeforeSampleCounting))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (depthStencilSwizzleOneSupport))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (polygonModePointSize))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (nonStrictSinglePixelWideLinesUseParallelogram))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (nonStrictWideLinesUseParallelogram))
    f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_5_PROPERTIES_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 20 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 24 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 28 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 32 :: Ptr Bool32)) (boolToBool32 (zero))
    poke ((p `plusPtr` 36 :: Ptr Bool32)) (boolToBool32 (zero))
    f

instance FromCStruct PhysicalDeviceMaintenance5PropertiesKHR where
  peekCStruct p = do
    earlyFragmentMultisampleCoverageAfterSampleCounting <- peek @Bool32 ((p `plusPtr` 16 :: Ptr Bool32))
    earlyFragmentSampleMaskTestBeforeSampleCounting <- peek @Bool32 ((p `plusPtr` 20 :: Ptr Bool32))
    depthStencilSwizzleOneSupport <- peek @Bool32 ((p `plusPtr` 24 :: Ptr Bool32))
    polygonModePointSize <- peek @Bool32 ((p `plusPtr` 28 :: Ptr Bool32))
    nonStrictSinglePixelWideLinesUseParallelogram <- peek @Bool32 ((p `plusPtr` 32 :: Ptr Bool32))
    nonStrictWideLinesUseParallelogram <- peek @Bool32 ((p `plusPtr` 36 :: Ptr Bool32))
    pure $ PhysicalDeviceMaintenance5PropertiesKHR
             (bool32ToBool earlyFragmentMultisampleCoverageAfterSampleCounting)
             (bool32ToBool earlyFragmentSampleMaskTestBeforeSampleCounting)
             (bool32ToBool depthStencilSwizzleOneSupport)
             (bool32ToBool polygonModePointSize)
             (bool32ToBool nonStrictSinglePixelWideLinesUseParallelogram)
             (bool32ToBool nonStrictWideLinesUseParallelogram)

instance Storable PhysicalDeviceMaintenance5PropertiesKHR where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero PhysicalDeviceMaintenance5PropertiesKHR where
  zero = PhysicalDeviceMaintenance5PropertiesKHR
           zero
           zero
           zero
           zero
           zero
           zero


-- | VkRenderingAreaInfoKHR - Structure describing rendering area granularity
-- query info
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Enums.Format.Format',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getRenderingAreaGranularityKHR'
data RenderingAreaInfoKHR = RenderingAreaInfoKHR
  { -- | @viewMask@ is the viewMask used for rendering.
    viewMask :: Word32
  , -- | @pColorAttachmentFormats@ is a pointer to an array of
    -- 'Vulkan.Core10.Enums.Format.Format' values defining the format of color
    -- attachments used in the render pass instance.
    colorAttachmentFormats :: Vector Format
  , -- | @depthAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the depth attachment used in the render pass
    -- instance.
    depthAttachmentFormat :: Format
  , -- | @stencilAttachmentFormat@ is a 'Vulkan.Core10.Enums.Format.Format' value
    -- defining the format of the stencil attachment used in the render pass
    -- instance.
    stencilAttachmentFormat :: Format
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (RenderingAreaInfoKHR)
#endif
deriving instance Show RenderingAreaInfoKHR

instance ToCStruct RenderingAreaInfoKHR where
  withCStruct x f = allocaBytes 40 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p RenderingAreaInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_AREA_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr Word32)) (viewMask)
    lift $ poke ((p `plusPtr` 20 :: Ptr Word32)) ((fromIntegral (Data.Vector.length $ (colorAttachmentFormats)) :: Word32))
    pPColorAttachmentFormats' <- ContT $ allocaBytes @Format ((Data.Vector.length (colorAttachmentFormats)) * 4)
    lift $ Data.Vector.imapM_ (\i e -> poke (pPColorAttachmentFormats' `plusPtr` (4 * (i)) :: Ptr Format) (e)) (colorAttachmentFormats)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr Format))) (pPColorAttachmentFormats')
    lift $ poke ((p `plusPtr` 32 :: Ptr Format)) (depthAttachmentFormat)
    lift $ poke ((p `plusPtr` 36 :: Ptr Format)) (stencilAttachmentFormat)
    lift $ f
  cStructSize = 40
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_RENDERING_AREA_INFO_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr Word32)) (zero)
    poke ((p `plusPtr` 32 :: Ptr Format)) (zero)
    poke ((p `plusPtr` 36 :: Ptr Format)) (zero)
    f

instance FromCStruct RenderingAreaInfoKHR where
  peekCStruct p = do
    viewMask <- peek @Word32 ((p `plusPtr` 16 :: Ptr Word32))
    colorAttachmentCount <- peek @Word32 ((p `plusPtr` 20 :: Ptr Word32))
    pColorAttachmentFormats <- peek @(Ptr Format) ((p `plusPtr` 24 :: Ptr (Ptr Format)))
    pColorAttachmentFormats' <- generateM (fromIntegral colorAttachmentCount) (\i -> peek @Format ((pColorAttachmentFormats `advancePtrBytes` (4 * (i)) :: Ptr Format)))
    depthAttachmentFormat <- peek @Format ((p `plusPtr` 32 :: Ptr Format))
    stencilAttachmentFormat <- peek @Format ((p `plusPtr` 36 :: Ptr Format))
    pure $ RenderingAreaInfoKHR
             viewMask
             pColorAttachmentFormats'
             depthAttachmentFormat
             stencilAttachmentFormat

instance Zero RenderingAreaInfoKHR where
  zero = RenderingAreaInfoKHR
           zero
           mempty
           zero
           zero


-- | VkImageSubresource2KHR - Structure specifying an image subresource
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'DeviceImageSubresourceInfoKHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.getImageSubresourceLayout2EXT',
-- 'getImageSubresourceLayout2KHR'
data ImageSubresource2KHR = ImageSubresource2KHR
  { -- | @imageSubresource@ is a
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
    -- structure.
    --
    -- #VUID-VkImageSubresource2KHR-imageSubresource-parameter#
    -- @imageSubresource@ /must/ be a valid
    -- 'Vulkan.Core10.SparseResourceMemoryManagement.ImageSubresource'
    -- structure
    imageSubresource :: ImageSubresource }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (ImageSubresource2KHR)
#endif
deriving instance Show ImageSubresource2KHR

instance ToCStruct ImageSubresource2KHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p ImageSubresource2KHR{..} f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresource)) (imageSubresource)
    f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = do
    poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_IMAGE_SUBRESOURCE_2_KHR)
    poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    poke ((p `plusPtr` 16 :: Ptr ImageSubresource)) (zero)
    f

instance FromCStruct ImageSubresource2KHR where
  peekCStruct p = do
    imageSubresource <- peekCStruct @ImageSubresource ((p `plusPtr` 16 :: Ptr ImageSubresource))
    pure $ ImageSubresource2KHR
             imageSubresource

instance Storable ImageSubresource2KHR where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek = peekCStruct
  poke ptr poked = pokeCStruct ptr poked (pure ())

instance Zero ImageSubresource2KHR where
  zero = ImageSubresource2KHR
           zero


-- | VkSubresourceLayout2KHR - Structure specifying subresource layout
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkSubresourceLayout2KHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR'
--
-- -   #VUID-VkSubresourceLayout2KHR-pNext-pNext# Each @pNext@ member of
--     any structure (including this one) in the @pNext@ chain /must/ be
--     either @NULL@ or a pointer to a valid instance of
--     'Vulkan.Extensions.VK_EXT_image_compression_control.ImageCompressionPropertiesEXT'
--     or
--     'Vulkan.Extensions.VK_EXT_host_image_copy.SubresourceHostMemcpySizeEXT'
--
-- -   #VUID-VkSubresourceLayout2KHR-sType-unique# The @sType@ value of
--     each struct in the @pNext@ chain /must/ be unique
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_host_image_copy VK_EXT_host_image_copy>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_image_compression_control VK_EXT_image_compression_control>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'Vulkan.Core10.Image.SubresourceLayout',
-- 'getDeviceImageSubresourceLayoutKHR',
-- 'Vulkan.Extensions.VK_EXT_host_image_copy.getImageSubresourceLayout2EXT',
-- 'getImageSubresourceLayout2KHR'
data SubresourceLayout2KHR (es :: [Type]) = SubresourceLayout2KHR
  { -- | @pNext@ is @NULL@ or a pointer to a structure extending this structure.
    next :: Chain es
  , -- | @subresourceLayout@ is a 'Vulkan.Core10.Image.SubresourceLayout'
    -- structure.
    subresourceLayout :: SubresourceLayout
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (SubresourceLayout2KHR (es :: [Type]))
#endif
deriving instance Show (Chain es) => Show (SubresourceLayout2KHR es)

instance Extensible SubresourceLayout2KHR where
  extensibleTypeName = "SubresourceLayout2KHR"
  setNext SubresourceLayout2KHR{..} next' = SubresourceLayout2KHR{next = next', ..}
  getNext SubresourceLayout2KHR{..} = next
  extends :: forall e b proxy. Typeable e => proxy e -> (Extends SubresourceLayout2KHR e => b) -> Maybe b
  extends _ f
    | Just Refl <- eqT @e @ImageCompressionPropertiesEXT = Just f
    | Just Refl <- eqT @e @SubresourceHostMemcpySizeEXT = Just f
    | otherwise = Nothing

instance ( Extendss SubresourceLayout2KHR es
         , PokeChain es ) => ToCStruct (SubresourceLayout2KHR es) where
  withCStruct x f = allocaBytes 56 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p SubresourceLayout2KHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR)
    pNext'' <- fmap castPtr . ContT $ withChain (next)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext''
    lift $ poke ((p `plusPtr` 16 :: Ptr SubresourceLayout)) (subresourceLayout)
    lift $ f
  cStructSize = 56
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_SUBRESOURCE_LAYOUT_2_KHR)
    pNext' <- fmap castPtr . ContT $ withZeroChain @es
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) pNext'
    lift $ poke ((p `plusPtr` 16 :: Ptr SubresourceLayout)) (zero)
    lift $ f

instance ( Extendss SubresourceLayout2KHR es
         , PeekChain es ) => FromCStruct (SubresourceLayout2KHR es) where
  peekCStruct p = do
    pNext <- peek @(Ptr ()) ((p `plusPtr` 8 :: Ptr (Ptr ())))
    next <- peekChain (castPtr pNext)
    subresourceLayout <- peekCStruct @SubresourceLayout ((p `plusPtr` 16 :: Ptr SubresourceLayout))
    pure $ SubresourceLayout2KHR
             next subresourceLayout

instance es ~ '[] => Zero (SubresourceLayout2KHR es) where
  zero = SubresourceLayout2KHR
           ()
           zero


-- | VkDeviceImageSubresourceInfoKHR - Image creation information for
-- querying subresource layout
--
-- == Valid Usage
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-aspectMask-00997# The
--     @aspectMask@ member of @pSubresource@ /must/ only have a single bit
--     set
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-mipLevel-01716# The @mipLevel@
--     member of @pSubresource@ /must/ be less than the @mipLevels@
--     specified in @pCreateInfo@
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-arrayLayer-01717# The
--     @arrayLayer@ member of @pSubresource@ /must/ be less than the
--     @arrayLayers@ specified in @pCreateInfo@
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-format-08886# If @format@ of
--     the @pCreateInfo@ is a color format, @tiling@ of the @pCreateInfo@
--     is 'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' or
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_OPTIMAL', and does not
--     have a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     the @aspectMask@ member of @pSubresource@ /must/ be
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_COLOR_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-format-04462# If @format@ of
--     the @pCreateInfo@ has a depth component, the @aspectMask@ member of
--     @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-format-04463# If @format@ of
--     the @pCreateInfo@ has a stencil component, the @aspectMask@ member
--     of @pSubresource@ /must/ contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-format-04464# If @format@ of
--     the @pCreateInfo@ does not contain a stencil or depth component, the
--     @aspectMask@ member of @pSubresource@ /must/ not contain
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_DEPTH_BIT' or
--     'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_STENCIL_BIT'
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-tiling-08717# If the @tiling@
--     of the @pCreateInfo@ is
--     'Vulkan.Core10.Enums.ImageTiling.IMAGE_TILING_LINEAR' and has a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar image format>,
--     then the @aspectMask@ member of @pSubresource@ /must/ be a single
--     valid
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-planes-image-aspect multi-planar aspect mask>
--     bit
--
-- == Valid Usage (Implicit)
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-sType-sType# @sType@ /must/ be
--     'Vulkan.Core10.Enums.StructureType.STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO_KHR'
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-pNext-pNext# @pNext@ /must/ be
--     @NULL@
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-pCreateInfo-parameter#
--     @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Vulkan.Core10.Image.ImageCreateInfo' structure
--
-- -   #VUID-VkDeviceImageSubresourceInfoKHR-pSubresource-parameter#
--     @pSubresource@ /must/ be a valid pointer to a valid
--     'ImageSubresource2KHR' structure
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>,
-- 'Vulkan.Core10.Image.ImageCreateInfo', 'ImageSubresource2KHR',
-- 'Vulkan.Core10.Enums.StructureType.StructureType',
-- 'getDeviceImageSubresourceLayoutKHR'
data DeviceImageSubresourceInfoKHR = DeviceImageSubresourceInfoKHR
  { -- | @pCreateInfo@ is a pointer to a 'Vulkan.Core10.Image.ImageCreateInfo'
    -- structure containing parameters affecting creation of the image to
    -- query.
    createInfo :: SomeStruct ImageCreateInfo
  , -- | @pSubresource@ pSubresource is a pointer to a 'ImageSubresource2KHR'
    -- structure selecting a specific image subresource for the query.
    subresource :: ImageSubresource2KHR
  }
  deriving (Typeable)
#if defined(GENERIC_INSTANCES)
deriving instance Generic (DeviceImageSubresourceInfoKHR)
#endif
deriving instance Show DeviceImageSubresourceInfoKHR

instance ToCStruct DeviceImageSubresourceInfoKHR where
  withCStruct x f = allocaBytes 32 $ \p -> pokeCStruct p x (f p)
  pokeCStruct p DeviceImageSubresourceInfoKHR{..} f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo (createInfo) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    pSubresource'' <- ContT $ withCStruct (subresource)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageSubresource2KHR))) pSubresource''
    lift $ f
  cStructSize = 32
  cStructAlignment = 8
  pokeZeroCStruct p f = evalContT $ do
    lift $ poke ((p `plusPtr` 0 :: Ptr StructureType)) (STRUCTURE_TYPE_DEVICE_IMAGE_SUBRESOURCE_INFO_KHR)
    lift $ poke ((p `plusPtr` 8 :: Ptr (Ptr ()))) (nullPtr)
    pCreateInfo'' <- ContT @_ @_ @(Ptr (ImageCreateInfo '[])) $ \cont -> withSomeCStruct @ImageCreateInfo ((SomeStruct zero)) (cont . castPtr)
    lift $ poke ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _)))) pCreateInfo''
    pSubresource'' <- ContT $ withCStruct (zero)
    lift $ poke ((p `plusPtr` 24 :: Ptr (Ptr ImageSubresource2KHR))) pSubresource''
    lift $ f

instance FromCStruct DeviceImageSubresourceInfoKHR where
  peekCStruct p = do
    pCreateInfo <- peekSomeCStruct . forgetExtensions =<< peek ((p `plusPtr` 16 :: Ptr (Ptr (ImageCreateInfo _))))
    pSubresource <- peekCStruct @ImageSubresource2KHR =<< peek ((p `plusPtr` 24 :: Ptr (Ptr ImageSubresource2KHR)))
    pure $ DeviceImageSubresourceInfoKHR
             pCreateInfo pSubresource

instance Zero DeviceImageSubresourceInfoKHR where
  zero = DeviceImageSubresourceInfoKHR
           (SomeStruct zero)
           zero


type PipelineCreateFlags2KHR = PipelineCreateFlagBits2KHR

-- | VkPipelineCreateFlagBits2KHR - Bitmask controlling how a pipeline is
-- created
--
-- = Description
--
-- -   'PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT_KHR' specifies that the
--     created pipeline will not be optimized. Using this flag /may/ reduce
--     the time taken to create the pipeline.
--
-- -   'PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT_KHR' specifies that the
--     pipeline to be created is allowed to be the parent of a pipeline
--     that will be created in a subsequent pipeline creation call.
--
-- -   'PIPELINE_CREATE_2_DERIVATIVE_BIT_KHR' specifies that the pipeline
--     to be created will be a child of a previously created parent
--     pipeline.
--
-- -   'PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR' specifies
--     that any shader input variables decorated as @ViewIndex@ will be
--     assigned values as if they were decorated as @DeviceIndex@.
--
-- -   'PIPELINE_CREATE_2_DISPATCH_BASE_BIT_KHR' specifies that a compute
--     pipeline /can/ be used with
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase'
--     with a non-zero base workgroup.
--
-- -   'PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV' specifies that a pipeline
--     is created with all shaders in the deferred state. Before using the
--     pipeline the application /must/ call
--     'Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV' exactly once
--     on each shader in the pipeline before using the pipeline.
--
-- -   'PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR' specifies that the
--     shader compiler should capture statistics for the pipeline
--     executables produced by the compile process which /can/ later be
--     retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time.
--
-- -   'PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR'
--     specifies that the shader compiler should capture the internal
--     representations of pipeline executables produced by the compile
--     process which /can/ later be retrieved by calling
--     'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR'.
--     Enabling this flag /must/ not affect the final compiled pipeline but
--     /may/ disable pipeline caching or otherwise affect pipeline creation
--     time. When capturing IR from pipelines created with pipeline
--     libraries, there is no guarantee that IR from libraries /can/ be
--     retrieved from the linked pipeline. Applications /should/ retrieve
--     IR from each library, and any linked pipelines, separately.
--
-- -   'PIPELINE_CREATE_2_LIBRARY_BIT_KHR' specifies that the pipeline
--     /cannot/ be used directly, and instead defines a /pipeline library/
--     that /can/ be combined with other pipelines using the
--     'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR'
--     structure. This is available in ray tracing and graphics pipelines.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR'
--     specifies that an any-hit shader will always be present when an
--     any-hit shader would be executed. A NULL any-hit shader is an
--     any-hit shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR'
--     specifies that a closest hit shader will always be present when a
--     closest hit shader would be executed. A NULL closest hit shader is a
--     closest hit shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR'
--     specifies that a miss shader will always be present when a miss
--     shader would be executed. A NULL miss shader is a miss shader which
--     is effectively 'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such
--     as from a shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR'
--     specifies that an intersection shader will always be present when an
--     intersection shader would be executed. A NULL intersection shader is
--     an intersection shader which is effectively
--     'Vulkan.Core10.APIConstants.SHADER_UNUSED_KHR', such as from a
--     shader group consisting entirely of zeros.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR' specifies
--     that triangle primitives will be skipped during traversal using
--     @OpTraceRayKHR@.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR' specifies that
--     AABB primitives will be skipped during traversal using
--     @OpTraceRayKHR@.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR'
--     specifies that the shader group handles /can/ be saved and reused on
--     a subsequent run (e.g. for trace capture and replay).
--
-- -   'PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV' specifies that the
--     pipeline can be used in combination with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#device-generated-commands>.
--
-- -   'PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR'
--     specifies that pipeline creation will fail if a compile is required
--     for creation of a valid 'Vulkan.Core10.Handles.Pipeline' object;
--     'Vulkan.Core10.Enums.Result.PIPELINE_COMPILE_REQUIRED' will be
--     returned by pipeline creation, and the
--     'Vulkan.Core10.Handles.Pipeline' will be set to
--     'Vulkan.Core10.APIConstants.NULL_HANDLE'.
--
-- -   When creating multiple pipelines,
--     'PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR' specifies that
--     control will be returned to the application if any individual
--     pipeline returns a result which is not
--     'Vulkan.Core10.Enums.Result.SUCCESS' rather than continuing to
--     create additional pipelines.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV' specifies that
--     the pipeline is allowed to use @OpTraceRayMotionNV@.
--
-- -   'PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     specifies that the pipeline will be used with a fragment shading
--     rate attachment.
--
-- -   'PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT'
--     specifies that the pipeline will be used with a fragment density map
--     attachment.
--
-- -   'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT' specifies that
--     pipeline libraries being linked into this library /should/ have link
--     time optimizations applied. If this bit is omitted, implementations
--     /should/ instead perform linking as rapidly as possible.
--
-- -   'PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT'
--     specifies that pipeline libraries should retain any information
--     necessary to later perform an optimal link with
--     'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT'.
--
-- -   'PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT' specifies that a
--     pipeline will be used with
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorbuffers descriptor buffers>,
--     rather than
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets descriptor sets>.
--
-- -   'PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT' specifies
--     that the pipeline /may/ be used with an attachment feedback loop
--     including color attachments.
--
-- -   'PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT'
--     specifies that the pipeline /may/ be used with an attachment
--     feedback loop including depth-stencil attachments.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT' specifies
--     that the ray tracing pipeline /can/ be used with acceleration
--     structures which reference an opacity micromap array.
--
-- -   'PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV'
--     specifies that the ray tracing pipeline /can/ be used with
--     acceleration structures which reference a displacement micromap
--     array.
--
-- -   'PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT' specifies that the
--     pipeline /must/ not be bound to a protected command buffer.
--
-- -   'PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT' specifies that the
--     pipeline /must/ not be bound to an unprotected command buffer.
--
-- It is valid to set both 'PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT_KHR'
-- and 'PIPELINE_CREATE_2_DERIVATIVE_BIT_KHR'. This allows a pipeline to be
-- both a parent and possibly a child in a pipeline hierarchy. See
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-pipeline-derivatives Pipeline Derivatives>
-- for more information.
--
-- When an implementation is looking up a pipeline in a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#pipelines-cache pipeline cache>,
-- if that pipeline is being created using linked libraries,
-- implementations /should/ always return an equivalent pipeline created
-- with 'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT' if available,
-- whether or not that bit was specified.
--
-- Note
--
-- Using 'PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT' (or not) when
-- linking pipeline libraries is intended as a performance tradeoff between
-- host and device. If the bit is omitted, linking should be faster and
-- produce a pipeline more rapidly, but performance of the pipeline on the
-- target device may be reduced. If the bit is included, linking may be
-- slower but should produce a pipeline with device performance comparable
-- to a monolithically created pipeline. Using both options can allow
-- latency-sensitive applications to generate a suboptimal but usable
-- pipeline quickly, and then perform an optimal link in the background,
-- substituting the result for the suboptimally linked pipeline as soon as
-- it is available.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_maintenance5 VK_KHR_maintenance5>
newtype PipelineCreateFlagBits2KHR = PipelineCreateFlagBits2KHR Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT_KHR"
pattern PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000001

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT_KHR"
pattern PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000002

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_DERIVATIVE_BIT_KHR"
pattern PIPELINE_CREATE_2_DERIVATIVE_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000004

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT"
pattern PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000020000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV"
pattern PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV = PipelineCreateFlagBits2KHR 0x0000000010000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT"
pattern PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000040000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT"
pattern PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000008000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000004000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000002000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT"
pattern PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000001000000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
pattern PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000000400000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000200000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV"
pattern PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV = PipelineCreateFlagBits2KHR 0x0000000000100000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV"
pattern PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV = PipelineCreateFlagBits2KHR 0x0000000000040000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000080000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000020000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000010000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000008000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000004000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000002000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
pattern PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000001000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_LIBRARY_BIT_KHR"
pattern PIPELINE_CREATE_2_LIBRARY_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000800

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT"
pattern PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000000800000

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT"
pattern PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT = PipelineCreateFlagBits2KHR 0x0000000000000400

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR"
pattern PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000200

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR"
pattern PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000100

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
pattern PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000080

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR"
pattern PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000040

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV"
pattern PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV = PipelineCreateFlagBits2KHR 0x0000000000000020

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_DISPATCH_BASE_BIT_KHR"
pattern PIPELINE_CREATE_2_DISPATCH_BASE_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000010

-- No documentation found for Nested "VkPipelineCreateFlagBits2KHR" "VK_PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR"
pattern PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR = PipelineCreateFlagBits2KHR 0x0000000000000008

conNamePipelineCreateFlagBits2KHR :: String
conNamePipelineCreateFlagBits2KHR = "PipelineCreateFlagBits2KHR"

enumPrefixPipelineCreateFlagBits2KHR :: String
enumPrefixPipelineCreateFlagBits2KHR = "PIPELINE_CREATE_2_"

showTablePipelineCreateFlagBits2KHR :: [(PipelineCreateFlagBits2KHR, String)]
showTablePipelineCreateFlagBits2KHR =
  [
    ( PIPELINE_CREATE_2_DISABLE_OPTIMIZATION_BIT_KHR
    , "DISABLE_OPTIMIZATION_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_ALLOW_DERIVATIVES_BIT_KHR
    , "ALLOW_DERIVATIVES_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_DERIVATIVE_BIT_KHR
    , "DERIVATIVE_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_DESCRIPTOR_BUFFER_BIT_EXT
    , "DESCRIPTOR_BUFFER_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV
    , "RAY_TRACING_DISPLACEMENT_MICROMAP_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_PROTECTED_ACCESS_ONLY_BIT_EXT
    , "PROTECTED_ACCESS_ONLY_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_NO_PROTECTED_ACCESS_BIT_EXT
    , "NO_PROTECTED_ACCESS_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "DEPTH_STENCIL_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "COLOR_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_OPACITY_MICROMAP_BIT_EXT
    , "RAY_TRACING_OPACITY_MICROMAP_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT
    , "RENDERING_FRAGMENT_DENSITY_MAP_ATTACHMENT_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "RENDERING_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_ALLOW_MOTION_BIT_NV
    , "RAY_TRACING_ALLOW_MOTION_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_INDIRECT_BINDABLE_BIT_NV
    , "INDIRECT_BINDABLE_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR
    , "RAY_TRACING_SHADER_GROUP_HANDLE_CAPTURE_REPLAY_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_INTERSECTION_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_MISS_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_CLOSEST_HIT_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR
    , "RAY_TRACING_NO_NULL_ANY_HIT_SHADERS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_SKIP_AABBS_BIT_KHR
    , "RAY_TRACING_SKIP_AABBS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RAY_TRACING_SKIP_TRIANGLES_BIT_KHR
    , "RAY_TRACING_SKIP_TRIANGLES_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_LIBRARY_BIT_KHR
    , "LIBRARY_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT
    , "RETAIN_LINK_TIME_OPTIMIZATION_INFO_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_LINK_TIME_OPTIMIZATION_BIT_EXT
    , "LINK_TIME_OPTIMIZATION_BIT_EXT"
    )
  ,
    ( PIPELINE_CREATE_2_EARLY_RETURN_ON_FAILURE_BIT_KHR
    , "EARLY_RETURN_ON_FAILURE_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR
    , "FAIL_ON_PIPELINE_COMPILE_REQUIRED_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR
    , "CAPTURE_INTERNAL_REPRESENTATIONS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_CAPTURE_STATISTICS_BIT_KHR
    , "CAPTURE_STATISTICS_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_DEFER_COMPILE_BIT_NV
    , "DEFER_COMPILE_BIT_NV"
    )
  ,
    ( PIPELINE_CREATE_2_DISPATCH_BASE_BIT_KHR
    , "DISPATCH_BASE_BIT_KHR"
    )
  ,
    ( PIPELINE_CREATE_2_VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR
    , "VIEW_INDEX_FROM_DEVICE_INDEX_BIT_KHR"
    )
  ]

instance Show PipelineCreateFlagBits2KHR where
  showsPrec =
    enumShowsPrec
      enumPrefixPipelineCreateFlagBits2KHR
      showTablePipelineCreateFlagBits2KHR
      conNamePipelineCreateFlagBits2KHR
      (\(PipelineCreateFlagBits2KHR x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read PipelineCreateFlagBits2KHR where
  readPrec =
    enumReadPrec
      enumPrefixPipelineCreateFlagBits2KHR
      showTablePipelineCreateFlagBits2KHR
      conNamePipelineCreateFlagBits2KHR
      PipelineCreateFlagBits2KHR

type KHR_MAINTENANCE_5_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_5_SPEC_VERSION"
pattern KHR_MAINTENANCE_5_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_MAINTENANCE_5_SPEC_VERSION = 1


type KHR_MAINTENANCE_5_EXTENSION_NAME = "VK_KHR_maintenance5"

-- No documentation found for TopLevel "VK_KHR_MAINTENANCE_5_EXTENSION_NAME"
pattern KHR_MAINTENANCE_5_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_MAINTENANCE_5_EXTENSION_NAME = "VK_KHR_maintenance5"

