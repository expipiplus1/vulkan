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
module Vulkan.Extensions.VK_KHR_maintenance5  ( BufferUsageFlags2CreateInfoKHR
                                              , DeviceImageSubresourceInfoKHR
                                              , ImageSubresource2KHR
                                              , PhysicalDeviceMaintenance5FeaturesKHR
                                              , PhysicalDeviceMaintenance5PropertiesKHR
                                              , PipelineCreateFlags2CreateInfoKHR
                                              , RenderingAreaInfoKHR
                                              , SubresourceLayout2KHR
                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Chain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (Extendss)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Vulkan.CStruct.Extends (PokeChain)
data BufferUsageFlags2CreateInfoKHR

instance ToCStruct BufferUsageFlags2CreateInfoKHR
instance Show BufferUsageFlags2CreateInfoKHR

instance FromCStruct BufferUsageFlags2CreateInfoKHR


data DeviceImageSubresourceInfoKHR

instance ToCStruct DeviceImageSubresourceInfoKHR
instance Show DeviceImageSubresourceInfoKHR

instance FromCStruct DeviceImageSubresourceInfoKHR


data ImageSubresource2KHR

instance ToCStruct ImageSubresource2KHR
instance Show ImageSubresource2KHR

instance FromCStruct ImageSubresource2KHR


data PhysicalDeviceMaintenance5FeaturesKHR

instance ToCStruct PhysicalDeviceMaintenance5FeaturesKHR
instance Show PhysicalDeviceMaintenance5FeaturesKHR

instance FromCStruct PhysicalDeviceMaintenance5FeaturesKHR


data PhysicalDeviceMaintenance5PropertiesKHR

instance ToCStruct PhysicalDeviceMaintenance5PropertiesKHR
instance Show PhysicalDeviceMaintenance5PropertiesKHR

instance FromCStruct PhysicalDeviceMaintenance5PropertiesKHR


data PipelineCreateFlags2CreateInfoKHR

instance ToCStruct PipelineCreateFlags2CreateInfoKHR
instance Show PipelineCreateFlags2CreateInfoKHR

instance FromCStruct PipelineCreateFlags2CreateInfoKHR


data RenderingAreaInfoKHR

instance ToCStruct RenderingAreaInfoKHR
instance Show RenderingAreaInfoKHR

instance FromCStruct RenderingAreaInfoKHR


type role SubresourceLayout2KHR nominal
data SubresourceLayout2KHR (es :: [Type])

instance ( Extendss SubresourceLayout2KHR es
         , PokeChain es ) => ToCStruct (SubresourceLayout2KHR es)
instance Show (Chain es) => Show (SubresourceLayout2KHR es)

instance ( Extendss SubresourceLayout2KHR es
         , PeekChain es ) => FromCStruct (SubresourceLayout2KHR es)

