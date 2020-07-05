{-# language CPP #-}
module Vulkan.Core10.Enums.StructureType  (StructureType( STRUCTURE_TYPE_APPLICATION_INFO
                                                        , STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                                                        , STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
                                                        , STRUCTURE_TYPE_DEVICE_CREATE_INFO
                                                        , STRUCTURE_TYPE_SUBMIT_INFO
                                                        , STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
                                                        , STRUCTURE_TYPE_BIND_SPARSE_INFO
                                                        , STRUCTURE_TYPE_FENCE_CREATE_INFO
                                                        , STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
                                                        , STRUCTURE_TYPE_EVENT_CREATE_INFO
                                                        , STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
                                                        , STRUCTURE_TYPE_BUFFER_CREATE_INFO
                                                        , STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
                                                        , STRUCTURE_TYPE_IMAGE_CREATE_INFO
                                                        , STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
                                                        , STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
                                                        , STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
                                                        , STRUCTURE_TYPE_SAMPLER_CREATE_INFO
                                                        , STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
                                                        , STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
                                                        , STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
                                                        , STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
                                                        , STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
                                                        , STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
                                                        , STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
                                                        , STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
                                                        , STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
                                                        , STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
                                                        , STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
                                                        , STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
                                                        , STRUCTURE_TYPE_MEMORY_BARRIER
                                                        , STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
                                                        , STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
                                                        , STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT
                                                        , STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT
                                                        , STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM
                                                        , STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV
                                                        , STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV
                                                        , STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV
                                                        , STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV
                                                        , STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR
                                                        , STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR
                                                        , STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR
                                                        , STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR
                                                        , STRUCTURE_TYPE_PIPELINE_INFO_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR
                                                        , STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT
                                                        , STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
                                                        , STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
                                                        , STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT
                                                        , STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV
                                                        , STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
                                                        , STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
                                                        , STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
                                                        , STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
                                                        , STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
                                                        , STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
                                                        , STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL
                                                        , STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL
                                                        , STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL
                                                        , STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL
                                                        , STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL
                                                        , STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL
                                                        , STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_CHECKPOINT_DATA_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
                                                        , STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
                                                        , STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
                                                        , STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
                                                        , STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
                                                        , STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
                                                        , STRUCTURE_TYPE_GEOMETRY_AABB_NV
                                                        , STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
                                                        , STRUCTURE_TYPE_GEOMETRY_NV
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
                                                        , STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
                                                        , STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV
                                                        , STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR
                                                        , STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR
                                                        , STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR
                                                        , STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR
                                                        , STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR
                                                        , STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR
                                                        , STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR
                                                        , STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
                                                        , STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
                                                        , STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
                                                        , STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
                                                        , STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
                                                        , STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
                                                        , STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
                                                        , STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
                                                        , STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
                                                        , STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
                                                        , STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
                                                        , STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
                                                        , STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
                                                        , STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
                                                        , STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
                                                        , STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
                                                        , STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
                                                        , STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
                                                        , STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
                                                        , STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR
                                                        , STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR
                                                        , STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR
                                                        , STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR
                                                        , STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR
                                                        , STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
                                                        , STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
                                                        , STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
                                                        , STRUCTURE_TYPE_HDR_METADATA_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
                                                        , STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
                                                        , STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
                                                        , STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
                                                        , STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
                                                        , STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PRESENT_REGIONS_KHR
                                                        , STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
                                                        , STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
                                                        , STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
                                                        , STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
                                                        , STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
                                                        , STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
                                                        , STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
                                                        , STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
                                                        , STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
                                                        , STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
                                                        , STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
                                                        , STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT
                                                        , STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
                                                        , STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
                                                        , STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
                                                        , STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
                                                        , STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
                                                        , STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
                                                        , STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
                                                        , STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
                                                        , STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
                                                        , STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
                                                        , STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
                                                        , STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
                                                        , STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX
                                                        , STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
                                                        , STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
                                                        , STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
                                                        , STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
                                                        , STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
                                                        , STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
                                                        , STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
                                                        , STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
                                                        , STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
                                                        , STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_PRESENT_INFO_KHR
                                                        , STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
                                                        , STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO
                                                        , STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO
                                                        , STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES
                                                        , STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO
                                                        , STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO
                                                        , STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO
                                                        , STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES
                                                        , STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT
                                                        , STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES
                                                        , STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO
                                                        , STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO
                                                        , STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES
                                                        , STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES
                                                        , STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES
                                                        , STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES
                                                        , STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT
                                                        , STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES
                                                        , STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES
                                                        , STRUCTURE_TYPE_SUBPASS_END_INFO
                                                        , STRUCTURE_TYPE_SUBPASS_BEGIN_INFO
                                                        , STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2
                                                        , STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2
                                                        , STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2
                                                        , STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2
                                                        , STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2
                                                        , STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
                                                        , STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
                                                        , STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
                                                        , STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
                                                        , STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
                                                        , STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
                                                        , STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
                                                        , STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
                                                        , STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
                                                        , STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
                                                        , STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
                                                        , STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
                                                        , STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
                                                        , STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
                                                        , STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
                                                        , STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
                                                        , STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
                                                        , STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
                                                        , STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
                                                        , STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
                                                        , STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
                                                        , STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
                                                        , STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
                                                        , STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
                                                        , STRUCTURE_TYPE_FORMAT_PROPERTIES_2
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
                                                        , STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
                                                        , STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
                                                        , STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
                                                        , STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
                                                        , STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
                                                        , STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
                                                        , STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
                                                        , STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
                                                        , STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
                                                        , STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
                                                        , STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
                                                        , STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
                                                        , STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
                                                        , STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
                                                        , ..
                                                        )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkStructureType - Vulkan structure types (@sType@)
--
-- = Description
--
-- Each value corresponds to a particular structure with a @sType@ member
-- with a matching name. As a general rule, the name of each
-- 'StructureType' value is obtained by taking the name of the structure,
-- stripping the leading @Vk@, prefixing each capital letter with @_@,
-- converting the entire resulting string to upper case, and prefixing it
-- with @VK_STRUCTURE_TYPE_@. For example, structures of type
-- 'Vulkan.Core10.Image.ImageCreateInfo' correspond to a 'StructureType' of
-- 'STRUCTURE_TYPE_IMAGE_CREATE_INFO', and thus its @sType@ member /must/
-- equal that when it is passed to the API.
--
-- The values 'STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO' and
-- 'STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO' are reserved for internal use
-- by the loader, and do not have corresponding Vulkan structures in this
-- Specification.
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureBuildGeometryInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateGeometryTypeInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureDeviceAddressInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryAabbsDataKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryInstancesDataKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureMemoryRequirementsInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.AccelerationStructureMemoryRequirementsInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.AccelerationStructureVersionKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_performance_query.AcquireProfilingLockInfoKHR',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferPropertiesANDROID',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferUsageANDROID',
-- 'Vulkan.Extensions.VK_KHR_android_surface.AndroidSurfaceCreateInfoKHR',
-- 'Vulkan.Core10.DeviceInitialization.ApplicationInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentDescriptionStencilLayout',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentReference2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.AttachmentReferenceStencilLayout',
-- 'Vulkan.CStruct.Extends.BaseInStructure',
-- 'Vulkan.CStruct.Extends.BaseOutStructure',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.BindAccelerationStructureMemoryInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindBufferMemoryDeviceGroupInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Vulkan.Extensions.VK_KHR_swapchain.BindImageMemorySwapchainInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo',
-- 'Vulkan.Core10.Buffer.BufferCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.BufferDeviceAddressCreateInfoEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferDeviceAddressInfo',
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.BufferMemoryRequirementsInfo2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferOpaqueCaptureAddressCreateInfo',
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_calibrated_timestamps.CalibratedTimestampInfoEXT',
-- 'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.CheckpointDataNV',
-- 'Vulkan.Core10.CommandBuffer.CommandBufferAllocateInfo',
-- 'Vulkan.Core10.CommandBuffer.CommandBufferBeginInfo',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.CommandBufferInheritanceConditionalRenderingInfoEXT',
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.CommandBufferInheritanceRenderPassTransformInfoQCOM',
-- 'Vulkan.Core10.CommandPool.CommandPoolCreateInfo',
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.CooperativeMatrixPropertiesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.CopyAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.CopyAccelerationStructureToMemoryInfoKHR',
-- 'Vulkan.Core10.DescriptorSet.CopyDescriptorSet',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.CopyMemoryToAccelerationStructureInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.D3D12FenceSubmitInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.DebugMarkerMarkerInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.DebugMarkerObjectNameInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.DebugMarkerObjectTagInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_report.DebugReportCallbackCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsLabelEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessengerCallbackDataEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsMessengerCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectNameInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.DebugUtilsObjectTagInfoEXT',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationBufferCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationImageCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.DeferredOperationInfoKHR',
-- 'Vulkan.Core10.DescriptorSet.DescriptorPoolCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.DescriptorPoolInlineUniformBlockCreateInfoEXT',
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetLayoutBindingFlagsCreateInfo',
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.DescriptorSetLayoutSupport',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetVariableDescriptorCountAllocateInfo',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.DescriptorSetVariableDescriptorCountLayoutSupport',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Vulkan.Core10.Device.DeviceCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_diagnostics_config.DeviceDiagnosticsConfigCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_display_control.DeviceEventInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupBindSparseInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupCommandBufferBeginInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupPresentInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupRenderPassBeginInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.DeviceGroupSubmitInfo',
-- 'Vulkan.Extensions.VK_KHR_swapchain.DeviceGroupSwapchainCreateInfoKHR',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.DeviceMemoryOpaqueCaptureAddressInfo',
-- 'Vulkan.Extensions.VK_AMD_memory_overallocation_behavior.DeviceMemoryOverallocationCreateInfoAMD',
-- 'Vulkan.Extensions.VK_EXT_private_data.DevicePrivateDataCreateInfoEXT',
-- 'Vulkan.Core10.Device.DeviceQueueCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_global_priority.DeviceQueueGlobalPriorityCreateInfoEXT',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.DeviceQueueInfo2',
-- 'Vulkan.Extensions.VK_EXT_directfb_surface.DirectFBSurfaceCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_display_control.DisplayEventInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_display.DisplayModeCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayModeProperties2KHR',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.DisplayNativeHdrSurfaceCapabilitiesAMD',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneCapabilities2KHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayPlaneProperties2KHR',
-- 'Vulkan.Extensions.VK_EXT_display_control.DisplayPowerInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.DisplayPresentInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.DisplayProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.DisplaySurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.DrmFormatModifierPropertiesListEXT',
-- 'Vulkan.Core10.Event.EventCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence.ExportFenceCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.ExportFenceWin32HandleInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExportMemoryAllocateInfo',
-- 'Vulkan.Extensions.VK_NV_external_memory.ExportMemoryAllocateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.ExportMemoryWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_NV_external_memory_win32.ExportMemoryWin32HandleInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore.ExportSemaphoreCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.ExportSemaphoreWin32HandleInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalBufferProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.ExternalFenceProperties',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ExternalFormatANDROID',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.ExternalImageFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryBufferCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo',
-- 'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.ExternalSemaphoreProperties',
-- 'Vulkan.Core10.Fence.FenceCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.FenceGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.FenceGetWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_filter_cubic.FilterCubicImageViewImageFormatPropertiesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.FormatProperties2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentImageInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentsCreateInfo',
-- 'Vulkan.Core10.Pass.FramebufferCreateInfo',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.FramebufferMixedSamplesCombinationNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsShaderGroupCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_hdr_metadata.HdrMetadataEXT',
-- 'Vulkan.Extensions.VK_EXT_headless_surface.HeadlessSurfaceCreateInfoEXT',
-- 'Vulkan.Extensions.VK_MVK_ios_surface.IOSSurfaceCreateInfoMVK',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierExplicitCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierListCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.ImageDrmFormatModifierPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.ImageFormatProperties2',
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2',
-- 'Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.ImagePipeSurfaceCreateInfoFUCHSIA',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageSparseMemoryRequirementsInfo2',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_separate_stencil_usage.ImageStencilUsageCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_swapchain.ImageSwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_astc_decode_mode.ImageViewASTCDecodeModeEXT',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewAddressPropertiesNVX',
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.ImageViewUsageCreateInfo',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.ImportAndroidHardwareBufferInfoANDROID',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.ImportFenceFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.ImportFenceWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.ImportMemoryFdInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.ImportMemoryHostPointerInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.ImportMemoryWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_NV_external_memory_win32.ImportMemoryWin32HandleInfoNV',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.ImportSemaphoreFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.ImportSemaphoreWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.InitializePerformanceApiInfoINTEL',
-- 'Vulkan.Core10.DeviceInitialization.InstanceCreateInfo',
-- 'Vulkan.Extensions.VK_MVK_macos_surface.MacOSSurfaceCreateInfoMVK',
-- 'Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.MemoryAllocateFlagsInfo',
-- 'Vulkan.Core10.Memory.MemoryAllocateInfo',
-- 'Vulkan.Core10.OtherTypes.MemoryBarrier',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedRequirements',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.MemoryFdPropertiesKHR',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.MemoryGetAndroidHardwareBufferInfoANDROID',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.MemoryGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.MemoryGetWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.MemoryHostPointerPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.MemoryOpaqueCaptureAddressAllocateInfo',
-- 'Vulkan.Extensions.VK_EXT_memory_priority.MemoryPriorityAllocateInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.MemoryRequirements2',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.MemoryWin32HandlePropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_metal_surface.MetalSurfaceCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.MultisamplePropertiesEXT',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceConfigurationAcquireInfoINTEL',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterDescriptionKHR',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceCounterKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceMarkerInfoINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceOverrideInfoINTEL',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PerformanceQuerySubmitInfoKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.PerformanceStreamMarkerInfoINTEL',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_16bit_storage.PhysicalDevice16BitStorageFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_8bit_storage.PhysicalDevice8BitStorageFeatures',
-- 'Vulkan.Extensions.VK_EXT_astc_decode_mode.PhysicalDeviceASTCDecodeFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeatures',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.PhysicalDeviceBufferDeviceAddressFeaturesEXT',
-- 'Vulkan.Extensions.VK_AMD_device_coherent_memory.PhysicalDeviceCoherentMemoryFeaturesAMD',
-- 'Vulkan.Extensions.VK_NV_compute_shader_derivatives.PhysicalDeviceComputeShaderDerivativesFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.PhysicalDeviceConditionalRenderingFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.PhysicalDeviceCooperativeMatrixPropertiesNV',
-- 'Vulkan.Extensions.VK_NV_corner_sampled_image.PhysicalDeviceCornerSampledImageFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PhysicalDeviceCoverageReductionModeFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.PhysicalDeviceCustomBorderColorPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing.PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PhysicalDeviceDepthClipEnableFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.PhysicalDeviceDepthStencilResolveProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_descriptor_indexing.PhysicalDeviceDescriptorIndexingProperties',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.PhysicalDeviceDeviceGeneratedCommandsPropertiesNV',
-- 'Vulkan.Extensions.VK_NV_device_diagnostics_config.PhysicalDeviceDiagnosticsConfigFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PhysicalDeviceDiscardRectanglePropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_driver_properties.PhysicalDeviceDriverProperties',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PhysicalDeviceExclusiveScissorFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.PhysicalDeviceExtendedDynamicStateFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalBufferInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.PhysicalDeviceExternalFenceInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceExternalImageFormatInfo',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.PhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.PhysicalDeviceExternalSemaphoreInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceFeatures2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float_controls.PhysicalDeviceFloatControlsProperties',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map2.PhysicalDeviceFragmentDensityMap2PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.PhysicalDeviceFragmentDensityMapPropertiesEXT',
-- 'Vulkan.Extensions.VK_NV_fragment_shader_barycentric.PhysicalDeviceFragmentShaderBarycentricFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_fragment_shader_interlock.PhysicalDeviceFragmentShaderInterlockFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.PhysicalDeviceHostQueryResetFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.PhysicalDeviceIDProperties',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.PhysicalDeviceImageDrmFormatModifierInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
-- 'Vulkan.Extensions.VK_EXT_filter_cubic.PhysicalDeviceImageViewImageFormatInfoEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.PhysicalDeviceImagelessFramebufferFeatures',
-- 'Vulkan.Extensions.VK_EXT_index_type_uint8.PhysicalDeviceIndexTypeUint8FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.PhysicalDeviceInlineUniformBlockPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PhysicalDeviceLineRasterizationPropertiesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.PhysicalDeviceMaintenance3Properties',
-- 'Vulkan.Extensions.VK_EXT_memory_budget.PhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_memory_priority.PhysicalDeviceMemoryPriorityFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceMemoryProperties2',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.PhysicalDeviceMeshShaderPropertiesNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewFeatures',
-- 'Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.PhysicalDeviceMultiviewProperties',
-- 'Vulkan.Extensions.VK_EXT_pci_bus_info.PhysicalDevicePCIBusInfoPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_performance_query.PhysicalDevicePerformanceQueryPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_pipeline_creation_cache_control.PhysicalDevicePipelineCreationCacheControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PhysicalDevicePipelineExecutablePropertiesFeaturesKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PhysicalDevicePointClippingProperties',
-- 'Vulkan.Extensions.VK_EXT_private_data.PhysicalDevicePrivateDataFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceProperties2',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryFeatures',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.PhysicalDeviceProtectedMemoryProperties',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.PhysicalDevicePushDescriptorPropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.PhysicalDeviceRayTracingFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.PhysicalDeviceRayTracingPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.PhysicalDeviceRayTracingPropertiesNV',
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PhysicalDeviceRepresentativeFragmentTestFeaturesNV',
-- 'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2FeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_robustness2.PhysicalDeviceRobustness2PropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.PhysicalDeviceSamplerFilterMinmaxProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.PhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_scalar_block_layout.PhysicalDeviceScalarBlockLayoutFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_separate_depth_stencil_layouts.PhysicalDeviceSeparateDepthStencilLayoutsFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_atomic_int64.PhysicalDeviceShaderAtomicInt64Features',
-- 'Vulkan.Extensions.VK_KHR_shader_clock.PhysicalDeviceShaderClockFeaturesKHR',
-- 'Vulkan.Extensions.VK_AMD_shader_core_properties2.PhysicalDeviceShaderCoreProperties2AMD',
-- 'Vulkan.Extensions.VK_AMD_shader_core_properties.PhysicalDeviceShaderCorePropertiesAMD',
-- 'Vulkan.Extensions.VK_EXT_shader_demote_to_helper_invocation.PhysicalDeviceShaderDemoteToHelperInvocationFeaturesEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_shader_draw_parameters.PhysicalDeviceShaderDrawParametersFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_float16_int8.PhysicalDeviceShaderFloat16Int8Features',
-- 'Vulkan.Extensions.VK_NV_shader_image_footprint.PhysicalDeviceShaderImageFootprintFeaturesNV',
-- 'Vulkan.Extensions.VK_INTEL_shader_integer_functions2.PhysicalDeviceShaderIntegerFunctions2FeaturesINTEL',
-- 'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_shader_sm_builtins.PhysicalDeviceShaderSMBuiltinsPropertiesNV',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_shader_subgroup_extended_types.PhysicalDeviceShaderSubgroupExtendedTypesFeatures',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImageFeaturesNV',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PhysicalDeviceShadingRateImagePropertiesNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceSparseImageFormatInfo2',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_subgroup.PhysicalDeviceSubgroupProperties',
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PhysicalDeviceSubgroupSizeControlPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.PhysicalDeviceSurfaceInfo2KHR',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_texel_buffer_alignment.PhysicalDeviceTexelBufferAlignmentPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_texture_compression_astc_hdr.PhysicalDeviceTextureCompressionASTCHDRFeaturesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreFeatures',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.PhysicalDeviceTimelineSemaphoreProperties',
-- 'Vulkan.Extensions.VK_EXT_tooling_info.PhysicalDeviceToolPropertiesEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_uniform_buffer_standard_layout.PhysicalDeviceUniformBufferStandardLayoutFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_variable_pointers.PhysicalDeviceVariablePointersFeatures',
-- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PhysicalDeviceVertexAttributeDivisorPropertiesEXT',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Features',
-- 'Vulkan.Core12.PhysicalDeviceVulkan11Properties',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Features',
-- 'Vulkan.Core12.PhysicalDeviceVulkan12Properties',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_vulkan_memory_model.PhysicalDeviceVulkanMemoryModelFeatures',
-- 'Vulkan.Extensions.VK_EXT_ycbcr_image_arrays.PhysicalDeviceYcbcrImageArraysFeaturesEXT',
-- 'Vulkan.Core10.PipelineCache.PipelineCacheCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_blend_operation_advanced.PipelineColorBlendAdvancedStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineColorBlendStateCreateInfo',
-- 'Vulkan.Extensions.VK_AMD_pipeline_compiler_control.PipelineCompilerControlCreateInfoAMD',
-- 'Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.PipelineCoverageModulationStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.PipelineCoverageReductionStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_fragment_coverage_to_color.PipelineCoverageToColorStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_pipeline_creation_feedback.PipelineCreationFeedbackCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineDepthStencilStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.PipelineDiscardRectangleStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineDynamicStateCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInternalRepresentationKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutablePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableStatisticKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineInfoKHR',
-- 'Vulkan.Core10.Pipeline.PipelineInputAssemblyStateCreateInfo',
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR',
-- 'Vulkan.Core10.Pipeline.PipelineMultisampleStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_conservative_rasterization.PipelineRasterizationConservativeStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_depth_clip_enable.PipelineRasterizationDepthClipStateCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.PipelineRasterizationLineStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineRasterizationStateCreateInfo',
-- 'Vulkan.Extensions.VK_AMD_rasterization_order.PipelineRasterizationStateRasterizationOrderAMD',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.PipelineRasterizationStateStreamCreateInfoEXT',
-- 'Vulkan.Extensions.VK_NV_representative_fragment_test.PipelineRepresentativeFragmentTestStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.PipelineSampleLocationsStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_subgroup_size_control.PipelineShaderStageRequiredSubgroupSizeCreateInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.PipelineTessellationDomainOriginStateCreateInfo',
-- 'Vulkan.Core10.Pipeline.PipelineTessellationStateCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.PipelineVertexInputDivisorStateCreateInfoEXT',
-- 'Vulkan.Core10.Pipeline.PipelineVertexInputStateCreateInfo',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportCoarseSampleOrderStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.PipelineViewportExclusiveScissorStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.PipelineViewportShadingRateImageStateCreateInfoNV',
-- 'Vulkan.Core10.Pipeline.PipelineViewportStateCreateInfo',
-- 'Vulkan.Extensions.VK_NV_viewport_swizzle.PipelineViewportSwizzleStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.PipelineViewportWScalingStateCreateInfoNV',
-- 'Vulkan.Extensions.VK_GGP_frame_token.PresentFrameTokenGGP',
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_incremental_present.PresentRegionsKHR',
-- 'Vulkan.Extensions.VK_GOOGLE_display_timing.PresentTimesInfoGOOGLE',
-- 'Vulkan.Extensions.VK_EXT_private_data.PrivateDataSlotCreateInfoEXT',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.ProtectedSubmitInfo',
-- 'Vulkan.Core10.Query.QueryPoolCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_performance_query.QueryPoolPerformanceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.QueryPoolPerformanceQueryCreateInfoINTEL',
-- 'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.QueueFamilyCheckpointPropertiesNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.QueueFamilyProperties2',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingPipelineInterfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.RayTracingShaderGroupCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingShaderGroupCreateInfoNV',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Vulkan.Core10.Pass.RenderPassCreateInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.RenderPassCreateInfo2',
-- 'Vulkan.Extensions.VK_EXT_fragment_density_map.RenderPassFragmentDensityMapCreateInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance2.RenderPassInputAttachmentAspectCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_multiview.RenderPassMultiviewCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.RenderPassSampleLocationsBeginInfoEXT',
-- 'Vulkan.Extensions.VK_QCOM_render_pass_transform.RenderPassTransformBeginInfoQCOM',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.SampleLocationsInfoEXT',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionImageFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionInfo',
-- 'Vulkan.Core10.QueueSemaphore.SemaphoreCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.SemaphoreGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.SemaphoreGetWin32HandleInfoKHR',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreSignalInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreTypeCreateInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo',
-- 'Vulkan.Core10.Shader.ShaderModuleCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.ShaderModuleValidationCacheCreateInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_shared_presentable_image.SharedPresentSurfaceCapabilitiesKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.SparseImageFormatProperties2',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.SparseImageMemoryRequirements2',
-- 'Vulkan.Extensions.VK_GGP_stream_descriptor_surface.StreamDescriptorSurfaceCreateInfoGGP',
-- 'Vulkan.Core10.Queue.SubmitInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassBeginInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDependency2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassDescription2',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.SubpassEndInfo',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.SurfaceCapabilities2EXT',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceCapabilities2KHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceCapabilitiesFullScreenExclusiveEXT',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.SurfaceFormat2KHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.SurfaceFullScreenExclusiveWin32InfoEXT',
-- 'Vulkan.Extensions.VK_KHR_surface_protected_capabilities.SurfaceProtectedCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_EXT_display_control.SwapchainCounterCreateInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.SwapchainDisplayNativeHdrCreateInfoAMD',
-- 'Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.TextureLODGatherFormatPropertiesAMD',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.TimelineSemaphoreSubmitInfo',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.ValidationCacheCreateInfoEXT',
-- 'Vulkan.Extensions.VK_EXT_validation_features.ValidationFeaturesEXT',
-- 'Vulkan.Extensions.VK_EXT_validation_flags.ValidationFlagsEXT',
-- 'Vulkan.Extensions.VK_NN_vi_surface.ViSurfaceCreateInfoNN',
-- 'Vulkan.Extensions.VK_KHR_wayland_surface.WaylandSurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
-- 'Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV',
-- 'Vulkan.Extensions.VK_KHR_win32_surface.Win32SurfaceCreateInfoKHR',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing.WriteDescriptorSetAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlockEXT',
-- 'Vulkan.Extensions.VK_KHR_xcb_surface.XcbSurfaceCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.XlibSurfaceCreateInfoKHR'
newtype StructureType = StructureType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_APPLICATION_INFO"
pattern STRUCTURE_TYPE_APPLICATION_INFO = StructureType 0
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
pattern STRUCTURE_TYPE_INSTANCE_CREATE_INFO = StructureType 1
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
pattern STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = StructureType 2
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"
pattern STRUCTURE_TYPE_DEVICE_CREATE_INFO = StructureType 3
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBMIT_INFO"
pattern STRUCTURE_TYPE_SUBMIT_INFO = StructureType 4
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = StructureType 5
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
pattern STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = StructureType 6
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_SPARSE_INFO"
pattern STRUCTURE_TYPE_BIND_SPARSE_INFO = StructureType 7
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_CREATE_INFO"
pattern STRUCTURE_TYPE_FENCE_CREATE_INFO = StructureType 8
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
pattern STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = StructureType 9
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EVENT_CREATE_INFO"
pattern STRUCTURE_TYPE_EVENT_CREATE_INFO = StructureType 10
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = StructureType 11
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO"
pattern STRUCTURE_TYPE_BUFFER_CREATE_INFO = StructureType 12
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
pattern STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = StructureType 13
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_CREATE_INFO = StructureType 14
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = StructureType 15
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
pattern STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = StructureType 16
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = StructureType 17
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = StructureType 18
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = StructureType 19
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = StructureType 20
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = StructureType 21
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = StructureType 22
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = StructureType 23
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = StructureType 24
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = StructureType 25
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = StructureType 26
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = StructureType 27
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
pattern STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = StructureType 28
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
pattern STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = StructureType 29
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = StructureType 30
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
pattern STRUCTURE_TYPE_SAMPLER_CREATE_INFO = StructureType 31
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = StructureType 32
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = StructureType 33
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = StructureType 34
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = StructureType 35
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
pattern STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = StructureType 36
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
pattern STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = StructureType 37
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = StructureType 38
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
pattern STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = StructureType 39
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = StructureType 40
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = StructureType 41
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = StructureType 42
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = StructureType 43
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = StructureType 44
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = StructureType 45
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_BARRIER"
pattern STRUCTURE_TYPE_MEMORY_BARRIER = StructureType 46
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
pattern STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = StructureType 47
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
pattern STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = StructureType 48
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT = StructureType 1000346000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT = StructureType 1000332001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT = StructureType 1000332000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV = StructureType 1000300001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV = StructureType 1000300000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT = StructureType 1000297000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT = StructureType 1000295002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT = StructureType 1000295001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT = StructureType 1000295000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR = StructureType 1000290000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT = StructureType 1000287002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT = StructureType 1000287001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT = StructureType 1000287000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT = StructureType 1000286001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT = StructureType 1000286000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM"
pattern STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM = StructureType 1000282001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM = StructureType 1000282000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT = StructureType 1000281001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT = StructureType 1000281000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV = StructureType 1000277007
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV"
pattern STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV = StructureType 1000277006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV"
pattern STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV = StructureType 1000277005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV = StructureType 1000277004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV"
pattern STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV = StructureType 1000277003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV = StructureType 1000277002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV = StructureType 1000277001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV = StructureType 1000277000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT = StructureType 1000276000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR"
pattern STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR = StructureType 1000269005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR"
pattern STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR = StructureType 1000269004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR = StructureType 1000269003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR = StructureType 1000269002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_INFO_KHR"
pattern STRUCTURE_TYPE_PIPELINE_INFO_KHR = StructureType 1000269001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR = StructureType 1000269000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR"
pattern STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR = StructureType 1000268000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT = StructureType 1000267000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT = StructureType 1000265000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT = StructureType 1000259002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT = StructureType 1000259001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT = StructureType 1000259000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT = StructureType 1000256000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT"
pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT = StructureType 1000255001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT = StructureType 1000255002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT"
pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT = StructureType 1000255000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT = StructureType 1000252000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT = StructureType 1000251000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV"
pattern STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV = StructureType 1000250002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV = StructureType 1000250001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV = StructureType 1000250000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV = StructureType 1000249002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV"
pattern STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV = StructureType 1000249001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV = StructureType 1000249000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT"
pattern STRUCTURE_TYPE_VALIDATION_FEATURES_EXT = StructureType 1000247000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT = StructureType 1000245000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT = StructureType 1000244002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT = StructureType 1000244000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV = StructureType 1000240000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR"
pattern STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR = StructureType 1000239000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT"
pattern STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT = StructureType 1000238001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT = StructureType 1000238000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT = StructureType 1000237000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD = StructureType 1000229000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD = StructureType 1000227000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT = StructureType 1000225002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT = StructureType 1000225001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT = StructureType 1000225000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT = StructureType 1000218002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT = StructureType 1000218001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT = StructureType 1000218000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT = StructureType 1000217000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA"
pattern STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA = StructureType 1000214000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD"
pattern STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD = StructureType 1000213001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD"
pattern STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD = StructureType 1000213000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT = StructureType 1000212000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL"
pattern STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL = StructureType 1000210005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL"
pattern STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL = StructureType 1000210004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL"
pattern STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL = StructureType 1000210003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL"
pattern STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL = StructureType 1000210002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL"
pattern STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL = StructureType 1000210001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL"
pattern STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL = StructureType 1000210000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL = StructureType 1000209000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV"
pattern STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV = StructureType 1000206001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV"
pattern STRUCTURE_TYPE_CHECKPOINT_DATA_NV = StructureType 1000206000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV = StructureType 1000205002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV = StructureType 1000205000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV = StructureType 1000204000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV = StructureType 1000203000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV = StructureType 1000202001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV = StructureType 1000202000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV = StructureType 1000201000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT = StructureType 1000192000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP"
pattern STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP = StructureType 1000191000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT = StructureType 1000190002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT = StructureType 1000190001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT = StructureType 1000190000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD"
pattern STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD = StructureType 1000189000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD = StructureType 1000185000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT"
pattern STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT = StructureType 1000184000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD"
pattern STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD = StructureType 1000183000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR = StructureType 1000181000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT = StructureType 1000178002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT = StructureType 1000178001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT"
pattern STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT = StructureType 1000178000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT = StructureType 1000174000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT = StructureType 1000170001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT = StructureType 1000170000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV = StructureType 1000166001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV = StructureType 1000166000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV = StructureType 1000165012
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV = StructureType 1000165011
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV = StructureType 1000165009
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV = StructureType 1000165008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV"
pattern STRUCTURE_TYPE_GEOMETRY_AABB_NV = StructureType 1000165005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV"
pattern STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV = StructureType 1000165004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GEOMETRY_NV"
pattern STRUCTURE_TYPE_GEOMETRY_NV = StructureType 1000165003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV = StructureType 1000165001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV = StructureType 1000165000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV = StructureType 1000164005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV = StructureType 1000164002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV = StructureType 1000164001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV = StructureType 1000164000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT = StructureType 1000160001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT = StructureType 1000160000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT = StructureType 1000158005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT = StructureType 1000158004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT = StructureType 1000158003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT = StructureType 1000158002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT = StructureType 1000158001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT"
pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT = StructureType 1000158000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV = StructureType 1000154001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV = StructureType 1000154000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV = StructureType 1000152000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR = StructureType 1000150018
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR = StructureType 1000150017
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR = StructureType 1000150016
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR = StructureType 1000150015
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR = StructureType 1000150014
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR = StructureType 1000150013
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR"
pattern STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR = StructureType 1000150012
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR = StructureType 1000150011
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR"
pattern STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR = StructureType 1000150010
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR = StructureType 1000150009
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR = StructureType 1000150008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR = StructureType 1000150006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR = StructureType 1000150005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR = StructureType 1000150004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR = StructureType 1000150003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR = StructureType 1000150002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR = StructureType 1000150001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR"
pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR = StructureType 1000150000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR"
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR = StructureType 1000165007
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR = StructureType 1000165006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV = StructureType 1000149000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT = StructureType 1000148002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT = StructureType 1000148001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT = StructureType 1000148000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT = StructureType 1000143004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT = StructureType 1000143003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT = StructureType 1000143002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT"
pattern STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT = StructureType 1000143001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT"
pattern STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT = StructureType 1000143000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT = StructureType 1000138003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT"
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT = StructureType 1000138002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT = StructureType 1000138001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT = StructureType 1000138000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID"
pattern STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID = StructureType 1000129005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
pattern STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = StructureType 1000129004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
pattern STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID = StructureType 1000129003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID"
pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID = StructureType 1000129002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID"
pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID = StructureType 1000129001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID"
pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID = StructureType 1000129000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT = StructureType 1000128004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT = StructureType 1000128003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT"
pattern STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT = StructureType 1000128002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT = StructureType 1000128001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT = StructureType 1000128000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK"
pattern STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK = StructureType 1000123000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK"
pattern STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK = StructureType 1000122000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR"
pattern STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR = StructureType 1000121004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR"
pattern STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR = StructureType 1000121003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR = StructureType 1000121002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR = StructureType 1000121001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR"
pattern STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR = StructureType 1000121000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR"
pattern STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR = StructureType 1000119002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR = StructureType 1000119001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR = StructureType 1000119000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR"
pattern STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR = StructureType 1000116006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR"
pattern STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR = StructureType 1000116005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR"
pattern STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR = StructureType 1000116004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR = StructureType 1000116003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR = StructureType 1000116002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR = StructureType 1000116001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR = StructureType 1000116000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR"
pattern STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR = StructureType 1000115001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR"
pattern STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR = StructureType 1000115000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR = StructureType 1000114002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR = StructureType 1000114001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR = StructureType 1000114000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR"
pattern STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR = StructureType 1000111000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_HDR_METADATA_EXT"
pattern STRUCTURE_TYPE_HDR_METADATA_EXT = StructureType 1000105000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT = StructureType 1000102001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT = StructureType 1000102000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT = StructureType 1000101001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT = StructureType 1000101000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT = StructureType 1000099001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT = StructureType 1000099000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV = StructureType 1000098000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX = StructureType 1000097000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
pattern STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE = StructureType 1000092000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT = StructureType 1000091003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT"
pattern STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT = StructureType 1000091002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT"
pattern STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT = StructureType 1000091001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT"
pattern STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT = StructureType 1000091000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT"
pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT = StructureType 1000090000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV = StructureType 1000087000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
pattern STRUCTURE_TYPE_PRESENT_REGIONS_KHR = StructureType 1000084000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT"
pattern STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT = StructureType 1000081002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT = StructureType 1000081001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT = StructureType 1000081000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR = StructureType 1000080000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR = StructureType 1000079001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR = StructureType 1000079000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR = StructureType 1000078003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR"
pattern STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR = StructureType 1000078002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = StructureType 1000078001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR = StructureType 1000078000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR"
pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR = StructureType 1000075000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR = StructureType 1000074002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR = StructureType 1000074001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR"
pattern STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR = StructureType 1000074000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR = StructureType 1000073003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR = StructureType 1000073002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR = StructureType 1000073001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR = StructureType 1000073000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT = StructureType 1000067001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT"
pattern STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT = StructureType 1000067000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT = StructureType 1000066000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN"
pattern STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN = StructureType 1000062000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT"
pattern STRUCTURE_TYPE_VALIDATION_FLAGS_EXT = StructureType 1000061000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR = StructureType 1000060012
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR = StructureType 1000060011
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR"
pattern STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR = StructureType 1000060010
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR = StructureType 1000060009
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR = StructureType 1000060008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR = StructureType 1000060007
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV"
pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV = StructureType 1000058000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV = StructureType 1000057001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV = StructureType 1000057000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV = StructureType 1000056001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV = StructureType 1000056000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV = StructureType 1000050000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP"
pattern STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP = StructureType 1000049000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD"
pattern STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD = StructureType 1000041000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX"
pattern STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX = StructureType 1000030001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX"
pattern STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX = StructureType 1000030000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT = StructureType 1000028002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT = StructureType 1000028001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT = StructureType 1000028000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV = StructureType 1000026002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV = StructureType 1000026001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV = StructureType 1000026000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT = StructureType 1000022002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT = StructureType 1000022001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT = StructureType 1000022000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD = StructureType 1000018000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT"
pattern STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT = StructureType 1000011000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR = StructureType 1000009000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR = StructureType 1000008000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR = StructureType 1000006000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR = StructureType 1000005000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR = StructureType 1000004000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR"
pattern STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR = StructureType 1000003000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR = StructureType 1000002001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR = StructureType 1000002000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PRESENT_INFO_KHR"
pattern STRUCTURE_TYPE_PRESENT_INFO_KHR = StructureType 1000001001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR = StructureType 1000001000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO"
pattern STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO = StructureType 1000257004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO = StructureType 1000257003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO"
pattern STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO = StructureType 1000257002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO"
pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO = StructureType 1000244001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES = StructureType 1000257000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO"
pattern STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO = StructureType 1000207005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO"
pattern STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO = StructureType 1000207004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO"
pattern STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO = StructureType 1000207003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO"
pattern STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO = StructureType 1000207002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES = StructureType 1000207001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES = StructureType 1000207000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES = StructureType 1000261000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT"
pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT = StructureType 1000241002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT"
pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT = StructureType 1000241001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES = StructureType 1000241000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES = StructureType 1000175000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES = StructureType 1000253000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO = StructureType 1000108003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO"
pattern STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO = StructureType 1000108002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO"
pattern STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO = StructureType 1000108001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES = StructureType 1000108000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES = StructureType 1000211000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO"
pattern STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO = StructureType 1000130001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES = StructureType 1000130000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO = StructureType 1000246000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES = StructureType 1000221000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE"
pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE = StructureType 1000199001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES = StructureType 1000199000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT = StructureType 1000161004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO = StructureType 1000161003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES = StructureType 1000161002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES = StructureType 1000161001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO = StructureType 1000161000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES = StructureType 1000197000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES = StructureType 1000082000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES = StructureType 1000180000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES = StructureType 1000196000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES = StructureType 1000177000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_END_INFO"
pattern STRUCTURE_TYPE_SUBPASS_END_INFO = StructureType 1000109006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO"
pattern STRUCTURE_TYPE_SUBPASS_BEGIN_INFO = StructureType 1000109005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2"
pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2 = StructureType 1000109004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2"
pattern STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2 = StructureType 1000109003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2"
pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2 = StructureType 1000109002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2"
pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2 = StructureType 1000109001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2"
pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2 = StructureType 1000109000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO = StructureType 1000147000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES = StructureType 52
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES = StructureType 51
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES = StructureType 50
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES = StructureType 49
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES = StructureType 1000063000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT = StructureType 1000168001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES = StructureType 1000168000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES"
pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES = StructureType 1000076001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO = StructureType 1000076000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO"
pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO = StructureType 1000077000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO"
pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO = StructureType 1000113000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES"
pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES = StructureType 1000112001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO = StructureType 1000112000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO = StructureType 1000072002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO = StructureType 1000072001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO"
pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO = StructureType 1000072000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES = StructureType 1000071004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES"
pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES = StructureType 1000071003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO = StructureType 1000071002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES"
pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES = StructureType 1000071001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO = StructureType 1000071000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO = StructureType 1000085000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES = StructureType 1000156005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES = StructureType 1000156004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
pattern STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO = StructureType 1000156003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
pattern STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO = StructureType 1000156002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO = StructureType 1000156001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO = StructureType 1000156000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2"
pattern STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 = StructureType 1000145003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES = StructureType 1000145002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES = StructureType 1000145001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO"
pattern STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO = StructureType 1000145000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES = StructureType 1000120000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES = StructureType 1000053002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES = StructureType 1000053001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO = StructureType 1000053000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO = StructureType 1000117003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO = StructureType 1000117002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO = StructureType 1000117001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES = StructureType 1000117000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 = StructureType 1000059008
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 = StructureType 1000059007
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 = StructureType 1000059006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 = StructureType 1000059005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 = StructureType 1000059004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 = StructureType 1000059003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2 = StructureType 1000059002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 = StructureType 1000059001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 = StructureType 1000059000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2"
pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 = StructureType 1000146004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2"
pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 = StructureType 1000146003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2"
pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 = StructureType 1000146002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2"
pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 = StructureType 1000146001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2"
pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 = StructureType 1000146000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO"
pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO = StructureType 1000070001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES = StructureType 1000070000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO = StructureType 1000060014
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO = StructureType 1000060013
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO"
pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO = StructureType 1000060006
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO"
pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO = StructureType 1000060005
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO"
pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO = StructureType 1000060004
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO"
pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO = StructureType 1000060003
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO"
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO = StructureType 1000060000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO = StructureType 1000127001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS = StructureType 1000127000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES = StructureType 1000083000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO = StructureType 1000157001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO = StructureType 1000157000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES = StructureType 1000094000
{-# complete STRUCTURE_TYPE_APPLICATION_INFO,
             STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
             STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
             STRUCTURE_TYPE_DEVICE_CREATE_INFO,
             STRUCTURE_TYPE_SUBMIT_INFO,
             STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
             STRUCTURE_TYPE_MAPPED_MEMORY_RANGE,
             STRUCTURE_TYPE_BIND_SPARSE_INFO,
             STRUCTURE_TYPE_FENCE_CREATE_INFO,
             STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO,
             STRUCTURE_TYPE_EVENT_CREATE_INFO,
             STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO,
             STRUCTURE_TYPE_BUFFER_CREATE_INFO,
             STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO,
             STRUCTURE_TYPE_IMAGE_CREATE_INFO,
             STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO,
             STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO,
             STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO,
             STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO,
             STRUCTURE_TYPE_SAMPLER_CREATE_INFO,
             STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO,
             STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO,
             STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO,
             STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET,
             STRUCTURE_TYPE_COPY_DESCRIPTOR_SET,
             STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO,
             STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO,
             STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
             STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
             STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO,
             STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
             STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO,
             STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
             STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER,
             STRUCTURE_TYPE_MEMORY_BARRIER,
             STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO,
             STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO,
             STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT,
             STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT,
             STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT,
             STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT,
             STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT,
             STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT,
             STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM,
             STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV,
             STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV,
             STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV,
             STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV,
             STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV,
             STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV,
             STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT,
             STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR,
             STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR,
             STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR,
             STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR,
             STRUCTURE_TYPE_PIPELINE_INFO_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR,
             STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT,
             STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT,
             STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT,
             STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT,
             STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT,
             STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV,
             STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV,
             STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV,
             STRUCTURE_TYPE_VALIDATION_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT,
             STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV,
             STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR,
             STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT,
             STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT,
             STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT,
             STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA,
             STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD,
             STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT,
             STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL,
             STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL,
             STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL,
             STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL,
             STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL,
             STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL,
             STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV,
             STRUCTURE_TYPE_CHECKPOINT_DATA_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV,
             STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV,
             STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT,
             STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT,
             STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD,
             STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT,
             STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT,
             STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT,
             STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT,
             STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT,
             STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT,
             STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV,
             STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV,
             STRUCTURE_TYPE_GEOMETRY_AABB_NV,
             STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV,
             STRUCTURE_TYPE_GEOMETRY_NV,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV,
             STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV,
             STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT,
             STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT,
             STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT,
             STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT,
             STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV,
             STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR,
             STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR,
             STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR,
             STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR,
             STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR,
             STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR,
             STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR,
             STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR,
             STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT,
             STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT,
             STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT,
             STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT,
             STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT,
             STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT,
             STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID,
             STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID,
             STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID,
             STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID,
             STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID,
             STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID,
             STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT,
             STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT,
             STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT,
             STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT,
             STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT,
             STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK,
             STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK,
             STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR,
             STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR,
             STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR,
             STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR,
             STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR,
             STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR,
             STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR,
             STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR,
             STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR,
             STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR,
             STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR,
             STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR,
             STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR,
             STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR,
             STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR,
             STRUCTURE_TYPE_HDR_METADATA_EXT,
             STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT,
             STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT,
             STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT,
             STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX,
             STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE,
             STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT,
             STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT,
             STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT,
             STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT,
             STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT,
             STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PRESENT_REGIONS_KHR,
             STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT,
             STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR,
             STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR,
             STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR,
             STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR,
             STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR,
             STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR,
             STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR,
             STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR,
             STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR,
             STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT,
             STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT,
             STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN,
             STRUCTURE_TYPE_VALIDATION_FLAGS_EXT,
             STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR,
             STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR,
             STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR,
             STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR,
             STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR,
             STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR,
             STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV,
             STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV,
             STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV,
             STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV,
             STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV,
             STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP,
             STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD,
             STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX,
             STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX,
             STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT,
             STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV,
             STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV,
             STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV,
             STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT,
             STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT,
             STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT,
             STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD,
             STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT,
             STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR,
             STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR,
             STRUCTURE_TYPE_PRESENT_INFO_KHR,
             STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR,
             STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO,
             STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO,
             STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO,
             STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES,
             STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO,
             STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO,
             STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO,
             STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES,
             STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT,
             STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES,
             STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO,
             STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO,
             STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES,
             STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES,
             STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES,
             STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES,
             STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT,
             STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES,
             STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES,
             STRUCTURE_TYPE_SUBPASS_END_INFO,
             STRUCTURE_TYPE_SUBPASS_BEGIN_INFO,
             STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2,
             STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2,
             STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2,
             STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2,
             STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2,
             STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES,
             STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES,
             STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO,
             STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO,
             STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO,
             STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO,
             STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO,
             STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO,
             STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES,
             STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO,
             STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO,
             STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO,
             STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES,
             STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO,
             STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO,
             STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO,
             STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO,
             STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES,
             STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES,
             STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO,
             STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO,
             STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO,
             STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2,
             STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2,
             STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2,
             STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2,
             STRUCTURE_TYPE_FORMAT_PROPERTIES_2,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2,
             STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2,
             STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2,
             STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2,
             STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2,
             STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2,
             STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES,
             STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO,
             STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO,
             STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO,
             STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO,
             STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO,
             STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO,
             STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO,
             STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO,
             STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES,
             STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO,
             STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO,
             STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES :: StructureType #-}

instance Show StructureType where
  showsPrec p = \case
    STRUCTURE_TYPE_APPLICATION_INFO -> showString "STRUCTURE_TYPE_APPLICATION_INFO"
    STRUCTURE_TYPE_INSTANCE_CREATE_INFO -> showString "STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
    STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO -> showString "STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
    STRUCTURE_TYPE_DEVICE_CREATE_INFO -> showString "STRUCTURE_TYPE_DEVICE_CREATE_INFO"
    STRUCTURE_TYPE_SUBMIT_INFO -> showString "STRUCTURE_TYPE_SUBMIT_INFO"
    STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
    STRUCTURE_TYPE_MAPPED_MEMORY_RANGE -> showString "STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
    STRUCTURE_TYPE_BIND_SPARSE_INFO -> showString "STRUCTURE_TYPE_BIND_SPARSE_INFO"
    STRUCTURE_TYPE_FENCE_CREATE_INFO -> showString "STRUCTURE_TYPE_FENCE_CREATE_INFO"
    STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO -> showString "STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
    STRUCTURE_TYPE_EVENT_CREATE_INFO -> showString "STRUCTURE_TYPE_EVENT_CREATE_INFO"
    STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO -> showString "STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
    STRUCTURE_TYPE_BUFFER_CREATE_INFO -> showString "STRUCTURE_TYPE_BUFFER_CREATE_INFO"
    STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO -> showString "STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
    STRUCTURE_TYPE_IMAGE_CREATE_INFO -> showString "STRUCTURE_TYPE_IMAGE_CREATE_INFO"
    STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO -> showString "STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
    STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO -> showString "STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
    STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO -> showString "STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
    STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO -> showString "STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
    STRUCTURE_TYPE_SAMPLER_CREATE_INFO -> showString "STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
    STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO -> showString "STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
    STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO -> showString "STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
    STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
    STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET -> showString "STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
    STRUCTURE_TYPE_COPY_DESCRIPTOR_SET -> showString "STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
    STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO -> showString "STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
    STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO -> showString "STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
    STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO -> showString "STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
    STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
    STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO -> showString "STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
    STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO -> showString "STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
    STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO -> showString "STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
    STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER -> showString "STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
    STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER -> showString "STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
    STRUCTURE_TYPE_MEMORY_BARRIER -> showString "STRUCTURE_TYPE_MEMORY_BARRIER"
    STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO -> showString "STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
    STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO -> showString "STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
    STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT"
    STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT"
    STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT"
    STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT"
    STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT"
    STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT"
    STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM -> showString "STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM"
    STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM -> showString "STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV"
    STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV -> showString "STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV"
    STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV -> showString "STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV"
    STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV"
    STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV -> showString "STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV"
    STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV"
    STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT"
    STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR -> showString "STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR"
    STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR -> showString "STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR"
    STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR -> showString "STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR"
    STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR -> showString "STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR"
    STRUCTURE_TYPE_PIPELINE_INFO_KHR -> showString "STRUCTURE_TYPE_PIPELINE_INFO_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR"
    STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR -> showString "STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT"
    STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT"
    STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT -> showString "STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT"
    STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT -> showString "STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT"
    STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT -> showString "STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT"
    STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV -> showString "STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV"
    STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV"
    STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV -> showString "STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV"
    STRUCTURE_TYPE_VALIDATION_FEATURES_EXT -> showString "STRUCTURE_TYPE_VALIDATION_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT"
    STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV"
    STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR -> showString "STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR"
    STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT -> showString "STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT"
    STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT"
    STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT"
    STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA -> showString "STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA"
    STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD -> showString "STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD"
    STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD -> showString "STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT"
    STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL -> showString "STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL"
    STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL -> showString "STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL"
    STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL -> showString "STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL"
    STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL -> showString "STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL"
    STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL -> showString "STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL"
    STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL -> showString "STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL"
    STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV -> showString "STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV"
    STRUCTURE_TYPE_CHECKPOINT_DATA_NV -> showString "STRUCTURE_TYPE_CHECKPOINT_DATA_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV"
    STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV"
    STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP -> showString "STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT"
    STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT"
    STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD -> showString "STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD"
    STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT -> showString "STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT"
    STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD -> showString "STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT"
    STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT"
    STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT -> showString "STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT"
    STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT"
    STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT"
    STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV"
    STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV"
    STRUCTURE_TYPE_GEOMETRY_AABB_NV -> showString "STRUCTURE_TYPE_GEOMETRY_AABB_NV"
    STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV -> showString "STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV"
    STRUCTURE_TYPE_GEOMETRY_NV -> showString "STRUCTURE_TYPE_GEOMETRY_NV"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV"
    STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV"
    STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
    STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT"
    STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT"
    STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT"
    STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT -> showString "STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV"
    STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR"
    STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR"
    STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR -> showString "STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR"
    STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR -> showString "STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR"
    STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR -> showString "STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR"
    STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR -> showString "STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR"
    STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR -> showString "STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR"
    STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR -> showString "STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR"
    STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT"
    STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT"
    STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT -> showString "STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT"
    STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT -> showString "STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT"
    STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT"
    STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT -> showString "STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT"
    STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID -> showString "STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID"
    STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> showString "STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
    STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> showString "STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
    STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID -> showString "STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID"
    STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID -> showString "STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID"
    STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID -> showString "STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID"
    STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
    STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT -> showString "STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
    STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT -> showString "STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT"
    STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT"
    STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
    STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK -> showString "STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK"
    STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK -> showString "STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK"
    STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR -> showString "STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR"
    STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR -> showString "STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR"
    STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR -> showString "STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR"
    STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR -> showString "STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR"
    STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR -> showString "STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR"
    STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR -> showString "STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR"
    STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR -> showString "STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR"
    STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR -> showString "STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR"
    STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR -> showString "STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR"
    STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR -> showString "STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR"
    STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR -> showString "STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR"
    STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR"
    STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR -> showString "STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR"
    STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR -> showString "STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR"
    STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR -> showString "STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR"
    STRUCTURE_TYPE_HDR_METADATA_EXT -> showString "STRUCTURE_TYPE_HDR_METADATA_EXT"
    STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT"
    STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
    STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT"
    STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX"
    STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE -> showString "STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
    STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT"
    STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT -> showString "STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT"
    STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT -> showString "STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT"
    STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT -> showString "STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT"
    STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT -> showString "STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT"
    STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PRESENT_REGIONS_KHR -> showString "STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
    STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT -> showString "STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT"
    STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT -> showString "STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
    STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR -> showString "STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
    STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR -> showString "STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
    STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR -> showString "STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR"
    STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR -> showString "STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR"
    STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR -> showString "STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR"
    STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR -> showString "STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR"
    STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR -> showString "STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR"
    STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR -> showString "STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR"
    STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> showString "STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT"
    STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT -> showString "STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT"
    STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN -> showString "STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN"
    STRUCTURE_TYPE_VALIDATION_FLAGS_EXT -> showString "STRUCTURE_TYPE_VALIDATION_FLAGS_EXT"
    STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR"
    STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR -> showString "STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR"
    STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR -> showString "STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR"
    STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR -> showString "STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR"
    STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR"
    STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR -> showString "STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR"
    STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV -> showString "STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV"
    STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV -> showString "STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
    STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV -> showString "STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
    STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV -> showString "STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV"
    STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV"
    STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP -> showString "STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP"
    STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD -> showString "STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD"
    STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX -> showString "STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX"
    STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX -> showString "STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX"
    STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT"
    STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV -> showString "STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
    STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
    STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV -> showString "STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
    STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT"
    STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT"
    STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT"
    STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD -> showString "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD"
    STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT -> showString "STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT"
    STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR -> showString "STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR"
    STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
    STRUCTURE_TYPE_PRESENT_INFO_KHR -> showString "STRUCTURE_TYPE_PRESENT_INFO_KHR"
    STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR -> showString "STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR"
    STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO -> showString "STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO"
    STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO"
    STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO -> showString "STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO"
    STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO -> showString "STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES"
    STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO -> showString "STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO"
    STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO -> showString "STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO"
    STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO -> showString "STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO"
    STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO -> showString "STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES"
    STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT -> showString "STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT"
    STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT -> showString "STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES"
    STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO -> showString "STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO"
    STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO -> showString "STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO"
    STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO -> showString "STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES"
    STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO -> showString "STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES"
    STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO -> showString "STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES"
    STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE -> showString "STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES"
    STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT -> showString "STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT"
    STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES"
    STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO -> showString "STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES"
    STRUCTURE_TYPE_SUBPASS_END_INFO -> showString "STRUCTURE_TYPE_SUBPASS_END_INFO"
    STRUCTURE_TYPE_SUBPASS_BEGIN_INFO -> showString "STRUCTURE_TYPE_SUBPASS_BEGIN_INFO"
    STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2 -> showString "STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2"
    STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2 -> showString "STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2"
    STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2 -> showString "STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2"
    STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2 -> showString "STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2"
    STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2 -> showString "STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2"
    STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO -> showString "STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES"
    STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT -> showString "STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES"
    STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES -> showString "STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO"
    STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO -> showString "STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO"
    STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO -> showString "STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO"
    STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES -> showString "STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO"
    STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO"
    STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO -> showString "STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO"
    STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO -> showString "STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES"
    STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES -> showString "STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO"
    STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES -> showString "STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO"
    STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO -> showString "STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO"
    STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES -> showString "STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
    STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO -> showString "STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
    STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO -> showString "STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
    STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO -> showString "STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
    STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO -> showString "STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
    STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 -> showString "STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES"
    STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO -> showString "STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
    STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO -> showString "STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
    STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO -> showString "STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO"
    STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO -> showString "STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO"
    STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO -> showString "STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
    STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 -> showString "STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
    STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 -> showString "STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
    STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 -> showString "STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
    STRUCTURE_TYPE_FORMAT_PROPERTIES_2 -> showString "STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
    STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 -> showString "STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2"
    STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 -> showString "STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2"
    STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 -> showString "STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2"
    STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 -> showString "STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2"
    STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 -> showString "STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2"
    STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO -> showString "STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES"
    STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO -> showString "STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO"
    STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO -> showString "STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO"
    STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO -> showString "STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO"
    STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO -> showString "STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO"
    STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO -> showString "STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO"
    STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO -> showString "STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO"
    STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO -> showString "STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO"
    STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO -> showString "STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
    STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS -> showString "STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES"
    STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO -> showString "STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
    STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO -> showString "STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
    STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES -> showString "STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES"
    StructureType x -> showParen (p >= 11) (showString "StructureType " . showsPrec 11 x)

instance Read StructureType where
  readPrec = parens (choose [("STRUCTURE_TYPE_APPLICATION_INFO", pure STRUCTURE_TYPE_APPLICATION_INFO)
                            , ("STRUCTURE_TYPE_INSTANCE_CREATE_INFO", pure STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO", pure STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_CREATE_INFO", pure STRUCTURE_TYPE_DEVICE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_SUBMIT_INFO", pure STRUCTURE_TYPE_SUBMIT_INFO)
                            , ("STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO", pure STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_MAPPED_MEMORY_RANGE", pure STRUCTURE_TYPE_MAPPED_MEMORY_RANGE)
                            , ("STRUCTURE_TYPE_BIND_SPARSE_INFO", pure STRUCTURE_TYPE_BIND_SPARSE_INFO)
                            , ("STRUCTURE_TYPE_FENCE_CREATE_INFO", pure STRUCTURE_TYPE_FENCE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO", pure STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_EVENT_CREATE_INFO", pure STRUCTURE_TYPE_EVENT_CREATE_INFO)
                            , ("STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO", pure STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
                            , ("STRUCTURE_TYPE_BUFFER_CREATE_INFO", pure STRUCTURE_TYPE_BUFFER_CREATE_INFO)
                            , ("STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO", pure STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
                            , ("STRUCTURE_TYPE_IMAGE_CREATE_INFO", pure STRUCTURE_TYPE_IMAGE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO", pure STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
                            , ("STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO", pure STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO", pure STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO", pure STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
                            , ("STRUCTURE_TYPE_SAMPLER_CREATE_INFO", pure STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO", pure STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO", pure STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO", pure STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET", pure STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
                            , ("STRUCTURE_TYPE_COPY_DESCRIPTOR_SET", pure STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
                            , ("STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO", pure STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
                            , ("STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO", pure STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
                            , ("STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO", pure STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
                            , ("STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO", pure STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO", pure STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
                            , ("STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO", pure STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                            , ("STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO", pure STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
                            , ("STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER", pure STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
                            , ("STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER", pure STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                            , ("STRUCTURE_TYPE_MEMORY_BARRIER", pure STRUCTURE_TYPE_MEMORY_BARRIER)
                            , ("STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO", pure STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO", pure STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_DIRECTFB_SURFACE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_2_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV", pure STRUCTURE_TYPE_DEVICE_DIAGNOSTICS_CONFIG_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DIAGNOSTICS_CONFIG_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_CREATION_CACHE_CONTROL_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PRIVATE_DATA_SLOT_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT", pure STRUCTURE_TYPE_DEVICE_PRIVATE_DATA_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PRIVATE_DATA_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR", pure STRUCTURE_TYPE_PIPELINE_LIBRARY_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_CUSTOM_BORDER_COLOR_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT", pure STRUCTURE_TYPE_SAMPLER_CUSTOM_BORDER_COLOR_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_ROBUSTNESS_2_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM", pure STRUCTURE_TYPE_RENDER_PASS_TRANSFORM_BEGIN_INFO_QCOM)
                            , ("STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM", pure STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_RENDER_PASS_TRANSFORM_INFO_QCOM)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXEL_BUFFER_ALIGNMENT_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_FEATURES_NV)
                            , ("STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV", pure STRUCTURE_TYPE_GENERATED_COMMANDS_MEMORY_REQUIREMENTS_INFO_NV)
                            , ("STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV", pure STRUCTURE_TYPE_GENERATED_COMMANDS_INFO_NV)
                            , ("STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV", pure STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV", pure STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_TOKEN_NV)
                            , ("STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV", pure STRUCTURE_TYPE_GRAPHICS_PIPELINE_SHADER_GROUPS_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV", pure STRUCTURE_TYPE_GRAPHICS_SHADER_GROUP_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DEVICE_GENERATED_COMMANDS_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DEMOTE_TO_HELPER_INVOCATION_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR", pure STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INTERNAL_REPRESENTATION_KHR)
                            , ("STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR", pure STRUCTURE_TYPE_PIPELINE_EXECUTABLE_STATISTIC_KHR)
                            , ("STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR", pure STRUCTURE_TYPE_PIPELINE_EXECUTABLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR", pure STRUCTURE_TYPE_PIPELINE_EXECUTABLE_PROPERTIES_KHR)
                            , ("STRUCTURE_TYPE_PIPELINE_INFO_KHR", pure STRUCTURE_TYPE_PIPELINE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PIPELINE_EXECUTABLE_PROPERTIES_FEATURES_KHR)
                            , ("STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR", pure STRUCTURE_TYPE_DEFERRED_OPERATION_INFO_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTENDED_DYNAMIC_STATE_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_INDEX_TYPE_UINT8_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_RASTERIZATION_LINE_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_LINE_RASTERIZATION_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT", pure STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT)
                            , ("STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT", pure STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT)
                            , ("STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT", pure STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_INTERLOCK_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV", pure STRUCTURE_TYPE_FRAMEBUFFER_MIXED_SAMPLES_COMBINATION_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_COVERAGE_REDUCTION_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_COVERAGE_REDUCTION_MODE_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV", pure STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV)
                            , ("STRUCTURE_TYPE_VALIDATION_FEATURES_EXT", pure STRUCTURE_TYPE_VALIDATION_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TOOL_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT", pure STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV)
                            , ("STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR", pure STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR)
                            , ("STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT", pure STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_COHERENT_MEMORY_FEATURES_AMD)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_2_AMD)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_REQUIRED_SUBGROUP_SIZE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_SIZE_CONTROL_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT", pure STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA", pure STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA)
                            , ("STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD", pure STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD)
                            , ("STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD", pure STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL", pure STRUCTURE_TYPE_PERFORMANCE_CONFIGURATION_ACQUIRE_INFO_INTEL)
                            , ("STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL", pure STRUCTURE_TYPE_PERFORMANCE_OVERRIDE_INFO_INTEL)
                            , ("STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL", pure STRUCTURE_TYPE_PERFORMANCE_STREAM_MARKER_INFO_INTEL)
                            , ("STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL", pure STRUCTURE_TYPE_PERFORMANCE_MARKER_INFO_INTEL)
                            , ("STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL", pure STRUCTURE_TYPE_INITIALIZE_PERFORMANCE_API_INFO_INTEL)
                            , ("STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL", pure STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_QUERY_CREATE_INFO_INTEL)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_INTEGER_FUNCTIONS_2_FEATURES_INTEL)
                            , ("STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV", pure STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_CHECKPOINT_DATA_NV", pure STRUCTURE_TYPE_CHECKPOINT_DATA_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP", pure STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD", pure STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD)
                            , ("STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT", pure STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD", pure STRUCTURE_TYPE_PIPELINE_COMPILER_CONTROL_CREATE_INFO_AMD)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CLOCK_FEATURES_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT", pure STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT", pure STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT", pure STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT", pure STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV)
                            , ("STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV", pure STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV)
                            , ("STRUCTURE_TYPE_GEOMETRY_AABB_NV", pure STRUCTURE_TYPE_GEOMETRY_AABB_NV)
                            , ("STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV", pure STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV)
                            , ("STRUCTURE_TYPE_GEOMETRY_NV", pure STRUCTURE_TYPE_GEOMETRY_NV)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV", pure STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT", pure STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT", pure STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT", pure STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT)
                            , ("STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT", pure STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT", pure STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_PROPERTIES_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SM_BUILTINS_FEATURES_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_RAY_TRACING_PIPELINE_INTERFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR", pure STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_FEATURES_KHR)
                            , ("STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR", pure STRUCTURE_TYPE_COPY_MEMORY_TO_ACCELERATION_STRUCTURE_INFO_KHR)
                            , ("STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR", pure STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_TO_MEMORY_INFO_KHR)
                            , ("STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR", pure STRUCTURE_TYPE_COPY_ACCELERATION_STRUCTURE_INFO_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_VERSION_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_TRIANGLES_DATA_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_INSTANCES_DATA_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_GEOMETRY_AABBS_DATA_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_DEVICE_ADDRESS_INFO_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_GEOMETRY_TYPE_INFO_KHR)
                            , ("STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR", pure STRUCTURE_TYPE_ACCELERATION_STRUCTURE_BUILD_GEOMETRY_INFO_KHR)
                            , ("STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR", pure STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_KHR)
                            , ("STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR", pure STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_KHR)
                            , ("STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT", pure STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT", pure STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT)
                            , ("STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT", pure STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT", pure STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT", pure STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID", pure STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID)
                            , ("STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID", pure STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
                            , ("STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID", pure STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID)
                            , ("STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID", pure STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID)
                            , ("STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID", pure STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID)
                            , ("STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID", pure STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID)
                            , ("STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT", pure STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT)
                            , ("STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT", pure STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT)
                            , ("STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT)
                            , ("STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK", pure STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK)
                            , ("STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK", pure STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK)
                            , ("STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR", pure STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR", pure STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR", pure STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR", pure STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR", pure STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR)
                            , ("STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR", pure STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR)
                            , ("STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR", pure STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR)
                            , ("STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR", pure STRUCTURE_TYPE_PERFORMANCE_COUNTER_DESCRIPTION_KHR)
                            , ("STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR", pure STRUCTURE_TYPE_PERFORMANCE_COUNTER_KHR)
                            , ("STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR", pure STRUCTURE_TYPE_ACQUIRE_PROFILING_LOCK_INFO_KHR)
                            , ("STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR", pure STRUCTURE_TYPE_PERFORMANCE_QUERY_SUBMIT_INFO_KHR)
                            , ("STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_QUERY_POOL_PERFORMANCE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_PROPERTIES_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PERFORMANCE_QUERY_FEATURES_KHR)
                            , ("STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR", pure STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR)
                            , ("STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR", pure STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR)
                            , ("STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR", pure STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR)
                            , ("STRUCTURE_TYPE_HDR_METADATA_EXT", pure STRUCTURE_TYPE_HDR_METADATA_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX)
                            , ("STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE", pure STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE)
                            , ("STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT", pure STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT", pure STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT", pure STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT)
                            , ("STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT", pure STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT)
                            , ("STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT", pure STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV", pure STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PRESENT_REGIONS_KHR", pure STRUCTURE_TYPE_PRESENT_REGIONS_KHR)
                            , ("STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT", pure STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT", pure STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR)
                            , ("STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR", pure STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR)
                            , ("STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR", pure STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR)
                            , ("STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR", pure STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR)
                            , ("STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR", pure STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR)
                            , ("STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR", pure STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR)
                            , ("STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR", pure STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR)
                            , ("STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR", pure STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR)
                            , ("STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR", pure STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR)
                            , ("STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR", pure STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT", pure STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TEXTURE_COMPRESSION_ASTC_HDR_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN", pure STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN)
                            , ("STRUCTURE_TYPE_VALIDATION_FLAGS_EXT", pure STRUCTURE_TYPE_VALIDATION_FLAGS_EXT)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR", pure STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR", pure STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR)
                            , ("STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR", pure STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR)
                            , ("STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR", pure STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR)
                            , ("STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR", pure STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR", pure STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR)
                            , ("STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV", pure STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV)
                            , ("STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV", pure STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV)
                            , ("STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV", pure STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV)
                            , ("STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV", pure STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV)
                            , ("STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV", pure STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV)
                            , ("STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP", pure STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP)
                            , ("STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD", pure STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD)
                            , ("STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX", pure STRUCTURE_TYPE_IMAGE_VIEW_ADDRESS_PROPERTIES_NVX)
                            , ("STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX", pure STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX)
                            , ("STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT", pure STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT)
                            , ("STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV", pure STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV)
                            , ("STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV", pure STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV", pure STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV)
                            , ("STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT)
                            , ("STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT)
                            , ("STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD", pure STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD)
                            , ("STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT", pure STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT)
                            , ("STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR", pure STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR", pure STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_PRESENT_INFO_KHR", pure STRUCTURE_TYPE_PRESENT_INFO_KHR)
                            , ("STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR", pure STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR)
                            , ("STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO", pure STRUCTURE_TYPE_DEVICE_MEMORY_OPAQUE_CAPTURE_ADDRESS_INFO)
                            , ("STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO", pure STRUCTURE_TYPE_MEMORY_OPAQUE_CAPTURE_ADDRESS_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO", pure STRUCTURE_TYPE_BUFFER_OPAQUE_CAPTURE_ADDRESS_CREATE_INFO)
                            , ("STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO", pure STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES)
                            , ("STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO", pure STRUCTURE_TYPE_SEMAPHORE_SIGNAL_INFO)
                            , ("STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO", pure STRUCTURE_TYPE_SEMAPHORE_WAIT_INFO)
                            , ("STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO", pure STRUCTURE_TYPE_TIMELINE_SEMAPHORE_SUBMIT_INFO)
                            , ("STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO", pure STRUCTURE_TYPE_SEMAPHORE_TYPE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_TIMELINE_SEMAPHORE_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES)
                            , ("STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT", pure STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_STENCIL_LAYOUT)
                            , ("STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT", pure STRUCTURE_TYPE_ATTACHMENT_REFERENCE_STENCIL_LAYOUT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SEPARATE_DEPTH_STENCIL_LAYOUTS_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_SUBGROUP_EXTENDED_TYPES_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_UNIFORM_BUFFER_STANDARD_LAYOUT_FEATURES)
                            , ("STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO", pure STRUCTURE_TYPE_RENDER_PASS_ATTACHMENT_BEGIN_INFO)
                            , ("STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO", pure STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENT_IMAGE_INFO)
                            , ("STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO", pure STRUCTURE_TYPE_FRAMEBUFFER_ATTACHMENTS_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGELESS_FRAMEBUFFER_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES)
                            , ("STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO", pure STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES)
                            , ("STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO", pure STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES)
                            , ("STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE", pure STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT", pure STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO", pure STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO", pure STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_FLOAT16_INT8_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES)
                            , ("STRUCTURE_TYPE_SUBPASS_END_INFO", pure STRUCTURE_TYPE_SUBPASS_END_INFO)
                            , ("STRUCTURE_TYPE_SUBPASS_BEGIN_INFO", pure STRUCTURE_TYPE_SUBPASS_BEGIN_INFO)
                            , ("STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2", pure STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2)
                            , ("STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2", pure STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2)
                            , ("STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2", pure STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2)
                            , ("STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2", pure STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2)
                            , ("STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2", pure STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2)
                            , ("STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO", pure STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_1_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT", pure STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES)
                            , ("STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES", pure STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO)
                            , ("STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO", pure STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO", pure STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES", pure STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO)
                            , ("STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO", pure STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO", pure STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO", pure STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES)
                            , ("STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES", pure STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO)
                            , ("STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES", pure STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO)
                            , ("STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO", pure STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES", pure STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES)
                            , ("STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO", pure STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO)
                            , ("STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO", pure STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO)
                            , ("STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO", pure STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO)
                            , ("STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO", pure STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2", pure STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES)
                            , ("STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO", pure STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES)
                            , ("STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO", pure STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO", pure STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO", pure STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO", pure STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2)
                            , ("STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2", pure STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2)
                            , ("STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2", pure STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2)
                            , ("STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2", pure STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2)
                            , ("STRUCTURE_TYPE_FORMAT_PROPERTIES_2", pure STRUCTURE_TYPE_FORMAT_PROPERTIES_2)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2)
                            , ("STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2", pure STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2)
                            , ("STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2", pure STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2)
                            , ("STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2", pure STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2)
                            , ("STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2", pure STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2)
                            , ("STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2", pure STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO", pure STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES)
                            , ("STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO", pure STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO)
                            , ("STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO", pure STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO", pure STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO", pure STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO", pure STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO)
                            , ("STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO", pure STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO)
                            , ("STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO", pure STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO)
                            , ("STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO", pure STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO)
                            , ("STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS", pure STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES)
                            , ("STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO", pure STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO)
                            , ("STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO", pure STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO)
                            , ("STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES", pure STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES)]
                     +++
                     prec 10 (do
                       expectP (Ident "StructureType")
                       v <- step readPrec
                       pure (StructureType v)))

