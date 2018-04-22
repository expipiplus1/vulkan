{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions
  ( module Graphics.Vulkan.Extensions.VK_KHR_surface
  , module Graphics.Vulkan.Extensions.VK_KHR_swapchain
  , module Graphics.Vulkan.Extensions.VK_KHR_display
  , module Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
#if defined(VK_USE_PLATFORM_XLIB_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
#endif
#if defined(VK_USE_PLATFORM_MIR_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_mir_surface
#endif
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_android_surface
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_win32_surface
#endif
  , module Graphics.Vulkan.Extensions.VK_EXT_debug_report
  , module Graphics.Vulkan.Extensions.VK_NV_glsl_shader
  , module Graphics.Vulkan.Extensions.VK_EXT_depth_range_unrestricted
  , module Graphics.Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
  , module Graphics.Vulkan.Extensions.VK_IMG_filter_cubic
  , module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_trinary_minmax
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_explicit_vertex_parameter
  , module Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  , module Graphics.Vulkan.Extensions.VK_AMD_gcn_shader
  , module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  , module Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count
  , module Graphics.Vulkan.Extensions.VK_AMD_negative_viewport_height
  , module Graphics.Vulkan.Extensions.VK_AMD_gpu_shader_half_float
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_ballot
  , module Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_info
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_image_load_store_lod
  , module Graphics.Vulkan.Extensions.VK_KHR_multiview
  , module Graphics.Vulkan.Extensions.VK_IMG_format_pvrtc
  , module Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  , module Graphics.Vulkan.Extensions.VK_NV_external_memory
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , module Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  , module Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
#endif
  , module Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2
  , module Graphics.Vulkan.Extensions.VK_KHR_device_group
  , module Graphics.Vulkan.Extensions.VK_EXT_validation_flags
#if defined(VK_USE_PLATFORM_VI_NN)
  , module Graphics.Vulkan.Extensions.VK_NN_vi_surface
#endif
  , module Graphics.Vulkan.Extensions.VK_KHR_shader_draw_parameters
  , module Graphics.Vulkan.Extensions.VK_EXT_shader_subgroup_ballot
  , module Graphics.Vulkan.Extensions.VK_EXT_shader_subgroup_vote
  , module Graphics.Vulkan.Extensions.VK_KHR_maintenance1
  , module Graphics.Vulkan.Extensions.VK_KHR_device_group_creation
  , module Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities
  , module Graphics.Vulkan.Extensions.VK_KHR_external_memory
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
#endif
  , module Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
#endif
  , module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities
  , module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
#endif
  , module Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  , module Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  , module Graphics.Vulkan.Extensions.VK_KHR_16bit_storage
  , module Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  , module Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template
  , module Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  , module Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  , module Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
  , module Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
#endif
  , module Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  , module Graphics.Vulkan.Extensions.VK_EXT_display_control
  , module Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  , module Graphics.Vulkan.Extensions.VK_NV_sample_mask_override_coverage
  , module Graphics.Vulkan.Extensions.VK_NV_geometry_shader_passthrough
  , module Graphics.Vulkan.Extensions.VK_NV_viewport_array2
  , module Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  , module Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  , module Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  , module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  , module Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace
  , module Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  , module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  , module Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities
  , module Graphics.Vulkan.Extensions.VK_KHR_external_fence
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , module Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
#endif
  , module Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  , module Graphics.Vulkan.Extensions.VK_KHR_maintenance2
  , module Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  , module Graphics.Vulkan.Extensions.VK_KHR_variable_pointers
#if defined(VK_USE_PLATFORM_IOS_MVK)
  , module Graphics.Vulkan.Extensions.VK_MVK_ios_surface
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
  , module Graphics.Vulkan.Extensions.VK_MVK_macos_surface
#endif
  , module Graphics.Vulkan.Extensions.VK_EXT_external_memory_dma_buf
  , module Graphics.Vulkan.Extensions.VK_EXT_queue_family_foreign
  , module Graphics.Vulkan.Extensions.VK_KHR_dedicated_allocation
  , module Graphics.Vulkan.Extensions.VK_EXT_debug_utils
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , module Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
#endif
  , module Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  , module Graphics.Vulkan.Extensions.VK_KHR_storage_buffer_storage_class
  , module Graphics.Vulkan.Extensions.VK_AMD_gpu_shader_int16
  , module Graphics.Vulkan.Extensions.VK_AMD_mixed_attachment_samples
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_fragment_mask
  , module Graphics.Vulkan.Extensions.VK_EXT_shader_stencil_export
  , module Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  , module Graphics.Vulkan.Extensions.VK_KHR_relaxed_block_layout
  , module Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2
  , module Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  , module Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  , module Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  , module Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  , module Graphics.Vulkan.Extensions.VK_NV_fill_rectangle
  , module Graphics.Vulkan.Extensions.VK_EXT_post_depth_coverage
  , module Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion
  , module Graphics.Vulkan.Extensions.VK_KHR_bind_memory2
  , module Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  , module Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  , module Graphics.Vulkan.Extensions.VK_EXT_shader_viewport_index_layer
  , module Graphics.Vulkan.Extensions.VK_KHR_maintenance3
  , module Graphics.Vulkan.Extensions.VK_EXT_global_priority
  , module Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  , module Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
  , module Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  , module Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  , module Graphics.Vulkan.Extensions.VK_NV_shader_subgroup_partitioned
  ) where


import Graphics.Vulkan.Extensions.VK_KHR_surface
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
import Graphics.Vulkan.Extensions.VK_KHR_display
import Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
#endif
#if defined(VK_USE_PLATFORM_MIR_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_mir_surface
#endif
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
#endif
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
import Graphics.Vulkan.Extensions.VK_NV_glsl_shader
import Graphics.Vulkan.Extensions.VK_EXT_depth_range_unrestricted
import Graphics.Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
import Graphics.Vulkan.Extensions.VK_IMG_filter_cubic
import Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
import Graphics.Vulkan.Extensions.VK_AMD_shader_trinary_minmax
import Graphics.Vulkan.Extensions.VK_AMD_shader_explicit_vertex_parameter
import Graphics.Vulkan.Extensions.VK_EXT_debug_marker
import Graphics.Vulkan.Extensions.VK_AMD_gcn_shader
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
import Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count
import Graphics.Vulkan.Extensions.VK_AMD_negative_viewport_height
import Graphics.Vulkan.Extensions.VK_AMD_gpu_shader_half_float
import Graphics.Vulkan.Extensions.VK_AMD_shader_ballot
import Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
import Graphics.Vulkan.Extensions.VK_AMD_shader_info
import Graphics.Vulkan.Extensions.VK_AMD_shader_image_load_store_lod
import Graphics.Vulkan.Extensions.VK_KHR_multiview
import Graphics.Vulkan.Extensions.VK_IMG_format_pvrtc
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
import Graphics.Vulkan.Extensions.VK_NV_external_memory
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
#endif
import Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2
import Graphics.Vulkan.Extensions.VK_KHR_device_group
import Graphics.Vulkan.Extensions.VK_EXT_validation_flags
#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
#endif
import Graphics.Vulkan.Extensions.VK_KHR_shader_draw_parameters
import Graphics.Vulkan.Extensions.VK_EXT_shader_subgroup_ballot
import Graphics.Vulkan.Extensions.VK_EXT_shader_subgroup_vote
import Graphics.Vulkan.Extensions.VK_KHR_maintenance1
import Graphics.Vulkan.Extensions.VK_KHR_device_group_creation
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities
import Graphics.Vulkan.Extensions.VK_KHR_external_memory
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
import Graphics.Vulkan.Extensions.VK_KHR_16bit_storage
import Graphics.Vulkan.Extensions.VK_KHR_incremental_present
import Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template
import Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
import Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
import Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
#endif
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
import Graphics.Vulkan.Extensions.VK_EXT_display_control
import Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
import Graphics.Vulkan.Extensions.VK_NV_sample_mask_override_coverage
import Graphics.Vulkan.Extensions.VK_NV_geometry_shader_passthrough
import Graphics.Vulkan.Extensions.VK_NV_viewport_array2
import Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
import Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
import Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
import Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
import Graphics.Vulkan.Extensions.VK_EXT_swapchain_colorspace
import Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities
import Graphics.Vulkan.Extensions.VK_KHR_external_fence
#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
import Graphics.Vulkan.Extensions.VK_KHR_maintenance2
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
import Graphics.Vulkan.Extensions.VK_KHR_variable_pointers
#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
#endif
import Graphics.Vulkan.Extensions.VK_EXT_external_memory_dma_buf
import Graphics.Vulkan.Extensions.VK_EXT_queue_family_foreign
import Graphics.Vulkan.Extensions.VK_KHR_dedicated_allocation
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
#endif
import Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
import Graphics.Vulkan.Extensions.VK_KHR_storage_buffer_storage_class
import Graphics.Vulkan.Extensions.VK_AMD_gpu_shader_int16
import Graphics.Vulkan.Extensions.VK_AMD_mixed_attachment_samples
import Graphics.Vulkan.Extensions.VK_AMD_shader_fragment_mask
import Graphics.Vulkan.Extensions.VK_EXT_shader_stencil_export
import Graphics.Vulkan.Extensions.VK_EXT_sample_locations
import Graphics.Vulkan.Extensions.VK_KHR_relaxed_block_layout
import Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2
import Graphics.Vulkan.Extensions.VK_KHR_image_format_list
import Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
import Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
import Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
import Graphics.Vulkan.Extensions.VK_NV_fill_rectangle
import Graphics.Vulkan.Extensions.VK_EXT_post_depth_coverage
import Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion
import Graphics.Vulkan.Extensions.VK_KHR_bind_memory2
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
import Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
import Graphics.Vulkan.Extensions.VK_EXT_shader_viewport_index_layer
import Graphics.Vulkan.Extensions.VK_KHR_maintenance3
import Graphics.Vulkan.Extensions.VK_EXT_global_priority
import Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
import Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
import Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
import Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
import Graphics.Vulkan.Extensions.VK_NV_shader_subgroup_partitioned



