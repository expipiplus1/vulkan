{-# language CPP #-}
module Graphics.Vulkan.Core10.Handles  ( Instance(..)
                                       , Instance_T
                                       , PhysicalDevice(..)
                                       , PhysicalDevice_T
                                       , Device(..)
                                       , Device_T
                                       , Queue(..)
                                       , Queue_T
                                       , CommandBuffer(..)
                                       , CommandBuffer_T
                                       , DeviceMemory(..)
                                       , CommandPool(..)
                                       , Buffer(..)
                                       , BufferView(..)
                                       , Image(..)
                                       , ImageView(..)
                                       , ShaderModule(..)
                                       , Pipeline(..)
                                       , PipelineLayout(..)
                                       , Sampler(..)
                                       , DescriptorSet(..)
                                       , DescriptorSetLayout(..)
                                       , DescriptorPool(..)
                                       , Fence(..)
                                       , Semaphore(..)
                                       , Event(..)
                                       , QueryPool(..)
                                       , Framebuffer(..)
                                       , RenderPass(..)
                                       , PipelineCache(..)
                                       ) where

import GHC.Show (showParen)
import Numeric (showHex)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Graphics.Vulkan.Dynamic (DeviceCmds)
import Graphics.Vulkan.Dynamic (InstanceCmds)
import Graphics.Vulkan.Core10.APIConstants (IsHandle)
import Graphics.Vulkan.Zero (Zero)
import Graphics.Vulkan.Zero (Zero(..))
-- | An opaque type for representing pointers to VkInstance handles
data Instance_T
-- | VkInstance - Opaque handle to an instance object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.createAndroidSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_headless_surface.createHeadlessSurfaceEXT',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.createIOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.createImagePipeSurfaceFUCHSIA',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.createInstance',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.createMacOSSurfaceMVK',
-- 'Graphics.Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT',
-- 'Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface.createStreamDescriptorSurfaceGGP',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.createViSurfaceNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.createWaylandSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.createXcbSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.createXlibSurfaceKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.debugReportMessageEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.destroyDebugReportCallbackEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.destroyDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.destroyInstance',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.enumeratePhysicalDeviceGroups',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group_creation.enumeratePhysicalDeviceGroupsKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getInstanceProcAddr',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.submitDebugUtilsMessageEXT'
data Instance = Instance
  { instanceHandle :: Ptr Instance_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Instance where
  zero = Instance zero zero


-- | An opaque type for representing pointers to VkPhysicalDevice handles
data PhysicalDevice_T
-- | VkPhysicalDevice - Opaque handle to a physical device object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.acquireXlibDisplayEXT',
-- 'Graphics.Vulkan.Core10.Device.createDevice',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Graphics.Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties',
-- 'Graphics.Vulkan.Core10.LayerDiscovery.enumerateDeviceLayerProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2.getDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2.getDisplayPlaneCapabilities2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getDisplayPlaneSupportedDisplaysKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps.getPhysicalDeviceCalibrateableTimeDomainsEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix.getPhysicalDeviceCooperativeMatrixPropertiesNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2.getPhysicalDeviceDisplayPlaneProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2.getPhysicalDeviceDisplayProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferPropertiesKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.getPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_capabilities.getPhysicalDeviceExternalFencePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphorePropertiesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2KHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.getPhysicalDeviceGeneratedCommandsPropertiesNVX',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2KHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceMemoryProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceMemoryProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceMemoryProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2KHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceSparseImageFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceSparseImageFormatProperties2KHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.getPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceFormats2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_tooling_info.getPhysicalDeviceToolPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.getPhysicalDeviceWaylandPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.getPhysicalDeviceWin32PresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.getPhysicalDeviceXcbPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.getPhysicalDeviceXlibPresentationSupportKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display.getRandROutputDisplayEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display.releaseDisplayEXT'
data PhysicalDevice = PhysicalDevice
  { physicalDeviceHandle :: Ptr PhysicalDevice_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero PhysicalDevice where
  zero = PhysicalDevice zero zero


-- | An opaque type for representing pointers to VkDevice handles
data Device_T
-- | VkDevice - Opaque handle to a device object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.acquireFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImage2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.acquirePerformanceConfigurationINTEL',
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.acquireProfilingLockKHR',
-- 'Graphics.Vulkan.Core10.CommandBuffer.allocateCommandBuffers',
-- 'Graphics.Vulkan.Core10.DescriptorSet.allocateDescriptorSets',
-- 'Graphics.Vulkan.Core10.Memory.allocateMemory',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.bindAccelerationStructureMemoryNV',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindBufferMemory2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_bind_memory2.bindBufferMemory2KHR',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_bind_memory2.bindImageMemory2KHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Buffer.createBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.createBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.createCommandPool',
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Graphics.Vulkan.Core10.DescriptorSet.createDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.createDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.createDevice',
-- 'Graphics.Vulkan.Core10.Event.createEvent',
-- 'Graphics.Vulkan.Core10.Fence.createFence',
-- 'Graphics.Vulkan.Core10.Pass.createFramebuffer',
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Graphics.Vulkan.Core10.Image.createImage',
-- 'Graphics.Vulkan.Core10.ImageView.createImageView',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.createIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.createObjectTableNVX',
-- 'Graphics.Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.createPipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.createQueryPool',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Graphics.Vulkan.Core10.Pass.createRenderPass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR',
-- 'Graphics.Vulkan.Core10.Sampler.createSampler',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.createSemaphore',
-- 'Graphics.Vulkan.Core10.Shader.createShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.createValidationCacheEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.debugMarkerSetObjectNameEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.debugMarkerSetObjectTagEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.Buffer.destroyBuffer',
-- 'Graphics.Vulkan.Core10.BufferView.destroyBufferView',
-- 'Graphics.Vulkan.Core10.CommandPool.destroyCommandPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.destroyDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.destroyDescriptorSetLayout',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.Core10.Device.destroyDevice',
-- 'Graphics.Vulkan.Core10.Event.destroyEvent',
-- 'Graphics.Vulkan.Core10.Fence.destroyFence',
-- 'Graphics.Vulkan.Core10.Pass.destroyFramebuffer',
-- 'Graphics.Vulkan.Core10.Image.destroyImage',
-- 'Graphics.Vulkan.Core10.ImageView.destroyImageView',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.destroyIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.destroyObjectTableNVX',
-- 'Graphics.Vulkan.Core10.Pipeline.destroyPipeline',
-- 'Graphics.Vulkan.Core10.PipelineCache.destroyPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineLayout.destroyPipelineLayout',
-- 'Graphics.Vulkan.Core10.Query.destroyQueryPool',
-- 'Graphics.Vulkan.Core10.Pass.destroyRenderPass',
-- 'Graphics.Vulkan.Core10.Sampler.destroySampler',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.destroySemaphore',
-- 'Graphics.Vulkan.Core10.Shader.destroyShaderModule',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.destroyValidationCacheEXT',
-- 'Graphics.Vulkan.Core10.Queue.deviceWaitIdle',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.displayPowerControlEXT',
-- 'Graphics.Vulkan.Core10.Memory.flushMappedMemoryRanges',
-- 'Graphics.Vulkan.Core10.CommandBuffer.freeCommandBuffers',
-- 'Graphics.Vulkan.Core10.DescriptorSet.freeDescriptorSets',
-- 'Graphics.Vulkan.Core10.Memory.freeMemory',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureMemoryRequirementsNV',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.getAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress',
-- 'Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address.getBufferDeviceAddressEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferDeviceAddressKHR',
-- 'Graphics.Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddress',
-- 'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddressKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps.getCalibratedTimestampsEXT',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.getDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance3.getDescriptorSetLayoutSupportKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.getDeviceGroupPeerMemoryFeatures',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group.getDeviceGroupPeerMemoryFeaturesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupPresentCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.getDeviceGroupSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR',
-- 'Graphics.Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddress',
-- 'Graphics.Vulkan.Extensions.VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddressKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.getDeviceProcAddr',
-- 'Graphics.Vulkan.Core10.Queue.getDeviceQueue',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.getDeviceQueue2',
-- 'Graphics.Vulkan.Core10.Event.getEventStatus',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.getFenceFdKHR',
-- 'Graphics.Vulkan.Core10.Fence.getFenceStatus',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.getFenceWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.getImageDrmFormatModifierPropertiesEXT',
-- 'Graphics.Vulkan.Core10.MemoryManagement.getImageMemoryRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.getImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageSparseMemoryRequirements2KHR',
-- 'Graphics.Vulkan.Core10.Image.getImageSubresourceLayout',
-- 'Graphics.Vulkan.Extensions.VK_NVX_image_view_handle.getImageViewHandleNVX',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.getMemoryAndroidHardwareBufferANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.getMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.getMemoryWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.getMemoryWin32HandlePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.getPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.getPerformanceParameterINTEL',
-- 'Graphics.Vulkan.Core10.PipelineCache.getPipelineCacheData',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutablePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR',
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.getRayTracingShaderGroupHandlesNV',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.getRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.Core10.Pass.getRenderAreaGranularity',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.getSemaphoreCounterValue',
-- 'Graphics.Vulkan.Extensions.VK_KHR_timeline_semaphore.getSemaphoreCounterValueKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.getSemaphoreFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.getSemaphoreWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_AMD_shader_info.getShaderInfoAMD',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.getSwapchainCounterEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.getSwapchainStatusKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.getValidationCacheDataEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.importFenceFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.importFenceWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.importSemaphoreFdKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.importSemaphoreWin32HandleKHR',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.initializePerformanceApiINTEL',
-- 'Graphics.Vulkan.Core10.Memory.invalidateMappedMemoryRanges',
-- 'Graphics.Vulkan.Core10.Memory.mapMemory',
-- 'Graphics.Vulkan.Core10.PipelineCache.mergePipelineCaches',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.mergeValidationCachesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDeviceEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.registerObjectsNVX',
-- 'Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive.releaseFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.releasePerformanceConfigurationINTEL',
-- 'Graphics.Vulkan.Extensions.VK_KHR_performance_query.releaseProfilingLockKHR',
-- 'Graphics.Vulkan.Core10.CommandPool.resetCommandPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.resetDescriptorPool',
-- 'Graphics.Vulkan.Core10.Event.resetEvent',
-- 'Graphics.Vulkan.Core10.Fence.resetFences',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool',
-- 'Graphics.Vulkan.Extensions.VK_EXT_host_query_reset.resetQueryPoolEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.setDebugUtilsObjectNameEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.setDebugUtilsObjectTagEXT',
-- 'Graphics.Vulkan.Core10.Event.setEvent',
-- 'Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata.setHdrMetadataEXT',
-- 'Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore',
-- 'Graphics.Vulkan.Extensions.VK_KHR_timeline_semaphore.signalSemaphoreKHR',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance1.trimCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance1.trimCommandPoolKHR',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.uninitializePerformanceApiINTEL',
-- 'Graphics.Vulkan.Core10.Memory.unmapMemory',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.unregisterObjectsNVX',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Core10.DescriptorSet.updateDescriptorSets',
-- 'Graphics.Vulkan.Core10.Fence.waitForFences',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.waitSemaphores',
-- 'Graphics.Vulkan.Extensions.VK_KHR_timeline_semaphore.waitSemaphoresKHR'
data Device = Device
  { deviceHandle :: Ptr Device_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Device where
  zero = Device zero zero


-- | An opaque type for representing pointers to VkQueue handles
data Queue_T
-- | VkQueue - Opaque handle to a queue object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Queue.getDeviceQueue',
-- 'Graphics.Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.getDeviceQueue2',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.getQueueCheckpointDataNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.queueBeginDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.queueBindSparse',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.queueEndDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.queueInsertDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.queueSetPerformanceConfigurationINTEL',
-- 'Graphics.Vulkan.Core10.Queue.queueSubmit',
-- 'Graphics.Vulkan.Core10.Queue.queueWaitIdle'
data Queue = Queue
  { queueHandle :: Ptr Queue_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Queue where
  zero = Queue zero zero


-- | An opaque type for representing pointers to VkCommandBuffer handles
data CommandBuffer_T
-- | VkCommandBuffer - Opaque handle to a command buffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.CmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.Queue.SubmitInfo',
-- 'Graphics.Vulkan.Core10.CommandBuffer.allocateCommandBuffers',
-- 'Graphics.Vulkan.Core10.CommandBuffer.beginCommandBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.cmdBeginDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdBeginRenderPass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.cmdBeginRenderPass2KHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBlitImage',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerBeginEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerEndEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerInsertEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatch',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group.cmdDispatchBaseKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDraw',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.cmdEndConditionalRenderingEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.cmdEndDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdEndQuery',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdEndRenderPass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdEndRenderPass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.cmdEndRenderPass2KHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.cmdInsertDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdNextSubpass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.cmdNextSubpass2KHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.cmdProcessCommandsNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPushConstants',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.cmdReserveSpaceForCommandsNVX',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResolveImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants',
-- 'Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.cmdSetCheckpointNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetCoarseSampleOrderNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdSetDeviceMask',
-- 'Graphics.Vulkan.Extensions.VK_KHR_device_group.cmdSetDeviceMaskKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetEvent',
-- 'Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_line_rasterization.cmdSetLineStippleEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.cmdSetPerformanceMarkerINTEL',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.cmdSetPerformanceOverrideINTEL',
-- 'Graphics.Vulkan.Extensions.VK_INTEL_performance_query.cmdSetPerformanceStreamMarkerINTEL',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetScissor',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetViewport',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp',
-- 'Graphics.Vulkan.Core10.CommandBuffer.endCommandBuffer',
-- 'Graphics.Vulkan.Core10.CommandBuffer.freeCommandBuffers',
-- 'Graphics.Vulkan.Core10.CommandBuffer.resetCommandBuffer'
data CommandBuffer = CommandBuffer
  { commandBufferHandle :: Ptr CommandBuffer_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero CommandBuffer where
  zero = CommandBuffer zero zero


-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.DeviceMemoryOpaqueCaptureAddressInfo',
-- 'Graphics.Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.MemoryGetAndroidHardwareBufferInfoANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.MemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.MemoryGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV',
-- 'Graphics.Vulkan.Core10.Memory.allocateMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Graphics.Vulkan.Core10.Memory.freeMemory',
-- 'Graphics.Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV',
-- 'Graphics.Vulkan.Core10.Memory.mapMemory',
-- 'Graphics.Vulkan.Core10.Memory.unmapMemory'
newtype DeviceMemory = DeviceMemory Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DeviceMemory where
  showsPrec p (DeviceMemory x) = showParen (p >= 11) (showString "DeviceMemory 0x" . showHex x)


-- | VkCommandPool - Opaque handle to a command pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferAllocateInfo',
-- 'Graphics.Vulkan.Core10.CommandPool.createCommandPool',
-- 'Graphics.Vulkan.Core10.CommandPool.destroyCommandPool',
-- 'Graphics.Vulkan.Core10.CommandBuffer.freeCommandBuffers',
-- 'Graphics.Vulkan.Core10.CommandPool.resetCommandPool',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_maintenance1.trimCommandPool',
-- 'Graphics.Vulkan.Extensions.VK_KHR_maintenance1.trimCommandPoolKHR'
newtype CommandPool = CommandPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show CommandPool where
  showsPrec p (CommandPool x) = showParen (p >= 11) (showString "CommandPool 0x" . showHex x)


-- | VkBuffer - Opaque handle to a buffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferDeviceAddressInfo',
-- 'Graphics.Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.BufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.CmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.IndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTableIndexBufferEntryNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTableVertexBufferEntryNVX',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseBufferMemoryBindInfo',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Graphics.Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.Core10.Buffer.createBuffer',
-- 'Graphics.Vulkan.Core10.Buffer.destroyBuffer',
-- 'Graphics.Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements'
newtype Buffer = Buffer Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Buffer where
  showsPrec p (Buffer x) = showParen (p >= 11) (showString "Buffer 0x" . showHex x)


-- | VkBufferView - Opaque handle to a buffer view object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Graphics.Vulkan.Core10.BufferView.createBufferView',
-- 'Graphics.Vulkan.Core10.BufferView.destroyBufferView'
newtype BufferView = BufferView Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show BufferView where
  showsPrec p (BufferView x) = showParen (p >= 11) (showString "BufferView 0x" . showHex x)


-- | VkImage - Opaque handle to an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBindInfo',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.SparseImageOpaqueMemoryBindInfo',
-- 'Graphics.Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBlitImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResolveImage',
-- 'Graphics.Vulkan.Core10.Image.createImage',
-- 'Graphics.Vulkan.Core10.Image.destroyImage',
-- 'Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier.getImageDrmFormatModifierPropertiesEXT',
-- 'Graphics.Vulkan.Core10.MemoryManagement.getImageMemoryRequirements',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.getImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.Core10.Image.getImageSubresourceLayout',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR'
newtype Image = Image Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Image where
  showsPrec p (Image x) = showParen (p >= 11) (showString "Image 0x" . showHex x)


-- | VkImageView - Opaque handle to an image view object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
-- 'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV',
-- 'Graphics.Vulkan.Core10.ImageView.createImageView',
-- 'Graphics.Vulkan.Core10.ImageView.destroyImageView'
newtype ImageView = ImageView Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show ImageView where
  showsPrec p (ImageView x) = showParen (p >= 11) (showString "ImageView 0x" . showHex x)


-- | VkShaderModule - Opaque handle to a shader module object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.Core10.Shader.createShaderModule',
-- 'Graphics.Vulkan.Core10.Shader.destroyShaderModule'
newtype ShaderModule = ShaderModule Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show ShaderModule where
  showsPrec p (ShaderModule x) = showParen (p >= 11) (showString "ShaderModule 0x" . showHex x)


-- | VkPipeline - Opaque handle to a pipeline object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTablePipelineEntryNVX',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV',
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Graphics.Vulkan.Core10.Pipeline.destroyPipeline',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.getRayTracingShaderGroupHandlesNV',
-- 'Graphics.Vulkan.Extensions.VK_AMD_shader_info.getShaderInfoAMD'
newtype Pipeline = Pipeline Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Pipeline where
  showsPrec p (Pipeline x) = showParen (p >= 11) (showString "Pipeline 0x" . showHex x)


-- | VkPipelineLayout - Opaque handle to a pipeline layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTablePushConstantEntryNVX',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdPushConstants',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.Core10.PipelineLayout.createPipelineLayout',
-- 'Graphics.Vulkan.Core10.PipelineLayout.destroyPipelineLayout'
newtype PipelineLayout = PipelineLayout Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show PipelineLayout where
  showsPrec p (PipelineLayout x) = showParen (p >= 11) (showString "PipelineLayout 0x" . showHex x)


-- | VkSampler - Opaque handle to a sampler object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.Core10.Sampler.createSampler',
-- 'Graphics.Vulkan.Core10.Sampler.destroySampler'
newtype Sampler = Sampler Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Sampler where
  showsPrec p (Sampler x) = showParen (p >= 11) (showString "Sampler 0x" . showHex x)


-- | VkDescriptorSet - Opaque handle to a descriptor set object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.CopyDescriptorSet',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.ObjectTableDescriptorSetEntryNVX',
-- 'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Graphics.Vulkan.Core10.DescriptorSet.allocateDescriptorSets',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Graphics.Vulkan.Core10.DescriptorSet.freeDescriptorSets',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.Extensions.VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplateKHR'
newtype DescriptorSet = DescriptorSet Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DescriptorSet where
  showsPrec p (DescriptorSet x) = showParen (p >= 11) (showString "DescriptorSet 0x" . showHex x)


-- | VkDescriptorSetLayout - Opaque handle to a descriptor set layout object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.Core10.DescriptorSet.createDescriptorSetLayout',
-- 'Graphics.Vulkan.Core10.DescriptorSet.destroyDescriptorSetLayout'
newtype DescriptorSetLayout = DescriptorSetLayout Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DescriptorSetLayout where
  showsPrec p (DescriptorSetLayout x) = showParen (p >= 11) (showString "DescriptorSetLayout 0x" . showHex x)


-- | VkDescriptorPool - Opaque handle to a descriptor pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.Core10.DescriptorSet.createDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.destroyDescriptorPool',
-- 'Graphics.Vulkan.Core10.DescriptorSet.freeDescriptorSets',
-- 'Graphics.Vulkan.Core10.DescriptorSet.resetDescriptorPool'
newtype DescriptorPool = DescriptorPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show DescriptorPool where
  showsPrec p (DescriptorPool x) = showParen (p >= 11) (showString "DescriptorPool 0x" . showHex x)


-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.FenceGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.FenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.ImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.ImportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Graphics.Vulkan.Core10.Fence.createFence',
-- 'Graphics.Vulkan.Core10.Fence.destroyFence',
-- 'Graphics.Vulkan.Core10.Fence.getFenceStatus',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.queueBindSparse',
-- 'Graphics.Vulkan.Core10.Queue.queueSubmit',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDeviceEventEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT',
-- 'Graphics.Vulkan.Core10.Fence.resetFences',
-- 'Graphics.Vulkan.Core10.Fence.waitForFences'
newtype Fence = Fence Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Fence where
  showsPrec p (Fence x) = showParen (p >= 11) (showString "Fence 0x" . showHex x)


-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.ImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.ImportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.SemaphoreGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.SemaphoreGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreSignalInfo',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo',
-- 'Graphics.Vulkan.Core10.Queue.SubmitInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.createSemaphore',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.destroySemaphore',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.getSemaphoreCounterValue',
-- 'Graphics.Vulkan.Extensions.VK_KHR_timeline_semaphore.getSemaphoreCounterValueKHR'
newtype Semaphore = Semaphore Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Semaphore where
  showsPrec p (Semaphore x) = showParen (p >= 11) (showString "Semaphore 0x" . showHex x)


-- | VkEvent - Opaque handle to an event object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdSetEvent',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents',
-- 'Graphics.Vulkan.Core10.Event.createEvent',
-- 'Graphics.Vulkan.Core10.Event.destroyEvent',
-- 'Graphics.Vulkan.Core10.Event.getEventStatus',
-- 'Graphics.Vulkan.Core10.Event.resetEvent',
-- 'Graphics.Vulkan.Core10.Event.setEvent'
newtype Event = Event Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Event where
  showsPrec p (Event x) = showParen (p >= 11) (showString "Event 0x" . showHex x)


-- | VkQueryPool - Opaque handle to a query pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdEndQuery',
-- 'Graphics.Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp',
-- 'Graphics.Vulkan.Core10.Query.createQueryPool',
-- 'Graphics.Vulkan.Core10.Query.destroyQueryPool',
-- 'Graphics.Vulkan.Core10.Query.getQueryPoolResults',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool',
-- 'Graphics.Vulkan.Extensions.VK_EXT_host_query_reset.resetQueryPoolEXT'
newtype QueryPool = QueryPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show QueryPool where
  showsPrec p (QueryPool x) = showParen (p >= 11) (showString "QueryPool 0x" . showHex x)


-- | VkFramebuffer - Opaque handle to a framebuffer object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Graphics.Vulkan.Core10.Pass.createFramebuffer',
-- 'Graphics.Vulkan.Core10.Pass.destroyFramebuffer'
newtype Framebuffer = Framebuffer Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show Framebuffer where
  showsPrec p (Framebuffer x) = showParen (p >= 11) (showString "Framebuffer 0x" . showHex x)


-- | VkRenderPass - Opaque handle to a render pass object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Core10.Pass.FramebufferCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Graphics.Vulkan.Core10.Pass.createRenderPass',
-- 'Graphics.Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR',
-- 'Graphics.Vulkan.Core10.Pass.destroyRenderPass',
-- 'Graphics.Vulkan.Core10.Pass.getRenderAreaGranularity'
newtype RenderPass = RenderPass Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show RenderPass where
  showsPrec p (RenderPass x) = showParen (p >= 11) (showString "RenderPass 0x" . showHex x)


-- | VkPipelineCache - Opaque handle to a pipeline cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Graphics.Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Graphics.Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Graphics.Vulkan.Core10.PipelineCache.destroyPipelineCache',
-- 'Graphics.Vulkan.Core10.PipelineCache.getPipelineCacheData',
-- 'Graphics.Vulkan.Core10.PipelineCache.mergePipelineCaches'
newtype PipelineCache = PipelineCache Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance Show PipelineCache where
  showsPrec p (PipelineCache x) = showParen (p >= 11) (showString "PipelineCache 0x" . showHex x)

