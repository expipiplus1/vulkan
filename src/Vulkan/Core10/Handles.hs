{-# language CPP #-}
module Vulkan.Core10.Handles  ( Instance(..)
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

import Foreign.Ptr (ptrToWordPtr)
import GHC.Show (showParen)
import Numeric (showHex)
import Foreign.Ptr (pattern WordPtr)
import Foreign.Storable (Storable)
import Foreign.Ptr (Ptr)
import Data.Word (Word64)
import Vulkan.Dynamic (DeviceCmds)
import Vulkan.Core10.APIConstants (HasObjectType(..))
import Vulkan.Dynamic (InstanceCmds)
import Vulkan.Core10.APIConstants (IsHandle)
import Vulkan.Zero (Zero)
import Vulkan.Zero (Zero(..))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_BUFFER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_BUFFER_VIEW))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_COMMAND_BUFFER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_COMMAND_POOL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_POOL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_SET))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEVICE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_DEVICE_MEMORY))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_EVENT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_FENCE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_FRAMEBUFFER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_IMAGE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_IMAGE_VIEW))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_INSTANCE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PHYSICAL_DEVICE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PIPELINE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PIPELINE_CACHE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_PIPELINE_LAYOUT))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_QUERY_POOL))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_QUEUE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_RENDER_PASS))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SAMPLER))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SEMAPHORE))
import Vulkan.Core10.Enums.ObjectType (ObjectType(OBJECT_TYPE_SHADER_MODULE))
-- | An opaque type for representing pointers to VkInstance handles
data Instance_T
-- | VkInstance - Opaque handle to an instance object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_android_surface.createAndroidSurfaceKHR',
-- 'Vulkan.Extensions.VK_EXT_debug_report.createDebugReportCallbackEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.createDebugUtilsMessengerEXT',
-- 'Vulkan.Extensions.VK_EXT_directfb_surface.createDirectFBSurfaceEXT',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayPlaneSurfaceKHR',
-- 'Vulkan.Extensions.VK_EXT_headless_surface.createHeadlessSurfaceEXT',
-- 'Vulkan.Extensions.VK_MVK_ios_surface.createIOSSurfaceMVK',
-- 'Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface.createImagePipeSurfaceFUCHSIA',
-- 'Vulkan.Core10.DeviceInitialization.createInstance',
-- 'Vulkan.Extensions.VK_MVK_macos_surface.createMacOSSurfaceMVK',
-- 'Vulkan.Extensions.VK_EXT_metal_surface.createMetalSurfaceEXT',
-- 'Vulkan.Extensions.VK_GGP_stream_descriptor_surface.createStreamDescriptorSurfaceGGP',
-- 'Vulkan.Extensions.VK_NN_vi_surface.createViSurfaceNN',
-- 'Vulkan.Extensions.VK_KHR_wayland_surface.createWaylandSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_win32_surface.createWin32SurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_xcb_surface.createXcbSurfaceKHR',
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.createXlibSurfaceKHR',
-- 'Vulkan.Extensions.VK_EXT_debug_report.debugReportMessageEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_report.destroyDebugReportCallbackEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.destroyDebugUtilsMessengerEXT',
-- 'Vulkan.Core10.DeviceInitialization.destroyInstance',
-- 'Vulkan.Extensions.VK_KHR_surface.destroySurfaceKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.enumeratePhysicalDeviceGroups',
-- 'Vulkan.Extensions.VK_KHR_device_group_creation.enumeratePhysicalDeviceGroupsKHR',
-- 'Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices',
-- 'Vulkan.Core10.DeviceInitialization.getInstanceProcAddr',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.submitDebugUtilsMessageEXT'
data Instance = Instance
  { instanceHandle :: Ptr Instance_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Instance where
  zero = Instance zero zero
instance HasObjectType Instance where
  objectTypeAndHandle (Instance (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_INSTANCE, fromIntegral h)


-- | An opaque type for representing pointers to VkPhysicalDevice handles
data PhysicalDevice_T
-- | VkPhysicalDevice - Opaque handle to a physical device object
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.DeviceGroupDeviceCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation.PhysicalDeviceGroupProperties',
-- 'Vulkan.Extensions.VK_EXT_acquire_xlib_display.acquireXlibDisplayEXT',
-- 'Vulkan.Core10.Device.createDevice',
-- 'Vulkan.Extensions.VK_KHR_display.createDisplayModeKHR',
-- 'Vulkan.Core10.ExtensionDiscovery.enumerateDeviceExtensionProperties',
-- 'Vulkan.Core10.LayerDiscovery.enumerateDeviceLayerProperties',
-- 'Vulkan.Extensions.VK_KHR_performance_query.enumeratePhysicalDeviceQueueFamilyPerformanceQueryCountersKHR',
-- 'Vulkan.Core10.DeviceInitialization.enumeratePhysicalDevices',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.getDisplayModeProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayModePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.getDisplayPlaneCapabilities2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_display.getDisplayPlaneSupportedDisplaysKHR',
-- 'Vulkan.Extensions.VK_EXT_calibrated_timestamps.getPhysicalDeviceCalibrateableTimeDomainsEXT',
-- 'Vulkan.Extensions.VK_NV_cooperative_matrix.getPhysicalDeviceCooperativeMatrixPropertiesNV',
-- 'Vulkan.Extensions.VK_EXT_directfb_surface.getPhysicalDeviceDirectFBPresentationSupportEXT',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.getPhysicalDeviceDisplayPlaneProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPlanePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_get_display_properties2.getPhysicalDeviceDisplayProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_display.getPhysicalDeviceDisplayPropertiesKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferProperties',
-- 'Vulkan.Extensions.VK_KHR_external_memory_capabilities.getPhysicalDeviceExternalBufferPropertiesKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_fence_capabilities.getPhysicalDeviceExternalFenceProperties',
-- 'Vulkan.Extensions.VK_KHR_external_fence_capabilities.getPhysicalDeviceExternalFencePropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphoreProperties',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_capabilities.getPhysicalDeviceExternalSemaphorePropertiesKHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFeatures',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFeatures2KHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.getPhysicalDeviceFragmentShadingRatesKHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceImageFormatProperties2KHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceMemoryProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceMemoryProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceMemoryProperties2KHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.getPhysicalDeviceMultisamplePropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getPhysicalDevicePresentRectanglesKHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceProperties2KHR',
-- 'Vulkan.Extensions.VK_KHR_performance_query.getPhysicalDeviceQueueFamilyPerformanceQueryPassesKHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceQueueFamilyProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceQueueFamilyProperties2KHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceSparseImageFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceSparseImageFormatProperties2KHR',
-- 'Vulkan.Extensions.VK_NV_coverage_reduction_mode.getPhysicalDeviceSupportedFramebufferMixedSamplesCombinationsNV',
-- 'Vulkan.Extensions.VK_EXT_display_surface_counter.getPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceCapabilities2KHR',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_KHR_get_surface_capabilities2.getPhysicalDeviceSurfaceFormats2KHR',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceFormatsKHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getPhysicalDeviceSurfacePresentModes2EXT',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfacePresentModesKHR',
-- 'Vulkan.Extensions.VK_KHR_surface.getPhysicalDeviceSurfaceSupportKHR',
-- 'Vulkan.Extensions.VK_EXT_tooling_info.getPhysicalDeviceToolPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_wayland_surface.getPhysicalDeviceWaylandPresentationSupportKHR',
-- 'Vulkan.Extensions.VK_KHR_win32_surface.getPhysicalDeviceWin32PresentationSupportKHR',
-- 'Vulkan.Extensions.VK_KHR_xcb_surface.getPhysicalDeviceXcbPresentationSupportKHR',
-- 'Vulkan.Extensions.VK_KHR_xlib_surface.getPhysicalDeviceXlibPresentationSupportKHR',
-- 'Vulkan.Extensions.VK_EXT_acquire_xlib_display.getRandROutputDisplayEXT',
-- 'Vulkan.Extensions.VK_EXT_direct_mode_display.releaseDisplayEXT'
data PhysicalDevice = PhysicalDevice
  { physicalDeviceHandle :: Ptr PhysicalDevice_T
  , instanceCmds :: InstanceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero PhysicalDevice where
  zero = PhysicalDevice zero zero
instance HasObjectType PhysicalDevice where
  objectTypeAndHandle (PhysicalDevice (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_PHYSICAL_DEVICE, fromIntegral h)


-- | An opaque type for representing pointers to VkDevice handles
data Device_T
-- | VkDevice - Opaque handle to a device object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.acquireFullScreenExclusiveModeEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImage2KHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.acquirePerformanceConfigurationINTEL',
-- 'Vulkan.Extensions.VK_KHR_performance_query.acquireProfilingLockKHR',
-- 'Vulkan.Core10.CommandBuffer.allocateCommandBuffers',
-- 'Vulkan.Core10.DescriptorSet.allocateDescriptorSets',
-- 'Vulkan.Core10.Memory.allocateMemory',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.bindAccelerationStructureMemoryNV',
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindBufferMemory2',
-- 'Vulkan.Extensions.VK_KHR_bind_memory2.bindBufferMemory2KHR',
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2',
-- 'Vulkan.Extensions.VK_KHR_bind_memory2.bindImageMemory2KHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.buildAccelerationStructuresKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.copyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.copyAccelerationStructureToMemoryKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.copyMemoryToAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.createAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createAccelerationStructureNV',
-- 'Vulkan.Core10.Buffer.createBuffer',
-- 'Vulkan.Core10.BufferView.createBufferView',
-- 'Vulkan.Core10.CommandPool.createCommandPool',
-- 'Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.createDeferredOperationKHR',
-- 'Vulkan.Core10.DescriptorSet.createDescriptorPool',
-- 'Vulkan.Core10.DescriptorSet.createDescriptorSetLayout',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.createDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.createDescriptorUpdateTemplateKHR',
-- 'Vulkan.Core10.Device.createDevice', 'Vulkan.Core10.Event.createEvent',
-- 'Vulkan.Core10.Fence.createFence',
-- 'Vulkan.Core10.Pass.createFramebuffer',
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Vulkan.Core10.Image.createImage',
-- 'Vulkan.Core10.ImageView.createImageView',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.createIndirectCommandsLayoutNV',
-- 'Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Vulkan.Core10.PipelineLayout.createPipelineLayout',
-- 'Vulkan.Extensions.VK_EXT_private_data.createPrivateDataSlotEXT',
-- 'Vulkan.Core10.Query.createQueryPool',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Vulkan.Core10.Pass.createRenderPass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR',
-- 'Vulkan.Core10.Sampler.createSampler',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.createSamplerYcbcrConversionKHR',
-- 'Vulkan.Core10.QueueSemaphore.createSemaphore',
-- 'Vulkan.Core10.Shader.createShaderModule',
-- 'Vulkan.Extensions.VK_KHR_display_swapchain.createSharedSwapchainsKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.createSwapchainKHR',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.createValidationCacheEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.debugMarkerSetObjectNameEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.debugMarkerSetObjectTagEXT',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.deferredOperationJoinKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.destroyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.destroyAccelerationStructureNV',
-- 'Vulkan.Core10.Buffer.destroyBuffer',
-- 'Vulkan.Core10.BufferView.destroyBufferView',
-- 'Vulkan.Core10.CommandPool.destroyCommandPool',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.destroyDeferredOperationKHR',
-- 'Vulkan.Core10.DescriptorSet.destroyDescriptorPool',
-- 'Vulkan.Core10.DescriptorSet.destroyDescriptorSetLayout',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.destroyDescriptorUpdateTemplateKHR',
-- 'Vulkan.Core10.Device.destroyDevice',
-- 'Vulkan.Core10.Event.destroyEvent', 'Vulkan.Core10.Fence.destroyFence',
-- 'Vulkan.Core10.Pass.destroyFramebuffer',
-- 'Vulkan.Core10.Image.destroyImage',
-- 'Vulkan.Core10.ImageView.destroyImageView',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.destroyIndirectCommandsLayoutNV',
-- 'Vulkan.Core10.Pipeline.destroyPipeline',
-- 'Vulkan.Core10.PipelineCache.destroyPipelineCache',
-- 'Vulkan.Core10.PipelineLayout.destroyPipelineLayout',
-- 'Vulkan.Extensions.VK_EXT_private_data.destroyPrivateDataSlotEXT',
-- 'Vulkan.Core10.Query.destroyQueryPool',
-- 'Vulkan.Core10.Pass.destroyRenderPass',
-- 'Vulkan.Core10.Sampler.destroySampler',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversion',
-- 'Vulkan.Extensions.VK_KHR_sampler_ycbcr_conversion.destroySamplerYcbcrConversionKHR',
-- 'Vulkan.Core10.QueueSemaphore.destroySemaphore',
-- 'Vulkan.Core10.Shader.destroyShaderModule',
-- 'Vulkan.Extensions.VK_KHR_swapchain.destroySwapchainKHR',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.destroyValidationCacheEXT',
-- 'Vulkan.Core10.Queue.deviceWaitIdle',
-- 'Vulkan.Extensions.VK_EXT_display_control.displayPowerControlEXT',
-- 'Vulkan.Core10.Memory.flushMappedMemoryRanges',
-- 'Vulkan.Core10.CommandBuffer.freeCommandBuffers',
-- 'Vulkan.Core10.DescriptorSet.freeDescriptorSets',
-- 'Vulkan.Core10.Memory.freeMemory',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureBuildSizesKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.getAccelerationStructureDeviceAddressKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureHandleNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getAccelerationStructureMemoryRequirementsNV',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.getAndroidHardwareBufferPropertiesANDROID',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferDeviceAddress',
-- 'Vulkan.Extensions.VK_EXT_buffer_device_address.getBufferDeviceAddressEXT',
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferDeviceAddressKHR',
-- 'Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2',
-- 'Vulkan.Extensions.VK_KHR_get_memory_requirements2.getBufferMemoryRequirements2KHR',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddress',
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.getBufferOpaqueCaptureAddressKHR',
-- 'Vulkan.Extensions.VK_EXT_calibrated_timestamps.getCalibratedTimestampsEXT',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.getDeferredOperationMaxConcurrencyKHR',
-- 'Vulkan.Extensions.VK_KHR_deferred_host_operations.getDeferredOperationResultKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance3.getDescriptorSetLayoutSupport',
-- 'Vulkan.Extensions.VK_KHR_maintenance3.getDescriptorSetLayoutSupportKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.getDeviceAccelerationStructureCompatibilityKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.getDeviceGroupPeerMemoryFeatures',
-- 'Vulkan.Extensions.VK_KHR_device_group.getDeviceGroupPeerMemoryFeaturesKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupPresentCapabilitiesKHR',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.getDeviceGroupSurfacePresentModes2EXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getDeviceGroupSurfacePresentModesKHR',
-- 'Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddress',
-- 'Vulkan.Extensions.VK_KHR_buffer_device_address.getDeviceMemoryOpaqueCaptureAddressKHR',
-- 'Vulkan.Core10.DeviceInitialization.getDeviceProcAddr',
-- 'Vulkan.Core10.Queue.getDeviceQueue',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.getDeviceQueue2',
-- 'Vulkan.Core10.Event.getEventStatus',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.getFenceFdKHR',
-- 'Vulkan.Core10.Fence.getFenceStatus',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.getFenceWin32HandleKHR',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.getGeneratedCommandsMemoryRequirementsNV',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.getImageDrmFormatModifierPropertiesEXT',
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageMemoryRequirements2',
-- 'Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageMemoryRequirements2KHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getImageSparseMemoryRequirements',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.getImageSparseMemoryRequirements2',
-- 'Vulkan.Extensions.VK_KHR_get_memory_requirements2.getImageSparseMemoryRequirements2KHR',
-- 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.getImageViewAddressNVX',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.getImageViewHandleNVX',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.getMemoryAndroidHardwareBufferANDROID',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.getMemoryFdPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_external_memory_host.getMemoryHostPointerPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.getMemoryWin32HandleKHR',
-- 'Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.getMemoryWin32HandlePropertiesKHR',
-- 'Vulkan.Extensions.VK_GOOGLE_display_timing.getPastPresentationTimingGOOGLE',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.getPerformanceParameterINTEL',
-- 'Vulkan.Core10.PipelineCache.getPipelineCacheData',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableInternalRepresentationsKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutablePropertiesKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.getPipelineExecutableStatisticsKHR',
-- 'Vulkan.Extensions.VK_EXT_private_data.getPrivateDataEXT',
-- 'Vulkan.Core10.Query.getQueryPoolResults',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.getRayTracingCaptureReplayShaderGroupHandlesKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.getRayTracingShaderGroupHandlesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getRayTracingShaderGroupHandlesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.getRayTracingShaderGroupStackSizeKHR',
-- 'Vulkan.Extensions.VK_GOOGLE_display_timing.getRefreshCycleDurationGOOGLE',
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.getSemaphoreCounterValue',
-- 'Vulkan.Extensions.VK_KHR_timeline_semaphore.getSemaphoreCounterValueKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.getSemaphoreFdKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.getSemaphoreWin32HandleKHR',
-- 'Vulkan.Extensions.VK_AMD_shader_info.getShaderInfoAMD',
-- 'Vulkan.Extensions.VK_EXT_display_control.getSwapchainCounterEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR',
-- 'Vulkan.Extensions.VK_KHR_shared_presentable_image.getSwapchainStatusKHR',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.getValidationCacheDataEXT',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.importFenceFdKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.importFenceWin32HandleKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.importSemaphoreFdKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.importSemaphoreWin32HandleKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.initializePerformanceApiINTEL',
-- 'Vulkan.Core10.Memory.invalidateMappedMemoryRanges',
-- 'Vulkan.Core10.Memory.mapMemory',
-- 'Vulkan.Core10.PipelineCache.mergePipelineCaches',
-- 'Vulkan.Extensions.VK_EXT_validation_cache.mergeValidationCachesEXT',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDeviceEventEXT',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT',
-- 'Vulkan.Extensions.VK_EXT_full_screen_exclusive.releaseFullScreenExclusiveModeEXT',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.releasePerformanceConfigurationINTEL',
-- 'Vulkan.Extensions.VK_KHR_performance_query.releaseProfilingLockKHR',
-- 'Vulkan.Core10.CommandPool.resetCommandPool',
-- 'Vulkan.Core10.DescriptorSet.resetDescriptorPool',
-- 'Vulkan.Core10.Event.resetEvent', 'Vulkan.Core10.Fence.resetFences',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool',
-- 'Vulkan.Extensions.VK_EXT_host_query_reset.resetQueryPoolEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.setDebugUtilsObjectNameEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.setDebugUtilsObjectTagEXT',
-- 'Vulkan.Core10.Event.setEvent',
-- 'Vulkan.Extensions.VK_EXT_hdr_metadata.setHdrMetadataEXT',
-- 'Vulkan.Extensions.VK_AMD_display_native_hdr.setLocalDimmingAMD',
-- 'Vulkan.Extensions.VK_EXT_private_data.setPrivateDataEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.signalSemaphore',
-- 'Vulkan.Extensions.VK_KHR_timeline_semaphore.signalSemaphoreKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance1.trimCommandPool',
-- 'Vulkan.Extensions.VK_KHR_maintenance1.trimCommandPoolKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.uninitializePerformanceApiINTEL',
-- 'Vulkan.Core10.Memory.unmapMemory',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplateKHR',
-- 'Vulkan.Core10.DescriptorSet.updateDescriptorSets',
-- 'Vulkan.Core10.Fence.waitForFences',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.waitSemaphores',
-- 'Vulkan.Extensions.VK_KHR_timeline_semaphore.waitSemaphoresKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.writeAccelerationStructuresPropertiesKHR'
data Device = Device
  { deviceHandle :: Ptr Device_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Device where
  zero = Device zero zero
instance HasObjectType Device where
  objectTypeAndHandle (Device (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_DEVICE, fromIntegral h)


-- | An opaque type for representing pointers to VkQueue handles
data Queue_T
-- | VkQueue - Opaque handle to a queue object
--
-- = See Also
--
-- 'Vulkan.Core10.Queue.getDeviceQueue',
-- 'Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory.getDeviceQueue2',
-- 'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.getQueueCheckpointDataNV',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.queueBeginDebugUtilsLabelEXT',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.queueBindSparse',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.queueEndDebugUtilsLabelEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.queueInsertDebugUtilsLabelEXT',
-- 'Vulkan.Extensions.VK_KHR_swapchain.queuePresentKHR',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.queueSetPerformanceConfigurationINTEL',
-- 'Vulkan.Core10.Queue.queueSubmit', 'Vulkan.Core10.Queue.queueWaitIdle'
data Queue = Queue
  { queueHandle :: Ptr Queue_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero Queue where
  zero = Queue zero zero
instance HasObjectType Queue where
  objectTypeAndHandle (Queue (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_QUEUE, fromIntegral h)


-- | An opaque type for representing pointers to VkCommandBuffer handles
data CommandBuffer_T
-- | VkCommandBuffer - Opaque handle to a command buffer object
--
-- = See Also
--
-- 'Vulkan.Core10.Queue.SubmitInfo',
-- 'Vulkan.Core10.CommandBuffer.allocateCommandBuffers',
-- 'Vulkan.Core10.CommandBuffer.beginCommandBuffer',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.cmdBeginConditionalRenderingEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.cmdBeginDebugUtilsLabelEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginRenderPass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdBeginRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdBeginRenderPass2KHR',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdBindPipelineShaderGroupNV',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdBlitImage2KHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresIndirectKHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdBuildAccelerationStructuresKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearAttachments',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyAccelerationStructureKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdCopyAccelerationStructureNV',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyAccelerationStructureToMemoryKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBuffer2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyBufferToImage2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImage2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdCopyImageToBuffer2KHR',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdCopyMemoryToAccelerationStructureKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerBeginEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerEndEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_marker.cmdDebugMarkerInsertEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatch',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdDispatchBase',
-- 'Vulkan.Extensions.VK_KHR_device_group.cmdDispatchBaseKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDraw',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexed',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksNV',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.cmdEndConditionalRenderingEXT',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.cmdEndDebugUtilsLabelEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndRenderPass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdEndRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdEndRenderPass2KHR',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdExecuteCommands',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdExecuteGeneratedCommandsNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Vulkan.Extensions.VK_EXT_debug_utils.cmdInsertDebugUtilsLabelEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdNextSubpass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.cmdNextSubpass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.cmdNextSubpass2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPipelineBarrier',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdPreprocessGeneratedCommandsNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPushConstants',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.cmdResolveImage2KHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetBlendConstants',
-- 'Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints.cmdSetCheckpointNV',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetCoarseSampleOrderNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetCullModeEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBias',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetDepthBounds',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthBoundsTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthCompareOpEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthTestEnableEXT',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetDepthWriteEnableEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_group.cmdSetDeviceMask',
-- 'Vulkan.Extensions.VK_KHR_device_group.cmdSetDeviceMaskKHR',
-- 'Vulkan.Extensions.VK_EXT_discard_rectangles.cmdSetDiscardRectangleEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent',
-- 'Vulkan.Extensions.VK_NV_scissor_exclusive.cmdSetExclusiveScissorNV',
-- 'Vulkan.Extensions.VK_NV_fragment_shading_rate_enums.cmdSetFragmentShadingRateEnumNV',
-- 'Vulkan.Extensions.VK_KHR_fragment_shading_rate.cmdSetFragmentShadingRateKHR',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetFrontFaceEXT',
-- 'Vulkan.Extensions.VK_EXT_line_rasterization.cmdSetLineStippleEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetLineWidth',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.cmdSetPerformanceMarkerINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.cmdSetPerformanceOverrideINTEL',
-- 'Vulkan.Extensions.VK_INTEL_performance_query.cmdSetPerformanceStreamMarkerINTEL',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetPrimitiveTopologyEXT',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdSetRayTracingPipelineStackSizeKHR',
-- 'Vulkan.Extensions.VK_EXT_sample_locations.cmdSetSampleLocationsEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetScissor',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetScissorWithCountEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilCompareMask',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilOpEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilReference',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetStencilTestEnableEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetStencilWriteMask',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetViewport',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdSetViewportShadingRatePaletteNV',
-- 'Vulkan.Extensions.VK_NV_clip_space_w_scaling.cmdSetViewportWScalingNV',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdSetViewportWithCountEXT',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysIndirectKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.cmdTraceRaysKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp',
-- 'Vulkan.Core10.CommandBuffer.endCommandBuffer',
-- 'Vulkan.Core10.CommandBuffer.freeCommandBuffers',
-- 'Vulkan.Core10.CommandBuffer.resetCommandBuffer'
data CommandBuffer = CommandBuffer
  { commandBufferHandle :: Ptr CommandBuffer_T
  , deviceCmds :: DeviceCmds
  }
  deriving stock (Eq, Show)
  deriving anyclass (IsHandle)
instance Zero CommandBuffer where
  zero = CommandBuffer zero zero
instance HasObjectType CommandBuffer where
  objectTypeAndHandle (CommandBuffer (ptrToWordPtr -> WordPtr h) _) = (OBJECT_TYPE_COMMAND_BUFFER, fromIntegral h)


-- | VkDeviceMemory - Opaque handle to a device memory object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_NV_ray_tracing.BindAccelerationStructureMemoryInfoNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.DeviceMemoryOpaqueCaptureAddressInfo',
-- 'Vulkan.Core10.Memory.MappedMemoryRange',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.MemoryGetAndroidHardwareBufferInfoANDROID',
-- 'Vulkan.Extensions.VK_KHR_external_memory_fd.MemoryGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_memory_win32.MemoryGetWin32HandleInfoKHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBind',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseMemoryBind',
-- 'Vulkan.Extensions.VK_KHR_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoKHR',
-- 'Vulkan.Extensions.VK_NV_win32_keyed_mutex.Win32KeyedMutexAcquireReleaseInfoNV',
-- 'Vulkan.Core10.Memory.allocateMemory',
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Vulkan.Core10.Memory.freeMemory',
-- 'Vulkan.Core10.Memory.getDeviceMemoryCommitment',
-- 'Vulkan.Extensions.VK_NV_external_memory_win32.getMemoryWin32HandleNV',
-- 'Vulkan.Core10.Memory.mapMemory', 'Vulkan.Core10.Memory.unmapMemory'
newtype DeviceMemory = DeviceMemory Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DeviceMemory where
  objectTypeAndHandle (DeviceMemory h) = (OBJECT_TYPE_DEVICE_MEMORY, h)
instance Show DeviceMemory where
  showsPrec p (DeviceMemory x) = showParen (p >= 11) (showString "DeviceMemory 0x" . showHex x)


-- | VkCommandPool - Opaque handle to a command pool object
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBuffer.CommandBufferAllocateInfo',
-- 'Vulkan.Core10.CommandPool.createCommandPool',
-- 'Vulkan.Core10.CommandPool.destroyCommandPool',
-- 'Vulkan.Core10.CommandBuffer.freeCommandBuffers',
-- 'Vulkan.Core10.CommandPool.resetCommandPool',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_maintenance1.trimCommandPool',
-- 'Vulkan.Extensions.VK_KHR_maintenance1.trimCommandPoolKHR'
newtype CommandPool = CommandPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType CommandPool where
  objectTypeAndHandle (CommandPool h) = (OBJECT_TYPE_COMMAND_POOL, h)
instance Show CommandPool where
  showsPrec p (CommandPool x) = showParen (p >= 11) (showString "CommandPool 0x" . showHex x)


-- | VkBuffer - Opaque handle to a buffer object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureCreateInfoKHR',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindBufferMemoryInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address.BufferDeviceAddressInfo',
-- 'Vulkan.Core10.OtherTypes.BufferMemoryBarrier',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.BufferMemoryRequirementsInfo2',
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_conditional_rendering.ConditionalRenderingBeginInfoEXT',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.CopyBufferInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.CopyBufferToImageInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.CopyImageToBufferInfo2KHR',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV',
-- 'Vulkan.Core10.DescriptorSet.DescriptorBufferInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryAABBNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsStreamNV',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseBufferMemoryBindInfo',
-- 'Vulkan.Core10.MemoryManagement.bindBufferMemory',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindIndexBuffer',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBindTransformFeedbackBuffersEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindVertexBuffers',
-- 'Vulkan.Extensions.VK_EXT_extended_dynamic_state.cmdBindVertexBuffers2EXT',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdBuildAccelerationStructureNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDispatchIndirect',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndexedIndirect',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndexedIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndexedIndirectCountKHR',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdDrawIndirect',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdDrawIndirectByteCountEXT',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_draw_indirect_count.cmdDrawIndirectCount',
-- 'Vulkan.Extensions.VK_AMD_draw_indirect_count.cmdDrawIndirectCountAMD',
-- 'Vulkan.Extensions.VK_KHR_draw_indirect_count.cmdDrawIndirectCountKHR',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectCountNV',
-- 'Vulkan.Extensions.VK_NV_mesh_shader.cmdDrawMeshTasksIndirectNV',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndTransformFeedbackEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdFillBuffer',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdTraceRaysNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdUpdateBuffer',
-- 'Vulkan.Extensions.VK_AMD_buffer_marker.cmdWriteBufferMarkerAMD',
-- 'Vulkan.Core10.Buffer.createBuffer',
-- 'Vulkan.Core10.Buffer.destroyBuffer',
-- 'Vulkan.Core10.MemoryManagement.getBufferMemoryRequirements'
newtype Buffer = Buffer Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Buffer where
  objectTypeAndHandle (Buffer h) = (OBJECT_TYPE_BUFFER, h)
instance Show Buffer where
  showsPrec p (Buffer x) = showParen (p >= 11) (showString "Buffer 0x" . showHex x)


-- | VkBufferView - Opaque handle to a buffer view object
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Vulkan.Core10.BufferView.createBufferView',
-- 'Vulkan.Core10.BufferView.destroyBufferView'
newtype BufferView = BufferView Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType BufferView where
  objectTypeAndHandle (BufferView h) = (OBJECT_TYPE_BUFFER_VIEW, h)
instance Show BufferView where
  showsPrec p (BufferView x) = showParen (p >= 11) (showString "BufferView 0x" . showHex x)


-- | VkImage - Opaque handle to an image object
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.BindImageMemoryInfo',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BlitImageInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.CopyBufferToImageInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.CopyImageInfo2KHR',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.CopyImageToBufferInfo2KHR',
-- 'Vulkan.Extensions.VK_NV_dedicated_allocation.DedicatedAllocationMemoryAllocateInfoNV',
-- 'Vulkan.Core10.OtherTypes.ImageMemoryBarrier',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageMemoryRequirementsInfo2',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_memory_requirements2.ImageSparseMemoryRequirementsInfo2',
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_dedicated_allocation.MemoryDedicatedAllocateInfo',
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.ResolveImageInfo2KHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageMemoryBindInfo',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.SparseImageOpaqueMemoryBindInfo',
-- 'Vulkan.Core10.MemoryManagement.bindImageMemory',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearColorImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdClearDepthStencilImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyBufferToImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImage',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyImageToBuffer',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResolveImage',
-- 'Vulkan.Core10.Image.createImage', 'Vulkan.Core10.Image.destroyImage',
-- 'Vulkan.Extensions.VK_EXT_image_drm_format_modifier.getImageDrmFormatModifierPropertiesEXT',
-- 'Vulkan.Core10.MemoryManagement.getImageMemoryRequirements',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getImageSparseMemoryRequirements',
-- 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- 'Vulkan.Extensions.VK_KHR_swapchain.getSwapchainImagesKHR'
newtype Image = Image Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Image where
  objectTypeAndHandle (Image h) = (OBJECT_TYPE_IMAGE, h)
instance Show Image where
  showsPrec p (Image x) = showParen (p >= 11) (showString "Image 0x" . showHex x)


-- | VkImageView - Opaque handle to an image view object
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
-- 'Vulkan.Core10.Pass.FramebufferCreateInfo',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.RenderPassAttachmentBeginInfo',
-- 'Vulkan.Extensions.VK_NV_shading_rate_image.cmdBindShadingRateImageNV',
-- 'Vulkan.Core10.ImageView.createImageView',
-- 'Vulkan.Core10.ImageView.destroyImageView',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.getImageViewAddressNVX'
newtype ImageView = ImageView Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType ImageView where
  objectTypeAndHandle (ImageView h) = (OBJECT_TYPE_IMAGE_VIEW, h)
instance Show ImageView where
  showsPrec p (ImageView x) = showParen (p >= 11) (showString "ImageView 0x" . showHex x)


-- | VkShaderModule - Opaque handle to a shader module object
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.PipelineShaderStageCreateInfo',
-- 'Vulkan.Core10.Shader.createShaderModule',
-- 'Vulkan.Core10.Shader.destroyShaderModule'
newtype ShaderModule = ShaderModule Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType ShaderModule where
  objectTypeAndHandle (ShaderModule h) = (OBJECT_TYPE_SHADER_MODULE, h)
instance Show ShaderModule where
  showsPrec p (ShaderModule x) = showParen (p >= 11) (showString "ShaderModule 0x" . showHex x)


-- | VkPipeline - Opaque handle to a pipeline object
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsInfoNV',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GeneratedCommandsMemoryRequirementsInfoNV',
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.GraphicsPipelineShaderGroupsCreateInfoNV',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineExecutableInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_executable_properties.PipelineInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_pipeline_library.PipelineLibraryCreateInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindPipeline',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.cmdBindPipelineShaderGroupNV',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.compileDeferredNV',
-- 'Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Vulkan.Core10.Pipeline.destroyPipeline',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.getRayTracingCaptureReplayShaderGroupHandlesKHR',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.getRayTracingShaderGroupHandlesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.getRayTracingShaderGroupHandlesNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.getRayTracingShaderGroupStackSizeKHR',
-- 'Vulkan.Extensions.VK_AMD_shader_info.getShaderInfoAMD'
newtype Pipeline = Pipeline Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Pipeline where
  objectTypeAndHandle (Pipeline h) = (OBJECT_TYPE_PIPELINE, h)
instance Show Pipeline where
  showsPrec p (Pipeline x) = showParen (p >= 11) (showString "Pipeline 0x" . showHex x)


-- | VkPipelineLayout - Opaque handle to a pipeline layout object
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.ComputePipelineCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Vulkan.Extensions.VK_NV_device_generated_commands.IndirectCommandsLayoutTokenNV',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.RayTracingPipelineCreateInfoKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.RayTracingPipelineCreateInfoNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdPushConstants',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetKHR',
-- 'Vulkan.Extensions.VK_KHR_push_descriptor.cmdPushDescriptorSetWithTemplateKHR',
-- 'Vulkan.Core10.PipelineLayout.createPipelineLayout',
-- 'Vulkan.Core10.PipelineLayout.destroyPipelineLayout'
newtype PipelineLayout = PipelineLayout Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PipelineLayout where
  objectTypeAndHandle (PipelineLayout h) = (OBJECT_TYPE_PIPELINE_LAYOUT, h)
instance Show PipelineLayout where
  showsPrec p (PipelineLayout x) = showParen (p >= 11) (showString "PipelineLayout 0x" . showHex x)


-- | VkSampler - Opaque handle to a sampler object
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.DescriptorImageInfo',
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Vulkan.Core10.Sampler.createSampler',
-- 'Vulkan.Core10.Sampler.destroySampler'
newtype Sampler = Sampler Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Sampler where
  objectTypeAndHandle (Sampler h) = (OBJECT_TYPE_SAMPLER, h)
instance Show Sampler where
  showsPrec p (Sampler x) = showParen (p >= 11) (showString "Sampler 0x" . showHex x)


-- | VkDescriptorSet - Opaque handle to a descriptor set object
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.CopyDescriptorSet',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet',
-- 'Vulkan.Core10.DescriptorSet.allocateDescriptorSets',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBindDescriptorSets',
-- 'Vulkan.Core10.DescriptorSet.freeDescriptorSets',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplate',
-- 'Vulkan.Extensions.VK_KHR_descriptor_update_template.updateDescriptorSetWithTemplateKHR'
newtype DescriptorSet = DescriptorSet Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DescriptorSet where
  objectTypeAndHandle (DescriptorSet h) = (OBJECT_TYPE_DESCRIPTOR_SET, h)
instance Show DescriptorSet where
  showsPrec p (DescriptorSet x) = showParen (p >= 11) (showString "DescriptorSet 0x" . showHex x)


-- | VkDescriptorSetLayout - Opaque handle to a descriptor set layout object
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateCreateInfo',
-- 'Vulkan.Core10.PipelineLayout.PipelineLayoutCreateInfo',
-- 'Vulkan.Core10.DescriptorSet.createDescriptorSetLayout',
-- 'Vulkan.Core10.DescriptorSet.destroyDescriptorSetLayout'
newtype DescriptorSetLayout = DescriptorSetLayout Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DescriptorSetLayout where
  objectTypeAndHandle (DescriptorSetLayout h) = (OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT, h)
instance Show DescriptorSetLayout where
  showsPrec p (DescriptorSetLayout x) = showParen (p >= 11) (showString "DescriptorSetLayout 0x" . showHex x)


-- | VkDescriptorPool - Opaque handle to a descriptor pool object
--
-- = See Also
--
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetAllocateInfo',
-- 'Vulkan.Core10.DescriptorSet.createDescriptorPool',
-- 'Vulkan.Core10.DescriptorSet.destroyDescriptorPool',
-- 'Vulkan.Core10.DescriptorSet.freeDescriptorSets',
-- 'Vulkan.Core10.DescriptorSet.resetDescriptorPool'
newtype DescriptorPool = DescriptorPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType DescriptorPool where
  objectTypeAndHandle (DescriptorPool h) = (OBJECT_TYPE_DESCRIPTOR_POOL, h)
instance Show DescriptorPool where
  showsPrec p (DescriptorPool x) = showParen (p >= 11) (showString "DescriptorPool 0x" . showHex x)


-- | VkFence - Opaque handle to a fence object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.FenceGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.FenceGetWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_fd.ImportFenceFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_fence_win32.ImportFenceWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Vulkan.Core10.Fence.createFence', 'Vulkan.Core10.Fence.destroyFence',
-- 'Vulkan.Core10.Fence.getFenceStatus',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.queueBindSparse',
-- 'Vulkan.Core10.Queue.queueSubmit',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDeviceEventEXT',
-- 'Vulkan.Extensions.VK_EXT_display_control.registerDisplayEventEXT',
-- 'Vulkan.Core10.Fence.resetFences', 'Vulkan.Core10.Fence.waitForFences'
newtype Fence = Fence Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Fence where
  objectTypeAndHandle (Fence h) = (OBJECT_TYPE_FENCE, h)
instance Show Fence where
  showsPrec p (Fence x) = showParen (p >= 11) (showString "Fence 0x" . showHex x)


-- | VkSemaphore - Opaque handle to a semaphore object
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_swapchain.AcquireNextImageInfoKHR',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.BindSparseInfo',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.ImportSemaphoreFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.ImportSemaphoreWin32HandleInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.PresentInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_fd.SemaphoreGetFdInfoKHR',
-- 'Vulkan.Extensions.VK_KHR_external_semaphore_win32.SemaphoreGetWin32HandleInfoKHR',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreSignalInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.SemaphoreWaitInfo',
-- 'Vulkan.Core10.Queue.SubmitInfo',
-- 'Vulkan.Extensions.VK_KHR_swapchain.acquireNextImageKHR',
-- 'Vulkan.Core10.QueueSemaphore.createSemaphore',
-- 'Vulkan.Core10.QueueSemaphore.destroySemaphore',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_timeline_semaphore.getSemaphoreCounterValue',
-- 'Vulkan.Extensions.VK_KHR_timeline_semaphore.getSemaphoreCounterValueKHR'
newtype Semaphore = Semaphore Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Semaphore where
  objectTypeAndHandle (Semaphore h) = (OBJECT_TYPE_SEMAPHORE, h)
instance Show Semaphore where
  showsPrec p (Semaphore x) = showParen (p >= 11) (showString "Semaphore 0x" . showHex x)


-- | VkEvent - Opaque handle to an event object
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetEvent',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdSetEvent',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWaitEvents',
-- 'Vulkan.Core10.Event.createEvent', 'Vulkan.Core10.Event.destroyEvent',
-- 'Vulkan.Core10.Event.getEventStatus', 'Vulkan.Core10.Event.resetEvent',
-- 'Vulkan.Core10.Event.setEvent'
newtype Event = Event Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Event where
  objectTypeAndHandle (Event h) = (OBJECT_TYPE_EVENT, h)
instance Show Event where
  showsPrec p (Event x) = showParen (p >= 11) (showString "Event 0x" . showHex x)


-- | VkQueryPool - Opaque handle to a query pool object
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBeginQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdBeginQueryIndexedEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdEndQuery',
-- 'Vulkan.Extensions.VK_EXT_transform_feedback.cmdEndQueryIndexedEXT',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdResetQueryPool',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdWriteTimestamp',
-- 'Vulkan.Core10.Query.createQueryPool',
-- 'Vulkan.Core10.Query.destroyQueryPool',
-- 'Vulkan.Core10.Query.getQueryPoolResults',
-- 'Vulkan.Core12.Promoted_From_VK_EXT_host_query_reset.resetQueryPool',
-- 'Vulkan.Extensions.VK_EXT_host_query_reset.resetQueryPoolEXT'
newtype QueryPool = QueryPool Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType QueryPool where
  objectTypeAndHandle (QueryPool h) = (OBJECT_TYPE_QUERY_POOL, h)
instance Show QueryPool where
  showsPrec p (QueryPool x) = showParen (p >= 11) (showString "QueryPool 0x" . showHex x)


-- | VkFramebuffer - Opaque handle to a framebuffer object
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Vulkan.Core10.Pass.createFramebuffer',
-- 'Vulkan.Core10.Pass.destroyFramebuffer'
newtype Framebuffer = Framebuffer Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType Framebuffer where
  objectTypeAndHandle (Framebuffer h) = (OBJECT_TYPE_FRAMEBUFFER, h)
instance Show Framebuffer where
  showsPrec p (Framebuffer x) = showParen (p >= 11) (showString "Framebuffer 0x" . showHex x)


-- | VkRenderPass - Opaque handle to a render pass object
--
-- = See Also
--
-- 'Vulkan.Core10.CommandBuffer.CommandBufferInheritanceInfo',
-- 'Vulkan.Core10.Pass.FramebufferCreateInfo',
-- 'Vulkan.Core10.Pipeline.GraphicsPipelineCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.RenderPassBeginInfo',
-- 'Vulkan.Core10.Pass.createRenderPass',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.createRenderPass2',
-- 'Vulkan.Extensions.VK_KHR_create_renderpass2.createRenderPass2KHR',
-- 'Vulkan.Core10.Pass.destroyRenderPass',
-- 'Vulkan.Core10.Pass.getRenderAreaGranularity'
newtype RenderPass = RenderPass Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType RenderPass where
  objectTypeAndHandle (RenderPass h) = (OBJECT_TYPE_RENDER_PASS, h)
instance Show RenderPass where
  showsPrec p (RenderPass x) = showParen (p >= 11) (showString "RenderPass 0x" . showHex x)


-- | VkPipelineCache - Opaque handle to a pipeline cache object
--
-- = See Also
--
-- 'Vulkan.Core10.Pipeline.createComputePipelines',
-- 'Vulkan.Core10.Pipeline.createGraphicsPipelines',
-- 'Vulkan.Core10.PipelineCache.createPipelineCache',
-- 'Vulkan.Extensions.VK_KHR_ray_tracing_pipeline.createRayTracingPipelinesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.createRayTracingPipelinesNV',
-- 'Vulkan.Core10.PipelineCache.destroyPipelineCache',
-- 'Vulkan.Core10.PipelineCache.getPipelineCacheData',
-- 'Vulkan.Core10.PipelineCache.mergePipelineCaches'
newtype PipelineCache = PipelineCache Word64
  deriving newtype (Eq, Ord, Storable, Zero)
  deriving anyclass (IsHandle)
instance HasObjectType PipelineCache where
  objectTypeAndHandle (PipelineCache h) = (OBJECT_TYPE_PIPELINE_CACHE, h)
instance Show PipelineCache where
  showsPrec p (PipelineCache x) = showParen (p >= 11) (showString "PipelineCache 0x" . showHex x)

