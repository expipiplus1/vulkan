{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.DeviceInitialization
  ( PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VkAllocationCallbacks
  , VkApplicationInfo
  , VkDevice
  , VkDeviceSize
  , VkExtent3D
  , VkFormatFeatureFlagBits
  , VkFormatFeatureFlags
  , VkFormatProperties
  , VkImageCreateFlagBits
  , VkImageCreateFlags
  , VkImageFormatProperties
  , VkImageTiling
  , VkImageType
  , VkImageUsageFlagBits
  , VkImageUsageFlags
  , VkInstance
  , VkInstanceCreateFlags
  , VkInstanceCreateInfo
  , VkInternalAllocationType
  , VkMemoryHeap
  , VkMemoryHeapFlagBits
  , VkMemoryHeapFlags
  , VkMemoryPropertyFlagBits
  , VkMemoryPropertyFlags
  , VkMemoryType
  , VkPhysicalDevice
  , VkPhysicalDeviceFeatures
  , VkPhysicalDeviceLimits
  , VkPhysicalDeviceMemoryProperties
  , VkPhysicalDeviceProperties
  , VkPhysicalDeviceSparseProperties
  , VkPhysicalDeviceType
  , VkQueueFamilyProperties
  , VkQueueFlagBits
  , VkQueueFlags
  , VkSampleCountFlagBits
  , VkSampleCountFlags
  , VkSystemAllocationScope
  , FN_vkCreateInstance
  , PFN_vkCreateInstance
  , FN_vkDestroyInstance
  , PFN_vkDestroyInstance
  , FN_vkEnumeratePhysicalDevices
  , PFN_vkEnumeratePhysicalDevices
  , FN_vkGetDeviceProcAddr
  , PFN_vkGetDeviceProcAddr
  , FN_vkGetInstanceProcAddr
  , PFN_vkGetInstanceProcAddr
  , vkGetInstanceProcAddr
  , FN_vkGetPhysicalDeviceFeatures
  , PFN_vkGetPhysicalDeviceFeatures
  , FN_vkGetPhysicalDeviceFormatProperties
  , PFN_vkGetPhysicalDeviceFormatProperties
  , FN_vkGetPhysicalDeviceImageFormatProperties
  , PFN_vkGetPhysicalDeviceImageFormatProperties
  , FN_vkGetPhysicalDeviceMemoryProperties
  , PFN_vkGetPhysicalDeviceMemoryProperties
  , FN_vkGetPhysicalDeviceProperties
  , PFN_vkGetPhysicalDeviceProperties
  , FN_vkGetPhysicalDeviceQueueFamilyProperties
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties
  ) where

import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CChar(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkFormat
  , VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Dynamic
  ( InstanceCmds
  )


-- | PFN_vkAllocationFunction - Application-defined memory allocation
-- function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @size@ is the size in bytes of the requested allocation.
--
-- -   @alignment@ is the requested alignment of the allocation in bytes
--     and /must/ be a power of two.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = Description
--
-- If @pfnAllocation@ is unable to allocate the requested memory, it /must/
-- return @NULL@. If the allocation was successful, it /must/ return a
-- valid pointer to memory allocation containing at least @size@ bytes, and
-- with the pointer value being a multiple of @alignment@.
--
-- __Note__
--
-- Correct Vulkan operation /cannot/ be assumed if the application does not
-- follow these rules.
--
-- For example, @pfnAllocation@ (or @pfnReallocation@) could cause
-- termination of running Vulkan instance(s) on a failed allocation for
-- debugging purposes, either directly or indirectly. In these
-- circumstances, it /cannot/ be assumed that any part of any affected
-- 'VkInstance' objects are going to operate correctly (even
-- 'vkDestroyInstance'), and the application /must/ ensure it cleans up
-- properly via other means (e.g. process termination).
--
-- If @pfnAllocation@ returns @NULL@, and if the implementation is unable
-- to continue correct processing of the current command without the
-- requested allocation, it /must/ treat this as a run-time error, and
-- generate 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY' at
-- the appropriate time for the command in which the condition was
-- detected, as described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fundamentals-errorcodes Return Codes>.
--
-- If the implementation is able to continue correct processing of the
-- current command without the requested allocation, then it /may/ do so,
-- and /must/ not generate
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY' as a result
-- of this failed allocation.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkAllocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))

-- | PFN_vkFreeFunction - Application-defined memory free function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @pMemory@ is the allocation to be freed.
--
-- = Description
--
-- @pMemory@ /may/ be @NULL@, which the callback /must/ handle safely. If
-- @pMemory@ is non-@NULL@, it /must/ be a pointer previously allocated by
-- @pfnAllocation@ or @pfnReallocation@. The application /should/ free this
-- memory.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkFreeFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pMemory" ::: Ptr ()) -> IO ())

-- | PFN_vkInternalAllocationNotification - Application-defined memory
-- allocation notification function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @size@ is the requested size of an allocation.
--
-- -   @allocationType@ is a 'VkInternalAllocationType' value specifying
--     the requested type of an allocation.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = Description
--
-- This is a purely informational callback.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkInternalAllocationNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())

-- | PFN_vkInternalFreeNotification - Application-defined memory free
-- notification function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @size@ is the requested size of an allocation.
--
-- -   @allocationType@ is a 'VkInternalAllocationType' value specifying
--     the requested type of an allocation.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkInternalFreeNotification = Ptr (("pUserData" ::: Ptr ()) -> ("size" ::: CSize) -> ("allocationType" ::: VkInternalAllocationType) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO ())

-- | PFN_vkReallocationFunction - Application-defined memory reallocation
-- function
--
-- = Parameters
--
-- -   @pUserData@ is the value specified for
--     'VkAllocationCallbacks'::@pUserData@ in the allocator specified by
--     the application.
--
-- -   @pOriginal@ /must/ be either @NULL@ or a pointer previously returned
--     by @pfnReallocation@ or @pfnAllocation@ of the same allocator.
--
-- -   @size@ is the size in bytes of the requested allocation.
--
-- -   @alignment@ is the requested alignment of the allocation in bytes
--     and /must/ be a power of two.
--
-- -   @allocationScope@ is a 'VkSystemAllocationScope' value specifying
--     the allocation scope of the lifetime of the allocation, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-host-allocation-scope here>.
--
-- = Description
--
-- @pfnReallocation@ /must/ return an allocation with enough space for
-- @size@ bytes, and the contents of the original allocation from bytes
-- zero to min(original size, new size) - 1 /must/ be preserved in the
-- returned allocation. If @size@ is larger than the old size, the contents
-- of the additional space are undefined. If satisfying these requirements
-- involves creating a new allocation, then the old allocation /should/ be
-- freed.
--
-- If @pOriginal@ is @NULL@, then @pfnReallocation@ /must/ behave
-- equivalently to a call to 'PFN_vkAllocationFunction' with the same
-- parameter values (without @pOriginal@).
--
-- If @size@ is zero, then @pfnReallocation@ /must/ behave equivalently to
-- a call to 'PFN_vkFreeFunction' with the same @pUserData@ parameter
-- value, and @pMemory@ equal to @pOriginal@.
--
-- If @pOriginal@ is non-@NULL@, the implementation /must/ ensure that
-- @alignment@ is equal to the @alignment@ used to originally allocate
-- @pOriginal@.
--
-- If this function fails and @pOriginal@ is non-@NULL@ the application
-- /must/ not free the old allocation.
--
-- @pfnReallocation@ /must/ follow the same
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vkAllocationFunction_return_rules rules for return values as >.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
type PFN_vkReallocationFunction = Ptr (("pUserData" ::: Ptr ()) -> ("pOriginal" ::: Ptr ()) -> ("size" ::: CSize) -> ("alignment" ::: CSize) -> ("allocationScope" ::: VkSystemAllocationScope) -> IO (Ptr ()))

-- | PFN_vkVoidFunction - Dummy function pointer type returned by queries
--
-- = See Also
--
-- 'vkGetDeviceProcAddr', 'vkGetInstanceProcAddr'
type PFN_vkVoidFunction = Ptr (() -> IO ())

data VkAllocationCallbacks

data VkApplicationInfo

-- | Dummy data to tag the 'Ptr' with
data VkDevice_T
-- | VkDevice - Opaque handle to a device object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkAcquireFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImage2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkAcquireNextImageKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkBindAccelerationStructureMemoryNV',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2.vkBindBufferMemory2KHR',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2.vkBindImageMemory2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCompileDeferredNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.C.Core10.Event.vkCreateEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCreateIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCreateObjectTableNVX',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Query.vkCreateQueryPool',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateRayTracingPipelinesNV',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.vkCreateRenderPass2KHR',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain.vkCreateSharedSwapchainsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkDebugMarkerSetObjectNameEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker.vkDebugMarkerSetObjectTagEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkDestroyAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkDestroyBufferView',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.C.Core10.Device.vkDestroyDevice',
-- 'Graphics.Vulkan.C.Core10.Event.vkDestroyEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Image.vkDestroyImage',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkDestroyImageView',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkDestroyIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkDestroyObjectTableNVX',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkDestroyPipeline',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkDestroyPipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkDestroyPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Query.vkDestroyQueryPool',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkDestroySampler',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversionKHR',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore',
-- 'Graphics.Vulkan.C.Core10.Shader.vkDestroyShaderModule',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkDestroySwapchainKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkDestroyValidationCacheEXT',
-- 'Graphics.Vulkan.C.Core10.Queue.vkDeviceWaitIdle',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkDisplayPowerControlEXT',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureHandleNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetAccelerationStructureMemoryRequirementsNV',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address.vkGetBufferDeviceAddressEXT',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.vkGetCalibratedTimestampsEXT',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupportKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeatures',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeaturesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupPresentCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group.vkGetDeviceGroupSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetDeviceGroupSurfacePresentModesKHR',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'vkGetDeviceProcAddr',
-- 'Graphics.Vulkan.C.Core10.Queue.vkGetDeviceQueue',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.C.Core10.Event.vkGetEventStatus',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.vkGetFenceFdKHR',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.vkGetFenceWin32HandleKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.vkGetImageDrmFormatModifierPropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2KHR',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2KHR',
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle.vkGetImageViewHandleNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.vkGetMemoryAndroidHardwareBufferANDROID',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd.vkGetMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.vkGetMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandleKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32.vkGetMemoryWin32HandleNV',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32.vkGetMemoryWin32HandlePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetPastPresentationTimingGOOGLE',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData',
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkGetRayTracingShaderGroupHandlesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing.vkGetRefreshCycleDurationGOOGLE',
-- 'Graphics.Vulkan.C.Core10.Pass.vkGetRenderAreaGranularity',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.vkGetSemaphoreFdKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.vkGetSemaphoreWin32HandleKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_shader_info.vkGetShaderInfoAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkGetSwapchainCounterEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetSwapchainImagesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.vkGetSwapchainStatusKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkGetValidationCacheDataEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd.vkImportFenceFdKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32.vkImportFenceWin32HandleKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd.vkImportSemaphoreFdKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32.vkImportSemaphoreWin32HandleKHR',
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges',
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkMergePipelineCaches',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkMergeValidationCachesEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDeviceEventEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkRegisterObjectsNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkReleaseFullScreenExclusiveModeEXT',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.Event.vkResetEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset.vkResetQueryPoolEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkSetDebugUtilsObjectNameEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkSetDebugUtilsObjectTagEXT',
-- 'Graphics.Vulkan.C.Core10.Event.vkSetEvent',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata.vkSetHdrMetadataEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr.vkSetLocalDimmingAMD',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_maintenance1.vkTrimCommandPoolKHR',
-- 'Graphics.Vulkan.C.Core10.Memory.vkUnmapMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkUnregisterObjectsNVX',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplateKHR',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type VkDevice = Ptr VkDevice_T

-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkAccelerationStructureCreateInfoNV',
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkBindAccelerationStructureMemoryInfoNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferCopy',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering.VkConditionalRenderingBeginInfoEXT',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryAABBNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.VkGeometryTrianglesNV',
-- 'VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo', 'VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget.VkPhysicalDeviceMemoryBudgetPropertiesEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.VkPhysicalDeviceTransformFeedbackPropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBeginTransformFeedbackEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdBindTransformFeedbackBuffersEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdBuildAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndexedIndirectCountAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndexedIndirectCountKHR',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdDrawIndirectByteCountEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count.vkCmdDrawIndirectCountAMD',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count.vkCmdDrawIndirectCountKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectCountNV',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader.vkCmdDrawMeshTasksIndirectNV',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback.vkCmdEndTransformFeedbackEXT',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCmdTraceRaysNV',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker.vkCmdWriteBufferMarkerAMD',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'
type VkDeviceSize = Word64

data VkExtent3D

data VkFormatFeatureFlagBits

-- | VkFormatFeatureFlags - Bitmask of VkFormatFeatureFlagBits
--
-- = Description
--
-- 'VkFormatFeatureFlags' is a bitmask type for setting a mask of zero or
-- more 'VkFormatFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkDrmFormatModifierPropertiesEXT',
-- 'VkFormatFeatureFlagBits', 'VkFormatProperties'
type VkFormatFeatureFlags = VkFormatFeatureFlagBits

data VkFormatProperties

data VkImageCreateFlagBits

-- | VkImageCreateFlags - Bitmask of VkImageCreateFlagBits
--
-- = Description
--
-- 'VkImageCreateFlags' is a bitmask type for setting a mask of zero or
-- more 'VkImageCreateFlagBits'.
--
-- = See Also
--
-- 'VkImageCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties'
type VkImageCreateFlags = VkImageCreateFlagBits

data VkImageFormatProperties

data VkImageTiling

data VkImageType

data VkImageUsageFlagBits

-- | VkImageUsageFlags - Bitmask of VkImageUsageFlagBits
--
-- = Description
--
-- 'VkImageUsageFlags' is a bitmask type for setting a mask of zero or more
-- 'VkImageUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT',
-- 'VkImageUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VkSharedPresentSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type VkImageUsageFlags = VkImageUsageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkInstance_T
-- | VkInstance - Opaque handle to an instance object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkCreateDebugReportCallbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCreateDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.vkCreateHeadlessSurfaceEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK',
-- 'Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface.vkCreateImagePipeSurfaceFUCHSIA',
-- 'vkCreateInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.vkCreateMetalSurfaceEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.vkCreateStreamDescriptorSurfaceGGP',
-- 'Graphics.Vulkan.C.Extensions.VK_NN_vi_surface.vkCreateViSurfaceNN',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDebugReportMessageEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDestroyDebugReportCallbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT',
-- 'vkDestroyInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkDestroySurfaceKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroups',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroupsKHR',
-- 'vkEnumeratePhysicalDevices', 'vkGetInstanceProcAddr',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkSubmitDebugUtilsMessageEXT'
type VkInstance = Ptr VkInstance_T

data VkInstanceCreateFlags

data VkInstanceCreateInfo

data VkInternalAllocationType

data VkMemoryHeap

data VkMemoryHeapFlagBits

-- | VkMemoryHeapFlags - Bitmask of VkMemoryHeapFlagBits
--
-- = Description
--
-- 'VkMemoryHeapFlags' is a bitmask type for setting a mask of zero or more
-- 'VkMemoryHeapFlagBits'.
--
-- = See Also
--
-- 'VkMemoryHeap', 'VkMemoryHeapFlagBits'
type VkMemoryHeapFlags = VkMemoryHeapFlagBits

data VkMemoryPropertyFlagBits

-- | VkMemoryPropertyFlags - Bitmask of VkMemoryPropertyFlagBits
--
-- = Description
--
-- 'VkMemoryPropertyFlags' is a bitmask type for setting a mask of zero or
-- more 'VkMemoryPropertyFlagBits'.
--
-- = See Also
--
-- 'VkMemoryPropertyFlagBits', 'VkMemoryType'
type VkMemoryPropertyFlags = VkMemoryPropertyFlagBits

data VkMemoryType

-- | Dummy data to tag the 'Ptr' with
data VkPhysicalDevice_T
-- | VkPhysicalDevice - Opaque handle to a physical device object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display.vkAcquireXlibDisplayEXT',
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateDeviceExtensionProperties',
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateDeviceLayerProperties',
-- 'vkEnumeratePhysicalDevices',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayModeProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayModePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetDisplayPlaneCapabilities2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetDisplayPlaneSupportedDisplaysKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps.vkGetPhysicalDeviceCalibrateableTimeDomainsEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix.vkGetPhysicalDeviceCooperativeMatrixPropertiesNV',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayPlaneProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPlanePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2.vkGetPhysicalDeviceDisplayProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkGetPhysicalDeviceDisplayPropertiesKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferPropertiesKHR',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFencePropertiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphoreProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphorePropertiesKHR',
-- 'vkGetPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2KHR',
-- 'vkGetPhysicalDeviceFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2KHR',
-- 'vkGetPhysicalDeviceMemoryProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkGetPhysicalDevicePresentRectanglesKHR',
-- 'vkGetPhysicalDeviceProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2KHR',
-- 'vkGetPhysicalDeviceQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2KHR',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.vkGetPhysicalDeviceSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceCapabilities2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2.vkGetPhysicalDeviceSurfaceFormats2KHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceFormatsKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive.vkGetPhysicalDeviceSurfacePresentModes2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfacePresentModesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceSupportKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.vkGetPhysicalDeviceWaylandPresentationSupportKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.vkGetPhysicalDeviceWin32PresentationSupportKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.vkGetPhysicalDeviceXcbPresentationSupportKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.vkGetPhysicalDeviceXlibPresentationSupportKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display.vkGetRandROutputDisplayEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display.vkReleaseDisplayEXT'
type VkPhysicalDevice = Ptr VkPhysicalDevice_T

data VkPhysicalDeviceFeatures

data VkPhysicalDeviceLimits

data VkPhysicalDeviceMemoryProperties

data VkPhysicalDeviceProperties

data VkPhysicalDeviceSparseProperties

data VkPhysicalDeviceType

data VkQueueFamilyProperties

data VkQueueFlagBits

-- | VkQueueFlags - Bitmask of VkQueueFlagBits
--
-- = Description
--
-- 'VkQueueFlags' is a bitmask type for setting a mask of zero or more
-- 'VkQueueFlagBits'.
--
-- = See Also
--
-- 'VkQueueFamilyProperties', 'VkQueueFlagBits'
type VkQueueFlags = VkQueueFlagBits

data VkSampleCountFlagBits

-- | VkSampleCountFlags - Bitmask of VkSampleCountFlagBits
--
-- = Description
--
-- 'VkSampleCountFlags' is a bitmask type for setting a mask of zero or
-- more 'VkSampleCountFlagBits'.
--
-- = See Also
--
-- 'VkImageFormatProperties', 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT',
-- 'VkSampleCountFlagBits'
type VkSampleCountFlags = VkSampleCountFlagBits

data VkSystemAllocationScope

type FN_vkCreateInstance = ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
type PFN_vkCreateInstance = FunPtr FN_vkCreateInstance

type FN_vkDestroyInstance = ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyInstance = FunPtr FN_vkDestroyInstance

type FN_vkEnumeratePhysicalDevices = ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
type PFN_vkEnumeratePhysicalDevices = FunPtr FN_vkEnumeratePhysicalDevices

type FN_vkGetDeviceProcAddr = ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetDeviceProcAddr = FunPtr FN_vkGetDeviceProcAddr

#if defined(EXPOSE_CORE10_COMMANDS)
vkGetInstanceProcAddr :: (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#else
vkGetInstanceProcAddr :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#endif
type FN_vkGetInstanceProcAddr = ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetInstanceProcAddr = FunPtr FN_vkGetInstanceProcAddr

type FN_vkGetPhysicalDeviceFeatures = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures = FunPtr FN_vkGetPhysicalDeviceFeatures

type FN_vkGetPhysicalDeviceFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties = FunPtr FN_vkGetPhysicalDeviceFormatProperties

type FN_vkGetPhysicalDeviceImageFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties

type FN_vkGetPhysicalDeviceMemoryProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties = FunPtr FN_vkGetPhysicalDeviceMemoryProperties

type FN_vkGetPhysicalDeviceProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
type PFN_vkGetPhysicalDeviceProperties = FunPtr FN_vkGetPhysicalDeviceProperties

type FN_vkGetPhysicalDeviceQueueFamilyProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties
