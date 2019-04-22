{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.DeviceInitialization
  ( withCStructAllocationCallbacks
  , fromCStructAllocationCallbacks
  , AllocationCallbacks(..)
  , withCStructApplicationInfo
  , fromCStructApplicationInfo
  , ApplicationInfo(..)
  , Device(..)
  , DeviceSize
  , withCStructExtent3D
  , fromCStructExtent3D
  , Extent3D(..)
  , FormatFeatureFlagBits
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  , pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT
  , pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
  , pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
  , pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
  , pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT
  , pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
  , pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern FORMAT_FEATURE_BLIT_SRC_BIT
  , pattern FORMAT_FEATURE_BLIT_DST_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  , pattern FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern FORMAT_FEATURE_DISJOINT_BIT
  , pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  , pattern FORMAT_FEATURE_RESERVED_27_BIT_KHR
  , pattern FORMAT_FEATURE_RESERVED_28_BIT_KHR
  , pattern FORMAT_FEATURE_RESERVED_25_BIT_KHR
  , pattern FORMAT_FEATURE_RESERVED_26_BIT_KHR
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  , pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , FormatFeatureFlags
  , withCStructFormatProperties
  , fromCStructFormatProperties
  , FormatProperties(..)
  , ImageCreateFlagBits
  , pattern IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_ALIAS_BIT
  , pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  , pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern IMAGE_CREATE_EXTENDED_USAGE_BIT
  , pattern IMAGE_CREATE_PROTECTED_BIT
  , pattern IMAGE_CREATE_DISJOINT_BIT
  , pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  , pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  , pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , ImageCreateFlags
  , withCStructImageFormatProperties
  , fromCStructImageFormatProperties
  , ImageFormatProperties(..)
  , ImageTiling
  , pattern IMAGE_TILING_OPTIMAL
  , pattern IMAGE_TILING_LINEAR
  , pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  , ImageType
  , pattern IMAGE_TYPE_1D
  , pattern IMAGE_TYPE_2D
  , pattern IMAGE_TYPE_3D
  , ImageUsageFlagBits
  , pattern IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern IMAGE_USAGE_SAMPLED_BIT
  , pattern IMAGE_USAGE_STORAGE_BIT
  , pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , pattern IMAGE_USAGE_RESERVED_13_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_14_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_15_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_10_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_11_BIT_KHR
  , pattern IMAGE_USAGE_RESERVED_12_BIT_KHR
  , pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  , pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , ImageUsageFlags
  , Instance(..)
  , InstanceCreateFlags
  , withCStructInstanceCreateInfo
  , fromCStructInstanceCreateInfo
  , InstanceCreateInfo(..)
  , withCStructMemoryHeap
  , fromCStructMemoryHeap
  , MemoryHeap(..)
  , MemoryHeapFlagBits
  , pattern MEMORY_HEAP_DEVICE_LOCAL_BIT
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  , MemoryHeapFlags
  , MemoryPropertyFlagBits
  , pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , pattern MEMORY_PROPERTY_PROTECTED_BIT
  , MemoryPropertyFlags
  , withCStructMemoryType
  , fromCStructMemoryType
  , MemoryType(..)
  , PhysicalDevice(..)
  , withCStructPhysicalDeviceFeatures
  , fromCStructPhysicalDeviceFeatures
  , PhysicalDeviceFeatures(..)
  , withCStructPhysicalDeviceLimits
  , fromCStructPhysicalDeviceLimits
  , PhysicalDeviceLimits(..)
  , withCStructPhysicalDeviceMemoryProperties
  , fromCStructPhysicalDeviceMemoryProperties
  , PhysicalDeviceMemoryProperties(..)
  , withCStructPhysicalDeviceProperties
  , fromCStructPhysicalDeviceProperties
  , PhysicalDeviceProperties(..)
  , withCStructPhysicalDeviceSparseProperties
  , fromCStructPhysicalDeviceSparseProperties
  , PhysicalDeviceSparseProperties(..)
  , PhysicalDeviceType
  , pattern PHYSICAL_DEVICE_TYPE_OTHER
  , pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern PHYSICAL_DEVICE_TYPE_CPU
  , withCStructQueueFamilyProperties
  , fromCStructQueueFamilyProperties
  , QueueFamilyProperties(..)
  , QueueFlagBits
  , pattern QUEUE_GRAPHICS_BIT
  , pattern QUEUE_COMPUTE_BIT
  , pattern QUEUE_TRANSFER_BIT
  , pattern QUEUE_SPARSE_BINDING_BIT
  , pattern QUEUE_PROTECTED_BIT
  , pattern QUEUE_RESERVED_6_BIT_KHR
  , pattern QUEUE_RESERVED_5_BIT_KHR
  , QueueFlags
  , SampleCountFlagBits
  , pattern SAMPLE_COUNT_1_BIT
  , pattern SAMPLE_COUNT_2_BIT
  , pattern SAMPLE_COUNT_4_BIT
  , pattern SAMPLE_COUNT_8_BIT
  , pattern SAMPLE_COUNT_16_BIT
  , pattern SAMPLE_COUNT_32_BIT
  , pattern SAMPLE_COUNT_64_BIT
  , SampleCountFlags
  , createInstance
  , destroyInstance
  , getNumPhysicalDevices
  , enumeratePhysicalDevices
  , enumerateAllPhysicalDevices
  , getDeviceProcAddr
  , getInstanceProcAddr
  , getPhysicalDeviceFeatures
  , getPhysicalDeviceFormatProperties
  , getPhysicalDeviceImageFormatProperties
  , getPhysicalDeviceMemoryProperties
  , getPhysicalDeviceProperties
  , getNumPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getAllPhysicalDeviceQueueFamilyProperties
  , withInstance
  , pattern VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR
  , pattern VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR
  , pattern VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR
  , pattern VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_10_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_11_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_12_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_13_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_14_BIT_KHR
  , pattern VK_IMAGE_USAGE_RESERVED_15_BIT_KHR
  , pattern VK_QUEUE_RESERVED_5_BIT_KHR
  , pattern VK_QUEUE_RESERVED_6_BIT_KHR
  ) where

import Control.Exception
  ( bracket
  , throwIO
  )
import Control.Monad
  ( (<=<)
  , when
  )
import Data.Bits
  ( zeroBits
  )
import Data.ByteString
  ( ByteString
  , packCString
  , packCStringLen
  , useAsCString
  )
import qualified Data.ByteString
  ( empty
  )
import Data.Function
  ( on
  )
import Data.Int
  ( Int32
  )
import Data.Vector
  ( Vector
  )
import qualified Data.Vector
  ( empty
  , generateM
  , length
  , take
  )
import qualified Data.Vector.Generic
  ( convert
  )
import Data.Vector.Generic.Sized
  ( fromTuple
  )
import qualified Data.Vector.Generic.Sized
  ( convert
  )
import qualified Data.Vector.Storable
  ( unsafeWith
  )
import qualified Data.Vector.Storable.Sized
  ( fromSized
  , unsafeIndex
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CFloat(..)
  , CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , peekElemOff
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkApplicationInfo(..)
  , VkExtent3D(..)
  , VkFormatFeatureFlagBits(..)
  , VkFormatProperties(..)
  , VkImageCreateFlagBits(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
  , VkInstanceCreateFlags(..)
  , VkInstanceCreateInfo(..)
  , VkMemoryHeap(..)
  , VkMemoryHeapFlagBits(..)
  , VkMemoryPropertyFlagBits(..)
  , VkMemoryType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkPhysicalDeviceType(..)
  , VkQueueFamilyProperties(..)
  , VkQueueFlagBits(..)
  , VkSampleCountFlagBits(..)
  , PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VkDevice
  , VkDeviceSize
  , VkInstance
  , VkPhysicalDevice
  , vkCreateInstance
  , vkDestroyInstance
  , vkEnumeratePhysicalDevices
  , vkGetDeviceProcAddr
  , vkGetInstanceProcAddr
  , vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceFormatProperties
  , vkGetPhysicalDeviceImageFormatProperties
  , vkGetPhysicalDeviceMemoryProperties
  , vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceQueueFamilyProperties
  , pattern VK_FORMAT_FEATURE_BLIT_DST_BIT
  , pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
  , pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
  , pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_IMAGE_TILING_LINEAR
  , pattern VK_IMAGE_TILING_OPTIMAL
  , pattern VK_IMAGE_TYPE_1D
  , pattern VK_IMAGE_TYPE_2D
  , pattern VK_IMAGE_TYPE_3D
  , pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_SAMPLED_BIT
  , pattern VK_IMAGE_USAGE_STORAGE_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , pattern VK_PHYSICAL_DEVICE_TYPE_CPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_OTHER
  , pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern VK_QUEUE_COMPUTE_BIT
  , pattern VK_QUEUE_GRAPHICS_BIT
  , pattern VK_QUEUE_SPARSE_BINDING_BIT
  , pattern VK_QUEUE_TRANSFER_BIT
  , pattern VK_SAMPLE_COUNT_16_BIT
  , pattern VK_SAMPLE_COUNT_1_BIT
  , pattern VK_SAMPLE_COUNT_2_BIT
  , pattern VK_SAMPLE_COUNT_32_BIT
  , pattern VK_SAMPLE_COUNT_4_BIT
  , pattern VK_SAMPLE_COUNT_64_BIT
  , pattern VK_SAMPLE_COUNT_8_BIT
  , pattern VK_UUID_SIZE
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_QUEUE_PROTECTED_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( pattern VK_FORMAT_FEATURE_TRANSFER_DST_BIT
  , pattern VK_FORMAT_FEATURE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( pattern VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
  , pattern VK_IMAGE_CREATE_EXTENDED_USAGE_BIT
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( pattern VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_DISJOINT_BIT
  , pattern VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
  , pattern VK_IMAGE_CREATE_DISJOINT_BIT
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  , initInstanceCmds
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( pattern VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
  , pattern VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT
  , pattern VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( pattern VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( pattern VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_IMG_filter_cubic
  ( pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( pattern VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( pattern VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
  )
import Graphics.Vulkan.Core10.Core
  ( Format
  , bool32ToBool
  , boolToBool32
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( byteStringToNullTerminatedSizedVector
  , byteStringToSizedVector
  , packCStringElemOff
  , padSized
  , withArray
  , withVec
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )



-- | VkAllocationCallbacks - Structure containing callback function pointers
-- for memory allocation
--
-- == Valid Usage
--
-- -   @pfnAllocation@ /must/ be a valid pointer to a valid user-defined
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkAllocationFunction'
--
-- -   @pfnReallocation@ /must/ be a valid pointer to a valid user-defined
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkReallocationFunction'
--
-- -   @pfnFree@ /must/ be a valid pointer to a valid user-defined
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkFreeFunction'
--
-- -   If either of @pfnInternalAllocation@ or @pfnInternalFree@ is not
--     @NULL@, both /must/ be valid callbacks
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkAllocationFunction',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkFreeFunction',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkInternalAllocationNotification',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkInternalFreeNotification',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkReallocationFunction',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkCreateAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_android_surface.vkCreateAndroidSurfaceKHR',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkCreateDebugReportCallbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkCreateDebugUtilsMessengerEXT',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplateKHR',
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayModeKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_display.vkCreateDisplayPlaneSurfaceKHR',
-- 'Graphics.Vulkan.C.Core10.Event.vkCreateEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface.vkCreateHeadlessSurfaceEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface.vkCreateIOSSurfaceMVK',
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface.vkCreateImagePipeSurfaceFUCHSIA',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.vkCreateIndirectCommandsLayoutNVX',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance',
-- 'Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface.vkCreateMacOSSurfaceMVK',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface.vkCreateMetalSurfaceEXT',
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
-- 'Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface.vkCreateStreamDescriptorSurfaceGGP',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkCreateSwapchainKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkCreateValidationCacheEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_NN_vi_surface.vkCreateViSurfaceNN',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface.vkCreateWaylandSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface.vkCreateWin32SurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface.vkCreateXcbSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface.vkCreateXlibSurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing.vkDestroyAccelerationStructureNV',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkDestroyBufferView',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.vkDestroyDebugReportCallbackEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.vkDestroyDebugUtilsMessengerEXT',
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkDestroyInstance',
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
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.vkDestroySurfaceKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.vkDestroySwapchainKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache.vkDestroyValidationCacheEXT',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDeviceEventEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_control.vkRegisterDisplayEventEXT'
data AllocationCallbacks = AllocationCallbacks
  { -- No documentation found for Nested "AllocationCallbacks" "pUserData"
  userData :: Ptr ()
  , -- No documentation found for Nested "AllocationCallbacks" "pfnAllocation"
  pfnAllocation :: PFN_vkAllocationFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnReallocation"
  pfnReallocation :: PFN_vkReallocationFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnFree"
  pfnFree :: PFN_vkFreeFunction
  , -- No documentation found for Nested "AllocationCallbacks" "pfnInternalAllocation"
  pfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- No documentation found for Nested "AllocationCallbacks" "pfnInternalFree"
  pfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkAllocationCallbacks' and
-- marshal a 'AllocationCallbacks' into it. The 'VkAllocationCallbacks' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructAllocationCallbacks :: AllocationCallbacks -> (VkAllocationCallbacks -> IO a) -> IO a
withCStructAllocationCallbacks marshalled cont = cont (VkAllocationCallbacks (userData (marshalled :: AllocationCallbacks)) (pfnAllocation (marshalled :: AllocationCallbacks)) (pfnReallocation (marshalled :: AllocationCallbacks)) (pfnFree (marshalled :: AllocationCallbacks)) (pfnInternalAllocation (marshalled :: AllocationCallbacks)) (pfnInternalFree (marshalled :: AllocationCallbacks)))

-- | A function to read a 'VkAllocationCallbacks' and all additional
-- structures in the pointer chain into a 'AllocationCallbacks'.
fromCStructAllocationCallbacks :: VkAllocationCallbacks -> IO AllocationCallbacks
fromCStructAllocationCallbacks c = AllocationCallbacks <$> pure (vkPUserData (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnAllocation (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnReallocation (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnFree (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnInternalAllocation (c :: VkAllocationCallbacks))
                                                       <*> pure (vkPfnInternalFree (c :: VkAllocationCallbacks))

instance Zero AllocationCallbacks where
  zero = AllocationCallbacks zero
                             zero
                             zero
                             zero
                             zero
                             zero



-- | VkApplicationInfo - Structure specifying application info
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_APPLICATION_INFO'
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   If @pApplicationName@ is not @NULL@, @pApplicationName@ /must/ be a
--     null-terminated UTF-8 string
--
-- -   If @pEngineName@ is not @NULL@, @pEngineName@ /must/ be a
--     null-terminated UTF-8 string
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data ApplicationInfo = ApplicationInfo
  { -- Univalued member elided
  -- No documentation found for Nested "ApplicationInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "ApplicationInfo" "pApplicationName"
  applicationName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "applicationVersion"
  applicationVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "pEngineName"
  engineName :: Maybe ByteString
  , -- No documentation found for Nested "ApplicationInfo" "engineVersion"
  engineVersion :: Word32
  , -- No documentation found for Nested "ApplicationInfo" "apiVersion"
  apiVersion :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkApplicationInfo' and
-- marshal a 'ApplicationInfo' into it. The 'VkApplicationInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructApplicationInfo :: ApplicationInfo -> (VkApplicationInfo -> IO a) -> IO a
withCStructApplicationInfo marshalled cont = maybeWith useAsCString (engineName (marshalled :: ApplicationInfo)) (\pPEngineName -> maybeWith useAsCString (applicationName (marshalled :: ApplicationInfo)) (\pPApplicationName -> maybeWith withSomeVkStruct (next (marshalled :: ApplicationInfo)) (\pPNext -> cont (VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO pPNext pPApplicationName (applicationVersion (marshalled :: ApplicationInfo)) pPEngineName (engineVersion (marshalled :: ApplicationInfo)) (apiVersion (marshalled :: ApplicationInfo))))))

-- | A function to read a 'VkApplicationInfo' and all additional
-- structures in the pointer chain into a 'ApplicationInfo'.
fromCStructApplicationInfo :: VkApplicationInfo -> IO ApplicationInfo
fromCStructApplicationInfo c = ApplicationInfo <$> -- Univalued Member elided
                                               maybePeek peekVkStruct (castPtr (vkPNext (c :: VkApplicationInfo)))
                                               <*> maybePeek packCString (vkPApplicationName (c :: VkApplicationInfo))
                                               <*> pure (vkApplicationVersion (c :: VkApplicationInfo))
                                               <*> maybePeek packCString (vkPEngineName (c :: VkApplicationInfo))
                                               <*> pure (vkEngineVersion (c :: VkApplicationInfo))
                                               <*> pure (vkApiVersion (c :: VkApplicationInfo))

instance Zero ApplicationInfo where
  zero = ApplicationInfo Nothing
                         Nothing
                         zero
                         Nothing
                         zero
                         zero


data Device = Device
  { deviceHandle :: VkDevice
  , deviceCmds    :: DeviceCmds
  }
  deriving Show

instance Eq Device where
  (==) = (==) `on` deviceHandle

instance Ord Device where
  compare = compare `on` deviceHandle


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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsTokenNVX',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits',
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
type DeviceSize = VkDeviceSize
  


-- | VkExtent3D - Structure specifying a three-dimensional extent
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind'
data Extent3D = Extent3D
  { -- No documentation found for Nested "Extent3D" "width"
  width :: Word32
  , -- No documentation found for Nested "Extent3D" "height"
  height :: Word32
  , -- No documentation found for Nested "Extent3D" "depth"
  depth :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkExtent3D' and
-- marshal a 'Extent3D' into it. The 'VkExtent3D' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructExtent3D :: Extent3D -> (VkExtent3D -> IO a) -> IO a
withCStructExtent3D marshalled cont = cont (VkExtent3D (width (marshalled :: Extent3D)) (height (marshalled :: Extent3D)) (depth (marshalled :: Extent3D)))

-- | A function to read a 'VkExtent3D' and all additional
-- structures in the pointer chain into a 'Extent3D'.
fromCStructExtent3D :: VkExtent3D -> IO Extent3D
fromCStructExtent3D c = Extent3D <$> pure (vkWidth (c :: VkExtent3D))
                                 <*> pure (vkHeight (c :: VkExtent3D))
                                 <*> pure (vkDepth (c :: VkExtent3D))

instance Zero Extent3D where
  zero = Extent3D zero
                  zero
                  zero


-- | VkFormatFeatureFlagBits - Bitmask specifying features supported by a
-- buffer
--
-- = Description
--
-- The following bits /may/ be set in @linearTilingFeatures@ and
-- @optimalTilingFeatures@, specifying that the features are supported by
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImage images>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkImageView image views>
-- created with the queried
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'::@format@:
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--     specifies that an image view /can/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT'
--     specifies that an image view /can/ be used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage images>.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
--     specifies that an image view /can/ be used as storage image that
--     supports atomic operations.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
--     specifies that an image view /can/ be used as a framebuffer color
--     attachment and as an input attachment.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'
--     specifies that an image view /can/ be used as a framebuffer color
--     attachment that supports blending and as an input attachment.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
--     specifies that an image view /can/ be used as a framebuffer
--     depth\/stencil attachment and as an input attachment.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'
--     specifies that an image /can/ be used as @srcImage@ for the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
--     command.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_DST_BIT'
--     specifies that an image /can/ be used as @dstImage@ for the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
--     command.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
--     specifies that if
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--     is also set, an image view /can/ be used with a sampler that has
--     either of @magFilter@ or @minFilter@ set to
--     'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR', or @mipmapMode@
--     set to
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_LINEAR'. If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'
--     is also set, an image can be used as the @srcImage@ to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' with
--     a @filter@ of 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR'.
--     This bit /must/ only be exposed for formats that also support the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
--     or
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'.
--
--     If the format being queried is a depth\/stencil format, this bit
--     only specifies that the depth aspect (not the stencil aspect) of an
--     image of this format supports linear filtering, and that linear
--     filtering of the depth aspect is supported whether depth compare is
--     enabled in the sampler or not. If this bit is not present, linear
--     filtering with depth compare disabled is unsupported and linear
--     filtering with depth compare enabled is supported, but /may/ compute
--     the filtered value in an implementation-dependent manner which
--     differs from the normal rules of linear filtering. The resulting
--     value /must/ be in the range [0,1] and /should/ be proportional to,
--     or a weighted average of, the number of comparison passes or
--     failures.
--
-- The following bits /may/ be set in @bufferFeatures@, specifying that the
-- features are supported by
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBuffer buffers>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#VkBufferView buffer views>
-- created with the queried
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceProperties'::@format@:
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT'
--     specifies that the format /can/ be used to create a buffer view that
--     /can/ be bound to a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     descriptor.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT'
--     specifies that the format /can/ be used to create a buffer view that
--     /can/ be bound to a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
--     specifies that atomic operations are supported on
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     with this format.
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT'
--     specifies that the format /can/ be used as a vertex attribute format
--     ('Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'::@format@).
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlags'
type FormatFeatureFlagBits = VkFormatFeatureFlagBits


{-# complete FORMAT_FEATURE_SAMPLED_IMAGE_BIT, FORMAT_FEATURE_STORAGE_IMAGE_BIT, FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT, FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT, FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT, FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT, FORMAT_FEATURE_VERTEX_BUFFER_BIT, FORMAT_FEATURE_COLOR_ATTACHMENT_BIT, FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT, FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT, FORMAT_FEATURE_BLIT_SRC_BIT, FORMAT_FEATURE_BLIT_DST_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT, FORMAT_FEATURE_TRANSFER_SRC_BIT, FORMAT_FEATURE_TRANSFER_DST_BIT, FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT, FORMAT_FEATURE_DISJOINT_BIT, FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT, FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG, FORMAT_FEATURE_RESERVED_27_BIT_KHR, FORMAT_FEATURE_RESERVED_28_BIT_KHR, FORMAT_FEATURE_RESERVED_25_BIT_KHR, FORMAT_FEATURE_RESERVED_26_BIT_KHR, FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT, FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT :: FormatFeatureFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
-- specifies that an image view /can/ be
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT'
-- specifies that an image view /can/ be used as a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage images>.
pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT = VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT'
-- specifies that an image view /can/ be used as storage image that
-- supports atomic operations.
pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT'
-- specifies that the format /can/ be used to create a buffer view that
-- /can/ be bound to a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
-- descriptor.
pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT'
-- specifies that the format /can/ be used to create a buffer view that
-- /can/ be bound to a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
-- descriptor.
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT'
-- specifies that atomic operations are supported on
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
-- with this format.
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT'
-- specifies that the format /can/ be used as a vertex attribute format
-- ('Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'::@format@).
pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT = VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT'
-- specifies that an image view /can/ be used as a framebuffer color
-- attachment and as an input attachment.
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT'
-- specifies that an image view /can/ be used as a framebuffer color
-- attachment that supports blending and as an input attachment.
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT'
-- specifies that an image view /can/ be used as a framebuffer
-- depth\/stencil attachment and as an input attachment.
pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'
-- specifies that an image /can/ be used as @srcImage@ for the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' command.
pattern FORMAT_FEATURE_BLIT_SRC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_BLIT_SRC_BIT = VK_FORMAT_FEATURE_BLIT_SRC_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_DST_BIT'
-- specifies that an image /can/ be used as @dstImage@ for the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' command.
pattern FORMAT_FEATURE_BLIT_DST_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_BLIT_DST_BIT = VK_FORMAT_FEATURE_BLIT_DST_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT'
-- specifies that if
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
-- is also set, an image view /can/ be used with a sampler that has either
-- of @magFilter@ or @minFilter@ set to
-- 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR', or @mipmapMode@ set
-- to 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_LINEAR'. If
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'
-- is also set, an image can be used as the @srcImage@ to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' with a
-- @filter@ of 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR'. This
-- bit /must/ only be exposed for formats that also support the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT'
-- or
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_FORMAT_FEATURE_BLIT_SRC_BIT'.
--
-- If the format being queried is a depth\/stencil format, this bit only
-- specifies that the depth aspect (not the stencil aspect) of an image of
-- this format supports linear filtering, and that linear filtering of the
-- depth aspect is supported whether depth compare is enabled in the
-- sampler or not. If this bit is not present, linear filtering with depth
-- compare disabled is unsupported and linear filtering with depth compare
-- enabled is supported, but /may/ compute the filtered value in an
-- implementation-dependent manner which differs from the normal rules of
-- linear filtering. The resulting value /must/ be in the range [0,1] and
-- /should/ be proportional to, or a weighted average of, the number of
-- comparison passes or failures.
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_TRANSFER_SRC_BIT"
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT = VK_FORMAT_FEATURE_TRANSFER_SRC_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_TRANSFER_DST_BIT"
pattern FORMAT_FEATURE_TRANSFER_DST_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_TRANSFER_DST_BIT = VK_FORMAT_FEATURE_TRANSFER_DST_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_DISJOINT_BIT"
pattern FORMAT_FEATURE_DISJOINT_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_DISJOINT_BIT = VK_FORMAT_FEATURE_DISJOINT_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT = VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_27_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_27_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_27_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_28_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_28_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_28_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_25_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_25_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_25_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_RESERVED_26_BIT_KHR"
pattern FORMAT_FEATURE_RESERVED_26_BIT_KHR :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_RESERVED_26_BIT_KHR = VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT = VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT


-- No documentation found for Nested "FormatFeatureFlagBits" "FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT :: (a ~ FormatFeatureFlagBits) => a
pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT = VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT

-- | VkFormatFeatureFlags - Bitmask of VkFormatFeatureFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlags' is
-- a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier.VkDrmFormatModifierPropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlagBits',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties'
type FormatFeatureFlags = FormatFeatureFlagBits


-- | VkFormatProperties - Structure specifying image format properties
--
-- = Description
--
-- __Note__
--
-- If no format feature flags are supported, then the only possible use
-- would be image transfers - which alone are not useful. As such, if no
-- format feature flags are supported, the format itself is not supported,
-- and images of that format cannot be created.
--
-- If @format@ is a block-compressed format, then @bufferFeatures@ /must/
-- not support any features for the format.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatFeatureFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties'
data FormatProperties = FormatProperties
  { -- No documentation found for Nested "FormatProperties" "linearTilingFeatures"
  linearTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "FormatProperties" "optimalTilingFeatures"
  optimalTilingFeatures :: FormatFeatureFlags
  , -- No documentation found for Nested "FormatProperties" "bufferFeatures"
  bufferFeatures :: FormatFeatureFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkFormatProperties' and
-- marshal a 'FormatProperties' into it. The 'VkFormatProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructFormatProperties :: FormatProperties -> (VkFormatProperties -> IO a) -> IO a
withCStructFormatProperties marshalled cont = cont (VkFormatProperties (linearTilingFeatures (marshalled :: FormatProperties)) (optimalTilingFeatures (marshalled :: FormatProperties)) (bufferFeatures (marshalled :: FormatProperties)))

-- | A function to read a 'VkFormatProperties' and all additional
-- structures in the pointer chain into a 'FormatProperties'.
fromCStructFormatProperties :: VkFormatProperties -> IO FormatProperties
fromCStructFormatProperties c = FormatProperties <$> pure (vkLinearTilingFeatures (c :: VkFormatProperties))
                                                 <*> pure (vkOptimalTilingFeatures (c :: VkFormatProperties))
                                                 <*> pure (vkBufferFeatures (c :: VkFormatProperties))

instance Zero FormatProperties where
  zero = FormatProperties zero
                          zero
                          zero


-- | VkImageCreateFlagBits - Bitmask specifying additional parameters of an
-- image
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-physicalfeatures Sparse Physical Device Features>
-- for more details.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags'
type ImageCreateFlagBits = VkImageCreateFlagBits


{-# complete IMAGE_CREATE_SPARSE_BINDING_BIT, IMAGE_CREATE_SPARSE_RESIDENCY_BIT, IMAGE_CREATE_SPARSE_ALIASED_BIT, IMAGE_CREATE_MUTABLE_FORMAT_BIT, IMAGE_CREATE_CUBE_COMPATIBLE_BIT, IMAGE_CREATE_ALIAS_BIT, IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT, IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT, IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT, IMAGE_CREATE_EXTENDED_USAGE_BIT, IMAGE_CREATE_PROTECTED_BIT, IMAGE_CREATE_DISJOINT_BIT, IMAGE_CREATE_CORNER_SAMPLED_BIT_NV, IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT, IMAGE_CREATE_SUBSAMPLED_BIT_EXT :: ImageCreateFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
-- specifies that the image will be backed using sparse memory binding.
pattern IMAGE_CREATE_SPARSE_BINDING_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPARSE_BINDING_BIT = VK_IMAGE_CREATE_SPARSE_BINDING_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT'
-- specifies that the image /can/ be partially backed using sparse memory
-- binding. Images created with this flag /must/ also be created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
-- flag.
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_ALIASED_BIT'
-- specifies that the image will be backed using sparse memory binding with
-- memory ranges that might also simultaneously be backing another image
-- (or another portion of the same image). Images created with this flag
-- /must/ also be created with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_SPARSE_BINDING_BIT'
-- flag
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT = VK_IMAGE_CREATE_SPARSE_ALIASED_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' with a different format
-- from the image.
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT = VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of type
-- 'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE' or
-- 'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'.
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_ALIAS_BIT"
pattern IMAGE_CREATE_ALIAS_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_ALIAS_BIT = VK_IMAGE_CREATE_ALIAS_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT = VK_IMAGE_CREATE_EXTENDED_USAGE_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_PROTECTED_BIT"
pattern IMAGE_CREATE_PROTECTED_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_PROTECTED_BIT = VK_IMAGE_CREATE_PROTECTED_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_DISJOINT_BIT"
pattern IMAGE_CREATE_DISJOINT_BIT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_DISJOINT_BIT = VK_IMAGE_CREATE_DISJOINT_BIT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV = VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT


-- No documentation found for Nested "ImageCreateFlagBits" "IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT :: (a ~ ImageCreateFlagBits) => a
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT = VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT

-- | VkImageCreateFlags - Bitmask of VkImageCreateFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
type ImageCreateFlags = ImageCreateFlagBits


-- | VkImageFormatProperties - Structure specifying an image format
-- properties
--
-- = Description
--
-- __Note__
--
-- There is no mechanism to query the size of an image before creating it,
-- to compare that size against @maxResourceSize@. If an application
-- attempts to create an image that exceeds this limit, the creation will
-- fail and 'Graphics.Vulkan.C.Core10.Image.vkCreateImage' will return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'. While the
-- advertised limit /must/ be at least 231, it /may/ not be possible to
-- create an image that approaches that size, particularly for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D'.
--
-- If the combination of parameters to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
-- is not supported by the implementation for use in
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage', then all members of
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'
-- will be filled with zero.
--
-- __Note__
--
-- Filling
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'
-- with zero for unsupported formats is an exception to the usual rule that
-- output structures have undefined contents on error. This exception was
-- unintentional, but is preserved for backwards compatibility.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
data ImageFormatProperties = ImageFormatProperties
  { -- No documentation found for Nested "ImageFormatProperties" "maxExtent"
  maxExtent :: Extent3D
  , -- No documentation found for Nested "ImageFormatProperties" "maxMipLevels"
  maxMipLevels :: Word32
  , -- No documentation found for Nested "ImageFormatProperties" "maxArrayLayers"
  maxArrayLayers :: Word32
  , -- No documentation found for Nested "ImageFormatProperties" "sampleCounts"
  sampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "ImageFormatProperties" "maxResourceSize"
  maxResourceSize :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkImageFormatProperties' and
-- marshal a 'ImageFormatProperties' into it. The 'VkImageFormatProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructImageFormatProperties :: ImageFormatProperties -> (VkImageFormatProperties -> IO a) -> IO a
withCStructImageFormatProperties marshalled cont = withCStructExtent3D (maxExtent (marshalled :: ImageFormatProperties)) (\maxExtent'' -> cont (VkImageFormatProperties maxExtent'' (maxMipLevels (marshalled :: ImageFormatProperties)) (maxArrayLayers (marshalled :: ImageFormatProperties)) (sampleCounts (marshalled :: ImageFormatProperties)) (maxResourceSize (marshalled :: ImageFormatProperties))))

-- | A function to read a 'VkImageFormatProperties' and all additional
-- structures in the pointer chain into a 'ImageFormatProperties'.
fromCStructImageFormatProperties :: VkImageFormatProperties -> IO ImageFormatProperties
fromCStructImageFormatProperties c = ImageFormatProperties <$> (fromCStructExtent3D (vkMaxExtent (c :: VkImageFormatProperties)))
                                                           <*> pure (vkMaxMipLevels (c :: VkImageFormatProperties))
                                                           <*> pure (vkMaxArrayLayers (c :: VkImageFormatProperties))
                                                           <*> pure (vkSampleCounts (c :: VkImageFormatProperties))
                                                           <*> pure (vkMaxResourceSize (c :: VkImageFormatProperties))

instance Zero ImageFormatProperties where
  zero = ImageFormatProperties zero
                               zero
                               zero
                               zero
                               zero


-- | VkImageTiling - Specifies the tiling arrangement of data in an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type ImageTiling = VkImageTiling


{-# complete IMAGE_TILING_OPTIMAL, IMAGE_TILING_LINEAR, IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: ImageTiling #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_OPTIMAL'
-- specifies optimal tiling (texels are laid out in an
-- implementation-dependent arrangement, for more optimal memory access).
pattern IMAGE_TILING_OPTIMAL :: (a ~ ImageTiling) => a
pattern IMAGE_TILING_OPTIMAL = VK_IMAGE_TILING_OPTIMAL


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TILING_LINEAR'
-- specifies linear tiling (texels are laid out in memory in row-major
-- order, possibly with some padding on each row).
pattern IMAGE_TILING_LINEAR :: (a ~ ImageTiling) => a
pattern IMAGE_TILING_LINEAR = VK_IMAGE_TILING_LINEAR


-- No documentation found for Nested "ImageTiling" "IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT"
pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: (a ~ ImageTiling) => a
pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT = VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT

-- | VkImageType - Specifies the type of an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type ImageType = VkImageType


{-# complete IMAGE_TYPE_1D, IMAGE_TYPE_2D, IMAGE_TYPE_3D :: ImageType #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_1D'
-- specifies a one-dimensional image.
pattern IMAGE_TYPE_1D :: (a ~ ImageType) => a
pattern IMAGE_TYPE_1D = VK_IMAGE_TYPE_1D


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_2D'
-- specifies a two-dimensional image.
pattern IMAGE_TYPE_2D :: (a ~ ImageType) => a
pattern IMAGE_TYPE_2D = VK_IMAGE_TYPE_2D


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_TYPE_3D'
-- specifies a three-dimensional image.
pattern IMAGE_TYPE_3D :: (a ~ ImageType) => a
pattern IMAGE_TYPE_3D = VK_IMAGE_TYPE_3D

-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags'
type ImageUsageFlagBits = VkImageUsageFlagBits


{-# complete IMAGE_USAGE_TRANSFER_SRC_BIT, IMAGE_USAGE_TRANSFER_DST_BIT, IMAGE_USAGE_SAMPLED_BIT, IMAGE_USAGE_STORAGE_BIT, IMAGE_USAGE_COLOR_ATTACHMENT_BIT, IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT, IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT, IMAGE_USAGE_INPUT_ATTACHMENT_BIT, IMAGE_USAGE_RESERVED_13_BIT_KHR, IMAGE_USAGE_RESERVED_14_BIT_KHR, IMAGE_USAGE_RESERVED_15_BIT_KHR, IMAGE_USAGE_RESERVED_10_BIT_KHR, IMAGE_USAGE_RESERVED_11_BIT_KHR, IMAGE_USAGE_RESERVED_12_BIT_KHR, IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV, IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT :: ImageUsageFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_SRC_BIT'
-- specifies that the image /can/ be used as the source of a transfer
-- command.
pattern IMAGE_USAGE_TRANSFER_SRC_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_TRANSFER_SRC_BIT = VK_IMAGE_USAGE_TRANSFER_SRC_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSFER_DST_BIT'
-- specifies that the image /can/ be used as the destination of a transfer
-- command.
pattern IMAGE_USAGE_TRANSFER_DST_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_TRANSFER_DST_BIT = VK_IMAGE_USAGE_TRANSFER_DST_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_SAMPLED_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for occupying
-- a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' slot either
-- of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
-- or
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- and be sampled by a shader.
pattern IMAGE_USAGE_SAMPLED_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_SAMPLED_BIT = VK_IMAGE_USAGE_SAMPLED_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_STORAGE_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for occupying
-- a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' slot of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'.
pattern IMAGE_USAGE_STORAGE_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_STORAGE_BIT = VK_IMAGE_USAGE_STORAGE_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for use as a
-- color or resolve attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'.
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for use as a
-- depth\/stencil attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'.
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT'
-- specifies that the memory bound to this image will have been allocated
-- with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
-- (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory>
-- for more detail). This bit /can/ be set for any image that /can/ be used
-- to create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable
-- for use as a color, resolve, depth\/stencil, or input attachment.
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT'
-- specifies that the image /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for occupying
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' slot of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT';
-- be read from a shader as an input attachment; and be used as an input
-- attachment in a framebuffer.
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_13_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_13_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_13_BIT_KHR = VK_IMAGE_USAGE_RESERVED_13_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_14_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_14_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_14_BIT_KHR = VK_IMAGE_USAGE_RESERVED_14_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_15_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_15_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_15_BIT_KHR = VK_IMAGE_USAGE_RESERVED_15_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_10_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_10_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_10_BIT_KHR = VK_IMAGE_USAGE_RESERVED_10_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_11_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_11_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_11_BIT_KHR = VK_IMAGE_USAGE_RESERVED_11_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_RESERVED_12_BIT_KHR"
pattern IMAGE_USAGE_RESERVED_12_BIT_KHR :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_RESERVED_12_BIT_KHR = VK_IMAGE_USAGE_RESERVED_12_BIT_KHR


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV = VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV


-- No documentation found for Nested "ImageUsageFlagBits" "IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT :: (a ~ ImageUsageFlagBits) => a
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT

-- | VkImageUsageFlags - Bitmask of VkImageUsageFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage.VkImageStencilUsageCreateInfoEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image.VkSharedPresentSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_surface.VkSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type ImageUsageFlags = ImageUsageFlagBits

data Instance = Instance
  { instanceHandle :: VkInstance
  , instanceCmds    :: InstanceCmds
  }
  deriving Show

instance Eq Instance where
  (==) = (==) `on` instanceHandle

instance Ord Instance where
  compare = compare `on` instanceHandle


-- | VkInstanceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateFlags' is
-- a bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'
type InstanceCreateFlags = VkInstanceCreateFlags


-- No complete pragma for InstanceCreateFlags as it has no patterns


-- | VkInstanceCreateInfo - Structure specifying parameters of a newly
-- created instance
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be
--     'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO'
--
-- -   Each @pNext@ member of any structure (including this one) in the
--     @pNext@ chain /must/ be either @NULL@ or a pointer to a valid
--     instance of
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT',
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_features.VkValidationFeaturesEXT',
--     or
--     'Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags.VkValidationFlagsEXT'
--
-- -   Each @sType@ member in the @pNext@ chain /must/ be unique
--
-- -   @flags@ /must/ be @0@
--
-- -   If @pApplicationInfo@ is not @NULL@, @pApplicationInfo@ /must/ be a
--     valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkApplicationInfo'
--     structure
--
-- -   If @enabledLayerCount@ is not @0@, @ppEnabledLayerNames@ /must/ be a
--     valid pointer to an array of @enabledLayerCount@ null-terminated
--     UTF-8 strings
--
-- -   If @enabledExtensionCount@ is not @0@, @ppEnabledExtensionNames@
--     /must/ be a valid pointer to an array of @enabledExtensionCount@
--     null-terminated UTF-8 strings
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkApplicationInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'
data InstanceCreateInfo = InstanceCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "InstanceCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "InstanceCreateInfo" "flags"
  flags :: InstanceCreateFlags
  , -- No documentation found for Nested "InstanceCreateInfo" "pApplicationInfo"
  applicationInfo :: Maybe ApplicationInfo
  -- Length valued member elided
  , -- No documentation found for Nested "InstanceCreateInfo" "ppEnabledLayerNames"
  enabledLayerNames :: Vector ByteString
  -- Length valued member elided
  , -- No documentation found for Nested "InstanceCreateInfo" "ppEnabledExtensionNames"
  enabledExtensionNames :: Vector ByteString
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkInstanceCreateInfo' and
-- marshal a 'InstanceCreateInfo' into it. The 'VkInstanceCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructInstanceCreateInfo :: InstanceCreateInfo -> (VkInstanceCreateInfo -> IO a) -> IO a
withCStructInstanceCreateInfo marshalled cont = withVec useAsCString (enabledExtensionNames (marshalled :: InstanceCreateInfo)) (\pPpEnabledExtensionNames -> withVec useAsCString (enabledLayerNames (marshalled :: InstanceCreateInfo)) (\pPpEnabledLayerNames -> maybeWith (\a -> withCStructApplicationInfo a . flip with) (applicationInfo (marshalled :: InstanceCreateInfo)) (\pPApplicationInfo -> maybeWith withSomeVkStruct (next (marshalled :: InstanceCreateInfo)) (\pPNext -> cont (VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO pPNext (flags (marshalled :: InstanceCreateInfo)) pPApplicationInfo (fromIntegral (Data.Vector.length (enabledLayerNames (marshalled :: InstanceCreateInfo)))) pPpEnabledLayerNames (fromIntegral (Data.Vector.length (enabledExtensionNames (marshalled :: InstanceCreateInfo)))) pPpEnabledExtensionNames)))))

-- | A function to read a 'VkInstanceCreateInfo' and all additional
-- structures in the pointer chain into a 'InstanceCreateInfo'.
fromCStructInstanceCreateInfo :: VkInstanceCreateInfo -> IO InstanceCreateInfo
fromCStructInstanceCreateInfo c = InstanceCreateInfo <$> -- Univalued Member elided
                                                     maybePeek peekVkStruct (castPtr (vkPNext (c :: VkInstanceCreateInfo)))
                                                     <*> pure (vkFlags (c :: VkInstanceCreateInfo))
                                                     <*> maybePeek (fromCStructApplicationInfo <=< peek) (vkPApplicationInfo (c :: VkInstanceCreateInfo))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkEnabledLayerCount (c :: VkInstanceCreateInfo))) (packCStringElemOff (vkPPEnabledLayerNames (c :: VkInstanceCreateInfo))))
                                                     -- Length valued member elided
                                                     <*> (Data.Vector.generateM (fromIntegral (vkEnabledExtensionCount (c :: VkInstanceCreateInfo))) (packCStringElemOff (vkPPEnabledExtensionNames (c :: VkInstanceCreateInfo))))

instance Zero InstanceCreateInfo where
  zero = InstanceCreateInfo Nothing
                            zero
                            Nothing
                            Data.Vector.empty
                            Data.Vector.empty



-- | VkMemoryHeap - Structure specifying a memory heap
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
data MemoryHeap = MemoryHeap
  { -- No documentation found for Nested "MemoryHeap" "size"
  size :: DeviceSize
  , -- No documentation found for Nested "MemoryHeap" "flags"
  flags :: MemoryHeapFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryHeap' and
-- marshal a 'MemoryHeap' into it. The 'VkMemoryHeap' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryHeap :: MemoryHeap -> (VkMemoryHeap -> IO a) -> IO a
withCStructMemoryHeap marshalled cont = cont (VkMemoryHeap (size (marshalled :: MemoryHeap)) (flags (marshalled :: MemoryHeap)))

-- | A function to read a 'VkMemoryHeap' and all additional
-- structures in the pointer chain into a 'MemoryHeap'.
fromCStructMemoryHeap :: VkMemoryHeap -> IO MemoryHeap
fromCStructMemoryHeap c = MemoryHeap <$> pure (vkSize (c :: VkMemoryHeap))
                                     <*> pure (vkFlags (c :: VkMemoryHeap))

instance Zero MemoryHeap where
  zero = MemoryHeap zero
                    zero


-- | VkMemoryHeapFlagBits - Bitmask specifying attribute flags for a heap
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlags'
type MemoryHeapFlagBits = VkMemoryHeapFlagBits


{-# complete MEMORY_HEAP_DEVICE_LOCAL_BIT, MEMORY_HEAP_MULTI_INSTANCE_BIT :: MemoryHeapFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_HEAP_DEVICE_LOCAL_BIT'
-- specifies that the heap corresponds to device local memory. Device local
-- memory /may/ have different performance characteristics than host local
-- memory, and /may/ support different memory property flags.
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT :: (a ~ MemoryHeapFlagBits) => a
pattern MEMORY_HEAP_DEVICE_LOCAL_BIT = VK_MEMORY_HEAP_DEVICE_LOCAL_BIT


-- No documentation found for Nested "MemoryHeapFlagBits" "MEMORY_HEAP_MULTI_INSTANCE_BIT"
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT :: (a ~ MemoryHeapFlagBits) => a
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT = VK_MEMORY_HEAP_MULTI_INSTANCE_BIT

-- | VkMemoryHeapFlags - Bitmask of VkMemoryHeapFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeapFlagBits'
type MemoryHeapFlags = MemoryHeapFlagBits

-- | VkMemoryPropertyFlagBits - Bitmask specifying properties for a memory
-- type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlags'
type MemoryPropertyFlagBits = VkMemoryPropertyFlagBits


{-# complete MEMORY_PROPERTY_DEVICE_LOCAL_BIT, MEMORY_PROPERTY_HOST_VISIBLE_BIT, MEMORY_PROPERTY_HOST_COHERENT_BIT, MEMORY_PROPERTY_HOST_CACHED_BIT, MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT, MEMORY_PROPERTY_PROTECTED_BIT :: MemoryPropertyFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
-- bit specifies that memory allocated with this type is the most efficient
-- for device access. This property will be set if and only if the memory
-- type belongs to a heap with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_HEAP_DEVICE_LOCAL_BIT'
-- set.
pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
-- bit specifies that memory allocated with this type /can/ be mapped for
-- host access using 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'.
pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_HOST_VISIBLE_BIT = VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- bit specifies that the host cache management commands
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges' and
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges' are not
-- needed to flush host writes to the device or make device writes visible
-- to the host, respectively.
pattern MEMORY_PROPERTY_HOST_COHERENT_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_HOST_COHERENT_BIT = VK_MEMORY_PROPERTY_HOST_COHERENT_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
-- bit specifies that memory allocated with this type is cached on the
-- host. Host memory accesses to uncached memory are slower than to cached
-- memory, however uncached memory is always host coherent.
pattern MEMORY_PROPERTY_HOST_CACHED_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_HOST_CACHED_BIT = VK_MEMORY_PROPERTY_HOST_CACHED_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
-- bit specifies that the memory type only allows device access to the
-- memory. Memory types /must/ not have both
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
-- set. Additionally, the object’s backing memory /may/ be provided by the
-- implementation lazily as specified in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-lazy_allocation Lazily Allocated Memory>.
pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT


-- No documentation found for Nested "MemoryPropertyFlagBits" "MEMORY_PROPERTY_PROTECTED_BIT"
pattern MEMORY_PROPERTY_PROTECTED_BIT :: (a ~ MemoryPropertyFlagBits) => a
pattern MEMORY_PROPERTY_PROTECTED_BIT = VK_MEMORY_PROPERTY_PROTECTED_BIT

-- | VkMemoryPropertyFlags - Bitmask of VkMemoryPropertyFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlags' is
-- a bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlagBits',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryType'
type MemoryPropertyFlags = MemoryPropertyFlagBits


-- | VkMemoryType - Structure specifying memory type
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryPropertyFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
data MemoryType = MemoryType
  { -- No documentation found for Nested "MemoryType" "propertyFlags"
  propertyFlags :: MemoryPropertyFlags
  , -- No documentation found for Nested "MemoryType" "heapIndex"
  heapIndex :: Word32
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkMemoryType' and
-- marshal a 'MemoryType' into it. The 'VkMemoryType' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructMemoryType :: MemoryType -> (VkMemoryType -> IO a) -> IO a
withCStructMemoryType marshalled cont = cont (VkMemoryType (propertyFlags (marshalled :: MemoryType)) (heapIndex (marshalled :: MemoryType)))

-- | A function to read a 'VkMemoryType' and all additional
-- structures in the pointer chain into a 'MemoryType'.
fromCStructMemoryType :: VkMemoryType -> IO MemoryType
fromCStructMemoryType c = MemoryType <$> pure (vkPropertyFlags (c :: VkMemoryType))
                                     <*> pure (vkHeapIndex (c :: VkMemoryType))

instance Zero MemoryType where
  zero = MemoryType zero
                    zero


data PhysicalDevice = PhysicalDevice
  { physicalDeviceHandle :: VkPhysicalDevice
  , physicalDeviceCmds    :: InstanceCmds
  }
  deriving Show

instance Eq PhysicalDevice where
  (==) = (==) `on` physicalDeviceHandle

instance Ord PhysicalDevice where
  compare = compare `on` physicalDeviceHandle



-- | VkPhysicalDeviceFeatures - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'
-- structure describe the following features:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFeatures'
data PhysicalDeviceFeatures = PhysicalDeviceFeatures
  { -- No documentation found for Nested "PhysicalDeviceFeatures" "robustBufferAccess"
  robustBufferAccess :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fullDrawIndexUint32"
  fullDrawIndexUint32 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "imageCubeArray"
  imageCubeArray :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "independentBlend"
  independentBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "geometryShader"
  geometryShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "tessellationShader"
  tessellationShader :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sampleRateShading"
  sampleRateShading :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "dualSrcBlend"
  dualSrcBlend :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "logicOp"
  logicOp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "multiDrawIndirect"
  multiDrawIndirect :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "drawIndirectFirstInstance"
  drawIndirectFirstInstance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthClamp"
  depthClamp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthBiasClamp"
  depthBiasClamp :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fillModeNonSolid"
  fillModeNonSolid :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "depthBounds"
  depthBounds :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "wideLines"
  wideLines :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "largePoints"
  largePoints :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "alphaToOne"
  alphaToOne :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "multiViewport"
  multiViewport :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "samplerAnisotropy"
  samplerAnisotropy :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionETC2"
  textureCompressionETC2 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionASTC_LDR"
  textureCompressionASTC_LDR :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "textureCompressionBC"
  textureCompressionBC :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "occlusionQueryPrecise"
  occlusionQueryPrecise :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "pipelineStatisticsQuery"
  pipelineStatisticsQuery :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "vertexPipelineStoresAndAtomics"
  vertexPipelineStoresAndAtomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "fragmentStoresAndAtomics"
  fragmentStoresAndAtomics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderTessellationAndGeometryPointSize"
  shaderTessellationAndGeometryPointSize :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderImageGatherExtended"
  shaderImageGatherExtended :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageExtendedFormats"
  shaderStorageImageExtendedFormats :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageMultisample"
  shaderStorageImageMultisample :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageReadWithoutFormat"
  shaderStorageImageReadWithoutFormat :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageWriteWithoutFormat"
  shaderStorageImageWriteWithoutFormat :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderUniformBufferArrayDynamicIndexing"
  shaderUniformBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderSampledImageArrayDynamicIndexing"
  shaderSampledImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageBufferArrayDynamicIndexing"
  shaderStorageBufferArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderStorageImageArrayDynamicIndexing"
  shaderStorageImageArrayDynamicIndexing :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderClipDistance"
  shaderClipDistance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderCullDistance"
  shaderCullDistance :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderFloat64"
  shaderFloat64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderInt64"
  shaderInt64 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderInt16"
  shaderInt16 :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderResourceResidency"
  shaderResourceResidency :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "shaderResourceMinLod"
  shaderResourceMinLod :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseBinding"
  sparseBinding :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyBuffer"
  sparseResidencyBuffer :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyImage2D"
  sparseResidencyImage2D :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyImage3D"
  sparseResidencyImage3D :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency2Samples"
  sparseResidency2Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency4Samples"
  sparseResidency4Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency8Samples"
  sparseResidency8Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidency16Samples"
  sparseResidency16Samples :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "sparseResidencyAliased"
  sparseResidencyAliased :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "variableMultisampleRate"
  variableMultisampleRate :: Bool
  , -- No documentation found for Nested "PhysicalDeviceFeatures" "inheritedQueries"
  inheritedQueries :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceFeatures' and
-- marshal a 'PhysicalDeviceFeatures' into it. The 'VkPhysicalDeviceFeatures' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceFeatures :: PhysicalDeviceFeatures -> (VkPhysicalDeviceFeatures -> IO a) -> IO a
withCStructPhysicalDeviceFeatures marshalled cont = cont (VkPhysicalDeviceFeatures (boolToBool32 (robustBufferAccess (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (fullDrawIndexUint32 (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (imageCubeArray (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (independentBlend (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (geometryShader (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (tessellationShader (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sampleRateShading (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (dualSrcBlend (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (logicOp (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (multiDrawIndirect (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (drawIndirectFirstInstance (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (depthClamp (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (depthBiasClamp (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (fillModeNonSolid (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (depthBounds (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (wideLines (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (largePoints (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (alphaToOne (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (multiViewport (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (samplerAnisotropy (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (textureCompressionETC2 (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (textureCompressionASTC_LDR (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (textureCompressionBC (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (occlusionQueryPrecise (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (pipelineStatisticsQuery (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (vertexPipelineStoresAndAtomics (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (fragmentStoresAndAtomics (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderTessellationAndGeometryPointSize (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderImageGatherExtended (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderStorageImageExtendedFormats (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderStorageImageMultisample (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderStorageImageReadWithoutFormat (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderStorageImageWriteWithoutFormat (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderUniformBufferArrayDynamicIndexing (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderSampledImageArrayDynamicIndexing (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderStorageBufferArrayDynamicIndexing (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderStorageImageArrayDynamicIndexing (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderClipDistance (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderCullDistance (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderFloat64 (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderInt64 (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderInt16 (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderResourceResidency (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (shaderResourceMinLod (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseBinding (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidencyBuffer (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidencyImage2D (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidencyImage3D (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidency2Samples (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidency4Samples (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidency8Samples (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidency16Samples (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (sparseResidencyAliased (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (variableMultisampleRate (marshalled :: PhysicalDeviceFeatures))) (boolToBool32 (inheritedQueries (marshalled :: PhysicalDeviceFeatures))))

-- | A function to read a 'VkPhysicalDeviceFeatures' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceFeatures'.
fromCStructPhysicalDeviceFeatures :: VkPhysicalDeviceFeatures -> IO PhysicalDeviceFeatures
fromCStructPhysicalDeviceFeatures c = PhysicalDeviceFeatures <$> pure (bool32ToBool (vkRobustBufferAccess (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkFullDrawIndexUint32 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkImageCubeArray (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkIndependentBlend (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkGeometryShader (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTessellationShader (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSampleRateShading (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDualSrcBlend (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkLogicOp (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkMultiDrawIndirect (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDrawIndirectFirstInstance (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDepthClamp (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDepthBiasClamp (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkFillModeNonSolid (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkDepthBounds (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkWideLines (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkLargePoints (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkAlphaToOne (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkMultiViewport (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSamplerAnisotropy (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTextureCompressionETC2 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTextureCompressionASTC_LDR (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkTextureCompressionBC (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkOcclusionQueryPrecise (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkPipelineStatisticsQuery (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkVertexPipelineStoresAndAtomics (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkFragmentStoresAndAtomics (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderTessellationAndGeometryPointSize (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderImageGatherExtended (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageExtendedFormats (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageMultisample (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageReadWithoutFormat (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageWriteWithoutFormat (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderUniformBufferArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderSampledImageArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageBufferArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderStorageImageArrayDynamicIndexing (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderClipDistance (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderCullDistance (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderFloat64 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderInt64 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderInt16 (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderResourceResidency (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkShaderResourceMinLod (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseBinding (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyBuffer (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyImage2D (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyImage3D (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency2Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency4Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency8Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidency16Samples (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkSparseResidencyAliased (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkVariableMultisampleRate (c :: VkPhysicalDeviceFeatures)))
                                                             <*> pure (bool32ToBool (vkInheritedQueries (c :: VkPhysicalDeviceFeatures)))

instance Zero PhysicalDeviceFeatures where
  zero = PhysicalDeviceFeatures False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False
                                False



-- | VkPhysicalDeviceLimits - Structure reporting implementation-dependent
-- physical device limits
--
-- = Members
--
-- The
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits'
-- are properties of the physical device. These are available in the
-- @limits@ member of the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
-- structure which is returned from
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceProperties'.
--
-- = Description
--
-- [1]
--     For all bitmasks of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits',
--     the sample count limits defined above represent the minimum
--     supported sample counts for each image type. Individual images /may/
--     support additional sample counts, which are queried using
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
--     as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-supported-sample-counts Supported Sample Counts>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags'
data PhysicalDeviceLimits = PhysicalDeviceLimits
  { -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension1D"
  maxImageDimension1D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension2D"
  maxImageDimension2D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimension3D"
  maxImageDimension3D :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageDimensionCube"
  maxImageDimensionCube :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxImageArrayLayers"
  maxImageArrayLayers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelBufferElements"
  maxTexelBufferElements :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxUniformBufferRange"
  maxUniformBufferRange :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxStorageBufferRange"
  maxStorageBufferRange :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPushConstantsSize"
  maxPushConstantsSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxMemoryAllocationCount"
  maxMemoryAllocationCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerAllocationCount"
  maxSamplerAllocationCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "bufferImageGranularity"
  bufferImageGranularity :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sparseAddressSpaceSize"
  sparseAddressSpaceSize :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxBoundDescriptorSets"
  maxBoundDescriptorSets :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorSamplers"
  maxPerStageDescriptorSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorUniformBuffers"
  maxPerStageDescriptorUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorStorageBuffers"
  maxPerStageDescriptorStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorSampledImages"
  maxPerStageDescriptorSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorStorageImages"
  maxPerStageDescriptorStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageDescriptorInputAttachments"
  maxPerStageDescriptorInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxPerStageResources"
  maxPerStageResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetSamplers"
  maxDescriptorSetSamplers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetUniformBuffers"
  maxDescriptorSetUniformBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetUniformBuffersDynamic"
  maxDescriptorSetUniformBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageBuffers"
  maxDescriptorSetStorageBuffers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageBuffersDynamic"
  maxDescriptorSetStorageBuffersDynamic :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetSampledImages"
  maxDescriptorSetSampledImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetStorageImages"
  maxDescriptorSetStorageImages :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDescriptorSetInputAttachments"
  maxDescriptorSetInputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputAttributes"
  maxVertexInputAttributes :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputBindings"
  maxVertexInputBindings :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputAttributeOffset"
  maxVertexInputAttributeOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexInputBindingStride"
  maxVertexInputBindingStride :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxVertexOutputComponents"
  maxVertexOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationGenerationLevel"
  maxTessellationGenerationLevel :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationPatchSize"
  maxTessellationPatchSize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerVertexInputComponents"
  maxTessellationControlPerVertexInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerVertexOutputComponents"
  maxTessellationControlPerVertexOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlPerPatchOutputComponents"
  maxTessellationControlPerPatchOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationControlTotalOutputComponents"
  maxTessellationControlTotalOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationEvaluationInputComponents"
  maxTessellationEvaluationInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTessellationEvaluationOutputComponents"
  maxTessellationEvaluationOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryShaderInvocations"
  maxGeometryShaderInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryInputComponents"
  maxGeometryInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryOutputComponents"
  maxGeometryOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryOutputVertices"
  maxGeometryOutputVertices :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxGeometryTotalOutputComponents"
  maxGeometryTotalOutputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentInputComponents"
  maxFragmentInputComponents :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentOutputAttachments"
  maxFragmentOutputAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentDualSrcAttachments"
  maxFragmentDualSrcAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFragmentCombinedOutputResources"
  maxFragmentCombinedOutputResources :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeSharedMemorySize"
  maxComputeSharedMemorySize :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupCount"
  maxComputeWorkGroupCount :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupInvocations"
  maxComputeWorkGroupInvocations :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxComputeWorkGroupSize"
  maxComputeWorkGroupSize :: (Word32, Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subPixelPrecisionBits"
  subPixelPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subTexelPrecisionBits"
  subTexelPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "mipmapPrecisionBits"
  mipmapPrecisionBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDrawIndexedIndexValue"
  maxDrawIndexedIndexValue :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxDrawIndirectCount"
  maxDrawIndirectCount :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerLodBias"
  maxSamplerLodBias :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSamplerAnisotropy"
  maxSamplerAnisotropy :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxViewports"
  maxViewports :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxViewportDimensions"
  maxViewportDimensions :: (Word32, Word32)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "viewportBoundsRange"
  viewportBoundsRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "viewportSubPixelBits"
  viewportSubPixelBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minMemoryMapAlignment"
  minMemoryMapAlignment :: CSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelBufferOffsetAlignment"
  minTexelBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minUniformBufferOffsetAlignment"
  minUniformBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minStorageBufferOffsetAlignment"
  minStorageBufferOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelOffset"
  minTexelOffset :: Int32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelOffset"
  maxTexelOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minTexelGatherOffset"
  minTexelGatherOffset :: Int32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxTexelGatherOffset"
  maxTexelGatherOffset :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "minInterpolationOffset"
  minInterpolationOffset :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxInterpolationOffset"
  maxInterpolationOffset :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "subPixelInterpolationOffsetBits"
  subPixelInterpolationOffsetBits :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferWidth"
  maxFramebufferWidth :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferHeight"
  maxFramebufferHeight :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxFramebufferLayers"
  maxFramebufferLayers :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferColorSampleCounts"
  framebufferColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferDepthSampleCounts"
  framebufferDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferStencilSampleCounts"
  framebufferStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "framebufferNoAttachmentsSampleCounts"
  framebufferNoAttachmentsSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxColorAttachments"
  maxColorAttachments :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageColorSampleCounts"
  sampledImageColorSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageIntegerSampleCounts"
  sampledImageIntegerSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageDepthSampleCounts"
  sampledImageDepthSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "sampledImageStencilSampleCounts"
  sampledImageStencilSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "storageImageSampleCounts"
  storageImageSampleCounts :: SampleCountFlags
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxSampleMaskWords"
  maxSampleMaskWords :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "timestampComputeAndGraphics"
  timestampComputeAndGraphics :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "timestampPeriod"
  timestampPeriod :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxClipDistances"
  maxClipDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxCullDistances"
  maxCullDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "maxCombinedClipAndCullDistances"
  maxCombinedClipAndCullDistances :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "discreteQueuePriorities"
  discreteQueuePriorities :: Word32
  , -- No documentation found for Nested "PhysicalDeviceLimits" "pointSizeRange"
  pointSizeRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "lineWidthRange"
  lineWidthRange :: (CFloat, CFloat)
  , -- No documentation found for Nested "PhysicalDeviceLimits" "pointSizeGranularity"
  pointSizeGranularity :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "lineWidthGranularity"
  lineWidthGranularity :: CFloat
  , -- No documentation found for Nested "PhysicalDeviceLimits" "strictLines"
  strictLines :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "standardSampleLocations"
  standardSampleLocations :: Bool
  , -- No documentation found for Nested "PhysicalDeviceLimits" "optimalBufferCopyOffsetAlignment"
  optimalBufferCopyOffsetAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "optimalBufferCopyRowPitchAlignment"
  optimalBufferCopyRowPitchAlignment :: DeviceSize
  , -- No documentation found for Nested "PhysicalDeviceLimits" "nonCoherentAtomSize"
  nonCoherentAtomSize :: DeviceSize
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceLimits' and
-- marshal a 'PhysicalDeviceLimits' into it. The 'VkPhysicalDeviceLimits' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceLimits :: PhysicalDeviceLimits -> (VkPhysicalDeviceLimits -> IO a) -> IO a
withCStructPhysicalDeviceLimits marshalled cont = cont (VkPhysicalDeviceLimits (maxImageDimension1D (marshalled :: PhysicalDeviceLimits)) (maxImageDimension2D (marshalled :: PhysicalDeviceLimits)) (maxImageDimension3D (marshalled :: PhysicalDeviceLimits)) (maxImageDimensionCube (marshalled :: PhysicalDeviceLimits)) (maxImageArrayLayers (marshalled :: PhysicalDeviceLimits)) (maxTexelBufferElements (marshalled :: PhysicalDeviceLimits)) (maxUniformBufferRange (marshalled :: PhysicalDeviceLimits)) (maxStorageBufferRange (marshalled :: PhysicalDeviceLimits)) (maxPushConstantsSize (marshalled :: PhysicalDeviceLimits)) (maxMemoryAllocationCount (marshalled :: PhysicalDeviceLimits)) (maxSamplerAllocationCount (marshalled :: PhysicalDeviceLimits)) (bufferImageGranularity (marshalled :: PhysicalDeviceLimits)) (sparseAddressSpaceSize (marshalled :: PhysicalDeviceLimits)) (maxBoundDescriptorSets (marshalled :: PhysicalDeviceLimits)) (maxPerStageDescriptorSamplers (marshalled :: PhysicalDeviceLimits)) (maxPerStageDescriptorUniformBuffers (marshalled :: PhysicalDeviceLimits)) (maxPerStageDescriptorStorageBuffers (marshalled :: PhysicalDeviceLimits)) (maxPerStageDescriptorSampledImages (marshalled :: PhysicalDeviceLimits)) (maxPerStageDescriptorStorageImages (marshalled :: PhysicalDeviceLimits)) (maxPerStageDescriptorInputAttachments (marshalled :: PhysicalDeviceLimits)) (maxPerStageResources (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetSamplers (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetUniformBuffers (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetUniformBuffersDynamic (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetStorageBuffers (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetStorageBuffersDynamic (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetSampledImages (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetStorageImages (marshalled :: PhysicalDeviceLimits)) (maxDescriptorSetInputAttachments (marshalled :: PhysicalDeviceLimits)) (maxVertexInputAttributes (marshalled :: PhysicalDeviceLimits)) (maxVertexInputBindings (marshalled :: PhysicalDeviceLimits)) (maxVertexInputAttributeOffset (marshalled :: PhysicalDeviceLimits)) (maxVertexInputBindingStride (marshalled :: PhysicalDeviceLimits)) (maxVertexOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxTessellationGenerationLevel (marshalled :: PhysicalDeviceLimits)) (maxTessellationPatchSize (marshalled :: PhysicalDeviceLimits)) (maxTessellationControlPerVertexInputComponents (marshalled :: PhysicalDeviceLimits)) (maxTessellationControlPerVertexOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxTessellationControlPerPatchOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxTessellationControlTotalOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxTessellationEvaluationInputComponents (marshalled :: PhysicalDeviceLimits)) (maxTessellationEvaluationOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxGeometryShaderInvocations (marshalled :: PhysicalDeviceLimits)) (maxGeometryInputComponents (marshalled :: PhysicalDeviceLimits)) (maxGeometryOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxGeometryOutputVertices (marshalled :: PhysicalDeviceLimits)) (maxGeometryTotalOutputComponents (marshalled :: PhysicalDeviceLimits)) (maxFragmentInputComponents (marshalled :: PhysicalDeviceLimits)) (maxFragmentOutputAttachments (marshalled :: PhysicalDeviceLimits)) (maxFragmentDualSrcAttachments (marshalled :: PhysicalDeviceLimits)) (maxFragmentCombinedOutputResources (marshalled :: PhysicalDeviceLimits)) (maxComputeSharedMemorySize (marshalled :: PhysicalDeviceLimits)) (fromTuple (maxComputeWorkGroupCount (marshalled :: PhysicalDeviceLimits))) (maxComputeWorkGroupInvocations (marshalled :: PhysicalDeviceLimits)) (fromTuple (maxComputeWorkGroupSize (marshalled :: PhysicalDeviceLimits))) (subPixelPrecisionBits (marshalled :: PhysicalDeviceLimits)) (subTexelPrecisionBits (marshalled :: PhysicalDeviceLimits)) (mipmapPrecisionBits (marshalled :: PhysicalDeviceLimits)) (maxDrawIndexedIndexValue (marshalled :: PhysicalDeviceLimits)) (maxDrawIndirectCount (marshalled :: PhysicalDeviceLimits)) (maxSamplerLodBias (marshalled :: PhysicalDeviceLimits)) (maxSamplerAnisotropy (marshalled :: PhysicalDeviceLimits)) (maxViewports (marshalled :: PhysicalDeviceLimits)) (fromTuple (maxViewportDimensions (marshalled :: PhysicalDeviceLimits))) (fromTuple (viewportBoundsRange (marshalled :: PhysicalDeviceLimits))) (viewportSubPixelBits (marshalled :: PhysicalDeviceLimits)) (minMemoryMapAlignment (marshalled :: PhysicalDeviceLimits)) (minTexelBufferOffsetAlignment (marshalled :: PhysicalDeviceLimits)) (minUniformBufferOffsetAlignment (marshalled :: PhysicalDeviceLimits)) (minStorageBufferOffsetAlignment (marshalled :: PhysicalDeviceLimits)) (minTexelOffset (marshalled :: PhysicalDeviceLimits)) (maxTexelOffset (marshalled :: PhysicalDeviceLimits)) (minTexelGatherOffset (marshalled :: PhysicalDeviceLimits)) (maxTexelGatherOffset (marshalled :: PhysicalDeviceLimits)) (minInterpolationOffset (marshalled :: PhysicalDeviceLimits)) (maxInterpolationOffset (marshalled :: PhysicalDeviceLimits)) (subPixelInterpolationOffsetBits (marshalled :: PhysicalDeviceLimits)) (maxFramebufferWidth (marshalled :: PhysicalDeviceLimits)) (maxFramebufferHeight (marshalled :: PhysicalDeviceLimits)) (maxFramebufferLayers (marshalled :: PhysicalDeviceLimits)) (framebufferColorSampleCounts (marshalled :: PhysicalDeviceLimits)) (framebufferDepthSampleCounts (marshalled :: PhysicalDeviceLimits)) (framebufferStencilSampleCounts (marshalled :: PhysicalDeviceLimits)) (framebufferNoAttachmentsSampleCounts (marshalled :: PhysicalDeviceLimits)) (maxColorAttachments (marshalled :: PhysicalDeviceLimits)) (sampledImageColorSampleCounts (marshalled :: PhysicalDeviceLimits)) (sampledImageIntegerSampleCounts (marshalled :: PhysicalDeviceLimits)) (sampledImageDepthSampleCounts (marshalled :: PhysicalDeviceLimits)) (sampledImageStencilSampleCounts (marshalled :: PhysicalDeviceLimits)) (storageImageSampleCounts (marshalled :: PhysicalDeviceLimits)) (maxSampleMaskWords (marshalled :: PhysicalDeviceLimits)) (boolToBool32 (timestampComputeAndGraphics (marshalled :: PhysicalDeviceLimits))) (timestampPeriod (marshalled :: PhysicalDeviceLimits)) (maxClipDistances (marshalled :: PhysicalDeviceLimits)) (maxCullDistances (marshalled :: PhysicalDeviceLimits)) (maxCombinedClipAndCullDistances (marshalled :: PhysicalDeviceLimits)) (discreteQueuePriorities (marshalled :: PhysicalDeviceLimits)) (fromTuple (pointSizeRange (marshalled :: PhysicalDeviceLimits))) (fromTuple (lineWidthRange (marshalled :: PhysicalDeviceLimits))) (pointSizeGranularity (marshalled :: PhysicalDeviceLimits)) (lineWidthGranularity (marshalled :: PhysicalDeviceLimits)) (boolToBool32 (strictLines (marshalled :: PhysicalDeviceLimits))) (boolToBool32 (standardSampleLocations (marshalled :: PhysicalDeviceLimits))) (optimalBufferCopyOffsetAlignment (marshalled :: PhysicalDeviceLimits)) (optimalBufferCopyRowPitchAlignment (marshalled :: PhysicalDeviceLimits)) (nonCoherentAtomSize (marshalled :: PhysicalDeviceLimits)))

-- | A function to read a 'VkPhysicalDeviceLimits' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceLimits'.
fromCStructPhysicalDeviceLimits :: VkPhysicalDeviceLimits -> IO PhysicalDeviceLimits
fromCStructPhysicalDeviceLimits c = PhysicalDeviceLimits <$> pure (vkMaxImageDimension1D (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageDimension2D (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageDimension3D (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageDimensionCube (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxImageArrayLayers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTexelBufferElements (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxUniformBufferRange (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxStorageBufferRange (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPushConstantsSize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxMemoryAllocationCount (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSamplerAllocationCount (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkBufferImageGranularity (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSparseAddressSpaceSize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxBoundDescriptorSets (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorSamplers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorUniformBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorStorageBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorSampledImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorStorageImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageDescriptorInputAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxPerStageResources (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetSamplers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetUniformBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetUniformBuffersDynamic (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetStorageBuffers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetStorageBuffersDynamic (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetSampledImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetStorageImages (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDescriptorSetInputAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputAttributes (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputBindings (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputAttributeOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexInputBindingStride (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxVertexOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationGenerationLevel (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationPatchSize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlPerVertexInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlPerVertexOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlPerPatchOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationControlTotalOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationEvaluationInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTessellationEvaluationOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryShaderInvocations (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryOutputVertices (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxGeometryTotalOutputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentInputComponents (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentOutputAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentDualSrcAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFragmentCombinedOutputResources (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxComputeSharedMemorySize (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let v = (vkMaxComputeWorkGroupCount (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 2 ))
                                                         <*> pure (vkMaxComputeWorkGroupInvocations (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let v = (vkMaxComputeWorkGroupSize (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 2 ))
                                                         <*> pure (vkSubPixelPrecisionBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSubTexelPrecisionBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMipmapPrecisionBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDrawIndexedIndexValue (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxDrawIndirectCount (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSamplerLodBias (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSamplerAnisotropy (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxViewports (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let v = (vkMaxViewportDimensions (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1 ))
                                                         <*> pure (let v = (vkViewportBoundsRange (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1 ))
                                                         <*> pure (vkViewportSubPixelBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinMemoryMapAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinTexelBufferOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinUniformBufferOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinStorageBufferOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinTexelOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTexelOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinTexelGatherOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxTexelGatherOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMinInterpolationOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxInterpolationOffset (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSubPixelInterpolationOffsetBits (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFramebufferWidth (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFramebufferHeight (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxFramebufferLayers (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferColorSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferDepthSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferStencilSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkFramebufferNoAttachmentsSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxColorAttachments (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageColorSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageIntegerSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageDepthSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkSampledImageStencilSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkStorageImageSampleCounts (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxSampleMaskWords (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (bool32ToBool (vkTimestampComputeAndGraphics (c :: VkPhysicalDeviceLimits)))
                                                         <*> pure (vkTimestampPeriod (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxClipDistances (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxCullDistances (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkMaxCombinedClipAndCullDistances (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkDiscreteQueuePriorities (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (let v = (vkPointSizeRange (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1 ))
                                                         <*> pure (let v = (vkLineWidthRange (c :: VkPhysicalDeviceLimits)) in ( Data.Vector.Storable.Sized.unsafeIndex v 0
                                                         , Data.Vector.Storable.Sized.unsafeIndex v 1 ))
                                                         <*> pure (vkPointSizeGranularity (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkLineWidthGranularity (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (bool32ToBool (vkStrictLines (c :: VkPhysicalDeviceLimits)))
                                                         <*> pure (bool32ToBool (vkStandardSampleLocations (c :: VkPhysicalDeviceLimits)))
                                                         <*> pure (vkOptimalBufferCopyOffsetAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkOptimalBufferCopyRowPitchAlignment (c :: VkPhysicalDeviceLimits))
                                                         <*> pure (vkNonCoherentAtomSize (c :: VkPhysicalDeviceLimits))

instance Zero PhysicalDeviceLimits where
  zero = PhysicalDeviceLimits zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              (zero, zero, zero)
                              zero
                              (zero, zero, zero)
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              (zero, zero)
                              (zero, zero)
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              False
                              zero
                              zero
                              zero
                              zero
                              zero
                              (zero, zero)
                              (zero, zero)
                              zero
                              zero
                              False
                              False
                              zero
                              zero
                              zero



-- | VkPhysicalDeviceMemoryProperties - Structure specifying physical device
-- memory properties
--
-- = Description
--
-- The
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
-- structure describes a number of /memory heaps/ as well as a number of
-- /memory types/ that /can/ be used to access memory allocated in those
-- heaps. Each heap describes a memory resource of a particular size, and
-- each memory type describes a set of memory properties (e.g. host cached
-- vs uncached) that /can/ be used with a given memory heap. Allocations
-- using a particular memory type will consume resources from the heap
-- indicated by that memory type’s heap index. More than one memory type
-- /may/ share each heap, and the heaps and memory types provide a
-- mechanism to advertise an accurate size of the physical memory resources
-- while allowing the memory to be used with a variety of different
-- properties.
--
-- The number of memory heaps is given by @memoryHeapCount@ and is less
-- than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MAX_MEMORY_HEAPS'.
-- Each heap is described by an element of the @memoryHeaps@ array as a
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap' structure.
-- The number of memory types available across all memory heaps is given by
-- @memoryTypeCount@ and is less than or equal to
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MAX_MEMORY_TYPES'.
-- Each memory type is described by an element of the @memoryTypes@ array
-- as a 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryType'
-- structure.
--
-- At least one heap /must/ include
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_HEAP_DEVICE_LOCAL_BIT'
-- in
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap'::@flags@.
-- If there are multiple heaps that all have similar performance
-- characteristics, they /may/ all include
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_HEAP_DEVICE_LOCAL_BIT'.
-- In a unified memory architecture (UMA) system there is often only a
-- single memory heap which is considered to be equally “local” to the host
-- and to the device, and such an implementation /must/ advertise the heap
-- as device-local.
--
-- Each memory type returned by
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'
-- /must/ have its @propertyFlags@ set to one of the following values:
--
-- -   0
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--     |
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
--
-- There /must/ be at least one memory type with both the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
-- and
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
-- bits set in its @propertyFlags@. There /must/ be at least one memory
-- type with the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
-- bit set in its @propertyFlags@.
--
-- For each pair of elements __X__ and __Y__ returned in @memoryTypes@,
-- __X__ /must/ be placed at a lower index position than __Y__ if:
--
-- -   either the set of bit flags returned in the @propertyFlags@ member
--     of __X__ is a strict subset of the set of bit flags returned in the
--     @propertyFlags@ member of __Y__.
--
-- -   or the @propertyFlags@ members of __X__ and __Y__ are equal, and
--     __X__ belongs to a memory heap with greater performance (as
--     determined in an implementation-specific manner).
--
-- __Note__
--
-- There is no ordering requirement between __X__ and __Y__ elements for
-- the case their @propertyFlags@ members are not in a subset relation.
-- That potentially allows more than one possible way to order the same set
-- of memory types. Notice that the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-bitmask-list list of all allowed memory property flag combinations>
-- is written in a valid order. But if instead
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
-- was before
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT'
-- |
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_MEMORY_PROPERTY_HOST_COHERENT_BIT',
-- the list would still be in a valid order.
--
-- This ordering requirement enables applications to use a simple search
-- loop to select the desired memory type along the lines of:
--
-- > // Find a memory in `memoryTypeBitsRequirement` that includes all of `requiredProperties`
-- > int32_t findProperties(const VkPhysicalDeviceMemoryProperties* pMemoryProperties,
-- >                        uint32_t memoryTypeBitsRequirement,
-- >                        VkMemoryPropertyFlags requiredProperties) {
-- >     const uint32_t memoryCount = pMemoryProperties->memoryTypeCount;
-- >     for (uint32_t memoryIndex = 0; memoryIndex < memoryCount; ++memoryIndex) {
-- >         const uint32_t memoryTypeBits = (1 << memoryIndex);
-- >         const bool isRequiredMemoryType = memoryTypeBitsRequirement & memoryTypeBits;
-- >
-- >         const VkMemoryPropertyFlags properties =
-- >             pMemoryProperties->memoryTypes[memoryIndex].propertyFlags;
-- >         const bool hasRequiredProperties =
-- >             (properties & requiredProperties) == requiredProperties;
-- >
-- >         if (isRequiredMemoryType && hasRequiredProperties)
-- >             return static_cast<int32_t>(memoryIndex);
-- >     }
-- >
-- >     // failed to find memory type
-- >     return -1;
-- > }
-- >
-- > // Try to find an optimal memory type, or if it does not exist try fallback memory type
-- > // `device` is the VkDevice
-- > // `image` is the VkImage that requires memory to be bound
-- > // `memoryProperties` properties as returned by vkGetPhysicalDeviceMemoryProperties
-- > // `requiredProperties` are the property flags that must be present
-- > // `optimalProperties` are the property flags that are preferred by the application
-- > VkMemoryRequirements memoryRequirements;
-- > vkGetImageMemoryRequirements(device, image, &memoryRequirements);
-- > int32_t memoryType =
-- >     findProperties(&memoryProperties, memoryRequirements.memoryTypeBits, optimalProperties);
-- > if (memoryType == -1) // not found; try fallback properties
-- >     memoryType =
-- >         findProperties(&memoryProperties, memoryRequirements.memoryTypeBits, requiredProperties);
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkMemoryType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceMemoryProperties'
data PhysicalDeviceMemoryProperties = PhysicalDeviceMemoryProperties
  { -- Fixed array valid count member elided
  -- No documentation found for Nested "PhysicalDeviceMemoryProperties" "memoryTypes"
  memoryTypes :: Vector MemoryType
  -- Fixed array valid count member elided
  , -- No documentation found for Nested "PhysicalDeviceMemoryProperties" "memoryHeaps"
  memoryHeaps :: Vector MemoryHeap
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceMemoryProperties' and
-- marshal a 'PhysicalDeviceMemoryProperties' into it. The 'VkPhysicalDeviceMemoryProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceMemoryProperties :: PhysicalDeviceMemoryProperties -> (VkPhysicalDeviceMemoryProperties -> IO a) -> IO a
withCStructPhysicalDeviceMemoryProperties marshalled cont = withArray withCStructMemoryHeap (memoryHeaps (marshalled :: PhysicalDeviceMemoryProperties)) (\pMemoryHeaps' -> withArray withCStructMemoryType (memoryTypes (marshalled :: PhysicalDeviceMemoryProperties)) (\pMemoryTypes' -> cont (VkPhysicalDeviceMemoryProperties (fromIntegral (Data.Vector.length (memoryTypes (marshalled :: PhysicalDeviceMemoryProperties)))) (Data.Vector.Generic.Sized.convert (padSized (VkMemoryType zeroBits 0) pMemoryTypes')) (fromIntegral (Data.Vector.length (memoryHeaps (marshalled :: PhysicalDeviceMemoryProperties)))) (Data.Vector.Generic.Sized.convert (padSized (VkMemoryHeap 0 zeroBits) pMemoryHeaps')))))

-- | A function to read a 'VkPhysicalDeviceMemoryProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceMemoryProperties'.
fromCStructPhysicalDeviceMemoryProperties :: VkPhysicalDeviceMemoryProperties -> IO PhysicalDeviceMemoryProperties
fromCStructPhysicalDeviceMemoryProperties c = PhysicalDeviceMemoryProperties <$> -- Fixed array valid count member elided
                                                                             traverse fromCStructMemoryType (Data.Vector.take (fromIntegral (vkMemoryTypeCount (c :: VkPhysicalDeviceMemoryProperties))) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkMemoryTypes (c :: VkPhysicalDeviceMemoryProperties)))))
                                                                             -- Fixed array valid count member elided
                                                                             <*> traverse fromCStructMemoryHeap (Data.Vector.take (fromIntegral (vkMemoryHeapCount (c :: VkPhysicalDeviceMemoryProperties))) (Data.Vector.Generic.convert (Data.Vector.Storable.Sized.fromSized (vkMemoryHeaps (c :: VkPhysicalDeviceMemoryProperties)))))

instance Zero PhysicalDeviceMemoryProperties where
  zero = PhysicalDeviceMemoryProperties Data.Vector.empty
                                        Data.Vector.empty



-- | VkPhysicalDeviceProperties - Structure specifying physical device
-- properties
--
-- = Description
--
-- The @vendorID@ and @deviceID@ fields are provided to allow applications
-- to adapt to device characteristics that are not adequately exposed by
-- other Vulkan queries.
--
-- __Note__
--
-- These /may/ include performance profiles, hardware errata, or other
-- characteristics.
--
-- The /vendor/ identified by @vendorID@ is the entity responsible for the
-- most salient characteristics of the underlying implementation of the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice' being
-- queried.
--
-- __Note__
--
-- For example, in the case of a discrete GPU implementation, this /should/
-- be the GPU chipset vendor. In the case of a hardware accelerator
-- integrated into a system-on-chip (SoC), this /should/ be the supplier of
-- the silicon IP used to create the accelerator.
--
-- If the vendor has a
-- <https://pcisig.com/membership/member-companies PCI vendor ID>, the low
-- 16 bits of @vendorID@ /must/ contain that PCI vendor ID, and the
-- remaining bits /must/ be set to zero. Otherwise, the value returned
-- /must/ be a valid Khronos vendor ID, obtained as described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vulkan-styleguide Vulkan Documentation and Extensions: Procedures and Conventions>
-- document in the section “Registering a Vendor ID with Khronos”. Khronos
-- vendor IDs are allocated starting at 0x10000, to distinguish them from
-- the PCI vendor ID namespace. Khronos vendor IDs are symbolically defined
-- in the 'Graphics.Vulkan.C.Core10.Core.VkVendorId' type.
--
-- The vendor is also responsible for the value returned in @deviceID@. If
-- the implementation is driven primarily by a
-- <https://pcisig.com/ PCI device> with a
-- <https://pcisig.com/ PCI device ID>, the low 16 bits of @deviceID@
-- /must/ contain that PCI device ID, and the remaining bits /must/ be set
-- to zero. Otherwise, the choice of what values to return /may/ be
-- dictated by operating system or platform policies - but /should/
-- uniquely identify both the device version and any major configuration
-- options (for example, core count in the case of multicore devices).
--
-- __Note__
--
-- The same device ID /should/ be used for all physical implementations of
-- that device version and configuration. For example, all uses of a
-- specific silicon IP GPU version and configuration /should/ use the same
-- device ID, even if those uses occur in different SoCs.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceSparseProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceProperties'
data PhysicalDeviceProperties = PhysicalDeviceProperties
  { -- No documentation found for Nested "PhysicalDeviceProperties" "apiVersion"
  apiVersion :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "driverVersion"
  driverVersion :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "vendorID"
  vendorID :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceID"
  deviceID :: Word32
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceType"
  deviceType :: PhysicalDeviceType
  , -- No documentation found for Nested "PhysicalDeviceProperties" "deviceName"
  deviceName :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceProperties" "pipelineCacheUUID"
  pipelineCacheUUID :: ByteString
  , -- No documentation found for Nested "PhysicalDeviceProperties" "limits"
  limits :: PhysicalDeviceLimits
  , -- No documentation found for Nested "PhysicalDeviceProperties" "sparseProperties"
  sparseProperties :: PhysicalDeviceSparseProperties
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceProperties' and
-- marshal a 'PhysicalDeviceProperties' into it. The 'VkPhysicalDeviceProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceProperties :: PhysicalDeviceProperties -> (VkPhysicalDeviceProperties -> IO a) -> IO a
withCStructPhysicalDeviceProperties marshalled cont = withCStructPhysicalDeviceSparseProperties (sparseProperties (marshalled :: PhysicalDeviceProperties)) (\sparseProperties'' -> withCStructPhysicalDeviceLimits (limits (marshalled :: PhysicalDeviceProperties)) (\limits'' -> cont (VkPhysicalDeviceProperties (apiVersion (marshalled :: PhysicalDeviceProperties)) (driverVersion (marshalled :: PhysicalDeviceProperties)) (vendorID (marshalled :: PhysicalDeviceProperties)) (deviceID (marshalled :: PhysicalDeviceProperties)) (deviceType (marshalled :: PhysicalDeviceProperties)) (byteStringToNullTerminatedSizedVector (deviceName (marshalled :: PhysicalDeviceProperties))) (byteStringToSizedVector (pipelineCacheUUID (marshalled :: PhysicalDeviceProperties))) limits'' sparseProperties'')))

-- | A function to read a 'VkPhysicalDeviceProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceProperties'.
fromCStructPhysicalDeviceProperties :: VkPhysicalDeviceProperties -> IO PhysicalDeviceProperties
fromCStructPhysicalDeviceProperties c = PhysicalDeviceProperties <$> pure (vkApiVersion (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkDriverVersion (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkVendorID (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkDeviceID (c :: VkPhysicalDeviceProperties))
                                                                 <*> pure (vkDeviceType (c :: VkPhysicalDeviceProperties))
                                                                 <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkDeviceName (c :: VkPhysicalDeviceProperties))) packCString
                                                                 <*> Data.Vector.Storable.unsafeWith (Data.Vector.Storable.Sized.fromSized (vkPipelineCacheUUID (c :: VkPhysicalDeviceProperties))) (\p -> packCStringLen (castPtr p, VK_UUID_SIZE))
                                                                 <*> (fromCStructPhysicalDeviceLimits (vkLimits (c :: VkPhysicalDeviceProperties)))
                                                                 <*> (fromCStructPhysicalDeviceSparseProperties (vkSparseProperties (c :: VkPhysicalDeviceProperties)))

instance Zero PhysicalDeviceProperties where
  zero = PhysicalDeviceProperties zero
                                  zero
                                  zero
                                  zero
                                  zero
                                  Data.ByteString.empty
                                  Data.ByteString.empty
                                  zero
                                  zero



-- | VkPhysicalDeviceSparseProperties - Structure specifying physical device
-- sparse memory properties
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
data PhysicalDeviceSparseProperties = PhysicalDeviceSparseProperties
  { -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard2DBlockShape"
  residencyStandard2DBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard2DMultisampleBlockShape"
  residencyStandard2DMultisampleBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyStandard3DBlockShape"
  residencyStandard3DBlockShape :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyAlignedMipSize"
  residencyAlignedMipSize :: Bool
  , -- No documentation found for Nested "PhysicalDeviceSparseProperties" "residencyNonResidentStrict"
  residencyNonResidentStrict :: Bool
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkPhysicalDeviceSparseProperties' and
-- marshal a 'PhysicalDeviceSparseProperties' into it. The 'VkPhysicalDeviceSparseProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructPhysicalDeviceSparseProperties :: PhysicalDeviceSparseProperties -> (VkPhysicalDeviceSparseProperties -> IO a) -> IO a
withCStructPhysicalDeviceSparseProperties marshalled cont = cont (VkPhysicalDeviceSparseProperties (boolToBool32 (residencyStandard2DBlockShape (marshalled :: PhysicalDeviceSparseProperties))) (boolToBool32 (residencyStandard2DMultisampleBlockShape (marshalled :: PhysicalDeviceSparseProperties))) (boolToBool32 (residencyStandard3DBlockShape (marshalled :: PhysicalDeviceSparseProperties))) (boolToBool32 (residencyAlignedMipSize (marshalled :: PhysicalDeviceSparseProperties))) (boolToBool32 (residencyNonResidentStrict (marshalled :: PhysicalDeviceSparseProperties))))

-- | A function to read a 'VkPhysicalDeviceSparseProperties' and all additional
-- structures in the pointer chain into a 'PhysicalDeviceSparseProperties'.
fromCStructPhysicalDeviceSparseProperties :: VkPhysicalDeviceSparseProperties -> IO PhysicalDeviceSparseProperties
fromCStructPhysicalDeviceSparseProperties c = PhysicalDeviceSparseProperties <$> pure (bool32ToBool (vkResidencyStandard2DBlockShape (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyStandard2DMultisampleBlockShape (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyStandard3DBlockShape (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyAlignedMipSize (c :: VkPhysicalDeviceSparseProperties)))
                                                                             <*> pure (bool32ToBool (vkResidencyNonResidentStrict (c :: VkPhysicalDeviceSparseProperties)))

instance Zero PhysicalDeviceSparseProperties where
  zero = PhysicalDeviceSparseProperties False
                                        False
                                        False
                                        False
                                        False


-- | VkPhysicalDeviceType - Supported physical device types
--
-- = Description
--
-- The physical device type is advertised for informational purposes only,
-- and does not directly affect the operation of the system. However, the
-- device type /may/ correlate with other advertised properties or
-- capabilities of the system, such as how many memory heaps there are.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
type PhysicalDeviceType = VkPhysicalDeviceType


{-# complete PHYSICAL_DEVICE_TYPE_OTHER, PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU, PHYSICAL_DEVICE_TYPE_DISCRETE_GPU, PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU, PHYSICAL_DEVICE_TYPE_CPU :: PhysicalDeviceType #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_PHYSICAL_DEVICE_TYPE_OTHER'
-- - the device does not match any other available types.
pattern PHYSICAL_DEVICE_TYPE_OTHER :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_OTHER = VK_PHYSICAL_DEVICE_TYPE_OTHER


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU'
-- - the device is typically one embedded in or tightly coupled with the
-- host.
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU'
-- - the device is typically a separate processor connected to the host via
-- an interlink.
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU'
-- - the device is typically a virtual node in a virtualization
-- environment.
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_PHYSICAL_DEVICE_TYPE_CPU'
-- - the device is typically running on the same processors as the host.
pattern PHYSICAL_DEVICE_TYPE_CPU :: (a ~ PhysicalDeviceType) => a
pattern PHYSICAL_DEVICE_TYPE_CPU = VK_PHYSICAL_DEVICE_TYPE_CPU


-- | VkQueueFamilyProperties - Structure providing information about a queue
-- family
--
-- = Description
--
-- The value returned in @minImageTransferGranularity@ has a unit of
-- compressed texel blocks for images having a block-compressed format, and
-- a unit of texels otherwise.
--
-- Possible values of @minImageTransferGranularity@ are:
--
-- -   (0,0,0) which indicates that only whole mip levels /must/ be
--     transferred using the image transfer operations on the corresponding
--     queues. In this case, the following restrictions apply to all offset
--     and extent parameters of image transfer operations:
--
--     -   The @x@, @y@, and @z@ members of a
--         'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D'
--         parameter /must/ always be zero.
--
--     -   The @width@, @height@, and @depth@ members of a
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D'
--         parameter /must/ always match the width, height, and depth of
--         the image subresource corresponding to the parameter,
--         respectively.
--
-- -   (Ax, Ay, Az) where Ax, Ay, and Az are all integer powers of two. In
--     this case the following restrictions apply to all image transfer
--     operations:
--
--     -   @x@, @y@, and @z@ of a
--         'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkOffset3D'
--         parameter /must/ be integer multiples of Ax, Ay, and Az,
--         respectively.
--
--     -   @width@ of a
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D'
--         parameter /must/ be an integer multiple of Ax, or else @x@ +
--         @width@ /must/ equal the width of the image subresource
--         corresponding to the parameter.
--
--     -   @height@ of a
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D'
--         parameter /must/ be an integer multiple of Ay, or else @y@ +
--         @height@ /must/ equal the height of the image subresource
--         corresponding to the parameter.
--
--     -   @depth@ of a
--         'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D'
--         parameter /must/ be an integer multiple of Az, or else @z@ +
--         @depth@ /must/ equal the depth of the image subresource
--         corresponding to the parameter.
--
--     -   If the format of the image corresponding to the parameters is
--         one of the block-compressed formats then for the purposes of the
--         above calculations the granularity /must/ be scaled up by the
--         compressed texel block dimensions.
--
-- Queues supporting graphics and\/or compute operations /must/ report
-- (1,1,1) in @minImageTransferGranularity@, meaning that there are no
-- additional restrictions on the granularity of image transfer operations
-- for these queues. Other queues supporting image transfer operations are
-- only /required/ to support whole mip level transfers, thus
-- @minImageTransferGranularity@ for queues belonging to such queue
-- families /may/ be (0,0,0).
--
-- The
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device Device Memory>
-- section describes memory properties queried from the physical device.
--
-- For physical device feature queries see the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features Features>
-- chapter.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkExtent3D',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceQueueFamilyProperties'
data QueueFamilyProperties = QueueFamilyProperties
  { -- No documentation found for Nested "QueueFamilyProperties" "queueFlags"
  queueFlags :: QueueFlags
  , -- No documentation found for Nested "QueueFamilyProperties" "queueCount"
  queueCount :: Word32
  , -- No documentation found for Nested "QueueFamilyProperties" "timestampValidBits"
  timestampValidBits :: Word32
  , -- No documentation found for Nested "QueueFamilyProperties" "minImageTransferGranularity"
  minImageTransferGranularity :: Extent3D
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkQueueFamilyProperties' and
-- marshal a 'QueueFamilyProperties' into it. The 'VkQueueFamilyProperties' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructQueueFamilyProperties :: QueueFamilyProperties -> (VkQueueFamilyProperties -> IO a) -> IO a
withCStructQueueFamilyProperties marshalled cont = withCStructExtent3D (minImageTransferGranularity (marshalled :: QueueFamilyProperties)) (\minImageTransferGranularity'' -> cont (VkQueueFamilyProperties (queueFlags (marshalled :: QueueFamilyProperties)) (queueCount (marshalled :: QueueFamilyProperties)) (timestampValidBits (marshalled :: QueueFamilyProperties)) minImageTransferGranularity''))

-- | A function to read a 'VkQueueFamilyProperties' and all additional
-- structures in the pointer chain into a 'QueueFamilyProperties'.
fromCStructQueueFamilyProperties :: VkQueueFamilyProperties -> IO QueueFamilyProperties
fromCStructQueueFamilyProperties c = QueueFamilyProperties <$> pure (vkQueueFlags (c :: VkQueueFamilyProperties))
                                                           <*> pure (vkQueueCount (c :: VkQueueFamilyProperties))
                                                           <*> pure (vkTimestampValidBits (c :: VkQueueFamilyProperties))
                                                           <*> (fromCStructExtent3D (vkMinImageTransferGranularity (c :: VkQueueFamilyProperties)))

instance Zero QueueFamilyProperties where
  zero = QueueFamilyProperties zero
                               zero
                               zero
                               zero


-- | VkQueueFlagBits - Bitmask specifying capabilities of queues in a queue
-- family
--
-- = Description
--
-- If an implementation exposes any queue family that supports graphics
-- operations, at least one queue family of at least one physical device
-- exposed by the implementation /must/ support both graphics and compute
-- operations.
--
-- __Note__
--
-- All commands that are allowed on a queue that supports transfer
-- operations are also allowed on a queue that supports either graphics or
-- compute operations. Thus, if the capabilities of a queue family include
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_GRAPHICS_BIT' or
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_COMPUTE_BIT',
-- then reporting the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_TRANSFER_BIT'
-- capability separately for that queue family is /optional/.
--
-- For further details see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-queues Queues>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlags'
type QueueFlagBits = VkQueueFlagBits


{-# complete QUEUE_GRAPHICS_BIT, QUEUE_COMPUTE_BIT, QUEUE_TRANSFER_BIT, QUEUE_SPARSE_BINDING_BIT, QUEUE_PROTECTED_BIT, QUEUE_RESERVED_6_BIT_KHR, QUEUE_RESERVED_5_BIT_KHR :: QueueFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_GRAPHICS_BIT'
-- specifies that queues in this queue family support graphics operations.
pattern QUEUE_GRAPHICS_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_GRAPHICS_BIT = VK_QUEUE_GRAPHICS_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_COMPUTE_BIT'
-- specifies that queues in this queue family support compute operations.
pattern QUEUE_COMPUTE_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_COMPUTE_BIT = VK_QUEUE_COMPUTE_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_TRANSFER_BIT'
-- specifies that queues in this queue family support transfer operations.
pattern QUEUE_TRANSFER_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_TRANSFER_BIT = VK_QUEUE_TRANSFER_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_QUEUE_SPARSE_BINDING_BIT'
-- specifies that queues in this queue family support sparse memory
-- management operations (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory Sparse Resources>).
-- If any of the sparse resource features are enabled, then at least one
-- queue family /must/ support this bit.
pattern QUEUE_SPARSE_BINDING_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_SPARSE_BINDING_BIT = VK_QUEUE_SPARSE_BINDING_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_PROTECTED_BIT"
pattern QUEUE_PROTECTED_BIT :: (a ~ QueueFlagBits) => a
pattern QUEUE_PROTECTED_BIT = VK_QUEUE_PROTECTED_BIT


-- No documentation found for Nested "QueueFlagBits" "QUEUE_RESERVED_6_BIT_KHR"
pattern QUEUE_RESERVED_6_BIT_KHR :: (a ~ QueueFlagBits) => a
pattern QUEUE_RESERVED_6_BIT_KHR = VK_QUEUE_RESERVED_6_BIT_KHR


-- No documentation found for Nested "QueueFlagBits" "QUEUE_RESERVED_5_BIT_KHR"
pattern QUEUE_RESERVED_5_BIT_KHR :: (a ~ QueueFlagBits) => a
pattern QUEUE_RESERVED_5_BIT_KHR = VK_QUEUE_RESERVED_5_BIT_KHR

-- | VkQueueFlags - Bitmask of VkQueueFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFlagBits'
type QueueFlags = QueueFlagBits

-- | VkSampleCountFlagBits - Bitmask specifying sample counts supported for
-- an image used for storage operations
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2.VkAttachmentDescription2KHR',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type SampleCountFlagBits = VkSampleCountFlagBits


{-# complete SAMPLE_COUNT_1_BIT, SAMPLE_COUNT_2_BIT, SAMPLE_COUNT_4_BIT, SAMPLE_COUNT_8_BIT, SAMPLE_COUNT_16_BIT, SAMPLE_COUNT_32_BIT, SAMPLE_COUNT_64_BIT :: SampleCountFlagBits #-}


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_1_BIT'
-- specifies an image with one sample per pixel.
pattern SAMPLE_COUNT_1_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_1_BIT = VK_SAMPLE_COUNT_1_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_2_BIT'
-- specifies an image with 2 samples per pixel.
pattern SAMPLE_COUNT_2_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_2_BIT = VK_SAMPLE_COUNT_2_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_4_BIT'
-- specifies an image with 4 samples per pixel.
pattern SAMPLE_COUNT_4_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_4_BIT = VK_SAMPLE_COUNT_4_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_8_BIT'
-- specifies an image with 8 samples per pixel.
pattern SAMPLE_COUNT_8_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_8_BIT = VK_SAMPLE_COUNT_8_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_16_BIT'
-- specifies an image with 16 samples per pixel.
pattern SAMPLE_COUNT_16_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_16_BIT = VK_SAMPLE_COUNT_16_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_32_BIT'
-- specifies an image with 32 samples per pixel.
pattern SAMPLE_COUNT_32_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_32_BIT = VK_SAMPLE_COUNT_32_BIT


-- | 'Graphics.Vulkan.C.Core10.DeviceInitialization.VK_SAMPLE_COUNT_64_BIT'
-- specifies an image with 64 samples per pixel.
pattern SAMPLE_COUNT_64_BIT :: (a ~ SampleCountFlagBits) => a
pattern SAMPLE_COUNT_64_BIT = VK_SAMPLE_COUNT_64_BIT

-- | VkSampleCountFlags - Bitmask of VkSampleCountFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkSampleCountFlagBits'
type SampleCountFlags = SampleCountFlagBits


-- | vkCreateInstance - Create a new Vulkan instance
--
-- = Parameters
--
-- -   @pCreateInfo@ points to an instance of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'
--     controlling creation of the instance.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pInstance@ points a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle in
--     which the resulting instance is returned.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'
-- verifies that the requested layers exist. If not,
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance' will
-- return 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_LAYER_NOT_PRESENT'. Next
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'
-- verifies that the requested extensions are supported (e.g. in the
-- implementation or in any enabled instance layer) and if any requested
-- extension is not supported,
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance' /must/
-- return 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_EXTENSION_NOT_PRESENT'.
-- After verifying and enabling the instance layers and extensions the
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' object is
-- created and returned to the application. If a requested extension is
-- only supported by a layer, both the layer and the extension need to be
-- specified at
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance' time
-- for the creation to succeed.
--
-- == Valid Usage
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required extensions>
--     for each extension in the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'::@ppEnabledExtensionNames@
--     list /must/ also be present in that list.
--
-- == Valid Usage (Implicit)
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'
--     structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- -   @pInstance@ /must/ be a valid pointer to a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_LAYER_NOT_PRESENT'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_EXTENSION_NOT_PRESENT'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INCOMPATIBLE_DRIVER'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo'
createInstance :: InstanceCreateInfo ->  Maybe AllocationCallbacks ->  IO (Instance)
createInstance = \createInfo' -> \allocator -> alloca (\pInstance' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructInstanceCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateInstance pCreateInfo' pAllocator pInstance' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pInstance' >>= (\instanceH -> Instance instanceH <$> initInstanceCmds instanceH))))))


-- | vkDestroyInstance - Destroy an instance of Vulkan
--
-- = Parameters
--
-- -   @instance@ is the handle of the instance to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All child objects created using @instance@ /must/ have been
--     destroyed prior to destroying @instance@
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @instance@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @instance@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   If @instance@ is not @NULL@, @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     structure
--
-- == Host Synchronization
--
-- -   Host access to @instance@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
destroyInstance :: Instance ->  Maybe AllocationCallbacks ->  IO ()
destroyInstance = \(Instance instance' commandTable) -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyInstance commandTable instance' pAllocator *> (pure ()))


-- | vkEnumeratePhysicalDevices - Enumerates the physical devices accessible
-- to a Vulkan instance
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'.
--
-- -   @pPhysicalDeviceCount@ is a pointer to an integer related to the
--     number of physical devices available or queried, as described below.
--
-- -   @pPhysicalDevices@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handles.
--
-- = Description
--
-- If @pPhysicalDevices@ is @NULL@, then the number of physical devices
-- available is returned in @pPhysicalDeviceCount@. Otherwise,
-- @pPhysicalDeviceCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pPhysicalDevices@ array, and on return the
-- variable is overwritten with the number of handles actually written to
-- @pPhysicalDevices@. If @pPhysicalDeviceCount@ is less than the number of
-- physical devices available, at most @pPhysicalDeviceCount@ structures
-- will be written. If @pPhysicalDeviceCount@ is smaller than the number of
-- physical devices available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available physical devices were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pPhysicalDeviceCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPhysicalDeviceCount@ is not @0@, and
--     @pPhysicalDevices@ is not @NULL@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @pPhysicalDeviceCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handles
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getNumPhysicalDevices :: Instance ->  IO (VkResult, Word32)
getNumPhysicalDevices = \(Instance instance' commandTable) -> alloca (\pPhysicalDeviceCount' -> vkEnumeratePhysicalDevices commandTable instance' pPhysicalDeviceCount' nullPtr >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>peek pPhysicalDeviceCount')))

-- | vkEnumeratePhysicalDevices - Enumerates the physical devices accessible
-- to a Vulkan instance
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.vkCreateInstance'.
--
-- -   @pPhysicalDeviceCount@ is a pointer to an integer related to the
--     number of physical devices available or queried, as described below.
--
-- -   @pPhysicalDevices@ is either @NULL@ or a pointer to an array of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handles.
--
-- = Description
--
-- If @pPhysicalDevices@ is @NULL@, then the number of physical devices
-- available is returned in @pPhysicalDeviceCount@. Otherwise,
-- @pPhysicalDeviceCount@ /must/ point to a variable set by the user to the
-- number of elements in the @pPhysicalDevices@ array, and on return the
-- variable is overwritten with the number of handles actually written to
-- @pPhysicalDevices@. If @pPhysicalDeviceCount@ is less than the number of
-- physical devices available, at most @pPhysicalDeviceCount@ structures
-- will be written. If @pPhysicalDeviceCount@ is smaller than the number of
-- physical devices available,
-- 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' will be returned instead
-- of 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS', to indicate that not all
-- the available physical devices were returned.
--
-- == Valid Usage (Implicit)
--
-- -   @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pPhysicalDeviceCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPhysicalDeviceCount@ is not @0@, and
--     @pPhysicalDevices@ is not @NULL@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @pPhysicalDeviceCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handles
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
enumeratePhysicalDevices :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDevice)
enumeratePhysicalDevices = \(Instance instance' commandTable) -> \physicalDeviceCount' -> allocaArray (fromIntegral physicalDeviceCount') (\pPhysicalDevices' -> with physicalDeviceCount' (\pPhysicalDeviceCount' -> vkEnumeratePhysicalDevices commandTable instance' pPhysicalDeviceCount' pPhysicalDevices' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(flip Data.Vector.generateM ((\p i -> PhysicalDevice <$> peekElemOff p i <*> pure commandTable) pPhysicalDevices') =<< (fromIntegral <$> (peek pPhysicalDeviceCount')))))))
-- | Returns all the values available from 'enumeratePhysicalDevices'.
enumerateAllPhysicalDevices :: Instance ->  IO (Vector PhysicalDevice)
enumerateAllPhysicalDevices instance' =
  snd <$> getNumPhysicalDevices instance'
    >>= \num -> snd <$> enumeratePhysicalDevices instance' num



-- | vkGetDeviceProcAddr - Return a function pointer for a command
--
-- = Parameters
--
-- The table below defines the various use cases for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetDeviceProcAddr' and
-- expected return value for each case.
--
-- = Description
--
-- The returned function pointer is of type
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkVoidFunction', and
-- must be cast to the type of the command being queried. The function
-- pointer /must/ only be called with a dispatchable object (the first
-- parameter) that is @device@ or a child of @device@.
--
-- > +-----------------------+-----------------------+-----------------------+
-- > | @device@              | @pName@               | return value          |
-- > +=======================+=======================+=======================+
-- > | @NULL@                | *                     | undefined             |
-- > +-----------------------+-----------------------+-----------------------+
-- > | invalid device        | *                     | undefined             |
-- > +-----------------------+-----------------------+-----------------------+
-- > | device                | @NULL@                | undefined             |
-- > +-----------------------+-----------------------+-----------------------+
-- > | device                | core device-level     | fp                    |
-- > |                       | Vulkan command        |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | device                | enabled device        | fp                    |
-- > |                       | extension commands    |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | device                | * (any @pName@ not    | @NULL@                |
-- > |                       | covered above)        |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > vkGetDeviceProcAddr behavior
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkVoidFunction',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice'
getDeviceProcAddr :: Device ->  ByteString ->  IO (PFN_vkVoidFunction)
getDeviceProcAddr = \(Device device' commandTable) -> \name' -> useAsCString name' (\pName' -> vkGetDeviceProcAddr commandTable device' pName' >>= (\ret -> pure ret))


-- | vkGetInstanceProcAddr - Return a function pointer for a command
--
-- = Parameters
--
-- -   @instance@ is the instance that the function pointer will be
--     compatible with, or @NULL@ for commands not dependent on any
--     instance.
--
-- -   @pName@ is the name of the command to obtain.
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetInstanceProcAddr'
-- itself is obtained in a platform- and loader- specific manner.
-- Typically, the loader library will export this command as a function
-- symbol, so applications /can/ link against the loader library, or load
-- it dynamically and look up the symbol using platform-specific APIs.
--
-- The table below defines the various use cases for
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetInstanceProcAddr'
-- and expected return value (“fp” is “function pointer”) for each case.
--
-- The returned function pointer is of type
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkVoidFunction', and
-- must be cast to the type of the command being queried.
--
-- > +-----------------------+-----------------------+-----------------------+
-- > | @instance@            | @pName@               | return value          |
-- > +=======================+=======================+=======================+
-- > | *                     | @NULL@                | undefined             |
-- > +-----------------------+-----------------------+-----------------------+
-- > | invalid instance      | *                     | undefined             |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @NULL@                | 'Graphics.Vulkan.C.Co | fp                    |
-- > |                       | re10.ExtensionDiscove |                       |
-- > |                       | ry.vkEnumerateInstanc |                       |
-- > |                       | eExtensionProperties' |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @NULL@                | 'Graphics.Vulkan.C.Co | fp                    |
-- > |                       | re10.LayerDiscovery.v |                       |
-- > |                       | kEnumerateInstanceLay |                       |
-- > |                       | erProperties'         |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @NULL@                | 'Graphics.Vulkan.C.Co | fp                    |
-- > |                       | re10.DeviceInitializa |                       |
-- > |                       | tion.vkCreateInstance |                       |
-- > |                       | '                     |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | @NULL@                | * (any @pName@ not    | @NULL@                |
-- > |                       | covered above)        |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | instance              | core Vulkan command   | fp1                   |
-- > +-----------------------+-----------------------+-----------------------+
-- > | instance              | enabled instance      | fp1                   |
-- > |                       | extension commands    |                       |
-- > |                       | for @instance@        |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | instance              | available device      | fp1                   |
-- > |                       | extension2 commands   |                       |
-- > |                       | for @instance@        |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- > | instance              | * (any @pName@ not    | @NULL@                |
-- > |                       | covered above)        |                       |
-- > +-----------------------+-----------------------+-----------------------+
-- >
-- > vkGetInstanceProcAddr behavior
--
-- [1]
--     The returned function pointer /must/ only be called with a
--     dispatchable object (the first parameter) that is @instance@ or a
--     child of @instance@, e.g.
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
--     'Graphics.Vulkan.C.Core10.Queue.VkQueue', or
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'.
--
-- [2]
--     An “available device extension” is a device extension supported by
--     any physical device enumerated by @instance@.
--
-- == Valid Usage (Implicit)
--
-- -   If @instance@ is not @NULL@, @instance@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance' handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.PFN_vkVoidFunction',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstance'
getInstanceProcAddr :: Instance ->  ByteString ->  IO (PFN_vkVoidFunction)
getInstanceProcAddr = \(Instance instance' commandTable) -> \name' -> useAsCString name' (\pName' -> vkGetInstanceProcAddr commandTable instance' pName' >>= (\ret -> pure ret))


-- | vkGetPhysicalDeviceFeatures - Reports capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     supported features.
--
-- -   @pFeatures@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'
--     structure in which the physical device features are returned. For
--     each feature, a value of 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--     specifies that the feature is supported on this physical device, and
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE' specifies that the feature
--     is not supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceFeatures'
getPhysicalDeviceFeatures :: PhysicalDevice ->  IO (PhysicalDeviceFeatures)
getPhysicalDeviceFeatures = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pFeatures' -> vkGetPhysicalDeviceFeatures commandTable physicalDevice' pFeatures' *> ((fromCStructPhysicalDeviceFeatures <=< peek) pFeatures'))


-- | vkGetPhysicalDeviceFormatProperties - Lists physical device’s format
-- capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     format properties.
--
-- -   @format@ is the format whose properties are queried.
--
-- -   @pFormatProperties@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties'
--     structure in which physical device properties for @format@ are
--     returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkFormatProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceFormatProperties :: PhysicalDevice ->  Format ->  IO (FormatProperties)
getPhysicalDeviceFormatProperties = \(PhysicalDevice physicalDevice' commandTable) -> \format' -> alloca (\pFormatProperties' -> vkGetPhysicalDeviceFormatProperties commandTable physicalDevice' format' pFormatProperties' *> ((fromCStructFormatProperties <=< peek) pFormatProperties'))


-- | vkGetPhysicalDeviceImageFormatProperties - Lists physical device’s image
-- format capabilities
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     image capabilities.
--
-- -   @format@ is a 'Graphics.Vulkan.C.Core10.Core.VkFormat' value
--     specifying the image format, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@format@.
--
-- -   @type@ is a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType' value
--     specifying the image type, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@imageType@.
--
-- -   @tiling@ is a
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling' value
--     specifying the image tiling, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@tiling@.
--
-- -   @usage@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlagBits'
--     specifying the intended usage of the image, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@usage@.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlagBits'
--     specifying additional parameters of the image, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@.
--
-- -   @pImageFormatProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties'
--     structure in which capabilities are returned.
--
-- = Description
--
-- The @format@, @type@, @tiling@, @usage@, and @flags@ parameters
-- correspond to parameters that would be consumed by
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage' (as members of
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo').
--
-- If @format@ is not a supported image format, or if the combination of
-- @format@, @type@, @tiling@, @usage@, and @flags@ is not supported for
-- images, then
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
-- returns 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- The limitations on an image format that are reported by
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties'
-- have the following property: if @usage1@ and @usage2@ of type
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags' are
-- such that the bits set in @usage1@ are a subset of the bits set in
-- @usage2@, and @flags1@ and @flags2@ of type
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags' are
-- such that the bits set in @flags1@ are a subset of the bits set in
-- @flags2@, then the limitations for @usage1@ and @flags1@ /must/ be no
-- more strict than the limitations for @usage2@ and @flags2@, for all
-- values of @format@, @type@, and @tiling@.
--
-- == Return Codes
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes Success>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS'
--
-- [<https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes Failure>]
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY'
--
--     -   'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED'
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageCreateFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageTiling',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageType',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkImageUsageFlags',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
getPhysicalDeviceImageFormatProperties :: PhysicalDevice ->  Format ->  ImageType ->  ImageTiling ->  ImageUsageFlags ->  ImageCreateFlags ->  IO (ImageFormatProperties)
getPhysicalDeviceImageFormatProperties = \(PhysicalDevice physicalDevice' commandTable) -> \format' -> \type' -> \tiling' -> \usage' -> \flags' -> alloca (\pImageFormatProperties' -> vkGetPhysicalDeviceImageFormatProperties commandTable physicalDevice' format' type' tiling' usage' flags' pImageFormatProperties' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((fromCStructImageFormatProperties <=< peek) pImageFormatProperties')))


-- | vkGetPhysicalDeviceMemoryProperties - Reports memory information for the
-- specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the device to query.
--
-- -   @pMemoryProperties@ points to an instance of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
--     structure in which the properties are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceMemoryProperties'
getPhysicalDeviceMemoryProperties :: PhysicalDevice ->  IO (PhysicalDeviceMemoryProperties)
getPhysicalDeviceMemoryProperties = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pMemoryProperties' -> vkGetPhysicalDeviceMemoryProperties commandTable physicalDevice' pMemoryProperties' *> ((fromCStructPhysicalDeviceMemoryProperties <=< peek) pMemoryProperties'))


-- | vkGetPhysicalDeviceProperties - Returns properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pProperties@ points to an instance of the
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
--     structure, that will be filled with returned information.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDeviceProperties'
getPhysicalDeviceProperties :: PhysicalDevice ->  IO (PhysicalDeviceProperties)
getPhysicalDeviceProperties = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pProperties' -> vkGetPhysicalDeviceProperties commandTable physicalDevice' pProperties' *> ((fromCStructPhysicalDeviceProperties <=< peek) pProperties'))


-- | vkGetPhysicalDeviceQueueFamilyProperties - Reports properties of the
-- queues of the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pQueueFamilyPropertyCount@ is a pointer to an integer related to
--     the number of queue families available or queried, as described
--     below.
--
-- -   @pQueueFamilyProperties@ is either @NULL@ or a pointer to an array
--     of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--     structures.
--
-- = Description
--
-- If @pQueueFamilyProperties@ is @NULL@, then the number of queue families
-- available is returned in @pQueueFamilyPropertyCount@. Implementations
-- /must/ support at least one queue family. Otherwise,
-- @pQueueFamilyPropertyCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pQueueFamilyProperties@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pQueueFamilyProperties@. If
-- @pQueueFamilyPropertyCount@ is less than the number of queue families
-- available, at most @pQueueFamilyPropertyCount@ structures will be
-- written.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
getNumPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  IO (Word32)
getNumPhysicalDeviceQueueFamilyProperties = \(PhysicalDevice physicalDevice' commandTable) -> alloca (\pQueueFamilyPropertyCount' -> vkGetPhysicalDeviceQueueFamilyProperties commandTable physicalDevice' pQueueFamilyPropertyCount' nullPtr *> (peek pQueueFamilyPropertyCount'))

-- | vkGetPhysicalDeviceQueueFamilyProperties - Reports properties of the
-- queues of the specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pQueueFamilyPropertyCount@ is a pointer to an integer related to
--     the number of queue families available or queried, as described
--     below.
--
-- -   @pQueueFamilyProperties@ is either @NULL@ or a pointer to an array
--     of
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--     structures.
--
-- = Description
--
-- If @pQueueFamilyProperties@ is @NULL@, then the number of queue families
-- available is returned in @pQueueFamilyPropertyCount@. Implementations
-- /must/ support at least one queue family. Otherwise,
-- @pQueueFamilyPropertyCount@ /must/ point to a variable set by the user
-- to the number of elements in the @pQueueFamilyProperties@ array, and on
-- return the variable is overwritten with the number of structures
-- actually written to @pQueueFamilyProperties@. If
-- @pQueueFamilyPropertyCount@ is less than the number of queue families
-- available, at most @pQueueFamilyPropertyCount@ structures will be
-- written.
--
-- == Valid Usage (Implicit)
--
-- -   @physicalDevice@ /must/ be a valid
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice'
--     handle
--
-- -   @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
--     structures
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkPhysicalDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkQueueFamilyProperties'
getPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  Word32 ->  IO (Vector QueueFamilyProperties)
getPhysicalDeviceQueueFamilyProperties = \(PhysicalDevice physicalDevice' commandTable) -> \queueFamilyPropertyCount' -> allocaArray (fromIntegral queueFamilyPropertyCount') (\pQueueFamilyProperties' -> with queueFamilyPropertyCount' (\pQueueFamilyPropertyCount' -> vkGetPhysicalDeviceQueueFamilyProperties commandTable physicalDevice' pQueueFamilyPropertyCount' pQueueFamilyProperties' *> ((flip Data.Vector.generateM ((\p -> fromCStructQueueFamilyProperties <=< peekElemOff p) pQueueFamilyProperties') =<< (fromIntegral <$> (peek pQueueFamilyPropertyCount'))))))
-- | Returns all the values available from 'getPhysicalDeviceQueueFamilyProperties'.
getAllPhysicalDeviceQueueFamilyProperties :: PhysicalDevice ->  IO (Vector QueueFamilyProperties)
getAllPhysicalDeviceQueueFamilyProperties physicalDevice' =
  getNumPhysicalDeviceQueueFamilyProperties physicalDevice'
    >>= \num -> getPhysicalDeviceQueueFamilyProperties physicalDevice' num


-- | A safe wrapper for 'createInstance' and 'destroyInstance' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withInstance
  :: InstanceCreateInfo -> Maybe (AllocationCallbacks) -> (Instance -> IO a) -> IO a
withInstance instanceCreateInfo allocationCallbacks = bracket
  (createInstance instanceCreateInfo allocationCallbacks)
  (\o -> destroyInstance o allocationCallbacks)

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR = VkFormatFeatureFlagBits 0x02000000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR = VkFormatFeatureFlagBits 0x04000000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR = VkFormatFeatureFlagBits 0x08000000

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR"
pattern VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR = VkFormatFeatureFlagBits 0x10000000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_10_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_10_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_10_BIT_KHR = VkImageUsageFlagBits 0x00000400

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_11_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_11_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_11_BIT_KHR = VkImageUsageFlagBits 0x00000800

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_12_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_12_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_12_BIT_KHR = VkImageUsageFlagBits 0x00001000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_13_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_13_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_13_BIT_KHR = VkImageUsageFlagBits 0x00002000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_14_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_14_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_14_BIT_KHR = VkImageUsageFlagBits 0x00004000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_RESERVED_15_BIT_KHR"
pattern VK_IMAGE_USAGE_RESERVED_15_BIT_KHR :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_RESERVED_15_BIT_KHR = VkImageUsageFlagBits 0x00008000

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_RESERVED_5_BIT_KHR"
pattern VK_QUEUE_RESERVED_5_BIT_KHR :: VkQueueFlagBits
pattern VK_QUEUE_RESERVED_5_BIT_KHR = VkQueueFlagBits 0x00000020

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_RESERVED_6_BIT_KHR"
pattern VK_QUEUE_RESERVED_6_BIT_KHR :: VkQueueFlagBits
pattern VK_QUEUE_RESERVED_6_BIT_KHR = VkQueueFlagBits 0x00000040
