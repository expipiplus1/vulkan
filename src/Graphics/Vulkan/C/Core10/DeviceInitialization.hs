{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Core10.DeviceInitialization
  ( PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , PFN_vkVoidFunction
  , VK_MAX_MEMORY_HEAPS
  , pattern VK_MAX_MEMORY_HEAPS
  , VK_MAX_MEMORY_TYPES
  , pattern VK_MAX_MEMORY_TYPES
  , VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , VK_UUID_SIZE
  , pattern VK_UUID_SIZE
  , VkAllocationCallbacks(..)
  , VkApplicationInfo(..)
  , VkDevice
  , VkDeviceSize
  , VkExtent3D(..)
  , VkFormatFeatureFlagBits(..)
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
  , pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
  , pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT
  , pattern VK_FORMAT_FEATURE_BLIT_DST_BIT
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
  , VkFormatFeatureFlags
  , VkFormatProperties(..)
  , VkImageCreateFlagBits(..)
  , pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT
  , pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT
  , pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT
  , pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT
  , VkImageCreateFlags
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , pattern VK_IMAGE_TILING_OPTIMAL
  , pattern VK_IMAGE_TILING_LINEAR
  , VkImageType(..)
  , pattern VK_IMAGE_TYPE_1D
  , pattern VK_IMAGE_TYPE_2D
  , pattern VK_IMAGE_TYPE_3D
  , VkImageUsageFlagBits(..)
  , pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT
  , pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT
  , pattern VK_IMAGE_USAGE_SAMPLED_BIT
  , pattern VK_IMAGE_USAGE_STORAGE_BIT
  , pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
  , pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT
  , VkImageUsageFlags
  , VkInstance
  , VkInstanceCreateFlags(..)
  , VkInstanceCreateInfo(..)
  , VkInternalAllocationType(..)
  , pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE
  , VkMemoryHeap(..)
  , VkMemoryHeapFlagBits(..)
  , pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT
  , VkMemoryHeapFlags
  , VkMemoryPropertyFlagBits(..)
  , pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
  , pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT
  , pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT
  , VkMemoryPropertyFlags
  , VkMemoryType(..)
  , VkPhysicalDevice
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkPhysicalDeviceType(..)
  , pattern VK_PHYSICAL_DEVICE_TYPE_OTHER
  , pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU
  , pattern VK_PHYSICAL_DEVICE_TYPE_CPU
  , VkQueueFamilyProperties(..)
  , VkQueueFlagBits(..)
  , pattern VK_QUEUE_GRAPHICS_BIT
  , pattern VK_QUEUE_COMPUTE_BIT
  , pattern VK_QUEUE_TRANSFER_BIT
  , pattern VK_QUEUE_SPARSE_BINDING_BIT
  , VkQueueFlags
  , VkSampleCountFlagBits(..)
  , pattern VK_SAMPLE_COUNT_1_BIT
  , pattern VK_SAMPLE_COUNT_2_BIT
  , pattern VK_SAMPLE_COUNT_4_BIT
  , pattern VK_SAMPLE_COUNT_8_BIT
  , pattern VK_SAMPLE_COUNT_16_BIT
  , pattern VK_SAMPLE_COUNT_32_BIT
  , pattern VK_SAMPLE_COUNT_64_BIT
  , VkSampleCountFlags
  , VkSystemAllocationScope(..)
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE
  , pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE
  , FN_vkCreateInstance
  , PFN_vkCreateInstance
  , vkCreateInstance
  , FN_vkDestroyInstance
  , PFN_vkDestroyInstance
  , vkDestroyInstance
  , FN_vkEnumeratePhysicalDevices
  , PFN_vkEnumeratePhysicalDevices
  , vkEnumeratePhysicalDevices
  , FN_vkGetDeviceProcAddr
  , PFN_vkGetDeviceProcAddr
  , vkGetDeviceProcAddr
  , vkGetInstanceProcAddr'
  , FN_vkGetInstanceProcAddr
  , PFN_vkGetInstanceProcAddr
  , vkGetInstanceProcAddr
  , FN_vkGetPhysicalDeviceFeatures
  , PFN_vkGetPhysicalDeviceFeatures
  , vkGetPhysicalDeviceFeatures
  , FN_vkGetPhysicalDeviceFormatProperties
  , PFN_vkGetPhysicalDeviceFormatProperties
  , vkGetPhysicalDeviceFormatProperties
  , FN_vkGetPhysicalDeviceImageFormatProperties
  , PFN_vkGetPhysicalDeviceImageFormatProperties
  , vkGetPhysicalDeviceImageFormatProperties
  , FN_vkGetPhysicalDeviceMemoryProperties
  , PFN_vkGetPhysicalDeviceMemoryProperties
  , vkGetPhysicalDeviceMemoryProperties
  , FN_vkGetPhysicalDeviceProperties
  , PFN_vkGetPhysicalDeviceProperties
  , vkGetPhysicalDeviceProperties
  , FN_vkGetPhysicalDeviceQueueFamilyProperties
  , PFN_vkGetPhysicalDeviceQueueFamilyProperties
  , vkGetPhysicalDeviceQueueFamilyProperties
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  , Word64
  , Word8
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , castPtrToFunPtr
  , nullPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import qualified GHC.Ptr
  ( Ptr(Ptr)
  )
import GHC.Read
  ( choose
  , expectP
  )
import System.IO.Unsafe
  ( unsafeDupablePerformIO
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
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

-- No documentation found for TopLevel "VK_MAX_MEMORY_HEAPS"
type VK_MAX_MEMORY_HEAPS = 16
-- No documentation found for Nested "Integral a => a" "VK_MAX_MEMORY_HEAPS"
pattern VK_MAX_MEMORY_HEAPS :: Integral a => a
pattern VK_MAX_MEMORY_HEAPS = 16

-- No documentation found for TopLevel "VK_MAX_MEMORY_TYPES"
type VK_MAX_MEMORY_TYPES = 32
-- No documentation found for Nested "Integral a => a" "VK_MAX_MEMORY_TYPES"
pattern VK_MAX_MEMORY_TYPES :: Integral a => a
pattern VK_MAX_MEMORY_TYPES = 32

-- No documentation found for TopLevel "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE"
type VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256
-- No documentation found for Nested "Integral a => a" "VK_MAX_PHYSICAL_DEVICE_NAME_SIZE"
pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE :: Integral a => a
pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE = 256

-- No documentation found for TopLevel "VK_UUID_SIZE"
type VK_UUID_SIZE = 16
-- No documentation found for Nested "Integral a => a" "VK_UUID_SIZE"
pattern VK_UUID_SIZE :: Integral a => a
pattern VK_UUID_SIZE = 16

-- | VkAllocationCallbacks - Structure containing callback function pointers
-- for memory allocation
--
-- == Valid Usage
--
-- -   @pfnAllocation@ /must/ be a valid pointer to a valid user-defined
--     'PFN_vkAllocationFunction'
--
-- -   @pfnReallocation@ /must/ be a valid pointer to a valid user-defined
--     'PFN_vkReallocationFunction'
--
-- -   @pfnFree@ /must/ be a valid pointer to a valid user-defined
--     'PFN_vkFreeFunction'
--
-- -   If either of @pfnInternalAllocation@ or @pfnInternalFree@ is not
--     @NULL@, both /must/ be valid callbacks
--
-- = See Also
--
-- 'PFN_vkAllocationFunction', 'PFN_vkFreeFunction',
-- 'PFN_vkInternalAllocationNotification',
-- 'PFN_vkInternalFreeNotification', 'PFN_vkReallocationFunction',
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
-- 'vkCreateInstance',
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
-- 'vkDestroyInstance',
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
data VkAllocationCallbacks = VkAllocationCallbacks
  { -- | @pUserData@ is a value to be interpreted by the implementation of the
  -- callbacks. When any of the callbacks in 'VkAllocationCallbacks' are
  -- called, the Vulkan implementation will pass this value as the first
  -- parameter to the callback. This value /can/ vary each time an allocator
  -- is passed into a command, even when the same object takes an allocator
  -- in multiple commands.
  vkPUserData :: Ptr ()
  , -- | @pfnAllocation@ is a pointer to an application-defined memory allocation
  -- function of type 'PFN_vkAllocationFunction'.
  vkPfnAllocation :: PFN_vkAllocationFunction
  , -- | @pfnReallocation@ is a pointer to an application-defined memory
  -- reallocation function of type 'PFN_vkReallocationFunction'.
  vkPfnReallocation :: PFN_vkReallocationFunction
  , -- | @pfnFree@ is a pointer to an application-defined memory free function of
  -- type 'PFN_vkFreeFunction'.
  vkPfnFree :: PFN_vkFreeFunction
  , -- | @pfnInternalAllocation@ is a pointer to an application-defined function
  -- that is called by the implementation when the implementation makes
  -- internal allocations, and it is of type
  -- 'PFN_vkInternalAllocationNotification'.
  vkPfnInternalAllocation :: PFN_vkInternalAllocationNotification
  , -- | @pfnInternalFree@ is a pointer to an application-defined function that
  -- is called by the implementation when the implementation frees internal
  -- allocations, and it is of type 'PFN_vkInternalFreeNotification'.
  vkPfnInternalFree :: PFN_vkInternalFreeNotification
  }
  deriving (Eq, Show)

instance Storable VkAllocationCallbacks where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkAllocationCallbacks <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
                                   <*> peek (ptr `plusPtr` 40)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPUserData (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 8) (vkPfnAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 16) (vkPfnReallocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 24) (vkPfnFree (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 32) (vkPfnInternalAllocation (poked :: VkAllocationCallbacks))
                *> poke (ptr `plusPtr` 40) (vkPfnInternalFree (poked :: VkAllocationCallbacks))

instance Zero VkAllocationCallbacks where
  zero = VkAllocationCallbacks zero
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
-- 'VkInstanceCreateInfo', 'Graphics.Vulkan.C.Core10.Core.VkStructureType'
data VkApplicationInfo = VkApplicationInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @pApplicationName@ is @NULL@ or is a pointer to a null-terminated UTF-8
  -- string containing the name of the application.
  vkPApplicationName :: Ptr CChar
  , -- | @applicationVersion@ is an unsigned integer variable containing the
  -- developer-supplied version number of the application.
  vkApplicationVersion :: Word32
  , -- | @pEngineName@ is @NULL@ or is a pointer to a null-terminated UTF-8
  -- string containing the name of the engine (if any) used to create the
  -- application.
  vkPEngineName :: Ptr CChar
  , -- | @engineVersion@ is an unsigned integer variable containing the
  -- developer-supplied version number of the engine used to create the
  -- application.
  vkEngineVersion :: Word32
  , -- | @apiVersion@ is the version of the Vulkan API against which the
  -- application expects to run, encoded as described in
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
  -- If @apiVersion@ is 0 the implementation /must/ ignore it, otherwise if
  -- the implementation does not support the requested @apiVersion@, or an
  -- effective substitute for @apiVersion@, it /must/ return
  -- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INCOMPATIBLE_DRIVER'. The patch
  -- version number specified in @apiVersion@ is ignored when creating an
  -- instance object. Only the major and minor versions of the instance
  -- /must/ match those requested in @apiVersion@.
  vkApiVersion :: Word32
  }
  deriving (Eq, Show)

instance Storable VkApplicationInfo where
  sizeOf ~_ = 48
  alignment ~_ = 8
  peek ptr = VkApplicationInfo <$> peek (ptr `plusPtr` 0)
                               <*> peek (ptr `plusPtr` 8)
                               <*> peek (ptr `plusPtr` 16)
                               <*> peek (ptr `plusPtr` 24)
                               <*> peek (ptr `plusPtr` 32)
                               <*> peek (ptr `plusPtr` 40)
                               <*> peek (ptr `plusPtr` 44)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 16) (vkPApplicationName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 24) (vkApplicationVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 32) (vkPEngineName (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 40) (vkEngineVersion (poked :: VkApplicationInfo))
                *> poke (ptr `plusPtr` 44) (vkApiVersion (poked :: VkApplicationInfo))

instance Zero VkApplicationInfo where
  zero = VkApplicationInfo VK_STRUCTURE_TYPE_APPLICATION_INFO
                           zero
                           zero
                           zero
                           zero
                           zero
                           zero

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

-- | VkExtent3D - Structure specifying a three-dimensional extent
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageCopy',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageResolve',
-- 'VkQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind'
data VkExtent3D = VkExtent3D
  { -- | @width@ is the width of the extent.
  vkWidth :: Word32
  , -- | @height@ is the height of the extent.
  vkHeight :: Word32
  , -- | @depth@ is the depth of the extent.
  vkDepth :: Word32
  }
  deriving (Eq, Show)

instance Storable VkExtent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkExtent3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkWidth (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 4) (vkHeight (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 8) (vkDepth (poked :: VkExtent3D))

instance Zero VkExtent3D where
  zero = VkExtent3D zero
                    zero
                    zero

-- ** VkFormatFeatureFlagBits

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
-- 'vkGetPhysicalDeviceFormatProperties'::@format@:
--
-- -   'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT' specifies that an image view
--     /can/ be
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
--
-- -   'VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT' specifies that an image view
--     /can/ be used as a
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage images>.
--
-- -   'VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT' specifies that an image
--     view /can/ be used as storage image that supports atomic operations.
--
-- -   'VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT' specifies that an image
--     view /can/ be used as a framebuffer color attachment and as an input
--     attachment.
--
-- -   'VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT' specifies that an
--     image view /can/ be used as a framebuffer color attachment that
--     supports blending and as an input attachment.
--
-- -   'VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that an
--     image view /can/ be used as a framebuffer depth\/stencil attachment
--     and as an input attachment.
--
-- -   'VK_FORMAT_FEATURE_BLIT_SRC_BIT' specifies that an image /can/ be
--     used as @srcImage@ for the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
--     command.
--
-- -   'VK_FORMAT_FEATURE_BLIT_DST_BIT' specifies that an image /can/ be
--     used as @dstImage@ for the
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage'
--     command.
--
-- -   'VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT' specifies that
--     if 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT' is also set, an image view
--     /can/ be used with a sampler that has either of @magFilter@ or
--     @minFilter@ set to
--     'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR', or @mipmapMode@
--     set to
--     'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_LINEAR'. If
--     'VK_FORMAT_FEATURE_BLIT_SRC_BIT' is also set, an image can be used
--     as the @srcImage@ to
--     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' with
--     a @filter@ of 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR'.
--     This bit /must/ only be exposed for formats that also support the
--     'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT' or
--     'VK_FORMAT_FEATURE_BLIT_SRC_BIT'.
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
-- created with the queried 'vkGetPhysicalDeviceProperties'::@format@:
--
-- -   'VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT' specifies that the
--     format /can/ be used to create a buffer view that /can/ be bound to
--     a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     descriptor.
--
-- -   'VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT' specifies that the
--     format /can/ be used to create a buffer view that /can/ be bound to
--     a
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor.
--
-- -   'VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT' specifies that
--     atomic operations are supported on
--     'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     with this format.
--
-- -   'VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT' specifies that the format
--     /can/ be used as a vertex attribute format
--     ('Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'::@format@).
--
-- = See Also
--
-- 'VkFormatFeatureFlags'
newtype VkFormatFeatureFlagBits = VkFormatFeatureFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkFormatFeatureFlagBits where
  showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = showString "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = showString "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = showString "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = showString "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
  showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = showString "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
  showsPrec _ VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_FORMAT_FEATURE_BLIT_SRC_BIT = showString "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
  showsPrec _ VK_FORMAT_FEATURE_BLIT_DST_BIT = showString "VK_FORMAT_FEATURE_BLIT_DST_BIT"
  showsPrec _ VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkFormatFeatureFlagBits 0x00004000) = showString "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00008000) = showString "VK_FORMAT_FEATURE_TRANSFER_DST_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00020000) = showString "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00040000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00080000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00100000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00200000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00400000) = showString "VK_FORMAT_FEATURE_DISJOINT_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00800000) = showString "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
  showsPrec _ (VkFormatFeatureFlagBits 0x00002000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG"
  showsPrec _ (VkFormatFeatureFlagBits 0x08000000) = showString "VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x10000000) = showString "VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x02000000) = showString "VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x04000000) = showString "VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR"
  showsPrec _ (VkFormatFeatureFlagBits 0x00010000) = showString "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT"
  showsPrec _ (VkFormatFeatureFlagBits 0x01000000) = showString "VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
  showsPrec p (VkFormatFeatureFlagBits x) = showParen (p >= 11) (showString "VkFormatFeatureFlagBits " . showsPrec 11 x)

instance Read VkFormatFeatureFlagBits where
  readPrec = parens ( choose [ ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT",               pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT",               pure VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT",        pure VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT)
                             , ("VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT",        pure VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT",        pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT", pure VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT)
                             , ("VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT",               pure VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT)
                             , ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT",            pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT)
                             , ("VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT",      pure VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT)
                             , ("VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT",    pure VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT)
                             , ("VK_FORMAT_FEATURE_BLIT_SRC_BIT",                    pure VK_FORMAT_FEATURE_BLIT_SRC_BIT)
                             , ("VK_FORMAT_FEATURE_BLIT_DST_BIT",                    pure VK_FORMAT_FEATURE_BLIT_DST_BIT)
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT", pure VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_FORMAT_FEATURE_TRANSFER_SRC_BIT",                                                            pure (VkFormatFeatureFlagBits 0x00004000))
                             , ("VK_FORMAT_FEATURE_TRANSFER_DST_BIT",                                                            pure (VkFormatFeatureFlagBits 0x00008000))
                             , ("VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT",                                                 pure (VkFormatFeatureFlagBits 0x00020000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT",                            pure (VkFormatFeatureFlagBits 0x00040000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT",           pure (VkFormatFeatureFlagBits 0x00080000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT",           pure (VkFormatFeatureFlagBits 0x00100000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT", pure (VkFormatFeatureFlagBits 0x00200000))
                             , ("VK_FORMAT_FEATURE_DISJOINT_BIT",                                                                pure (VkFormatFeatureFlagBits 0x00400000))
                             , ("VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT",                                                  pure (VkFormatFeatureFlagBits 0x00800000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG",                                          pure (VkFormatFeatureFlagBits 0x00002000))
                             , ("VK_FORMAT_FEATURE_RESERVED_27_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x08000000))
                             , ("VK_FORMAT_FEATURE_RESERVED_28_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x10000000))
                             , ("VK_FORMAT_FEATURE_RESERVED_25_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x02000000))
                             , ("VK_FORMAT_FEATURE_RESERVED_26_BIT_KHR",                                                         pure (VkFormatFeatureFlagBits 0x04000000))
                             , ("VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT_EXT",                                         pure (VkFormatFeatureFlagBits 0x00010000))
                             , ("VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT",                                                pure (VkFormatFeatureFlagBits 0x01000000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFormatFeatureFlagBits")
                        v <- step readPrec
                        pure (VkFormatFeatureFlagBits v)
                        )
                    )

-- | 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT' specifies that an image view /can/
-- be
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT = VkFormatFeatureFlagBits 0x00000001

-- | 'VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT' specifies that an image view /can/
-- be used as a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage storage images>.
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT = VkFormatFeatureFlagBits 0x00000002

-- | 'VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT' specifies that an image
-- view /can/ be used as storage image that supports atomic operations.
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT = VkFormatFeatureFlagBits 0x00000004

-- | 'VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT' specifies that the format
-- /can/ be used to create a buffer view that /can/ be bound to a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
-- descriptor.
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000008

-- | 'VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT' specifies that the format
-- /can/ be used to create a buffer view that /can/ be bound to a
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
-- descriptor.
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000010

-- | 'VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT' specifies that
-- atomic operations are supported on
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
-- with this format.
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = VkFormatFeatureFlagBits 0x00000020

-- | 'VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT' specifies that the format /can/ be
-- used as a vertex attribute format
-- ('Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'::@format@).
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT = VkFormatFeatureFlagBits 0x00000040

-- | 'VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT' specifies that an image view
-- /can/ be used as a framebuffer color attachment and as an input
-- attachment.
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x00000080

-- | 'VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT' specifies that an image
-- view /can/ be used as a framebuffer color attachment that supports
-- blending and as an input attachment.
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT = VkFormatFeatureFlagBits 0x00000100

-- | 'VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that an image
-- view /can/ be used as a framebuffer depth\/stencil attachment and as an
-- input attachment.
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT = VkFormatFeatureFlagBits 0x00000200

-- | 'VK_FORMAT_FEATURE_BLIT_SRC_BIT' specifies that an image /can/ be used
-- as @srcImage@ for the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' command.
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_BLIT_SRC_BIT = VkFormatFeatureFlagBits 0x00000400

-- | 'VK_FORMAT_FEATURE_BLIT_DST_BIT' specifies that an image /can/ be used
-- as @dstImage@ for the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' command.
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_BLIT_DST_BIT = VkFormatFeatureFlagBits 0x00000800

-- | 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT' specifies that if
-- 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT' is also set, an image view /can/
-- be used with a sampler that has either of @magFilter@ or @minFilter@ set
-- to 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR', or @mipmapMode@
-- set to 'Graphics.Vulkan.C.Core10.Sampler.VK_SAMPLER_MIPMAP_MODE_LINEAR'.
-- If 'VK_FORMAT_FEATURE_BLIT_SRC_BIT' is also set, an image can be used as
-- the @srcImage@ to
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBlitImage' with a
-- @filter@ of 'Graphics.Vulkan.C.Core10.Sampler.VK_FILTER_LINEAR'. This
-- bit /must/ only be exposed for formats that also support the
-- 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT' or
-- 'VK_FORMAT_FEATURE_BLIT_SRC_BIT'.
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
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT = VkFormatFeatureFlagBits 0x00001000

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
-- 'VkFormatFeatureFlags',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2',
-- 'vkGetPhysicalDeviceFormatProperties'
data VkFormatProperties = VkFormatProperties
  { -- | @linearTilingFeatures@ is a bitmask of 'VkFormatFeatureFlagBits'
  -- specifying features supported by images created with a @tiling@
  -- parameter of 'VK_IMAGE_TILING_LINEAR'.
  vkLinearTilingFeatures :: VkFormatFeatureFlags
  , -- | @optimalTilingFeatures@ is a bitmask of 'VkFormatFeatureFlagBits'
  -- specifying features supported by images created with a @tiling@
  -- parameter of 'VK_IMAGE_TILING_OPTIMAL'.
  vkOptimalTilingFeatures :: VkFormatFeatureFlags
  , -- | @bufferFeatures@ is a bitmask of 'VkFormatFeatureFlagBits' specifying
  -- features supported by buffers.
  vkBufferFeatures :: VkFormatFeatureFlags
  }
  deriving (Eq, Show)

instance Storable VkFormatProperties where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkFormatProperties <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 4)
                                <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkLinearTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 4) (vkOptimalTilingFeatures (poked :: VkFormatProperties))
                *> poke (ptr `plusPtr` 8) (vkBufferFeatures (poked :: VkFormatProperties))

instance Zero VkFormatProperties where
  zero = VkFormatProperties zero
                            zero
                            zero

-- ** VkImageCreateFlagBits

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
-- 'VkImageCreateFlags'
newtype VkImageCreateFlagBits = VkImageCreateFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkImageCreateFlagBits where
  showsPrec _ VK_IMAGE_CREATE_SPARSE_BINDING_BIT = showString "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = showString "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
  showsPrec _ VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = showString "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
  showsPrec _ VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = showString "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
  showsPrec _ VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = showString "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageCreateFlagBits 0x00000400) = showString "VK_IMAGE_CREATE_ALIAS_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000040) = showString "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000020) = showString "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000080) = showString "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000100) = showString "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000800) = showString "VK_IMAGE_CREATE_PROTECTED_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00000200) = showString "VK_IMAGE_CREATE_DISJOINT_BIT"
  showsPrec _ (VkImageCreateFlagBits 0x00002000) = showString "VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
  showsPrec _ (VkImageCreateFlagBits 0x00001000) = showString "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
  showsPrec _ (VkImageCreateFlagBits 0x00004000) = showString "VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
  showsPrec p (VkImageCreateFlagBits x) = showParen (p >= 11) (showString "VkImageCreateFlagBits " . showsPrec 11 x)

instance Read VkImageCreateFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_CREATE_SPARSE_BINDING_BIT",   pure VK_IMAGE_CREATE_SPARSE_BINDING_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT", pure VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT)
                             , ("VK_IMAGE_CREATE_SPARSE_ALIASED_BIT",   pure VK_IMAGE_CREATE_SPARSE_ALIASED_BIT)
                             , ("VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT",   pure VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT)
                             , ("VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT",  pure VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_CREATE_ALIAS_BIT",                                 pure (VkImageCreateFlagBits 0x00000400))
                             , ("VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT",           pure (VkImageCreateFlagBits 0x00000040))
                             , ("VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT",                   pure (VkImageCreateFlagBits 0x00000020))
                             , ("VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT",           pure (VkImageCreateFlagBits 0x00000080))
                             , ("VK_IMAGE_CREATE_EXTENDED_USAGE_BIT",                        pure (VkImageCreateFlagBits 0x00000100))
                             , ("VK_IMAGE_CREATE_PROTECTED_BIT",                             pure (VkImageCreateFlagBits 0x00000800))
                             , ("VK_IMAGE_CREATE_DISJOINT_BIT",                              pure (VkImageCreateFlagBits 0x00000200))
                             , ("VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV",                     pure (VkImageCreateFlagBits 0x00002000))
                             , ("VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT", pure (VkImageCreateFlagBits 0x00001000))
                             , ("VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT",                        pure (VkImageCreateFlagBits 0x00004000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageCreateFlagBits")
                        v <- step readPrec
                        pure (VkImageCreateFlagBits v)
                        )
                    )

-- | 'VK_IMAGE_CREATE_SPARSE_BINDING_BIT' specifies that the image will be
-- backed using sparse memory binding.
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_BINDING_BIT = VkImageCreateFlagBits 0x00000001

-- | 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' specifies that the image /can/ be
-- partially backed using sparse memory binding. Images created with this
-- flag /must/ also be created with the
-- 'VK_IMAGE_CREATE_SPARSE_BINDING_BIT' flag.
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT = VkImageCreateFlagBits 0x00000002

-- | 'VK_IMAGE_CREATE_SPARSE_ALIASED_BIT' specifies that the image will be
-- backed using sparse memory binding with memory ranges that might also
-- simultaneously be backing another image (or another portion of the same
-- image). Images created with this flag /must/ also be created with the
-- 'VK_IMAGE_CREATE_SPARSE_BINDING_BIT' flag
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_SPARSE_ALIASED_BIT = VkImageCreateFlagBits 0x00000004

-- | 'VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT' specifies that the image /can/ be
-- used to create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' with a
-- different format from the image.
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT = VkImageCreateFlagBits 0x00000008

-- | 'VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT' specifies that the image /can/ be
-- used to create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' of
-- type 'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE' or
-- 'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY'.
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT = VkImageCreateFlagBits 0x00000010

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
-- 'VK_IMAGE_TYPE_1D'.
--
-- If the combination of parameters to
-- 'vkGetPhysicalDeviceImageFormatProperties' is not supported by the
-- implementation for use in
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage', then all members of
-- 'VkImageFormatProperties' will be filled with zero.
--
-- __Note__
--
-- Filling 'VkImageFormatProperties' with zero for unsupported formats is
-- an exception to the usual rule that output structures have undefined
-- contents on error. This exception was unintentional, but is preserved
-- for backwards compatibility.
--
-- = See Also
--
-- 'VkDeviceSize', 'VkExtent3D',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.VkExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2',
-- 'VkSampleCountFlags', 'vkGetPhysicalDeviceImageFormatProperties'
data VkImageFormatProperties = VkImageFormatProperties
  { -- | @maxExtent@ are the maximum image dimensions. See the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-extentperimagetype Allowed Extent Values>
  -- section below for how these values are constrained by @type@.
  vkMaxExtent :: VkExtent3D
  , -- | @maxMipLevels@ is the maximum number of mipmap levels. @maxMipLevels@
  -- /must/ be equal to the number of levels in the complete mipmap chain
  -- based on the @maxExtent.width@, @maxExtent.height@, and
  -- @maxExtent.depth@, except when one of the following conditions is true,
  -- in which case it /may/ instead be @1@:
  --
  -- -   'vkGetPhysicalDeviceImageFormatProperties'::@tiling@ was
  --     'VK_IMAGE_TILING_LINEAR'
  --
  vkMaxMipLevels :: Word32
  , -- | @maxArrayLayers@ is the maximum number of array layers. @maxArrayLayers@
  -- /must/ be no less than 'VkPhysicalDeviceLimits'::@maxImageArrayLayers@,
  -- except when one of the following conditions is true, in which case it
  -- /may/ instead be @1@:
  --
  -- -   @tiling@ is 'VK_IMAGE_TILING_LINEAR'
  --
  -- -   @tiling@ is 'VK_IMAGE_TILING_OPTIMAL' and @type@ is
  --     'VK_IMAGE_TYPE_3D'
  --
  vkMaxArrayLayers :: Word32
  , -- | @sampleCounts@ is a bitmask of 'VkSampleCountFlagBits' specifying all
  -- the supported sample counts for this image as described
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-supported-sample-counts below>.
  vkSampleCounts :: VkSampleCountFlags
  , -- | @maxResourceSize@ is an upper bound on the total image size in bytes,
  -- inclusive of all image subresources. Implementations /may/ have an
  -- address space limit on total size of a resource, which is advertised by
  -- this property. @maxResourceSize@ /must/ be at least 231.
  vkMaxResourceSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkImageFormatProperties where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkImageFormatProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 12)
                                     <*> peek (ptr `plusPtr` 16)
                                     <*> peek (ptr `plusPtr` 20)
                                     <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxExtent (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 12) (vkMaxMipLevels (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 16) (vkMaxArrayLayers (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 20) (vkSampleCounts (poked :: VkImageFormatProperties))
                *> poke (ptr `plusPtr` 24) (vkMaxResourceSize (poked :: VkImageFormatProperties))

instance Zero VkImageFormatProperties where
  zero = VkImageFormatProperties zero
                                 zero
                                 zero
                                 zero
                                 zero

-- ** VkImageTiling

-- | VkImageTiling - Specifies the tiling arrangement of data in an image
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkImageTiling = VkImageTiling Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkImageTiling where
  showsPrec _ VK_IMAGE_TILING_OPTIMAL = showString "VK_IMAGE_TILING_OPTIMAL"
  showsPrec _ VK_IMAGE_TILING_LINEAR = showString "VK_IMAGE_TILING_LINEAR"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageTiling 1000158000) = showString "VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT"
  showsPrec p (VkImageTiling x) = showParen (p >= 11) (showString "VkImageTiling " . showsPrec 11 x)

instance Read VkImageTiling where
  readPrec = parens ( choose [ ("VK_IMAGE_TILING_OPTIMAL", pure VK_IMAGE_TILING_OPTIMAL)
                             , ("VK_IMAGE_TILING_LINEAR",  pure VK_IMAGE_TILING_LINEAR)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT", pure (VkImageTiling 1000158000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageTiling")
                        v <- step readPrec
                        pure (VkImageTiling v)
                        )
                    )

-- | 'VK_IMAGE_TILING_OPTIMAL' specifies optimal tiling (texels are laid out
-- in an implementation-dependent arrangement, for more optimal memory
-- access).
pattern VK_IMAGE_TILING_OPTIMAL :: VkImageTiling
pattern VK_IMAGE_TILING_OPTIMAL = VkImageTiling 0

-- | 'VK_IMAGE_TILING_LINEAR' specifies linear tiling (texels are laid out in
-- memory in row-major order, possibly with some padding on each row).
pattern VK_IMAGE_TILING_LINEAR :: VkImageTiling
pattern VK_IMAGE_TILING_LINEAR = VkImageTiling 1

-- ** VkImageType

-- | VkImageType - Specifies the type of an image object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkImageType = VkImageType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkImageType where
  showsPrec _ VK_IMAGE_TYPE_1D = showString "VK_IMAGE_TYPE_1D"
  showsPrec _ VK_IMAGE_TYPE_2D = showString "VK_IMAGE_TYPE_2D"
  showsPrec _ VK_IMAGE_TYPE_3D = showString "VK_IMAGE_TYPE_3D"
  showsPrec p (VkImageType x) = showParen (p >= 11) (showString "VkImageType " . showsPrec 11 x)

instance Read VkImageType where
  readPrec = parens ( choose [ ("VK_IMAGE_TYPE_1D", pure VK_IMAGE_TYPE_1D)
                             , ("VK_IMAGE_TYPE_2D", pure VK_IMAGE_TYPE_2D)
                             , ("VK_IMAGE_TYPE_3D", pure VK_IMAGE_TYPE_3D)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageType")
                        v <- step readPrec
                        pure (VkImageType v)
                        )
                    )

-- | 'VK_IMAGE_TYPE_1D' specifies a one-dimensional image.
pattern VK_IMAGE_TYPE_1D :: VkImageType
pattern VK_IMAGE_TYPE_1D = VkImageType 0

-- | 'VK_IMAGE_TYPE_2D' specifies a two-dimensional image.
pattern VK_IMAGE_TYPE_2D :: VkImageType
pattern VK_IMAGE_TYPE_2D = VkImageType 1

-- | 'VK_IMAGE_TYPE_3D' specifies a three-dimensional image.
pattern VK_IMAGE_TYPE_3D :: VkImageType
pattern VK_IMAGE_TYPE_3D = VkImageType 2

-- ** VkImageUsageFlagBits

-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = See Also
--
-- 'VkImageUsageFlags'
newtype VkImageUsageFlagBits = VkImageUsageFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkImageUsageFlagBits where
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_SRC_BIT = showString "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSFER_DST_BIT = showString "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
  showsPrec _ VK_IMAGE_USAGE_SAMPLED_BIT = showString "VK_IMAGE_USAGE_SAMPLED_BIT"
  showsPrec _ VK_IMAGE_USAGE_STORAGE_BIT = showString "VK_IMAGE_USAGE_STORAGE_BIT"
  showsPrec _ VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
  showsPrec _ VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = showString "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkImageUsageFlagBits 0x00002000) = showString "VK_IMAGE_USAGE_RESERVED_13_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00004000) = showString "VK_IMAGE_USAGE_RESERVED_14_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00008000) = showString "VK_IMAGE_USAGE_RESERVED_15_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00000400) = showString "VK_IMAGE_USAGE_RESERVED_10_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00000800) = showString "VK_IMAGE_USAGE_RESERVED_11_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00001000) = showString "VK_IMAGE_USAGE_RESERVED_12_BIT_KHR"
  showsPrec _ (VkImageUsageFlagBits 0x00000100) = showString "VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
  showsPrec _ (VkImageUsageFlagBits 0x00000200) = showString "VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
  showsPrec p (VkImageUsageFlagBits x) = showParen (p >= 11) (showString "VkImageUsageFlagBits " . showsPrec 11 x)

instance Read VkImageUsageFlagBits where
  readPrec = parens ( choose [ ("VK_IMAGE_USAGE_TRANSFER_SRC_BIT",             pure VK_IMAGE_USAGE_TRANSFER_SRC_BIT)
                             , ("VK_IMAGE_USAGE_TRANSFER_DST_BIT",             pure VK_IMAGE_USAGE_TRANSFER_DST_BIT)
                             , ("VK_IMAGE_USAGE_SAMPLED_BIT",                  pure VK_IMAGE_USAGE_SAMPLED_BIT)
                             , ("VK_IMAGE_USAGE_STORAGE_BIT",                  pure VK_IMAGE_USAGE_STORAGE_BIT)
                             , ("VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT",         pure VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT", pure VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT",     pure VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT)
                             , ("VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT",         pure VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_IMAGE_USAGE_RESERVED_13_BIT_KHR",          pure (VkImageUsageFlagBits 0x00002000))
                             , ("VK_IMAGE_USAGE_RESERVED_14_BIT_KHR",          pure (VkImageUsageFlagBits 0x00004000))
                             , ("VK_IMAGE_USAGE_RESERVED_15_BIT_KHR",          pure (VkImageUsageFlagBits 0x00008000))
                             , ("VK_IMAGE_USAGE_RESERVED_10_BIT_KHR",          pure (VkImageUsageFlagBits 0x00000400))
                             , ("VK_IMAGE_USAGE_RESERVED_11_BIT_KHR",          pure (VkImageUsageFlagBits 0x00000800))
                             , ("VK_IMAGE_USAGE_RESERVED_12_BIT_KHR",          pure (VkImageUsageFlagBits 0x00001000))
                             , ("VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV",    pure (VkImageUsageFlagBits 0x00000100))
                             , ("VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT", pure (VkImageUsageFlagBits 0x00000200))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkImageUsageFlagBits")
                        v <- step readPrec
                        pure (VkImageUsageFlagBits v)
                        )
                    )

-- | 'VK_IMAGE_USAGE_TRANSFER_SRC_BIT' specifies that the image /can/ be used
-- as the source of a transfer command.
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_SRC_BIT = VkImageUsageFlagBits 0x00000001

-- | 'VK_IMAGE_USAGE_TRANSFER_DST_BIT' specifies that the image /can/ be used
-- as the destination of a transfer command.
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSFER_DST_BIT = VkImageUsageFlagBits 0x00000002

-- | 'VK_IMAGE_USAGE_SAMPLED_BIT' specifies that the image /can/ be used to
-- create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for
-- occupying a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- slot either of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
-- or
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- and be sampled by a shader.
pattern VK_IMAGE_USAGE_SAMPLED_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_SAMPLED_BIT = VkImageUsageFlagBits 0x00000004

-- | 'VK_IMAGE_USAGE_STORAGE_BIT' specifies that the image /can/ be used to
-- create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for
-- occupying a 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet'
-- slot of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'.
pattern VK_IMAGE_USAGE_STORAGE_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_STORAGE_BIT = VkImageUsageFlagBits 0x00000008

-- | 'VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT' specifies that the image /can/ be
-- used to create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'
-- suitable for use as a color or resolve attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'.
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000010

-- | 'VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that the image
-- /can/ be used to create a
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable for use as a
-- depth\/stencil attachment in a
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebuffer'.
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000020

-- | 'VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT' specifies that the memory
-- bound to this image will have been allocated with the
-- 'VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT' (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory>
-- for more detail). This bit /can/ be set for any image that /can/ be used
-- to create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView' suitable
-- for use as a color, resolve, depth\/stencil, or input attachment.
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000040

-- | 'VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT' specifies that the image /can/ be
-- used to create a 'Graphics.Vulkan.C.Core10.ImageView.VkImageView'
-- suitable for occupying
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSet' slot of type
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT';
-- be read from a shader as an input attachment; and be used as an input
-- attachment in a framebuffer.
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT :: VkImageUsageFlagBits
pattern VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT = VkImageUsageFlagBits 0x00000080

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

-- ** VkInstanceCreateFlags

-- | VkInstanceCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkInstanceCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkInstanceCreateInfo'
newtype VkInstanceCreateFlags = VkInstanceCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkInstanceCreateFlags where
  
  showsPrec p (VkInstanceCreateFlags x) = showParen (p >= 11) (showString "VkInstanceCreateFlags " . showsPrec 11 x)

instance Read VkInstanceCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkInstanceCreateFlags")
                        v <- step readPrec
                        pure (VkInstanceCreateFlags v)
                        )
                    )



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
--     valid pointer to a valid 'VkApplicationInfo' structure
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
-- 'VkApplicationInfo', 'VkInstanceCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType', 'vkCreateInstance'
data VkInstanceCreateInfo = VkInstanceCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkInstanceCreateFlags
  , -- | @pApplicationInfo@ is @NULL@ or a pointer to an instance of
  -- 'VkApplicationInfo'. If not @NULL@, this information helps
  -- implementations recognize behavior inherent to classes of applications.
  -- 'VkApplicationInfo' is defined in detail below.
  vkPApplicationInfo :: Ptr VkApplicationInfo
  , -- | @enabledLayerCount@ is the number of global layers to enable.
  vkEnabledLayerCount :: Word32
  , -- | @ppEnabledLayerNames@ is a pointer to an array of @enabledLayerCount@
  -- null-terminated UTF-8 strings containing the names of layers to enable
  -- for the created instance. See the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-layers>
  -- section for further details.
  vkPPEnabledLayerNames :: Ptr (Ptr CChar)
  , -- | @enabledExtensionCount@ is the number of global extensions to enable.
  vkEnabledExtensionCount :: Word32
  , -- | @ppEnabledExtensionNames@ is a pointer to an array of
  -- @enabledExtensionCount@ null-terminated UTF-8 strings containing the
  -- names of extensions to enable.
  vkPPEnabledExtensionNames :: Ptr (Ptr CChar)
  }
  deriving (Eq, Show)

instance Storable VkInstanceCreateInfo where
  sizeOf ~_ = 64
  alignment ~_ = 8
  peek ptr = VkInstanceCreateInfo <$> peek (ptr `plusPtr` 0)
                                  <*> peek (ptr `plusPtr` 8)
                                  <*> peek (ptr `plusPtr` 16)
                                  <*> peek (ptr `plusPtr` 24)
                                  <*> peek (ptr `plusPtr` 32)
                                  <*> peek (ptr `plusPtr` 40)
                                  <*> peek (ptr `plusPtr` 48)
                                  <*> peek (ptr `plusPtr` 56)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPApplicationInfo (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 32) (vkEnabledLayerCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 40) (vkPPEnabledLayerNames (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 48) (vkEnabledExtensionCount (poked :: VkInstanceCreateInfo))
                *> poke (ptr `plusPtr` 56) (vkPPEnabledExtensionNames (poked :: VkInstanceCreateInfo))

instance Zero VkInstanceCreateInfo where
  zero = VkInstanceCreateInfo VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero
                              zero

-- ** VkInternalAllocationType

-- | VkInternalAllocationType - Allocation type
--
-- = See Also
--
-- 'PFN_vkInternalAllocationNotification', 'PFN_vkInternalFreeNotification'
newtype VkInternalAllocationType = VkInternalAllocationType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkInternalAllocationType where
  showsPrec _ VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = showString "VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE"
  showsPrec p (VkInternalAllocationType x) = showParen (p >= 11) (showString "VkInternalAllocationType " . showsPrec 11 x)

instance Read VkInternalAllocationType where
  readPrec = parens ( choose [ ("VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE", pure VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkInternalAllocationType")
                        v <- step readPrec
                        pure (VkInternalAllocationType v)
                        )
                    )

-- | 'VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE' specifies that the allocation
-- is intended for execution by the host.
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE :: VkInternalAllocationType
pattern VK_INTERNAL_ALLOCATION_TYPE_EXECUTABLE = VkInternalAllocationType 0

-- | VkMemoryHeap - Structure specifying a memory heap
--
-- = See Also
--
-- 'VkDeviceSize', 'VkMemoryHeapFlags', 'VkPhysicalDeviceMemoryProperties'
data VkMemoryHeap = VkMemoryHeap
  { -- | @size@ is the total memory size in bytes in the heap.
  vkSize :: VkDeviceSize
  , -- | @flags@ is a bitmask of 'VkMemoryHeapFlagBits' specifying attribute
  -- flags for the heap.
  vkFlags :: VkMemoryHeapFlags
  }
  deriving (Eq, Show)

instance Storable VkMemoryHeap where
  sizeOf ~_ = 16
  alignment ~_ = 8
  peek ptr = VkMemoryHeap <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSize (poked :: VkMemoryHeap))
                *> poke (ptr `plusPtr` 8) (vkFlags (poked :: VkMemoryHeap))

instance Zero VkMemoryHeap where
  zero = VkMemoryHeap zero
                      zero

-- ** VkMemoryHeapFlagBits

-- | VkMemoryHeapFlagBits - Bitmask specifying attribute flags for a heap
--
-- = See Also
--
-- 'VkMemoryHeapFlags'
newtype VkMemoryHeapFlagBits = VkMemoryHeapFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkMemoryHeapFlagBits where
  showsPrec _ VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = showString "VK_MEMORY_HEAP_DEVICE_LOCAL_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkMemoryHeapFlagBits 0x00000002) = showString "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT"
  showsPrec p (VkMemoryHeapFlagBits x) = showParen (p >= 11) (showString "VkMemoryHeapFlagBits " . showsPrec 11 x)

instance Read VkMemoryHeapFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_HEAP_DEVICE_LOCAL_BIT", pure VK_MEMORY_HEAP_DEVICE_LOCAL_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_MEMORY_HEAP_MULTI_INSTANCE_BIT", pure (VkMemoryHeapFlagBits 0x00000002))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryHeapFlagBits")
                        v <- step readPrec
                        pure (VkMemoryHeapFlagBits v)
                        )
                    )

-- | 'VK_MEMORY_HEAP_DEVICE_LOCAL_BIT' specifies that the heap corresponds to
-- device local memory. Device local memory /may/ have different
-- performance characteristics than host local memory, and /may/ support
-- different memory property flags.
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_DEVICE_LOCAL_BIT = VkMemoryHeapFlagBits 0x00000001

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

-- ** VkMemoryPropertyFlagBits

-- | VkMemoryPropertyFlagBits - Bitmask specifying properties for a memory
-- type
--
-- = See Also
--
-- 'VkMemoryPropertyFlags'
newtype VkMemoryPropertyFlagBits = VkMemoryPropertyFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkMemoryPropertyFlagBits where
  showsPrec _ VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = showString "VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = showString "VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = showString "VK_MEMORY_PROPERTY_HOST_COHERENT_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_HOST_CACHED_BIT = showString "VK_MEMORY_PROPERTY_HOST_CACHED_BIT"
  showsPrec _ VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = showString "VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkMemoryPropertyFlagBits 0x00000020) = showString "VK_MEMORY_PROPERTY_PROTECTED_BIT"
  showsPrec p (VkMemoryPropertyFlagBits x) = showParen (p >= 11) (showString "VkMemoryPropertyFlagBits " . showsPrec 11 x)

instance Read VkMemoryPropertyFlagBits where
  readPrec = parens ( choose [ ("VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT",     pure VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT",     pure VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_COHERENT_BIT",    pure VK_MEMORY_PROPERTY_HOST_COHERENT_BIT)
                             , ("VK_MEMORY_PROPERTY_HOST_CACHED_BIT",      pure VK_MEMORY_PROPERTY_HOST_CACHED_BIT)
                             , ("VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT", pure VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_MEMORY_PROPERTY_PROTECTED_BIT", pure (VkMemoryPropertyFlagBits 0x00000020))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkMemoryPropertyFlagBits")
                        v <- step readPrec
                        pure (VkMemoryPropertyFlagBits v)
                        )
                    )

-- | 'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' bit specifies that memory
-- allocated with this type is the most efficient for device access. This
-- property will be set if and only if the memory type belongs to a heap
-- with the 'VK_MEMORY_HEAP_DEVICE_LOCAL_BIT' set.
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT = VkMemoryPropertyFlagBits 0x00000001

-- | 'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' bit specifies that memory
-- allocated with this type /can/ be mapped for host access using
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory'.
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT = VkMemoryPropertyFlagBits 0x00000002

-- | 'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT' bit specifies that the host cache
-- management commands
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges' and
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges' are not
-- needed to flush host writes to the device or make device writes visible
-- to the host, respectively.
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_COHERENT_BIT = VkMemoryPropertyFlagBits 0x00000004

-- | 'VK_MEMORY_PROPERTY_HOST_CACHED_BIT' bit specifies that memory allocated
-- with this type is cached on the host. Host memory accesses to uncached
-- memory are slower than to cached memory, however uncached memory is
-- always host coherent.
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_HOST_CACHED_BIT = VkMemoryPropertyFlagBits 0x00000008

-- | 'VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT' bit specifies that the memory
-- type only allows device access to the memory. Memory types /must/ not
-- have both 'VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT' and
-- 'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' set. Additionally, the object’s
-- backing memory /may/ be provided by the implementation lazily as
-- specified in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-lazy_allocation Lazily Allocated Memory>.
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT = VkMemoryPropertyFlagBits 0x00000010

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

-- | VkMemoryType - Structure specifying memory type
--
-- = See Also
--
-- 'VkMemoryPropertyFlags', 'VkPhysicalDeviceMemoryProperties'
data VkMemoryType = VkMemoryType
  { -- | @propertyFlags@ is a bitmask of 'VkMemoryPropertyFlagBits' of properties
  -- for this memory type.
  vkPropertyFlags :: VkMemoryPropertyFlags
  , -- | @heapIndex@ describes which memory heap this memory type corresponds to,
  -- and /must/ be less than @memoryHeapCount@ from the
  -- 'VkPhysicalDeviceMemoryProperties' structure.
  vkHeapIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkMemoryType where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkMemoryType <$> peek (ptr `plusPtr` 0)
                          <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkPropertyFlags (poked :: VkMemoryType))
                *> poke (ptr `plusPtr` 4) (vkHeapIndex (poked :: VkMemoryType))

instance Zero VkMemoryType where
  zero = VkMemoryType zero
                      zero

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

-- | VkPhysicalDeviceFeatures - Structure describing the fine-grained
-- features that can be supported by an implementation
--
-- = Members
--
-- The members of the 'VkPhysicalDeviceFeatures' structure describe the
-- following features:
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32',
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- 'vkGetPhysicalDeviceFeatures'
data VkPhysicalDeviceFeatures = VkPhysicalDeviceFeatures
  { -- | @robustBufferAccess@ specifies that accesses to buffers are
  -- bounds-checked against the range of the buffer descriptor (as determined
  -- by
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo'::@range@,
  -- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo'::@range@,
  -- or the size of the buffer). Out of bounds accesses /must/ not cause
  -- application termination, and the effects of shader loads, stores, and
  -- atomics /must/ conform to an implementation-dependent behavior as
  -- described below.
  --
  -- -   A buffer access is considered to be out of bounds if any of the
  --     following are true:
  --
  --     -   The pointer was formed by @OpImageTexelPointer@ and the
  --         coordinate is less than zero or greater than or equal to the
  --         number of whole elements in the bound range.
  --
  --     -   The pointer was not formed by @OpImageTexelPointer@ and the
  --         object pointed to is not wholly contained within the bound
  --         range.
  --
  --         __Note__
  --
  --         If a SPIR-V @OpLoad@ instruction loads a structure and the tail
  --         end of the structure is out of bounds, then all members of the
  --         structure are considered out of bounds even if the members at
  --         the end are not statically used.
  --
  --     -   If any buffer access in a given SPIR-V block is determined to be
  --         out of bounds, then any other access of the same type (load,
  --         store, or atomic) in the same SPIR-V block that accesses an
  --         address less than 16 bytes away from the out of bounds address
  --         /may/ also be considered out of bounds.
  --
  -- -   Out-of-bounds buffer loads will return any of the following values:
  --
  --     -   Values from anywhere within the memory range(s) bound to the
  --         buffer (possibly including bytes of memory past the end of the
  --         buffer, up to the end of the bound range).
  --
  --     -   Zero values, or (0,0,0,x) vectors for vector reads where x is a
  --         valid value represented in the type of the vector components and
  --         /may/ be any of:
  --
  --         -   0, 1, or the maximum representable positive integer value,
  --             for signed or unsigned integer components
  --
  --         -   0.0 or 1.0, for floating-point components
  --
  -- -   Out-of-bounds writes /may/ modify values within the memory range(s)
  --     bound to the buffer, but /must/ not modify any other memory.
  --
  -- -   Out-of-bounds atomics /may/ modify values within the memory range(s)
  --     bound to the buffer, but /must/ not modify any other memory, and
  --     return an undefined value.
  --
  -- -   Vertex input attributes are considered out of bounds if the offset
  --     of the attribute in the bound vertex buffer range plus the size of
  --     the attribute is greater than either:
  --
  --     -   @vertexBufferRangeSize@, if @bindingStride@ == 0; or
  --
  --     -   (@vertexBufferRangeSize@ - (@vertexBufferRangeSize@ %
  --         @bindingStride@))
  --
  --     where @vertexBufferRangeSize@ is the byte size of the memory range
  --     bound to the vertex buffer binding and @bindingStride@ is the byte
  --     stride of the corresponding vertex input binding. Further, if any
  --     vertex input attribute using a specific vertex input binding is out
  --     of bounds, then all vertex input attributes using that vertex input
  --     binding for that vertex shader invocation are considered out of
  --     bounds.
  --
  --     -   If a vertex input attribute is out of bounds, it will be
  --         assigned one of the following values:
  --
  --         -   Values from anywhere within the memory range(s) bound to the
  --             buffer, converted according to the format of the attribute.
  --
  --         -   Zero values, format converted according to the format of the
  --             attribute.
  --
  --         -   Zero values, or (0,0,0,x) vectors, as described above.
  --
  -- -   If @robustBufferAccess@ is not enabled, applications /must/ not
  --     perform out of bounds accesses.
  --
  vkRobustBufferAccess :: VkBool32
  , -- | @fullDrawIndexUint32@ specifies the full 32-bit range of indices is
  -- supported for indexed draw calls when using a
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' of
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT32'.
  -- @maxDrawIndexedIndexValue@ is the maximum index value that /may/ be used
  -- (aside from the primitive restart index, which is always 232-1 when the
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkIndexType' is
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VK_INDEX_TYPE_UINT32').
  -- If this feature is supported, @maxDrawIndexedIndexValue@ /must/ be
  -- 232-1; otherwise it /must/ be no smaller than 224-1. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxDrawIndexedIndexValue maxDrawIndexedIndexValue>.
  vkFullDrawIndexUint32 :: VkBool32
  , -- | @imageCubeArray@ specifies whether image views with a
  -- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewType' of
  -- 'Graphics.Vulkan.C.Core10.ImageView.VK_IMAGE_VIEW_TYPE_CUBE_ARRAY' /can/
  -- be created, and that the corresponding @SampledCubeArray@ and
  -- @ImageCubeArray@ SPIR-V capabilities /can/ be used in shader code.
  vkImageCubeArray :: VkBool32
  , -- | @independentBlend@ specifies whether the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
  -- settings are controlled independently per-attachment. If this feature is
  -- not enabled, the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
  -- settings for all color attachments /must/ be identical. Otherwise, a
  -- different
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendAttachmentState'
  -- /can/ be provided for each bound color attachment.
  vkIndependentBlend :: VkBool32
  , -- | @geometryShader@ specifies whether geometry shaders are supported. If
  -- this feature is not enabled, the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_GEOMETRY_BIT' and
  -- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_GEOMETRY_SHADER_BIT'
  -- enum values /must/ not be used. This also specifies whether shader
  -- modules /can/ declare the @Geometry@ capability.
  vkGeometryShader :: VkBool32
  , -- | @tessellationShader@ specifies whether tessellation control and
  -- evaluation shaders are supported. If this feature is not enabled, the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_CONTROL_BIT',
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_SHADER_STAGE_TESSELLATION_EVALUATION_BIT',
  -- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_CONTROL_SHADER_BIT',
  -- 'Graphics.Vulkan.C.Core10.Queue.VK_PIPELINE_STAGE_TESSELLATION_EVALUATION_SHADER_BIT',
  -- and
  -- 'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO'
  -- enum values /must/ not be used. This also specifies whether shader
  -- modules /can/ declare the @Tessellation@ capability.
  vkTessellationShader :: VkBool32
  , -- | @sampleRateShading@ specifies whether
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-sampleshading Sample Shading>
  -- and multisample interpolation are supported. If this feature is not
  -- enabled, the @sampleShadingEnable@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
  -- structure /must/ be set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE' and
  -- the @minSampleShading@ member is ignored. This also specifies whether
  -- shader modules /can/ declare the @SampleRateShading@ capability.
  vkSampleRateShading :: VkBool32
  , -- | @dualSrcBlend@ specifies whether blend operations which take two sources
  -- are supported. If this feature is not enabled, the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_COLOR',
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_COLOR',
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_SRC1_ALPHA', and
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_BLEND_FACTOR_ONE_MINUS_SRC1_ALPHA'
  -- enum values /must/ not be used as source or destination blending
  -- factors. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-dsb>.
  vkDualSrcBlend :: VkBool32
  , -- | @logicOp@ specifies whether logic operations are supported. If this
  -- feature is not enabled, the @logicOpEnable@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo'
  -- structure /must/ be set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', and
  -- the @logicOp@ member is ignored.
  vkLogicOp :: VkBool32
  , -- | @multiDrawIndirect@ specifies whether multiple draw indirect is
  -- supported. If this feature is not enabled, the @drawCount@ parameter to
  -- the 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect'
  -- and
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
  -- commands /must/ be 0 or 1. The @maxDrawIndirectCount@ member of the
  -- 'VkPhysicalDeviceLimits' structure /must/ also be 1 if this feature is
  -- not supported. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits-maxDrawIndirectCount maxDrawIndirectCount>.
  vkMultiDrawIndirect :: VkBool32
  , -- | @drawIndirectFirstInstance@ specifies whether indirect draw calls
  -- support the @firstInstance@ parameter. If this feature is not enabled,
  -- the @firstInstance@ member of all
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndirectCommand'
  -- and
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkDrawIndexedIndirectCommand'
  -- structures that are provided to the
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect' and
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect'
  -- commands /must/ be 0.
  vkDrawIndirectFirstInstance :: VkBool32
  , -- | @depthClamp@ specifies whether depth clamping is supported. If this
  -- feature is not enabled, the @depthClampEnable@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
  -- structure /must/ be set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
  -- Otherwise, setting @depthClampEnable@ to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' will enable depth clamping.
  vkDepthClamp :: VkBool32
  , -- | @depthBiasClamp@ specifies whether depth bias clamping is supported. If
  -- this feature is not enabled, the @depthBiasClamp@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
  -- structure /must/ be set to 0.0 unless the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_DEPTH_BIAS' dynamic
  -- state is enabled, and the @depthBiasClamp@ parameter to
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetDepthBias'
  -- /must/ be set to 0.0.
  vkDepthBiasClamp :: VkBool32
  , -- | @fillModeNonSolid@ specifies whether point and wireframe fill modes are
  -- supported. If this feature is not enabled, the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_POLYGON_MODE_POINT' and
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_POLYGON_MODE_LINE' enum values
  -- /must/ not be used.
  vkFillModeNonSolid :: VkBool32
  , -- | @depthBounds@ specifies whether depth bounds tests are supported. If
  -- this feature is not enabled, the @depthBoundsTestEnable@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
  -- structure /must/ be set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
  -- When @depthBoundsTestEnable@ is set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', the @minDepthBounds@ and
  -- @maxDepthBounds@ members of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo'
  -- structure are ignored.
  vkDepthBounds :: VkBool32
  , -- | @wideLines@ specifies whether lines with width other than 1.0 are
  -- supported. If this feature is not enabled, the @lineWidth@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
  -- structure /must/ be set to 1.0 unless the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VK_DYNAMIC_STATE_LINE_WIDTH' dynamic
  -- state is enabled, and the @lineWidth@ parameter to
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetLineWidth'
  -- /must/ be set to 1.0. When this feature is supported, the range and
  -- granularity of supported line widths are indicated by the
  -- @lineWidthRange@ and @lineWidthGranularity@ members of the
  -- 'VkPhysicalDeviceLimits' structure, respectively.
  vkWideLines :: VkBool32
  , -- | @largePoints@ specifies whether points with size greater than 1.0 are
  -- supported. If this feature is not enabled, only a point size of 1.0
  -- written by a shader is supported. The range and granularity of supported
  -- point sizes are indicated by the @pointSizeRange@ and
  -- @pointSizeGranularity@ members of the 'VkPhysicalDeviceLimits'
  -- structure, respectively.
  vkLargePoints :: VkBool32
  , -- | @alphaToOne@ specifies whether the implementation is able to replace the
  -- alpha value of the color fragment output from the fragment shader with
  -- the maximum representable alpha value for fixed-point colors or 1.0 for
  -- floating-point colors. If this feature is not enabled, then the
  -- @alphaToOneEnable@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'
  -- structure /must/ be set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
  -- Otherwise setting @alphaToOneEnable@ to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' will enable alpha-to-one
  -- behavior.
  vkAlphaToOne :: VkBool32
  , -- | @multiViewport@ specifies whether more than one viewport is supported.
  -- If this feature is not enabled:
  --
  -- -   The @viewportCount@ and @scissorCount@ members of the
  --     'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
  --     structure /must/ be set to 1.
  --
  -- -   The @firstViewport@ and @viewportCount@ parameters to the
  --     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetViewport'
  --     command /must/ be set to 0 and 1, respectively.
  --
  -- -   The @firstScissor@ and @scissorCount@ parameters to the
  --     'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetScissor'
  --     command /must/ be set to 0 and 1, respectively.
  --
  vkMultiViewport :: VkBool32
  , -- | @samplerAnisotropy@ specifies whether anisotropic filtering is
  -- supported. If this feature is not enabled, the @anisotropyEnable@ member
  -- of the 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo' structure
  -- /must/ be 'Graphics.Vulkan.C.Core10.Core.VK_FALSE'.
  vkSamplerAnisotropy :: VkBool32
  , -- | @textureCompressionETC2@ specifies whether all of the ETC2 and EAC
  -- compressed texture formats are supported. If this feature is enabled,
  -- then the 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
  -- 'VK_FORMAT_FEATURE_BLIT_SRC_BIT' and
  -- 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT' features /must/ be
  -- supported in @optimalTilingFeatures@ for the following formats:
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11_SNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11G11_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11G11_SNORM_BLOCK'
  --
  -- To query for additional properties, or if the feature is not enabled,
  -- 'vkGetPhysicalDeviceFormatProperties' and
  -- 'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check for
  -- supported properties of individual formats as normal.
  vkTextureCompressionETC2 :: VkBool32
  , -- | @textureCompressionASTC_LDR@ specifies whether all of the ASTC LDR
  -- compressed texture formats are supported. If this feature is enabled,
  -- then the 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT',
  -- 'VK_FORMAT_FEATURE_BLIT_SRC_BIT' and
  -- 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT' features /must/ be
  -- supported in @optimalTilingFeatures@ for the following formats:
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_4x4_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_4x4_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x4_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x4_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x5_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x5_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x5_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x5_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x6_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x6_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x5_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x5_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x6_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x6_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x8_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x8_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x5_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x5_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x6_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x6_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x8_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x8_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x10_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x10_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x10_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x10_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x12_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x12_SRGB_BLOCK'
  --
  -- To query for additional properties, or if the feature is not enabled,
  -- 'vkGetPhysicalDeviceFormatProperties' and
  -- 'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check for
  -- supported properties of individual formats as normal.
  vkTextureCompressionASTC_LDR :: VkBool32
  , -- | @textureCompressionBC@ specifies whether all of the BC compressed
  -- texture formats are supported. If this feature is enabled, then the
  -- 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT', 'VK_FORMAT_FEATURE_BLIT_SRC_BIT'
  -- and 'VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT' features /must/
  -- be supported in @optimalTilingFeatures@ for the following formats:
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGB_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGB_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGBA_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGBA_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC2_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC2_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC3_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC3_SRGB_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC4_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC4_SNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC5_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC5_SNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC6H_UFLOAT_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC6H_SFLOAT_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC7_UNORM_BLOCK'
  --
  -- -   'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC7_SRGB_BLOCK'
  --
  -- To query for additional properties, or if the feature is not enabled,
  -- 'vkGetPhysicalDeviceFormatProperties' and
  -- 'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check for
  -- supported properties of individual formats as normal.
  vkTextureCompressionBC :: VkBool32
  , -- | @occlusionQueryPrecise@ specifies whether occlusion queries returning
  -- actual sample counts are supported. Occlusion queries are created in a
  -- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool' by specifying the
  -- @queryType@ of 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION'
  -- in the 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo' structure
  -- which is passed to 'Graphics.Vulkan.C.Core10.Query.vkCreateQueryPool'.
  -- If this feature is enabled, queries of this type /can/ enable
  -- 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT' in
  -- the @flags@ parameter to
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery'. If
  -- this feature is not supported, the implementation supports only boolean
  -- occlusion queries. When any samples are passed, boolean queries will
  -- return a non-zero result value, otherwise a result value of zero is
  -- returned. When this feature is enabled and
  -- 'Graphics.Vulkan.C.Core10.CommandBuffer.VK_QUERY_CONTROL_PRECISE_BIT' is
  -- set, occlusion queries will report the actual number of samples passed.
  vkOcclusionQueryPrecise :: VkBool32
  , -- | @pipelineStatisticsQuery@ specifies whether the pipeline statistics
  -- queries are supported. If this feature is not enabled, queries of type
  -- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
  -- /cannot/ be created, and none of the
  -- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlagBits' bits
  -- /can/ be set in the @pipelineStatistics@ member of the
  -- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo' structure.
  vkPipelineStatisticsQuery :: VkBool32
  , -- | @vertexPipelineStoresAndAtomics@ specifies whether storage buffers and
  -- images support stores and atomic operations in the vertex, tessellation,
  -- and geometry shader stages. If this feature is not enabled, all storage
  -- image, storage texel buffers, and storage buffer variables used by these
  -- stages in shader modules /must/ be decorated with the @NonWritable@
  -- decoration (or the @readonly@ memory qualifier in GLSL).
  vkVertexPipelineStoresAndAtomics :: VkBool32
  , -- | @fragmentStoresAndAtomics@ specifies whether storage buffers and images
  -- support stores and atomic operations in the fragment shader stage. If
  -- this feature is not enabled, all storage image, storage texel buffers,
  -- and storage buffer variables used by the fragment stage in shader
  -- modules /must/ be decorated with the @NonWritable@ decoration (or the
  -- @readonly@ memory qualifier in GLSL).
  vkFragmentStoresAndAtomics :: VkBool32
  , -- | @shaderTessellationAndGeometryPointSize@ specifies whether the
  -- @PointSize@ built-in decoration is available in the tessellation
  -- control, tessellation evaluation, and geometry shader stages. If this
  -- feature is not enabled, members decorated with the @PointSize@ built-in
  -- decoration /must/ not be read from or written to and all points written
  -- from a tessellation or geometry shader will have a size of 1.0. This
  -- also specifies whether shader modules /can/ declare the
  -- @TessellationPointSize@ capability for tessellation control and
  -- evaluation shaders, or if the shader modules /can/ declare the
  -- @GeometryPointSize@ capability for geometry shaders. An implementation
  -- supporting this feature /must/ also support one or both of the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-tessellationShader tessellationShader>
  -- or
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-geometryShader geometryShader>
  -- features.
  vkShaderTessellationAndGeometryPointSize :: VkBool32
  , -- | @shaderImageGatherExtended@ specifies whether the extended set of image
  -- gather instructions are available in shader code. If this feature is not
  -- enabled, the @OpImage@*@Gather@ instructions do not support the @Offset@
  -- and @ConstOffsets@ operands. This also specifies whether shader modules
  -- /can/ declare the @ImageGatherExtended@ capability.
  vkShaderImageGatherExtended :: VkBool32
  , -- | @shaderStorageImageExtendedFormats@ specifies whether all the extended
  -- storage image formats are available in shader code. If this feature is
  -- enabled then the 'VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT' feature /must/ be
  -- supported in @optimalTilingFeatures@ for all of the extended formats. To
  -- query for additional properties, or if the feature is not enabled,
  -- 'vkGetPhysicalDeviceFormatProperties' and
  -- 'vkGetPhysicalDeviceImageFormatProperties' /can/ be used to check for
  -- supported properties of individual formats as normal.
  vkShaderStorageImageExtendedFormats :: VkBool32
  , -- | @shaderStorageImageMultisample@ specifies whether multisampled storage
  -- images are supported. If this feature is not enabled, images that are
  -- created with a @usage@ that includes 'VK_IMAGE_USAGE_STORAGE_BIT' /must/
  -- be created with @samples@ equal to 'VK_SAMPLE_COUNT_1_BIT'. This also
  -- specifies whether shader modules /can/ declare the
  -- @StorageImageMultisample@ capability.
  vkShaderStorageImageMultisample :: VkBool32
  , -- | @shaderStorageImageReadWithoutFormat@ specifies whether storage images
  -- require a format qualifier to be specified when reading from storage
  -- images. If this feature is not enabled, the @OpImageRead@ instruction
  -- /must/ not have an @OpTypeImage@ of @Unknown@. This also specifies
  -- whether shader modules /can/ declare the @StorageImageReadWithoutFormat@
  -- capability.
  vkShaderStorageImageReadWithoutFormat :: VkBool32
  , -- | @shaderStorageImageWriteWithoutFormat@ specifies whether storage images
  -- require a format qualifier to be specified when writing to storage
  -- images. If this feature is not enabled, the @OpImageWrite@ instruction
  -- /must/ not have an @OpTypeImage@ of @Unknown@. This also specifies
  -- whether shader modules /can/ declare the
  -- @StorageImageWriteWithoutFormat@ capability.
  vkShaderStorageImageWriteWithoutFormat :: VkBool32
  , -- | @shaderUniformBufferArrayDynamicIndexing@ specifies whether arrays of
  -- uniform buffers /can/ be indexed by /dynamically uniform/ integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also specifies whether shader modules
  -- /can/ declare the @UniformBufferArrayDynamicIndexing@ capability.
  vkShaderUniformBufferArrayDynamicIndexing :: VkBool32
  , -- | @shaderSampledImageArrayDynamicIndexing@ specifies whether arrays of
  -- samplers or sampled images /can/ be indexed by dynamically uniform
  -- integer expressions in shader code. If this feature is not enabled,
  -- resources with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also specifies whether shader modules
  -- /can/ declare the @SampledImageArrayDynamicIndexing@ capability.
  vkShaderSampledImageArrayDynamicIndexing :: VkBool32
  , -- | @shaderStorageBufferArrayDynamicIndexing@ specifies whether arrays of
  -- storage buffers /can/ be indexed by dynamically uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also specifies whether shader modules
  -- /can/ declare the @StorageBufferArrayDynamicIndexing@ capability.
  vkShaderStorageBufferArrayDynamicIndexing :: VkBool32
  , -- | @shaderStorageImageArrayDynamicIndexing@ specifies whether arrays of
  -- storage images /can/ be indexed by dynamically uniform integer
  -- expressions in shader code. If this feature is not enabled, resources
  -- with a descriptor type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE'
  -- /must/ be indexed only by constant integral expressions when aggregated
  -- into arrays in shader code. This also specifies whether shader modules
  -- /can/ declare the @StorageImageArrayDynamicIndexing@ capability.
  vkShaderStorageImageArrayDynamicIndexing :: VkBool32
  , -- | @shaderClipDistance@ specifies whether clip distances are supported in
  -- shader code. If this feature is not enabled, any members decorated with
  -- the @ClipDistance@ built-in decoration /must/ not be read from or
  -- written to in shader modules. This also specifies whether shader modules
  -- /can/ declare the @ClipDistance@ capability.
  vkShaderClipDistance :: VkBool32
  , -- | @shaderCullDistance@ specifies whether cull distances are supported in
  -- shader code. If this feature is not enabled, any members decorated with
  -- the @CullDistance@ built-in decoration /must/ not be read from or
  -- written to in shader modules. This also specifies whether shader modules
  -- /can/ declare the @CullDistance@ capability.
  vkShaderCullDistance :: VkBool32
  , -- | @shaderFloat64@ specifies whether 64-bit floats (doubles) are supported
  -- in shader code. If this feature is not enabled, 64-bit floating-point
  -- types /must/ not be used in shader code. This also specifies whether
  -- shader modules /can/ declare the @Float64@ capability.
  vkShaderFloat64 :: VkBool32
  , -- | @shaderInt64@ specifies whether 64-bit integers (signed and unsigned)
  -- are supported in shader code. If this feature is not enabled, 64-bit
  -- integer types /must/ not be used in shader code. This also specifies
  -- whether shader modules /can/ declare the @Int64@ capability.
  vkShaderInt64 :: VkBool32
  , -- | @shaderInt16@ specifies whether 16-bit integers (signed and unsigned)
  -- are supported in shader code. If this feature is not enabled, 16-bit
  -- integer types /must/ not be used in shader code. This also specifies
  -- whether shader modules /can/ declare the @Int16@ capability.
  vkShaderInt16 :: VkBool32
  , -- | @shaderResourceResidency@ specifies whether image operations that return
  -- resource residency information are supported in shader code. If this
  -- feature is not enabled, the @OpImageSparse@* instructions /must/ not be
  -- used in shader code. This also specifies whether shader modules /can/
  -- declare the @SparseResidency@ capability. The feature requires at least
  -- one of the @sparseResidency*@ features to be supported.
  vkShaderResourceResidency :: VkBool32
  , -- | @shaderResourceMinLod@ specifies whether image operations that specify
  -- the minimum resource LOD are supported in shader code. If this feature
  -- is not enabled, the @MinLod@ image operand /must/ not be used in shader
  -- code. This also specifies whether shader modules /can/ declare the
  -- @MinLod@ capability.
  vkShaderResourceMinLod :: VkBool32
  , -- | @sparseBinding@ specifies whether resource memory /can/ be managed at
  -- opaque sparse block level instead of at the object level. If this
  -- feature is not enabled, resource memory /must/ be bound only on a
  -- per-object basis using the
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory' and
  -- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory' commands.
  -- In this case, buffers and images /must/ not be created with
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_BINDING_BIT'
  -- and 'VK_IMAGE_CREATE_SPARSE_BINDING_BIT' set in the @flags@ member of
  -- the 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' and
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structures,
  -- respectively. Otherwise resource memory /can/ be managed as described in
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>.
  vkSparseBinding :: VkBool32
  , -- | @sparseResidencyBuffer@ specifies whether the device /can/ access
  -- partially resident buffers. If this feature is not enabled, buffers
  -- /must/ not be created with
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_RESIDENCY_BIT'
  -- set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' structure.
  vkSparseResidencyBuffer :: VkBool32
  , -- | @sparseResidencyImage2D@ specifies whether the device /can/ access
  -- partially resident 2D images with 1 sample per pixel. If this feature is
  -- not enabled, images with an @imageType@ of 'VK_IMAGE_TYPE_2D' and
  -- @samples@ set to 'VK_SAMPLE_COUNT_1_BIT' /must/ not be created with
  -- 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure.
  vkSparseResidencyImage2D :: VkBool32
  , -- | @sparseResidencyImage3D@ specifies whether the device /can/ access
  -- partially resident 3D images. If this feature is not enabled, images
  -- with an @imageType@ of 'VK_IMAGE_TYPE_3D' /must/ not be created with
  -- 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure.
  vkSparseResidencyImage3D :: VkBool32
  , -- | @sparseResidency2Samples@ specifies whether the physical device /can/
  -- access partially resident 2D images with 2 samples per pixel. If this
  -- feature is not enabled, images with an @imageType@ of 'VK_IMAGE_TYPE_2D'
  -- and @samples@ set to 'VK_SAMPLE_COUNT_2_BIT' /must/ not be created with
  -- 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure.
  vkSparseResidency2Samples :: VkBool32
  , -- | @sparseResidency4Samples@ specifies whether the physical device /can/
  -- access partially resident 2D images with 4 samples per pixel. If this
  -- feature is not enabled, images with an @imageType@ of 'VK_IMAGE_TYPE_2D'
  -- and @samples@ set to 'VK_SAMPLE_COUNT_4_BIT' /must/ not be created with
  -- 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure.
  vkSparseResidency4Samples :: VkBool32
  , -- | @sparseResidency8Samples@ specifies whether the physical device /can/
  -- access partially resident 2D images with 8 samples per pixel. If this
  -- feature is not enabled, images with an @imageType@ of 'VK_IMAGE_TYPE_2D'
  -- and @samples@ set to 'VK_SAMPLE_COUNT_8_BIT' /must/ not be created with
  -- 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure.
  vkSparseResidency8Samples :: VkBool32
  , -- | @sparseResidency16Samples@ specifies whether the physical device /can/
  -- access partially resident 2D images with 16 samples per pixel. If this
  -- feature is not enabled, images with an @imageType@ of 'VK_IMAGE_TYPE_2D'
  -- and @samples@ set to 'VK_SAMPLE_COUNT_16_BIT' /must/ not be created with
  -- 'VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT' set in the @flags@ member of the
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structure.
  vkSparseResidency16Samples :: VkBool32
  , -- | @sparseResidencyAliased@ specifies whether the physical device /can/
  -- correctly access data aliased into multiple locations. If this feature
  -- is not enabled, the
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_CREATE_SPARSE_ALIASED_BIT'
  -- and 'VK_IMAGE_CREATE_SPARSE_ALIASED_BIT' enum values /must/ not be used
  -- in @flags@ members of the
  -- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' and
  -- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo' structures,
  -- respectively.
  vkSparseResidencyAliased :: VkBool32
  , -- | @variableMultisampleRate@ specifies whether all pipelines that will be
  -- bound to a command buffer during a subpass with no attachments /must/
  -- have the same value for
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo'::@rasterizationSamples@.
  -- If set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the implementation
  -- supports variable multisample rates in a subpass with no attachments. If
  -- set to 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', then all pipelines
  -- bound in such a subpass /must/ have the same multisample rate. This has
  -- no effect in situations where a subpass uses any attachments.
  vkVariableMultisampleRate :: VkBool32
  , -- | @inheritedQueries@ specifies whether a secondary command buffer /may/ be
  -- executed while a query is active.
  vkInheritedQueries :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceFeatures where
  sizeOf ~_ = 220
  alignment ~_ = 4
  peek ptr = VkPhysicalDeviceFeatures <$> peek (ptr `plusPtr` 0)
                                      <*> peek (ptr `plusPtr` 4)
                                      <*> peek (ptr `plusPtr` 8)
                                      <*> peek (ptr `plusPtr` 12)
                                      <*> peek (ptr `plusPtr` 16)
                                      <*> peek (ptr `plusPtr` 20)
                                      <*> peek (ptr `plusPtr` 24)
                                      <*> peek (ptr `plusPtr` 28)
                                      <*> peek (ptr `plusPtr` 32)
                                      <*> peek (ptr `plusPtr` 36)
                                      <*> peek (ptr `plusPtr` 40)
                                      <*> peek (ptr `plusPtr` 44)
                                      <*> peek (ptr `plusPtr` 48)
                                      <*> peek (ptr `plusPtr` 52)
                                      <*> peek (ptr `plusPtr` 56)
                                      <*> peek (ptr `plusPtr` 60)
                                      <*> peek (ptr `plusPtr` 64)
                                      <*> peek (ptr `plusPtr` 68)
                                      <*> peek (ptr `plusPtr` 72)
                                      <*> peek (ptr `plusPtr` 76)
                                      <*> peek (ptr `plusPtr` 80)
                                      <*> peek (ptr `plusPtr` 84)
                                      <*> peek (ptr `plusPtr` 88)
                                      <*> peek (ptr `plusPtr` 92)
                                      <*> peek (ptr `plusPtr` 96)
                                      <*> peek (ptr `plusPtr` 100)
                                      <*> peek (ptr `plusPtr` 104)
                                      <*> peek (ptr `plusPtr` 108)
                                      <*> peek (ptr `plusPtr` 112)
                                      <*> peek (ptr `plusPtr` 116)
                                      <*> peek (ptr `plusPtr` 120)
                                      <*> peek (ptr `plusPtr` 124)
                                      <*> peek (ptr `plusPtr` 128)
                                      <*> peek (ptr `plusPtr` 132)
                                      <*> peek (ptr `plusPtr` 136)
                                      <*> peek (ptr `plusPtr` 140)
                                      <*> peek (ptr `plusPtr` 144)
                                      <*> peek (ptr `plusPtr` 148)
                                      <*> peek (ptr `plusPtr` 152)
                                      <*> peek (ptr `plusPtr` 156)
                                      <*> peek (ptr `plusPtr` 160)
                                      <*> peek (ptr `plusPtr` 164)
                                      <*> peek (ptr `plusPtr` 168)
                                      <*> peek (ptr `plusPtr` 172)
                                      <*> peek (ptr `plusPtr` 176)
                                      <*> peek (ptr `plusPtr` 180)
                                      <*> peek (ptr `plusPtr` 184)
                                      <*> peek (ptr `plusPtr` 188)
                                      <*> peek (ptr `plusPtr` 192)
                                      <*> peek (ptr `plusPtr` 196)
                                      <*> peek (ptr `plusPtr` 200)
                                      <*> peek (ptr `plusPtr` 204)
                                      <*> peek (ptr `plusPtr` 208)
                                      <*> peek (ptr `plusPtr` 212)
                                      <*> peek (ptr `plusPtr` 216)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkRobustBufferAccess (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 4) (vkFullDrawIndexUint32 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 8) (vkImageCubeArray (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 12) (vkIndependentBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 16) (vkGeometryShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 20) (vkTessellationShader (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 24) (vkSampleRateShading (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 28) (vkDualSrcBlend (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 32) (vkLogicOp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 36) (vkMultiDrawIndirect (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 40) (vkDrawIndirectFirstInstance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 44) (vkDepthClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 48) (vkDepthBiasClamp (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 52) (vkFillModeNonSolid (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 56) (vkDepthBounds (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 60) (vkWideLines (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 64) (vkLargePoints (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 68) (vkAlphaToOne (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 72) (vkMultiViewport (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 76) (vkSamplerAnisotropy (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 80) (vkTextureCompressionETC2 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 84) (vkTextureCompressionASTC_LDR (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 88) (vkTextureCompressionBC (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 92) (vkOcclusionQueryPrecise (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 96) (vkPipelineStatisticsQuery (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 100) (vkVertexPipelineStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 104) (vkFragmentStoresAndAtomics (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 108) (vkShaderTessellationAndGeometryPointSize (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 112) (vkShaderImageGatherExtended (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 116) (vkShaderStorageImageExtendedFormats (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 120) (vkShaderStorageImageMultisample (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 124) (vkShaderStorageImageReadWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 128) (vkShaderStorageImageWriteWithoutFormat (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 132) (vkShaderUniformBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 136) (vkShaderSampledImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 140) (vkShaderStorageBufferArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 144) (vkShaderStorageImageArrayDynamicIndexing (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 148) (vkShaderClipDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 152) (vkShaderCullDistance (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 156) (vkShaderFloat64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 160) (vkShaderInt64 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 164) (vkShaderInt16 (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 168) (vkShaderResourceResidency (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 172) (vkShaderResourceMinLod (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 176) (vkSparseBinding (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 180) (vkSparseResidencyBuffer (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 184) (vkSparseResidencyImage2D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 188) (vkSparseResidencyImage3D (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 192) (vkSparseResidency2Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 196) (vkSparseResidency4Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 200) (vkSparseResidency8Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 204) (vkSparseResidency16Samples (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 208) (vkSparseResidencyAliased (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 212) (vkVariableMultisampleRate (poked :: VkPhysicalDeviceFeatures))
                *> poke (ptr `plusPtr` 216) (vkInheritedQueries (poked :: VkPhysicalDeviceFeatures))

instance Zero VkPhysicalDeviceFeatures where
  zero = VkPhysicalDeviceFeatures zero
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
                                  zero
                                  zero
                                  zero

-- | VkPhysicalDeviceLimits - Structure reporting implementation-dependent
-- physical device limits
--
-- = Members
--
-- The 'VkPhysicalDeviceLimits' are properties of the physical device.
-- These are available in the @limits@ member of the
-- 'VkPhysicalDeviceProperties' structure which is returned from
-- 'vkGetPhysicalDeviceProperties'.
--
-- = Description
--
-- [1]
--     For all bitmasks of 'VkSampleCountFlagBits', the sample count limits
--     defined above represent the minimum supported sample counts for each
--     image type. Individual images /may/ support additional sample
--     counts, which are queried using
--     'vkGetPhysicalDeviceImageFormatProperties' as described in
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-supported-sample-counts Supported Sample Counts>.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32', 'VkDeviceSize',
-- 'VkPhysicalDeviceProperties', 'VkSampleCountFlags'
data VkPhysicalDeviceLimits = VkPhysicalDeviceLimits
  { -- | @maxImageDimension1D@ is the maximum dimension (@width@) supported for
  -- all images created with an @imageType@ of 'VK_IMAGE_TYPE_1D'.
  vkMaxImageDimension1D :: Word32
  , -- | @maxImageDimension2D@ is the maximum dimension (@width@ or @height@)
  -- supported for all images created with an @imageType@ of
  -- 'VK_IMAGE_TYPE_2D' and without 'VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT' set
  -- in @flags@.
  vkMaxImageDimension2D :: Word32
  , -- | @maxImageDimension3D@ is the maximum dimension (@width@, @height@, or
  -- @depth@) supported for all images created with an @imageType@ of
  -- 'VK_IMAGE_TYPE_3D'.
  vkMaxImageDimension3D :: Word32
  , -- | @maxImageDimensionCube@ is the maximum dimension (@width@ or @height@)
  -- supported for all images created with an @imageType@ of
  -- 'VK_IMAGE_TYPE_2D' and with 'VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT' set in
  -- @flags@.
  vkMaxImageDimensionCube :: Word32
  , -- | @maxImageArrayLayers@ is the maximum number of layers (@arrayLayers@)
  -- for an image.
  vkMaxImageArrayLayers :: Word32
  , -- | @maxTexelBufferElements@ is the maximum number of addressable texels for
  -- a buffer view created on a buffer which was created with the
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
  -- or
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
  -- set in the @usage@ member of the
  -- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' structure.
  vkMaxTexelBufferElements :: Word32
  , -- | @maxUniformBufferRange@ is the maximum value that /can/ be specified in
  -- the @range@ member of any
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo'
  -- structures passed to a call to
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets' for
  -- descriptors of type
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'.
  vkMaxUniformBufferRange :: Word32
  , -- | @maxStorageBufferRange@ is the maximum value that /can/ be specified in
  -- the @range@ member of any
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo'
  -- structures passed to a call to
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets' for
  -- descriptors of type
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'.
  vkMaxStorageBufferRange :: Word32
  , -- | @maxPushConstantsSize@ is the maximum size, in bytes, of the pool of
  -- push constant memory. For each of the push constant ranges indicated by
  -- the @pPushConstantRanges@ member of the
  -- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo'
  -- structure, (@offset@ + @size@) /must/ be less than or equal to this
  -- limit.
  vkMaxPushConstantsSize :: Word32
  , -- | @maxMemoryAllocationCount@ is the maximum number of device memory
  -- allocations, as created by
  -- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory', which /can/
  -- simultaneously exist.
  vkMaxMemoryAllocationCount :: Word32
  , -- | @maxSamplerAllocationCount@ is the maximum number of sampler objects, as
  -- created by 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler', which
  -- /can/ simultaneously exist on a device.
  vkMaxSamplerAllocationCount :: Word32
  , -- | @bufferImageGranularity@ is the granularity, in bytes, at which buffer
  -- or linear image resources, and optimal image resources /can/ be bound to
  -- adjacent offsets in the same
  -- 'Graphics.Vulkan.C.Core10.Memory.VkDeviceMemory' object without
  -- aliasing. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#resources-bufferimagegranularity Buffer-Image Granularity>
  -- for more details.
  vkBufferImageGranularity :: VkDeviceSize
  , -- | @sparseAddressSpaceSize@ is the total amount of address space available,
  -- in bytes, for sparse memory resources. This is an upper bound on the sum
  -- of the size of all sparse resources, regardless of whether any memory is
  -- bound to them.
  vkSparseAddressSpaceSize :: VkDeviceSize
  , -- | @maxBoundDescriptorSets@ is the maximum number of descriptor sets that
  -- /can/ be simultaneously used by a pipeline. All
  -- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSet' decorations in
  -- shader modules /must/ have a value less than @maxBoundDescriptorSets@.
  -- See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sets>.
  vkMaxBoundDescriptorSets :: Word32
  , -- | @maxPerStageDescriptorSamplers@ is the maximum number of samplers that
  -- /can/ be accessible to a single shader stage in a pipeline layout.
  -- Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER' or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
  -- count against this limit. A descriptor is accessible to a shader stage
  -- when the @stageFlags@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
  -- structure has the bit for that shader stage set. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampler>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>.
  vkMaxPerStageDescriptorSamplers :: Word32
  , -- | @maxPerStageDescriptorUniformBuffers@ is the maximum number of uniform
  -- buffers that /can/ be accessible to a single shader stage in a pipeline
  -- layout. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- count against this limit. A descriptor is accessible to a shader stage
  -- when the @stageFlags@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
  -- structure has the bit for that shader stage set. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
  vkMaxPerStageDescriptorUniformBuffers :: Word32
  , -- | @maxPerStageDescriptorStorageBuffers@ is the maximum number of storage
  -- buffers that /can/ be accessible to a single shader stage in a pipeline
  -- layout. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- count against this limit. A descriptor is accessible to a pipeline
  -- shader stage when the @stageFlags@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
  -- structure has the bit for that shader stage set. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
  vkMaxPerStageDescriptorStorageBuffers :: Word32
  , -- | @maxPerStageDescriptorSampledImages@ is the maximum number of sampled
  -- images that /can/ be accessible to a single shader stage in a pipeline
  -- layout. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
  -- count against this limit. A descriptor is accessible to a pipeline
  -- shader stage when the @stageFlags@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
  -- structure has the bit for that shader stage set. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>,
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage>,
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer>.
  vkMaxPerStageDescriptorSampledImages :: Word32
  , -- | @maxPerStageDescriptorStorageImages@ is the maximum number of storage
  -- images that /can/ be accessible to a single shader stage in a pipeline
  -- layout. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
  -- count against this limit. A descriptor is accessible to a pipeline
  -- shader stage when the @stageFlags@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
  -- structure has the bit for that shader stage set. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage>,
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer>.
  vkMaxPerStageDescriptorStorageImages :: Word32
  , -- | @maxPerStageDescriptorInputAttachments@ is the maximum number of input
  -- attachments that /can/ be accessible to a single shader stage in a
  -- pipeline layout. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
  -- count against this limit. A descriptor is accessible to a pipeline
  -- shader stage when the @stageFlags@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutBinding'
  -- structure has the bit for that shader stage set. These are only
  -- supported for the fragment stage. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-inputattachment>.
  vkMaxPerStageDescriptorInputAttachments :: Word32
  , -- | @maxPerStageResources@ is the maximum number of resources that /can/ be
  -- accessible to a single shader stage in a pipeline layout. Descriptors
  -- with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
  -- count against this limit. For the fragment shader stage the framebuffer
  -- color attachments also count against this limit.
  vkMaxPerStageResources :: Word32
  , -- | @maxDescriptorSetSamplers@ is the maximum number of samplers that /can/
  -- be included in descriptor bindings in a pipeline layout across all
  -- pipeline shader stages and descriptor set numbers. Descriptors with a
  -- type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLER' or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampler>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>.
  vkMaxDescriptorSetSamplers :: Word32
  , -- | @maxDescriptorSetUniformBuffers@ is the maximum number of uniform
  -- buffers that /can/ be included in descriptor bindings in a pipeline
  -- layout across all pipeline shader stages and descriptor set numbers.
  -- Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbuffer>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
  vkMaxDescriptorSetUniformBuffers :: Word32
  , -- | @maxDescriptorSetUniformBuffersDynamic@ is the maximum number of dynamic
  -- uniform buffers that /can/ be included in descriptor bindings in a
  -- pipeline layout across all pipeline shader stages and descriptor set
  -- numbers. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic>.
  vkMaxDescriptorSetUniformBuffersDynamic :: Word32
  , -- | @maxDescriptorSetStorageBuffers@ is the maximum number of storage
  -- buffers that /can/ be included in descriptor bindings in a pipeline
  -- layout across all pipeline shader stages and descriptor set numbers.
  -- Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebuffer>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
  vkMaxDescriptorSetStorageBuffers :: Word32
  , -- | @maxDescriptorSetStorageBuffersDynamic@ is the maximum number of dynamic
  -- storage buffers that /can/ be included in descriptor bindings in a
  -- pipeline layout across all pipeline shader stages and descriptor set
  -- numbers. Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic>.
  vkMaxDescriptorSetStorageBuffersDynamic :: Word32
  , -- | @maxDescriptorSetSampledImages@ is the maximum number of sampled images
  -- that /can/ be included in descriptor bindings in a pipeline layout
  -- across all pipeline shader stages and descriptor set numbers.
  -- Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-combinedimagesampler>,
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-sampledimage>,
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer>.
  vkMaxDescriptorSetSampledImages :: Word32
  , -- | @maxDescriptorSetStorageImages@ is the maximum number of storage images
  -- that /can/ be included in descriptor bindings in a pipeline layout
  -- across all pipeline shader stages and descriptor set numbers.
  -- Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_IMAGE',
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storageimage>,
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer>.
  vkMaxDescriptorSetStorageImages :: Word32
  , -- | @maxDescriptorSetInputAttachments@ is the maximum number of input
  -- attachments that /can/ be included in descriptor bindings in a pipeline
  -- layout across all pipeline shader stages and descriptor set numbers.
  -- Descriptors with a type of
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT'
  -- count against this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#descriptorsets-inputattachment>.
  vkMaxDescriptorSetInputAttachments :: Word32
  , -- | @maxVertexInputAttributes@ is the maximum number of vertex input
  -- attributes that /can/ be specified for a graphics pipeline. These are
  -- described in the array of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'
  -- structures that are provided at graphics pipeline creation time via the
  -- @pVertexAttributeDescriptions@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo'
  -- structure. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-attrib>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-input>.
  vkMaxVertexInputAttributes :: Word32
  , -- | @maxVertexInputBindings@ is the maximum number of vertex buffers that
  -- /can/ be specified for providing vertex attributes to a graphics
  -- pipeline. These are described in the array of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
  -- structures that are provided at graphics pipeline creation time via the
  -- @pVertexBindingDescriptions@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo'
  -- structure. The @binding@ member of
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
  -- /must/ be less than this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-input>.
  vkMaxVertexInputBindings :: Word32
  , -- | @maxVertexInputAttributeOffset@ is the maximum vertex input attribute
  -- offset that /can/ be added to the vertex input binding stride. The
  -- @offset@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription'
  -- structure /must/ be less than or equal to this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-input>.
  vkMaxVertexInputAttributeOffset :: Word32
  , -- | @maxVertexInputBindingStride@ is the maximum vertex input binding stride
  -- that /can/ be specified in a vertex input binding. The @stride@ member
  -- of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputBindingDescription'
  -- structure /must/ be less than or equal to this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fxvertex-input>.
  vkMaxVertexInputBindingStride :: Word32
  , -- | @maxVertexOutputComponents@ is the maximum number of components of
  -- output variables which /can/ be output by a vertex shader. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-vertex>.
  vkMaxVertexOutputComponents :: Word32
  , -- | @maxTessellationGenerationLevel@ is the maximum tessellation generation
  -- level supported by the fixed-function tessellation primitive generator.
  -- See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#tessellation>.
  vkMaxTessellationGenerationLevel :: Word32
  , -- | @maxTessellationPatchSize@ is the maximum patch size, in vertices, of
  -- patches that /can/ be processed by the tessellation control shader and
  -- tessellation primitive generator. The @patchControlPoints@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo'
  -- structure specified at pipeline creation time and the value provided in
  -- the @OutputVertices@ execution mode of shader modules /must/ be less
  -- than or equal to this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#tessellation>.
  vkMaxTessellationPatchSize :: Word32
  , -- | @maxTessellationControlPerVertexInputComponents@ is the maximum number
  -- of components of input variables which /can/ be provided as per-vertex
  -- inputs to the tessellation control shader stage.
  vkMaxTessellationControlPerVertexInputComponents :: Word32
  , -- | @maxTessellationControlPerVertexOutputComponents@ is the maximum number
  -- of components of per-vertex output variables which /can/ be output from
  -- the tessellation control shader stage.
  vkMaxTessellationControlPerVertexOutputComponents :: Word32
  , -- | @maxTessellationControlPerPatchOutputComponents@ is the maximum number
  -- of components of per-patch output variables which /can/ be output from
  -- the tessellation control shader stage.
  vkMaxTessellationControlPerPatchOutputComponents :: Word32
  , -- | @maxTessellationControlTotalOutputComponents@ is the maximum total
  -- number of components of per-vertex and per-patch output variables which
  -- /can/ be output from the tessellation control shader stage.
  vkMaxTessellationControlTotalOutputComponents :: Word32
  , -- | @maxTessellationEvaluationInputComponents@ is the maximum number of
  -- components of input variables which /can/ be provided as per-vertex
  -- inputs to the tessellation evaluation shader stage.
  vkMaxTessellationEvaluationInputComponents :: Word32
  , -- | @maxTessellationEvaluationOutputComponents@ is the maximum number of
  -- components of per-vertex output variables which /can/ be output from the
  -- tessellation evaluation shader stage.
  vkMaxTessellationEvaluationOutputComponents :: Word32
  , -- | @maxGeometryShaderInvocations@ is the maximum invocation count supported
  -- for instanced geometry shaders. The value provided in the @Invocations@
  -- execution mode of shader modules /must/ be less than or equal to this
  -- limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#geometry>.
  vkMaxGeometryShaderInvocations :: Word32
  , -- | @maxGeometryInputComponents@ is the maximum number of components of
  -- input variables which /can/ be provided as inputs to the geometry shader
  -- stage.
  vkMaxGeometryInputComponents :: Word32
  , -- | @maxGeometryOutputComponents@ is the maximum number of components of
  -- output variables which /can/ be output from the geometry shader stage.
  vkMaxGeometryOutputComponents :: Word32
  , -- | @maxGeometryOutputVertices@ is the maximum number of vertices which
  -- /can/ be emitted by any geometry shader.
  vkMaxGeometryOutputVertices :: Word32
  , -- | @maxGeometryTotalOutputComponents@ is the maximum total number of
  -- components of output, across all emitted vertices, which /can/ be output
  -- from the geometry shader stage.
  vkMaxGeometryTotalOutputComponents :: Word32
  , -- | @maxFragmentInputComponents@ is the maximum number of components of
  -- input variables which /can/ be provided as inputs to the fragment shader
  -- stage.
  vkMaxFragmentInputComponents :: Word32
  , -- | @maxFragmentOutputAttachments@ is the maximum number of output
  -- attachments which /can/ be written to by the fragment shader stage.
  vkMaxFragmentOutputAttachments :: Word32
  , -- | @maxFragmentDualSrcAttachments@ is the maximum number of output
  -- attachments which /can/ be written to by the fragment shader stage when
  -- blending is enabled and one of the dual source blend modes is in use.
  -- See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#framebuffer-dsb>
  -- and
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-dualSrcBlend dualSrcBlend>.
  vkMaxFragmentDualSrcAttachments :: Word32
  , -- | @maxFragmentCombinedOutputResources@ is the total number of storage
  -- buffers, storage images, and output buffers which /can/ be used in the
  -- fragment shader stage.
  vkMaxFragmentCombinedOutputResources :: Word32
  , -- | @maxComputeSharedMemorySize@ is the maximum total storage size, in
  -- bytes, available for variables declared with the @Workgroup@ storage
  -- class in shader modules (or with the @shared@ storage qualifier in GLSL)
  -- in the compute shader stage. The amount of storage consumed by the
  -- variables declared with the @Workgroup@ storage class is
  -- implementation-dependent. However, the amount of storage consumed may
  -- not exceed the largest block size that would be obtained if all active
  -- variables declared with @Workgroup@ storage class were assigned offsets
  -- in an arbitrary order by successively taking the smallest valid offset
  -- according to the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#interfaces-resources-layout-std430 Standard Storage Buffer Layout>
  -- rules. (This is equivalent to using the GLSL std430 layout rules.)
  vkMaxComputeSharedMemorySize :: Word32
  , -- | @maxComputeWorkGroupCount@[3] is the maximum number of local workgroups
  -- that /can/ be dispatched by a single dispatch command. These three
  -- values represent the maximum number of local workgroups for the X, Y,
  -- and Z dimensions, respectively. The workgroup count parameters to the
  -- dispatch commands /must/ be less than or equal to the corresponding
  -- limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#dispatch>.
  vkMaxComputeWorkGroupCount :: Vector 3 Word32
  , -- | @maxComputeWorkGroupInvocations@ is the maximum total number of compute
  -- shader invocations in a single local workgroup. The product of the X, Y,
  -- and Z sizes as specified by the @LocalSize@ execution mode in shader
  -- modules and by the object decorated by the @WorkgroupSize@ decoration
  -- /must/ be less than or equal to this limit.
  vkMaxComputeWorkGroupInvocations :: Word32
  , -- | @maxComputeWorkGroupSize@[3] is the maximum size of a local compute
  -- workgroup, per dimension. These three values represent the maximum local
  -- workgroup size in the X, Y, and Z dimensions, respectively. The @x@,
  -- @y@, and @z@ sizes specified by the @LocalSize@ execution mode and by
  -- the object decorated by the @WorkgroupSize@ decoration in shader modules
  -- /must/ be less than or equal to the corresponding limit.
  vkMaxComputeWorkGroupSize :: Vector 3 Word32
  , -- | @subPixelPrecisionBits@ is the number of bits of subpixel precision in
  -- framebuffer coordinates xf and yf. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast>.
  vkSubPixelPrecisionBits :: Word32
  , -- | @subTexelPrecisionBits@ is the number of bits of precision in the
  -- division along an axis of an image used for minification and
  -- magnification filters. 2@subTexelPrecisionBits@ is the actual number of
  -- divisions along each axis of the image represented. Sub-texel values
  -- calculated during image sampling will snap to these locations when
  -- generating the filtered results.
  vkSubTexelPrecisionBits :: Word32
  , -- | @mipmapPrecisionBits@ is the number of bits of division that the LOD
  -- calculation for mipmap fetching get snapped to when determining the
  -- contribution from each mip level to the mip filtered results.
  -- 2@mipmapPrecisionBits@ is the actual number of divisions.
  vkMipmapPrecisionBits :: Word32
  , -- | @maxDrawIndexedIndexValue@ is the maximum index value that /can/ be used
  -- for indexed draw calls when using 32-bit indices. This excludes the
  -- primitive restart index value of 0xFFFFFFFF. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-fullDrawIndexUint32 fullDrawIndexUint32>.
  vkMaxDrawIndexedIndexValue :: Word32
  , -- | @maxDrawIndirectCount@ is the maximum draw count that is supported for
  -- indirect draw calls. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-multiDrawIndirect multiDrawIndirect>.
  vkMaxDrawIndirectCount :: Word32
  , -- | @maxSamplerLodBias@ is the maximum absolute sampler LOD bias. The sum of
  -- the @mipLodBias@ member of the
  -- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo' structure and the
  -- @Bias@ operand of image sampling operations in shader modules (or 0 if
  -- no @Bias@ operand is provided to an image sampling operation) are
  -- clamped to the range [-@maxSamplerLodBias@,+@maxSamplerLodBias@]. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-mipLodBias>.
  vkMaxSamplerLodBias :: CFloat
  , -- | @maxSamplerAnisotropy@ is the maximum degree of sampler anisotropy. The
  -- maximum degree of anisotropic filtering used for an image sampling
  -- operation is the minimum of the @maxAnisotropy@ member of the
  -- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo' structure and
  -- this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#samplers-maxAnisotropy>.
  vkMaxSamplerAnisotropy :: CFloat
  , -- | @maxViewports@ is the maximum number of active viewports. The
  -- @viewportCount@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo'
  -- structure that is provided at pipeline creation /must/ be less than or
  -- equal to this limit.
  vkMaxViewports :: Word32
  , -- | @maxViewportDimensions@[2] are the maximum viewport dimensions in the X
  -- (width) and Y (height) dimensions, respectively. The maximum viewport
  -- dimensions /must/ be greater than or equal to the largest image which
  -- /can/ be created and used as a framebuffer attachment. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-viewport Controlling the Viewport>.
  vkMaxViewportDimensions :: Vector 2 Word32
  , -- | @viewportBoundsRange@[2] is the [minimum, maximum] range that the
  -- corners of a viewport /must/ be contained in. This range /must/ be at
  -- least [-2 × @size@, 2 × @size@ - 1], where @size@ =
  -- max(@maxViewportDimensions@[0], @maxViewportDimensions@[1]). See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-viewport Controlling the Viewport>.
  --
  -- __Note__
  --
  -- The intent of the @viewportBoundsRange@ limit is to allow a maximum
  -- sized viewport to be arbitrarily shifted relative to the output target
  -- as long as at least some portion intersects. This would give a bounds
  -- limit of [-@size@ + 1, 2 × @size@ - 1] which would allow all possible
  -- non-empty-set intersections of the output target and the viewport. Since
  -- these numbers are typically powers of two, picking the signed number
  -- range using the smallest possible number of bits ends up with the
  -- specified range.
  vkViewportBoundsRange :: Vector 2 CFloat
  , -- | @viewportSubPixelBits@ is the number of bits of subpixel precision for
  -- viewport bounds. The subpixel precision that floating-point viewport
  -- bounds are interpreted at is given by this limit.
  vkViewportSubPixelBits :: Word32
  , -- | @minMemoryMapAlignment@ is the minimum /required/ alignment, in bytes,
  -- of host visible memory allocations within the host address space. When
  -- mapping a memory allocation with
  -- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory', subtracting @offset@
  -- bytes from the returned pointer will always produce an integer multiple
  -- of this limit. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-hostaccess>.
  vkMinMemoryMapAlignment :: CSize
  , -- | @minTexelBufferOffsetAlignment@ is the minimum /required/ alignment, in
  -- bytes, for the @offset@ member of the
  -- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo' structure
  -- for texel buffers. When a buffer view is created for a buffer which was
  -- created with
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_UNIFORM_TEXEL_BUFFER_BIT'
  -- or
  -- 'Graphics.Vulkan.C.Core10.Buffer.VK_BUFFER_USAGE_STORAGE_TEXEL_BUFFER_BIT'
  -- set in the @usage@ member of the
  -- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo' structure, the
  -- @offset@ /must/ be an integer multiple of this limit.
  vkMinTexelBufferOffsetAlignment :: VkDeviceSize
  , -- | @minUniformBufferOffsetAlignment@ is the minimum /required/ alignment,
  -- in bytes, for the @offset@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo'
  -- structure for uniform buffers. When a descriptor of type
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC'
  -- is updated, the @offset@ /must/ be an integer multiple of this limit.
  -- Similarly, dynamic offsets for uniform buffers /must/ be multiples of
  -- this limit.
  vkMinUniformBufferOffsetAlignment :: VkDeviceSize
  , -- | @minStorageBufferOffsetAlignment@ is the minimum /required/ alignment,
  -- in bytes, for the @offset@ member of the
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo'
  -- structure for storage buffers. When a descriptor of type
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER'
  -- or
  -- 'Graphics.Vulkan.C.Core10.DescriptorSet.VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC'
  -- is updated, the @offset@ /must/ be an integer multiple of this limit.
  -- Similarly, dynamic offsets for storage buffers /must/ be multiples of
  -- this limit.
  vkMinStorageBufferOffsetAlignment :: VkDeviceSize
  , -- | @minTexelOffset@ is the minimum offset value for the @ConstOffset@ image
  -- operand of any of the @OpImageSample@* or @OpImageFetch@* image
  -- instructions.
  vkMinTexelOffset :: Int32
  , -- | @maxTexelOffset@ is the maximum offset value for the @ConstOffset@ image
  -- operand of any of the @OpImageSample@* or @OpImageFetch@* image
  -- instructions.
  vkMaxTexelOffset :: Word32
  , -- | @minTexelGatherOffset@ is the minimum offset value for the @Offset@ or
  -- @ConstOffsets@ image operands of any of the @OpImage@*@Gather@ image
  -- instructions.
  vkMinTexelGatherOffset :: Int32
  , -- | @maxTexelGatherOffset@ is the maximum offset value for the @Offset@ or
  -- @ConstOffsets@ image operands of any of the @OpImage@*@Gather@ image
  -- instructions.
  vkMaxTexelGatherOffset :: Word32
  , -- | @minInterpolationOffset@ is the minimum negative offset value for the
  -- @offset@ operand of the @InterpolateAtOffset@ extended instruction.
  vkMinInterpolationOffset :: CFloat
  , -- | @maxInterpolationOffset@ is the maximum positive offset value for the
  -- @offset@ operand of the @InterpolateAtOffset@ extended instruction.
  vkMaxInterpolationOffset :: CFloat
  , -- | @subPixelInterpolationOffsetBits@ is the number of subpixel fractional
  -- bits that the @x@ and @y@ offsets to the @InterpolateAtOffset@ extended
  -- instruction /may/ be rounded to as fixed-point values.
  vkSubPixelInterpolationOffsetBits :: Word32
  , -- | @maxFramebufferWidth@ is the maximum width for a framebuffer. The
  -- @width@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo' structure /must/
  -- be less than or equal to this limit.
  vkMaxFramebufferWidth :: Word32
  , -- | @maxFramebufferHeight@ is the maximum height for a framebuffer. The
  -- @height@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo' structure /must/
  -- be less than or equal to this limit.
  vkMaxFramebufferHeight :: Word32
  , -- | @maxFramebufferLayers@ is the maximum layer count for a layered
  -- framebuffer. The @layers@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo' structure /must/
  -- be less than or equal to this limit.
  vkMaxFramebufferLayers :: Word32
  , -- | @framebufferColorSampleCounts@ is a bitmask1 of 'VkSampleCountFlagBits'
  -- indicating the color sample counts that are supported for all
  -- framebuffer color attachments with floating- or fixed-point formats.
  -- There is no limit that specifies the color sample counts that are
  -- supported for all color attachments with integer formats.
  vkFramebufferColorSampleCounts :: VkSampleCountFlags
  , -- | @framebufferDepthSampleCounts@ is a bitmask1 of 'VkSampleCountFlagBits'
  -- indicating the supported depth sample counts for all framebuffer
  -- depth\/stencil attachments, when the format includes a depth component.
  vkFramebufferDepthSampleCounts :: VkSampleCountFlags
  , -- | @framebufferStencilSampleCounts@ is a bitmask1 of
  -- 'VkSampleCountFlagBits' indicating the supported stencil sample counts
  -- for all framebuffer depth\/stencil attachments, when the format includes
  -- a stencil component.
  vkFramebufferStencilSampleCounts :: VkSampleCountFlags
  , -- | @framebufferNoAttachmentsSampleCounts@ is a bitmask1 of
  -- 'VkSampleCountFlagBits' indicating the supported sample counts for a
  -- framebuffer with no attachments.
  vkFramebufferNoAttachmentsSampleCounts :: VkSampleCountFlags
  , -- | @maxColorAttachments@ is the maximum number of color attachments that
  -- /can/ be used by a subpass in a render pass. The @colorAttachmentCount@
  -- member of the 'Graphics.Vulkan.C.Core10.Pass.VkSubpassDescription'
  -- structure /must/ be less than or equal to this limit.
  vkMaxColorAttachments :: Word32
  , -- | @sampledImageColorSampleCounts@ is a bitmask1 of 'VkSampleCountFlagBits'
  -- indicating the sample counts supported for all 2D images created with
  -- 'VK_IMAGE_TILING_OPTIMAL', @usage@ containing
  -- 'VK_IMAGE_USAGE_SAMPLED_BIT', and a non-integer color format.
  vkSampledImageColorSampleCounts :: VkSampleCountFlags
  , -- | @sampledImageIntegerSampleCounts@ is a bitmask1 of
  -- 'VkSampleCountFlagBits' indicating the sample counts supported for all
  -- 2D images created with 'VK_IMAGE_TILING_OPTIMAL', @usage@ containing
  -- 'VK_IMAGE_USAGE_SAMPLED_BIT', and an integer color format.
  vkSampledImageIntegerSampleCounts :: VkSampleCountFlags
  , -- | @sampledImageDepthSampleCounts@ is a bitmask1 of 'VkSampleCountFlagBits'
  -- indicating the sample counts supported for all 2D images created with
  -- 'VK_IMAGE_TILING_OPTIMAL', @usage@ containing
  -- 'VK_IMAGE_USAGE_SAMPLED_BIT', and a depth format.
  vkSampledImageDepthSampleCounts :: VkSampleCountFlags
  , -- | @sampledImageStencilSampleCounts@ is a bitmask1 of
  -- 'VkSampleCountFlagBits' indicating the sample supported for all 2D
  -- images created with 'VK_IMAGE_TILING_OPTIMAL', @usage@ containing
  -- 'VK_IMAGE_USAGE_SAMPLED_BIT', and a stencil format.
  vkSampledImageStencilSampleCounts :: VkSampleCountFlags
  , -- | @storageImageSampleCounts@ is a bitmask1 of 'VkSampleCountFlagBits'
  -- indicating the sample counts supported for all 2D images created with
  -- 'VK_IMAGE_TILING_OPTIMAL', and @usage@ containing
  -- 'VK_IMAGE_USAGE_STORAGE_BIT'.
  vkStorageImageSampleCounts :: VkSampleCountFlags
  , -- | @maxSampleMaskWords@ is the maximum number of array elements of a
  -- variable decorated with the 'Graphics.Vulkan.Core10.Pipeline.SampleMask'
  -- built-in decoration.
  vkMaxSampleMaskWords :: Word32
  , -- | @timestampComputeAndGraphics@ specifies support for timestamps on all
  -- graphics and compute queues. If this limit is set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', all queues that advertise the
  -- 'VK_QUEUE_GRAPHICS_BIT' or 'VK_QUEUE_COMPUTE_BIT' in the
  -- 'VkQueueFamilyProperties'::@queueFlags@ support
  -- 'VkQueueFamilyProperties'::@timestampValidBits@ of at least 36. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-timestamps Timestamp Queries>.
  vkTimestampComputeAndGraphics :: VkBool32
  , -- | @timestampPeriod@ is the number of nanoseconds /required/ for a
  -- timestamp query to be incremented by 1. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-timestamps Timestamp Queries>.
  vkTimestampPeriod :: CFloat
  , -- | @maxClipDistances@ is the maximum number of clip distances that /can/ be
  -- used in a single shader stage. The size of any array declared with the
  -- @ClipDistance@ built-in decoration in a shader module /must/ be less
  -- than or equal to this limit.
  vkMaxClipDistances :: Word32
  , -- | @maxCullDistances@ is the maximum number of cull distances that /can/ be
  -- used in a single shader stage. The size of any array declared with the
  -- @CullDistance@ built-in decoration in a shader module /must/ be less
  -- than or equal to this limit.
  vkMaxCullDistances :: Word32
  , -- | @maxCombinedClipAndCullDistances@ is the maximum combined number of clip
  -- and cull distances that /can/ be used in a single shader stage. The sum
  -- of the sizes of any pair of arrays declared with the @ClipDistance@ and
  -- @CullDistance@ built-in decoration used by a single shader stage in a
  -- shader module /must/ be less than or equal to this limit.
  vkMaxCombinedClipAndCullDistances :: Word32
  , -- | @discreteQueuePriorities@ is the number of discrete priorities that
  -- /can/ be assigned to a queue based on the value of each member of
  -- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo'::@pQueuePriorities@.
  -- This /must/ be at least 2, and levels /must/ be spread evenly over the
  -- range, with at least one level at 1.0, and another at 0.0. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-priority>.
  vkDiscreteQueuePriorities :: Word32
  , -- | @pointSizeRange@[2] is the range [@minimum@,@maximum@] of supported
  -- sizes for points. Values written to variables decorated with the
  -- @PointSize@ built-in decoration are clamped to this range.
  vkPointSizeRange :: Vector 2 CFloat
  , -- | @lineWidthRange@[2] is the range [@minimum@,@maximum@] of supported
  -- widths for lines. Values specified by the @lineWidth@ member of the
  -- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo'
  -- or the @lineWidth@ parameter to
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdSetLineWidth' are
  -- clamped to this range.
  vkLineWidthRange :: Vector 2 CFloat
  , -- | @pointSizeGranularity@ is the granularity of supported point sizes. Not
  -- all point sizes in the range defined by @pointSizeRange@ are supported.
  -- This limit specifies the granularity (or increment) between successive
  -- supported point sizes.
  vkPointSizeGranularity :: CFloat
  , -- | @lineWidthGranularity@ is the granularity of supported line widths. Not
  -- all line widths in the range defined by @lineWidthRange@ are supported.
  -- This limit specifies the granularity (or increment) between successive
  -- supported line widths.
  vkLineWidthGranularity :: CFloat
  , -- | @strictLines@ specifies whether lines are rasterized according to the
  -- preferred method of rasterization. If set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', lines /may/ be rasterized
  -- under a relaxed set of rules. If set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', lines are rasterized as per the
  -- strict definition. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-lines-basic Basic Line Segment Rasterization>.
  vkStrictLines :: VkBool32
  , -- | @standardSampleLocations@ specifies whether rasterization uses the
  -- standard sample locations as documented in
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#primsrast-multisampling Multisampling>.
  -- If set to 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', the implementation
  -- uses the documented sample locations. If set to
  -- 'Graphics.Vulkan.C.Core10.Core.VK_FALSE', the implementation /may/ use
  -- different sample locations.
  vkStandardSampleLocations :: VkBool32
  , -- | @optimalBufferCopyOffsetAlignment@ is the optimal buffer offset
  -- alignment in bytes for
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage'
  -- and
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'.
  -- The per texel alignment requirements are enforced, but applications
  -- /should/ use the optimal alignment for optimal performance and power
  -- use.
  vkOptimalBufferCopyOffsetAlignment :: VkDeviceSize
  , -- | @optimalBufferCopyRowPitchAlignment@ is the optimal buffer row pitch
  -- alignment in bytes for
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyBufferToImage'
  -- and
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyImageToBuffer'.
  -- Row pitch is the number of bytes between texels with the same X
  -- coordinate in adjacent rows (Y coordinates differ by one). The per texel
  -- alignment requirements are enforced, but applications /should/ use the
  -- optimal alignment for optimal performance and power use.
  vkOptimalBufferCopyRowPitchAlignment :: VkDeviceSize
  , -- | @nonCoherentAtomSize@ is the size and alignment in bytes that bounds
  -- concurrent access to
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-device-hostaccess host-mapped device memory>.
  vkNonCoherentAtomSize :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceLimits where
  sizeOf ~_ = 504
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceLimits <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 4)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 12)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 20)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 28)
                                    <*> peek (ptr `plusPtr` 32)
                                    <*> peek (ptr `plusPtr` 36)
                                    <*> peek (ptr `plusPtr` 40)
                                    <*> peek (ptr `plusPtr` 48)
                                    <*> peek (ptr `plusPtr` 56)
                                    <*> peek (ptr `plusPtr` 64)
                                    <*> peek (ptr `plusPtr` 68)
                                    <*> peek (ptr `plusPtr` 72)
                                    <*> peek (ptr `plusPtr` 76)
                                    <*> peek (ptr `plusPtr` 80)
                                    <*> peek (ptr `plusPtr` 84)
                                    <*> peek (ptr `plusPtr` 88)
                                    <*> peek (ptr `plusPtr` 92)
                                    <*> peek (ptr `plusPtr` 96)
                                    <*> peek (ptr `plusPtr` 100)
                                    <*> peek (ptr `plusPtr` 104)
                                    <*> peek (ptr `plusPtr` 108)
                                    <*> peek (ptr `plusPtr` 112)
                                    <*> peek (ptr `plusPtr` 116)
                                    <*> peek (ptr `plusPtr` 120)
                                    <*> peek (ptr `plusPtr` 124)
                                    <*> peek (ptr `plusPtr` 128)
                                    <*> peek (ptr `plusPtr` 132)
                                    <*> peek (ptr `plusPtr` 136)
                                    <*> peek (ptr `plusPtr` 140)
                                    <*> peek (ptr `plusPtr` 144)
                                    <*> peek (ptr `plusPtr` 148)
                                    <*> peek (ptr `plusPtr` 152)
                                    <*> peek (ptr `plusPtr` 156)
                                    <*> peek (ptr `plusPtr` 160)
                                    <*> peek (ptr `plusPtr` 164)
                                    <*> peek (ptr `plusPtr` 168)
                                    <*> peek (ptr `plusPtr` 172)
                                    <*> peek (ptr `plusPtr` 176)
                                    <*> peek (ptr `plusPtr` 180)
                                    <*> peek (ptr `plusPtr` 184)
                                    <*> peek (ptr `plusPtr` 188)
                                    <*> peek (ptr `plusPtr` 192)
                                    <*> peek (ptr `plusPtr` 196)
                                    <*> peek (ptr `plusPtr` 200)
                                    <*> peek (ptr `plusPtr` 204)
                                    <*> peek (ptr `plusPtr` 208)
                                    <*> peek (ptr `plusPtr` 212)
                                    <*> peek (ptr `plusPtr` 216)
                                    <*> peek (ptr `plusPtr` 220)
                                    <*> peek (ptr `plusPtr` 232)
                                    <*> peek (ptr `plusPtr` 236)
                                    <*> peek (ptr `plusPtr` 248)
                                    <*> peek (ptr `plusPtr` 252)
                                    <*> peek (ptr `plusPtr` 256)
                                    <*> peek (ptr `plusPtr` 260)
                                    <*> peek (ptr `plusPtr` 264)
                                    <*> peek (ptr `plusPtr` 268)
                                    <*> peek (ptr `plusPtr` 272)
                                    <*> peek (ptr `plusPtr` 276)
                                    <*> peek (ptr `plusPtr` 280)
                                    <*> peek (ptr `plusPtr` 288)
                                    <*> peek (ptr `plusPtr` 296)
                                    <*> peek (ptr `plusPtr` 304)
                                    <*> peek (ptr `plusPtr` 312)
                                    <*> peek (ptr `plusPtr` 320)
                                    <*> peek (ptr `plusPtr` 328)
                                    <*> peek (ptr `plusPtr` 336)
                                    <*> peek (ptr `plusPtr` 340)
                                    <*> peek (ptr `plusPtr` 344)
                                    <*> peek (ptr `plusPtr` 348)
                                    <*> peek (ptr `plusPtr` 352)
                                    <*> peek (ptr `plusPtr` 356)
                                    <*> peek (ptr `plusPtr` 360)
                                    <*> peek (ptr `plusPtr` 364)
                                    <*> peek (ptr `plusPtr` 368)
                                    <*> peek (ptr `plusPtr` 372)
                                    <*> peek (ptr `plusPtr` 376)
                                    <*> peek (ptr `plusPtr` 380)
                                    <*> peek (ptr `plusPtr` 384)
                                    <*> peek (ptr `plusPtr` 388)
                                    <*> peek (ptr `plusPtr` 392)
                                    <*> peek (ptr `plusPtr` 396)
                                    <*> peek (ptr `plusPtr` 400)
                                    <*> peek (ptr `plusPtr` 404)
                                    <*> peek (ptr `plusPtr` 408)
                                    <*> peek (ptr `plusPtr` 412)
                                    <*> peek (ptr `plusPtr` 416)
                                    <*> peek (ptr `plusPtr` 420)
                                    <*> peek (ptr `plusPtr` 424)
                                    <*> peek (ptr `plusPtr` 428)
                                    <*> peek (ptr `plusPtr` 432)
                                    <*> peek (ptr `plusPtr` 436)
                                    <*> peek (ptr `plusPtr` 440)
                                    <*> peek (ptr `plusPtr` 444)
                                    <*> peek (ptr `plusPtr` 452)
                                    <*> peek (ptr `plusPtr` 460)
                                    <*> peek (ptr `plusPtr` 464)
                                    <*> peek (ptr `plusPtr` 468)
                                    <*> peek (ptr `plusPtr` 472)
                                    <*> peek (ptr `plusPtr` 480)
                                    <*> peek (ptr `plusPtr` 488)
                                    <*> peek (ptr `plusPtr` 496)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMaxImageDimension1D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 4) (vkMaxImageDimension2D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 8) (vkMaxImageDimension3D (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 12) (vkMaxImageDimensionCube (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 16) (vkMaxImageArrayLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 20) (vkMaxTexelBufferElements (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 24) (vkMaxUniformBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 28) (vkMaxStorageBufferRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 32) (vkMaxPushConstantsSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 36) (vkMaxMemoryAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 40) (vkMaxSamplerAllocationCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 48) (vkBufferImageGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 56) (vkSparseAddressSpaceSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 64) (vkMaxBoundDescriptorSets (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 68) (vkMaxPerStageDescriptorSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 72) (vkMaxPerStageDescriptorUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 76) (vkMaxPerStageDescriptorStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 80) (vkMaxPerStageDescriptorSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 84) (vkMaxPerStageDescriptorStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 88) (vkMaxPerStageDescriptorInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 92) (vkMaxPerStageResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 96) (vkMaxDescriptorSetSamplers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 100) (vkMaxDescriptorSetUniformBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 104) (vkMaxDescriptorSetUniformBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 108) (vkMaxDescriptorSetStorageBuffers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 112) (vkMaxDescriptorSetStorageBuffersDynamic (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 116) (vkMaxDescriptorSetSampledImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 120) (vkMaxDescriptorSetStorageImages (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 124) (vkMaxDescriptorSetInputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 128) (vkMaxVertexInputAttributes (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 132) (vkMaxVertexInputBindings (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 136) (vkMaxVertexInputAttributeOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 140) (vkMaxVertexInputBindingStride (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 144) (vkMaxVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 148) (vkMaxTessellationGenerationLevel (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 152) (vkMaxTessellationPatchSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 156) (vkMaxTessellationControlPerVertexInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 160) (vkMaxTessellationControlPerVertexOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 164) (vkMaxTessellationControlPerPatchOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 168) (vkMaxTessellationControlTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 172) (vkMaxTessellationEvaluationInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 176) (vkMaxTessellationEvaluationOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 180) (vkMaxGeometryShaderInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 184) (vkMaxGeometryInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 188) (vkMaxGeometryOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 192) (vkMaxGeometryOutputVertices (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 196) (vkMaxGeometryTotalOutputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 200) (vkMaxFragmentInputComponents (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 204) (vkMaxFragmentOutputAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 208) (vkMaxFragmentDualSrcAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 212) (vkMaxFragmentCombinedOutputResources (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 216) (vkMaxComputeSharedMemorySize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 220) (vkMaxComputeWorkGroupCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 232) (vkMaxComputeWorkGroupInvocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 236) (vkMaxComputeWorkGroupSize (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 248) (vkSubPixelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 252) (vkSubTexelPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 256) (vkMipmapPrecisionBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 260) (vkMaxDrawIndexedIndexValue (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 264) (vkMaxDrawIndirectCount (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 268) (vkMaxSamplerLodBias (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 272) (vkMaxSamplerAnisotropy (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 276) (vkMaxViewports (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 280) (vkMaxViewportDimensions (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 288) (vkViewportBoundsRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 296) (vkViewportSubPixelBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 304) (vkMinMemoryMapAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 312) (vkMinTexelBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 320) (vkMinUniformBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 328) (vkMinStorageBufferOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 336) (vkMinTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 340) (vkMaxTexelOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 344) (vkMinTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 348) (vkMaxTexelGatherOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 352) (vkMinInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 356) (vkMaxInterpolationOffset (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 360) (vkSubPixelInterpolationOffsetBits (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 364) (vkMaxFramebufferWidth (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 368) (vkMaxFramebufferHeight (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 372) (vkMaxFramebufferLayers (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 376) (vkFramebufferColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 380) (vkFramebufferDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 384) (vkFramebufferStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 388) (vkFramebufferNoAttachmentsSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 392) (vkMaxColorAttachments (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 396) (vkSampledImageColorSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 400) (vkSampledImageIntegerSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 404) (vkSampledImageDepthSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 408) (vkSampledImageStencilSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 412) (vkStorageImageSampleCounts (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 416) (vkMaxSampleMaskWords (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 420) (vkTimestampComputeAndGraphics (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 424) (vkTimestampPeriod (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 428) (vkMaxClipDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 432) (vkMaxCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 436) (vkMaxCombinedClipAndCullDistances (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 440) (vkDiscreteQueuePriorities (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 444) (vkPointSizeRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 452) (vkLineWidthRange (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 460) (vkPointSizeGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 464) (vkLineWidthGranularity (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 468) (vkStrictLines (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 472) (vkStandardSampleLocations (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 480) (vkOptimalBufferCopyOffsetAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 488) (vkOptimalBufferCopyRowPitchAlignment (poked :: VkPhysicalDeviceLimits))
                *> poke (ptr `plusPtr` 496) (vkNonCoherentAtomSize (poked :: VkPhysicalDeviceLimits))

instance Zero VkPhysicalDeviceLimits where
  zero = VkPhysicalDeviceLimits zero
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
                                zero
                                zero
                                zero

-- | VkPhysicalDeviceMemoryProperties - Structure specifying physical device
-- memory properties
--
-- = Description
--
-- The 'VkPhysicalDeviceMemoryProperties' structure describes a number of
-- /memory heaps/ as well as a number of /memory types/ that /can/ be used
-- to access memory allocated in those heaps. Each heap describes a memory
-- resource of a particular size, and each memory type describes a set of
-- memory properties (e.g. host cached vs uncached) that /can/ be used with
-- a given memory heap. Allocations using a particular memory type will
-- consume resources from the heap indicated by that memory type’s heap
-- index. More than one memory type /may/ share each heap, and the heaps
-- and memory types provide a mechanism to advertise an accurate size of
-- the physical memory resources while allowing the memory to be used with
-- a variety of different properties.
--
-- The number of memory heaps is given by @memoryHeapCount@ and is less
-- than or equal to 'VK_MAX_MEMORY_HEAPS'. Each heap is described by an
-- element of the @memoryHeaps@ array as a 'VkMemoryHeap' structure. The
-- number of memory types available across all memory heaps is given by
-- @memoryTypeCount@ and is less than or equal to 'VK_MAX_MEMORY_TYPES'.
-- Each memory type is described by an element of the @memoryTypes@ array
-- as a 'VkMemoryType' structure.
--
-- At least one heap /must/ include 'VK_MEMORY_HEAP_DEVICE_LOCAL_BIT' in
-- 'VkMemoryHeap'::@flags@. If there are multiple heaps that all have
-- similar performance characteristics, they /may/ all include
-- 'VK_MEMORY_HEAP_DEVICE_LOCAL_BIT'. In a unified memory architecture
-- (UMA) system there is often only a single memory heap which is
-- considered to be equally “local” to the host and to the device, and such
-- an implementation /must/ advertise the heap as device-local.
--
-- Each memory type returned by 'vkGetPhysicalDeviceMemoryProperties'
-- /must/ have its @propertyFlags@ set to one of the following values:
--
-- -   0
--
-- -   'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
--
-- -   'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_CACHED_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT'
--
-- -   'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_CACHED_BIT'
--
-- -   'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_CACHED_BIT' |
--     'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT'
--
-- -   'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' |
--     'VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
--
-- There /must/ be at least one memory type with both the
-- 'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' and
-- 'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT' bits set in its @propertyFlags@.
-- There /must/ be at least one memory type with the
-- 'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' bit set in its @propertyFlags@.
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
-- 'VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT' was before
-- 'VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT' |
-- 'VK_MEMORY_PROPERTY_HOST_COHERENT_BIT', the list would still be in a
-- valid order.
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
-- 'VkMemoryHeap', 'VkMemoryType',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2',
-- 'vkGetPhysicalDeviceMemoryProperties'
data VkPhysicalDeviceMemoryProperties = VkPhysicalDeviceMemoryProperties
  { -- | @memoryTypeCount@ is the number of valid elements in the @memoryTypes@
  -- array.
  vkMemoryTypeCount :: Word32
  , -- | @memoryTypes@ is an array of 'VkMemoryType' structures describing the
  -- /memory types/ that /can/ be used to access memory allocated from the
  -- heaps specified by @memoryHeaps@.
  vkMemoryTypes :: Vector VK_MAX_MEMORY_TYPES VkMemoryType
  , -- | @memoryHeapCount@ is the number of valid elements in the @memoryHeaps@
  -- array.
  vkMemoryHeapCount :: Word32
  , -- | @memoryHeaps@ is an array of 'VkMemoryHeap' structures describing the
  -- /memory heaps/ from which memory /can/ be allocated.
  vkMemoryHeaps :: Vector VK_MAX_MEMORY_HEAPS VkMemoryHeap
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryProperties where
  sizeOf ~_ = 520
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 260)
                                              <*> peek (ptr `plusPtr` 264)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkMemoryTypeCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 4) (vkMemoryTypes (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 260) (vkMemoryHeapCount (poked :: VkPhysicalDeviceMemoryProperties))
                *> poke (ptr `plusPtr` 264) (vkMemoryHeaps (poked :: VkPhysicalDeviceMemoryProperties))

instance Zero VkPhysicalDeviceMemoryProperties where
  zero = VkPhysicalDeviceMemoryProperties zero
                                          zero
                                          zero
                                          zero

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
-- 'VkPhysicalDevice' being queried.
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
-- 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- 'VkPhysicalDeviceSparseProperties', 'VkPhysicalDeviceType',
-- 'vkGetPhysicalDeviceProperties'
data VkPhysicalDeviceProperties = VkPhysicalDeviceProperties
  { -- | @apiVersion@ is the version of Vulkan supported by the device, encoded
  -- as described in
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-coreversions-versionnumbers>.
  vkApiVersion :: Word32
  , -- | @driverVersion@ is the vendor-specified version of the driver.
  vkDriverVersion :: Word32
  , -- | @vendorID@ is a unique identifier for the /vendor/ (see below) of the
  -- physical device.
  vkVendorID :: Word32
  , -- | @deviceID@ is a unique identifier for the physical device among devices
  -- available from the vendor.
  vkDeviceID :: Word32
  , -- | @deviceType@ is a 'VkPhysicalDeviceType' specifying the type of device.
  vkDeviceType :: VkPhysicalDeviceType
  , -- | @deviceName@ is a null-terminated UTF-8 string containing the name of
  -- the device.
  vkDeviceName :: Vector VK_MAX_PHYSICAL_DEVICE_NAME_SIZE CChar
  , -- | @pipelineCacheUUID@ is an array of size 'VK_UUID_SIZE', containing 8-bit
  -- values that represent a universally unique identifier for the device.
  vkPipelineCacheUUID :: Vector VK_UUID_SIZE Word8
  , -- | @limits@ is the 'VkPhysicalDeviceLimits' structure which specifies
  -- device-specific limits of the physical device. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#limits Limits>
  -- for details.
  vkLimits :: VkPhysicalDeviceLimits
  , -- | @sparseProperties@ is the 'VkPhysicalDeviceSparseProperties' structure
  -- which specifies various sparse related properties of the physical
  -- device. See
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-physicalprops Sparse Properties>
  -- for details.
  vkSparseProperties :: VkPhysicalDeviceSparseProperties
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProperties where
  sizeOf ~_ = 824
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProperties <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 4)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 12)
                                        <*> peek (ptr `plusPtr` 16)
                                        <*> peek (ptr `plusPtr` 20)
                                        <*> peek (ptr `plusPtr` 276)
                                        <*> peek (ptr `plusPtr` 296)
                                        <*> peek (ptr `plusPtr` 800)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkApiVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 4) (vkDriverVersion (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 8) (vkVendorID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 12) (vkDeviceID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 16) (vkDeviceType (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 20) (vkDeviceName (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 276) (vkPipelineCacheUUID (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 296) (vkLimits (poked :: VkPhysicalDeviceProperties))
                *> poke (ptr `plusPtr` 800) (vkSparseProperties (poked :: VkPhysicalDeviceProperties))

instance Zero VkPhysicalDeviceProperties where
  zero = VkPhysicalDeviceProperties zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero
                                    zero

-- | VkPhysicalDeviceSparseProperties - Structure specifying physical device
-- sparse memory properties
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkBool32', 'VkPhysicalDeviceProperties'
data VkPhysicalDeviceSparseProperties = VkPhysicalDeviceSparseProperties
  { -- | @residencyStandard2DBlockShape@ is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if the physical device will
  -- access all single-sample 2D sparse resources using the standard sparse
  -- image block shapes (based on image format), as described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseblockshapessingle Standard Sparse Image Block Shapes (Single Sample)>
  -- table. If this property is not supported the value returned in the
  -- @imageGranularity@ member of the
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
  -- structure for single-sample 2D images is not /required/ to match the
  -- standard sparse image block dimensions listed in the table.
  vkResidencyStandard2DBlockShape :: VkBool32
  , -- | @residencyStandard2DMultisampleBlockShape@ is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if the physical device will
  -- access all multisample 2D sparse resources using the standard sparse
  -- image block shapes (based on image format), as described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseblockshapesmsaa Standard Sparse Image Block Shapes (MSAA)>
  -- table. If this property is not supported, the value returned in the
  -- @imageGranularity@ member of the
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
  -- structure for multisample 2D images is not /required/ to match the
  -- standard sparse image block dimensions listed in the table.
  vkResidencyStandard2DMultisampleBlockShape :: VkBool32
  , -- | @residencyStandard3DBlockShape@ is
  -- 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if the physical device will
  -- access all 3D sparse resources using the standard sparse image block
  -- shapes (based on image format), as described in the
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory-sparseblockshapessingle Standard Sparse Image Block Shapes (Single Sample)>
  -- table. If this property is not supported, the value returned in the
  -- @imageGranularity@ member of the
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
  -- structure for 3D images is not /required/ to match the standard sparse
  -- image block dimensions listed in the table.
  vkResidencyStandard3DBlockShape :: VkBool32
  , -- | @residencyAlignedMipSize@ is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE' if
  -- images with mip level dimensions that are not integer multiples of the
  -- corresponding dimensions of the sparse image block /may/ be placed in
  -- the mip tail. If this property is not reported, only mip levels with
  -- dimensions smaller than the @imageGranularity@ member of the
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties'
  -- structure will be placed in the mip tail. If this property is reported
  -- the implementation is allowed to return
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VK_SPARSE_IMAGE_FORMAT_ALIGNED_MIP_SIZE_BIT'
  -- in the @flags@ member of
  -- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageFormatProperties',
  -- indicating that mip level dimensions that are not integer multiples of
  -- the corresponding dimensions of the sparse image block will be placed in
  -- the mip tail.
  vkResidencyAlignedMipSize :: VkBool32
  , -- | @residencyNonResidentStrict@ specifies whether the physical device /can/
  -- consistently access non-resident regions of a resource. If this property
  -- is 'Graphics.Vulkan.C.Core10.Core.VK_TRUE', access to non-resident
  -- regions of resources will be guaranteed to return values as if the
  -- resource were populated with 0; writes to non-resident regions will be
  -- discarded.
  vkResidencyNonResidentStrict :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceSparseProperties where
  sizeOf ~_ = 20
  alignment ~_ = 4
  peek ptr = VkPhysicalDeviceSparseProperties <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 4)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 12)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkResidencyStandard2DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 4) (vkResidencyStandard2DMultisampleBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 8) (vkResidencyStandard3DBlockShape (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 12) (vkResidencyAlignedMipSize (poked :: VkPhysicalDeviceSparseProperties))
                *> poke (ptr `plusPtr` 16) (vkResidencyNonResidentStrict (poked :: VkPhysicalDeviceSparseProperties))

instance Zero VkPhysicalDeviceSparseProperties where
  zero = VkPhysicalDeviceSparseProperties zero
                                          zero
                                          zero
                                          zero
                                          zero

-- ** VkPhysicalDeviceType

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
-- 'VkPhysicalDeviceProperties'
newtype VkPhysicalDeviceType = VkPhysicalDeviceType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkPhysicalDeviceType where
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_OTHER = showString "VK_PHYSICAL_DEVICE_TYPE_OTHER"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = showString "VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU"
  showsPrec _ VK_PHYSICAL_DEVICE_TYPE_CPU = showString "VK_PHYSICAL_DEVICE_TYPE_CPU"
  showsPrec p (VkPhysicalDeviceType x) = showParen (p >= 11) (showString "VkPhysicalDeviceType " . showsPrec 11 x)

instance Read VkPhysicalDeviceType where
  readPrec = parens ( choose [ ("VK_PHYSICAL_DEVICE_TYPE_OTHER",          pure VK_PHYSICAL_DEVICE_TYPE_OTHER)
                             , ("VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU", pure VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU",   pure VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU",    pure VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU)
                             , ("VK_PHYSICAL_DEVICE_TYPE_CPU",            pure VK_PHYSICAL_DEVICE_TYPE_CPU)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkPhysicalDeviceType")
                        v <- step readPrec
                        pure (VkPhysicalDeviceType v)
                        )
                    )

-- | 'VK_PHYSICAL_DEVICE_TYPE_OTHER' - the device does not match any other
-- available types.
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_OTHER = VkPhysicalDeviceType 0

-- | 'VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU' - the device is typically one
-- embedded in or tightly coupled with the host.
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_INTEGRATED_GPU = VkPhysicalDeviceType 1

-- | 'VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU' - the device is typically a
-- separate processor connected to the host via an interlink.
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_DISCRETE_GPU = VkPhysicalDeviceType 2

-- | 'VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU' - the device is typically a
-- virtual node in a virtualization environment.
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_VIRTUAL_GPU = VkPhysicalDeviceType 3

-- | 'VK_PHYSICAL_DEVICE_TYPE_CPU' - the device is typically running on the
-- same processors as the host.
pattern VK_PHYSICAL_DEVICE_TYPE_CPU :: VkPhysicalDeviceType
pattern VK_PHYSICAL_DEVICE_TYPE_CPU = VkPhysicalDeviceType 4

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
--     -   The @width@, @height@, and @depth@ members of a 'VkExtent3D'
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
--     -   @width@ of a 'VkExtent3D' parameter /must/ be an integer
--         multiple of Ax, or else @x@ + @width@ /must/ equal the width of
--         the image subresource corresponding to the parameter.
--
--     -   @height@ of a 'VkExtent3D' parameter /must/ be an integer
--         multiple of Ay, or else @y@ + @height@ /must/ equal the height
--         of the image subresource corresponding to the parameter.
--
--     -   @depth@ of a 'VkExtent3D' parameter /must/ be an integer
--         multiple of Az, or else @z@ + @depth@ /must/ equal the depth of
--         the image subresource corresponding to the parameter.
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
-- 'VkExtent3D',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2',
-- 'VkQueueFlags', 'vkGetPhysicalDeviceQueueFamilyProperties'
data VkQueueFamilyProperties = VkQueueFamilyProperties
  { -- | @queueFlags@ is a bitmask of 'VkQueueFlagBits' indicating capabilities
  -- of the queues in this queue family.
  vkQueueFlags :: VkQueueFlags
  , -- | @queueCount@ is the unsigned integer count of queues in this queue
  -- family. Each queue family /must/ support at least one queue.
  vkQueueCount :: Word32
  , -- | @timestampValidBits@ is the unsigned integer count of meaningful bits in
  -- the timestamps written via
  -- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp'.
  -- The valid range for the count is 36..64 bits, or a value of 0,
  -- indicating no support for timestamps. Bits outside the valid range are
  -- guaranteed to be zeros.
  vkTimestampValidBits :: Word32
  , -- | @minImageTransferGranularity@ is the minimum granularity supported for
  -- image transfer operations on the queues in this queue family.
  vkMinImageTransferGranularity :: VkExtent3D
  }
  deriving (Eq, Show)

instance Storable VkQueueFamilyProperties where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkQueueFamilyProperties <$> peek (ptr `plusPtr` 0)
                                     <*> peek (ptr `plusPtr` 4)
                                     <*> peek (ptr `plusPtr` 8)
                                     <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkQueueFlags (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 4) (vkQueueCount (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 8) (vkTimestampValidBits (poked :: VkQueueFamilyProperties))
                *> poke (ptr `plusPtr` 12) (vkMinImageTransferGranularity (poked :: VkQueueFamilyProperties))

instance Zero VkQueueFamilyProperties where
  zero = VkQueueFamilyProperties zero
                                 zero
                                 zero
                                 zero

-- ** VkQueueFlagBits

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
-- 'VK_QUEUE_GRAPHICS_BIT' or 'VK_QUEUE_COMPUTE_BIT', then reporting the
-- 'VK_QUEUE_TRANSFER_BIT' capability separately for that queue family is
-- /optional/.
--
-- For further details see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-queues Queues>.
--
-- = See Also
--
-- 'VkQueueFlags'
newtype VkQueueFlagBits = VkQueueFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkQueueFlagBits where
  showsPrec _ VK_QUEUE_GRAPHICS_BIT = showString "VK_QUEUE_GRAPHICS_BIT"
  showsPrec _ VK_QUEUE_COMPUTE_BIT = showString "VK_QUEUE_COMPUTE_BIT"
  showsPrec _ VK_QUEUE_TRANSFER_BIT = showString "VK_QUEUE_TRANSFER_BIT"
  showsPrec _ VK_QUEUE_SPARSE_BINDING_BIT = showString "VK_QUEUE_SPARSE_BINDING_BIT"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkQueueFlagBits 0x00000010) = showString "VK_QUEUE_PROTECTED_BIT"
  showsPrec _ (VkQueueFlagBits 0x00000040) = showString "VK_QUEUE_RESERVED_6_BIT_KHR"
  showsPrec _ (VkQueueFlagBits 0x00000020) = showString "VK_QUEUE_RESERVED_5_BIT_KHR"
  showsPrec p (VkQueueFlagBits x) = showParen (p >= 11) (showString "VkQueueFlagBits " . showsPrec 11 x)

instance Read VkQueueFlagBits where
  readPrec = parens ( choose [ ("VK_QUEUE_GRAPHICS_BIT",       pure VK_QUEUE_GRAPHICS_BIT)
                             , ("VK_QUEUE_COMPUTE_BIT",        pure VK_QUEUE_COMPUTE_BIT)
                             , ("VK_QUEUE_TRANSFER_BIT",       pure VK_QUEUE_TRANSFER_BIT)
                             , ("VK_QUEUE_SPARSE_BINDING_BIT", pure VK_QUEUE_SPARSE_BINDING_BIT)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_QUEUE_PROTECTED_BIT",      pure (VkQueueFlagBits 0x00000010))
                             , ("VK_QUEUE_RESERVED_6_BIT_KHR", pure (VkQueueFlagBits 0x00000040))
                             , ("VK_QUEUE_RESERVED_5_BIT_KHR", pure (VkQueueFlagBits 0x00000020))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueueFlagBits")
                        v <- step readPrec
                        pure (VkQueueFlagBits v)
                        )
                    )

-- | 'VK_QUEUE_GRAPHICS_BIT' specifies that queues in this queue family
-- support graphics operations.
pattern VK_QUEUE_GRAPHICS_BIT :: VkQueueFlagBits
pattern VK_QUEUE_GRAPHICS_BIT = VkQueueFlagBits 0x00000001

-- | 'VK_QUEUE_COMPUTE_BIT' specifies that queues in this queue family
-- support compute operations.
pattern VK_QUEUE_COMPUTE_BIT :: VkQueueFlagBits
pattern VK_QUEUE_COMPUTE_BIT = VkQueueFlagBits 0x00000002

-- | 'VK_QUEUE_TRANSFER_BIT' specifies that queues in this queue family
-- support transfer operations.
pattern VK_QUEUE_TRANSFER_BIT :: VkQueueFlagBits
pattern VK_QUEUE_TRANSFER_BIT = VkQueueFlagBits 0x00000004

-- | 'VK_QUEUE_SPARSE_BINDING_BIT' specifies that queues in this queue family
-- support sparse memory management operations (see
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#sparsememory Sparse Resources>).
-- If any of the sparse resource features are enabled, then at least one
-- queue family /must/ support this bit.
pattern VK_QUEUE_SPARSE_BINDING_BIT :: VkQueueFlagBits
pattern VK_QUEUE_SPARSE_BINDING_BIT = VkQueueFlagBits 0x00000008

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

-- ** VkSampleCountFlagBits

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
-- 'VkSampleCountFlags',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT',
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations.vkGetPhysicalDeviceMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkSampleCountFlagBits = VkSampleCountFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

instance Show VkSampleCountFlagBits where
  showsPrec _ VK_SAMPLE_COUNT_1_BIT = showString "VK_SAMPLE_COUNT_1_BIT"
  showsPrec _ VK_SAMPLE_COUNT_2_BIT = showString "VK_SAMPLE_COUNT_2_BIT"
  showsPrec _ VK_SAMPLE_COUNT_4_BIT = showString "VK_SAMPLE_COUNT_4_BIT"
  showsPrec _ VK_SAMPLE_COUNT_8_BIT = showString "VK_SAMPLE_COUNT_8_BIT"
  showsPrec _ VK_SAMPLE_COUNT_16_BIT = showString "VK_SAMPLE_COUNT_16_BIT"
  showsPrec _ VK_SAMPLE_COUNT_32_BIT = showString "VK_SAMPLE_COUNT_32_BIT"
  showsPrec _ VK_SAMPLE_COUNT_64_BIT = showString "VK_SAMPLE_COUNT_64_BIT"
  showsPrec p (VkSampleCountFlagBits x) = showParen (p >= 11) (showString "VkSampleCountFlagBits " . showsPrec 11 x)

instance Read VkSampleCountFlagBits where
  readPrec = parens ( choose [ ("VK_SAMPLE_COUNT_1_BIT",  pure VK_SAMPLE_COUNT_1_BIT)
                             , ("VK_SAMPLE_COUNT_2_BIT",  pure VK_SAMPLE_COUNT_2_BIT)
                             , ("VK_SAMPLE_COUNT_4_BIT",  pure VK_SAMPLE_COUNT_4_BIT)
                             , ("VK_SAMPLE_COUNT_8_BIT",  pure VK_SAMPLE_COUNT_8_BIT)
                             , ("VK_SAMPLE_COUNT_16_BIT", pure VK_SAMPLE_COUNT_16_BIT)
                             , ("VK_SAMPLE_COUNT_32_BIT", pure VK_SAMPLE_COUNT_32_BIT)
                             , ("VK_SAMPLE_COUNT_64_BIT", pure VK_SAMPLE_COUNT_64_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSampleCountFlagBits")
                        v <- step readPrec
                        pure (VkSampleCountFlagBits v)
                        )
                    )

-- | 'VK_SAMPLE_COUNT_1_BIT' specifies an image with one sample per pixel.
pattern VK_SAMPLE_COUNT_1_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_1_BIT = VkSampleCountFlagBits 0x00000001

-- | 'VK_SAMPLE_COUNT_2_BIT' specifies an image with 2 samples per pixel.
pattern VK_SAMPLE_COUNT_2_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_2_BIT = VkSampleCountFlagBits 0x00000002

-- | 'VK_SAMPLE_COUNT_4_BIT' specifies an image with 4 samples per pixel.
pattern VK_SAMPLE_COUNT_4_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_4_BIT = VkSampleCountFlagBits 0x00000004

-- | 'VK_SAMPLE_COUNT_8_BIT' specifies an image with 8 samples per pixel.
pattern VK_SAMPLE_COUNT_8_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_8_BIT = VkSampleCountFlagBits 0x00000008

-- | 'VK_SAMPLE_COUNT_16_BIT' specifies an image with 16 samples per pixel.
pattern VK_SAMPLE_COUNT_16_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_16_BIT = VkSampleCountFlagBits 0x00000010

-- | 'VK_SAMPLE_COUNT_32_BIT' specifies an image with 32 samples per pixel.
pattern VK_SAMPLE_COUNT_32_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_32_BIT = VkSampleCountFlagBits 0x00000020

-- | 'VK_SAMPLE_COUNT_64_BIT' specifies an image with 64 samples per pixel.
pattern VK_SAMPLE_COUNT_64_BIT :: VkSampleCountFlagBits
pattern VK_SAMPLE_COUNT_64_BIT = VkSampleCountFlagBits 0x00000040

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

-- ** VkSystemAllocationScope

-- | VkSystemAllocationScope - Allocation scope
--
-- = Description
--
-- -   'VK_SYSTEM_ALLOCATION_SCOPE_COMMAND' specifies that the allocation
--     is scoped to the duration of the Vulkan command.
--
-- -   'VK_SYSTEM_ALLOCATION_SCOPE_OBJECT' specifies that the allocation is
--     scoped to the lifetime of the Vulkan object that is being created or
--     used.
--
-- -   'VK_SYSTEM_ALLOCATION_SCOPE_CACHE' specifies that the allocation is
--     scoped to the lifetime of a
--     'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache' object.
--
-- -   'VK_SYSTEM_ALLOCATION_SCOPE_DEVICE' specifies that the allocation is
--     scoped to the lifetime of the Vulkan device.
--
-- -   'VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE' specifies that the allocation
--     is scoped to the lifetime of the Vulkan instance.
--
-- Most Vulkan commands operate on a single object, or there is a sole
-- object that is being created or manipulated. When an allocation uses an
-- allocation scope of 'VK_SYSTEM_ALLOCATION_SCOPE_OBJECT' or
-- 'VK_SYSTEM_ALLOCATION_SCOPE_CACHE', the allocation is scoped to the
-- object being created or manipulated.
--
-- When an implementation requires host memory, it will make callbacks to
-- the application using the most specific allocator and allocation scope
-- available:
--
-- -   If an allocation is scoped to the duration of a command, the
--     allocator will use the 'VK_SYSTEM_ALLOCATION_SCOPE_COMMAND'
--     allocation scope. The most specific allocator available is used: if
--     the object being created or manipulated has an allocator, that
--     object’s allocator will be used, else if the parent 'VkDevice' has
--     an allocator it will be used, else if the parent 'VkInstance' has an
--     allocator it will be used. Else,
--
-- -   If an allocation is associated with an object of type
--     'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCache', the
--     allocator will use the 'VK_SYSTEM_ALLOCATION_SCOPE_CACHE' allocation
--     scope. The most specific allocator available is used (cache, else
--     device, else instance). Else,
--
-- -   If an allocation is scoped to the lifetime of an object, that object
--     is being created or manipulated by the command, and that object’s
--     type is not 'VkDevice' or 'VkInstance', the allocator will use an
--     allocation scope of 'VK_SYSTEM_ALLOCATION_SCOPE_OBJECT'. The most
--     specific allocator available is used (object, else device, else
--     instance). Else,
--
-- -   If an allocation is scoped to the lifetime of a device, the
--     allocator will use an allocation scope of
--     'VK_SYSTEM_ALLOCATION_SCOPE_DEVICE'. The most specific allocator
--     available is used (device, else instance). Else,
--
-- -   If the allocation is scoped to the lifetime of an instance and the
--     instance has an allocator, its allocator will be used with an
--     allocation scope of 'VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE'.
--
-- -   Otherwise an implementation will allocate memory through an
--     alternative mechanism that is unspecified.
--
-- = See Also
--
-- 'VkAllocationCallbacks'
newtype VkSystemAllocationScope = VkSystemAllocationScope Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkSystemAllocationScope where
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = showString "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = showString "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_CACHE = showString "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = showString "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
  showsPrec _ VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = showString "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
  showsPrec p (VkSystemAllocationScope x) = showParen (p >= 11) (showString "VkSystemAllocationScope " . showsPrec 11 x)

instance Read VkSystemAllocationScope where
  readPrec = parens ( choose [ ("VK_SYSTEM_ALLOCATION_SCOPE_COMMAND",  pure VK_SYSTEM_ALLOCATION_SCOPE_COMMAND)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_OBJECT",   pure VK_SYSTEM_ALLOCATION_SCOPE_OBJECT)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_CACHE",    pure VK_SYSTEM_ALLOCATION_SCOPE_CACHE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_DEVICE",   pure VK_SYSTEM_ALLOCATION_SCOPE_DEVICE)
                             , ("VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE", pure VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSystemAllocationScope")
                        v <- step readPrec
                        pure (VkSystemAllocationScope v)
                        )
                    )

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_COMMAND"
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_COMMAND = VkSystemAllocationScope 0

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_OBJECT"
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_OBJECT = VkSystemAllocationScope 1

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_CACHE"
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_CACHE = VkSystemAllocationScope 2

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_DEVICE"
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_DEVICE = VkSystemAllocationScope 3

-- No documentation found for Nested "VkSystemAllocationScope" "VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE"
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE :: VkSystemAllocationScope
pattern VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE = VkSystemAllocationScope 4

-- | vkCreateInstance - Create a new Vulkan instance
--
-- = Parameters
--
-- -   @pCreateInfo@ points to an instance of 'VkInstanceCreateInfo'
--     controlling creation of the instance.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pInstance@ points a 'VkInstance' handle in which the resulting
--     instance is returned.
--
-- = Description
--
-- 'vkCreateInstance' verifies that the requested layers exist. If not,
-- 'vkCreateInstance' will return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_LAYER_NOT_PRESENT'. Next
-- 'vkCreateInstance' verifies that the requested extensions are supported
-- (e.g. in the implementation or in any enabled instance layer) and if any
-- requested extension is not supported, 'vkCreateInstance' /must/ return
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_EXTENSION_NOT_PRESENT'. After
-- verifying and enabling the instance layers and extensions the
-- 'VkInstance' object is created and returned to the application. If a
-- requested extension is only supported by a layer, both the layer and the
-- extension need to be specified at 'vkCreateInstance' time for the
-- creation to succeed.
--
-- == Valid Usage
--
-- -   All
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#extendingvulkan-extensions-extensiondependencies required extensions>
--     for each extension in the
--     'VkInstanceCreateInfo'::@ppEnabledExtensionNames@ list /must/ also
--     be present in that list.
--
-- == Valid Usage (Implicit)
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     'VkInstanceCreateInfo' structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid 'VkAllocationCallbacks' structure
--
-- -   @pInstance@ /must/ be a valid pointer to a 'VkInstance' handle
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
-- 'VkAllocationCallbacks', 'VkInstance', 'VkInstanceCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateInstance" vkCreateInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
#else
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateInstance
  :: FunPtr (("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult) -> (("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult)

vkCreateInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
vkCreateInstance = mkVkCreateInstance procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkCreateInstance $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr' nullPtr (GHC.Ptr.Ptr "vkCreateInstance\NUL"#)
#endif

type FN_vkCreateInstance = ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
type PFN_vkCreateInstance = FunPtr FN_vkCreateInstance

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
-- -   If 'VkAllocationCallbacks' were provided when @instance@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no 'VkAllocationCallbacks' were provided when @instance@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   If @instance@ is not @NULL@, @instance@ /must/ be a valid
--     'VkInstance' handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid 'VkAllocationCallbacks' structure
--
-- == Host Synchronization
--
-- -   Host access to @instance@ /must/ be externally synchronized
--
-- = See Also
--
-- 'VkAllocationCallbacks', 'VkInstance'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyInstance" vkDestroyInstance :: ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyInstance :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyInstance deviceCmds = mkVkDestroyInstance (pVkDestroyInstance deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyInstance
  :: FunPtr (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyInstance = ("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyInstance = FunPtr FN_vkDestroyInstance

-- | vkEnumeratePhysicalDevices - Enumerates the physical devices accessible
-- to a Vulkan instance
--
-- = Parameters
--
-- -   @instance@ is a handle to a Vulkan instance previously created with
--     'vkCreateInstance'.
--
-- -   @pPhysicalDeviceCount@ is a pointer to an integer related to the
--     number of physical devices available or queried, as described below.
--
-- -   @pPhysicalDevices@ is either @NULL@ or a pointer to an array of
--     'VkPhysicalDevice' handles.
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
-- -   @instance@ /must/ be a valid 'VkInstance' handle
--
-- -   @pPhysicalDeviceCount@ /must/ be a valid pointer to a @uint32_t@
--     value
--
-- -   If the value referenced by @pPhysicalDeviceCount@ is not @0@, and
--     @pPhysicalDevices@ is not @NULL@, @pPhysicalDevices@ /must/ be a
--     valid pointer to an array of @pPhysicalDeviceCount@
--     'VkPhysicalDevice' handles
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
-- 'VkInstance', 'VkPhysicalDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumeratePhysicalDevices" vkEnumeratePhysicalDevices :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
#else
vkEnumeratePhysicalDevices :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
vkEnumeratePhysicalDevices deviceCmds = mkVkEnumeratePhysicalDevices (pVkEnumeratePhysicalDevices deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDevices
  :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult)
#endif

type FN_vkEnumeratePhysicalDevices = ("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult
type PFN_vkEnumeratePhysicalDevices = FunPtr FN_vkEnumeratePhysicalDevices

-- | vkGetDeviceProcAddr - Return a function pointer for a command
--
-- = Parameters
--
-- The table below defines the various use cases for 'vkGetDeviceProcAddr'
-- and expected return value for each case.
--
-- = Description
--
-- The returned function pointer is of type 'PFN_vkVoidFunction', and must
-- be cast to the type of the command being queried. The function pointer
-- /must/ only be called with a dispatchable object (the first parameter)
-- that is @device@ or a child of @device@.
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
-- 'PFN_vkVoidFunction', 'VkDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceProcAddr" vkGetDeviceProcAddr :: ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
#else
vkGetDeviceProcAddr :: DeviceCmds -> ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
vkGetDeviceProcAddr deviceCmds = mkVkGetDeviceProcAddr (pVkGetDeviceProcAddr deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
  :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#endif

type FN_vkGetDeviceProcAddr = ("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetDeviceProcAddr = FunPtr FN_vkGetDeviceProcAddr

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
-- 'vkGetInstanceProcAddr' itself is obtained in a platform- and loader-
-- specific manner. Typically, the loader library will export this command
-- as a function symbol, so applications /can/ link against the loader
-- library, or load it dynamically and look up the symbol using
-- platform-specific APIs.
--
-- The table below defines the various use cases for
-- 'vkGetInstanceProcAddr' and expected return value (“fp” is “function
-- pointer”) for each case.
--
-- The returned function pointer is of type 'PFN_vkVoidFunction', and must
-- be cast to the type of the command being queried.
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
-- > | @NULL@                | 'vkCreateInstance'    | fp                    |
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
--     child of @instance@, e.g. 'VkInstance', 'VkPhysicalDevice',
--     'VkDevice', 'Graphics.Vulkan.C.Core10.Queue.VkQueue', or
--     'Graphics.Vulkan.C.Core10.Queue.VkCommandBuffer'.
--
-- [2]
--     An “available device extension” is a device extension supported by
--     any physical device enumerated by @instance@.
--
-- == Valid Usage (Implicit)
--
-- -   If @instance@ is not @NULL@, @instance@ /must/ be a valid
--     'VkInstance' handle
--
-- -   @pName@ /must/ be a null-terminated UTF-8 string
--
-- = See Also
--
-- 'PFN_vkVoidFunction', 'VkInstance'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" vkGetInstanceProcAddr :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
#else
vkGetInstanceProcAddr :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
vkGetInstanceProcAddr deviceCmds = mkVkGetInstanceProcAddr (pVkGetInstanceProcAddr deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetInstanceProcAddr
  :: FunPtr (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
#endif
-- | A version of 'vkGetInstanceProcAddr' which can be called with a
-- null pointer for the instance.
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" vkGetInstanceProcAddr' :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type FN_vkGetInstanceProcAddr = ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction
type PFN_vkGetInstanceProcAddr = FunPtr FN_vkGetInstanceProcAddr

-- | vkGetPhysicalDeviceFeatures - Reports capabilities of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the physical device from which to query the
--     supported features.
--
-- -   @pFeatures@ is a pointer to a 'VkPhysicalDeviceFeatures' structure
--     in which the physical device features are returned. For each
--     feature, a value of 'Graphics.Vulkan.C.Core10.Core.VK_TRUE'
--     specifies that the feature is supported on this physical device, and
--     'Graphics.Vulkan.C.Core10.Core.VK_FALSE' specifies that the feature
--     is not supported.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkPhysicalDeviceFeatures'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFeatures" vkGetPhysicalDeviceFeatures :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
#else
vkGetPhysicalDeviceFeatures :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
vkGetPhysicalDeviceFeatures deviceCmds = mkVkGetPhysicalDeviceFeatures (pVkGetPhysicalDeviceFeatures deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ())
#endif

type FN_vkGetPhysicalDeviceFeatures = ("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()
type PFN_vkGetPhysicalDeviceFeatures = FunPtr FN_vkGetPhysicalDeviceFeatures

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
-- -   @pFormatProperties@ is a pointer to a 'VkFormatProperties' structure
--     in which physical device properties for @format@ are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat', 'VkFormatProperties',
-- 'VkPhysicalDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceFormatProperties" vkGetPhysicalDeviceFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
#else
vkGetPhysicalDeviceFormatProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
vkGetPhysicalDeviceFormatProperties deviceCmds = mkVkGetPhysicalDeviceFormatProperties (pVkGetPhysicalDeviceFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()
type PFN_vkGetPhysicalDeviceFormatProperties = FunPtr FN_vkGetPhysicalDeviceFormatProperties

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
-- -   @type@ is a 'VkImageType' value specifying the image type,
--     corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@imageType@.
--
-- -   @tiling@ is a 'VkImageTiling' value specifying the image tiling,
--     corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@tiling@.
--
-- -   @usage@ is a bitmask of 'VkImageUsageFlagBits' specifying the
--     intended usage of the image, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@usage@.
--
-- -   @flags@ is a bitmask of 'VkImageCreateFlagBits' specifying
--     additional parameters of the image, corresponding to
--     'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'::@flags@.
--
-- -   @pImageFormatProperties@ points to an instance of the
--     'VkImageFormatProperties' structure in which capabilities are
--     returned.
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
-- images, then 'vkGetPhysicalDeviceImageFormatProperties' returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED'.
--
-- The limitations on an image format that are reported by
-- 'vkGetPhysicalDeviceImageFormatProperties' have the following property:
-- if @usage1@ and @usage2@ of type 'VkImageUsageFlags' are such that the
-- bits set in @usage1@ are a subset of the bits set in @usage2@, and
-- @flags1@ and @flags2@ of type 'VkImageCreateFlags' are such that the
-- bits set in @flags1@ are a subset of the bits set in @flags2@, then the
-- limitations for @usage1@ and @flags1@ /must/ be no more strict than the
-- limitations for @usage2@ and @flags2@, for all values of @format@,
-- @type@, and @tiling@.
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
-- 'Graphics.Vulkan.C.Core10.Core.VkFormat', 'VkImageCreateFlags',
-- 'VkImageFormatProperties', 'VkImageTiling', 'VkImageType',
-- 'VkImageUsageFlags', 'VkPhysicalDevice'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceImageFormatProperties" vkGetPhysicalDeviceImageFormatProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
#else
vkGetPhysicalDeviceImageFormatProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
vkGetPhysicalDeviceImageFormatProperties deviceCmds = mkVkGetPhysicalDeviceImageFormatProperties (pVkGetPhysicalDeviceImageFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult)
#endif

type FN_vkGetPhysicalDeviceImageFormatProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult
type PFN_vkGetPhysicalDeviceImageFormatProperties = FunPtr FN_vkGetPhysicalDeviceImageFormatProperties

-- | vkGetPhysicalDeviceMemoryProperties - Reports memory information for the
-- specified physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the device to query.
--
-- -   @pMemoryProperties@ points to an instance of
--     'VkPhysicalDeviceMemoryProperties' structure in which the properties
--     are returned.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkPhysicalDeviceMemoryProperties'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceMemoryProperties" vkGetPhysicalDeviceMemoryProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
#else
vkGetPhysicalDeviceMemoryProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
vkGetPhysicalDeviceMemoryProperties deviceCmds = mkVkGetPhysicalDeviceMemoryProperties (pVkGetPhysicalDeviceMemoryProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceMemoryProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()
type PFN_vkGetPhysicalDeviceMemoryProperties = FunPtr FN_vkGetPhysicalDeviceMemoryProperties

-- | vkGetPhysicalDeviceProperties - Returns properties of a physical device
--
-- = Parameters
--
-- -   @physicalDevice@ is the handle to the physical device whose
--     properties will be queried.
--
-- -   @pProperties@ points to an instance of the
--     'VkPhysicalDeviceProperties' structure, that will be filled with
--     returned information.
--
-- == Valid Usage (Implicit)
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkPhysicalDeviceProperties'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceProperties" vkGetPhysicalDeviceProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
#else
vkGetPhysicalDeviceProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
vkGetPhysicalDeviceProperties deviceCmds = mkVkGetPhysicalDeviceProperties (pVkGetPhysicalDeviceProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()
type PFN_vkGetPhysicalDeviceProperties = FunPtr FN_vkGetPhysicalDeviceProperties

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
--     of 'VkQueueFamilyProperties' structures.
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
-- -   @physicalDevice@ /must/ be a valid 'VkPhysicalDevice' handle
--
-- -   @pQueueFamilyPropertyCount@ /must/ be a valid pointer to a
--     @uint32_t@ value
--
-- -   If the value referenced by @pQueueFamilyPropertyCount@ is not @0@,
--     and @pQueueFamilyProperties@ is not @NULL@, @pQueueFamilyProperties@
--     /must/ be a valid pointer to an array of @pQueueFamilyPropertyCount@
--     'VkQueueFamilyProperties' structures
--
-- = See Also
--
-- 'VkPhysicalDevice', 'VkQueueFamilyProperties'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetPhysicalDeviceQueueFamilyProperties" vkGetPhysicalDeviceQueueFamilyProperties :: ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
#else
vkGetPhysicalDeviceQueueFamilyProperties :: InstanceCmds -> ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
vkGetPhysicalDeviceQueueFamilyProperties deviceCmds = mkVkGetPhysicalDeviceQueueFamilyProperties (pVkGetPhysicalDeviceQueueFamilyProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ())
#endif

type FN_vkGetPhysicalDeviceQueueFamilyProperties = ("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()
type PFN_vkGetPhysicalDeviceQueueFamilyProperties = FunPtr FN_vkGetPhysicalDeviceQueueFamilyProperties
