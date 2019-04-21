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
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkAllocateCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkAllocateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.Memory.vkAllocateMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindBufferMemory2',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.vkBindImageMemory2',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkCreateBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkCreateBufferView',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkCreateCommandPool',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkCreateDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkCreateDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.C.Core10.Event.vkCreateEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkCreateFence',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Core10.Image.vkCreateImage',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkCreateImageView',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkCreatePipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Query.vkCreateQueryPool',
-- 'Graphics.Vulkan.C.Core10.Pass.vkCreateRenderPass',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkCreateSampler',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkCreateSamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkCreateSemaphore',
-- 'Graphics.Vulkan.C.Core10.Shader.vkCreateShaderModule',
-- 'Graphics.Vulkan.C.Core10.Buffer.vkDestroyBuffer',
-- 'Graphics.Vulkan.C.Core10.BufferView.vkDestroyBufferView',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkDestroyCommandPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkDestroyDescriptorSetLayout',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkDestroyDescriptorUpdateTemplate',
-- 'Graphics.Vulkan.C.Core10.Device.vkDestroyDevice',
-- 'Graphics.Vulkan.C.Core10.Event.vkDestroyEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkDestroyFence',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyFramebuffer',
-- 'Graphics.Vulkan.C.Core10.Image.vkDestroyImage',
-- 'Graphics.Vulkan.C.Core10.ImageView.vkDestroyImageView',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkDestroyPipeline',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkDestroyPipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.vkDestroyPipelineLayout',
-- 'Graphics.Vulkan.C.Core10.Query.vkDestroyQueryPool',
-- 'Graphics.Vulkan.C.Core10.Pass.vkDestroyRenderPass',
-- 'Graphics.Vulkan.C.Core10.Sampler.vkDestroySampler',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.vkDestroySamplerYcbcrConversion',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.vkDestroySemaphore',
-- 'Graphics.Vulkan.C.Core10.Shader.vkDestroyShaderModule',
-- 'Graphics.Vulkan.C.Core10.Queue.vkDeviceWaitIdle',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFlushMappedMemoryRanges',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkFreeCommandBuffers',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkFreeDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.Memory.vkFreeMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetBufferMemoryRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetBufferMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.vkGetDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.vkGetDeviceGroupPeerMemoryFeatures',
-- 'Graphics.Vulkan.C.Core10.Memory.vkGetDeviceMemoryCommitment',
-- 'vkGetDeviceProcAddr',
-- 'Graphics.Vulkan.C.Core10.Queue.vkGetDeviceQueue',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.vkGetDeviceQueue2',
-- 'Graphics.Vulkan.C.Core10.Event.vkGetEventStatus',
-- 'Graphics.Vulkan.C.Core10.Fence.vkGetFenceStatus',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkGetImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetImageSparseMemoryRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.vkGetImageSparseMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core10.Image.vkGetImageSubresourceLayout',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData',
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.Pass.vkGetRenderAreaGranularity',
-- 'Graphics.Vulkan.C.Core10.Memory.vkInvalidateMappedMemoryRanges',
-- 'Graphics.Vulkan.C.Core10.Memory.vkMapMemory',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkMergePipelineCaches',
-- 'Graphics.Vulkan.C.Core10.CommandPool.vkResetCommandPool',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkResetDescriptorPool',
-- 'Graphics.Vulkan.C.Core10.Event.vkResetEvent',
-- 'Graphics.Vulkan.C.Core10.Fence.vkResetFences',
-- 'Graphics.Vulkan.C.Core10.Event.vkSetEvent',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.vkTrimCommandPool',
-- 'Graphics.Vulkan.C.Core10.Memory.vkUnmapMemory',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.vkUpdateDescriptorSetWithTemplate',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.vkUpdateDescriptorSets',
-- 'Graphics.Vulkan.C.Core10.Fence.vkWaitForFences'
type VkDevice = Ptr VkDevice_T

-- | VkDeviceSize - Vulkan device memory size and offsets
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferCopy',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferImageCopy',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorBufferInfo',
-- 'VkImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo', 'VkMemoryHeap',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.VkMemoryRequirements',
-- 'VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryBind',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseImageMemoryRequirements',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkSparseMemoryBind',
-- 'Graphics.Vulkan.C.Core10.Image.VkSubresourceLayout',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindBufferMemory',
-- 'Graphics.Vulkan.C.Core10.MemoryManagement.vkBindImageMemory',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindIndexBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBindVertexBuffers',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDispatchIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndexedIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdDrawIndirect',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdFillBuffer',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdUpdateBuffer',
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
-- 'VkImageUsageFlagBits',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type VkImageUsageFlags = VkImageUsageFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkInstance_T
-- | VkInstance - Opaque handle to an instance object
--
-- = See Also
--
-- 'vkCreateInstance', 'vkDestroyInstance',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.vkEnumeratePhysicalDeviceGroups',
-- 'vkEnumeratePhysicalDevices', 'vkGetInstanceProcAddr'
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
-- 'Graphics.Vulkan.C.Core10.Device.vkCreateDevice',
-- 'Graphics.Vulkan.C.Core10.ExtensionDiscovery.vkEnumerateDeviceExtensionProperties',
-- 'Graphics.Vulkan.C.Core10.LayerDiscovery.vkEnumerateDeviceLayerProperties',
-- 'vkEnumeratePhysicalDevices',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.vkGetPhysicalDeviceExternalBufferProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.vkGetPhysicalDeviceExternalFenceProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.vkGetPhysicalDeviceExternalSemaphoreProperties',
-- 'vkGetPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFeatures2',
-- 'vkGetPhysicalDeviceFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2',
-- 'vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceImageFormatProperties2',
-- 'vkGetPhysicalDeviceMemoryProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceMemoryProperties2',
-- 'vkGetPhysicalDeviceProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceProperties2',
-- 'vkGetPhysicalDeviceQueueFamilyProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceQueueFamilyProperties2',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceSparseImageFormatProperties2'
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
