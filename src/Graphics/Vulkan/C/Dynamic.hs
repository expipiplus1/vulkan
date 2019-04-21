{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Dynamic
  ( initInstanceCmds
  , initDeviceCmds
  , DeviceCmds(..)
  , InstanceCmds(..)
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CInt(..)
  , CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , castPtrToFunPtr
  )
import qualified GHC.Ptr
  ( Ptr(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateInfo
  , FN_vkCreateBuffer
  , FN_vkDestroyBuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateInfo
  , FN_vkCreateBufferView
  , FN_vkDestroyBufferView
  , VkBufferView
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo
  , VkCommandBufferBeginInfo
  , FN_vkAllocateCommandBuffers
  , FN_vkBeginCommandBuffer
  , FN_vkEndCommandBuffer
  , FN_vkFreeCommandBuffers
  , FN_vkResetCommandBuffer
  , VkCommandBufferResetFlags
  , VkQueryControlFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkBufferCopy
  , VkBufferImageCopy
  , VkBufferMemoryBarrier
  , VkClearAttachment
  , VkClearColorValue
  , VkClearDepthStencilValue
  , VkClearRect
  , VkImageBlit
  , VkImageCopy
  , VkImageMemoryBarrier
  , VkImageResolve
  , VkIndexType
  , VkMemoryBarrier
  , VkRenderPassBeginInfo
  , VkSubpassContents
  , FN_vkCmdBeginQuery
  , FN_vkCmdBeginRenderPass
  , FN_vkCmdBindDescriptorSets
  , FN_vkCmdBindIndexBuffer
  , FN_vkCmdBindPipeline
  , FN_vkCmdBindVertexBuffers
  , FN_vkCmdBlitImage
  , FN_vkCmdClearAttachments
  , FN_vkCmdClearColorImage
  , FN_vkCmdClearDepthStencilImage
  , FN_vkCmdCopyBuffer
  , FN_vkCmdCopyBufferToImage
  , FN_vkCmdCopyImage
  , FN_vkCmdCopyImageToBuffer
  , FN_vkCmdCopyQueryPoolResults
  , FN_vkCmdDispatch
  , FN_vkCmdDispatchIndirect
  , FN_vkCmdDraw
  , FN_vkCmdDrawIndexed
  , FN_vkCmdDrawIndexedIndirect
  , FN_vkCmdDrawIndirect
  , FN_vkCmdEndQuery
  , FN_vkCmdEndRenderPass
  , FN_vkCmdExecuteCommands
  , FN_vkCmdFillBuffer
  , FN_vkCmdNextSubpass
  , FN_vkCmdPipelineBarrier
  , FN_vkCmdPushConstants
  , FN_vkCmdResetEvent
  , FN_vkCmdResetQueryPool
  , FN_vkCmdResolveImage
  , FN_vkCmdSetBlendConstants
  , FN_vkCmdSetDepthBias
  , FN_vkCmdSetDepthBounds
  , FN_vkCmdSetEvent
  , FN_vkCmdSetLineWidth
  , FN_vkCmdSetScissor
  , FN_vkCmdSetStencilCompareMask
  , FN_vkCmdSetStencilReference
  , FN_vkCmdSetStencilWriteMask
  , FN_vkCmdSetViewport
  , FN_vkCmdUpdateBuffer
  , FN_vkCmdWaitEvents
  , FN_vkCmdWriteTimestamp
  , VkStencilFaceFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateInfo
  , FN_vkCreateCommandPool
  , FN_vkDestroyCommandPool
  , FN_vkResetCommandPool
  , VkCommandPool
  , VkCommandPoolResetFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet
  , VkDescriptorPoolCreateInfo
  , VkDescriptorPoolResetFlags
  , VkDescriptorSetAllocateInfo
  , VkDescriptorSetLayoutCreateInfo
  , VkWriteDescriptorSet
  , FN_vkAllocateDescriptorSets
  , FN_vkCreateDescriptorPool
  , FN_vkCreateDescriptorSetLayout
  , FN_vkDestroyDescriptorPool
  , FN_vkDestroyDescriptorSetLayout
  , FN_vkFreeDescriptorSets
  , FN_vkResetDescriptorPool
  , FN_vkUpdateDescriptorSets
  , VkDescriptorPool
  , VkDescriptorSet
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateInfo
  , FN_vkCreateDevice
  , FN_vkDestroyDevice
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkFormatProperties
  , VkImageFormatProperties
  , VkImageTiling
  , VkImageType
  , VkPhysicalDeviceFeatures
  , VkPhysicalDeviceMemoryProperties
  , VkPhysicalDeviceProperties
  , VkQueueFamilyProperties
  , VkSampleCountFlagBits
  , FN_vkDestroyInstance
  , FN_vkEnumeratePhysicalDevices
  , FN_vkGetDeviceProcAddr
  , FN_vkGetInstanceProcAddr
  , FN_vkGetPhysicalDeviceFeatures
  , FN_vkGetPhysicalDeviceFormatProperties
  , FN_vkGetPhysicalDeviceImageFormatProperties
  , FN_vkGetPhysicalDeviceMemoryProperties
  , FN_vkGetPhysicalDeviceProperties
  , FN_vkGetPhysicalDeviceQueueFamilyProperties
  , PFN_vkVoidFunction
  , VkDevice
  , VkDeviceSize
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkInstance
  , VkPhysicalDevice
  , vkGetInstanceProcAddr
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateInfo
  , FN_vkCreateEvent
  , FN_vkDestroyEvent
  , FN_vkGetEventStatus
  , FN_vkResetEvent
  , FN_vkSetEvent
  , VkEvent
  )
import {-# source #-} Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties
  , FN_vkEnumerateDeviceExtensionProperties
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateInfo
  , FN_vkCreateFence
  , FN_vkDestroyFence
  , FN_vkGetFenceStatus
  , FN_vkResetFences
  , FN_vkWaitForFences
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo
  , VkImageLayout
  , VkSubresourceLayout
  , FN_vkCreateImage
  , FN_vkDestroyImage
  , FN_vkGetImageSubresourceLayout
  )
import {-# source #-} Graphics.Vulkan.C.Core10.ImageView
  ( VkImageSubresourceRange
  , VkImageViewCreateInfo
  , FN_vkCreateImageView
  , FN_vkDestroyImageView
  , VkImageView
  )
import {-# source #-} Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties
  , FN_vkEnumerateDeviceLayerProperties
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange
  , VkMemoryAllocateInfo
  , VkMemoryMapFlags
  , FN_vkAllocateMemory
  , FN_vkFlushMappedMemoryRanges
  , FN_vkFreeMemory
  , FN_vkGetDeviceMemoryCommitment
  , FN_vkInvalidateMappedMemoryRanges
  , FN_vkMapMemory
  , FN_vkUnmapMemory
  , VkDeviceMemory
  )
import {-# source #-} Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements
  , FN_vkBindBufferMemory
  , FN_vkBindImageMemory
  , FN_vkGetBufferMemoryRequirements
  , FN_vkGetImageMemoryRequirements
  , VkBuffer
  , VkImage
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pass
  ( VkFramebufferCreateInfo
  , VkPipelineBindPoint
  , VkRenderPassCreateInfo
  , FN_vkCreateFramebuffer
  , FN_vkCreateRenderPass
  , FN_vkDestroyFramebuffer
  , FN_vkDestroyRenderPass
  , FN_vkGetRenderAreaGranularity
  , VkDependencyFlags
  , VkFramebuffer
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Pipeline
  ( VkComputePipelineCreateInfo
  , VkExtent2D
  , VkGraphicsPipelineCreateInfo
  , VkRect2D
  , VkShaderStageFlagBits
  , VkViewport
  , FN_vkCreateComputePipelines
  , FN_vkCreateGraphicsPipelines
  , FN_vkDestroyPipeline
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  )
import {-# source #-} Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateInfo
  , FN_vkCreatePipelineCache
  , FN_vkDestroyPipelineCache
  , FN_vkGetPipelineCacheData
  , FN_vkMergePipelineCaches
  , VkPipelineCache
  )
import {-# source #-} Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateInfo
  , FN_vkCreatePipelineLayout
  , FN_vkDestroyPipelineLayout
  , VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Query
  ( VkQueryPoolCreateInfo
  , VkQueryType
  , FN_vkCreateQueryPool
  , FN_vkDestroyQueryPool
  , FN_vkGetQueryPoolResults
  , VkQueryPool
  , VkQueryResultFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits
  , VkSubmitInfo
  , FN_vkDeviceWaitIdle
  , FN_vkGetDeviceQueue
  , FN_vkQueueSubmit
  , FN_vkQueueWaitIdle
  , VkCommandBuffer
  , VkFence
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  )
import {-# source #-} Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateInfo
  , FN_vkCreateSemaphore
  , FN_vkDestroySemaphore
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter
  , VkSamplerCreateInfo
  , FN_vkCreateSampler
  , FN_vkDestroySampler
  , VkSampler
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateInfo
  , FN_vkCreateShaderModule
  , FN_vkDestroyShaderModule
  , VkShaderModule
  )
import {-# source #-} Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo
  , VkImageSubresource
  , VkSparseImageFormatProperties
  , VkSparseImageMemoryRequirements
  , FN_vkGetImageSparseMemoryRequirements
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties
  , FN_vkQueueBindSparse
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2
  , FN_vkGetDeviceQueue2
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo
  , VkBindImageMemoryInfo
  , FN_vkBindBufferMemory2
  , FN_vkBindImageMemory2
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateInfo
  , FN_vkCreateDescriptorUpdateTemplate
  , FN_vkDestroyDescriptorUpdateTemplate
  , FN_vkUpdateDescriptorSetWithTemplate
  , VkDescriptorUpdateTemplate
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( FN_vkCmdDispatchBase
  , FN_vkCmdSetDeviceMask
  , FN_vkGetDeviceGroupPeerMemoryFeatures
  , VkPeerMemoryFeatureFlags
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkPhysicalDeviceGroupProperties
  , FN_vkEnumeratePhysicalDeviceGroups
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceProperties
  , VkPhysicalDeviceExternalFenceInfo
  , FN_vkGetPhysicalDeviceExternalFenceProperties
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties
  , VkExternalMemoryHandleTypeFlagBits
  , VkPhysicalDeviceExternalBufferInfo
  , FN_vkGetPhysicalDeviceExternalBufferProperties
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreProperties
  , VkPhysicalDeviceExternalSemaphoreInfo
  , FN_vkGetPhysicalDeviceExternalSemaphoreProperties
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2
  , VkImageMemoryRequirementsInfo2
  , VkImageSparseMemoryRequirementsInfo2
  , VkMemoryRequirements2
  , VkSparseImageMemoryRequirements2
  , FN_vkGetBufferMemoryRequirements2
  , FN_vkGetImageMemoryRequirements2
  , FN_vkGetImageSparseMemoryRequirements2
  , VkMemoryRequirements2KHR
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2
  , VkImageFormatProperties2
  , VkPhysicalDeviceFeatures2
  , VkPhysicalDeviceImageFormatInfo2
  , VkPhysicalDeviceMemoryProperties2
  , VkPhysicalDeviceProperties2
  , VkPhysicalDeviceSparseImageFormatInfo2
  , VkQueueFamilyProperties2
  , VkSparseImageFormatProperties2
  , FN_vkGetPhysicalDeviceFeatures2
  , FN_vkGetPhysicalDeviceFormatProperties2
  , FN_vkGetPhysicalDeviceImageFormatProperties2
  , FN_vkGetPhysicalDeviceMemoryProperties2
  , FN_vkGetPhysicalDeviceProperties2
  , FN_vkGetPhysicalDeviceQueueFamilyProperties2
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties2
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags
  , FN_vkTrimCommandPool
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport
  , FN_vkGetDescriptorSetLayoutSupport
  )
import {-# source #-} Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkSamplerYcbcrConversionCreateInfo
  , FN_vkCreateSamplerYcbcrConversion
  , FN_vkDestroySamplerYcbcrConversion
  , VkSamplerYcbcrConversion
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( FN_vkCmdWriteBufferMarkerAMD
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( FN_vkSetLocalDimmingAMD
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD
  , FN_vkGetShaderInfoAMD
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( FN_vkGetAndroidHardwareBufferPropertiesANDROID
  , FN_vkGetMemoryAndroidHardwareBufferANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferPropertiesANDROID
  , VkMemoryGetAndroidHardwareBufferInfoANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AHardwareBuffer
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( FN_vkAcquireXlibDisplayEXT
  , FN_vkGetRandROutputDisplayEXT
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressInfoEXT
  , FN_vkGetBufferDeviceAddressEXT
  , VkDeviceAddress
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT
  , VkTimeDomainEXT
  , FN_vkGetCalibratedTimestampsEXT
  , FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkConditionalRenderingBeginInfoEXT
  , FN_vkCmdBeginConditionalRenderingEXT
  , FN_vkCmdEndConditionalRenderingEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT
  , VkDebugMarkerObjectNameInfoEXT
  , VkDebugMarkerObjectTagInfoEXT
  , FN_vkCmdDebugMarkerBeginEXT
  , FN_vkCmdDebugMarkerEndEXT
  , FN_vkCmdDebugMarkerInsertEXT
  , FN_vkDebugMarkerSetObjectNameEXT
  , FN_vkDebugMarkerSetObjectTagEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT
  , VkDebugReportObjectTypeEXT
  , FN_vkCreateDebugReportCallbackEXT
  , FN_vkDebugReportMessageEXT
  , FN_vkDestroyDebugReportCallbackEXT
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagsEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT
  , VkDebugUtilsMessageSeverityFlagBitsEXT
  , VkDebugUtilsMessengerCallbackDataEXT
  , VkDebugUtilsMessengerCreateInfoEXT
  , VkDebugUtilsObjectNameInfoEXT
  , VkDebugUtilsObjectTagInfoEXT
  , FN_vkCmdBeginDebugUtilsLabelEXT
  , FN_vkCmdEndDebugUtilsLabelEXT
  , FN_vkCmdInsertDebugUtilsLabelEXT
  , FN_vkCreateDebugUtilsMessengerEXT
  , FN_vkDestroyDebugUtilsMessengerEXT
  , FN_vkQueueBeginDebugUtilsLabelEXT
  , FN_vkQueueEndDebugUtilsLabelEXT
  , FN_vkQueueInsertDebugUtilsLabelEXT
  , FN_vkSetDebugUtilsObjectNameEXT
  , FN_vkSetDebugUtilsObjectTagEXT
  , FN_vkSubmitDebugUtilsMessageEXT
  , VkDebugUtilsMessageTypeFlagsEXT
  , VkDebugUtilsMessengerEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( FN_vkReleaseDisplayEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( FN_vkCmdSetDiscardRectangleEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT
  , VkDisplayEventInfoEXT
  , VkDisplayPowerInfoEXT
  , FN_vkDisplayPowerControlEXT
  , FN_vkGetSwapchainCounterEXT
  , FN_vkRegisterDeviceEventEXT
  , FN_vkRegisterDisplayEventEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT
  , VkSurfaceCounterFlagBitsEXT
  , FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkMemoryHostPointerPropertiesEXT
  , FN_vkGetMemoryHostPointerPropertiesEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( FN_vkAcquireFullScreenExclusiveModeEXT
  , FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , FN_vkReleaseFullScreenExclusiveModeEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT
  , FN_vkSetHdrMetadataEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateInfoEXT
  , FN_vkCreateHeadlessSurfaceEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( FN_vkResetQueryPoolEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkImageDrmFormatModifierPropertiesEXT
  , FN_vkGetImageDrmFormatModifierPropertiesEXT
  )

#if defined(VK_USE_PLATFORM_METAL_EXT)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( FN_vkCreateMetalSurfaceEXT
  )
#endif

#if defined(VK_USE_PLATFORM_METAL_EXT)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateInfoEXT
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkMultisamplePropertiesEXT
  , VkSampleLocationsInfoEXT
  , FN_vkCmdSetSampleLocationsEXT
  , FN_vkGetPhysicalDeviceMultisamplePropertiesEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( FN_vkCmdBeginQueryIndexedEXT
  , FN_vkCmdBeginTransformFeedbackEXT
  , FN_vkCmdBindTransformFeedbackBuffersEXT
  , FN_vkCmdDrawIndirectByteCountEXT
  , FN_vkCmdEndQueryIndexedEXT
  , FN_vkCmdEndTransformFeedbackEXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateInfoEXT
  , FN_vkCreateValidationCacheEXT
  , FN_vkDestroyValidationCacheEXT
  , FN_vkGetValidationCacheDataEXT
  , FN_vkMergeValidationCachesEXT
  , VkValidationCacheEXT
  )

#if defined(VK_USE_PLATFORM_FUCHSIA)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( FN_vkCreateImagePipeSurfaceFUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_FUCHSIA)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateInfoFUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( FN_vkCreateStreamDescriptorSurfaceGGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateInfoGGP
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE
  , VkRefreshCycleDurationGOOGLE
  , FN_vkGetPastPresentationTimingGOOGLE
  , FN_vkGetRefreshCycleDurationGOOGLE
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( FN_vkCreateAndroidSurfaceKHR
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkRenderPassCreateInfo2KHR
  , VkSubpassBeginInfoKHR
  , VkSubpassEndInfoKHR
  , FN_vkCmdBeginRenderPass2KHR
  , FN_vkCmdEndRenderPass2KHR
  , FN_vkCmdNextSubpass2KHR
  , FN_vkCreateRenderPass2KHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( FN_vkGetDeviceGroupSurfacePresentModes2EXT
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateInfoKHR
  , VkDisplayModePropertiesKHR
  , VkDisplayPlaneCapabilitiesKHR
  , VkDisplayPlanePropertiesKHR
  , VkDisplayPropertiesKHR
  , VkDisplaySurfaceCreateInfoKHR
  , FN_vkCreateDisplayModeKHR
  , FN_vkCreateDisplayPlaneSurfaceKHR
  , FN_vkGetDisplayModePropertiesKHR
  , FN_vkGetDisplayPlaneCapabilitiesKHR
  , FN_vkGetDisplayPlaneSupportedDisplaysKHR
  , FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , FN_vkGetPhysicalDeviceDisplayPropertiesKHR
  , VkDisplayKHR
  , VkDisplayModeKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( FN_vkCreateSharedSwapchainsKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( FN_vkCmdDrawIndexedIndirectCountKHR
  , FN_vkCmdDrawIndirectCountKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR
  , VkImportFenceFdInfoKHR
  , FN_vkGetFenceFdKHR
  , FN_vkImportFenceFdKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( FN_vkGetFenceWin32HandleKHR
  , FN_vkImportFenceWin32HandleKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkFenceGetWin32HandleInfoKHR
  , VkImportFenceWin32HandleInfoKHR
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkMemoryFdPropertiesKHR
  , VkMemoryGetFdInfoKHR
  , FN_vkGetMemoryFdKHR
  , FN_vkGetMemoryFdPropertiesKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( FN_vkGetMemoryWin32HandleKHR
  , FN_vkGetMemoryWin32HandlePropertiesKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkMemoryGetWin32HandleInfoKHR
  , VkMemoryWin32HandlePropertiesKHR
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR
  , VkSemaphoreGetFdInfoKHR
  , FN_vkGetSemaphoreFdKHR
  , FN_vkImportSemaphoreFdKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( FN_vkGetSemaphoreWin32HandleKHR
  , FN_vkImportSemaphoreWin32HandleKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkImportSemaphoreWin32HandleInfoKHR
  , VkSemaphoreGetWin32HandleInfoKHR
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR
  , VkDisplayPlaneCapabilities2KHR
  , VkDisplayPlaneInfo2KHR
  , VkDisplayPlaneProperties2KHR
  , VkDisplayProperties2KHR
  , FN_vkGetDisplayModeProperties2KHR
  , FN_vkGetDisplayPlaneCapabilities2KHR
  , FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , FN_vkGetPhysicalDeviceDisplayProperties2KHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR
  , VkSurfaceCapabilities2KHR
  , VkSurfaceFormat2KHR
  , FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR
  , FN_vkGetPhysicalDeviceSurfaceFormats2KHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( FN_vkCmdPushDescriptorSetKHR
  , FN_vkCmdPushDescriptorSetWithTemplateKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( FN_vkGetSwapchainStatusKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkPresentModeKHR
  , VkSurfaceCapabilitiesKHR
  , VkSurfaceFormatKHR
  , FN_vkDestroySurfaceKHR
  , FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , FN_vkGetPhysicalDeviceSurfaceFormatsKHR
  , FN_vkGetPhysicalDeviceSurfacePresentModesKHR
  , FN_vkGetPhysicalDeviceSurfaceSupportKHR
  , VkSurfaceKHR
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR
  , VkDeviceGroupPresentCapabilitiesKHR
  , VkPresentInfoKHR
  , VkSwapchainCreateInfoKHR
  , FN_vkAcquireNextImage2KHR
  , FN_vkAcquireNextImageKHR
  , FN_vkCreateSwapchainKHR
  , FN_vkDestroySwapchainKHR
  , FN_vkGetDeviceGroupPresentCapabilitiesKHR
  , FN_vkGetDeviceGroupSurfacePresentModesKHR
  , FN_vkGetPhysicalDevicePresentRectanglesKHR
  , FN_vkGetSwapchainImagesKHR
  , FN_vkQueuePresentKHR
  , VkDeviceGroupPresentModeFlagsKHR
  , VkSwapchainKHR
  )

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( FN_vkCreateWaylandSurfaceKHR
  , FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR
  , Wl_display
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( FN_vkCreateWin32SurfaceKHR
  , FN_vkGetPhysicalDeviceWin32PresentationSupportKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( FN_vkCreateXcbSurfaceKHR
  , FN_vkGetPhysicalDeviceXcbPresentationSupportKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR
  , Xcb_connection_t
  , Xcb_visualid_t
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( FN_vkCreateXlibSurfaceKHR
  , FN_vkGetPhysicalDeviceXlibPresentationSupportKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( VkXlibSurfaceCreateInfoKHR
  , VisualID
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR) || defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( FN_vkCreateIOSSurfaceMVK
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( FN_vkCreateMacOSSurfaceMVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( FN_vkCreateViSurfaceNN
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX
  , VkCmdReserveSpaceForCommandsInfoNVX
  , VkDeviceGeneratedCommandsFeaturesNVX
  , VkDeviceGeneratedCommandsLimitsNVX
  , VkIndirectCommandsLayoutCreateInfoNVX
  , VkObjectEntryTypeNVX
  , VkObjectTableCreateInfoNVX
  , VkObjectTableEntryNVX
  , FN_vkCmdProcessCommandsNVX
  , FN_vkCmdReserveSpaceForCommandsNVX
  , FN_vkCreateIndirectCommandsLayoutNVX
  , FN_vkCreateObjectTableNVX
  , FN_vkDestroyIndirectCommandsLayoutNVX
  , FN_vkDestroyObjectTableNVX
  , FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , FN_vkRegisterObjectsNVX
  , FN_vkUnregisterObjectsNVX
  , VkIndirectCommandsLayoutNVX
  , VkObjectTableNVX
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX
  , FN_vkGetImageViewHandleNVX
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkViewportWScalingNV
  , FN_vkCmdSetViewportWScalingNV
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkCooperativeMatrixPropertiesNV
  , FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV
  , FN_vkCmdSetCheckpointNV
  , FN_vkGetQueueCheckpointDataNV
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV
  , FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  , VkExternalMemoryHandleTypeFlagsNV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( FN_vkGetMemoryWin32HandleNV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  )
#endif
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( FN_vkCmdDrawMeshTasksIndirectCountNV
  , FN_vkCmdDrawMeshTasksIndirectNV
  , FN_vkCmdDrawMeshTasksNV
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV
  , VkAccelerationStructureInfoNV
  , VkAccelerationStructureMemoryRequirementsInfoNV
  , VkBindAccelerationStructureMemoryInfoNV
  , VkCopyAccelerationStructureModeNV
  , VkRayTracingPipelineCreateInfoNV
  , FN_vkBindAccelerationStructureMemoryNV
  , FN_vkCmdBuildAccelerationStructureNV
  , FN_vkCmdCopyAccelerationStructureNV
  , FN_vkCmdTraceRaysNV
  , FN_vkCmdWriteAccelerationStructuresPropertiesNV
  , FN_vkCompileDeferredNV
  , FN_vkCreateAccelerationStructureNV
  , FN_vkCreateRayTracingPipelinesNV
  , FN_vkDestroyAccelerationStructureNV
  , FN_vkGetAccelerationStructureHandleNV
  , FN_vkGetAccelerationStructureMemoryRequirementsNV
  , FN_vkGetRayTracingShaderGroupHandlesNV
  , VkAccelerationStructureNV
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( FN_vkCmdSetExclusiveScissorNV
  )
import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleOrderCustomNV
  , VkCoarseSampleOrderTypeNV
  , VkShadingRatePaletteNV
  , FN_vkCmdBindShadingRateImageNV
  , FN_vkCmdSetCoarseSampleOrderNV
  , FN_vkCmdSetViewportShadingRatePaletteNV
  )


data DeviceCmds = DeviceCmds
  { deviceCmdsHandle :: VkDevice
  , pVkGetDeviceProcAddr :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
  , pVkDestroyDevice :: FunPtr (("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetDeviceQueue :: FunPtr (("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
  , pVkQueueSubmit :: FunPtr (("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult)
  , pVkQueueWaitIdle :: FunPtr (("queue" ::: VkQueue) -> IO VkResult)
  , pVkDeviceWaitIdle :: FunPtr (("device" ::: VkDevice) -> IO VkResult)
  , pVkAllocateMemory :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult)
  , pVkFreeMemory :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkMapMemory :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult)
  , pVkUnmapMemory :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ())
  , pVkFlushMappedMemoryRanges :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
  , pVkInvalidateMappedMemoryRanges :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
  , pVkGetDeviceMemoryCommitment :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ())
  , pVkGetBufferMemoryRequirements :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ())
  , pVkBindBufferMemory :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult)
  , pVkGetImageMemoryRequirements :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ())
  , pVkBindImageMemory :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult)
  , pVkGetImageSparseMemoryRequirements :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ())
  , pVkQueueBindSparse :: FunPtr (("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult)
  , pVkCreateFence :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
  , pVkDestroyFence :: FunPtr (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkResetFences :: FunPtr (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult)
  , pVkGetFenceStatus :: FunPtr (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult)
  , pVkWaitForFences :: FunPtr (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult)
  , pVkCreateSemaphore :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult)
  , pVkDestroySemaphore :: FunPtr (("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateEvent :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult)
  , pVkDestroyEvent :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetEventStatus :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
  , pVkSetEvent :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
  , pVkResetEvent :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
  , pVkCreateQueryPool :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult)
  , pVkDestroyQueryPool :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetQueryPoolResults :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult)
  , pVkResetQueryPoolEXT :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
  , pVkCreateBuffer :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult)
  , pVkDestroyBuffer :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateBufferView :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult)
  , pVkDestroyBufferView :: FunPtr (("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateImage :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult)
  , pVkDestroyImage :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetImageSubresourceLayout :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ())
  , pVkCreateImageView :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult)
  , pVkDestroyImageView :: FunPtr (("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateShaderModule :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult)
  , pVkDestroyShaderModule :: FunPtr (("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreatePipelineCache :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult)
  , pVkDestroyPipelineCache :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetPipelineCacheData :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
  , pVkMergePipelineCaches :: FunPtr (("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult)
  , pVkCreateGraphicsPipelines :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
  , pVkCreateComputePipelines :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
  , pVkDestroyPipeline :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreatePipelineLayout :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult)
  , pVkDestroyPipelineLayout :: FunPtr (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateSampler :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult)
  , pVkDestroySampler :: FunPtr (("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateDescriptorSetLayout :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult)
  , pVkDestroyDescriptorSetLayout :: FunPtr (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateDescriptorPool :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult)
  , pVkDestroyDescriptorPool :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkResetDescriptorPool :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult)
  , pVkAllocateDescriptorSets :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
  , pVkFreeDescriptorSets :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
  , pVkUpdateDescriptorSets :: FunPtr (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ())
  , pVkCreateFramebuffer :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult)
  , pVkDestroyFramebuffer :: FunPtr (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateRenderPass :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
  , pVkDestroyRenderPass :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetRenderAreaGranularity :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ())
  , pVkCreateCommandPool :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult)
  , pVkDestroyCommandPool :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkResetCommandPool :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult)
  , pVkAllocateCommandBuffers :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult)
  , pVkFreeCommandBuffers :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
  , pVkBeginCommandBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult)
  , pVkEndCommandBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO VkResult)
  , pVkResetCommandBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult)
  , pVkCmdBindPipeline :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ())
  , pVkCmdSetViewport :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ())
  , pVkCmdSetScissor :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ())
  , pVkCmdSetLineWidth :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ())
  , pVkCmdSetDepthBias :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ())
  , pVkCmdSetBlendConstants :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ())
  , pVkCmdSetDepthBounds :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ())
  , pVkCmdSetStencilCompareMask :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ())
  , pVkCmdSetStencilWriteMask :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ())
  , pVkCmdSetStencilReference :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ())
  , pVkCmdBindDescriptorSets :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ())
  , pVkCmdBindIndexBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ())
  , pVkCmdBindVertexBuffers :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ())
  , pVkCmdDraw :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ())
  , pVkCmdDrawIndexed :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ())
  , pVkCmdDrawIndirect :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawIndexedIndirect :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDispatch :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
  , pVkCmdDispatchIndirect :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ())
  , pVkCmdCopyBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ())
  , pVkCmdCopyImage :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ())
  , pVkCmdBlitImage :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ())
  , pVkCmdCopyBufferToImage :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ())
  , pVkCmdCopyImageToBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ())
  , pVkCmdUpdateBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ())
  , pVkCmdFillBuffer :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ())
  , pVkCmdClearColorImage :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ())
  , pVkCmdClearDepthStencilImage :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ())
  , pVkCmdClearAttachments :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ())
  , pVkCmdResolveImage :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ())
  , pVkCmdSetEvent :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ())
  , pVkCmdResetEvent :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ())
  , pVkCmdWaitEvents :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ())
  , pVkCmdPipelineBarrier :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ())
  , pVkCmdBeginQuery :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ())
  , pVkCmdEndQuery :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ())
  , pVkCmdBeginConditionalRenderingEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ())
  , pVkCmdEndConditionalRenderingEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ())
  , pVkCmdResetQueryPool :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
  , pVkCmdWriteTimestamp :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ())
  , pVkCmdCopyQueryPoolResults :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ())
  , pVkCmdPushConstants :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ())
  , pVkCmdBeginRenderPass :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ())
  , pVkCmdNextSubpass :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ())
  , pVkCmdEndRenderPass :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ())
  , pVkCmdExecuteCommands :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
  , pVkCreateSharedSwapchainsKHR :: FunPtr (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult)
  , pVkCreateSwapchainKHR :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult)
  , pVkDestroySwapchainKHR :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetSwapchainImagesKHR :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult)
  , pVkAcquireNextImageKHR :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult)
  , pVkQueuePresentKHR :: FunPtr (("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult)
  , pVkDebugMarkerSetObjectNameEXT :: FunPtr (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult)
  , pVkDebugMarkerSetObjectTagEXT :: FunPtr (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult)
  , pVkCmdDebugMarkerBeginEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
  , pVkCmdDebugMarkerEndEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ())
  , pVkCmdDebugMarkerInsertEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkGetMemoryWin32HandleNV :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
#endif
  , pVkCmdProcessCommandsNVX :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ())
  , pVkCmdReserveSpaceForCommandsNVX :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ())
  , pVkCreateIndirectCommandsLayoutNVX :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult)
  , pVkDestroyIndirectCommandsLayoutNVX :: FunPtr (("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkCreateObjectTableNVX :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult)
  , pVkDestroyObjectTableNVX :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkRegisterObjectsNVX :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
  , pVkUnregisterObjectsNVX :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
  , pVkCmdPushDescriptorSetKHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ())
  , pVkTrimCommandPool :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ())
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkGetMemoryWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
  , pVkGetMemoryWin32HandlePropertiesKHR :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult)
#endif
  , pVkGetMemoryFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
  , pVkGetMemoryFdPropertiesKHR :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult)
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkGetSemaphoreWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
  , pVkImportSemaphoreWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult)
#endif
  , pVkGetSemaphoreFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
  , pVkImportSemaphoreFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult)
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkGetFenceWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
  , pVkImportFenceWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult)
#endif
  , pVkGetFenceFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
  , pVkImportFenceFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult)
  , pVkDisplayPowerControlEXT :: FunPtr (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult)
  , pVkRegisterDeviceEventEXT :: FunPtr (("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
  , pVkRegisterDisplayEventEXT :: FunPtr (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
  , pVkGetSwapchainCounterEXT :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult)
  , pVkGetDeviceGroupPeerMemoryFeatures :: FunPtr (("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ())
  , pVkBindBufferMemory2 :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult)
  , pVkBindImageMemory2 :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult)
  , pVkCmdSetDeviceMask :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ())
  , pVkGetDeviceGroupPresentCapabilitiesKHR :: FunPtr (("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult)
  , pVkGetDeviceGroupSurfacePresentModesKHR :: FunPtr (("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
  , pVkAcquireNextImage2KHR :: FunPtr (("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult)
  , pVkCmdDispatchBase :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
  , pVkCreateDescriptorUpdateTemplate :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult)
  , pVkDestroyDescriptorUpdateTemplate :: FunPtr (("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkUpdateDescriptorSetWithTemplate :: FunPtr (("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ())
  , pVkCmdPushDescriptorSetWithTemplateKHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ())
  , pVkSetHdrMetadataEXT :: FunPtr (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ())
  , pVkGetSwapchainStatusKHR :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
  , pVkGetRefreshCycleDurationGOOGLE :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult)
  , pVkGetPastPresentationTimingGOOGLE :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult)
  , pVkCmdSetViewportWScalingNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ())
  , pVkCmdSetDiscardRectangleEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr VkRect2D) -> IO ())
  , pVkCmdSetSampleLocationsEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ())
  , pVkGetBufferMemoryRequirements2 :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
  , pVkGetImageMemoryRequirements2 :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
  , pVkGetImageSparseMemoryRequirements2 :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ())
  , pVkCreateSamplerYcbcrConversion :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult)
  , pVkDestroySamplerYcbcrConversion :: FunPtr (("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetDeviceQueue2 :: FunPtr (("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
  , pVkCreateValidationCacheEXT :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
  , pVkDestroyValidationCacheEXT :: FunPtr (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetValidationCacheDataEXT :: FunPtr (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
  , pVkMergeValidationCachesEXT :: FunPtr (("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
  , pVkGetDescriptorSetLayoutSupport :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ())
  , pVkGetShaderInfoAMD :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult)
  , pVkSetLocalDimmingAMD :: FunPtr (("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ())
  , pVkGetCalibratedTimestampsEXT :: FunPtr (("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult)
  , pVkSetDebugUtilsObjectNameEXT :: FunPtr (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult)
  , pVkSetDebugUtilsObjectTagEXT :: FunPtr (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult)
  , pVkQueueBeginDebugUtilsLabelEXT :: FunPtr (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
  , pVkQueueEndDebugUtilsLabelEXT :: FunPtr (("queue" ::: VkQueue) -> IO ())
  , pVkQueueInsertDebugUtilsLabelEXT :: FunPtr (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
  , pVkCmdBeginDebugUtilsLabelEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
  , pVkCmdEndDebugUtilsLabelEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ())
  , pVkCmdInsertDebugUtilsLabelEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
  , pVkGetMemoryHostPointerPropertiesEXT :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult)
  , pVkCmdWriteBufferMarkerAMD :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ())
  , pVkCreateRenderPass2KHR :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
  , pVkCmdBeginRenderPass2KHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ())
  , pVkCmdNextSubpass2KHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
  , pVkCmdEndRenderPass2KHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
#if VK_USE_PLATFORM_ANDROID_KHR
  , pVkGetAndroidHardwareBufferPropertiesANDROID :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult)
  , pVkGetMemoryAndroidHardwareBufferANDROID :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult)
#endif
  , pVkCmdDrawIndirectCountKHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawIndexedIndirectCountKHR :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdSetCheckpointNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ())
  , pVkGetQueueCheckpointDataNV :: FunPtr (("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ())
  , pVkCmdBindTransformFeedbackBuffersEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ())
  , pVkCmdBeginTransformFeedbackEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
  , pVkCmdEndTransformFeedbackEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
  , pVkCmdBeginQueryIndexedEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ())
  , pVkCmdEndQueryIndexedEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ())
  , pVkCmdDrawIndirectByteCountEXT :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ())
  , pVkCmdSetExclusiveScissorNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ())
  , pVkCmdBindShadingRateImageNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ())
  , pVkCmdSetViewportShadingRatePaletteNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ())
  , pVkCmdSetCoarseSampleOrderNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ())
  , pVkCmdDrawMeshTasksNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksIndirectNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawMeshTasksIndirectCountNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCompileDeferredNV :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult)
  , pVkCreateAccelerationStructureNV :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult)
  , pVkDestroyAccelerationStructureNV :: FunPtr (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetAccelerationStructureMemoryRequirementsNV :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ())
  , pVkBindAccelerationStructureMemoryNV :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult)
  , pVkCmdCopyAccelerationStructureNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ())
  , pVkCmdWriteAccelerationStructuresPropertiesNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ())
  , pVkCmdBuildAccelerationStructureNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ())
  , pVkCmdTraceRaysNV :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ())
  , pVkGetRayTracingShaderGroupHandlesNV :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
  , pVkGetAccelerationStructureHandleNV :: FunPtr (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
  , pVkCreateRayTracingPipelinesNV :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
  , pVkGetImageDrmFormatModifierPropertiesEXT :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult)
  , pVkGetBufferDeviceAddressEXT :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress)
  , pVkGetImageViewHandleNVX :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32)
  , pVkGetDeviceGroupSurfacePresentModes2EXT :: FunPtr (("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
  , pVkAcquireFullScreenExclusiveModeEXT :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
  , pVkReleaseFullScreenExclusiveModeEXT :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
  }
  deriving (Show)

data InstanceCmds = InstanceCmds
  { instanceCmdsHandle :: VkInstance
  , pVkDestroyInstance :: FunPtr (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkEnumeratePhysicalDevices :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult)
  , pVkGetInstanceProcAddr :: FunPtr (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
  , pVkGetPhysicalDeviceProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ())
  , pVkGetPhysicalDeviceQueueFamilyProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ())
  , pVkGetPhysicalDeviceMemoryProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ())
  , pVkGetPhysicalDeviceFeatures :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ())
  , pVkGetPhysicalDeviceFormatProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ())
  , pVkGetPhysicalDeviceImageFormatProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult)
  , pVkCreateDevice :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult)
  , pVkEnumerateDeviceLayerProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)
  , pVkEnumerateDeviceExtensionProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)
  , pVkGetPhysicalDeviceSparseImageFormatProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ())
#if VK_USE_PLATFORM_ANDROID_KHR
  , pVkCreateAndroidSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
  , pVkGetPhysicalDeviceDisplayPropertiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceDisplayPlanePropertiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult)
  , pVkGetDisplayPlaneSupportedDisplaysKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult)
  , pVkGetDisplayModePropertiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult)
  , pVkCreateDisplayModeKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult)
  , pVkGetDisplayPlaneCapabilitiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult)
  , pVkCreateDisplayPlaneSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
  , pVkDestroySurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkGetPhysicalDeviceSurfaceSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult)
  , pVkGetPhysicalDeviceSurfaceCapabilitiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceSurfaceFormatsKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceSurfacePresentModesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#if VK_USE_PLATFORM_VI_NN
  , pVkCreateViSurfaceNN :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_WAYLAND_KHR
  , pVkCreateWaylandSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceWaylandPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkCreateWin32SurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceWin32PresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_XLIB_KHR
  , pVkCreateXlibSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceXlibPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_XCB_KHR
  , pVkCreateXcbSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
  , pVkGetPhysicalDeviceXcbPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_FUCHSIA
  , pVkCreateImagePipeSurfaceFUCHSIA :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_GGP
  , pVkCreateStreamDescriptorSurfaceGGP :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
  , pVkCreateDebugReportCallbackEXT :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult)
  , pVkDestroyDebugReportCallbackEXT :: FunPtr (("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkDebugReportMessageEXT :: FunPtr (("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ())
  , pVkGetPhysicalDeviceExternalImageFormatPropertiesNV :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult)
  , pVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ())
  , pVkGetPhysicalDeviceFeatures2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ())
  , pVkGetPhysicalDeviceProperties2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ())
  , pVkGetPhysicalDeviceFormatProperties2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ())
  , pVkGetPhysicalDeviceImageFormatProperties2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult)
  , pVkGetPhysicalDeviceQueueFamilyProperties2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ())
  , pVkGetPhysicalDeviceMemoryProperties2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ())
  , pVkGetPhysicalDeviceSparseImageFormatProperties2 :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ())
  , pVkGetPhysicalDeviceExternalBufferProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ())
  , pVkGetPhysicalDeviceExternalSemaphoreProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ())
  , pVkGetPhysicalDeviceExternalFenceProperties :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ())
  , pVkReleaseDisplayEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
#if VK_USE_PLATFORM_XLIB_XRANDR_EXT
  , pVkAcquireXlibDisplayEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
  , pVkGetRandROutputDisplayEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult)
#endif
  , pVkGetPhysicalDeviceSurfaceCapabilities2EXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult)
  , pVkEnumeratePhysicalDeviceGroups :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult)
  , pVkGetPhysicalDevicePresentRectanglesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult)
#if VK_USE_PLATFORM_IOS_MVK
  , pVkCreateIOSSurfaceMVK :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_MACOS_MVK
  , pVkCreateMacOSSurfaceMVK :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_METAL_EXT
  , pVkCreateMetalSurfaceEXT :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
  , pVkGetPhysicalDeviceMultisamplePropertiesEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ())
  , pVkGetPhysicalDeviceSurfaceCapabilities2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2KHR) -> IO VkResult)
  , pVkGetPhysicalDeviceSurfaceFormats2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormat2KHR) -> IO VkResult)
  , pVkGetPhysicalDeviceDisplayProperties2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult)
  , pVkGetPhysicalDeviceDisplayPlaneProperties2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult)
  , pVkGetDisplayModeProperties2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult)
  , pVkGetDisplayPlaneCapabilities2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult)
  , pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult)
  , pVkCreateDebugUtilsMessengerEXT :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult)
  , pVkDestroyDebugUtilsMessengerEXT :: FunPtr (("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
  , pVkSubmitDebugUtilsMessageEXT :: FunPtr (("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ())
  , pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult)
  , pVkGetPhysicalDeviceSurfacePresentModes2EXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
  , pVkCreateHeadlessSurfaceEXT :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkHeadlessSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
  }
  deriving (Show)

foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
  :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)

initDeviceCmds :: InstanceCmds -> VkDevice -> IO DeviceCmds
initDeviceCmds instanceCmds handle = do
  pGetDeviceProcAddr <- castPtrToFunPtr @_ @FN_vkGetDeviceProcAddr
    <$> vkGetInstanceProcAddr instanceCmds (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr\NUL"#)
  let getDeviceProcAddr' = mkVkGetDeviceProcAddr pGetDeviceProcAddr
  DeviceCmds handle
    <$> (castPtrToFunPtr @_ @FN_vkGetDeviceProcAddr <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceProcAddr\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyDevice <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyDevice\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceQueue <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceQueue\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueueSubmit <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueueSubmit\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueueWaitIdle <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueueWaitIdle\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDeviceWaitIdle <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDeviceWaitIdle\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkAllocateMemory <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAllocateMemory\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkFreeMemory <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkFreeMemory\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkMapMemory <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkMapMemory\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkUnmapMemory <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkUnmapMemory\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkFlushMappedMemoryRanges <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkFlushMappedMemoryRanges\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkInvalidateMappedMemoryRanges <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkInvalidateMappedMemoryRanges\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceMemoryCommitment <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceMemoryCommitment\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetBufferMemoryRequirements <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetBufferMemoryRequirements\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkBindBufferMemory <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkBindBufferMemory\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageMemoryRequirements <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageMemoryRequirements\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkBindImageMemory <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkBindImageMemory\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageSparseMemoryRequirements <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageSparseMemoryRequirements\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueueBindSparse <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueueBindSparse\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateFence <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateFence\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyFence <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyFence\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkResetFences <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkResetFences\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetFenceStatus <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetFenceStatus\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkWaitForFences <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkWaitForFences\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateSemaphore <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateSemaphore\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroySemaphore <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroySemaphore\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateEvent <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateEvent\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyEvent <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyEvent\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetEventStatus <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetEventStatus\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkSetEvent <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkSetEvent\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkResetEvent <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkResetEvent\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateQueryPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateQueryPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyQueryPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyQueryPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetQueryPoolResults <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetQueryPoolResults\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkResetQueryPoolEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkResetQueryPoolEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateBufferView <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateBufferView\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyBufferView <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyBufferView\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageSubresourceLayout <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageSubresourceLayout\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateImageView <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateImageView\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyImageView <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyImageView\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateShaderModule <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateShaderModule\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyShaderModule <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyShaderModule\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreatePipelineCache <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreatePipelineCache\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyPipelineCache <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyPipelineCache\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetPipelineCacheData <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetPipelineCacheData\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkMergePipelineCaches <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkMergePipelineCaches\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateGraphicsPipelines <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateGraphicsPipelines\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateComputePipelines <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateComputePipelines\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyPipeline <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyPipeline\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreatePipelineLayout <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreatePipelineLayout\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyPipelineLayout <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyPipelineLayout\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateSampler <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateSampler\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroySampler <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroySampler\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateDescriptorSetLayout <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDescriptorSetLayout\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyDescriptorSetLayout <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyDescriptorSetLayout\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateDescriptorPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDescriptorPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyDescriptorPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyDescriptorPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkResetDescriptorPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkResetDescriptorPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkAllocateDescriptorSets <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAllocateDescriptorSets\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkFreeDescriptorSets <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkFreeDescriptorSets\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkUpdateDescriptorSets <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkUpdateDescriptorSets\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateFramebuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateFramebuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyFramebuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyFramebuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateRenderPass <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateRenderPass\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyRenderPass <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyRenderPass\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetRenderAreaGranularity <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetRenderAreaGranularity\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateCommandPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateCommandPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyCommandPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyCommandPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkResetCommandPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkResetCommandPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkAllocateCommandBuffers <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAllocateCommandBuffers\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkFreeCommandBuffers <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkFreeCommandBuffers\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkBeginCommandBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkBeginCommandBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkEndCommandBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkEndCommandBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkResetCommandBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkResetCommandBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBindPipeline <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBindPipeline\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetViewport <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetViewport\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetScissor <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetScissor\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetLineWidth <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetLineWidth\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetDepthBias <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetDepthBias\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetBlendConstants <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetBlendConstants\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetDepthBounds <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetDepthBounds\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetStencilCompareMask <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetStencilCompareMask\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetStencilWriteMask <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetStencilWriteMask\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetStencilReference <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetStencilReference\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBindDescriptorSets <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBindDescriptorSets\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBindIndexBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBindIndexBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBindVertexBuffers <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBindVertexBuffers\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDraw <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDraw\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndexed <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndexed\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndirect <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndirect\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndexedIndirect <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndexedIndirect\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDispatch <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDispatch\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDispatchIndirect <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDispatchIndirect\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdCopyBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdCopyBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdCopyImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdCopyImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBlitImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBlitImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdCopyBufferToImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdCopyBufferToImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdCopyImageToBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdCopyImageToBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdUpdateBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdUpdateBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdFillBuffer <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdFillBuffer\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdClearColorImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdClearColorImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdClearDepthStencilImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdClearDepthStencilImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdClearAttachments <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdClearAttachments\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdResolveImage <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdResolveImage\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetEvent <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetEvent\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdResetEvent <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdResetEvent\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdWaitEvents <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdWaitEvents\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdPipelineBarrier <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdPipelineBarrier\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginQuery <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginQuery\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndQuery <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndQuery\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginConditionalRenderingEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginConditionalRenderingEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndConditionalRenderingEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndConditionalRenderingEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdResetQueryPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdResetQueryPool\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdWriteTimestamp <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdWriteTimestamp\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdCopyQueryPoolResults <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdCopyQueryPoolResults\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdPushConstants <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdPushConstants\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginRenderPass <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginRenderPass\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdNextSubpass <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdNextSubpass\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndRenderPass <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndRenderPass\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdExecuteCommands <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdExecuteCommands\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateSharedSwapchainsKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateSharedSwapchainsKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateSwapchainKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateSwapchainKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroySwapchainKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroySwapchainKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetSwapchainImagesKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetSwapchainImagesKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkAcquireNextImageKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAcquireNextImageKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueuePresentKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueuePresentKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDebugMarkerSetObjectNameEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDebugMarkerSetObjectNameEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDebugMarkerSetObjectTagEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDebugMarkerSetObjectTagEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDebugMarkerBeginEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDebugMarkerBeginEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDebugMarkerEndEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDebugMarkerEndEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDebugMarkerInsertEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDebugMarkerInsertEXT\NUL"#))
#if VK_USE_PLATFORM_WIN32_KHR
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryWin32HandleNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryWin32HandleNV\NUL"#))
#endif
    <*> (castPtrToFunPtr @_ @FN_vkCmdProcessCommandsNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdProcessCommandsNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdReserveSpaceForCommandsNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdReserveSpaceForCommandsNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateIndirectCommandsLayoutNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateIndirectCommandsLayoutNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyIndirectCommandsLayoutNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyIndirectCommandsLayoutNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateObjectTableNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateObjectTableNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyObjectTableNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyObjectTableNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkRegisterObjectsNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkRegisterObjectsNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkUnregisterObjectsNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkUnregisterObjectsNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdPushDescriptorSetKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdPushDescriptorSetKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkTrimCommandPool <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkTrimCommandPool\NUL"#))
#if VK_USE_PLATFORM_WIN32_KHR
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryWin32HandleKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryWin32HandleKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryWin32HandlePropertiesKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryWin32HandlePropertiesKHR\NUL"#))
#endif
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryFdKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryFdKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryFdPropertiesKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryFdPropertiesKHR\NUL"#))
#if VK_USE_PLATFORM_WIN32_KHR
    <*> (castPtrToFunPtr @_ @FN_vkGetSemaphoreWin32HandleKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetSemaphoreWin32HandleKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkImportSemaphoreWin32HandleKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkImportSemaphoreWin32HandleKHR\NUL"#))
#endif
    <*> (castPtrToFunPtr @_ @FN_vkGetSemaphoreFdKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetSemaphoreFdKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkImportSemaphoreFdKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkImportSemaphoreFdKHR\NUL"#))
#if VK_USE_PLATFORM_WIN32_KHR
    <*> (castPtrToFunPtr @_ @FN_vkGetFenceWin32HandleKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetFenceWin32HandleKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkImportFenceWin32HandleKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkImportFenceWin32HandleKHR\NUL"#))
#endif
    <*> (castPtrToFunPtr @_ @FN_vkGetFenceFdKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetFenceFdKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkImportFenceFdKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkImportFenceFdKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDisplayPowerControlEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDisplayPowerControlEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkRegisterDeviceEventEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkRegisterDeviceEventEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkRegisterDisplayEventEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkRegisterDisplayEventEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetSwapchainCounterEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetSwapchainCounterEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceGroupPeerMemoryFeatures <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceGroupPeerMemoryFeatures\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkBindBufferMemory2 <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkBindBufferMemory2\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkBindImageMemory2 <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkBindImageMemory2\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetDeviceMask <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetDeviceMask\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceGroupPresentCapabilitiesKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceGroupPresentCapabilitiesKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceGroupSurfacePresentModesKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceGroupSurfacePresentModesKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkAcquireNextImage2KHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAcquireNextImage2KHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDispatchBase <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDispatchBase\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateDescriptorUpdateTemplate <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDescriptorUpdateTemplate\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyDescriptorUpdateTemplate <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyDescriptorUpdateTemplate\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkUpdateDescriptorSetWithTemplate <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkUpdateDescriptorSetWithTemplate\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdPushDescriptorSetWithTemplateKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdPushDescriptorSetWithTemplateKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkSetHdrMetadataEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkSetHdrMetadataEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetSwapchainStatusKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetSwapchainStatusKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetRefreshCycleDurationGOOGLE <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetRefreshCycleDurationGOOGLE\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetPastPresentationTimingGOOGLE <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetPastPresentationTimingGOOGLE\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetViewportWScalingNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetViewportWScalingNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetDiscardRectangleEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetDiscardRectangleEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetSampleLocationsEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetSampleLocationsEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetBufferMemoryRequirements2 <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetBufferMemoryRequirements2\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageMemoryRequirements2 <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageMemoryRequirements2\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageSparseMemoryRequirements2 <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageSparseMemoryRequirements2\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateSamplerYcbcrConversion <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateSamplerYcbcrConversion\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroySamplerYcbcrConversion <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroySamplerYcbcrConversion\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceQueue2 <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceQueue2\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateValidationCacheEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateValidationCacheEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyValidationCacheEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyValidationCacheEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetValidationCacheDataEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetValidationCacheDataEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkMergeValidationCachesEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkMergeValidationCachesEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDescriptorSetLayoutSupport <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDescriptorSetLayoutSupport\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetShaderInfoAMD <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetShaderInfoAMD\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkSetLocalDimmingAMD <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkSetLocalDimmingAMD\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetCalibratedTimestampsEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetCalibratedTimestampsEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkSetDebugUtilsObjectNameEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkSetDebugUtilsObjectNameEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkSetDebugUtilsObjectTagEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkSetDebugUtilsObjectTagEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueueBeginDebugUtilsLabelEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueueBeginDebugUtilsLabelEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueueEndDebugUtilsLabelEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueueEndDebugUtilsLabelEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkQueueInsertDebugUtilsLabelEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkQueueInsertDebugUtilsLabelEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginDebugUtilsLabelEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginDebugUtilsLabelEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndDebugUtilsLabelEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndDebugUtilsLabelEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdInsertDebugUtilsLabelEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdInsertDebugUtilsLabelEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryHostPointerPropertiesEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryHostPointerPropertiesEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdWriteBufferMarkerAMD <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdWriteBufferMarkerAMD\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateRenderPass2KHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateRenderPass2KHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginRenderPass2KHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginRenderPass2KHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdNextSubpass2KHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdNextSubpass2KHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndRenderPass2KHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndRenderPass2KHR\NUL"#))
#if VK_USE_PLATFORM_ANDROID_KHR
    <*> (castPtrToFunPtr @_ @FN_vkGetAndroidHardwareBufferPropertiesANDROID <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetAndroidHardwareBufferPropertiesANDROID\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetMemoryAndroidHardwareBufferANDROID <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetMemoryAndroidHardwareBufferANDROID\NUL"#))
#endif
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndirectCountKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndirectCountKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndexedIndirectCountKHR <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndexedIndirectCountKHR\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetCheckpointNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetCheckpointNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetQueueCheckpointDataNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetQueueCheckpointDataNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBindTransformFeedbackBuffersEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBindTransformFeedbackBuffersEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginTransformFeedbackEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginTransformFeedbackEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndTransformFeedbackEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndTransformFeedbackEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBeginQueryIndexedEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBeginQueryIndexedEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdEndQueryIndexedEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdEndQueryIndexedEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndirectByteCountEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndirectByteCountEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetExclusiveScissorNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetExclusiveScissorNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBindShadingRateImageNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBindShadingRateImageNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetViewportShadingRatePaletteNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetViewportShadingRatePaletteNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdSetCoarseSampleOrderNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdSetCoarseSampleOrderNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawMeshTasksNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawMeshTasksNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawMeshTasksIndirectNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawMeshTasksIndirectNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawMeshTasksIndirectCountNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawMeshTasksIndirectCountNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCompileDeferredNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCompileDeferredNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateAccelerationStructureNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateAccelerationStructureNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkDestroyAccelerationStructureNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyAccelerationStructureNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetAccelerationStructureMemoryRequirementsNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetAccelerationStructureMemoryRequirementsNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkBindAccelerationStructureMemoryNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkBindAccelerationStructureMemoryNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdCopyAccelerationStructureNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdCopyAccelerationStructureNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdWriteAccelerationStructuresPropertiesNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdWriteAccelerationStructuresPropertiesNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdBuildAccelerationStructureNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdBuildAccelerationStructureNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdTraceRaysNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdTraceRaysNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetRayTracingShaderGroupHandlesNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetRayTracingShaderGroupHandlesNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetAccelerationStructureHandleNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetAccelerationStructureHandleNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCreateRayTracingPipelinesNV <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCreateRayTracingPipelinesNV\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageDrmFormatModifierPropertiesEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageDrmFormatModifierPropertiesEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetBufferDeviceAddressEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetBufferDeviceAddressEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetImageViewHandleNVX <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetImageViewHandleNVX\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkGetDeviceGroupSurfacePresentModes2EXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkGetDeviceGroupSurfacePresentModes2EXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkAcquireFullScreenExclusiveModeEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAcquireFullScreenExclusiveModeEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkReleaseFullScreenExclusiveModeEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkReleaseFullScreenExclusiveModeEXT\NUL"#))

-- | A version of 'vkGetInstanceProcAddr' which can be called with a
-- null pointer for the instance.
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetInstanceProcAddr" vkGetInstanceProcAddr' :: ("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction

initInstanceCmds :: VkInstance -> IO InstanceCmds
initInstanceCmds handle = InstanceCmds handle
  <$> (castPtrToFunPtr @_ @FN_vkDestroyInstance <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyInstance\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumeratePhysicalDevices <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkEnumeratePhysicalDevices\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetInstanceProcAddr <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetInstanceProcAddr\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceQueueFamilyProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceQueueFamilyProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceMemoryProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMemoryProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFeatures <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFeatures\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFormatProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFormatProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceImageFormatProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceImageFormatProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDevice <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDevice\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumerateDeviceLayerProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkEnumerateDeviceLayerProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumerateDeviceExtensionProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkEnumerateDeviceExtensionProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSparseImageFormatProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSparseImageFormatProperties\NUL"#))
#if VK_USE_PLATFORM_ANDROID_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateAndroidSurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateAndroidSurfaceKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayPropertiesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPlanePropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayPlaneSupportedDisplaysKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetDisplayPlaneSupportedDisplaysKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayModePropertiesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetDisplayModePropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDisplayModeKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDisplayModeKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayPlaneCapabilitiesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetDisplayPlaneCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDisplayPlaneSurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDisplayPlaneSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDestroySurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkDestroySurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceSupportKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceSupportKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceFormatsKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceFormatsKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfacePresentModesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfacePresentModesKHR\NUL"#))
#if VK_USE_PLATFORM_VI_NN
  <*> (castPtrToFunPtr @_ @FN_vkCreateViSurfaceNN <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateViSurfaceNN\NUL"#))
#endif
#if VK_USE_PLATFORM_WAYLAND_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateWaylandSurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateWaylandSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceWaylandPresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_WIN32_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateWin32SurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateWin32SurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceWin32PresentationSupportKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceWin32PresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_XLIB_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateXlibSurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateXlibSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceXlibPresentationSupportKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceXlibPresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_XCB_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateXcbSurfaceKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateXcbSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceXcbPresentationSupportKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceXcbPresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_FUCHSIA
  <*> (castPtrToFunPtr @_ @FN_vkCreateImagePipeSurfaceFUCHSIA <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateImagePipeSurfaceFUCHSIA\NUL"#))
#endif
#if VK_USE_PLATFORM_GGP
  <*> (castPtrToFunPtr @_ @FN_vkCreateStreamDescriptorSurfaceGGP <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateStreamDescriptorSurfaceGGP\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkCreateDebugReportCallbackEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDebugReportCallbackEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDestroyDebugReportCallbackEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyDebugReportCallbackEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDebugReportMessageEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkDebugReportMessageEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalImageFormatPropertiesNV\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFeatures2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFeatures2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceProperties2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFormatProperties2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceImageFormatProperties2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceImageFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceQueueFamilyProperties2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceQueueFamilyProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceMemoryProperties2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMemoryProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSparseImageFormatProperties2 <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalBufferProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalBufferProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalSemaphoreProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalSemaphoreProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalFenceProperties <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalFenceProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkReleaseDisplayEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkReleaseDisplayEXT\NUL"#))
#if VK_USE_PLATFORM_XLIB_XRANDR_EXT
  <*> (castPtrToFunPtr @_ @FN_vkAcquireXlibDisplayEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkAcquireXlibDisplayEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetRandROutputDisplayEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetRandROutputDisplayEXT\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilities2EXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumeratePhysicalDeviceGroups <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkEnumeratePhysicalDeviceGroups\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDevicePresentRectanglesKHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDevicePresentRectanglesKHR\NUL"#))
#if VK_USE_PLATFORM_IOS_MVK
  <*> (castPtrToFunPtr @_ @FN_vkCreateIOSSurfaceMVK <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateIOSSurfaceMVK\NUL"#))
#endif
#if VK_USE_PLATFORM_MACOS_MVK
  <*> (castPtrToFunPtr @_ @FN_vkCreateMacOSSurfaceMVK <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateMacOSSurfaceMVK\NUL"#))
#endif
#if VK_USE_PLATFORM_METAL_EXT
  <*> (castPtrToFunPtr @_ @FN_vkCreateMetalSurfaceEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateMetalSurfaceEXT\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceMultisamplePropertiesEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMultisamplePropertiesEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilities2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceFormats2KHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceFormats2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayProperties2KHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayProperties2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPlaneProperties2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayModeProperties2KHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetDisplayModeProperties2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayPlaneCapabilities2KHR <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetDisplayPlaneCapabilities2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDebugUtilsMessengerEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateDebugUtilsMessengerEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDestroyDebugUtilsMessengerEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkDestroyDebugUtilsMessengerEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkSubmitDebugUtilsMessageEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkSubmitDebugUtilsMessageEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfacePresentModes2EXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfacePresentModes2EXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateHeadlessSurfaceEXT <$> vkGetInstanceProcAddr' handle (GHC.Ptr.Ptr "vkCreateHeadlessSurfaceEXT\NUL"#))
