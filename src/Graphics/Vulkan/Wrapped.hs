{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Wrapped
  ( 
  ) where

import Data.ByteString
  ( ByteString
  , useAsCString
  )
import Data.Int
  ( Int32
  )
import Data.Vector.Storable
  ( Vector
  , unsafeFromForeignPtr0
  , unsafeWith
  )
import Data.Word
  ( Word32
  , Word64
  )
import Foreign.C.Types
  ( CFloat(..)
  , CInt(..)
  , CSize(..)
  )
import Foreign.ForeignPtr
  ( mallocForeignPtrArray
  , withForeignPtr
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  , nullPtr
  )
import Foreign.Storable
  ( peek
  , pokeElemOff
  )
import qualified Data.Vector.Storable
  ( length
  )


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferCreateInfo(..)
  , vkCreateBuffer
  , vkDestroyBuffer
  )
import Graphics.Vulkan.Core10.BufferView
  ( VkBufferViewCreateInfo(..)
  , VkBufferView
  , vkCreateBufferView
  , vkDestroyBufferView
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferResetFlags
  , VkQueryControlFlags
  , vkAllocateCommandBuffers
  , vkBeginCommandBuffer
  , vkEndCommandBuffer
  , vkFreeCommandBuffers
  , vkResetCommandBuffer
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( VkBufferCopy(..)
  , VkBufferImageCopy(..)
  , VkBufferMemoryBarrier(..)
  , VkClearAttachment(..)
  , VkClearColorValue(..)
  , VkClearDepthStencilValue(..)
  , VkClearRect(..)
  , VkImageBlit(..)
  , VkImageCopy(..)
  , VkImageMemoryBarrier(..)
  , VkImageResolve(..)
  , VkIndexType(..)
  , VkMemoryBarrier(..)
  , VkRenderPassBeginInfo(..)
  , VkSubpassContents(..)
  , VkStencilFaceFlags
  , vkCmdBeginQuery
  , vkCmdBeginRenderPass
  , vkCmdBindDescriptorSets
  , vkCmdBindIndexBuffer
  , vkCmdBindPipeline
  , vkCmdBlitImage
  , vkCmdClearAttachments
  , vkCmdClearColorImage
  , vkCmdClearDepthStencilImage
  , vkCmdCopyBuffer
  , vkCmdCopyBufferToImage
  , vkCmdCopyImage
  , vkCmdCopyImageToBuffer
  , vkCmdCopyQueryPoolResults
  , vkCmdDispatch
  , vkCmdDispatchIndirect
  , vkCmdDraw
  , vkCmdDrawIndexed
  , vkCmdDrawIndexedIndirect
  , vkCmdDrawIndirect
  , vkCmdEndQuery
  , vkCmdEndRenderPass
  , vkCmdExecuteCommands
  , vkCmdFillBuffer
  , vkCmdNextSubpass
  , vkCmdPipelineBarrier
  , vkCmdPushConstants
  , vkCmdResetEvent
  , vkCmdResetQueryPool
  , vkCmdResolveImage
  , vkCmdSetBlendConstants
  , vkCmdSetDepthBias
  , vkCmdSetDepthBounds
  , vkCmdSetEvent
  , vkCmdSetLineWidth
  , vkCmdSetScissor
  , vkCmdSetStencilCompareMask
  , vkCmdSetStencilReference
  , vkCmdSetStencilWriteMask
  , vkCmdSetViewport
  , vkCmdUpdateBuffer
  , vkCmdWaitEvents
  , vkCmdWriteTimestamp
  )
import Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPoolCreateInfo(..)
  , VkCommandPool
  , VkCommandPoolResetFlags
  , vkCreateCommandPool
  , vkDestroyCommandPool
  , vkResetCommandPool
  )
import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkWriteDescriptorSet(..)
  , VkDescriptorPool
  , VkDescriptorSet
  , vkAllocateDescriptorSets
  , vkCreateDescriptorPool
  , vkCreateDescriptorSetLayout
  , vkDestroyDescriptorPool
  , vkDestroyDescriptorSetLayout
  , vkFreeDescriptorSets
  , vkResetDescriptorPool
  , vkUpdateDescriptorSets
  )
import Graphics.Vulkan.Core10.Device
  ( VkDeviceCreateInfo(..)
  , vkCreateDevice
  , vkDestroyDevice
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkInstanceCreateInfo(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkQueueFamilyProperties(..)
  , VkSampleCountFlagBits(..)
  , PFN_vkVoidFunction
  , VkDevice
  , VkDeviceSize
  , VkImageCreateFlags
  , VkImageUsageFlags
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
  )
import Graphics.Vulkan.Core10.Event
  ( VkEventCreateInfo(..)
  , VkEvent
  , vkCreateEvent
  , vkDestroyEvent
  , vkGetEventStatus
  , vkResetEvent
  , vkSetEvent
  )
import Graphics.Vulkan.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
  , vkEnumerateDeviceExtensionProperties
  , vkEnumerateInstanceExtensionProperties
  )
import Graphics.Vulkan.Core10.Fence
  ( VkFenceCreateInfo(..)
  , vkCreateFence
  , vkDestroyFence
  , vkGetFenceStatus
  , vkResetFences
  , vkWaitForFences
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , VkSubresourceLayout(..)
  , vkCreateImage
  , vkDestroyImage
  , vkGetImageSubresourceLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkImageSubresourceRange(..)
  , VkImageViewCreateInfo(..)
  , VkImageView
  , vkCreateImageView
  , vkDestroyImageView
  )
import Graphics.Vulkan.Core10.LayerDiscovery
  ( VkLayerProperties(..)
  , vkEnumerateDeviceLayerProperties
  , vkEnumerateInstanceLayerProperties
  )
import Graphics.Vulkan.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkDeviceMemory
  , vkAllocateMemory
  , vkFlushMappedMemoryRanges
  , vkFreeMemory
  , vkGetDeviceMemoryCommitment
  , vkInvalidateMappedMemoryRanges
  , vkUnmapMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  , vkBindBufferMemory
  , vkBindImageMemory
  , vkGetBufferMemoryRequirements
  , vkGetImageMemoryRequirements
  )
import Graphics.Vulkan.Core10.Pass
  ( VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateInfo(..)
  , VkDependencyFlags
  , VkFramebuffer
  , vkCreateFramebuffer
  , vkCreateRenderPass
  , vkDestroyFramebuffer
  , vkDestroyRenderPass
  , vkGetRenderAreaGranularity
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkComputePipelineCreateInfo(..)
  , VkExtent2D(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkRect2D(..)
  , VkViewport(..)
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  , vkCreateComputePipelines
  , vkCreateGraphicsPipelines
  , vkDestroyPipeline
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( VkPipelineCacheCreateInfo(..)
  , VkPipelineCache
  , vkCreatePipelineCache
  , vkDestroyPipelineCache
  , vkMergePipelineCaches
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkPipelineLayoutCreateInfo(..)
  , VkDescriptorSetLayout
  , VkShaderStageFlags
  , vkCreatePipelineLayout
  , vkDestroyPipelineLayout
  )
import Graphics.Vulkan.Core10.Query
  ( VkQueryPoolCreateInfo(..)
  , VkQueryPool
  , VkQueryResultFlags
  , vkCreateQueryPool
  , vkDestroyQueryPool
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkSubmitInfo(..)
  , VkCommandBuffer
  , VkFence
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  , vkDeviceWaitIdle
  , vkGetDeviceQueue
  , vkQueueSubmit
  , vkQueueWaitIdle
  )
import Graphics.Vulkan.Core10.QueueSemaphore
  ( VkSemaphoreCreateInfo(..)
  , vkCreateSemaphore
  , vkDestroySemaphore
  )
import Graphics.Vulkan.Core10.Sampler
  ( VkFilter(..)
  , VkSamplerCreateInfo(..)
  , VkSampler
  , vkCreateSampler
  , vkDestroySampler
  )
import Graphics.Vulkan.Core10.Shader
  ( VkShaderModuleCreateInfo(..)
  , VkShaderModule
  , vkCreateShaderModule
  , vkDestroyShaderModule
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageSubresource(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryRequirements(..)
  , vkGetImageSparseMemoryRequirements
  , vkGetPhysicalDeviceSparseImageFormatProperties
  , vkQueueBindSparse
  )
import Graphics.Vulkan.Core11.DeviceInitialization
  ( vkEnumerateInstanceVersion
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , vkGetDeviceQueue2
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , vkBindBufferMemory2
  , vkBindImageMemory2
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplate
  , vkCreateDescriptorUpdateTemplate
  , vkDestroyDescriptorUpdateTemplate
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( VkPeerMemoryFeatureFlags
  , vkCmdDispatchBase
  , vkCmdSetDeviceMask
  , vkGetDeviceGroupPeerMemoryFeatures
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkPhysicalDeviceGroupProperties(..)
  , vkEnumeratePhysicalDeviceGroups
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , vkGetPhysicalDeviceExternalFenceProperties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , vkGetPhysicalDeviceExternalBufferProperties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , vkGetPhysicalDeviceExternalSemaphoreProperties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
  , vkGetBufferMemoryRequirements2
  , vkGetImageMemoryRequirements2
  , vkGetImageSparseMemoryRequirements2
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkSparseImageFormatProperties2(..)
  , vkGetPhysicalDeviceFeatures2
  , vkGetPhysicalDeviceFormatProperties2
  , vkGetPhysicalDeviceImageFormatProperties2
  , vkGetPhysicalDeviceMemoryProperties2
  , vkGetPhysicalDeviceProperties2
  , vkGetPhysicalDeviceQueueFamilyProperties2
  , vkGetPhysicalDeviceSparseImageFormatProperties2
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , vkTrimCommandPool
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , vkGetDescriptorSetLayoutSupport
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversion
  , vkCreateSamplerYcbcrConversion
  , vkDestroySamplerYcbcrConversion
  )
import Graphics.Vulkan.Extensions.VK_AMD_buffer_marker
  ( vkCmdWriteBufferMarkerAMD
  )
import Graphics.Vulkan.Extensions.VK_AMD_draw_indirect_count
  ( vkCmdDrawIndexedIndirectCountAMD
  , vkCmdDrawIndirectCountAMD
  )
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferPropertiesANDROID(..)
  , AHardwareBuffer
  , vkGetAndroidHardwareBufferPropertiesANDROID
  )
import Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  , vkAcquireXlibDisplayEXT
  , vkGetRandROutputDisplayEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , vkCmdDebugMarkerBeginEXT
  , vkCmdDebugMarkerEndEXT
  , vkCmdDebugMarkerInsertEXT
  , vkDebugMarkerSetObjectNameEXT
  , vkDebugMarkerSetObjectTagEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportObjectTypeEXT(..)
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagsEXT
  , vkCreateDebugReportCallbackEXT
  , vkDebugReportMessageEXT
  , vkDestroyDebugReportCallbackEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
  , VkDebugUtilsMessageTypeFlagsEXT
  , VkDebugUtilsMessengerEXT
  , vkCmdBeginDebugUtilsLabelEXT
  , vkCmdEndDebugUtilsLabelEXT
  , vkCmdInsertDebugUtilsLabelEXT
  , vkCreateDebugUtilsMessengerEXT
  , vkDestroyDebugUtilsMessengerEXT
  , vkQueueBeginDebugUtilsLabelEXT
  , vkQueueEndDebugUtilsLabelEXT
  , vkQueueInsertDebugUtilsLabelEXT
  , vkSetDebugUtilsObjectNameEXT
  , vkSetDebugUtilsObjectTagEXT
  , vkSubmitDebugUtilsMessageEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_direct_mode_display
  ( vkReleaseDisplayEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( vkCmdSetDiscardRectangleEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkDisplayPowerInfoEXT(..)
  , vkDisplayPowerControlEXT
  , vkGetSwapchainCounterEXT
  , vkRegisterDeviceEventEXT
  , vkRegisterDisplayEventEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagBitsEXT(..)
  , vkGetPhysicalDeviceSurfaceCapabilities2EXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( VkMultisamplePropertiesEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , vkCmdSetSampleLocationsEXT
  , vkGetPhysicalDeviceMultisamplePropertiesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateInfoEXT(..)
  , VkValidationCacheEXT
  , vkCreateValidationCacheEXT
  , vkDestroyValidationCacheEXT
  , vkMergeValidationCachesEXT
  )
import Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  , vkGetPastPresentationTimingGOOGLE
  , vkGetRefreshCycleDurationGOOGLE
  )
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR(..)
  , vkCreateAndroidSurfaceKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  , vkCreateDisplayModeKHR
  , vkCreateDisplayPlaneSurfaceKHR
  , vkGetDisplayModePropertiesKHR
  , vkGetDisplayPlaneCapabilitiesKHR
  , vkGetDisplayPlaneSupportedDisplaysKHR
  , vkGetPhysicalDeviceDisplayPlanePropertiesKHR
  , vkGetPhysicalDeviceDisplayPropertiesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( vkCreateSharedSwapchainsKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , vkGetFenceFdKHR
  , vkImportFenceFdKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , vkGetFenceWin32HandleKHR
  , vkImportFenceWin32HandleKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  , vkGetMemoryFdKHR
  , vkGetMemoryFdPropertiesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , vkGetMemoryWin32HandleKHR
  , vkGetMemoryWin32HandlePropertiesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , vkGetSemaphoreFdKHR
  , vkImportSemaphoreFdKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  , vkGetSemaphoreWin32HandleKHR
  , vkImportSemaphoreWin32HandleKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  , vkGetPhysicalDeviceSurfaceCapabilities2KHR
  , vkGetPhysicalDeviceSurfaceFormats2KHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_mir_surface
  ( VkMirSurfaceCreateInfoKHR(..)
  , MirConnection
  , vkCreateMirSurfaceKHR
  , vkGetPhysicalDeviceMirPresentationSupportKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( vkCmdPushDescriptorSetKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( vkGetSwapchainStatusKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkSurfaceKHR
  , vkDestroySurfaceKHR
  , vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , vkGetPhysicalDeviceSurfaceFormatsKHR
  , vkGetPhysicalDeviceSurfacePresentModesKHR
  , vkGetPhysicalDeviceSurfaceSupportKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateInfoKHR(..)
  , VkDeviceGroupPresentModeFlagsKHR
  , VkSwapchainKHR
  , vkAcquireNextImage2KHR
  , vkAcquireNextImageKHR
  , vkCreateSwapchainKHR
  , vkDestroySwapchainKHR
  , vkGetDeviceGroupPresentCapabilitiesKHR
  , vkGetDeviceGroupSurfacePresentModesKHR
  , vkGetPhysicalDevicePresentRectanglesKHR
  , vkGetSwapchainImagesKHR
  , vkQueuePresentKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR(..)
  , Wl_display
  , vkCreateWaylandSurfaceKHR
  , vkGetPhysicalDeviceWaylandPresentationSupportKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR(..)
  , vkCreateWin32SurfaceKHR
  , vkGetPhysicalDeviceWin32PresentationSupportKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  , vkCreateXcbSurfaceKHR
  , vkGetPhysicalDeviceXcbPresentationSupportKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , VisualID
  , vkCreateXlibSurfaceKHR
  , vkGetPhysicalDeviceXlibPresentationSupportKHR
  )
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK(..)
  , vkCreateIOSSurfaceMVK
  )
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK(..)
  , vkCreateMacOSSurfaceMVK
  )
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN(..)
  , vkCreateViSurfaceNN
  )
import Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkIndirectCommandsLayoutNVX
  , VkObjectTableNVX
  , vkCmdProcessCommandsNVX
  , vkCmdReserveSpaceForCommandsNVX
  , vkCreateIndirectCommandsLayoutNVX
  , vkCreateObjectTableNVX
  , vkDestroyIndirectCommandsLayoutNVX
  , vkDestroyObjectTableNVX
  , vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  )
import Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( VkViewportWScalingNV(..)
  , vkCmdSetViewportWScalingNV
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryHandleTypeFlagsNV
  , vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  , vkGetMemoryWin32HandleNV
  )



-- | Wrapper for vkCreateInstance
createInstance :: VkInstanceCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                              , VkInstance )
createInstance = \createInfo -> \allocator -> alloca (\pInstance -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateInstance pCreateInfo pAllocator pInstance >>= (\r -> (,) <$> pure r<*>peek pInstance))))

-- | Wrapper for vkDestroyInstance
destroyInstance :: VkInstance ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyInstance = \instance' -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyInstance instance' pAllocator)

-- | Wrapper for vkEnumeratePhysicalDevices
getNumenumeratePhysicalDevices :: VkInstance ->  IO ( VkResult
                                                    , Word32 )
getNumenumeratePhysicalDevices = \instance' -> alloca (\pPhysicalDeviceCount -> vkEnumeratePhysicalDevices instance' pPhysicalDeviceCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPhysicalDeviceCount))

-- | Wrapper for vkEnumeratePhysicalDevices
enumeratePhysicalDevices :: VkInstance ->  Word32 ->  IO ( VkResult
                                                         , Vector VkPhysicalDevice )
enumeratePhysicalDevices = \instance' -> \physicalDeviceCount -> mallocForeignPtrArray (fromIntegral (physicalDeviceCount)) >>= (\fpPhysicalDevices -> withForeignPtr fpPhysicalDevices (\pPhysicalDevices -> with physicalDeviceCount (\pPhysicalDeviceCount -> vkEnumeratePhysicalDevices instance' pPhysicalDeviceCount pPhysicalDevices >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpPhysicalDevices . fromIntegral <$> peek pPhysicalDeviceCount)))))

-- | Wrapper for vkGetDeviceProcAddr
getDeviceProcAddr :: VkDevice ->  ByteString ->  IO ( PFN_vkVoidFunction )
getDeviceProcAddr = \device -> \name -> useAsCString name (\pName -> vkGetDeviceProcAddr device pName)

-- | Wrapper for vkGetInstanceProcAddr
getInstanceProcAddr :: VkInstance ->  ByteString ->  IO ( PFN_vkVoidFunction )
getInstanceProcAddr = \instance' -> \name -> useAsCString name (\pName -> vkGetInstanceProcAddr instance' pName)

-- | Wrapper for vkGetPhysicalDeviceProperties
getPhysicalDeviceProperties :: VkPhysicalDevice ->  IO ( ()
                                                       , VkPhysicalDeviceProperties )
getPhysicalDeviceProperties = \physicalDevice -> alloca (\pProperties -> vkGetPhysicalDeviceProperties physicalDevice pProperties >>= (\r -> (,) <$> pure r<*>peek pProperties))

-- | Wrapper for vkGetPhysicalDeviceQueueFamilyProperties
getNumgetPhysicalDeviceQueueFamilyProperties :: VkPhysicalDevice ->  IO ( ()
                                                                        , Word32 )
getNumgetPhysicalDeviceQueueFamilyProperties = \physicalDevice -> alloca (\pQueueFamilyPropertyCount -> vkGetPhysicalDeviceQueueFamilyProperties physicalDevice pQueueFamilyPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pQueueFamilyPropertyCount))

-- | Wrapper for vkGetPhysicalDeviceQueueFamilyProperties
getPhysicalDeviceQueueFamilyProperties :: VkPhysicalDevice ->  Word32 ->  IO ( ()
                                                                             , Vector VkQueueFamilyProperties )
getPhysicalDeviceQueueFamilyProperties = \physicalDevice -> \queueFamilyPropertyCount -> mallocForeignPtrArray (fromIntegral (queueFamilyPropertyCount)) >>= (\fpQueueFamilyProperties -> withForeignPtr fpQueueFamilyProperties (\pQueueFamilyProperties -> with queueFamilyPropertyCount (\pQueueFamilyPropertyCount -> vkGetPhysicalDeviceQueueFamilyProperties physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpQueueFamilyProperties . fromIntegral <$> peek pQueueFamilyPropertyCount)))))

-- | Wrapper for vkGetPhysicalDeviceMemoryProperties
getPhysicalDeviceMemoryProperties :: VkPhysicalDevice ->  IO ( ()
                                                             , VkPhysicalDeviceMemoryProperties )
getPhysicalDeviceMemoryProperties = \physicalDevice -> alloca (\pMemoryProperties -> vkGetPhysicalDeviceMemoryProperties physicalDevice pMemoryProperties >>= (\r -> (,) <$> pure r<*>peek pMemoryProperties))

-- | Wrapper for vkGetPhysicalDeviceFeatures
getPhysicalDeviceFeatures :: VkPhysicalDevice ->  IO ( ()
                                                     , VkPhysicalDeviceFeatures )
getPhysicalDeviceFeatures = \physicalDevice -> alloca (\pFeatures -> vkGetPhysicalDeviceFeatures physicalDevice pFeatures >>= (\r -> (,) <$> pure r<*>peek pFeatures))

-- | Wrapper for vkGetPhysicalDeviceFormatProperties
getPhysicalDeviceFormatProperties :: VkPhysicalDevice ->  VkFormat ->  IO ( ()
                                                                          , VkFormatProperties )
getPhysicalDeviceFormatProperties = \physicalDevice -> \format -> alloca (\pFormatProperties -> vkGetPhysicalDeviceFormatProperties physicalDevice format pFormatProperties >>= (\r -> (,) <$> pure r<*>peek pFormatProperties))

-- | Wrapper for vkGetPhysicalDeviceImageFormatProperties
getPhysicalDeviceImageFormatProperties :: VkPhysicalDevice ->  VkFormat ->  VkImageType ->  VkImageTiling ->  VkImageUsageFlags ->  VkImageCreateFlags ->  IO ( VkResult
                                                                                                                                                              , VkImageFormatProperties )
getPhysicalDeviceImageFormatProperties = \physicalDevice -> \format -> \type' -> \tiling -> \usage -> \flags -> alloca (\pImageFormatProperties -> vkGetPhysicalDeviceImageFormatProperties physicalDevice format type' tiling usage flags pImageFormatProperties >>= (\r -> (,) <$> pure r<*>peek pImageFormatProperties))

-- | Wrapper for vkCreateDevice
createDevice :: VkPhysicalDevice ->  VkDeviceCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                               , VkDevice )
createDevice = \physicalDevice -> \createInfo -> \allocator -> alloca (\pDevice -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDevice physicalDevice pCreateInfo pAllocator pDevice >>= (\r -> (,) <$> pure r<*>peek pDevice))))

-- | Wrapper for vkDestroyDevice
destroyDevice :: VkDevice ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyDevice = \device -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyDevice device pAllocator)

-- | Wrapper for vkEnumerateInstanceVersion
enumerateInstanceVersion :: IO ( VkResult
                               , Word32 )
enumerateInstanceVersion = alloca (\pApiVersion -> vkEnumerateInstanceVersion pApiVersion >>= (\r -> (,) <$> pure r<*>peek pApiVersion))

-- | Wrapper for vkEnumerateInstanceLayerProperties
getNumenumerateInstanceLayerProperties :: IO ( VkResult
                                             , Word32 )
getNumenumerateInstanceLayerProperties = alloca (\pPropertyCount -> vkEnumerateInstanceLayerProperties pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount))

-- | Wrapper for vkEnumerateInstanceLayerProperties
enumerateInstanceLayerProperties :: Word32 ->  IO ( VkResult
                                                  , Vector VkLayerProperties )
enumerateInstanceLayerProperties = \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> vkEnumerateInstanceLayerProperties pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount)))))

-- | Wrapper for vkEnumerateInstanceExtensionProperties
getNumenumerateInstanceExtensionProperties :: Maybe ByteString ->  IO ( VkResult
                                                                      , Word32 )
getNumenumerateInstanceExtensionProperties = \layerName -> alloca (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> vkEnumerateInstanceExtensionProperties pLayerName pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for vkEnumerateInstanceExtensionProperties
enumerateInstanceExtensionProperties :: Maybe ByteString ->  Word32 ->  IO ( VkResult
                                                                           , Vector VkExtensionProperties )
enumerateInstanceExtensionProperties = \layerName -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> vkEnumerateInstanceExtensionProperties pLayerName pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount))))))

-- | Wrapper for vkEnumerateDeviceLayerProperties
getNumenumerateDeviceLayerProperties :: VkPhysicalDevice ->  IO ( VkResult
                                                                , Word32 )
getNumenumerateDeviceLayerProperties = \physicalDevice -> alloca (\pPropertyCount -> vkEnumerateDeviceLayerProperties physicalDevice pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount))

-- | Wrapper for vkEnumerateDeviceLayerProperties
enumerateDeviceLayerProperties :: VkPhysicalDevice ->  Word32 ->  IO ( VkResult
                                                                     , Vector VkLayerProperties )
enumerateDeviceLayerProperties = \physicalDevice -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> vkEnumerateDeviceLayerProperties physicalDevice pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount)))))

-- | Wrapper for vkEnumerateDeviceExtensionProperties
getNumenumerateDeviceExtensionProperties :: VkPhysicalDevice ->  Maybe ByteString ->  IO ( VkResult
                                                                                         , Word32 )
getNumenumerateDeviceExtensionProperties = \physicalDevice -> \layerName -> alloca (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> vkEnumerateDeviceExtensionProperties physicalDevice pLayerName pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for vkEnumerateDeviceExtensionProperties
enumerateDeviceExtensionProperties :: VkPhysicalDevice ->  Maybe ByteString ->  Word32 ->  IO ( VkResult
                                                                                              , Vector VkExtensionProperties )
enumerateDeviceExtensionProperties = \physicalDevice -> \layerName -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> maybeWith useAsCString layerName (\pLayerName -> vkEnumerateDeviceExtensionProperties physicalDevice pLayerName pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount))))))

-- | Wrapper for vkGetDeviceQueue
getDeviceQueue :: VkDevice ->  Word32 ->  Word32 ->  IO ( ()
                                                        , VkQueue )
getDeviceQueue = \device -> \queueFamilyIndex -> \queueIndex -> alloca (\pQueue -> vkGetDeviceQueue device queueFamilyIndex queueIndex pQueue >>= (\r -> (,) <$> pure r<*>peek pQueue))

-- | Wrapper for vkQueueSubmit
queueSubmit :: VkQueue ->  Vector VkSubmitInfo ->  VkFence ->  IO ( VkResult )
queueSubmit = \queue -> \submits -> \fence -> unsafeWith submits (\pSubmits -> vkQueueSubmit queue (fromIntegral $ Data.Vector.Storable.length submits) pSubmits fence)

-- | Wrapper for vkQueueWaitIdle
queueWaitIdle :: VkQueue ->  IO ( VkResult )
queueWaitIdle = \queue -> vkQueueWaitIdle queue

-- | Wrapper for vkDeviceWaitIdle
deviceWaitIdle :: VkDevice ->  IO ( VkResult )
deviceWaitIdle = \device -> vkDeviceWaitIdle device

-- | Wrapper for vkAllocateMemory
allocateMemory :: VkDevice ->  VkMemoryAllocateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                           , VkDeviceMemory )
allocateMemory = \device -> \allocateInfo -> \allocator -> alloca (\pMemory -> maybeWith with allocator (\pAllocator -> with allocateInfo (\pAllocateInfo -> vkAllocateMemory device pAllocateInfo pAllocator pMemory >>= (\r -> (,) <$> pure r<*>peek pMemory))))

-- | Wrapper for vkFreeMemory
freeMemory :: VkDevice ->  VkDeviceMemory ->  Maybe VkAllocationCallbacks ->  IO ( () )
freeMemory = \device -> \memory -> \allocator -> maybeWith with allocator (\pAllocator -> vkFreeMemory device memory pAllocator)

-- | Wrapper for vkUnmapMemory
unmapMemory :: VkDevice ->  VkDeviceMemory ->  IO ( () )
unmapMemory = \device -> \memory -> vkUnmapMemory device memory

-- | Wrapper for vkFlushMappedMemoryRanges
flushMappedMemoryRanges :: VkDevice ->  Vector VkMappedMemoryRange ->  IO ( VkResult )
flushMappedMemoryRanges = \device -> \memoryRanges -> unsafeWith memoryRanges (\pMemoryRanges -> vkFlushMappedMemoryRanges device (fromIntegral $ Data.Vector.Storable.length memoryRanges) pMemoryRanges)

-- | Wrapper for vkInvalidateMappedMemoryRanges
invalidateMappedMemoryRanges :: VkDevice ->  Vector VkMappedMemoryRange ->  IO ( VkResult )
invalidateMappedMemoryRanges = \device -> \memoryRanges -> unsafeWith memoryRanges (\pMemoryRanges -> vkInvalidateMappedMemoryRanges device (fromIntegral $ Data.Vector.Storable.length memoryRanges) pMemoryRanges)

-- | Wrapper for vkGetDeviceMemoryCommitment
getDeviceMemoryCommitment :: VkDevice ->  VkDeviceMemory ->  IO ( ()
                                                                , VkDeviceSize )
getDeviceMemoryCommitment = \device -> \memory -> alloca (\pCommittedMemoryInBytes -> vkGetDeviceMemoryCommitment device memory pCommittedMemoryInBytes >>= (\r -> (,) <$> pure r<*>peek pCommittedMemoryInBytes))

-- | Wrapper for vkGetBufferMemoryRequirements
getBufferMemoryRequirements :: VkDevice ->  VkBuffer ->  IO ( ()
                                                            , VkMemoryRequirements )
getBufferMemoryRequirements = \device -> \buffer -> alloca (\pMemoryRequirements -> vkGetBufferMemoryRequirements device buffer pMemoryRequirements >>= (\r -> (,) <$> pure r<*>peek pMemoryRequirements))

-- | Wrapper for vkBindBufferMemory
bindBufferMemory :: VkDevice ->  VkBuffer ->  VkDeviceMemory ->  VkDeviceSize ->  IO ( VkResult )
bindBufferMemory = \device -> \buffer -> \memory -> \memoryOffset -> vkBindBufferMemory device buffer memory memoryOffset

-- | Wrapper for vkGetImageMemoryRequirements
getImageMemoryRequirements :: VkDevice ->  VkImage ->  IO ( ()
                                                          , VkMemoryRequirements )
getImageMemoryRequirements = \device -> \image -> alloca (\pMemoryRequirements -> vkGetImageMemoryRequirements device image pMemoryRequirements >>= (\r -> (,) <$> pure r<*>peek pMemoryRequirements))

-- | Wrapper for vkBindImageMemory
bindImageMemory :: VkDevice ->  VkImage ->  VkDeviceMemory ->  VkDeviceSize ->  IO ( VkResult )
bindImageMemory = \device -> \image -> \memory -> \memoryOffset -> vkBindImageMemory device image memory memoryOffset

-- | Wrapper for vkGetImageSparseMemoryRequirements
getNumgetImageSparseMemoryRequirements :: VkDevice ->  VkImage ->  IO ( ()
                                                                      , Word32 )
getNumgetImageSparseMemoryRequirements = \device -> \image -> alloca (\pSparseMemoryRequirementCount -> vkGetImageSparseMemoryRequirements device image pSparseMemoryRequirementCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pSparseMemoryRequirementCount))

-- | Wrapper for vkGetImageSparseMemoryRequirements
getImageSparseMemoryRequirements :: VkDevice ->  VkImage ->  Word32 ->  IO ( ()
                                                                           , Vector VkSparseImageMemoryRequirements )
getImageSparseMemoryRequirements = \device -> \image -> \sparseMemoryRequirementCount -> mallocForeignPtrArray (fromIntegral (sparseMemoryRequirementCount)) >>= (\fpSparseMemoryRequirements -> withForeignPtr fpSparseMemoryRequirements (\pSparseMemoryRequirements -> with sparseMemoryRequirementCount (\pSparseMemoryRequirementCount -> vkGetImageSparseMemoryRequirements device image pSparseMemoryRequirementCount pSparseMemoryRequirements >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpSparseMemoryRequirements . fromIntegral <$> peek pSparseMemoryRequirementCount)))))

-- | Wrapper for vkGetPhysicalDeviceSparseImageFormatProperties
getNumgetPhysicalDeviceSparseImageFormatProperties :: VkPhysicalDevice ->  VkFormat ->  VkImageType ->  VkSampleCountFlagBits ->  VkImageUsageFlags ->  VkImageTiling ->  IO ( ()
                                                                                                                                                                             , Word32 )
getNumgetPhysicalDeviceSparseImageFormatProperties = \physicalDevice -> \format -> \type' -> \samples -> \usage -> \tiling -> alloca (\pPropertyCount -> vkGetPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount))

-- | Wrapper for vkGetPhysicalDeviceSparseImageFormatProperties
getPhysicalDeviceSparseImageFormatProperties :: VkPhysicalDevice ->  VkFormat ->  VkImageType ->  VkSampleCountFlagBits ->  VkImageUsageFlags ->  VkImageTiling ->  Word32 ->  IO ( ()
                                                                                                                                                                                  , Vector VkSparseImageFormatProperties )
getPhysicalDeviceSparseImageFormatProperties = \physicalDevice -> \format -> \type' -> \samples -> \usage -> \tiling -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> vkGetPhysicalDeviceSparseImageFormatProperties physicalDevice format type' samples usage tiling pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount)))))

-- | Wrapper for vkQueueBindSparse
queueBindSparse :: VkQueue ->  Vector VkBindSparseInfo ->  VkFence ->  IO ( VkResult )
queueBindSparse = \queue -> \bindInfo -> \fence -> unsafeWith bindInfo (\pBindInfo -> vkQueueBindSparse queue (fromIntegral $ Data.Vector.Storable.length bindInfo) pBindInfo fence)

-- | Wrapper for vkCreateFence
createFence :: VkDevice ->  VkFenceCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                     , VkFence )
createFence = \device -> \createInfo -> \allocator -> alloca (\pFence -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateFence device pCreateInfo pAllocator pFence >>= (\r -> (,) <$> pure r<*>peek pFence))))

-- | Wrapper for vkDestroyFence
destroyFence :: VkDevice ->  VkFence ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyFence = \device -> \fence -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyFence device fence pAllocator)

-- | Wrapper for vkResetFences
resetFences :: VkDevice ->  Vector VkFence ->  IO ( VkResult )
resetFences = \device -> \fences -> unsafeWith fences (\pFences -> vkResetFences device (fromIntegral $ Data.Vector.Storable.length fences) pFences)

-- | Wrapper for vkGetFenceStatus
getFenceStatus :: VkDevice ->  VkFence ->  IO ( VkResult )
getFenceStatus = \device -> \fence -> vkGetFenceStatus device fence

-- | Wrapper for vkWaitForFences
waitForFences :: VkDevice ->  Vector VkFence ->  VkBool32 ->  Word64 ->  IO ( VkResult )
waitForFences = \device -> \fences -> \waitAll -> \timeout -> unsafeWith fences (\pFences -> vkWaitForFences device (fromIntegral $ Data.Vector.Storable.length fences) pFences waitAll timeout)

-- | Wrapper for vkCreateSemaphore
createSemaphore :: VkDevice ->  VkSemaphoreCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                             , VkSemaphore )
createSemaphore = \device -> \createInfo -> \allocator -> alloca (\pSemaphore -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateSemaphore device pCreateInfo pAllocator pSemaphore >>= (\r -> (,) <$> pure r<*>peek pSemaphore))))

-- | Wrapper for vkDestroySemaphore
destroySemaphore :: VkDevice ->  VkSemaphore ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroySemaphore = \device -> \semaphore -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroySemaphore device semaphore pAllocator)

-- | Wrapper for vkCreateEvent
createEvent :: VkDevice ->  VkEventCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                     , VkEvent )
createEvent = \device -> \createInfo -> \allocator -> alloca (\pEvent -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateEvent device pCreateInfo pAllocator pEvent >>= (\r -> (,) <$> pure r<*>peek pEvent))))

-- | Wrapper for vkDestroyEvent
destroyEvent :: VkDevice ->  VkEvent ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyEvent = \device -> \event -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyEvent device event pAllocator)

-- | Wrapper for vkGetEventStatus
getEventStatus :: VkDevice ->  VkEvent ->  IO ( VkResult )
getEventStatus = \device -> \event -> vkGetEventStatus device event

-- | Wrapper for vkSetEvent
setEvent :: VkDevice ->  VkEvent ->  IO ( VkResult )
setEvent = \device -> \event -> vkSetEvent device event

-- | Wrapper for vkResetEvent
resetEvent :: VkDevice ->  VkEvent ->  IO ( VkResult )
resetEvent = \device -> \event -> vkResetEvent device event

-- | Wrapper for vkCreateQueryPool
createQueryPool :: VkDevice ->  VkQueryPoolCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                             , VkQueryPool )
createQueryPool = \device -> \createInfo -> \allocator -> alloca (\pQueryPool -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateQueryPool device pCreateInfo pAllocator pQueryPool >>= (\r -> (,) <$> pure r<*>peek pQueryPool))))

-- | Wrapper for vkDestroyQueryPool
destroyQueryPool :: VkDevice ->  VkQueryPool ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyQueryPool = \device -> \queryPool -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyQueryPool device queryPool pAllocator)

-- | Wrapper for vkCreateBuffer
createBuffer :: VkDevice ->  VkBufferCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                       , VkBuffer )
createBuffer = \device -> \createInfo -> \allocator -> alloca (\pBuffer -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateBuffer device pCreateInfo pAllocator pBuffer >>= (\r -> (,) <$> pure r<*>peek pBuffer))))

-- | Wrapper for vkDestroyBuffer
destroyBuffer :: VkDevice ->  VkBuffer ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyBuffer = \device -> \buffer -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyBuffer device buffer pAllocator)

-- | Wrapper for vkCreateBufferView
createBufferView :: VkDevice ->  VkBufferViewCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                               , VkBufferView )
createBufferView = \device -> \createInfo -> \allocator -> alloca (\pView -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateBufferView device pCreateInfo pAllocator pView >>= (\r -> (,) <$> pure r<*>peek pView))))

-- | Wrapper for vkDestroyBufferView
destroyBufferView :: VkDevice ->  VkBufferView ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyBufferView = \device -> \bufferView -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyBufferView device bufferView pAllocator)

-- | Wrapper for vkCreateImage
createImage :: VkDevice ->  VkImageCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                     , VkImage )
createImage = \device -> \createInfo -> \allocator -> alloca (\pImage -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateImage device pCreateInfo pAllocator pImage >>= (\r -> (,) <$> pure r<*>peek pImage))))

-- | Wrapper for vkDestroyImage
destroyImage :: VkDevice ->  VkImage ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyImage = \device -> \image -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyImage device image pAllocator)

-- | Wrapper for vkGetImageSubresourceLayout
getImageSubresourceLayout :: VkDevice ->  VkImage ->  VkImageSubresource ->  IO ( ()
                                                                                , VkSubresourceLayout )
getImageSubresourceLayout = \device -> \image -> \subresource -> alloca (\pLayout -> with subresource (\pSubresource -> vkGetImageSubresourceLayout device image pSubresource pLayout >>= (\r -> (,) <$> pure r<*>peek pLayout)))

-- | Wrapper for vkCreateImageView
createImageView :: VkDevice ->  VkImageViewCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                             , VkImageView )
createImageView = \device -> \createInfo -> \allocator -> alloca (\pView -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateImageView device pCreateInfo pAllocator pView >>= (\r -> (,) <$> pure r<*>peek pView))))

-- | Wrapper for vkDestroyImageView
destroyImageView :: VkDevice ->  VkImageView ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyImageView = \device -> \imageView -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyImageView device imageView pAllocator)

-- | Wrapper for vkCreateShaderModule
createShaderModule :: VkDevice ->  VkShaderModuleCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                   , VkShaderModule )
createShaderModule = \device -> \createInfo -> \allocator -> alloca (\pShaderModule -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateShaderModule device pCreateInfo pAllocator pShaderModule >>= (\r -> (,) <$> pure r<*>peek pShaderModule))))

-- | Wrapper for vkDestroyShaderModule
destroyShaderModule :: VkDevice ->  VkShaderModule ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyShaderModule = \device -> \shaderModule -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyShaderModule device shaderModule pAllocator)

-- | Wrapper for vkCreatePipelineCache
createPipelineCache :: VkDevice ->  VkPipelineCacheCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                     , VkPipelineCache )
createPipelineCache = \device -> \createInfo -> \allocator -> alloca (\pPipelineCache -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreatePipelineCache device pCreateInfo pAllocator pPipelineCache >>= (\r -> (,) <$> pure r<*>peek pPipelineCache))))

-- | Wrapper for vkDestroyPipelineCache
destroyPipelineCache :: VkDevice ->  VkPipelineCache ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyPipelineCache = \device -> \pipelineCache -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyPipelineCache device pipelineCache pAllocator)

-- | Wrapper for vkMergePipelineCaches
mergePipelineCaches :: VkDevice ->  VkPipelineCache ->  Vector VkPipelineCache ->  IO ( VkResult )
mergePipelineCaches = \device -> \dstCache -> \srcCaches -> unsafeWith srcCaches (\pSrcCaches -> vkMergePipelineCaches device dstCache (fromIntegral $ Data.Vector.Storable.length srcCaches) pSrcCaches)

-- | Wrapper for vkCreateGraphicsPipelines
createGraphicsPipelines :: VkDevice ->  VkPipelineCache ->  Vector VkGraphicsPipelineCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                                       , Vector VkPipeline )
createGraphicsPipelines = \device -> \pipelineCache -> \createInfos -> \allocator -> mallocForeignPtrArray (fromIntegral ((Data.Vector.Storable.length createInfos))) >>= (\fpPipelines -> withForeignPtr fpPipelines (\pPipelines -> maybeWith with allocator (\pAllocator -> unsafeWith createInfos (\pCreateInfos -> vkCreateGraphicsPipelines device pipelineCache (fromIntegral $ Data.Vector.Storable.length createInfos) pCreateInfos pAllocator pPipelines >>= (\r -> (,) <$> pure r<*>pure (unsafeFromForeignPtr0 fpPipelines (fromIntegral ((Data.Vector.Storable.length createInfos)))))))))

-- | Wrapper for vkCreateComputePipelines
createComputePipelines :: VkDevice ->  VkPipelineCache ->  Vector VkComputePipelineCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                                     , Vector VkPipeline )
createComputePipelines = \device -> \pipelineCache -> \createInfos -> \allocator -> mallocForeignPtrArray (fromIntegral ((Data.Vector.Storable.length createInfos))) >>= (\fpPipelines -> withForeignPtr fpPipelines (\pPipelines -> maybeWith with allocator (\pAllocator -> unsafeWith createInfos (\pCreateInfos -> vkCreateComputePipelines device pipelineCache (fromIntegral $ Data.Vector.Storable.length createInfos) pCreateInfos pAllocator pPipelines >>= (\r -> (,) <$> pure r<*>pure (unsafeFromForeignPtr0 fpPipelines (fromIntegral ((Data.Vector.Storable.length createInfos)))))))))

-- | Wrapper for vkDestroyPipeline
destroyPipeline :: VkDevice ->  VkPipeline ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyPipeline = \device -> \pipeline -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyPipeline device pipeline pAllocator)

-- | Wrapper for vkCreatePipelineLayout
createPipelineLayout :: VkDevice ->  VkPipelineLayoutCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                       , VkPipelineLayout )
createPipelineLayout = \device -> \createInfo -> \allocator -> alloca (\pPipelineLayout -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreatePipelineLayout device pCreateInfo pAllocator pPipelineLayout >>= (\r -> (,) <$> pure r<*>peek pPipelineLayout))))

-- | Wrapper for vkDestroyPipelineLayout
destroyPipelineLayout :: VkDevice ->  VkPipelineLayout ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyPipelineLayout = \device -> \pipelineLayout -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyPipelineLayout device pipelineLayout pAllocator)

-- | Wrapper for vkCreateSampler
createSampler :: VkDevice ->  VkSamplerCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                         , VkSampler )
createSampler = \device -> \createInfo -> \allocator -> alloca (\pSampler -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateSampler device pCreateInfo pAllocator pSampler >>= (\r -> (,) <$> pure r<*>peek pSampler))))

-- | Wrapper for vkDestroySampler
destroySampler :: VkDevice ->  VkSampler ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroySampler = \device -> \sampler -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroySampler device sampler pAllocator)

-- | Wrapper for vkCreateDescriptorSetLayout
createDescriptorSetLayout :: VkDevice ->  VkDescriptorSetLayoutCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                 , VkDescriptorSetLayout )
createDescriptorSetLayout = \device -> \createInfo -> \allocator -> alloca (\pSetLayout -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDescriptorSetLayout device pCreateInfo pAllocator pSetLayout >>= (\r -> (,) <$> pure r<*>peek pSetLayout))))

-- | Wrapper for vkDestroyDescriptorSetLayout
destroyDescriptorSetLayout :: VkDevice ->  VkDescriptorSetLayout ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyDescriptorSetLayout = \device -> \descriptorSetLayout -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyDescriptorSetLayout device descriptorSetLayout pAllocator)

-- | Wrapper for vkCreateDescriptorPool
createDescriptorPool :: VkDevice ->  VkDescriptorPoolCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                       , VkDescriptorPool )
createDescriptorPool = \device -> \createInfo -> \allocator -> alloca (\pDescriptorPool -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDescriptorPool device pCreateInfo pAllocator pDescriptorPool >>= (\r -> (,) <$> pure r<*>peek pDescriptorPool))))

-- | Wrapper for vkDestroyDescriptorPool
destroyDescriptorPool :: VkDevice ->  VkDescriptorPool ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyDescriptorPool = \device -> \descriptorPool -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyDescriptorPool device descriptorPool pAllocator)

-- | Wrapper for vkResetDescriptorPool
resetDescriptorPool :: VkDevice ->  VkDescriptorPool ->  VkDescriptorPoolResetFlags ->  IO ( VkResult )
resetDescriptorPool = \device -> \descriptorPool -> \flags -> vkResetDescriptorPool device descriptorPool flags

-- | Wrapper for vkAllocateDescriptorSets
allocateDescriptorSets :: VkDevice ->  VkDescriptorSetAllocateInfo ->  IO ( VkResult
                                                                          , Vector VkDescriptorSet )
allocateDescriptorSets = \device -> \allocateInfo -> mallocForeignPtrArray (fromIntegral (vkDescriptorSetCount allocateInfo)) >>= (\fpDescriptorSets -> withForeignPtr fpDescriptorSets (\pDescriptorSets -> with allocateInfo (\pAllocateInfo -> vkAllocateDescriptorSets device pAllocateInfo pDescriptorSets >>= (\r -> (,) <$> pure r<*>pure (unsafeFromForeignPtr0 fpDescriptorSets (fromIntegral (vkDescriptorSetCount allocateInfo)))))))

-- | Wrapper for vkFreeDescriptorSets
freeDescriptorSets :: VkDevice ->  VkDescriptorPool ->  Vector VkDescriptorSet ->  IO ( VkResult )
freeDescriptorSets = \device -> \descriptorPool -> \descriptorSets -> unsafeWith descriptorSets (\pDescriptorSets -> vkFreeDescriptorSets device descriptorPool (fromIntegral $ Data.Vector.Storable.length descriptorSets) pDescriptorSets)

-- | Wrapper for vkUpdateDescriptorSets
updateDescriptorSets :: VkDevice ->  Vector VkWriteDescriptorSet ->  Vector VkCopyDescriptorSet ->  IO ( () )
updateDescriptorSets = \device -> \descriptorWrites -> \descriptorCopies -> unsafeWith descriptorCopies (\pDescriptorCopies -> unsafeWith descriptorWrites (\pDescriptorWrites -> vkUpdateDescriptorSets device (fromIntegral $ Data.Vector.Storable.length descriptorWrites) pDescriptorWrites (fromIntegral $ Data.Vector.Storable.length descriptorCopies) pDescriptorCopies))

-- | Wrapper for vkCreateFramebuffer
createFramebuffer :: VkDevice ->  VkFramebufferCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                 , VkFramebuffer )
createFramebuffer = \device -> \createInfo -> \allocator -> alloca (\pFramebuffer -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateFramebuffer device pCreateInfo pAllocator pFramebuffer >>= (\r -> (,) <$> pure r<*>peek pFramebuffer))))

-- | Wrapper for vkDestroyFramebuffer
destroyFramebuffer :: VkDevice ->  VkFramebuffer ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyFramebuffer = \device -> \framebuffer -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyFramebuffer device framebuffer pAllocator)

-- | Wrapper for vkCreateRenderPass
createRenderPass :: VkDevice ->  VkRenderPassCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                               , VkRenderPass )
createRenderPass = \device -> \createInfo -> \allocator -> alloca (\pRenderPass -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateRenderPass device pCreateInfo pAllocator pRenderPass >>= (\r -> (,) <$> pure r<*>peek pRenderPass))))

-- | Wrapper for vkDestroyRenderPass
destroyRenderPass :: VkDevice ->  VkRenderPass ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyRenderPass = \device -> \renderPass -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyRenderPass device renderPass pAllocator)

-- | Wrapper for vkGetRenderAreaGranularity
getRenderAreaGranularity :: VkDevice ->  VkRenderPass ->  IO ( ()
                                                             , VkExtent2D )
getRenderAreaGranularity = \device -> \renderPass -> alloca (\pGranularity -> vkGetRenderAreaGranularity device renderPass pGranularity >>= (\r -> (,) <$> pure r<*>peek pGranularity))

-- | Wrapper for vkCreateCommandPool
createCommandPool :: VkDevice ->  VkCommandPoolCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                 , VkCommandPool )
createCommandPool = \device -> \createInfo -> \allocator -> alloca (\pCommandPool -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateCommandPool device pCreateInfo pAllocator pCommandPool >>= (\r -> (,) <$> pure r<*>peek pCommandPool))))

-- | Wrapper for vkDestroyCommandPool
destroyCommandPool :: VkDevice ->  VkCommandPool ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyCommandPool = \device -> \commandPool -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyCommandPool device commandPool pAllocator)

-- | Wrapper for vkResetCommandPool
resetCommandPool :: VkDevice ->  VkCommandPool ->  VkCommandPoolResetFlags ->  IO ( VkResult )
resetCommandPool = \device -> \commandPool -> \flags -> vkResetCommandPool device commandPool flags

-- | Wrapper for vkAllocateCommandBuffers
allocateCommandBuffers :: VkDevice ->  VkCommandBufferAllocateInfo ->  IO ( VkResult
                                                                          , Vector VkCommandBuffer )
allocateCommandBuffers = \device -> \allocateInfo -> mallocForeignPtrArray (fromIntegral (vkCommandBufferCount allocateInfo)) >>= (\fpCommandBuffers -> withForeignPtr fpCommandBuffers (\pCommandBuffers -> with allocateInfo (\pAllocateInfo -> vkAllocateCommandBuffers device pAllocateInfo pCommandBuffers >>= (\r -> (,) <$> pure r<*>pure (unsafeFromForeignPtr0 fpCommandBuffers (fromIntegral (vkCommandBufferCount allocateInfo)))))))

-- | Wrapper for vkFreeCommandBuffers
freeCommandBuffers :: VkDevice ->  VkCommandPool ->  Vector VkCommandBuffer ->  IO ( () )
freeCommandBuffers = \device -> \commandPool -> \commandBuffers -> unsafeWith commandBuffers (\pCommandBuffers -> vkFreeCommandBuffers device commandPool (fromIntegral $ Data.Vector.Storable.length commandBuffers) pCommandBuffers)

-- | Wrapper for vkBeginCommandBuffer
beginCommandBuffer :: VkCommandBuffer ->  VkCommandBufferBeginInfo ->  IO ( VkResult )
beginCommandBuffer = \commandBuffer -> \beginInfo -> with beginInfo (\pBeginInfo -> vkBeginCommandBuffer commandBuffer pBeginInfo)

-- | Wrapper for vkEndCommandBuffer
endCommandBuffer :: VkCommandBuffer ->  IO ( VkResult )
endCommandBuffer = \commandBuffer -> vkEndCommandBuffer commandBuffer

-- | Wrapper for vkResetCommandBuffer
resetCommandBuffer :: VkCommandBuffer ->  VkCommandBufferResetFlags ->  IO ( VkResult )
resetCommandBuffer = \commandBuffer -> \flags -> vkResetCommandBuffer commandBuffer flags

-- | Wrapper for vkCmdBindPipeline
cmdBindPipeline :: VkCommandBuffer ->  VkPipelineBindPoint ->  VkPipeline ->  IO ( () )
cmdBindPipeline = \commandBuffer -> \pipelineBindPoint -> \pipeline -> vkCmdBindPipeline commandBuffer pipelineBindPoint pipeline

-- | Wrapper for vkCmdSetViewport
cmdSetViewport :: VkCommandBuffer ->  Word32 ->  Vector VkViewport ->  IO ( () )
cmdSetViewport = \commandBuffer -> \firstViewport -> \viewports -> unsafeWith viewports (\pViewports -> vkCmdSetViewport commandBuffer firstViewport (fromIntegral $ Data.Vector.Storable.length viewports) pViewports)

-- | Wrapper for vkCmdSetScissor
cmdSetScissor :: VkCommandBuffer ->  Word32 ->  Vector VkRect2D ->  IO ( () )
cmdSetScissor = \commandBuffer -> \firstScissor -> \scissors -> unsafeWith scissors (\pScissors -> vkCmdSetScissor commandBuffer firstScissor (fromIntegral $ Data.Vector.Storable.length scissors) pScissors)

-- | Wrapper for vkCmdSetLineWidth
cmdSetLineWidth :: VkCommandBuffer ->  CFloat ->  IO ( () )
cmdSetLineWidth = \commandBuffer -> \lineWidth -> vkCmdSetLineWidth commandBuffer lineWidth

-- | Wrapper for vkCmdSetDepthBias
cmdSetDepthBias :: VkCommandBuffer ->  CFloat ->  CFloat ->  CFloat ->  IO ( () )
cmdSetDepthBias = \commandBuffer -> \depthBiasConstantFactor -> \depthBiasClamp -> \depthBiasSlopeFactor -> vkCmdSetDepthBias commandBuffer depthBiasConstantFactor depthBiasClamp depthBiasSlopeFactor

-- | Wrapper for vkCmdSetBlendConstants
cmdSetBlendConstants :: VkCommandBuffer ->  ( CFloat
                                            , CFloat
                                            , CFloat
                                            , CFloat ) ->  IO (())
cmdSetBlendConstants = \commandBuffer -> \( blendConstants0
                                          , blendConstants1
                                          , blendConstants2
                                          , blendConstants3 ) -> allocaArray 4 (\pBlendConstants -> pokeElemOff pBlendConstants 0 blendConstants0*> pokeElemOff pBlendConstants 1 blendConstants1*> pokeElemOff pBlendConstants 2 blendConstants2*> pokeElemOff pBlendConstants 3 blendConstants3 *> vkCmdSetBlendConstants commandBuffer pBlendConstants)

-- | Wrapper for vkCmdSetDepthBounds
cmdSetDepthBounds :: VkCommandBuffer ->  CFloat ->  CFloat ->  IO ( () )
cmdSetDepthBounds = \commandBuffer -> \minDepthBounds -> \maxDepthBounds -> vkCmdSetDepthBounds commandBuffer minDepthBounds maxDepthBounds

-- | Wrapper for vkCmdSetStencilCompareMask
cmdSetStencilCompareMask :: VkCommandBuffer ->  VkStencilFaceFlags ->  Word32 ->  IO ( () )
cmdSetStencilCompareMask = \commandBuffer -> \faceMask -> \compareMask -> vkCmdSetStencilCompareMask commandBuffer faceMask compareMask

-- | Wrapper for vkCmdSetStencilWriteMask
cmdSetStencilWriteMask :: VkCommandBuffer ->  VkStencilFaceFlags ->  Word32 ->  IO ( () )
cmdSetStencilWriteMask = \commandBuffer -> \faceMask -> \writeMask -> vkCmdSetStencilWriteMask commandBuffer faceMask writeMask

-- | Wrapper for vkCmdSetStencilReference
cmdSetStencilReference :: VkCommandBuffer ->  VkStencilFaceFlags ->  Word32 ->  IO ( () )
cmdSetStencilReference = \commandBuffer -> \faceMask -> \reference -> vkCmdSetStencilReference commandBuffer faceMask reference

-- | Wrapper for vkCmdBindDescriptorSets
cmdBindDescriptorSets :: VkCommandBuffer ->  VkPipelineBindPoint ->  VkPipelineLayout ->  Word32 ->  Vector VkDescriptorSet ->  Vector Word32 ->  IO ( () )
cmdBindDescriptorSets = \commandBuffer -> \pipelineBindPoint -> \layout -> \firstSet -> \descriptorSets -> \dynamicOffsets -> unsafeWith dynamicOffsets (\pDynamicOffsets -> unsafeWith descriptorSets (\pDescriptorSets -> vkCmdBindDescriptorSets commandBuffer pipelineBindPoint layout firstSet (fromIntegral $ Data.Vector.Storable.length descriptorSets) pDescriptorSets (fromIntegral $ Data.Vector.Storable.length dynamicOffsets) pDynamicOffsets))

-- | Wrapper for vkCmdBindIndexBuffer
cmdBindIndexBuffer :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  VkIndexType ->  IO ( () )
cmdBindIndexBuffer = \commandBuffer -> \buffer -> \offset -> \indexType -> vkCmdBindIndexBuffer commandBuffer buffer offset indexType

-- | Wrapper for vkCmdDraw
cmdDraw :: VkCommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ( () )
cmdDraw = \commandBuffer -> \vertexCount -> \instanceCount -> \firstVertex -> \firstInstance -> vkCmdDraw commandBuffer vertexCount instanceCount firstVertex firstInstance

-- | Wrapper for vkCmdDrawIndexed
cmdDrawIndexed :: VkCommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Int32 ->  Word32 ->  IO ( () )
cmdDrawIndexed = \commandBuffer -> \indexCount -> \instanceCount -> \firstIndex -> \vertexOffset -> \firstInstance -> vkCmdDrawIndexed commandBuffer indexCount instanceCount firstIndex vertexOffset firstInstance

-- | Wrapper for vkCmdDrawIndirect
cmdDrawIndirect :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  Word32 ->  Word32 ->  IO ( () )
cmdDrawIndirect = \commandBuffer -> \buffer -> \offset -> \drawCount -> \stride -> vkCmdDrawIndirect commandBuffer buffer offset drawCount stride

-- | Wrapper for vkCmdDrawIndexedIndirect
cmdDrawIndexedIndirect :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  Word32 ->  Word32 ->  IO ( () )
cmdDrawIndexedIndirect = \commandBuffer -> \buffer -> \offset -> \drawCount -> \stride -> vkCmdDrawIndexedIndirect commandBuffer buffer offset drawCount stride

-- | Wrapper for vkCmdDispatch
cmdDispatch :: VkCommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  IO ( () )
cmdDispatch = \commandBuffer -> \groupCountX -> \groupCountY -> \groupCountZ -> vkCmdDispatch commandBuffer groupCountX groupCountY groupCountZ

-- | Wrapper for vkCmdDispatchIndirect
cmdDispatchIndirect :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  IO ( () )
cmdDispatchIndirect = \commandBuffer -> \buffer -> \offset -> vkCmdDispatchIndirect commandBuffer buffer offset

-- | Wrapper for vkCmdCopyBuffer
cmdCopyBuffer :: VkCommandBuffer ->  VkBuffer ->  VkBuffer ->  Vector VkBufferCopy ->  IO ( () )
cmdCopyBuffer = \commandBuffer -> \srcBuffer -> \dstBuffer -> \regions -> unsafeWith regions (\pRegions -> vkCmdCopyBuffer commandBuffer srcBuffer dstBuffer (fromIntegral $ Data.Vector.Storable.length regions) pRegions)

-- | Wrapper for vkCmdCopyImage
cmdCopyImage :: VkCommandBuffer ->  VkImage ->  VkImageLayout ->  VkImage ->  VkImageLayout ->  Vector VkImageCopy ->  IO ( () )
cmdCopyImage = \commandBuffer -> \srcImage -> \srcImageLayout -> \dstImage -> \dstImageLayout -> \regions -> unsafeWith regions (\pRegions -> vkCmdCopyImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout (fromIntegral $ Data.Vector.Storable.length regions) pRegions)

-- | Wrapper for vkCmdBlitImage
cmdBlitImage :: VkCommandBuffer ->  VkImage ->  VkImageLayout ->  VkImage ->  VkImageLayout ->  Vector VkImageBlit ->  VkFilter ->  IO ( () )
cmdBlitImage = \commandBuffer -> \srcImage -> \srcImageLayout -> \dstImage -> \dstImageLayout -> \regions -> \filter -> unsafeWith regions (\pRegions -> vkCmdBlitImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout (fromIntegral $ Data.Vector.Storable.length regions) pRegions filter)

-- | Wrapper for vkCmdCopyBufferToImage
cmdCopyBufferToImage :: VkCommandBuffer ->  VkBuffer ->  VkImage ->  VkImageLayout ->  Vector VkBufferImageCopy ->  IO ( () )
cmdCopyBufferToImage = \commandBuffer -> \srcBuffer -> \dstImage -> \dstImageLayout -> \regions -> unsafeWith regions (\pRegions -> vkCmdCopyBufferToImage commandBuffer srcBuffer dstImage dstImageLayout (fromIntegral $ Data.Vector.Storable.length regions) pRegions)

-- | Wrapper for vkCmdCopyImageToBuffer
cmdCopyImageToBuffer :: VkCommandBuffer ->  VkImage ->  VkImageLayout ->  VkBuffer ->  Vector VkBufferImageCopy ->  IO ( () )
cmdCopyImageToBuffer = \commandBuffer -> \srcImage -> \srcImageLayout -> \dstBuffer -> \regions -> unsafeWith regions (\pRegions -> vkCmdCopyImageToBuffer commandBuffer srcImage srcImageLayout dstBuffer (fromIntegral $ Data.Vector.Storable.length regions) pRegions)

-- | Wrapper for vkCmdUpdateBuffer
cmdUpdateBuffer :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  Vector a ->  IO ( () )
cmdUpdateBuffer = \commandBuffer -> \dstBuffer -> \dstOffset -> \data' -> unsafeWith data' (\pData -> vkCmdUpdateBuffer commandBuffer dstBuffer dstOffset (fromIntegral $ Data.Vector.Storable.length data') (castPtr pData))

-- | Wrapper for vkCmdFillBuffer
cmdFillBuffer :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  VkDeviceSize ->  Word32 ->  IO ( () )
cmdFillBuffer = \commandBuffer -> \dstBuffer -> \dstOffset -> \size -> \data' -> vkCmdFillBuffer commandBuffer dstBuffer dstOffset size data'

-- | Wrapper for vkCmdClearColorImage
cmdClearColorImage :: VkCommandBuffer ->  VkImage ->  VkImageLayout ->  VkClearColorValue ->  Vector VkImageSubresourceRange ->  IO ( () )
cmdClearColorImage = \commandBuffer -> \image -> \imageLayout -> \color -> \ranges -> unsafeWith ranges (\pRanges -> with color (\pColor -> vkCmdClearColorImage commandBuffer image imageLayout pColor (fromIntegral $ Data.Vector.Storable.length ranges) pRanges))

-- | Wrapper for vkCmdClearDepthStencilImage
cmdClearDepthStencilImage :: VkCommandBuffer ->  VkImage ->  VkImageLayout ->  VkClearDepthStencilValue ->  Vector VkImageSubresourceRange ->  IO ( () )
cmdClearDepthStencilImage = \commandBuffer -> \image -> \imageLayout -> \depthStencil -> \ranges -> unsafeWith ranges (\pRanges -> with depthStencil (\pDepthStencil -> vkCmdClearDepthStencilImage commandBuffer image imageLayout pDepthStencil (fromIntegral $ Data.Vector.Storable.length ranges) pRanges))

-- | Wrapper for vkCmdClearAttachments
cmdClearAttachments :: VkCommandBuffer ->  Vector VkClearAttachment ->  Vector VkClearRect ->  IO ( () )
cmdClearAttachments = \commandBuffer -> \attachments -> \rects -> unsafeWith rects (\pRects -> unsafeWith attachments (\pAttachments -> vkCmdClearAttachments commandBuffer (fromIntegral $ Data.Vector.Storable.length attachments) pAttachments (fromIntegral $ Data.Vector.Storable.length rects) pRects))

-- | Wrapper for vkCmdResolveImage
cmdResolveImage :: VkCommandBuffer ->  VkImage ->  VkImageLayout ->  VkImage ->  VkImageLayout ->  Vector VkImageResolve ->  IO ( () )
cmdResolveImage = \commandBuffer -> \srcImage -> \srcImageLayout -> \dstImage -> \dstImageLayout -> \regions -> unsafeWith regions (\pRegions -> vkCmdResolveImage commandBuffer srcImage srcImageLayout dstImage dstImageLayout (fromIntegral $ Data.Vector.Storable.length regions) pRegions)

-- | Wrapper for vkCmdSetEvent
cmdSetEvent :: VkCommandBuffer ->  VkEvent ->  VkPipelineStageFlags ->  IO ( () )
cmdSetEvent = \commandBuffer -> \event -> \stageMask -> vkCmdSetEvent commandBuffer event stageMask

-- | Wrapper for vkCmdResetEvent
cmdResetEvent :: VkCommandBuffer ->  VkEvent ->  VkPipelineStageFlags ->  IO ( () )
cmdResetEvent = \commandBuffer -> \event -> \stageMask -> vkCmdResetEvent commandBuffer event stageMask

-- | Wrapper for vkCmdWaitEvents
cmdWaitEvents :: VkCommandBuffer ->  Vector VkEvent ->  VkPipelineStageFlags ->  VkPipelineStageFlags ->  Vector VkMemoryBarrier ->  Vector VkBufferMemoryBarrier ->  Vector VkImageMemoryBarrier ->  IO ( () )
cmdWaitEvents = \commandBuffer -> \events -> \srcStageMask -> \dstStageMask -> \memoryBarriers -> \bufferMemoryBarriers -> \imageMemoryBarriers -> unsafeWith imageMemoryBarriers (\pImageMemoryBarriers -> unsafeWith bufferMemoryBarriers (\pBufferMemoryBarriers -> unsafeWith memoryBarriers (\pMemoryBarriers -> unsafeWith events (\pEvents -> vkCmdWaitEvents commandBuffer (fromIntegral $ Data.Vector.Storable.length events) pEvents srcStageMask dstStageMask (fromIntegral $ Data.Vector.Storable.length memoryBarriers) pMemoryBarriers (fromIntegral $ Data.Vector.Storable.length bufferMemoryBarriers) pBufferMemoryBarriers (fromIntegral $ Data.Vector.Storable.length imageMemoryBarriers) pImageMemoryBarriers))))

-- | Wrapper for vkCmdPipelineBarrier
cmdPipelineBarrier :: VkCommandBuffer ->  VkPipelineStageFlags ->  VkPipelineStageFlags ->  VkDependencyFlags ->  Vector VkMemoryBarrier ->  Vector VkBufferMemoryBarrier ->  Vector VkImageMemoryBarrier ->  IO ( () )
cmdPipelineBarrier = \commandBuffer -> \srcStageMask -> \dstStageMask -> \dependencyFlags -> \memoryBarriers -> \bufferMemoryBarriers -> \imageMemoryBarriers -> unsafeWith imageMemoryBarriers (\pImageMemoryBarriers -> unsafeWith bufferMemoryBarriers (\pBufferMemoryBarriers -> unsafeWith memoryBarriers (\pMemoryBarriers -> vkCmdPipelineBarrier commandBuffer srcStageMask dstStageMask dependencyFlags (fromIntegral $ Data.Vector.Storable.length memoryBarriers) pMemoryBarriers (fromIntegral $ Data.Vector.Storable.length bufferMemoryBarriers) pBufferMemoryBarriers (fromIntegral $ Data.Vector.Storable.length imageMemoryBarriers) pImageMemoryBarriers)))

-- | Wrapper for vkCmdBeginQuery
cmdBeginQuery :: VkCommandBuffer ->  VkQueryPool ->  Word32 ->  VkQueryControlFlags ->  IO ( () )
cmdBeginQuery = \commandBuffer -> \queryPool -> \query -> \flags -> vkCmdBeginQuery commandBuffer queryPool query flags

-- | Wrapper for vkCmdEndQuery
cmdEndQuery :: VkCommandBuffer ->  VkQueryPool ->  Word32 ->  IO ( () )
cmdEndQuery = \commandBuffer -> \queryPool -> \query -> vkCmdEndQuery commandBuffer queryPool query

-- | Wrapper for vkCmdResetQueryPool
cmdResetQueryPool :: VkCommandBuffer ->  VkQueryPool ->  Word32 ->  Word32 ->  IO ( () )
cmdResetQueryPool = \commandBuffer -> \queryPool -> \firstQuery -> \queryCount -> vkCmdResetQueryPool commandBuffer queryPool firstQuery queryCount

-- | Wrapper for vkCmdWriteTimestamp
cmdWriteTimestamp :: VkCommandBuffer ->  VkPipelineStageFlagBits ->  VkQueryPool ->  Word32 ->  IO ( () )
cmdWriteTimestamp = \commandBuffer -> \pipelineStage -> \queryPool -> \query -> vkCmdWriteTimestamp commandBuffer pipelineStage queryPool query

-- | Wrapper for vkCmdCopyQueryPoolResults
cmdCopyQueryPoolResults :: VkCommandBuffer ->  VkQueryPool ->  Word32 ->  Word32 ->  VkBuffer ->  VkDeviceSize ->  VkDeviceSize ->  VkQueryResultFlags ->  IO ( () )
cmdCopyQueryPoolResults = \commandBuffer -> \queryPool -> \firstQuery -> \queryCount -> \dstBuffer -> \dstOffset -> \stride -> \flags -> vkCmdCopyQueryPoolResults commandBuffer queryPool firstQuery queryCount dstBuffer dstOffset stride flags

-- | Wrapper for vkCmdPushConstants
cmdPushConstants :: VkCommandBuffer ->  VkPipelineLayout ->  VkShaderStageFlags ->  Word32 ->  Vector a ->  IO ( () )
cmdPushConstants = \commandBuffer -> \layout -> \stageFlags -> \offset -> \values -> unsafeWith values (\pValues -> vkCmdPushConstants commandBuffer layout stageFlags offset (fromIntegral $ Data.Vector.Storable.length values) (castPtr pValues))

-- | Wrapper for vkCmdBeginRenderPass
cmdBeginRenderPass :: VkCommandBuffer ->  VkRenderPassBeginInfo ->  VkSubpassContents ->  IO ( () )
cmdBeginRenderPass = \commandBuffer -> \renderPassBegin -> \contents -> with renderPassBegin (\pRenderPassBegin -> vkCmdBeginRenderPass commandBuffer pRenderPassBegin contents)

-- | Wrapper for vkCmdNextSubpass
cmdNextSubpass :: VkCommandBuffer ->  VkSubpassContents ->  IO ( () )
cmdNextSubpass = \commandBuffer -> \contents -> vkCmdNextSubpass commandBuffer contents

-- | Wrapper for vkCmdEndRenderPass
cmdEndRenderPass :: VkCommandBuffer ->  IO ( () )
cmdEndRenderPass = \commandBuffer -> vkCmdEndRenderPass commandBuffer

-- | Wrapper for vkCmdExecuteCommands
cmdExecuteCommands :: VkCommandBuffer ->  Vector VkCommandBuffer ->  IO ( () )
cmdExecuteCommands = \commandBuffer -> \commandBuffers -> unsafeWith commandBuffers (\pCommandBuffers -> vkCmdExecuteCommands commandBuffer (fromIntegral $ Data.Vector.Storable.length commandBuffers) pCommandBuffers)

-- | Wrapper for vkCreateAndroidSurfaceKHR
createAndroidSurfaceKHR :: VkInstance ->  VkAndroidSurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                               , VkSurfaceKHR )
createAndroidSurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateAndroidSurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkGetPhysicalDeviceDisplayPropertiesKHR
getNumgetPhysicalDeviceDisplayPropertiesKHR :: VkPhysicalDevice ->  IO ( VkResult
                                                                       , Word32 )
getNumgetPhysicalDeviceDisplayPropertiesKHR = \physicalDevice -> alloca (\pPropertyCount -> vkGetPhysicalDeviceDisplayPropertiesKHR physicalDevice pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount))

-- | Wrapper for vkGetPhysicalDeviceDisplayPropertiesKHR
getPhysicalDeviceDisplayPropertiesKHR :: VkPhysicalDevice ->  Word32 ->  IO ( VkResult
                                                                            , Vector VkDisplayPropertiesKHR )
getPhysicalDeviceDisplayPropertiesKHR = \physicalDevice -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> vkGetPhysicalDeviceDisplayPropertiesKHR physicalDevice pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount)))))

-- | Wrapper for vkGetPhysicalDeviceDisplayPlanePropertiesKHR
getNumgetPhysicalDeviceDisplayPlanePropertiesKHR :: VkPhysicalDevice ->  IO ( VkResult
                                                                            , Word32 )
getNumgetPhysicalDeviceDisplayPlanePropertiesKHR = \physicalDevice -> alloca (\pPropertyCount -> vkGetPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount))

-- | Wrapper for vkGetPhysicalDeviceDisplayPlanePropertiesKHR
getPhysicalDeviceDisplayPlanePropertiesKHR :: VkPhysicalDevice ->  Word32 ->  IO ( VkResult
                                                                                 , Vector VkDisplayPlanePropertiesKHR )
getPhysicalDeviceDisplayPlanePropertiesKHR = \physicalDevice -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> vkGetPhysicalDeviceDisplayPlanePropertiesKHR physicalDevice pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount)))))

-- | Wrapper for vkGetDisplayPlaneSupportedDisplaysKHR
getNumgetDisplayPlaneSupportedDisplaysKHR :: VkPhysicalDevice ->  Word32 ->  IO ( VkResult
                                                                                , Word32 )
getNumgetDisplayPlaneSupportedDisplaysKHR = \physicalDevice -> \planeIndex -> alloca (\pDisplayCount -> vkGetDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex pDisplayCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pDisplayCount))

-- | Wrapper for vkGetDisplayPlaneSupportedDisplaysKHR
getDisplayPlaneSupportedDisplaysKHR :: VkPhysicalDevice ->  Word32 ->  Word32 ->  IO ( VkResult
                                                                                     , Vector VkDisplayKHR )
getDisplayPlaneSupportedDisplaysKHR = \physicalDevice -> \planeIndex -> \displayCount -> mallocForeignPtrArray (fromIntegral (displayCount)) >>= (\fpDisplays -> withForeignPtr fpDisplays (\pDisplays -> with displayCount (\pDisplayCount -> vkGetDisplayPlaneSupportedDisplaysKHR physicalDevice planeIndex pDisplayCount pDisplays >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpDisplays . fromIntegral <$> peek pDisplayCount)))))

-- | Wrapper for vkGetDisplayModePropertiesKHR
getNumgetDisplayModePropertiesKHR :: VkPhysicalDevice ->  VkDisplayKHR ->  IO ( VkResult
                                                                              , Word32 )
getNumgetDisplayModePropertiesKHR = \physicalDevice -> \display -> alloca (\pPropertyCount -> vkGetDisplayModePropertiesKHR physicalDevice display pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount))

-- | Wrapper for vkGetDisplayModePropertiesKHR
getDisplayModePropertiesKHR :: VkPhysicalDevice ->  VkDisplayKHR ->  Word32 ->  IO ( VkResult
                                                                                   , Vector VkDisplayModePropertiesKHR )
getDisplayModePropertiesKHR = \physicalDevice -> \display -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> vkGetDisplayModePropertiesKHR physicalDevice display pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount)))))

-- | Wrapper for vkCreateDisplayModeKHR
createDisplayModeKHR :: VkPhysicalDevice ->  VkDisplayKHR ->  VkDisplayModeCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                                , VkDisplayModeKHR )
createDisplayModeKHR = \physicalDevice -> \display -> \createInfo -> \allocator -> alloca (\pMode -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDisplayModeKHR physicalDevice display pCreateInfo pAllocator pMode >>= (\r -> (,) <$> pure r<*>peek pMode))))

-- | Wrapper for vkGetDisplayPlaneCapabilitiesKHR
getDisplayPlaneCapabilitiesKHR :: VkPhysicalDevice ->  VkDisplayModeKHR ->  Word32 ->  IO ( VkResult
                                                                                          , VkDisplayPlaneCapabilitiesKHR )
getDisplayPlaneCapabilitiesKHR = \physicalDevice -> \mode -> \planeIndex -> alloca (\pCapabilities -> vkGetDisplayPlaneCapabilitiesKHR physicalDevice mode planeIndex pCapabilities >>= (\r -> (,) <$> pure r<*>peek pCapabilities))

-- | Wrapper for vkCreateDisplayPlaneSurfaceKHR
createDisplayPlaneSurfaceKHR :: VkInstance ->  VkDisplaySurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                    , VkSurfaceKHR )
createDisplayPlaneSurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDisplayPlaneSurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkCreateSharedSwapchainsKHR
createSharedSwapchainsKHR :: VkDevice ->  Vector VkSwapchainCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                 , Vector VkSwapchainKHR )
createSharedSwapchainsKHR = \device -> \createInfos -> \allocator -> mallocForeignPtrArray (fromIntegral ((Data.Vector.Storable.length createInfos))) >>= (\fpSwapchains -> withForeignPtr fpSwapchains (\pSwapchains -> maybeWith with allocator (\pAllocator -> unsafeWith createInfos (\pCreateInfos -> vkCreateSharedSwapchainsKHR device (fromIntegral $ Data.Vector.Storable.length createInfos) pCreateInfos pAllocator pSwapchains >>= (\r -> (,) <$> pure r<*>pure (unsafeFromForeignPtr0 fpSwapchains (fromIntegral ((Data.Vector.Storable.length createInfos)))))))))

-- | Wrapper for vkCreateMirSurfaceKHR
createMirSurfaceKHR :: VkInstance ->  VkMirSurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                       , VkSurfaceKHR )
createMirSurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateMirSurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkGetPhysicalDeviceMirPresentationSupportKHR
getPhysicalDeviceMirPresentationSupportKHR :: VkPhysicalDevice ->  Word32 ->  IO ( VkBool32
                                                                                 , MirConnection )
getPhysicalDeviceMirPresentationSupportKHR = \physicalDevice -> \queueFamilyIndex -> alloca (\pConnection -> vkGetPhysicalDeviceMirPresentationSupportKHR physicalDevice queueFamilyIndex pConnection >>= (\r -> (,) <$> pure r<*>peek pConnection))

-- | Wrapper for vkDestroySurfaceKHR
destroySurfaceKHR :: VkInstance ->  VkSurfaceKHR ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroySurfaceKHR = \instance' -> \surface -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroySurfaceKHR instance' surface pAllocator)

-- | Wrapper for vkGetPhysicalDeviceSurfaceSupportKHR
getPhysicalDeviceSurfaceSupportKHR :: VkPhysicalDevice ->  Word32 ->  VkSurfaceKHR ->  IO ( VkResult
                                                                                          , VkBool32 )
getPhysicalDeviceSurfaceSupportKHR = \physicalDevice -> \queueFamilyIndex -> \surface -> alloca (\pSupported -> vkGetPhysicalDeviceSurfaceSupportKHR physicalDevice queueFamilyIndex surface pSupported >>= (\r -> (,) <$> pure r<*>peek pSupported))

-- | Wrapper for vkGetPhysicalDeviceSurfaceCapabilitiesKHR
getPhysicalDeviceSurfaceCapabilitiesKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  IO ( VkResult
                                                                                    , VkSurfaceCapabilitiesKHR )
getPhysicalDeviceSurfaceCapabilitiesKHR = \physicalDevice -> \surface -> alloca (\pSurfaceCapabilities -> vkGetPhysicalDeviceSurfaceCapabilitiesKHR physicalDevice surface pSurfaceCapabilities >>= (\r -> (,) <$> pure r<*>peek pSurfaceCapabilities))

-- | Wrapper for vkGetPhysicalDeviceSurfaceFormatsKHR
getNumgetPhysicalDeviceSurfaceFormatsKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  IO ( VkResult
                                                                                     , Word32 )
getNumgetPhysicalDeviceSurfaceFormatsKHR = \physicalDevice -> \surface -> alloca (\pSurfaceFormatCount -> vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface pSurfaceFormatCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pSurfaceFormatCount))

-- | Wrapper for vkGetPhysicalDeviceSurfaceFormatsKHR
getPhysicalDeviceSurfaceFormatsKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  Word32 ->  IO ( VkResult
                                                                                          , Vector VkSurfaceFormatKHR )
getPhysicalDeviceSurfaceFormatsKHR = \physicalDevice -> \surface -> \surfaceFormatCount -> mallocForeignPtrArray (fromIntegral (surfaceFormatCount)) >>= (\fpSurfaceFormats -> withForeignPtr fpSurfaceFormats (\pSurfaceFormats -> with surfaceFormatCount (\pSurfaceFormatCount -> vkGetPhysicalDeviceSurfaceFormatsKHR physicalDevice surface pSurfaceFormatCount pSurfaceFormats >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpSurfaceFormats . fromIntegral <$> peek pSurfaceFormatCount)))))

-- | Wrapper for vkGetPhysicalDeviceSurfacePresentModesKHR
getNumgetPhysicalDeviceSurfacePresentModesKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  IO ( VkResult
                                                                                          , Word32 )
getNumgetPhysicalDeviceSurfacePresentModesKHR = \physicalDevice -> \surface -> alloca (\pPresentModeCount -> vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface pPresentModeCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPresentModeCount))

-- | Wrapper for vkGetPhysicalDeviceSurfacePresentModesKHR
getPhysicalDeviceSurfacePresentModesKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  Word32 ->  IO ( VkResult
                                                                                               , Vector VkPresentModeKHR )
getPhysicalDeviceSurfacePresentModesKHR = \physicalDevice -> \surface -> \presentModeCount -> mallocForeignPtrArray (fromIntegral (presentModeCount)) >>= (\fpPresentModes -> withForeignPtr fpPresentModes (\pPresentModes -> with presentModeCount (\pPresentModeCount -> vkGetPhysicalDeviceSurfacePresentModesKHR physicalDevice surface pPresentModeCount pPresentModes >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpPresentModes . fromIntegral <$> peek pPresentModeCount)))))

-- | Wrapper for vkCreateSwapchainKHR
createSwapchainKHR :: VkDevice ->  VkSwapchainCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                   , VkSwapchainKHR )
createSwapchainKHR = \device -> \createInfo -> \allocator -> alloca (\pSwapchain -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateSwapchainKHR device pCreateInfo pAllocator pSwapchain >>= (\r -> (,) <$> pure r<*>peek pSwapchain))))

-- | Wrapper for vkDestroySwapchainKHR
destroySwapchainKHR :: VkDevice ->  VkSwapchainKHR ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroySwapchainKHR = \device -> \swapchain -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroySwapchainKHR device swapchain pAllocator)

-- | Wrapper for vkGetSwapchainImagesKHR
getNumgetSwapchainImagesKHR :: VkDevice ->  VkSwapchainKHR ->  IO ( VkResult
                                                                  , Word32 )
getNumgetSwapchainImagesKHR = \device -> \swapchain -> alloca (\pSwapchainImageCount -> vkGetSwapchainImagesKHR device swapchain pSwapchainImageCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pSwapchainImageCount))

-- | Wrapper for vkGetSwapchainImagesKHR
getSwapchainImagesKHR :: VkDevice ->  VkSwapchainKHR ->  Word32 ->  IO ( VkResult
                                                                       , Vector VkImage )
getSwapchainImagesKHR = \device -> \swapchain -> \swapchainImageCount -> mallocForeignPtrArray (fromIntegral (swapchainImageCount)) >>= (\fpSwapchainImages -> withForeignPtr fpSwapchainImages (\pSwapchainImages -> with swapchainImageCount (\pSwapchainImageCount -> vkGetSwapchainImagesKHR device swapchain pSwapchainImageCount pSwapchainImages >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpSwapchainImages . fromIntegral <$> peek pSwapchainImageCount)))))

-- | Wrapper for vkAcquireNextImageKHR
acquireNextImageKHR :: VkDevice ->  VkSwapchainKHR ->  Word64 ->  VkSemaphore ->  VkFence ->  IO ( VkResult
                                                                                                 , Word32 )
acquireNextImageKHR = \device -> \swapchain -> \timeout -> \semaphore -> \fence -> alloca (\pImageIndex -> vkAcquireNextImageKHR device swapchain timeout semaphore fence pImageIndex >>= (\r -> (,) <$> pure r<*>peek pImageIndex))

-- | Wrapper for vkQueuePresentKHR
queuePresentKHR :: VkQueue ->  VkPresentInfoKHR ->  IO ( VkResult )
queuePresentKHR = \queue -> \presentInfo -> with presentInfo (\pPresentInfo -> vkQueuePresentKHR queue pPresentInfo)

-- | Wrapper for vkCreateViSurfaceNN
createViSurfaceNN :: VkInstance ->  VkViSurfaceCreateInfoNN ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                   , VkSurfaceKHR )
createViSurfaceNN = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateViSurfaceNN instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkCreateWaylandSurfaceKHR
createWaylandSurfaceKHR :: VkInstance ->  VkWaylandSurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                               , VkSurfaceKHR )
createWaylandSurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateWaylandSurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkGetPhysicalDeviceWaylandPresentationSupportKHR
getPhysicalDeviceWaylandPresentationSupportKHR :: VkPhysicalDevice ->  Word32 ->  IO ( VkBool32
                                                                                     , Wl_display )
getPhysicalDeviceWaylandPresentationSupportKHR = \physicalDevice -> \queueFamilyIndex -> alloca (\pDisplay -> vkGetPhysicalDeviceWaylandPresentationSupportKHR physicalDevice queueFamilyIndex pDisplay >>= (\r -> (,) <$> pure r<*>peek pDisplay))

-- | Wrapper for vkCreateWin32SurfaceKHR
createWin32SurfaceKHR :: VkInstance ->  VkWin32SurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                           , VkSurfaceKHR )
createWin32SurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateWin32SurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkGetPhysicalDeviceWin32PresentationSupportKHR
getPhysicalDeviceWin32PresentationSupportKHR :: VkPhysicalDevice ->  Word32 ->  IO ( VkBool32 )
getPhysicalDeviceWin32PresentationSupportKHR = \physicalDevice -> \queueFamilyIndex -> vkGetPhysicalDeviceWin32PresentationSupportKHR physicalDevice queueFamilyIndex

-- | Wrapper for vkCreateXlibSurfaceKHR
createXlibSurfaceKHR :: VkInstance ->  VkXlibSurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                         , VkSurfaceKHR )
createXlibSurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateXlibSurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkGetPhysicalDeviceXlibPresentationSupportKHR
getPhysicalDeviceXlibPresentationSupportKHR :: VkPhysicalDevice ->  Word32 ->  VisualID ->  IO ( VkBool32
                                                                                               , Display )
getPhysicalDeviceXlibPresentationSupportKHR = \physicalDevice -> \queueFamilyIndex -> \visualID -> alloca (\pDpy -> vkGetPhysicalDeviceXlibPresentationSupportKHR physicalDevice queueFamilyIndex pDpy visualID >>= (\r -> (,) <$> pure r<*>peek pDpy))

-- | Wrapper for vkCreateXcbSurfaceKHR
createXcbSurfaceKHR :: VkInstance ->  VkXcbSurfaceCreateInfoKHR ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                       , VkSurfaceKHR )
createXcbSurfaceKHR = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateXcbSurfaceKHR instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkGetPhysicalDeviceXcbPresentationSupportKHR
getPhysicalDeviceXcbPresentationSupportKHR :: VkPhysicalDevice ->  Word32 ->  Xcb_visualid_t ->  IO ( VkBool32
                                                                                                    , Xcb_connection_t )
getPhysicalDeviceXcbPresentationSupportKHR = \physicalDevice -> \queueFamilyIndex -> \visual_id -> alloca (\pConnection -> vkGetPhysicalDeviceXcbPresentationSupportKHR physicalDevice queueFamilyIndex pConnection visual_id >>= (\r -> (,) <$> pure r<*>peek pConnection))

-- | Wrapper for vkCreateDebugReportCallbackEXT
createDebugReportCallbackEXT :: VkInstance ->  VkDebugReportCallbackCreateInfoEXT ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                         , VkDebugReportCallbackEXT )
createDebugReportCallbackEXT = \instance' -> \createInfo -> \allocator -> alloca (\pCallback -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDebugReportCallbackEXT instance' pCreateInfo pAllocator pCallback >>= (\r -> (,) <$> pure r<*>peek pCallback))))

-- | Wrapper for vkDestroyDebugReportCallbackEXT
destroyDebugReportCallbackEXT :: VkInstance ->  VkDebugReportCallbackEXT ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyDebugReportCallbackEXT = \instance' -> \callback -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyDebugReportCallbackEXT instance' callback pAllocator)

-- | Wrapper for vkDebugReportMessageEXT
debugReportMessageEXT :: VkInstance ->  VkDebugReportFlagsEXT ->  VkDebugReportObjectTypeEXT ->  Word64 ->  CSize ->  Int32 ->  ByteString ->  ByteString ->  IO ( () )
debugReportMessageEXT = \instance' -> \flags -> \objectType -> \object -> \location -> \messageCode -> \layerPrefix -> \message -> useAsCString message (\pMessage -> useAsCString layerPrefix (\pLayerPrefix -> vkDebugReportMessageEXT instance' flags objectType object location messageCode pLayerPrefix pMessage))

-- | Wrapper for vkDebugMarkerSetObjectNameEXT
debugMarkerSetObjectNameEXT :: VkDevice ->  VkDebugMarkerObjectNameInfoEXT ->  IO ( VkResult )
debugMarkerSetObjectNameEXT = \device -> \nameInfo -> with nameInfo (\pNameInfo -> vkDebugMarkerSetObjectNameEXT device pNameInfo)

-- | Wrapper for vkDebugMarkerSetObjectTagEXT
debugMarkerSetObjectTagEXT :: VkDevice ->  VkDebugMarkerObjectTagInfoEXT ->  IO ( VkResult )
debugMarkerSetObjectTagEXT = \device -> \tagInfo -> with tagInfo (\pTagInfo -> vkDebugMarkerSetObjectTagEXT device pTagInfo)

-- | Wrapper for vkCmdDebugMarkerBeginEXT
cmdDebugMarkerBeginEXT :: VkCommandBuffer ->  VkDebugMarkerMarkerInfoEXT ->  IO ( () )
cmdDebugMarkerBeginEXT = \commandBuffer -> \markerInfo -> with markerInfo (\pMarkerInfo -> vkCmdDebugMarkerBeginEXT commandBuffer pMarkerInfo)

-- | Wrapper for vkCmdDebugMarkerEndEXT
cmdDebugMarkerEndEXT :: VkCommandBuffer ->  IO ( () )
cmdDebugMarkerEndEXT = \commandBuffer -> vkCmdDebugMarkerEndEXT commandBuffer

-- | Wrapper for vkCmdDebugMarkerInsertEXT
cmdDebugMarkerInsertEXT :: VkCommandBuffer ->  VkDebugMarkerMarkerInfoEXT ->  IO ( () )
cmdDebugMarkerInsertEXT = \commandBuffer -> \markerInfo -> with markerInfo (\pMarkerInfo -> vkCmdDebugMarkerInsertEXT commandBuffer pMarkerInfo)

-- | Wrapper for vkGetPhysicalDeviceExternalImageFormatPropertiesNV
getPhysicalDeviceExternalImageFormatPropertiesNV :: VkPhysicalDevice ->  VkFormat ->  VkImageType ->  VkImageTiling ->  VkImageUsageFlags ->  VkImageCreateFlags ->  VkExternalMemoryHandleTypeFlagsNV ->  IO ( VkResult
                                                                                                                                                                                                              , VkExternalImageFormatPropertiesNV )
getPhysicalDeviceExternalImageFormatPropertiesNV = \physicalDevice -> \format -> \type' -> \tiling -> \usage -> \flags -> \externalHandleType -> alloca (\pExternalImageFormatProperties -> vkGetPhysicalDeviceExternalImageFormatPropertiesNV physicalDevice format type' tiling usage flags externalHandleType pExternalImageFormatProperties >>= (\r -> (,) <$> pure r<*>peek pExternalImageFormatProperties))

-- | Wrapper for vkGetMemoryWin32HandleNV
getMemoryWin32HandleNV :: VkDevice ->  VkDeviceMemory ->  VkExternalMemoryHandleTypeFlagsNV ->  IO ( VkResult
                                                                                                   , HANDLE )
getMemoryWin32HandleNV = \device -> \memory -> \handleType -> alloca (\pHandle -> vkGetMemoryWin32HandleNV device memory handleType pHandle >>= (\r -> (,) <$> pure r<*>peek pHandle))

-- | Wrapper for vkCmdDrawIndirectCountAMD
cmdDrawIndirectCountAMD :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  VkBuffer ->  VkDeviceSize ->  Word32 ->  Word32 ->  IO ( () )
cmdDrawIndirectCountAMD = \commandBuffer -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> vkCmdDrawIndirectCountAMD commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride

-- | Wrapper for vkCmdDrawIndexedIndirectCountAMD
cmdDrawIndexedIndirectCountAMD :: VkCommandBuffer ->  VkBuffer ->  VkDeviceSize ->  VkBuffer ->  VkDeviceSize ->  Word32 ->  Word32 ->  IO ( () )
cmdDrawIndexedIndirectCountAMD = \commandBuffer -> \buffer -> \offset -> \countBuffer -> \countBufferOffset -> \maxDrawCount -> \stride -> vkCmdDrawIndexedIndirectCountAMD commandBuffer buffer offset countBuffer countBufferOffset maxDrawCount stride

-- | Wrapper for vkCmdProcessCommandsNVX
cmdProcessCommandsNVX :: VkCommandBuffer ->  VkCmdProcessCommandsInfoNVX ->  IO ( () )
cmdProcessCommandsNVX = \commandBuffer -> \processCommandsInfo -> with processCommandsInfo (\pProcessCommandsInfo -> vkCmdProcessCommandsNVX commandBuffer pProcessCommandsInfo)

-- | Wrapper for vkCmdReserveSpaceForCommandsNVX
cmdReserveSpaceForCommandsNVX :: VkCommandBuffer ->  VkCmdReserveSpaceForCommandsInfoNVX ->  IO ( () )
cmdReserveSpaceForCommandsNVX = \commandBuffer -> \reserveSpaceInfo -> with reserveSpaceInfo (\pReserveSpaceInfo -> vkCmdReserveSpaceForCommandsNVX commandBuffer pReserveSpaceInfo)

-- | Wrapper for vkCreateIndirectCommandsLayoutNVX
createIndirectCommandsLayoutNVX :: VkDevice ->  VkIndirectCommandsLayoutCreateInfoNVX ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                             , VkIndirectCommandsLayoutNVX )
createIndirectCommandsLayoutNVX = \device -> \createInfo -> \allocator -> alloca (\pIndirectCommandsLayout -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateIndirectCommandsLayoutNVX device pCreateInfo pAllocator pIndirectCommandsLayout >>= (\r -> (,) <$> pure r<*>peek pIndirectCommandsLayout))))

-- | Wrapper for vkDestroyIndirectCommandsLayoutNVX
destroyIndirectCommandsLayoutNVX :: VkDevice ->  VkIndirectCommandsLayoutNVX ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyIndirectCommandsLayoutNVX = \device -> \indirectCommandsLayout -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyIndirectCommandsLayoutNVX device indirectCommandsLayout pAllocator)

-- | Wrapper for vkCreateObjectTableNVX
createObjectTableNVX :: VkDevice ->  VkObjectTableCreateInfoNVX ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                       , VkObjectTableNVX )
createObjectTableNVX = \device -> \createInfo -> \allocator -> alloca (\pObjectTable -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateObjectTableNVX device pCreateInfo pAllocator pObjectTable >>= (\r -> (,) <$> pure r<*>peek pObjectTable))))

-- | Wrapper for vkDestroyObjectTableNVX
destroyObjectTableNVX :: VkDevice ->  VkObjectTableNVX ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyObjectTableNVX = \device -> \objectTable -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyObjectTableNVX device objectTable pAllocator)

-- | Wrapper for vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
getPhysicalDeviceGeneratedCommandsPropertiesNVX :: VkPhysicalDevice ->  IO ( ()
                                                                           , VkDeviceGeneratedCommandsFeaturesNVX
                                                                           , VkDeviceGeneratedCommandsLimitsNVX )
getPhysicalDeviceGeneratedCommandsPropertiesNVX = \physicalDevice -> alloca (\pLimits -> alloca (\pFeatures -> vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX physicalDevice pFeatures pLimits >>= (\r -> (,,) <$> pure r<*>peek pFeatures<*>peek pLimits)))

-- | Wrapper for vkGetPhysicalDeviceFeatures2
getPhysicalDeviceFeatures2 :: VkPhysicalDevice ->  IO ( ()
                                                      , VkPhysicalDeviceFeatures2 )
getPhysicalDeviceFeatures2 = \physicalDevice -> alloca (\pFeatures -> vkGetPhysicalDeviceFeatures2 physicalDevice pFeatures >>= (\r -> (,) <$> pure r<*>peek pFeatures))

-- | Wrapper for vkGetPhysicalDeviceProperties2
getPhysicalDeviceProperties2 :: VkPhysicalDevice ->  IO ( ()
                                                        , VkPhysicalDeviceProperties2 )
getPhysicalDeviceProperties2 = \physicalDevice -> alloca (\pProperties -> vkGetPhysicalDeviceProperties2 physicalDevice pProperties >>= (\r -> (,) <$> pure r<*>peek pProperties))

-- | Wrapper for vkGetPhysicalDeviceFormatProperties2
getPhysicalDeviceFormatProperties2 :: VkPhysicalDevice ->  VkFormat ->  IO ( ()
                                                                           , VkFormatProperties2 )
getPhysicalDeviceFormatProperties2 = \physicalDevice -> \format -> alloca (\pFormatProperties -> vkGetPhysicalDeviceFormatProperties2 physicalDevice format pFormatProperties >>= (\r -> (,) <$> pure r<*>peek pFormatProperties))

-- | Wrapper for vkGetPhysicalDeviceImageFormatProperties2
getPhysicalDeviceImageFormatProperties2 :: VkPhysicalDevice ->  VkPhysicalDeviceImageFormatInfo2 ->  IO ( VkResult
                                                                                                        , VkImageFormatProperties2 )
getPhysicalDeviceImageFormatProperties2 = \physicalDevice -> \imageFormatInfo -> alloca (\pImageFormatProperties -> with imageFormatInfo (\pImageFormatInfo -> vkGetPhysicalDeviceImageFormatProperties2 physicalDevice pImageFormatInfo pImageFormatProperties >>= (\r -> (,) <$> pure r<*>peek pImageFormatProperties)))

-- | Wrapper for vkGetPhysicalDeviceQueueFamilyProperties2
getNumgetPhysicalDeviceQueueFamilyProperties2 :: VkPhysicalDevice ->  IO ( ()
                                                                         , Word32 )
getNumgetPhysicalDeviceQueueFamilyProperties2 = \physicalDevice -> alloca (\pQueueFamilyPropertyCount -> vkGetPhysicalDeviceQueueFamilyProperties2 physicalDevice pQueueFamilyPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pQueueFamilyPropertyCount))

-- | Wrapper for vkGetPhysicalDeviceQueueFamilyProperties2
getPhysicalDeviceQueueFamilyProperties2 :: VkPhysicalDevice ->  Word32 ->  IO ( ()
                                                                              , Vector VkQueueFamilyProperties2 )
getPhysicalDeviceQueueFamilyProperties2 = \physicalDevice -> \queueFamilyPropertyCount -> mallocForeignPtrArray (fromIntegral (queueFamilyPropertyCount)) >>= (\fpQueueFamilyProperties -> withForeignPtr fpQueueFamilyProperties (\pQueueFamilyProperties -> with queueFamilyPropertyCount (\pQueueFamilyPropertyCount -> vkGetPhysicalDeviceQueueFamilyProperties2 physicalDevice pQueueFamilyPropertyCount pQueueFamilyProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpQueueFamilyProperties . fromIntegral <$> peek pQueueFamilyPropertyCount)))))

-- | Wrapper for vkGetPhysicalDeviceMemoryProperties2
getPhysicalDeviceMemoryProperties2 :: VkPhysicalDevice ->  IO ( ()
                                                              , VkPhysicalDeviceMemoryProperties2 )
getPhysicalDeviceMemoryProperties2 = \physicalDevice -> alloca (\pMemoryProperties -> vkGetPhysicalDeviceMemoryProperties2 physicalDevice pMemoryProperties >>= (\r -> (,) <$> pure r<*>peek pMemoryProperties))

-- | Wrapper for vkGetPhysicalDeviceSparseImageFormatProperties2
getNumgetPhysicalDeviceSparseImageFormatProperties2 :: VkPhysicalDevice ->  VkPhysicalDeviceSparseImageFormatInfo2 ->  IO ( ()
                                                                                                                          , Word32 )
getNumgetPhysicalDeviceSparseImageFormatProperties2 = \physicalDevice -> \formatInfo -> alloca (\pPropertyCount -> with formatInfo (\pFormatInfo -> vkGetPhysicalDeviceSparseImageFormatProperties2 physicalDevice pFormatInfo pPropertyCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPropertyCount)))

-- | Wrapper for vkGetPhysicalDeviceSparseImageFormatProperties2
getPhysicalDeviceSparseImageFormatProperties2 :: VkPhysicalDevice ->  VkPhysicalDeviceSparseImageFormatInfo2 ->  Word32 ->  IO ( ()
                                                                                                                               , Vector VkSparseImageFormatProperties2 )
getPhysicalDeviceSparseImageFormatProperties2 = \physicalDevice -> \formatInfo -> \propertyCount -> mallocForeignPtrArray (fromIntegral (propertyCount)) >>= (\fpProperties -> withForeignPtr fpProperties (\pProperties -> with propertyCount (\pPropertyCount -> with formatInfo (\pFormatInfo -> vkGetPhysicalDeviceSparseImageFormatProperties2 physicalDevice pFormatInfo pPropertyCount pProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpProperties . fromIntegral <$> peek pPropertyCount))))))

-- | Wrapper for vkCmdPushDescriptorSetKHR
cmdPushDescriptorSetKHR :: VkCommandBuffer ->  VkPipelineBindPoint ->  VkPipelineLayout ->  Word32 ->  Vector VkWriteDescriptorSet ->  IO ( () )
cmdPushDescriptorSetKHR = \commandBuffer -> \pipelineBindPoint -> \layout -> \set -> \descriptorWrites -> unsafeWith descriptorWrites (\pDescriptorWrites -> vkCmdPushDescriptorSetKHR commandBuffer pipelineBindPoint layout set (fromIntegral $ Data.Vector.Storable.length descriptorWrites) pDescriptorWrites)

-- | Wrapper for vkTrimCommandPool
trimCommandPool :: VkDevice ->  VkCommandPool ->  VkCommandPoolTrimFlags ->  IO ( () )
trimCommandPool = \device -> \commandPool -> \flags -> vkTrimCommandPool device commandPool flags

-- | Wrapper for vkGetPhysicalDeviceExternalBufferProperties
getPhysicalDeviceExternalBufferProperties :: VkPhysicalDevice ->  VkPhysicalDeviceExternalBufferInfo ->  IO ( ()
                                                                                                            , VkExternalBufferProperties )
getPhysicalDeviceExternalBufferProperties = \physicalDevice -> \externalBufferInfo -> alloca (\pExternalBufferProperties -> with externalBufferInfo (\pExternalBufferInfo -> vkGetPhysicalDeviceExternalBufferProperties physicalDevice pExternalBufferInfo pExternalBufferProperties >>= (\r -> (,) <$> pure r<*>peek pExternalBufferProperties)))

-- | Wrapper for vkGetMemoryWin32HandleKHR
getMemoryWin32HandleKHR :: VkDevice ->  VkMemoryGetWin32HandleInfoKHR ->  IO ( VkResult
                                                                             , HANDLE )
getMemoryWin32HandleKHR = \device -> \getWin32HandleInfo -> alloca (\pHandle -> with getWin32HandleInfo (\pGetWin32HandleInfo -> vkGetMemoryWin32HandleKHR device pGetWin32HandleInfo pHandle >>= (\r -> (,) <$> pure r<*>peek pHandle)))

-- | Wrapper for vkGetMemoryWin32HandlePropertiesKHR
getMemoryWin32HandlePropertiesKHR :: VkDevice ->  VkExternalMemoryHandleTypeFlagBits ->  HANDLE ->  IO ( VkResult
                                                                                                       , VkMemoryWin32HandlePropertiesKHR )
getMemoryWin32HandlePropertiesKHR = \device -> \handleType -> \handle -> alloca (\pMemoryWin32HandleProperties -> vkGetMemoryWin32HandlePropertiesKHR device handleType handle pMemoryWin32HandleProperties >>= (\r -> (,) <$> pure r<*>peek pMemoryWin32HandleProperties))

-- | Wrapper for vkGetMemoryFdKHR
getMemoryFdKHR :: VkDevice ->  VkMemoryGetFdInfoKHR ->  IO ( VkResult
                                                           , CInt )
getMemoryFdKHR = \device -> \getFdInfo -> alloca (\pFd -> with getFdInfo (\pGetFdInfo -> vkGetMemoryFdKHR device pGetFdInfo pFd >>= (\r -> (,) <$> pure r<*>peek pFd)))

-- | Wrapper for vkGetMemoryFdPropertiesKHR
getMemoryFdPropertiesKHR :: VkDevice ->  VkExternalMemoryHandleTypeFlagBits ->  CInt ->  IO ( VkResult
                                                                                            , VkMemoryFdPropertiesKHR )
getMemoryFdPropertiesKHR = \device -> \handleType -> \fd -> alloca (\pMemoryFdProperties -> vkGetMemoryFdPropertiesKHR device handleType fd pMemoryFdProperties >>= (\r -> (,) <$> pure r<*>peek pMemoryFdProperties))

-- | Wrapper for vkGetPhysicalDeviceExternalSemaphoreProperties
getPhysicalDeviceExternalSemaphoreProperties :: VkPhysicalDevice ->  VkPhysicalDeviceExternalSemaphoreInfo ->  IO ( ()
                                                                                                                  , VkExternalSemaphoreProperties )
getPhysicalDeviceExternalSemaphoreProperties = \physicalDevice -> \externalSemaphoreInfo -> alloca (\pExternalSemaphoreProperties -> with externalSemaphoreInfo (\pExternalSemaphoreInfo -> vkGetPhysicalDeviceExternalSemaphoreProperties physicalDevice pExternalSemaphoreInfo pExternalSemaphoreProperties >>= (\r -> (,) <$> pure r<*>peek pExternalSemaphoreProperties)))

-- | Wrapper for vkGetSemaphoreWin32HandleKHR
getSemaphoreWin32HandleKHR :: VkDevice ->  VkSemaphoreGetWin32HandleInfoKHR ->  IO ( VkResult
                                                                                   , HANDLE )
getSemaphoreWin32HandleKHR = \device -> \getWin32HandleInfo -> alloca (\pHandle -> with getWin32HandleInfo (\pGetWin32HandleInfo -> vkGetSemaphoreWin32HandleKHR device pGetWin32HandleInfo pHandle >>= (\r -> (,) <$> pure r<*>peek pHandle)))

-- | Wrapper for vkImportSemaphoreWin32HandleKHR
importSemaphoreWin32HandleKHR :: VkDevice ->  VkImportSemaphoreWin32HandleInfoKHR ->  IO ( VkResult )
importSemaphoreWin32HandleKHR = \device -> \importSemaphoreWin32HandleInfo -> with importSemaphoreWin32HandleInfo (\pImportSemaphoreWin32HandleInfo -> vkImportSemaphoreWin32HandleKHR device pImportSemaphoreWin32HandleInfo)

-- | Wrapper for vkGetSemaphoreFdKHR
getSemaphoreFdKHR :: VkDevice ->  VkSemaphoreGetFdInfoKHR ->  IO ( VkResult
                                                                 , CInt )
getSemaphoreFdKHR = \device -> \getFdInfo -> alloca (\pFd -> with getFdInfo (\pGetFdInfo -> vkGetSemaphoreFdKHR device pGetFdInfo pFd >>= (\r -> (,) <$> pure r<*>peek pFd)))

-- | Wrapper for vkImportSemaphoreFdKHR
importSemaphoreFdKHR :: VkDevice ->  VkImportSemaphoreFdInfoKHR ->  IO ( VkResult )
importSemaphoreFdKHR = \device -> \importSemaphoreFdInfo -> with importSemaphoreFdInfo (\pImportSemaphoreFdInfo -> vkImportSemaphoreFdKHR device pImportSemaphoreFdInfo)

-- | Wrapper for vkGetPhysicalDeviceExternalFenceProperties
getPhysicalDeviceExternalFenceProperties :: VkPhysicalDevice ->  VkPhysicalDeviceExternalFenceInfo ->  IO ( ()
                                                                                                          , VkExternalFenceProperties )
getPhysicalDeviceExternalFenceProperties = \physicalDevice -> \externalFenceInfo -> alloca (\pExternalFenceProperties -> with externalFenceInfo (\pExternalFenceInfo -> vkGetPhysicalDeviceExternalFenceProperties physicalDevice pExternalFenceInfo pExternalFenceProperties >>= (\r -> (,) <$> pure r<*>peek pExternalFenceProperties)))

-- | Wrapper for vkGetFenceWin32HandleKHR
getFenceWin32HandleKHR :: VkDevice ->  VkFenceGetWin32HandleInfoKHR ->  IO ( VkResult
                                                                           , HANDLE )
getFenceWin32HandleKHR = \device -> \getWin32HandleInfo -> alloca (\pHandle -> with getWin32HandleInfo (\pGetWin32HandleInfo -> vkGetFenceWin32HandleKHR device pGetWin32HandleInfo pHandle >>= (\r -> (,) <$> pure r<*>peek pHandle)))

-- | Wrapper for vkImportFenceWin32HandleKHR
importFenceWin32HandleKHR :: VkDevice ->  VkImportFenceWin32HandleInfoKHR ->  IO ( VkResult )
importFenceWin32HandleKHR = \device -> \importFenceWin32HandleInfo -> with importFenceWin32HandleInfo (\pImportFenceWin32HandleInfo -> vkImportFenceWin32HandleKHR device pImportFenceWin32HandleInfo)

-- | Wrapper for vkGetFenceFdKHR
getFenceFdKHR :: VkDevice ->  VkFenceGetFdInfoKHR ->  IO ( VkResult
                                                         , CInt )
getFenceFdKHR = \device -> \getFdInfo -> alloca (\pFd -> with getFdInfo (\pGetFdInfo -> vkGetFenceFdKHR device pGetFdInfo pFd >>= (\r -> (,) <$> pure r<*>peek pFd)))

-- | Wrapper for vkImportFenceFdKHR
importFenceFdKHR :: VkDevice ->  VkImportFenceFdInfoKHR ->  IO ( VkResult )
importFenceFdKHR = \device -> \importFenceFdInfo -> with importFenceFdInfo (\pImportFenceFdInfo -> vkImportFenceFdKHR device pImportFenceFdInfo)

-- | Wrapper for vkReleaseDisplayEXT
releaseDisplayEXT :: VkPhysicalDevice ->  VkDisplayKHR ->  IO ( VkResult )
releaseDisplayEXT = \physicalDevice -> \display -> vkReleaseDisplayEXT physicalDevice display

-- | Wrapper for vkAcquireXlibDisplayEXT
acquireXlibDisplayEXT :: VkPhysicalDevice ->  VkDisplayKHR ->  IO ( VkResult
                                                                  , Display )
acquireXlibDisplayEXT = \physicalDevice -> \display -> alloca (\pDpy -> vkAcquireXlibDisplayEXT physicalDevice pDpy display >>= (\r -> (,) <$> pure r<*>peek pDpy))

-- | Wrapper for vkGetRandROutputDisplayEXT
getRandROutputDisplayEXT :: VkPhysicalDevice ->  RROutput ->  IO ( VkResult
                                                                 , Display
                                                                 , VkDisplayKHR )
getRandROutputDisplayEXT = \physicalDevice -> \rrOutput -> alloca (\pDisplay -> alloca (\pDpy -> vkGetRandROutputDisplayEXT physicalDevice pDpy rrOutput pDisplay >>= (\r -> (,,) <$> pure r<*>peek pDpy<*>peek pDisplay)))

-- | Wrapper for vkDisplayPowerControlEXT
displayPowerControlEXT :: VkDevice ->  VkDisplayKHR ->  VkDisplayPowerInfoEXT ->  IO ( VkResult )
displayPowerControlEXT = \device -> \display -> \displayPowerInfo -> with displayPowerInfo (\pDisplayPowerInfo -> vkDisplayPowerControlEXT device display pDisplayPowerInfo)

-- | Wrapper for vkRegisterDeviceEventEXT
registerDeviceEventEXT :: VkDevice ->  VkDeviceEventInfoEXT ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                   , VkFence )
registerDeviceEventEXT = \device -> \deviceEventInfo -> \allocator -> alloca (\pFence -> maybeWith with allocator (\pAllocator -> with deviceEventInfo (\pDeviceEventInfo -> vkRegisterDeviceEventEXT device pDeviceEventInfo pAllocator pFence >>= (\r -> (,) <$> pure r<*>peek pFence))))

-- | Wrapper for vkRegisterDisplayEventEXT
registerDisplayEventEXT :: VkDevice ->  VkDisplayKHR ->  VkDisplayEventInfoEXT ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                      , VkFence )
registerDisplayEventEXT = \device -> \display -> \displayEventInfo -> \allocator -> alloca (\pFence -> maybeWith with allocator (\pAllocator -> with displayEventInfo (\pDisplayEventInfo -> vkRegisterDisplayEventEXT device display pDisplayEventInfo pAllocator pFence >>= (\r -> (,) <$> pure r<*>peek pFence))))

-- | Wrapper for vkGetSwapchainCounterEXT
getSwapchainCounterEXT :: VkDevice ->  VkSwapchainKHR ->  VkSurfaceCounterFlagBitsEXT ->  IO ( VkResult
                                                                                             , Word64 )
getSwapchainCounterEXT = \device -> \swapchain -> \counter -> alloca (\pCounterValue -> vkGetSwapchainCounterEXT device swapchain counter pCounterValue >>= (\r -> (,) <$> pure r<*>peek pCounterValue))

-- | Wrapper for vkGetPhysicalDeviceSurfaceCapabilities2EXT
getPhysicalDeviceSurfaceCapabilities2EXT :: VkPhysicalDevice ->  VkSurfaceKHR ->  IO ( VkResult
                                                                                     , VkSurfaceCapabilities2EXT )
getPhysicalDeviceSurfaceCapabilities2EXT = \physicalDevice -> \surface -> alloca (\pSurfaceCapabilities -> vkGetPhysicalDeviceSurfaceCapabilities2EXT physicalDevice surface pSurfaceCapabilities >>= (\r -> (,) <$> pure r<*>peek pSurfaceCapabilities))

-- | Wrapper for vkEnumeratePhysicalDeviceGroups
getNumenumeratePhysicalDeviceGroups :: VkInstance ->  IO ( VkResult
                                                         , Word32 )
getNumenumeratePhysicalDeviceGroups = \instance' -> alloca (\pPhysicalDeviceGroupCount -> vkEnumeratePhysicalDeviceGroups instance' pPhysicalDeviceGroupCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPhysicalDeviceGroupCount))

-- | Wrapper for vkEnumeratePhysicalDeviceGroups
enumeratePhysicalDeviceGroups :: VkInstance ->  Word32 ->  IO ( VkResult
                                                              , Vector VkPhysicalDeviceGroupProperties )
enumeratePhysicalDeviceGroups = \instance' -> \physicalDeviceGroupCount -> mallocForeignPtrArray (fromIntegral (physicalDeviceGroupCount)) >>= (\fpPhysicalDeviceGroupProperties -> withForeignPtr fpPhysicalDeviceGroupProperties (\pPhysicalDeviceGroupProperties -> with physicalDeviceGroupCount (\pPhysicalDeviceGroupCount -> vkEnumeratePhysicalDeviceGroups instance' pPhysicalDeviceGroupCount pPhysicalDeviceGroupProperties >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpPhysicalDeviceGroupProperties . fromIntegral <$> peek pPhysicalDeviceGroupCount)))))

-- | Wrapper for vkGetDeviceGroupPeerMemoryFeatures
getDeviceGroupPeerMemoryFeatures :: VkDevice ->  Word32 ->  Word32 ->  Word32 ->  IO ( ()
                                                                                     , VkPeerMemoryFeatureFlags )
getDeviceGroupPeerMemoryFeatures = \device -> \heapIndex -> \localDeviceIndex -> \remoteDeviceIndex -> alloca (\pPeerMemoryFeatures -> vkGetDeviceGroupPeerMemoryFeatures device heapIndex localDeviceIndex remoteDeviceIndex pPeerMemoryFeatures >>= (\r -> (,) <$> pure r<*>peek pPeerMemoryFeatures))

-- | Wrapper for vkBindBufferMemory2
bindBufferMemory2 :: VkDevice ->  Vector VkBindBufferMemoryInfo ->  IO ( VkResult )
bindBufferMemory2 = \device -> \bindInfos -> unsafeWith bindInfos (\pBindInfos -> vkBindBufferMemory2 device (fromIntegral $ Data.Vector.Storable.length bindInfos) pBindInfos)

-- | Wrapper for vkBindImageMemory2
bindImageMemory2 :: VkDevice ->  Vector VkBindImageMemoryInfo ->  IO ( VkResult )
bindImageMemory2 = \device -> \bindInfos -> unsafeWith bindInfos (\pBindInfos -> vkBindImageMemory2 device (fromIntegral $ Data.Vector.Storable.length bindInfos) pBindInfos)

-- | Wrapper for vkCmdSetDeviceMask
cmdSetDeviceMask :: VkCommandBuffer ->  Word32 ->  IO ( () )
cmdSetDeviceMask = \commandBuffer -> \deviceMask -> vkCmdSetDeviceMask commandBuffer deviceMask

-- | Wrapper for vkGetDeviceGroupPresentCapabilitiesKHR
getDeviceGroupPresentCapabilitiesKHR :: VkDevice ->  IO ( VkResult
                                                        , VkDeviceGroupPresentCapabilitiesKHR )
getDeviceGroupPresentCapabilitiesKHR = \device -> alloca (\pDeviceGroupPresentCapabilities -> vkGetDeviceGroupPresentCapabilitiesKHR device pDeviceGroupPresentCapabilities >>= (\r -> (,) <$> pure r<*>peek pDeviceGroupPresentCapabilities))

-- | Wrapper for vkGetDeviceGroupSurfacePresentModesKHR
getDeviceGroupSurfacePresentModesKHR :: VkDevice ->  VkSurfaceKHR ->  IO ( VkResult
                                                                         , VkDeviceGroupPresentModeFlagsKHR )
getDeviceGroupSurfacePresentModesKHR = \device -> \surface -> alloca (\pModes -> vkGetDeviceGroupSurfacePresentModesKHR device surface pModes >>= (\r -> (,) <$> pure r<*>peek pModes))

-- | Wrapper for vkAcquireNextImage2KHR
acquireNextImage2KHR :: VkDevice ->  VkAcquireNextImageInfoKHR ->  IO ( VkResult
                                                                      , Word32 )
acquireNextImage2KHR = \device -> \acquireInfo -> alloca (\pImageIndex -> with acquireInfo (\pAcquireInfo -> vkAcquireNextImage2KHR device pAcquireInfo pImageIndex >>= (\r -> (,) <$> pure r<*>peek pImageIndex)))

-- | Wrapper for vkCmdDispatchBase
cmdDispatchBase :: VkCommandBuffer ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  Word32 ->  IO ( () )
cmdDispatchBase = \commandBuffer -> \baseGroupX -> \baseGroupY -> \baseGroupZ -> \groupCountX -> \groupCountY -> \groupCountZ -> vkCmdDispatchBase commandBuffer baseGroupX baseGroupY baseGroupZ groupCountX groupCountY groupCountZ

-- | Wrapper for vkGetPhysicalDevicePresentRectanglesKHR
getNumgetPhysicalDevicePresentRectanglesKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  IO ( VkResult
                                                                                        , Word32 )
getNumgetPhysicalDevicePresentRectanglesKHR = \physicalDevice -> \surface -> alloca (\pRectCount -> vkGetPhysicalDevicePresentRectanglesKHR physicalDevice surface pRectCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pRectCount))

-- | Wrapper for vkGetPhysicalDevicePresentRectanglesKHR
getPhysicalDevicePresentRectanglesKHR :: VkPhysicalDevice ->  VkSurfaceKHR ->  Word32 ->  IO ( VkResult
                                                                                             , Vector VkRect2D )
getPhysicalDevicePresentRectanglesKHR = \physicalDevice -> \surface -> \rectCount -> mallocForeignPtrArray (fromIntegral (rectCount)) >>= (\fpRects -> withForeignPtr fpRects (\pRects -> with rectCount (\pRectCount -> vkGetPhysicalDevicePresentRectanglesKHR physicalDevice surface pRectCount pRects >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpRects . fromIntegral <$> peek pRectCount)))))

-- | Wrapper for vkCreateDescriptorUpdateTemplate
createDescriptorUpdateTemplate :: VkDevice ->  VkDescriptorUpdateTemplateCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                           , VkDescriptorUpdateTemplate )
createDescriptorUpdateTemplate = \device -> \createInfo -> \allocator -> alloca (\pDescriptorUpdateTemplate -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDescriptorUpdateTemplate device pCreateInfo pAllocator pDescriptorUpdateTemplate >>= (\r -> (,) <$> pure r<*>peek pDescriptorUpdateTemplate))))

-- | Wrapper for vkDestroyDescriptorUpdateTemplate
destroyDescriptorUpdateTemplate :: VkDevice ->  VkDescriptorUpdateTemplate ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyDescriptorUpdateTemplate = \device -> \descriptorUpdateTemplate -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyDescriptorUpdateTemplate device descriptorUpdateTemplate pAllocator)

-- | Wrapper for vkGetSwapchainStatusKHR
getSwapchainStatusKHR :: VkDevice ->  VkSwapchainKHR ->  IO ( VkResult )
getSwapchainStatusKHR = \device -> \swapchain -> vkGetSwapchainStatusKHR device swapchain

-- | Wrapper for vkGetRefreshCycleDurationGOOGLE
getRefreshCycleDurationGOOGLE :: VkDevice ->  VkSwapchainKHR ->  IO ( VkResult
                                                                    , VkRefreshCycleDurationGOOGLE )
getRefreshCycleDurationGOOGLE = \device -> \swapchain -> alloca (\pDisplayTimingProperties -> vkGetRefreshCycleDurationGOOGLE device swapchain pDisplayTimingProperties >>= (\r -> (,) <$> pure r<*>peek pDisplayTimingProperties))

-- | Wrapper for vkGetPastPresentationTimingGOOGLE
getNumgetPastPresentationTimingGOOGLE :: VkDevice ->  VkSwapchainKHR ->  IO ( VkResult
                                                                            , Word32 )
getNumgetPastPresentationTimingGOOGLE = \device -> \swapchain -> alloca (\pPresentationTimingCount -> vkGetPastPresentationTimingGOOGLE device swapchain pPresentationTimingCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pPresentationTimingCount))

-- | Wrapper for vkGetPastPresentationTimingGOOGLE
getPastPresentationTimingGOOGLE :: VkDevice ->  VkSwapchainKHR ->  Word32 ->  IO ( VkResult
                                                                                 , Vector VkPastPresentationTimingGOOGLE )
getPastPresentationTimingGOOGLE = \device -> \swapchain -> \presentationTimingCount -> mallocForeignPtrArray (fromIntegral (presentationTimingCount)) >>= (\fpPresentationTimings -> withForeignPtr fpPresentationTimings (\pPresentationTimings -> with presentationTimingCount (\pPresentationTimingCount -> vkGetPastPresentationTimingGOOGLE device swapchain pPresentationTimingCount pPresentationTimings >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpPresentationTimings . fromIntegral <$> peek pPresentationTimingCount)))))

-- | Wrapper for vkCreateIOSSurfaceMVK
createIOSSurfaceMVK :: VkInstance ->  VkIOSSurfaceCreateInfoMVK ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                       , VkSurfaceKHR )
createIOSSurfaceMVK = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateIOSSurfaceMVK instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkCreateMacOSSurfaceMVK
createMacOSSurfaceMVK :: VkInstance ->  VkMacOSSurfaceCreateInfoMVK ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                           , VkSurfaceKHR )
createMacOSSurfaceMVK = \instance' -> \createInfo -> \allocator -> alloca (\pSurface -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateMacOSSurfaceMVK instance' pCreateInfo pAllocator pSurface >>= (\r -> (,) <$> pure r<*>peek pSurface))))

-- | Wrapper for vkCmdSetViewportWScalingNV
cmdSetViewportWScalingNV :: VkCommandBuffer ->  Word32 ->  Vector VkViewportWScalingNV ->  IO ( () )
cmdSetViewportWScalingNV = \commandBuffer -> \firstViewport -> \viewportWScalings -> unsafeWith viewportWScalings (\pViewportWScalings -> vkCmdSetViewportWScalingNV commandBuffer firstViewport (fromIntegral $ Data.Vector.Storable.length viewportWScalings) pViewportWScalings)

-- | Wrapper for vkCmdSetDiscardRectangleEXT
cmdSetDiscardRectangleEXT :: VkCommandBuffer ->  Word32 ->  Vector VkRect2D ->  IO ( () )
cmdSetDiscardRectangleEXT = \commandBuffer -> \firstDiscardRectangle -> \discardRectangles -> unsafeWith discardRectangles (\pDiscardRectangles -> vkCmdSetDiscardRectangleEXT commandBuffer firstDiscardRectangle (fromIntegral $ Data.Vector.Storable.length discardRectangles) pDiscardRectangles)

-- | Wrapper for vkCmdSetSampleLocationsEXT
cmdSetSampleLocationsEXT :: VkCommandBuffer ->  VkSampleLocationsInfoEXT ->  IO ( () )
cmdSetSampleLocationsEXT = \commandBuffer -> \sampleLocationsInfo -> with sampleLocationsInfo (\pSampleLocationsInfo -> vkCmdSetSampleLocationsEXT commandBuffer pSampleLocationsInfo)

-- | Wrapper for vkGetPhysicalDeviceMultisamplePropertiesEXT
getPhysicalDeviceMultisamplePropertiesEXT :: VkPhysicalDevice ->  VkSampleCountFlagBits ->  IO ( ()
                                                                                               , VkMultisamplePropertiesEXT )
getPhysicalDeviceMultisamplePropertiesEXT = \physicalDevice -> \samples -> alloca (\pMultisampleProperties -> vkGetPhysicalDeviceMultisamplePropertiesEXT physicalDevice samples pMultisampleProperties >>= (\r -> (,) <$> pure r<*>peek pMultisampleProperties))

-- | Wrapper for vkGetPhysicalDeviceSurfaceCapabilities2KHR
getPhysicalDeviceSurfaceCapabilities2KHR :: VkPhysicalDevice ->  VkPhysicalDeviceSurfaceInfo2KHR ->  IO ( VkResult
                                                                                                        , VkSurfaceCapabilities2KHR )
getPhysicalDeviceSurfaceCapabilities2KHR = \physicalDevice -> \surfaceInfo -> alloca (\pSurfaceCapabilities -> with surfaceInfo (\pSurfaceInfo -> vkGetPhysicalDeviceSurfaceCapabilities2KHR physicalDevice pSurfaceInfo pSurfaceCapabilities >>= (\r -> (,) <$> pure r<*>peek pSurfaceCapabilities)))

-- | Wrapper for vkGetPhysicalDeviceSurfaceFormats2KHR
getNumgetPhysicalDeviceSurfaceFormats2KHR :: VkPhysicalDevice ->  VkPhysicalDeviceSurfaceInfo2KHR ->  IO ( VkResult
                                                                                                         , Word32 )
getNumgetPhysicalDeviceSurfaceFormats2KHR = \physicalDevice -> \surfaceInfo -> alloca (\pSurfaceFormatCount -> with surfaceInfo (\pSurfaceInfo -> vkGetPhysicalDeviceSurfaceFormats2KHR physicalDevice pSurfaceInfo pSurfaceFormatCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pSurfaceFormatCount)))

-- | Wrapper for vkGetPhysicalDeviceSurfaceFormats2KHR
getPhysicalDeviceSurfaceFormats2KHR :: VkPhysicalDevice ->  VkPhysicalDeviceSurfaceInfo2KHR ->  Word32 ->  IO ( VkResult
                                                                                                              , Vector VkSurfaceFormat2KHR )
getPhysicalDeviceSurfaceFormats2KHR = \physicalDevice -> \surfaceInfo -> \surfaceFormatCount -> mallocForeignPtrArray (fromIntegral (surfaceFormatCount)) >>= (\fpSurfaceFormats -> withForeignPtr fpSurfaceFormats (\pSurfaceFormats -> with surfaceFormatCount (\pSurfaceFormatCount -> with surfaceInfo (\pSurfaceInfo -> vkGetPhysicalDeviceSurfaceFormats2KHR physicalDevice pSurfaceInfo pSurfaceFormatCount pSurfaceFormats >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpSurfaceFormats . fromIntegral <$> peek pSurfaceFormatCount))))))

-- | Wrapper for vkGetBufferMemoryRequirements2
getBufferMemoryRequirements2 :: VkDevice ->  VkBufferMemoryRequirementsInfo2 ->  IO ( ()
                                                                                    , VkMemoryRequirements2 )
getBufferMemoryRequirements2 = \device -> \info -> alloca (\pMemoryRequirements -> with info (\pInfo -> vkGetBufferMemoryRequirements2 device pInfo pMemoryRequirements >>= (\r -> (,) <$> pure r<*>peek pMemoryRequirements)))

-- | Wrapper for vkGetImageMemoryRequirements2
getImageMemoryRequirements2 :: VkDevice ->  VkImageMemoryRequirementsInfo2 ->  IO ( ()
                                                                                  , VkMemoryRequirements2 )
getImageMemoryRequirements2 = \device -> \info -> alloca (\pMemoryRequirements -> with info (\pInfo -> vkGetImageMemoryRequirements2 device pInfo pMemoryRequirements >>= (\r -> (,) <$> pure r<*>peek pMemoryRequirements)))

-- | Wrapper for vkGetImageSparseMemoryRequirements2
getNumgetImageSparseMemoryRequirements2 :: VkDevice ->  VkImageSparseMemoryRequirementsInfo2 ->  IO ( ()
                                                                                                    , Word32 )
getNumgetImageSparseMemoryRequirements2 = \device -> \info -> alloca (\pSparseMemoryRequirementCount -> with info (\pInfo -> vkGetImageSparseMemoryRequirements2 device pInfo pSparseMemoryRequirementCount nullPtr >>= (\r -> (,) <$> pure r<*>peek pSparseMemoryRequirementCount)))

-- | Wrapper for vkGetImageSparseMemoryRequirements2
getImageSparseMemoryRequirements2 :: VkDevice ->  VkImageSparseMemoryRequirementsInfo2 ->  Word32 ->  IO ( ()
                                                                                                         , Vector VkSparseImageMemoryRequirements2 )
getImageSparseMemoryRequirements2 = \device -> \info -> \sparseMemoryRequirementCount -> mallocForeignPtrArray (fromIntegral (sparseMemoryRequirementCount)) >>= (\fpSparseMemoryRequirements -> withForeignPtr fpSparseMemoryRequirements (\pSparseMemoryRequirements -> with sparseMemoryRequirementCount (\pSparseMemoryRequirementCount -> with info (\pInfo -> vkGetImageSparseMemoryRequirements2 device pInfo pSparseMemoryRequirementCount pSparseMemoryRequirements >>= (\r -> (,) <$> pure r<*>(unsafeFromForeignPtr0 fpSparseMemoryRequirements . fromIntegral <$> peek pSparseMemoryRequirementCount))))))

-- | Wrapper for vkCreateSamplerYcbcrConversion
createSamplerYcbcrConversion :: VkDevice ->  VkSamplerYcbcrConversionCreateInfo ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                       , VkSamplerYcbcrConversion )
createSamplerYcbcrConversion = \device -> \createInfo -> \allocator -> alloca (\pYcbcrConversion -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateSamplerYcbcrConversion device pCreateInfo pAllocator pYcbcrConversion >>= (\r -> (,) <$> pure r<*>peek pYcbcrConversion))))

-- | Wrapper for vkDestroySamplerYcbcrConversion
destroySamplerYcbcrConversion :: VkDevice ->  VkSamplerYcbcrConversion ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroySamplerYcbcrConversion = \device -> \ycbcrConversion -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroySamplerYcbcrConversion device ycbcrConversion pAllocator)

-- | Wrapper for vkGetDeviceQueue2
getDeviceQueue2 :: VkDevice ->  VkDeviceQueueInfo2 ->  IO ( ()
                                                          , VkQueue )
getDeviceQueue2 = \device -> \queueInfo -> alloca (\pQueue -> with queueInfo (\pQueueInfo -> vkGetDeviceQueue2 device pQueueInfo pQueue >>= (\r -> (,) <$> pure r<*>peek pQueue)))

-- | Wrapper for vkCreateValidationCacheEXT
createValidationCacheEXT :: VkDevice ->  VkValidationCacheCreateInfoEXT ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                               , VkValidationCacheEXT )
createValidationCacheEXT = \device -> \createInfo -> \allocator -> alloca (\pValidationCache -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateValidationCacheEXT device pCreateInfo pAllocator pValidationCache >>= (\r -> (,) <$> pure r<*>peek pValidationCache))))

-- | Wrapper for vkDestroyValidationCacheEXT
destroyValidationCacheEXT :: VkDevice ->  VkValidationCacheEXT ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyValidationCacheEXT = \device -> \validationCache -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyValidationCacheEXT device validationCache pAllocator)

-- | Wrapper for vkMergeValidationCachesEXT
mergeValidationCachesEXT :: VkDevice ->  VkValidationCacheEXT ->  Vector VkValidationCacheEXT ->  IO ( VkResult )
mergeValidationCachesEXT = \device -> \dstCache -> \srcCaches -> unsafeWith srcCaches (\pSrcCaches -> vkMergeValidationCachesEXT device dstCache (fromIntegral $ Data.Vector.Storable.length srcCaches) pSrcCaches)

-- | Wrapper for vkGetDescriptorSetLayoutSupport
getDescriptorSetLayoutSupport :: VkDevice ->  VkDescriptorSetLayoutCreateInfo ->  IO ( ()
                                                                                     , VkDescriptorSetLayoutSupport )
getDescriptorSetLayoutSupport = \device -> \createInfo -> alloca (\pSupport -> with createInfo (\pCreateInfo -> vkGetDescriptorSetLayoutSupport device pCreateInfo pSupport >>= (\r -> (,) <$> pure r<*>peek pSupport)))

-- | Wrapper for vkSetDebugUtilsObjectNameEXT
setDebugUtilsObjectNameEXT :: VkDevice ->  VkDebugUtilsObjectNameInfoEXT ->  IO ( VkResult )
setDebugUtilsObjectNameEXT = \device -> \nameInfo -> with nameInfo (\pNameInfo -> vkSetDebugUtilsObjectNameEXT device pNameInfo)

-- | Wrapper for vkSetDebugUtilsObjectTagEXT
setDebugUtilsObjectTagEXT :: VkDevice ->  VkDebugUtilsObjectTagInfoEXT ->  IO ( VkResult )
setDebugUtilsObjectTagEXT = \device -> \tagInfo -> with tagInfo (\pTagInfo -> vkSetDebugUtilsObjectTagEXT device pTagInfo)

-- | Wrapper for vkQueueBeginDebugUtilsLabelEXT
queueBeginDebugUtilsLabelEXT :: VkQueue ->  VkDebugUtilsLabelEXT ->  IO ( () )
queueBeginDebugUtilsLabelEXT = \queue -> \labelInfo -> with labelInfo (\pLabelInfo -> vkQueueBeginDebugUtilsLabelEXT queue pLabelInfo)

-- | Wrapper for vkQueueEndDebugUtilsLabelEXT
queueEndDebugUtilsLabelEXT :: VkQueue ->  IO ( () )
queueEndDebugUtilsLabelEXT = \queue -> vkQueueEndDebugUtilsLabelEXT queue

-- | Wrapper for vkQueueInsertDebugUtilsLabelEXT
queueInsertDebugUtilsLabelEXT :: VkQueue ->  VkDebugUtilsLabelEXT ->  IO ( () )
queueInsertDebugUtilsLabelEXT = \queue -> \labelInfo -> with labelInfo (\pLabelInfo -> vkQueueInsertDebugUtilsLabelEXT queue pLabelInfo)

-- | Wrapper for vkCmdBeginDebugUtilsLabelEXT
cmdBeginDebugUtilsLabelEXT :: VkCommandBuffer ->  VkDebugUtilsLabelEXT ->  IO ( () )
cmdBeginDebugUtilsLabelEXT = \commandBuffer -> \labelInfo -> with labelInfo (\pLabelInfo -> vkCmdBeginDebugUtilsLabelEXT commandBuffer pLabelInfo)

-- | Wrapper for vkCmdEndDebugUtilsLabelEXT
cmdEndDebugUtilsLabelEXT :: VkCommandBuffer ->  IO ( () )
cmdEndDebugUtilsLabelEXT = \commandBuffer -> vkCmdEndDebugUtilsLabelEXT commandBuffer

-- | Wrapper for vkCmdInsertDebugUtilsLabelEXT
cmdInsertDebugUtilsLabelEXT :: VkCommandBuffer ->  VkDebugUtilsLabelEXT ->  IO ( () )
cmdInsertDebugUtilsLabelEXT = \commandBuffer -> \labelInfo -> with labelInfo (\pLabelInfo -> vkCmdInsertDebugUtilsLabelEXT commandBuffer pLabelInfo)

-- | Wrapper for vkCreateDebugUtilsMessengerEXT
createDebugUtilsMessengerEXT :: VkInstance ->  VkDebugUtilsMessengerCreateInfoEXT ->  Maybe VkAllocationCallbacks ->  IO ( VkResult
                                                                                                                         , VkDebugUtilsMessengerEXT )
createDebugUtilsMessengerEXT = \instance' -> \createInfo -> \allocator -> alloca (\pMessenger -> maybeWith with allocator (\pAllocator -> with createInfo (\pCreateInfo -> vkCreateDebugUtilsMessengerEXT instance' pCreateInfo pAllocator pMessenger >>= (\r -> (,) <$> pure r<*>peek pMessenger))))

-- | Wrapper for vkDestroyDebugUtilsMessengerEXT
destroyDebugUtilsMessengerEXT :: VkInstance ->  VkDebugUtilsMessengerEXT ->  Maybe VkAllocationCallbacks ->  IO ( () )
destroyDebugUtilsMessengerEXT = \instance' -> \messenger -> \allocator -> maybeWith with allocator (\pAllocator -> vkDestroyDebugUtilsMessengerEXT instance' messenger pAllocator)

-- | Wrapper for vkSubmitDebugUtilsMessageEXT
submitDebugUtilsMessageEXT :: VkInstance ->  VkDebugUtilsMessageSeverityFlagBitsEXT ->  VkDebugUtilsMessageTypeFlagsEXT ->  VkDebugUtilsMessengerCallbackDataEXT ->  IO ( () )
submitDebugUtilsMessageEXT = \instance' -> \messageSeverity -> \messageTypes -> \callbackData -> with callbackData (\pCallbackData -> vkSubmitDebugUtilsMessageEXT instance' messageSeverity messageTypes pCallbackData)

-- | Wrapper for vkCmdWriteBufferMarkerAMD
cmdWriteBufferMarkerAMD :: VkCommandBuffer ->  VkPipelineStageFlagBits ->  VkBuffer ->  VkDeviceSize ->  Word32 ->  IO ( () )
cmdWriteBufferMarkerAMD = \commandBuffer -> \pipelineStage -> \dstBuffer -> \dstOffset -> \marker -> vkCmdWriteBufferMarkerAMD commandBuffer pipelineStage dstBuffer dstOffset marker

-- | Wrapper for vkGetAndroidHardwareBufferPropertiesANDROID
getAndroidHardwareBufferPropertiesANDROID :: VkDevice ->  AHardwareBuffer ->  IO ( VkResult
                                                                                 , VkAndroidHardwareBufferPropertiesANDROID )
getAndroidHardwareBufferPropertiesANDROID = \device -> \buffer -> alloca (\pProperties -> with buffer (\pBuffer -> vkGetAndroidHardwareBufferPropertiesANDROID device pBuffer pProperties >>= (\r -> (,) <$> pure r<*>peek pProperties)))
