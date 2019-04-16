{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language ForeignFunctionInterface #-}
{-# language MagicHash #-}
{-# language TypeApplications #-}

module Graphics.Vulkan.C.Dynamic
  ( initInstanceCmds
  , initDeviceCmds
  , DeviceCmds(..)
  , InstanceCmds(..)
  , getDeviceProcAddr
  , destroyDevice
  , getDeviceQueue
  , queueSubmit
  , queueWaitIdle
  , deviceWaitIdle
  , allocateMemory
  , freeMemory
  , mapMemory
  , unmapMemory
  , flushMappedMemoryRanges
  , invalidateMappedMemoryRanges
  , getDeviceMemoryCommitment
  , getBufferMemoryRequirements
  , bindBufferMemory
  , getImageMemoryRequirements
  , bindImageMemory
  , getImageSparseMemoryRequirements
  , queueBindSparse
  , createFence
  , destroyFence
  , resetFences
  , getFenceStatus
  , waitForFences
  , createSemaphore
  , destroySemaphore
  , createEvent
  , destroyEvent
  , getEventStatus
  , setEvent
  , resetEvent
  , createQueryPool
  , destroyQueryPool
  , getQueryPoolResults
  , resetQueryPoolEXT
  , createBuffer
  , destroyBuffer
  , createBufferView
  , destroyBufferView
  , createImage
  , destroyImage
  , getImageSubresourceLayout
  , createImageView
  , destroyImageView
  , createShaderModule
  , destroyShaderModule
  , createPipelineCache
  , destroyPipelineCache
  , getPipelineCacheData
  , mergePipelineCaches
  , createGraphicsPipelines
  , createComputePipelines
  , destroyPipeline
  , createPipelineLayout
  , destroyPipelineLayout
  , createSampler
  , destroySampler
  , createDescriptorSetLayout
  , destroyDescriptorSetLayout
  , createDescriptorPool
  , destroyDescriptorPool
  , resetDescriptorPool
  , allocateDescriptorSets
  , freeDescriptorSets
  , updateDescriptorSets
  , createFramebuffer
  , destroyFramebuffer
  , createRenderPass
  , destroyRenderPass
  , getRenderAreaGranularity
  , createCommandPool
  , destroyCommandPool
  , resetCommandPool
  , allocateCommandBuffers
  , freeCommandBuffers
  , beginCommandBuffer
  , endCommandBuffer
  , resetCommandBuffer
  , cmdBindPipeline
  , cmdSetViewport
  , cmdSetScissor
  , cmdSetLineWidth
  , cmdSetDepthBias
  , cmdSetBlendConstants
  , cmdSetDepthBounds
  , cmdSetStencilCompareMask
  , cmdSetStencilWriteMask
  , cmdSetStencilReference
  , cmdBindDescriptorSets
  , cmdBindIndexBuffer
  , cmdBindVertexBuffers
  , cmdDraw
  , cmdDrawIndexed
  , cmdDrawIndirect
  , cmdDrawIndexedIndirect
  , cmdDispatch
  , cmdDispatchIndirect
  , cmdCopyBuffer
  , cmdCopyImage
  , cmdBlitImage
  , cmdCopyBufferToImage
  , cmdCopyImageToBuffer
  , cmdUpdateBuffer
  , cmdFillBuffer
  , cmdClearColorImage
  , cmdClearDepthStencilImage
  , cmdClearAttachments
  , cmdResolveImage
  , cmdSetEvent
  , cmdResetEvent
  , cmdWaitEvents
  , cmdPipelineBarrier
  , cmdBeginQuery
  , cmdEndQuery
  , cmdBeginConditionalRenderingEXT
  , cmdEndConditionalRenderingEXT
  , cmdResetQueryPool
  , cmdWriteTimestamp
  , cmdCopyQueryPoolResults
  , cmdPushConstants
  , cmdBeginRenderPass
  , cmdNextSubpass
  , cmdEndRenderPass
  , cmdExecuteCommands
  , createSharedSwapchainsKHR
  , createSwapchainKHR
  , destroySwapchainKHR
  , getSwapchainImagesKHR
  , acquireNextImageKHR
  , queuePresentKHR
  , debugMarkerSetObjectNameEXT
  , debugMarkerSetObjectTagEXT
  , cmdDebugMarkerBeginEXT
  , cmdDebugMarkerEndEXT
  , cmdDebugMarkerInsertEXT
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getMemoryWin32HandleNV
#endif
  , cmdDrawIndirectCountAMD
  , cmdDrawIndexedIndirectCountAMD
  , cmdProcessCommandsNVX
  , cmdReserveSpaceForCommandsNVX
  , createIndirectCommandsLayoutNVX
  , destroyIndirectCommandsLayoutNVX
  , createObjectTableNVX
  , destroyObjectTableNVX
  , registerObjectsNVX
  , unregisterObjectsNVX
  , cmdPushDescriptorSetKHR
  , trimCommandPool
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getMemoryWin32HandleKHR
  , getMemoryWin32HandlePropertiesKHR
#endif
  , getMemoryFdKHR
  , getMemoryFdPropertiesKHR
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getSemaphoreWin32HandleKHR
  , importSemaphoreWin32HandleKHR
#endif
  , getSemaphoreFdKHR
  , importSemaphoreFdKHR
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getFenceWin32HandleKHR
  , importFenceWin32HandleKHR
#endif
  , getFenceFdKHR
  , importFenceFdKHR
  , displayPowerControlEXT
  , registerDeviceEventEXT
  , registerDisplayEventEXT
  , getSwapchainCounterEXT
  , getDeviceGroupPeerMemoryFeatures
  , bindBufferMemory2
  , bindImageMemory2
  , cmdSetDeviceMask
  , getDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , acquireNextImage2KHR
  , cmdDispatchBase
  , createDescriptorUpdateTemplate
  , destroyDescriptorUpdateTemplate
  , updateDescriptorSetWithTemplate
  , cmdPushDescriptorSetWithTemplateKHR
  , setHdrMetadataEXT
  , getSwapchainStatusKHR
  , getRefreshCycleDurationGOOGLE
  , getPastPresentationTimingGOOGLE
  , cmdSetViewportWScalingNV
  , cmdSetDiscardRectangleEXT
  , cmdSetSampleLocationsEXT
  , getBufferMemoryRequirements2
  , getImageMemoryRequirements2
  , getImageSparseMemoryRequirements2
  , createSamplerYcbcrConversion
  , destroySamplerYcbcrConversion
  , getDeviceQueue2
  , createValidationCacheEXT
  , destroyValidationCacheEXT
  , getValidationCacheDataEXT
  , mergeValidationCachesEXT
  , getDescriptorSetLayoutSupport
  , getShaderInfoAMD
  , setLocalDimmingAMD
  , getCalibratedTimestampsEXT
  , setDebugUtilsObjectNameEXT
  , setDebugUtilsObjectTagEXT
  , queueBeginDebugUtilsLabelEXT
  , queueEndDebugUtilsLabelEXT
  , queueInsertDebugUtilsLabelEXT
  , cmdBeginDebugUtilsLabelEXT
  , cmdEndDebugUtilsLabelEXT
  , cmdInsertDebugUtilsLabelEXT
  , getMemoryHostPointerPropertiesEXT
  , cmdWriteBufferMarkerAMD
  , createRenderPass2KHR
  , cmdBeginRenderPass2KHR
  , cmdNextSubpass2KHR
  , cmdEndRenderPass2KHR
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , getAndroidHardwareBufferPropertiesANDROID
  , getMemoryAndroidHardwareBufferANDROID
#endif
  , cmdDrawIndirectCountKHR
  , cmdDrawIndexedIndirectCountKHR
  , cmdSetCheckpointNV
  , getQueueCheckpointDataNV
  , cmdBindTransformFeedbackBuffersEXT
  , cmdBeginTransformFeedbackEXT
  , cmdEndTransformFeedbackEXT
  , cmdBeginQueryIndexedEXT
  , cmdEndQueryIndexedEXT
  , cmdDrawIndirectByteCountEXT
  , cmdSetExclusiveScissorNV
  , cmdBindShadingRateImageNV
  , cmdSetViewportShadingRatePaletteNV
  , cmdSetCoarseSampleOrderNV
  , cmdDrawMeshTasksNV
  , cmdDrawMeshTasksIndirectNV
  , cmdDrawMeshTasksIndirectCountNV
  , compileDeferredNV
  , createAccelerationStructureNV
  , destroyAccelerationStructureNV
  , getAccelerationStructureMemoryRequirementsNV
  , bindAccelerationStructureMemoryNV
  , cmdCopyAccelerationStructureNV
  , cmdWriteAccelerationStructuresPropertiesNV
  , cmdBuildAccelerationStructureNV
  , cmdTraceRaysNV
  , getRayTracingShaderGroupHandlesNV
  , getAccelerationStructureHandleNV
  , createRayTracingPipelinesNV
  , getImageDrmFormatModifierPropertiesEXT
  , getBufferDeviceAddressEXT
  , getImageViewHandleNVX
  , getDeviceGroupSurfacePresentModes2EXT
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , acquireFullScreenExclusiveModeEXT
  , releaseFullScreenExclusiveModeEXT
#endif
  , destroyInstance
  , enumeratePhysicalDevices
  , getInstanceProcAddr
  , getPhysicalDeviceProperties
  , getPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceMemoryProperties
  , getPhysicalDeviceFeatures
  , getPhysicalDeviceFormatProperties
  , getPhysicalDeviceImageFormatProperties
  , createDevice
  , enumerateDeviceLayerProperties
  , enumerateDeviceExtensionProperties
  , getPhysicalDeviceSparseImageFormatProperties
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , createAndroidSurfaceKHR
#endif
  , getPhysicalDeviceDisplayPropertiesKHR
  , getPhysicalDeviceDisplayPlanePropertiesKHR
  , getDisplayPlaneSupportedDisplaysKHR
  , getDisplayModePropertiesKHR
  , createDisplayModeKHR
  , getDisplayPlaneCapabilitiesKHR
  , createDisplayPlaneSurfaceKHR
  , destroySurfaceKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfacePresentModesKHR
#if defined(VK_USE_PLATFORM_VI_NN)
  , createViSurfaceNN
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
  , createWaylandSurfaceKHR
  , getPhysicalDeviceWaylandPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , createWin32SurfaceKHR
  , getPhysicalDeviceWin32PresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
  , createXlibSurfaceKHR
  , getPhysicalDeviceXlibPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
  , createXcbSurfaceKHR
  , getPhysicalDeviceXcbPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_FUCHSIA)
  , createImagePipeSurfaceFUCHSIA
#endif
#if defined(VK_USE_PLATFORM_GGP)
  , createStreamDescriptorSurfaceGGP
#endif
  , createDebugReportCallbackEXT
  , destroyDebugReportCallbackEXT
  , debugReportMessageEXT
  , getPhysicalDeviceExternalImageFormatPropertiesNV
  , getPhysicalDeviceGeneratedCommandsPropertiesNVX
  , getPhysicalDeviceFeatures2
  , getPhysicalDeviceProperties2
  , getPhysicalDeviceFormatProperties2
  , getPhysicalDeviceImageFormatProperties2
  , getPhysicalDeviceQueueFamilyProperties2
  , getPhysicalDeviceMemoryProperties2
  , getPhysicalDeviceSparseImageFormatProperties2
  , getPhysicalDeviceExternalBufferProperties
  , getPhysicalDeviceExternalSemaphoreProperties
  , getPhysicalDeviceExternalFenceProperties
  , releaseDisplayEXT
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
  , acquireXlibDisplayEXT
  , getRandROutputDisplayEXT
#endif
  , getPhysicalDeviceSurfaceCapabilities2EXT
  , enumeratePhysicalDeviceGroups
  , getPhysicalDevicePresentRectanglesKHR
#if defined(VK_USE_PLATFORM_IOS_MVK)
  , createIOSSurfaceMVK
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
  , createMacOSSurfaceMVK
#endif
#if defined(VK_USE_PLATFORM_METAL_EXT)
  , createMetalSurfaceEXT
#endif
  , getPhysicalDeviceMultisamplePropertiesEXT
  , getPhysicalDeviceSurfaceCapabilities2KHR
  , getPhysicalDeviceSurfaceFormats2KHR
  , getPhysicalDeviceDisplayProperties2KHR
  , getPhysicalDeviceDisplayPlaneProperties2KHR
  , getDisplayModeProperties2KHR
  , getDisplayPlaneCapabilities2KHR
  , getPhysicalDeviceCalibrateableTimeDomainsEXT
  , createDebugUtilsMessengerEXT
  , destroyDebugUtilsMessengerEXT
  , submitDebugUtilsMessageEXT
  , getPhysicalDeviceCooperativeMatrixPropertiesNV
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getPhysicalDeviceSurfacePresentModes2EXT
#endif
  , enumerateInstanceVersion
  , enumerateInstanceExtensionProperties
  , enumerateInstanceLayerProperties
  , createInstance
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
  , nullPtr
  )
import qualified GHC.Ptr
  ( Ptr(..)
  )
import System.IO.Unsafe
  ( unsafeDupablePerformIO
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateInfo(..)
  , FN_vkCreateBuffer
  , FN_vkDestroyBuffer
  )
import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateInfo(..)
  , FN_vkCreateBufferView
  , FN_vkDestroyBufferView
  , VkBufferView
  )
import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferResetFlagBits(..)
  , VkQueryControlFlagBits(..)
  , FN_vkAllocateCommandBuffers
  , FN_vkBeginCommandBuffer
  , FN_vkEndCommandBuffer
  , FN_vkFreeCommandBuffers
  , FN_vkResetCommandBuffer
  , VkCommandBufferResetFlags
  , VkQueryControlFlags
  )
import Graphics.Vulkan.C.Core10.CommandBufferBuilding
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
  , VkStencilFaceFlagBits(..)
  , VkSubpassContents(..)
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
import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateInfo(..)
  , VkCommandPoolResetFlagBits(..)
  , FN_vkCreateCommandPool
  , FN_vkDestroyCommandPool
  , FN_vkResetCommandPool
  , VkCommandPool
  , VkCommandPoolResetFlags
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkResult(..)
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolResetFlags(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkWriteDescriptorSet(..)
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
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateInfo(..)
  , FN_vkCreateDevice
  , FN_vkDestroyDevice
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkFormatProperties(..)
  , VkImageCreateFlagBits(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
  , VkInstanceCreateInfo(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkQueueFamilyProperties(..)
  , VkSampleCountFlagBits(..)
  , FN_vkCreateInstance
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
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateInfo(..)
  , FN_vkCreateEvent
  , FN_vkDestroyEvent
  , FN_vkGetEventStatus
  , FN_vkResetEvent
  , FN_vkSetEvent
  , VkEvent
  )
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
  , FN_vkEnumerateDeviceExtensionProperties
  , FN_vkEnumerateInstanceExtensionProperties
  )
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateInfo(..)
  , FN_vkCreateFence
  , FN_vkDestroyFence
  , FN_vkGetFenceStatus
  , FN_vkResetFences
  , FN_vkWaitForFences
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , VkSubresourceLayout(..)
  , FN_vkCreateImage
  , FN_vkDestroyImage
  , FN_vkGetImageSubresourceLayout
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageSubresourceRange(..)
  , VkImageViewCreateInfo(..)
  , FN_vkCreateImageView
  , FN_vkDestroyImageView
  , VkImageView
  )
import Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties(..)
  , FN_vkEnumerateDeviceLayerProperties
  , FN_vkEnumerateInstanceLayerProperties
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkMemoryMapFlags(..)
  , FN_vkAllocateMemory
  , FN_vkFlushMappedMemoryRanges
  , FN_vkFreeMemory
  , FN_vkGetDeviceMemoryCommitment
  , FN_vkInvalidateMappedMemoryRanges
  , FN_vkMapMemory
  , FN_vkUnmapMemory
  , VkDeviceMemory
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , FN_vkBindBufferMemory
  , FN_vkBindImageMemory
  , FN_vkGetBufferMemoryRequirements
  , FN_vkGetImageMemoryRequirements
  , VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkDependencyFlagBits(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateInfo(..)
  , FN_vkCreateFramebuffer
  , FN_vkCreateRenderPass
  , FN_vkDestroyFramebuffer
  , FN_vkDestroyRenderPass
  , FN_vkGetRenderAreaGranularity
  , VkDependencyFlags
  , VkFramebuffer
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkComputePipelineCreateInfo(..)
  , VkExtent2D(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkRect2D(..)
  , VkShaderStageFlagBits(..)
  , VkViewport(..)
  , FN_vkCreateComputePipelines
  , FN_vkCreateGraphicsPipelines
  , FN_vkDestroyPipeline
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateInfo(..)
  , FN_vkCreatePipelineCache
  , FN_vkDestroyPipelineCache
  , FN_vkGetPipelineCacheData
  , FN_vkMergePipelineCaches
  , VkPipelineCache
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateInfo(..)
  , FN_vkCreatePipelineLayout
  , FN_vkDestroyPipelineLayout
  , VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPoolCreateInfo(..)
  , VkQueryResultFlagBits(..)
  , VkQueryType(..)
  , FN_vkCreateQueryPool
  , FN_vkDestroyQueryPool
  , FN_vkGetQueryPoolResults
  , VkQueryPool
  , VkQueryResultFlags
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkSubmitInfo(..)
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
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateInfo(..)
  , FN_vkCreateSemaphore
  , FN_vkDestroySemaphore
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkFilter(..)
  , VkSamplerCreateInfo(..)
  , FN_vkCreateSampler
  , FN_vkDestroySampler
  , VkSampler
  )
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateInfo(..)
  , FN_vkCreateShaderModule
  , FN_vkDestroyShaderModule
  , VkShaderModule
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageSubresource(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryRequirements(..)
  , FN_vkGetImageSparseMemoryRequirements
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties
  , FN_vkQueueBindSparse
  )
import Graphics.Vulkan.C.Core11.DeviceInitialization
  ( FN_vkEnumerateInstanceVersion
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , FN_vkGetDeviceQueue2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , FN_vkBindBufferMemory2
  , FN_vkBindImageMemory2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateInfo(..)
  , FN_vkCreateDescriptorUpdateTemplate
  , FN_vkDestroyDescriptorUpdateTemplate
  , FN_vkUpdateDescriptorSetWithTemplate
  , VkDescriptorUpdateTemplate
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( FN_vkCmdDispatchBase
  , FN_vkCmdSetDeviceMask
  , FN_vkGetDeviceGroupPeerMemoryFeatures
  , VkPeerMemoryFeatureFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkPhysicalDeviceGroupProperties(..)
  , FN_vkEnumeratePhysicalDeviceGroups
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , FN_vkGetPhysicalDeviceExternalFenceProperties
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , FN_vkGetPhysicalDeviceExternalBufferProperties
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , FN_vkGetPhysicalDeviceExternalSemaphoreProperties
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
  , FN_vkGetBufferMemoryRequirements2
  , FN_vkGetImageMemoryRequirements2
  , FN_vkGetImageSparseMemoryRequirements2
  , VkMemoryRequirements2KHR
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( VkFormatProperties2(..)
  , VkImageFormatProperties2(..)
  , VkPhysicalDeviceFeatures2(..)
  , VkPhysicalDeviceImageFormatInfo2(..)
  , VkPhysicalDeviceMemoryProperties2(..)
  , VkPhysicalDeviceProperties2(..)
  , VkPhysicalDeviceSparseImageFormatInfo2(..)
  , VkQueueFamilyProperties2(..)
  , VkSparseImageFormatProperties2(..)
  , FN_vkGetPhysicalDeviceFeatures2
  , FN_vkGetPhysicalDeviceFormatProperties2
  , FN_vkGetPhysicalDeviceImageFormatProperties2
  , FN_vkGetPhysicalDeviceMemoryProperties2
  , FN_vkGetPhysicalDeviceProperties2
  , FN_vkGetPhysicalDeviceQueueFamilyProperties2
  , FN_vkGetPhysicalDeviceSparseImageFormatProperties2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  , FN_vkTrimCommandPool
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , FN_vkGetDescriptorSetLayoutSupport
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkSamplerYcbcrConversionCreateInfo(..)
  , FN_vkCreateSamplerYcbcrConversion
  , FN_vkDestroySamplerYcbcrConversion
  , VkSamplerYcbcrConversion
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_buffer_marker
  ( FN_vkCmdWriteBufferMarkerAMD
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( FN_vkSetLocalDimmingAMD
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_draw_indirect_count
  ( FN_vkCmdDrawIndexedIndirectCountAMD
  , FN_vkCmdDrawIndirectCountAMD
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  , FN_vkGetShaderInfoAMD
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , AHardwareBuffer
  , FN_vkGetAndroidHardwareBufferPropertiesANDROID
  , FN_vkGetMemoryAndroidHardwareBufferANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import Graphics.Vulkan.C.Extensions.VK_EXT_acquire_xlib_display
  ( FN_vkAcquireXlibDisplayEXT
  , FN_vkGetRandROutputDisplayEXT
  , RROutput
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressInfoEXT(..)
  , FN_vkGetBufferDeviceAddressEXT
  , VkDeviceAddress
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , VkTimeDomainEXT(..)
  , FN_vkGetCalibratedTimestampsEXT
  , FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkConditionalRenderingBeginInfoEXT(..)
  , FN_vkCmdBeginConditionalRenderingEXT
  , FN_vkCmdEndConditionalRenderingEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , FN_vkCmdDebugMarkerBeginEXT
  , FN_vkCmdDebugMarkerEndEXT
  , FN_vkCmdDebugMarkerInsertEXT
  , FN_vkDebugMarkerSetObjectNameEXT
  , FN_vkDebugMarkerSetObjectTagEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportFlagBitsEXT(..)
  , VkDebugReportObjectTypeEXT(..)
  , FN_vkCreateDebugReportCallbackEXT
  , FN_vkDebugReportMessageEXT
  , FN_vkDestroyDebugReportCallbackEXT
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , VkDebugUtilsMessageTypeFlagBitsEXT(..)
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_direct_mode_display
  ( FN_vkReleaseDisplayEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( FN_vkCmdSetDiscardRectangleEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkDisplayPowerInfoEXT(..)
  , FN_vkDisplayPowerControlEXT
  , FN_vkGetSwapchainCounterEXT
  , FN_vkRegisterDeviceEventEXT
  , FN_vkRegisterDisplayEventEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagBitsEXT(..)
  , FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkMemoryHostPointerPropertiesEXT(..)
  , FN_vkGetMemoryHostPointerPropertiesEXT
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( FN_vkAcquireFullScreenExclusiveModeEXT
  , FN_vkGetPhysicalDeviceSurfacePresentModes2EXT
  , FN_vkReleaseFullScreenExclusiveModeEXT
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , FN_vkSetHdrMetadataEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( FN_vkResetQueryPoolEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkImageDrmFormatModifierPropertiesEXT(..)
  , FN_vkGetImageDrmFormatModifierPropertiesEXT
  )

#if defined(VK_USE_PLATFORM_METAL_EXT)
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateInfoEXT(..)
  , FN_vkCreateMetalSurfaceEXT
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkMultisamplePropertiesEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , FN_vkCmdSetSampleLocationsEXT
  , FN_vkGetPhysicalDeviceMultisamplePropertiesEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( FN_vkCmdBeginQueryIndexedEXT
  , FN_vkCmdBeginTransformFeedbackEXT
  , FN_vkCmdBindTransformFeedbackBuffersEXT
  , FN_vkCmdDrawIndirectByteCountEXT
  , FN_vkCmdEndQueryIndexedEXT
  , FN_vkCmdEndTransformFeedbackEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateInfoEXT(..)
  , FN_vkCreateValidationCacheEXT
  , FN_vkDestroyValidationCacheEXT
  , FN_vkGetValidationCacheDataEXT
  , FN_vkMergeValidationCachesEXT
  , VkValidationCacheEXT
  )

#if defined(VK_USE_PLATFORM_FUCHSIA)
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  , FN_vkCreateImagePipeSurfaceFUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateInfoGGP(..)
  , FN_vkCreateStreamDescriptorSurfaceGGP
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  , FN_vkGetPastPresentationTimingGOOGLE
  , FN_vkGetRefreshCycleDurationGOOGLE
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR(..)
  , FN_vkCreateAndroidSurfaceKHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkRenderPassCreateInfo2KHR(..)
  , VkSubpassBeginInfoKHR(..)
  , VkSubpassEndInfoKHR(..)
  , FN_vkCmdBeginRenderPass2KHR
  , FN_vkCmdEndRenderPass2KHR
  , FN_vkCmdNextSubpass2KHR
  , FN_vkCreateRenderPass2KHR
  )

import Graphics.Vulkan.C.Extensions.VK_KHR_device_group
  ( FN_vkGetDeviceGroupSurfacePresentModes2EXT
  )
#if defined(VK_USE_PLATFORM_WIN32_KHR)
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
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
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( FN_vkCreateSharedSwapchainsKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_draw_indirect_count
  ( FN_vkCmdDrawIndexedIndirectCountKHR
  , FN_vkCmdDrawIndirectCountKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , FN_vkGetFenceFdKHR
  , FN_vkImportFenceFdKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , FN_vkGetFenceWin32HandleKHR
  , FN_vkImportFenceWin32HandleKHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  , FN_vkGetMemoryFdKHR
  , FN_vkGetMemoryFdPropertiesKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , FN_vkGetMemoryWin32HandleKHR
  , FN_vkGetMemoryWin32HandlePropertiesKHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , FN_vkGetSemaphoreFdKHR
  , FN_vkImportSemaphoreFdKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  , FN_vkGetSemaphoreWin32HandleKHR
  , FN_vkImportSemaphoreWin32HandleKHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  , FN_vkGetDisplayModeProperties2KHR
  , FN_vkGetDisplayPlaneCapabilities2KHR
  , FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR
  , FN_vkGetPhysicalDeviceDisplayProperties2KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  , FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR
  , FN_vkGetPhysicalDeviceSurfaceFormats2KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( FN_vkCmdPushDescriptorSetKHR
  , FN_vkCmdPushDescriptorSetWithTemplateKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( FN_vkGetSwapchainStatusKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , FN_vkDestroySurfaceKHR
  , FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR
  , FN_vkGetPhysicalDeviceSurfaceFormatsKHR
  , FN_vkGetPhysicalDeviceSurfacePresentModesKHR
  , FN_vkGetPhysicalDeviceSurfaceSupportKHR
  , VkSurfaceKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateInfoKHR(..)
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
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR(..)
  , FN_vkCreateWaylandSurfaceKHR
  , FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR
  , Wl_display
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR(..)
  , FN_vkCreateWin32SurfaceKHR
  , FN_vkGetPhysicalDeviceWin32PresentationSupportKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR(..)
  , FN_vkCreateXcbSurfaceKHR
  , FN_vkGetPhysicalDeviceXcbPresentationSupportKHR
  , Xcb_connection_t
  , Xcb_visualid_t
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , FN_vkCreateXlibSurfaceKHR
  , FN_vkGetPhysicalDeviceXlibPresentationSupportKHR
  , VisualID
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK(..)
  , FN_vkCreateIOSSurfaceMVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK(..)
  , FN_vkCreateMacOSSurfaceMVK
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN(..)
  , FN_vkCreateViSurfaceNN
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkObjectEntryTypeNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableEntryNVX(..)
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
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  , FN_vkGetImageViewHandleNVX
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkViewportWScalingNV(..)
  , FN_vkCmdSetViewportWScalingNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkCooperativeMatrixPropertiesNV(..)
  , FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , FN_vkCmdSetCheckpointNV
  , FN_vkGetQueueCheckpointDataNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryHandleTypeFlagBitsNV(..)
  , FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV
  , VkExternalMemoryHandleTypeFlagsNV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( FN_vkGetMemoryWin32HandleNV
  , HANDLE
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( FN_vkCmdDrawMeshTasksIndirectCountNV
  , FN_vkCmdDrawMeshTasksIndirectNV
  , FN_vkCmdDrawMeshTasksNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV(..)
  , VkAccelerationStructureInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsInfoNV(..)
  , VkBindAccelerationStructureMemoryInfoNV(..)
  , VkCopyAccelerationStructureModeNV(..)
  , VkRayTracingPipelineCreateInfoNV(..)
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
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( FN_vkCmdSetExclusiveScissorNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleOrderCustomNV(..)
  , VkCoarseSampleOrderTypeNV(..)
  , VkShadingRatePaletteNV(..)
  , FN_vkCmdBindShadingRateImageNV
  , FN_vkCmdSetCoarseSampleOrderNV
  , FN_vkCmdSetViewportShadingRatePaletteNV
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceVersion
  :: FunPtr (("pApiVersion" ::: Ptr Word32) -> IO VkResult) -> (("pApiVersion" ::: Ptr Word32) -> IO VkResult)

enumerateInstanceVersion :: ("pApiVersion" ::: Ptr Word32) -> IO VkResult
enumerateInstanceVersion = mkVkEnumerateInstanceVersion procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceVersion $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceVersion\NUL"#)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceExtensionProperties
  :: FunPtr (("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult) -> (("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)

enumerateInstanceExtensionProperties :: ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult
enumerateInstanceExtensionProperties = mkVkEnumerateInstanceExtensionProperties procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceExtensionProperties $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceExtensionProperties\NUL"#)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateInstanceLayerProperties
  :: FunPtr (("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult) -> (("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)

enumerateInstanceLayerProperties :: ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult
enumerateInstanceLayerProperties = mkVkEnumerateInstanceLayerProperties procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkEnumerateInstanceLayerProperties $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr nullPtr (GHC.Ptr.Ptr "vkEnumerateInstanceLayerProperties\NUL"#)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateInstance
  :: FunPtr (("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult) -> (("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult)

createInstance :: ("pCreateInfo" ::: Ptr VkInstanceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pInstance" ::: Ptr VkInstance) -> IO VkResult
createInstance = mkVkCreateInstance procAddr
  where
    procAddr = castPtrToFunPtr @_ @FN_vkCreateInstance $
      unsafeDupablePerformIO
        $ vkGetInstanceProcAddr nullPtr (GHC.Ptr.Ptr "vkCreateInstance\NUL"#)

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
  , pVkCmdDrawIndirectCountAMD :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
  , pVkCmdDrawIndexedIndirectCountAMD :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
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
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkAcquireFullScreenExclusiveModeEXT :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
  , pVkReleaseFullScreenExclusiveModeEXT :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
#endif
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
#if VK_USE_PLATFORM_WIN32_KHR
  , pVkGetPhysicalDeviceSurfacePresentModes2EXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#endif
  }
  deriving (Show)

initDeviceCmds :: InstanceCmds -> VkDevice -> IO DeviceCmds
initDeviceCmds instanceCmds handle = do
  pGetDeviceProcAddr <- castPtrToFunPtr @_ @FN_vkGetDeviceProcAddr
    <$> getInstanceProcAddr instanceCmds (instanceCmdsHandle instanceCmds) (GHC.Ptr.Ptr "vkGetDeviceProcAddr\NUL"#)
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
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndirectCountAMD <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndirectCountAMD\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkCmdDrawIndexedIndirectCountAMD <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkCmdDrawIndexedIndirectCountAMD\NUL"#))
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
#if VK_USE_PLATFORM_WIN32_KHR
    <*> (castPtrToFunPtr @_ @FN_vkAcquireFullScreenExclusiveModeEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkAcquireFullScreenExclusiveModeEXT\NUL"#))
    <*> (castPtrToFunPtr @_ @FN_vkReleaseFullScreenExclusiveModeEXT <$> getDeviceProcAddr' handle (GHC.Ptr.Ptr "vkReleaseFullScreenExclusiveModeEXT\NUL"#))
#endif

initInstanceCmds :: VkInstance -> IO InstanceCmds
initInstanceCmds handle = InstanceCmds handle
  <$> (castPtrToFunPtr @_ @FN_vkDestroyInstance <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroyInstance\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumeratePhysicalDevices <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumeratePhysicalDevices\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetInstanceProcAddr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetInstanceProcAddr\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceQueueFamilyProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceQueueFamilyProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceMemoryProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMemoryProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFeatures <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFeatures\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFormatProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFormatProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceImageFormatProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceImageFormatProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDevice <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDevice\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumerateDeviceLayerProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumerateDeviceLayerProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumerateDeviceExtensionProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumerateDeviceExtensionProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSparseImageFormatProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSparseImageFormatProperties\NUL"#))
#if VK_USE_PLATFORM_ANDROID_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateAndroidSurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateAndroidSurfaceKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayPropertiesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayPlanePropertiesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPlanePropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayPlaneSupportedDisplaysKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayPlaneSupportedDisplaysKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayModePropertiesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayModePropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDisplayModeKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDisplayModeKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayPlaneCapabilitiesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayPlaneCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDisplayPlaneSurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDisplayPlaneSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDestroySurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroySurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceSupportKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceSupportKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceCapabilitiesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceFormatsKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceFormatsKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfacePresentModesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfacePresentModesKHR\NUL"#))
#if VK_USE_PLATFORM_VI_NN
  <*> (castPtrToFunPtr @_ @FN_vkCreateViSurfaceNN <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateViSurfaceNN\NUL"#))
#endif
#if VK_USE_PLATFORM_WAYLAND_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateWaylandSurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateWaylandSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceWaylandPresentationSupportKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceWaylandPresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_WIN32_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateWin32SurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateWin32SurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceWin32PresentationSupportKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceWin32PresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_XLIB_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateXlibSurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateXlibSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceXlibPresentationSupportKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceXlibPresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_XCB_KHR
  <*> (castPtrToFunPtr @_ @FN_vkCreateXcbSurfaceKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateXcbSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceXcbPresentationSupportKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceXcbPresentationSupportKHR\NUL"#))
#endif
#if VK_USE_PLATFORM_FUCHSIA
  <*> (castPtrToFunPtr @_ @FN_vkCreateImagePipeSurfaceFUCHSIA <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateImagePipeSurfaceFUCHSIA\NUL"#))
#endif
#if VK_USE_PLATFORM_GGP
  <*> (castPtrToFunPtr @_ @FN_vkCreateStreamDescriptorSurfaceGGP <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateStreamDescriptorSurfaceGGP\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkCreateDebugReportCallbackEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDebugReportCallbackEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDestroyDebugReportCallbackEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDebugReportCallbackEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDebugReportMessageEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDebugReportMessageEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalImageFormatPropertiesNV <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalImageFormatPropertiesNV\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFeatures2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFeatures2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceProperties2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceFormatProperties2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceImageFormatProperties2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceImageFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceQueueFamilyProperties2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceQueueFamilyProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceMemoryProperties2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMemoryProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSparseImageFormatProperties2 <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalBufferProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalBufferProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalSemaphoreProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalSemaphoreProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceExternalFenceProperties <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalFenceProperties\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkReleaseDisplayEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkReleaseDisplayEXT\NUL"#))
#if VK_USE_PLATFORM_XLIB_XRANDR_EXT
  <*> (castPtrToFunPtr @_ @FN_vkAcquireXlibDisplayEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkAcquireXlibDisplayEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetRandROutputDisplayEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetRandROutputDisplayEXT\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceCapabilities2EXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilities2EXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkEnumeratePhysicalDeviceGroups <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumeratePhysicalDeviceGroups\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDevicePresentRectanglesKHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDevicePresentRectanglesKHR\NUL"#))
#if VK_USE_PLATFORM_IOS_MVK
  <*> (castPtrToFunPtr @_ @FN_vkCreateIOSSurfaceMVK <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateIOSSurfaceMVK\NUL"#))
#endif
#if VK_USE_PLATFORM_MACOS_MVK
  <*> (castPtrToFunPtr @_ @FN_vkCreateMacOSSurfaceMVK <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateMacOSSurfaceMVK\NUL"#))
#endif
#if VK_USE_PLATFORM_METAL_EXT
  <*> (castPtrToFunPtr @_ @FN_vkCreateMetalSurfaceEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateMetalSurfaceEXT\NUL"#))
#endif
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceMultisamplePropertiesEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMultisamplePropertiesEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceCapabilities2KHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilities2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfaceFormats2KHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceFormats2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayProperties2KHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayProperties2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceDisplayPlaneProperties2KHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPlaneProperties2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayModeProperties2KHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayModeProperties2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetDisplayPlaneCapabilities2KHR <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayPlaneCapabilities2KHR\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceCalibrateableTimeDomainsEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceCalibrateableTimeDomainsEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkCreateDebugUtilsMessengerEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDebugUtilsMessengerEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkDestroyDebugUtilsMessengerEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDebugUtilsMessengerEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkSubmitDebugUtilsMessageEXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkSubmitDebugUtilsMessageEXT\NUL"#))
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceCooperativeMatrixPropertiesNV <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceCooperativeMatrixPropertiesNV\NUL"#))
#if VK_USE_PLATFORM_WIN32_KHR
  <*> (castPtrToFunPtr @_ @FN_vkGetPhysicalDeviceSurfacePresentModes2EXT <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfacePresentModes2EXT\NUL"#))
#endif

-- * Device commands
getDeviceProcAddr :: DeviceCmds -> (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
getDeviceProcAddr deviceCmds = mkVkGetDeviceProcAddr (pVkGetDeviceProcAddr deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceProcAddr
  :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
destroyDevice :: DeviceCmds -> (("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyDevice deviceCmds = mkVkDestroyDevice (pVkDestroyDevice deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDevice
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getDeviceQueue :: DeviceCmds -> (("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
getDeviceQueue deviceCmds = mkVkGetDeviceQueue (pVkGetDeviceQueue deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue
  :: FunPtr (("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ()) -> (("device" ::: VkDevice) -> ("queueFamilyIndex" ::: Word32) -> ("queueIndex" ::: Word32) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
queueSubmit :: DeviceCmds -> (("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult)
queueSubmit deviceCmds = mkVkQueueSubmit (pVkQueueSubmit deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSubmit
  :: FunPtr (("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult) -> (("queue" ::: VkQueue) -> ("submitCount" ::: Word32) -> ("pSubmits" ::: Ptr VkSubmitInfo) -> ("fence" ::: VkFence) -> IO VkResult)
queueWaitIdle :: DeviceCmds -> (("queue" ::: VkQueue) -> IO VkResult)
queueWaitIdle deviceCmds = mkVkQueueWaitIdle (pVkQueueWaitIdle deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueWaitIdle
  :: FunPtr (("queue" ::: VkQueue) -> IO VkResult) -> (("queue" ::: VkQueue) -> IO VkResult)
deviceWaitIdle :: DeviceCmds -> (("device" ::: VkDevice) -> IO VkResult)
deviceWaitIdle deviceCmds = mkVkDeviceWaitIdle (pVkDeviceWaitIdle deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDeviceWaitIdle
  :: FunPtr (("device" ::: VkDevice) -> IO VkResult) -> (("device" ::: VkDevice) -> IO VkResult)
allocateMemory :: DeviceCmds -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult)
allocateMemory deviceCmds = mkVkAllocateMemory (pVkAllocateMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateMemory
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkMemoryAllocateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMemory" ::: Ptr VkDeviceMemory) -> IO VkResult)
freeMemory :: DeviceCmds -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
freeMemory deviceCmds = mkVkFreeMemory (pVkFreeMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
mapMemory :: DeviceCmds -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult)
mapMemory deviceCmds = mkVkMapMemory (pVkMapMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMapMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("offset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("flags" ::: VkMemoryMapFlags) -> ("ppData" ::: Ptr (Ptr ())) -> IO VkResult)
unmapMemory :: DeviceCmds -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ())
unmapMemory deviceCmds = mkVkUnmapMemory (pVkUnmapMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnmapMemory
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> IO ())
flushMappedMemoryRanges :: DeviceCmds -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
flushMappedMemoryRanges deviceCmds = mkVkFlushMappedMemoryRanges (pVkFlushMappedMemoryRanges deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFlushMappedMemoryRanges
  :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
invalidateMappedMemoryRanges :: DeviceCmds -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
invalidateMappedMemoryRanges deviceCmds = mkVkInvalidateMappedMemoryRanges (pVkInvalidateMappedMemoryRanges deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkInvalidateMappedMemoryRanges
  :: FunPtr (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memoryRangeCount" ::: Word32) -> ("pMemoryRanges" ::: Ptr VkMappedMemoryRange) -> IO VkResult)
getDeviceMemoryCommitment :: DeviceCmds -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ())
getDeviceMemoryCommitment deviceCmds = mkVkGetDeviceMemoryCommitment (pVkGetDeviceMemoryCommitment deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceMemoryCommitment
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ()) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("pCommittedMemoryInBytes" ::: Ptr VkDeviceSize) -> IO ())
getBufferMemoryRequirements :: DeviceCmds -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ())
getBufferMemoryRequirements deviceCmds = mkVkGetBufferMemoryRequirements (pVkGetBufferMemoryRequirements deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements
  :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()) -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ())
bindBufferMemory :: DeviceCmds -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult)
bindBufferMemory deviceCmds = mkVkBindBufferMemory (pVkBindBufferMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory
  :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult) -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult)
getImageMemoryRequirements :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ())
getImageMemoryRequirements deviceCmds = mkVkGetImageMemoryRequirements (pVkGetImageMemoryRequirements deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ()) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements) -> IO ())
bindImageMemory :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult)
bindImageMemory deviceCmds = mkVkBindImageMemory (pVkBindImageMemory deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> IO VkResult)
getImageSparseMemoryRequirements :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ())
getImageSparseMemoryRequirements deviceCmds = mkVkGetImageSparseMemoryRequirements (pVkGetImageSparseMemoryRequirements deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ()) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements) -> IO ())
queueBindSparse :: DeviceCmds -> (("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult)
queueBindSparse deviceCmds = mkVkQueueBindSparse (pVkQueueBindSparse deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBindSparse
  :: FunPtr (("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult) -> (("queue" ::: VkQueue) -> ("bindInfoCount" ::: Word32) -> ("pBindInfo" ::: Ptr VkBindSparseInfo) -> ("fence" ::: VkFence) -> IO VkResult)
createFence :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
createFence deviceCmds = mkVkCreateFence (pVkCreateFence deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFence
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFenceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
destroyFence :: DeviceCmds -> (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyFence deviceCmds = mkVkDestroyFence (pVkDestroyFence deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFence
  :: FunPtr (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
resetFences :: DeviceCmds -> (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult)
resetFences deviceCmds = mkVkResetFences (pVkResetFences deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetFences
  :: FunPtr (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> IO VkResult)
getFenceStatus :: DeviceCmds -> (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult)
getFenceStatus deviceCmds = mkVkGetFenceStatus (pVkGetFenceStatus deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceStatus
  :: FunPtr (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("fence" ::: VkFence) -> IO VkResult)
waitForFences :: DeviceCmds -> (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult)
waitForFences deviceCmds = mkVkWaitForFences (pVkWaitForFences deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkWaitForFences
  :: FunPtr (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult) -> (("device" ::: VkDevice) -> ("fenceCount" ::: Word32) -> ("pFences" ::: Ptr VkFence) -> ("waitAll" ::: VkBool32) -> ("timeout" ::: Word64) -> IO VkResult)
createSemaphore :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult)
createSemaphore deviceCmds = mkVkCreateSemaphore (pVkCreateSemaphore deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSemaphore
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSemaphoreCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSemaphore" ::: Ptr VkSemaphore) -> IO VkResult)
destroySemaphore :: DeviceCmds -> (("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroySemaphore deviceCmds = mkVkDestroySemaphore (pVkDestroySemaphore deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySemaphore
  :: FunPtr (("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("semaphore" ::: VkSemaphore) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createEvent :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult)
createEvent deviceCmds = mkVkCreateEvent (pVkCreateEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateEvent
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkEventCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pEvent" ::: Ptr VkEvent) -> IO VkResult)
destroyEvent :: DeviceCmds -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyEvent deviceCmds = mkVkDestroyEvent (pVkDestroyEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyEvent
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getEventStatus :: DeviceCmds -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
getEventStatus deviceCmds = mkVkGetEventStatus (pVkGetEventStatus deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetEventStatus
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
setEvent :: DeviceCmds -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
setEvent deviceCmds = mkVkSetEvent (pVkSetEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetEvent
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
resetEvent :: DeviceCmds -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
resetEvent deviceCmds = mkVkResetEvent (pVkResetEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetEvent
  :: FunPtr (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult) -> (("device" ::: VkDevice) -> ("event" ::: VkEvent) -> IO VkResult)
createQueryPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult)
createQueryPool deviceCmds = mkVkCreateQueryPool (pVkCreateQueryPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateQueryPool
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult)
destroyQueryPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyQueryPool deviceCmds = mkVkDestroyQueryPool (pVkDestroyQueryPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyQueryPool
  :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getQueryPoolResults :: DeviceCmds -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult)
getQueryPoolResults deviceCmds = mkVkGetQueryPoolResults (pVkGetQueryPoolResults deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueryPoolResults
  :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult) -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult)
resetQueryPoolEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
resetQueryPoolEXT deviceCmds = mkVkResetQueryPoolEXT (pVkResetQueryPoolEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetQueryPoolEXT
  :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()) -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
createBuffer :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult)
createBuffer deviceCmds = mkVkCreateBuffer (pVkCreateBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBuffer
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pBuffer" ::: Ptr VkBuffer) -> IO VkResult)
destroyBuffer :: DeviceCmds -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyBuffer deviceCmds = mkVkDestroyBuffer (pVkDestroyBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBuffer
  :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("buffer" ::: VkBuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createBufferView :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult)
createBufferView deviceCmds = mkVkCreateBufferView (pVkCreateBufferView deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateBufferView
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkBufferViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkBufferView) -> IO VkResult)
destroyBufferView :: DeviceCmds -> (("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyBufferView deviceCmds = mkVkDestroyBufferView (pVkDestroyBufferView deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyBufferView
  :: FunPtr (("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("bufferView" ::: VkBufferView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createImage :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult)
createImage deviceCmds = mkVkCreateImage (pVkCreateImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImage
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pImage" ::: Ptr VkImage) -> IO VkResult)
destroyImage :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyImage deviceCmds = mkVkDestroyImage (pVkDestroyImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImage
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getImageSubresourceLayout :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ())
getImageSubresourceLayout deviceCmds = mkVkGetImageSubresourceLayout (pVkGetImageSubresourceLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSubresourceLayout
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ()) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pSubresource" ::: Ptr VkImageSubresource) -> ("pLayout" ::: Ptr VkSubresourceLayout) -> IO ())
createImageView :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult)
createImageView deviceCmds = mkVkCreateImageView (pVkCreateImageView deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImageView
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkImageViewCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pView" ::: Ptr VkImageView) -> IO VkResult)
destroyImageView :: DeviceCmds -> (("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyImageView deviceCmds = mkVkDestroyImageView (pVkDestroyImageView deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyImageView
  :: FunPtr (("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("imageView" ::: VkImageView) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createShaderModule :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult)
createShaderModule deviceCmds = mkVkCreateShaderModule (pVkCreateShaderModule deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateShaderModule
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkShaderModuleCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pShaderModule" ::: Ptr VkShaderModule) -> IO VkResult)
destroyShaderModule :: DeviceCmds -> (("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyShaderModule deviceCmds = mkVkDestroyShaderModule (pVkDestroyShaderModule deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyShaderModule
  :: FunPtr (("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("shaderModule" ::: VkShaderModule) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createPipelineCache :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult)
createPipelineCache deviceCmds = mkVkCreatePipelineCache (pVkCreatePipelineCache deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineCache
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult)
destroyPipelineCache :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyPipelineCache deviceCmds = mkVkDestroyPipelineCache (pVkDestroyPipelineCache deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineCache
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getPipelineCacheData :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
getPipelineCacheData deviceCmds = mkVkGetPipelineCacheData (pVkGetPipelineCacheData deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPipelineCacheData
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
mergePipelineCaches :: DeviceCmds -> (("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult)
mergePipelineCaches deviceCmds = mkVkMergePipelineCaches (pVkMergePipelineCaches deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergePipelineCaches
  :: FunPtr (("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult) -> (("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult)
createGraphicsPipelines :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
createGraphicsPipelines deviceCmds = mkVkCreateGraphicsPipelines (pVkCreateGraphicsPipelines deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateGraphicsPipelines
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkGraphicsPipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
createComputePipelines :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
createComputePipelines deviceCmds = mkVkCreateComputePipelines (pVkCreateComputePipelines deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateComputePipelines
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkComputePipelineCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
destroyPipeline :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyPipeline deviceCmds = mkVkDestroyPipeline (pVkDestroyPipeline deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipeline
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createPipelineLayout :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult)
createPipelineLayout deviceCmds = mkVkCreatePipelineLayout (pVkCreatePipelineLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreatePipelineLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineLayout" ::: Ptr VkPipelineLayout) -> IO VkResult)
destroyPipelineLayout :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyPipelineLayout deviceCmds = mkVkDestroyPipelineLayout (pVkDestroyPipelineLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyPipelineLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("pipelineLayout" ::: VkPipelineLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createSampler :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult)
createSampler deviceCmds = mkVkCreateSampler (pVkCreateSampler deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSampler
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSampler" ::: Ptr VkSampler) -> IO VkResult)
destroySampler :: DeviceCmds -> (("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroySampler deviceCmds = mkVkDestroySampler (pVkDestroySampler deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySampler
  :: FunPtr (("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("sampler" ::: VkSampler) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createDescriptorSetLayout :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult)
createDescriptorSetLayout deviceCmds = mkVkCreateDescriptorSetLayout (pVkCreateDescriptorSetLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorSetLayout
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSetLayout" ::: Ptr VkDescriptorSetLayout) -> IO VkResult)
destroyDescriptorSetLayout :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyDescriptorSetLayout deviceCmds = mkVkDestroyDescriptorSetLayout (pVkDestroyDescriptorSetLayout deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorSetLayout
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorSetLayout" ::: VkDescriptorSetLayout) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createDescriptorPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult)
createDescriptorPool deviceCmds = mkVkCreateDescriptorPool (pVkCreateDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorPool" ::: Ptr VkDescriptorPool) -> IO VkResult)
destroyDescriptorPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyDescriptorPool deviceCmds = mkVkDestroyDescriptorPool (pVkDestroyDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
resetDescriptorPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult)
resetDescriptorPool deviceCmds = mkVkResetDescriptorPool (pVkResetDescriptorPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetDescriptorPool
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("flags" ::: VkDescriptorPoolResetFlags) -> IO VkResult)
allocateDescriptorSets :: DeviceCmds -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
allocateDescriptorSets deviceCmds = mkVkAllocateDescriptorSets (pVkAllocateDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkDescriptorSetAllocateInfo) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
freeDescriptorSets :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
freeDescriptorSets deviceCmds = mkVkFreeDescriptorSets (pVkFreeDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult) -> (("device" ::: VkDevice) -> ("descriptorPool" ::: VkDescriptorPool) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> IO VkResult)
updateDescriptorSets :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ())
updateDescriptorSets deviceCmds = mkVkUpdateDescriptorSets (pVkUpdateDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSets
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> ("descriptorCopyCount" ::: Word32) -> ("pDescriptorCopies" ::: Ptr VkCopyDescriptorSet) -> IO ())
createFramebuffer :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult)
createFramebuffer deviceCmds = mkVkCreateFramebuffer (pVkCreateFramebuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateFramebuffer
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkFramebufferCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFramebuffer" ::: Ptr VkFramebuffer) -> IO VkResult)
destroyFramebuffer :: DeviceCmds -> (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyFramebuffer deviceCmds = mkVkDestroyFramebuffer (pVkDestroyFramebuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyFramebuffer
  :: FunPtr (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("framebuffer" ::: VkFramebuffer) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createRenderPass :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
createRenderPass deviceCmds = mkVkCreateRenderPass (pVkCreateRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
destroyRenderPass :: DeviceCmds -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyRenderPass deviceCmds = mkVkDestroyRenderPass (pVkDestroyRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyRenderPass
  :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getRenderAreaGranularity :: DeviceCmds -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ())
getRenderAreaGranularity deviceCmds = mkVkGetRenderAreaGranularity (pVkGetRenderAreaGranularity deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRenderAreaGranularity
  :: FunPtr (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ()) -> (("device" ::: VkDevice) -> ("renderPass" ::: VkRenderPass) -> ("pGranularity" ::: Ptr VkExtent2D) -> IO ())
createCommandPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult)
createCommandPool deviceCmds = mkVkCreateCommandPool (pVkCreateCommandPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateCommandPool
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkCommandPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCommandPool" ::: Ptr VkCommandPool) -> IO VkResult)
destroyCommandPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyCommandPool deviceCmds = mkVkDestroyCommandPool (pVkDestroyCommandPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyCommandPool
  :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
resetCommandPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult)
resetCommandPool deviceCmds = mkVkResetCommandPool (pVkResetCommandPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandPool
  :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult) -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolResetFlags) -> IO VkResult)
allocateCommandBuffers :: DeviceCmds -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult)
allocateCommandBuffers deviceCmds = mkVkAllocateCommandBuffers (pVkAllocateCommandBuffers deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAllocateCommandBuffers
  :: FunPtr (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAllocateInfo" ::: Ptr VkCommandBufferAllocateInfo) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO VkResult)
freeCommandBuffers :: DeviceCmds -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
freeCommandBuffers deviceCmds = mkVkFreeCommandBuffers (pVkFreeCommandBuffers deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkFreeCommandBuffers
  :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()) -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
beginCommandBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult)
beginCommandBuffer deviceCmds = mkVkBeginCommandBuffer (pVkBeginCommandBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBeginCommandBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pBeginInfo" ::: Ptr VkCommandBufferBeginInfo) -> IO VkResult)
endCommandBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> IO VkResult)
endCommandBuffer deviceCmds = mkVkEndCommandBuffer (pVkEndCommandBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEndCommandBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO VkResult) -> (("commandBuffer" ::: VkCommandBuffer) -> IO VkResult)
resetCommandBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult)
resetCommandBuffer deviceCmds = mkVkResetCommandBuffer (pVkResetCommandBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkResetCommandBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult) -> (("commandBuffer" ::: VkCommandBuffer) -> ("flags" ::: VkCommandBufferResetFlags) -> IO VkResult)
cmdBindPipeline :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ())
cmdBindPipeline deviceCmds = mkVkCmdBindPipeline (pVkCmdBindPipeline deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindPipeline
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("pipeline" ::: VkPipeline) -> IO ())
cmdSetViewport :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ())
cmdSetViewport deviceCmds = mkVkCmdSetViewport (pVkCmdSetViewport deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewport
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewports" ::: Ptr VkViewport) -> IO ())
cmdSetScissor :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ())
cmdSetScissor deviceCmds = mkVkCmdSetScissor (pVkCmdSetScissor deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetScissor
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstScissor" ::: Word32) -> ("scissorCount" ::: Word32) -> ("pScissors" ::: Ptr VkRect2D) -> IO ())
cmdSetLineWidth :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ())
cmdSetLineWidth deviceCmds = mkVkCmdSetLineWidth (pVkCmdSetLineWidth deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetLineWidth
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("lineWidth" ::: CFloat) -> IO ())
cmdSetDepthBias :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ())
cmdSetDepthBias deviceCmds = mkVkCmdSetDepthBias (pVkCmdSetDepthBias deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBias
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("depthBiasConstantFactor" ::: CFloat) -> ("depthBiasClamp" ::: CFloat) -> ("depthBiasSlopeFactor" ::: CFloat) -> IO ())
cmdSetBlendConstants :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ())
cmdSetBlendConstants deviceCmds = mkVkCmdSetBlendConstants (pVkCmdSetBlendConstants deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetBlendConstants
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("blendConstants" ::: Ptr CFloat) -> IO ())
cmdSetDepthBounds :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ())
cmdSetDepthBounds deviceCmds = mkVkCmdSetDepthBounds (pVkCmdSetDepthBounds deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDepthBounds
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("minDepthBounds" ::: CFloat) -> ("maxDepthBounds" ::: CFloat) -> IO ())
cmdSetStencilCompareMask :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ())
cmdSetStencilCompareMask deviceCmds = mkVkCmdSetStencilCompareMask (pVkCmdSetStencilCompareMask deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilCompareMask
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("compareMask" ::: Word32) -> IO ())
cmdSetStencilWriteMask :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ())
cmdSetStencilWriteMask deviceCmds = mkVkCmdSetStencilWriteMask (pVkCmdSetStencilWriteMask deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilWriteMask
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("writeMask" ::: Word32) -> IO ())
cmdSetStencilReference :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ())
cmdSetStencilReference deviceCmds = mkVkCmdSetStencilReference (pVkCmdSetStencilReference deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetStencilReference
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("faceMask" ::: VkStencilFaceFlags) -> ("reference" ::: Word32) -> IO ())
cmdBindDescriptorSets :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ())
cmdBindDescriptorSets deviceCmds = mkVkCmdBindDescriptorSets (pVkCmdBindDescriptorSets deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindDescriptorSets
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("firstSet" ::: Word32) -> ("descriptorSetCount" ::: Word32) -> ("pDescriptorSets" ::: Ptr VkDescriptorSet) -> ("dynamicOffsetCount" ::: Word32) -> ("pDynamicOffsets" ::: Ptr Word32) -> IO ())
cmdBindIndexBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ())
cmdBindIndexBuffer deviceCmds = mkVkCmdBindIndexBuffer (pVkCmdBindIndexBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindIndexBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("indexType" ::: VkIndexType) -> IO ())
cmdBindVertexBuffers :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ())
cmdBindVertexBuffers deviceCmds = mkVkCmdBindVertexBuffers (pVkCmdBindVertexBuffers deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindVertexBuffers
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> IO ())
cmdDraw :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ())
cmdDraw deviceCmds = mkVkCmdDraw (pVkCmdDraw deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDraw
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("vertexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstVertex" ::: Word32) -> ("firstInstance" ::: Word32) -> IO ())
cmdDrawIndexed :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ())
cmdDrawIndexed deviceCmds = mkVkCmdDrawIndexed (pVkCmdDrawIndexed deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexed
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("indexCount" ::: Word32) -> ("instanceCount" ::: Word32) -> ("firstIndex" ::: Word32) -> ("vertexOffset" ::: Int32) -> ("firstInstance" ::: Word32) -> IO ())
cmdDrawIndirect :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndirect deviceCmds = mkVkCmdDrawIndirect (pVkCmdDrawIndirect deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirect
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndexedIndirect :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndexedIndirect deviceCmds = mkVkCmdDrawIndexedIndirect (pVkCmdDrawIndexedIndirect deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirect
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDispatch :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
cmdDispatch deviceCmds = mkVkCmdDispatch (pVkCmdDispatch deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatch
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
cmdDispatchIndirect :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ())
cmdDispatchIndirect deviceCmds = mkVkCmdDispatchIndirect (pVkCmdDispatchIndirect deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchIndirect
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> IO ())
cmdCopyBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ())
cmdCopyBuffer deviceCmds = mkVkCmdCopyBuffer (pVkCmdCopyBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferCopy) -> IO ())
cmdCopyImage :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ())
cmdCopyImage deviceCmds = mkVkCmdCopyImage (pVkCmdCopyImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImage
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageCopy) -> IO ())
cmdBlitImage :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ())
cmdBlitImage deviceCmds = mkVkCmdBlitImage (pVkCmdBlitImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBlitImage
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageBlit) -> ("filter" ::: VkFilter) -> IO ())
cmdCopyBufferToImage :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ())
cmdCopyBufferToImage deviceCmds = mkVkCmdCopyBufferToImage (pVkCmdCopyBufferToImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyBufferToImage
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcBuffer" ::: VkBuffer) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ())
cmdCopyImageToBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ())
cmdCopyImageToBuffer deviceCmds = mkVkCmdCopyImageToBuffer (pVkCmdCopyImageToBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyImageToBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstBuffer" ::: VkBuffer) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkBufferImageCopy) -> IO ())
cmdUpdateBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ())
cmdUpdateBuffer deviceCmds = mkVkCmdUpdateBuffer (pVkCmdUpdateBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdUpdateBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("dataSize" ::: VkDeviceSize) -> ("pData" ::: Ptr ()) -> IO ())
cmdFillBuffer :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ())
cmdFillBuffer deviceCmds = mkVkCmdFillBuffer (pVkCmdFillBuffer deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdFillBuffer
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("size" ::: VkDeviceSize) -> ("data" ::: Word32) -> IO ())
cmdClearColorImage :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ())
cmdClearColorImage deviceCmds = mkVkCmdClearColorImage (pVkCmdClearColorImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearColorImage
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pColor" ::: Ptr VkClearColorValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ())
cmdClearDepthStencilImage :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ())
cmdClearDepthStencilImage deviceCmds = mkVkCmdClearDepthStencilImage (pVkCmdClearDepthStencilImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearDepthStencilImage
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("image" ::: VkImage) -> ("imageLayout" ::: VkImageLayout) -> ("pDepthStencil" ::: Ptr VkClearDepthStencilValue) -> ("rangeCount" ::: Word32) -> ("pRanges" ::: Ptr VkImageSubresourceRange) -> IO ())
cmdClearAttachments :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ())
cmdClearAttachments deviceCmds = mkVkCmdClearAttachments (pVkCmdClearAttachments deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdClearAttachments
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("attachmentCount" ::: Word32) -> ("pAttachments" ::: Ptr VkClearAttachment) -> ("rectCount" ::: Word32) -> ("pRects" ::: Ptr VkClearRect) -> IO ())
cmdResolveImage :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ())
cmdResolveImage deviceCmds = mkVkCmdResolveImage (pVkCmdResolveImage deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResolveImage
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcImage" ::: VkImage) -> ("srcImageLayout" ::: VkImageLayout) -> ("dstImage" ::: VkImage) -> ("dstImageLayout" ::: VkImageLayout) -> ("regionCount" ::: Word32) -> ("pRegions" ::: Ptr VkImageResolve) -> IO ())
cmdSetEvent :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ())
cmdSetEvent deviceCmds = mkVkCmdSetEvent (pVkCmdSetEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetEvent
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ())
cmdResetEvent :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ())
cmdResetEvent deviceCmds = mkVkCmdResetEvent (pVkCmdResetEvent deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetEvent
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("event" ::: VkEvent) -> ("stageMask" ::: VkPipelineStageFlags) -> IO ())
cmdWaitEvents :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ())
cmdWaitEvents deviceCmds = mkVkCmdWaitEvents (pVkCmdWaitEvents deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWaitEvents
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("eventCount" ::: Word32) -> ("pEvents" ::: Ptr VkEvent) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ())
cmdPipelineBarrier :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ())
cmdPipelineBarrier deviceCmds = mkVkCmdPipelineBarrier (pVkCmdPipelineBarrier deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPipelineBarrier
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("srcStageMask" ::: VkPipelineStageFlags) -> ("dstStageMask" ::: VkPipelineStageFlags) -> ("dependencyFlags" ::: VkDependencyFlags) -> ("memoryBarrierCount" ::: Word32) -> ("pMemoryBarriers" ::: Ptr VkMemoryBarrier) -> ("bufferMemoryBarrierCount" ::: Word32) -> ("pBufferMemoryBarriers" ::: Ptr VkBufferMemoryBarrier) -> ("imageMemoryBarrierCount" ::: Word32) -> ("pImageMemoryBarriers" ::: Ptr VkImageMemoryBarrier) -> IO ())
cmdBeginQuery :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ())
cmdBeginQuery deviceCmds = mkVkCmdBeginQuery (pVkCmdBeginQuery deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQuery
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> IO ())
cmdEndQuery :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ())
cmdEndQuery deviceCmds = mkVkCmdEndQuery (pVkCmdEndQuery deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQuery
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ())
cmdBeginConditionalRenderingEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ())
cmdBeginConditionalRenderingEXT deviceCmds = mkVkCmdBeginConditionalRenderingEXT (pVkCmdBeginConditionalRenderingEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginConditionalRenderingEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pConditionalRenderingBegin" ::: Ptr VkConditionalRenderingBeginInfoEXT) -> IO ())
cmdEndConditionalRenderingEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdEndConditionalRenderingEXT deviceCmds = mkVkCmdEndConditionalRenderingEXT (pVkCmdEndConditionalRenderingEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndConditionalRenderingEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdResetQueryPool :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
cmdResetQueryPool deviceCmds = mkVkCmdResetQueryPool (pVkCmdResetQueryPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdResetQueryPool
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> IO ())
cmdWriteTimestamp :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ())
cmdWriteTimestamp deviceCmds = mkVkCmdWriteTimestamp (pVkCmdWriteTimestamp deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteTimestamp
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> IO ())
cmdCopyQueryPoolResults :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ())
cmdCopyQueryPoolResults deviceCmds = mkVkCmdCopyQueryPoolResults (pVkCmdCopyQueryPoolResults deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyQueryPoolResults
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO ())
cmdPushConstants :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ())
cmdPushConstants deviceCmds = mkVkCmdPushConstants (pVkCmdPushConstants deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushConstants
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("layout" ::: VkPipelineLayout) -> ("stageFlags" ::: VkShaderStageFlags) -> ("offset" ::: Word32) -> ("size" ::: Word32) -> ("pValues" ::: Ptr ()) -> IO ())
cmdBeginRenderPass :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ())
cmdBeginRenderPass deviceCmds = mkVkCmdBeginRenderPass (pVkCmdBeginRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("contents" ::: VkSubpassContents) -> IO ())
cmdNextSubpass :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ())
cmdNextSubpass deviceCmds = mkVkCmdNextSubpass (pVkCmdNextSubpass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("contents" ::: VkSubpassContents) -> IO ())
cmdEndRenderPass :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdEndRenderPass deviceCmds = mkVkCmdEndRenderPass (pVkCmdEndRenderPass deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdExecuteCommands :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
cmdExecuteCommands deviceCmds = mkVkCmdExecuteCommands (pVkCmdExecuteCommands deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdExecuteCommands
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("commandBufferCount" ::: Word32) -> ("pCommandBuffers" ::: Ptr VkCommandBuffer) -> IO ())
createSharedSwapchainsKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult)
createSharedSwapchainsKHR deviceCmds = mkVkCreateSharedSwapchainsKHR (pVkCreateSharedSwapchainsKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSharedSwapchainsKHR
  :: FunPtr (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> IO VkResult)
createSwapchainKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult)
createSwapchainKHR deviceCmds = mkVkCreateSwapchainKHR (pVkCreateSwapchainKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSwapchainKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSwapchainCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSwapchain" ::: Ptr VkSwapchainKHR) -> IO VkResult)
destroySwapchainKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroySwapchainKHR deviceCmds = mkVkDestroySwapchainKHR (pVkDestroySwapchainKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySwapchainKHR
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getSwapchainImagesKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult)
getSwapchainImagesKHR deviceCmds = mkVkGetSwapchainImagesKHR (pVkGetSwapchainImagesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainImagesKHR
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pSwapchainImageCount" ::: Ptr Word32) -> ("pSwapchainImages" ::: Ptr VkImage) -> IO VkResult)
acquireNextImageKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult)
acquireNextImageKHR deviceCmds = mkVkAcquireNextImageKHR (pVkAcquireNextImageKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireNextImageKHR
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("timeout" ::: Word64) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult)
queuePresentKHR :: DeviceCmds -> (("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult)
queuePresentKHR deviceCmds = mkVkQueuePresentKHR (pVkQueuePresentKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueuePresentKHR
  :: FunPtr (("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult) -> (("queue" ::: VkQueue) -> ("pPresentInfo" ::: Ptr VkPresentInfoKHR) -> IO VkResult)
debugMarkerSetObjectNameEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult)
debugMarkerSetObjectNameEXT deviceCmds = mkVkDebugMarkerSetObjectNameEXT (pVkDebugMarkerSetObjectNameEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectNameEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugMarkerObjectNameInfoEXT) -> IO VkResult)
debugMarkerSetObjectTagEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult)
debugMarkerSetObjectTagEXT deviceCmds = mkVkDebugMarkerSetObjectTagEXT (pVkDebugMarkerSetObjectTagEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugMarkerSetObjectTagEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugMarkerObjectTagInfoEXT) -> IO VkResult)
cmdDebugMarkerBeginEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
cmdDebugMarkerBeginEXT deviceCmds = mkVkCmdDebugMarkerBeginEXT (pVkCmdDebugMarkerBeginEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerBeginEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
cmdDebugMarkerEndEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdDebugMarkerEndEXT deviceCmds = mkVkCmdDebugMarkerEndEXT (pVkCmdDebugMarkerEndEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerEndEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdDebugMarkerInsertEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
cmdDebugMarkerInsertEXT deviceCmds = mkVkCmdDebugMarkerInsertEXT (pVkCmdDebugMarkerInsertEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDebugMarkerInsertEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pMarkerInfo" ::: Ptr VkDebugMarkerMarkerInfoEXT) -> IO ())
#if VK_USE_PLATFORM_WIN32_KHR
getMemoryWin32HandleNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
getMemoryWin32HandleNV deviceCmds = mkVkGetMemoryWin32HandleNV (pVkGetMemoryWin32HandleNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleNV
  :: FunPtr (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("memory" ::: VkDeviceMemory) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
#endif
cmdDrawIndirectCountAMD :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndirectCountAMD deviceCmds = mkVkCmdDrawIndirectCountAMD (pVkCmdDrawIndirectCountAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectCountAMD
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndexedIndirectCountAMD :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndexedIndirectCountAMD deviceCmds = mkVkCmdDrawIndexedIndirectCountAMD (pVkCmdDrawIndexedIndirectCountAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirectCountAMD
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdProcessCommandsNVX :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ())
cmdProcessCommandsNVX deviceCmds = mkVkCmdProcessCommandsNVX (pVkCmdProcessCommandsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdProcessCommandsNVX
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pProcessCommandsInfo" ::: Ptr VkCmdProcessCommandsInfoNVX) -> IO ())
cmdReserveSpaceForCommandsNVX :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ())
cmdReserveSpaceForCommandsNVX deviceCmds = mkVkCmdReserveSpaceForCommandsNVX (pVkCmdReserveSpaceForCommandsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdReserveSpaceForCommandsNVX
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pReserveSpaceInfo" ::: Ptr VkCmdReserveSpaceForCommandsInfoNVX) -> IO ())
createIndirectCommandsLayoutNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult)
createIndirectCommandsLayoutNVX deviceCmds = mkVkCreateIndirectCommandsLayoutNVX (pVkCreateIndirectCommandsLayoutNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIndirectCommandsLayoutNVX
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkIndirectCommandsLayoutCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pIndirectCommandsLayout" ::: Ptr VkIndirectCommandsLayoutNVX) -> IO VkResult)
destroyIndirectCommandsLayoutNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyIndirectCommandsLayoutNVX deviceCmds = mkVkDestroyIndirectCommandsLayoutNVX (pVkDestroyIndirectCommandsLayoutNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyIndirectCommandsLayoutNVX
  :: FunPtr (("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("indirectCommandsLayout" ::: VkIndirectCommandsLayoutNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
createObjectTableNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult)
createObjectTableNVX deviceCmds = mkVkCreateObjectTableNVX (pVkCreateObjectTableNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateObjectTableNVX
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkObjectTableCreateInfoNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pObjectTable" ::: Ptr VkObjectTableNVX) -> IO VkResult)
destroyObjectTableNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyObjectTableNVX deviceCmds = mkVkDestroyObjectTableNVX (pVkDestroyObjectTableNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyObjectTableNVX
  :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
registerObjectsNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
registerObjectsNVX deviceCmds = mkVkRegisterObjectsNVX (pVkRegisterObjectsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterObjectsNVX
  :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("ppObjectTableEntries" ::: Ptr (Ptr VkObjectTableEntryNVX)) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
unregisterObjectsNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
unregisterObjectsNVX deviceCmds = mkVkUnregisterObjectsNVX (pVkUnregisterObjectsNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUnregisterObjectsNVX
  :: FunPtr (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("objectTable" ::: VkObjectTableNVX) -> ("objectCount" ::: Word32) -> ("pObjectEntryTypes" ::: Ptr VkObjectEntryTypeNVX) -> ("pObjectIndices" ::: Ptr Word32) -> IO VkResult)
cmdPushDescriptorSetKHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ())
cmdPushDescriptorSetKHR deviceCmds = mkVkCmdPushDescriptorSetKHR (pVkCmdPushDescriptorSetKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineBindPoint" ::: VkPipelineBindPoint) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("descriptorWriteCount" ::: Word32) -> ("pDescriptorWrites" ::: Ptr VkWriteDescriptorSet) -> IO ())
trimCommandPool :: DeviceCmds -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ())
trimCommandPool deviceCmds = mkVkTrimCommandPool (pVkTrimCommandPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkTrimCommandPool
  :: FunPtr (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ()) -> (("device" ::: VkDevice) -> ("commandPool" ::: VkCommandPool) -> ("flags" ::: VkCommandPoolTrimFlags) -> IO ())
#if VK_USE_PLATFORM_WIN32_KHR
getMemoryWin32HandleKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
getMemoryWin32HandleKHR deviceCmds = mkVkGetMemoryWin32HandleKHR (pVkGetMemoryWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
getMemoryWin32HandlePropertiesKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult)
getMemoryWin32HandlePropertiesKHR deviceCmds = mkVkGetMemoryWin32HandlePropertiesKHR (pVkGetMemoryWin32HandlePropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryWin32HandlePropertiesKHR
  :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult)
#endif
getMemoryFdKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
getMemoryFdKHR deviceCmds = mkVkGetMemoryFdKHR (pVkGetMemoryFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
getMemoryFdPropertiesKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult)
getMemoryFdPropertiesKHR deviceCmds = mkVkGetMemoryFdPropertiesKHR (pVkGetMemoryFdPropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryFdPropertiesKHR
  :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult)
#if VK_USE_PLATFORM_WIN32_KHR
getSemaphoreWin32HandleKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
getSemaphoreWin32HandleKHR deviceCmds = mkVkGetSemaphoreWin32HandleKHR (pVkGetSemaphoreWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
importSemaphoreWin32HandleKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult)
importSemaphoreWin32HandleKHR deviceCmds = mkVkImportSemaphoreWin32HandleKHR (pVkImportSemaphoreWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult)
#endif
getSemaphoreFdKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
getSemaphoreFdKHR deviceCmds = mkVkGetSemaphoreFdKHR (pVkGetSemaphoreFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSemaphoreFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
importSemaphoreFdKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult)
importSemaphoreFdKHR deviceCmds = mkVkImportSemaphoreFdKHR (pVkImportSemaphoreFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportSemaphoreFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult)
#if VK_USE_PLATFORM_WIN32_KHR
getFenceWin32HandleKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
getFenceWin32HandleKHR deviceCmds = mkVkGetFenceWin32HandleKHR (pVkGetFenceWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkFenceGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
importFenceWin32HandleKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult)
importFenceWin32HandleKHR deviceCmds = mkVkImportFenceWin32HandleKHR (pVkImportFenceWin32HandleKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceWin32HandleKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportFenceWin32HandleInfo" ::: Ptr VkImportFenceWin32HandleInfoKHR) -> IO VkResult)
#endif
getFenceFdKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
getFenceFdKHR deviceCmds = mkVkGetFenceFdKHR (pVkGetFenceFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetFenceFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkFenceGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
importFenceFdKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult)
importFenceFdKHR deviceCmds = mkVkImportFenceFdKHR (pVkImportFenceFdKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkImportFenceFdKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pImportFenceFdInfo" ::: Ptr VkImportFenceFdInfoKHR) -> IO VkResult)
displayPowerControlEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult)
displayPowerControlEXT deviceCmds = mkVkDisplayPowerControlEXT (pVkDisplayPowerControlEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDisplayPowerControlEXT
  :: FunPtr (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayPowerInfo" ::: Ptr VkDisplayPowerInfoEXT) -> IO VkResult)
registerDeviceEventEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
registerDeviceEventEXT deviceCmds = mkVkRegisterDeviceEventEXT (pVkRegisterDeviceEventEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDeviceEventEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pDeviceEventInfo" ::: Ptr VkDeviceEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
registerDisplayEventEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
registerDisplayEventEXT deviceCmds = mkVkRegisterDisplayEventEXT (pVkRegisterDisplayEventEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkRegisterDisplayEventEXT
  :: FunPtr (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("display" ::: VkDisplayKHR) -> ("pDisplayEventInfo" ::: Ptr VkDisplayEventInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pFence" ::: Ptr VkFence) -> IO VkResult)
getSwapchainCounterEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult)
getSwapchainCounterEXT deviceCmds = mkVkGetSwapchainCounterEXT (pVkGetSwapchainCounterEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainCounterEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("counter" ::: VkSurfaceCounterFlagBitsEXT) -> ("pCounterValue" ::: Ptr Word64) -> IO VkResult)
getDeviceGroupPeerMemoryFeatures :: DeviceCmds -> (("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ())
getDeviceGroupPeerMemoryFeatures deviceCmds = mkVkGetDeviceGroupPeerMemoryFeatures (pVkGetDeviceGroupPeerMemoryFeatures deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPeerMemoryFeatures
  :: FunPtr (("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ()) -> (("device" ::: VkDevice) -> ("heapIndex" ::: Word32) -> ("localDeviceIndex" ::: Word32) -> ("remoteDeviceIndex" ::: Word32) -> ("pPeerMemoryFeatures" ::: Ptr VkPeerMemoryFeatureFlags) -> IO ())
bindBufferMemory2 :: DeviceCmds -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult)
bindBufferMemory2 deviceCmds = mkVkBindBufferMemory2 (pVkBindBufferMemory2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory2
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult)
bindImageMemory2 :: DeviceCmds -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult)
bindImageMemory2 deviceCmds = mkVkBindImageMemory2 (pVkBindImageMemory2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory2
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult)
cmdSetDeviceMask :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ())
cmdSetDeviceMask deviceCmds = mkVkCmdSetDeviceMask (pVkCmdSetDeviceMask deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDeviceMask
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("deviceMask" ::: Word32) -> IO ())
getDeviceGroupPresentCapabilitiesKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult)
getDeviceGroupPresentCapabilitiesKHR deviceCmds = mkVkGetDeviceGroupPresentCapabilitiesKHR (pVkGetDeviceGroupPresentCapabilitiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupPresentCapabilitiesKHR
  :: FunPtr (("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pDeviceGroupPresentCapabilities" ::: Ptr VkDeviceGroupPresentCapabilitiesKHR) -> IO VkResult)
getDeviceGroupSurfacePresentModesKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
getDeviceGroupSurfacePresentModesKHR deviceCmds = mkVkGetDeviceGroupSurfacePresentModesKHR (pVkGetDeviceGroupSurfacePresentModesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModesKHR
  :: FunPtr (("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
acquireNextImage2KHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult)
acquireNextImage2KHR deviceCmds = mkVkAcquireNextImage2KHR (pVkAcquireNextImage2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireNextImage2KHR
  :: FunPtr (("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pAcquireInfo" ::: Ptr VkAcquireNextImageInfoKHR) -> ("pImageIndex" ::: Ptr Word32) -> IO VkResult)
cmdDispatchBase :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
cmdDispatchBase deviceCmds = mkVkCmdDispatchBase (pVkCmdDispatchBase deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDispatchBase
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("baseGroupX" ::: Word32) -> ("baseGroupY" ::: Word32) -> ("baseGroupZ" ::: Word32) -> ("groupCountX" ::: Word32) -> ("groupCountY" ::: Word32) -> ("groupCountZ" ::: Word32) -> IO ())
createDescriptorUpdateTemplate :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult)
createDescriptorUpdateTemplate deviceCmds = mkVkCreateDescriptorUpdateTemplate (pVkCreateDescriptorUpdateTemplate deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDescriptorUpdateTemplate
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorUpdateTemplateCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDescriptorUpdateTemplate" ::: Ptr VkDescriptorUpdateTemplate) -> IO VkResult)
destroyDescriptorUpdateTemplate :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyDescriptorUpdateTemplate deviceCmds = mkVkDestroyDescriptorUpdateTemplate (pVkDestroyDescriptorUpdateTemplate deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDescriptorUpdateTemplate
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
updateDescriptorSetWithTemplate :: DeviceCmds -> (("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ())
updateDescriptorSetWithTemplate deviceCmds = mkVkUpdateDescriptorSetWithTemplate (pVkUpdateDescriptorSetWithTemplate deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkUpdateDescriptorSetWithTemplate
  :: FunPtr (("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ()) -> (("device" ::: VkDevice) -> ("descriptorSet" ::: VkDescriptorSet) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("pData" ::: Ptr ()) -> IO ())
cmdPushDescriptorSetWithTemplateKHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ())
cmdPushDescriptorSetWithTemplateKHR deviceCmds = mkVkCmdPushDescriptorSetWithTemplateKHR (pVkCmdPushDescriptorSetWithTemplateKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdPushDescriptorSetWithTemplateKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("descriptorUpdateTemplate" ::: VkDescriptorUpdateTemplate) -> ("layout" ::: VkPipelineLayout) -> ("set" ::: Word32) -> ("pData" ::: Ptr ()) -> IO ())
setHdrMetadataEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ())
setHdrMetadataEXT deviceCmds = mkVkSetHdrMetadataEXT (pVkSetHdrMetadataEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetHdrMetadataEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ()) -> (("device" ::: VkDevice) -> ("swapchainCount" ::: Word32) -> ("pSwapchains" ::: Ptr VkSwapchainKHR) -> ("pMetadata" ::: Ptr VkHdrMetadataEXT) -> IO ())
getSwapchainStatusKHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
getSwapchainStatusKHR deviceCmds = mkVkGetSwapchainStatusKHR (pVkGetSwapchainStatusKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainStatusKHR
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
getRefreshCycleDurationGOOGLE :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult)
getRefreshCycleDurationGOOGLE deviceCmds = mkVkGetRefreshCycleDurationGOOGLE (pVkGetRefreshCycleDurationGOOGLE deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRefreshCycleDurationGOOGLE
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pDisplayTimingProperties" ::: Ptr VkRefreshCycleDurationGOOGLE) -> IO VkResult)
getPastPresentationTimingGOOGLE :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult)
getPastPresentationTimingGOOGLE deviceCmds = mkVkGetPastPresentationTimingGOOGLE (pVkGetPastPresentationTimingGOOGLE deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPastPresentationTimingGOOGLE
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> ("pPresentationTimingCount" ::: Ptr Word32) -> ("pPresentationTimings" ::: Ptr VkPastPresentationTimingGOOGLE) -> IO VkResult)
cmdSetViewportWScalingNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ())
cmdSetViewportWScalingNV deviceCmds = mkVkCmdSetViewportWScalingNV (pVkCmdSetViewportWScalingNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportWScalingNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pViewportWScalings" ::: Ptr VkViewportWScalingNV) -> IO ())
cmdSetDiscardRectangleEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr VkRect2D) -> IO ())
cmdSetDiscardRectangleEXT deviceCmds = mkVkCmdSetDiscardRectangleEXT (pVkCmdSetDiscardRectangleEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetDiscardRectangleEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr VkRect2D) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstDiscardRectangle" ::: Word32) -> ("discardRectangleCount" ::: Word32) -> ("pDiscardRectangles" ::: Ptr VkRect2D) -> IO ())
cmdSetSampleLocationsEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ())
cmdSetSampleLocationsEXT deviceCmds = mkVkCmdSetSampleLocationsEXT (pVkCmdSetSampleLocationsEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetSampleLocationsEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSampleLocationsInfo" ::: Ptr VkSampleLocationsInfoEXT) -> IO ())
getBufferMemoryRequirements2 :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
getBufferMemoryRequirements2 deviceCmds = mkVkGetBufferMemoryRequirements2 (pVkGetBufferMemoryRequirements2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferMemoryRequirements2
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
getImageMemoryRequirements2 :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
getImageMemoryRequirements2 deviceCmds = mkVkGetImageMemoryRequirements2 (pVkGetImageMemoryRequirements2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageMemoryRequirements2
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageMemoryRequirementsInfo2) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2) -> IO ())
getImageSparseMemoryRequirements2 :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ())
getImageSparseMemoryRequirements2 deviceCmds = mkVkGetImageSparseMemoryRequirements2 (pVkGetImageSparseMemoryRequirements2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageSparseMemoryRequirements2
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageSparseMemoryRequirementsInfo2) -> ("pSparseMemoryRequirementCount" ::: Ptr Word32) -> ("pSparseMemoryRequirements" ::: Ptr VkSparseImageMemoryRequirements2) -> IO ())
createSamplerYcbcrConversion :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult)
createSamplerYcbcrConversion deviceCmds = mkVkCreateSamplerYcbcrConversion (pVkCreateSamplerYcbcrConversion deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateSamplerYcbcrConversion
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkSamplerYcbcrConversionCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pYcbcrConversion" ::: Ptr VkSamplerYcbcrConversion) -> IO VkResult)
destroySamplerYcbcrConversion :: DeviceCmds -> (("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroySamplerYcbcrConversion deviceCmds = mkVkDestroySamplerYcbcrConversion (pVkDestroySamplerYcbcrConversion deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySamplerYcbcrConversion
  :: FunPtr (("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("ycbcrConversion" ::: VkSamplerYcbcrConversion) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getDeviceQueue2 :: DeviceCmds -> (("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
getDeviceQueue2 deviceCmds = mkVkGetDeviceQueue2 (pVkGetDeviceQueue2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue2
  :: FunPtr (("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()) -> (("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
createValidationCacheEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
createValidationCacheEXT deviceCmds = mkVkCreateValidationCacheEXT (pVkCreateValidationCacheEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateValidationCacheEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkValidationCacheCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pValidationCache" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
destroyValidationCacheEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyValidationCacheEXT deviceCmds = mkVkDestroyValidationCacheEXT (pVkDestroyValidationCacheEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyValidationCacheEXT
  :: FunPtr (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getValidationCacheDataEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
getValidationCacheDataEXT deviceCmds = mkVkGetValidationCacheDataEXT (pVkGetValidationCacheDataEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetValidationCacheDataEXT
  :: FunPtr (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("validationCache" ::: VkValidationCacheEXT) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
mergeValidationCachesEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
mergeValidationCachesEXT deviceCmds = mkVkMergeValidationCachesEXT (pVkMergeValidationCachesEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkMergeValidationCachesEXT
  :: FunPtr (("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("dstCache" ::: VkValidationCacheEXT) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkValidationCacheEXT) -> IO VkResult)
getDescriptorSetLayoutSupport :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ())
getDescriptorSetLayoutSupport deviceCmds = mkVkGetDescriptorSetLayoutSupport (pVkGetDescriptorSetLayoutSupport deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDescriptorSetLayoutSupport
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ()) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkDescriptorSetLayoutCreateInfo) -> ("pSupport" ::: Ptr VkDescriptorSetLayoutSupport) -> IO ())
getShaderInfoAMD :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult)
getShaderInfoAMD deviceCmds = mkVkGetShaderInfoAMD (pVkGetShaderInfoAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderInfoAMD
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult)
setLocalDimmingAMD :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ())
setLocalDimmingAMD deviceCmds = mkVkSetLocalDimmingAMD (pVkSetLocalDimmingAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetLocalDimmingAMD
  :: FunPtr (("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ()) -> (("device" ::: VkDevice) -> ("swapChain" ::: VkSwapchainKHR) -> ("localDimmingEnable" ::: VkBool32) -> IO ())
getCalibratedTimestampsEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult)
getCalibratedTimestampsEXT deviceCmds = mkVkGetCalibratedTimestampsEXT (pVkGetCalibratedTimestampsEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetCalibratedTimestampsEXT
  :: FunPtr (("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult) -> (("device" ::: VkDevice) -> ("timestampCount" ::: Word32) -> ("pTimestampInfos" ::: Ptr VkCalibratedTimestampInfoEXT) -> ("pTimestamps" ::: Ptr Word64) -> ("pMaxDeviation" ::: Ptr Word64) -> IO VkResult)
setDebugUtilsObjectNameEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult)
setDebugUtilsObjectNameEXT deviceCmds = mkVkSetDebugUtilsObjectNameEXT (pVkSetDebugUtilsObjectNameEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDebugUtilsObjectNameEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pNameInfo" ::: Ptr VkDebugUtilsObjectNameInfoEXT) -> IO VkResult)
setDebugUtilsObjectTagEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult)
setDebugUtilsObjectTagEXT deviceCmds = mkVkSetDebugUtilsObjectTagEXT (pVkSetDebugUtilsObjectTagEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSetDebugUtilsObjectTagEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pTagInfo" ::: Ptr VkDebugUtilsObjectTagInfoEXT) -> IO VkResult)
queueBeginDebugUtilsLabelEXT :: DeviceCmds -> (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
queueBeginDebugUtilsLabelEXT deviceCmds = mkVkQueueBeginDebugUtilsLabelEXT (pVkQueueBeginDebugUtilsLabelEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueBeginDebugUtilsLabelEXT
  :: FunPtr (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()) -> (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
queueEndDebugUtilsLabelEXT :: DeviceCmds -> (("queue" ::: VkQueue) -> IO ())
queueEndDebugUtilsLabelEXT deviceCmds = mkVkQueueEndDebugUtilsLabelEXT (pVkQueueEndDebugUtilsLabelEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueEndDebugUtilsLabelEXT
  :: FunPtr (("queue" ::: VkQueue) -> IO ()) -> (("queue" ::: VkQueue) -> IO ())
queueInsertDebugUtilsLabelEXT :: DeviceCmds -> (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
queueInsertDebugUtilsLabelEXT deviceCmds = mkVkQueueInsertDebugUtilsLabelEXT (pVkQueueInsertDebugUtilsLabelEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueInsertDebugUtilsLabelEXT
  :: FunPtr (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()) -> (("queue" ::: VkQueue) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
cmdBeginDebugUtilsLabelEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
cmdBeginDebugUtilsLabelEXT deviceCmds = mkVkCmdBeginDebugUtilsLabelEXT (pVkCmdBeginDebugUtilsLabelEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginDebugUtilsLabelEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
cmdEndDebugUtilsLabelEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdEndDebugUtilsLabelEXT deviceCmds = mkVkCmdEndDebugUtilsLabelEXT (pVkCmdEndDebugUtilsLabelEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndDebugUtilsLabelEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> IO ())
cmdInsertDebugUtilsLabelEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
cmdInsertDebugUtilsLabelEXT deviceCmds = mkVkCmdInsertDebugUtilsLabelEXT (pVkCmdInsertDebugUtilsLabelEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdInsertDebugUtilsLabelEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pLabelInfo" ::: Ptr VkDebugUtilsLabelEXT) -> IO ())
getMemoryHostPointerPropertiesEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult)
getMemoryHostPointerPropertiesEXT deviceCmds = mkVkGetMemoryHostPointerPropertiesEXT (pVkGetMemoryHostPointerPropertiesEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryHostPointerPropertiesEXT
  :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("pHostPointer" ::: Ptr ()) -> ("pMemoryHostPointerProperties" ::: Ptr VkMemoryHostPointerPropertiesEXT) -> IO VkResult)
cmdWriteBufferMarkerAMD :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ())
cmdWriteBufferMarkerAMD deviceCmds = mkVkCmdWriteBufferMarkerAMD (pVkCmdWriteBufferMarkerAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteBufferMarkerAMD
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pipelineStage" ::: VkPipelineStageFlagBits) -> ("dstBuffer" ::: VkBuffer) -> ("dstOffset" ::: VkDeviceSize) -> ("marker" ::: Word32) -> IO ())
createRenderPass2KHR :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
createRenderPass2KHR deviceCmds = mkVkCreateRenderPass2KHR (pVkCreateRenderPass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRenderPass2KHR
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkRenderPassCreateInfo2KHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pRenderPass" ::: Ptr VkRenderPass) -> IO VkResult)
cmdBeginRenderPass2KHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ())
cmdBeginRenderPass2KHR deviceCmds = mkVkCmdBeginRenderPass2KHR (pVkCmdBeginRenderPass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginRenderPass2KHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pRenderPassBegin" ::: Ptr VkRenderPassBeginInfo) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> IO ())
cmdNextSubpass2KHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
cmdNextSubpass2KHR deviceCmds = mkVkCmdNextSubpass2KHR (pVkCmdNextSubpass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdNextSubpass2KHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassBeginInfo" ::: Ptr VkSubpassBeginInfoKHR) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
cmdEndRenderPass2KHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
cmdEndRenderPass2KHR deviceCmds = mkVkCmdEndRenderPass2KHR (pVkCmdEndRenderPass2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndRenderPass2KHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pSubpassEndInfo" ::: Ptr VkSubpassEndInfoKHR) -> IO ())
#if VK_USE_PLATFORM_ANDROID_KHR
getAndroidHardwareBufferPropertiesANDROID :: DeviceCmds -> (("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult)
getAndroidHardwareBufferPropertiesANDROID deviceCmds = mkVkGetAndroidHardwareBufferPropertiesANDROID (pVkGetAndroidHardwareBufferPropertiesANDROID deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAndroidHardwareBufferPropertiesANDROID
  :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult) -> (("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult)
getMemoryAndroidHardwareBufferANDROID :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult)
getMemoryAndroidHardwareBufferANDROID deviceCmds = mkVkGetMemoryAndroidHardwareBufferANDROID (pVkGetMemoryAndroidHardwareBufferANDROID deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetMemoryAndroidHardwareBufferANDROID
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult)
#endif
cmdDrawIndirectCountKHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndirectCountKHR deviceCmds = mkVkCmdDrawIndirectCountKHR (pVkCmdDrawIndirectCountKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectCountKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndexedIndirectCountKHR :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawIndexedIndirectCountKHR deviceCmds = mkVkCmdDrawIndexedIndirectCountKHR (pVkCmdDrawIndexedIndirectCountKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndexedIndirectCountKHR
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdSetCheckpointNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ())
cmdSetCheckpointNV deviceCmds = mkVkCmdSetCheckpointNV (pVkCmdSetCheckpointNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCheckpointNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pCheckpointMarker" ::: Ptr ()) -> IO ())
getQueueCheckpointDataNV :: DeviceCmds -> (("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ())
getQueueCheckpointDataNV deviceCmds = mkVkGetQueueCheckpointDataNV (pVkGetQueueCheckpointDataNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueueCheckpointDataNV
  :: FunPtr (("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ()) -> (("queue" ::: VkQueue) -> ("pCheckpointDataCount" ::: Ptr Word32) -> ("pCheckpointData" ::: Ptr VkCheckpointDataNV) -> IO ())
cmdBindTransformFeedbackBuffersEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ())
cmdBindTransformFeedbackBuffersEXT deviceCmds = mkVkCmdBindTransformFeedbackBuffersEXT (pVkCmdBindTransformFeedbackBuffersEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindTransformFeedbackBuffersEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstBinding" ::: Word32) -> ("bindingCount" ::: Word32) -> ("pBuffers" ::: Ptr VkBuffer) -> ("pOffsets" ::: Ptr VkDeviceSize) -> ("pSizes" ::: Ptr VkDeviceSize) -> IO ())
cmdBeginTransformFeedbackEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
cmdBeginTransformFeedbackEXT deviceCmds = mkVkCmdBeginTransformFeedbackEXT (pVkCmdBeginTransformFeedbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginTransformFeedbackEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
cmdEndTransformFeedbackEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
cmdEndTransformFeedbackEXT deviceCmds = mkVkCmdEndTransformFeedbackEXT (pVkCmdEndTransformFeedbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndTransformFeedbackEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstCounterBuffer" ::: Word32) -> ("counterBufferCount" ::: Word32) -> ("pCounterBuffers" ::: Ptr VkBuffer) -> ("pCounterBufferOffsets" ::: Ptr VkDeviceSize) -> IO ())
cmdBeginQueryIndexedEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ())
cmdBeginQueryIndexedEXT deviceCmds = mkVkCmdBeginQueryIndexedEXT (pVkCmdBeginQueryIndexedEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBeginQueryIndexedEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("flags" ::: VkQueryControlFlags) -> ("index" ::: Word32) -> IO ())
cmdEndQueryIndexedEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ())
cmdEndQueryIndexedEXT deviceCmds = mkVkCmdEndQueryIndexedEXT (pVkCmdEndQueryIndexedEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdEndQueryIndexedEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("queryPool" ::: VkQueryPool) -> ("query" ::: Word32) -> ("index" ::: Word32) -> IO ())
cmdDrawIndirectByteCountEXT :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ())
cmdDrawIndirectByteCountEXT deviceCmds = mkVkCmdDrawIndirectByteCountEXT (pVkCmdDrawIndirectByteCountEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawIndirectByteCountEXT
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("instanceCount" ::: Word32) -> ("firstInstance" ::: Word32) -> ("counterBuffer" ::: VkBuffer) -> ("counterBufferOffset" ::: VkDeviceSize) -> ("counterOffset" ::: Word32) -> ("vertexStride" ::: Word32) -> IO ())
cmdSetExclusiveScissorNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ())
cmdSetExclusiveScissorNV deviceCmds = mkVkCmdSetExclusiveScissorNV (pVkCmdSetExclusiveScissorNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetExclusiveScissorNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstExclusiveScissor" ::: Word32) -> ("exclusiveScissorCount" ::: Word32) -> ("pExclusiveScissors" ::: Ptr VkRect2D) -> IO ())
cmdBindShadingRateImageNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ())
cmdBindShadingRateImageNV deviceCmds = mkVkCmdBindShadingRateImageNV (pVkCmdBindShadingRateImageNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBindShadingRateImageNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("imageView" ::: VkImageView) -> ("imageLayout" ::: VkImageLayout) -> IO ())
cmdSetViewportShadingRatePaletteNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ())
cmdSetViewportShadingRatePaletteNV deviceCmds = mkVkCmdSetViewportShadingRatePaletteNV (pVkCmdSetViewportShadingRatePaletteNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetViewportShadingRatePaletteNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("firstViewport" ::: Word32) -> ("viewportCount" ::: Word32) -> ("pShadingRatePalettes" ::: Ptr VkShadingRatePaletteNV) -> IO ())
cmdSetCoarseSampleOrderNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ())
cmdSetCoarseSampleOrderNV deviceCmds = mkVkCmdSetCoarseSampleOrderNV (pVkCmdSetCoarseSampleOrderNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdSetCoarseSampleOrderNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("sampleOrderType" ::: VkCoarseSampleOrderTypeNV) -> ("customSampleOrderCount" ::: Word32) -> ("pCustomSampleOrders" ::: Ptr VkCoarseSampleOrderCustomNV) -> IO ())
cmdDrawMeshTasksNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ())
cmdDrawMeshTasksNV deviceCmds = mkVkCmdDrawMeshTasksNV (pVkCmdDrawMeshTasksNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("taskCount" ::: Word32) -> ("firstTask" ::: Word32) -> IO ())
cmdDrawMeshTasksIndirectNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawMeshTasksIndirectNV deviceCmds = mkVkCmdDrawMeshTasksIndirectNV (pVkCmdDrawMeshTasksIndirectNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("drawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawMeshTasksIndirectCountNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
cmdDrawMeshTasksIndirectCountNV deviceCmds = mkVkCmdDrawMeshTasksIndirectCountNV (pVkCmdDrawMeshTasksIndirectCountNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdDrawMeshTasksIndirectCountNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("buffer" ::: VkBuffer) -> ("offset" ::: VkDeviceSize) -> ("countBuffer" ::: VkBuffer) -> ("countBufferOffset" ::: VkDeviceSize) -> ("maxDrawCount" ::: Word32) -> ("stride" ::: Word32) -> IO ())
compileDeferredNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult)
compileDeferredNV deviceCmds = mkVkCompileDeferredNV (pVkCompileDeferredNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCompileDeferredNV
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shader" ::: Word32) -> IO VkResult)
createAccelerationStructureNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult)
createAccelerationStructureNV deviceCmds = mkVkCreateAccelerationStructureNV (pVkCreateAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAccelerationStructureNV
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkAccelerationStructureCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pAccelerationStructure" ::: Ptr VkAccelerationStructureNV) -> IO VkResult)
destroyAccelerationStructureNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyAccelerationStructureNV deviceCmds = mkVkDestroyAccelerationStructureNV (pVkDestroyAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyAccelerationStructureNV
  :: FunPtr (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getAccelerationStructureMemoryRequirementsNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ())
getAccelerationStructureMemoryRequirementsNV deviceCmds = mkVkGetAccelerationStructureMemoryRequirementsNV (pVkGetAccelerationStructureMemoryRequirementsNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureMemoryRequirementsNV
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ()) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkAccelerationStructureMemoryRequirementsInfoNV) -> ("pMemoryRequirements" ::: Ptr VkMemoryRequirements2KHR) -> IO ())
bindAccelerationStructureMemoryNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult)
bindAccelerationStructureMemoryNV deviceCmds = mkVkBindAccelerationStructureMemoryNV (pVkBindAccelerationStructureMemoryNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindAccelerationStructureMemoryNV
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindAccelerationStructureMemoryInfoNV) -> IO VkResult)
cmdCopyAccelerationStructureNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ())
cmdCopyAccelerationStructureNV deviceCmds = mkVkCmdCopyAccelerationStructureNV (pVkCmdCopyAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdCopyAccelerationStructureNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("mode" ::: VkCopyAccelerationStructureModeNV) -> IO ())
cmdWriteAccelerationStructuresPropertiesNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ())
cmdWriteAccelerationStructuresPropertiesNV deviceCmds = mkVkCmdWriteAccelerationStructuresPropertiesNV (pVkCmdWriteAccelerationStructuresPropertiesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdWriteAccelerationStructuresPropertiesNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("accelerationStructureCount" ::: Word32) -> ("pAccelerationStructures" ::: Ptr VkAccelerationStructureNV) -> ("queryType" ::: VkQueryType) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> IO ())
cmdBuildAccelerationStructureNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ())
cmdBuildAccelerationStructureNV deviceCmds = mkVkCmdBuildAccelerationStructureNV (pVkCmdBuildAccelerationStructureNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdBuildAccelerationStructureNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("pInfo" ::: Ptr VkAccelerationStructureInfoNV) -> ("instanceData" ::: VkBuffer) -> ("instanceOffset" ::: VkDeviceSize) -> ("update" ::: VkBool32) -> ("dst" ::: VkAccelerationStructureNV) -> ("src" ::: VkAccelerationStructureNV) -> ("scratch" ::: VkBuffer) -> ("scratchOffset" ::: VkDeviceSize) -> IO ())
cmdTraceRaysNV :: DeviceCmds -> (("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ())
cmdTraceRaysNV deviceCmds = mkVkCmdTraceRaysNV (pVkCmdTraceRaysNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCmdTraceRaysNV
  :: FunPtr (("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ()) -> (("commandBuffer" ::: VkCommandBuffer) -> ("raygenShaderBindingTableBuffer" ::: VkBuffer) -> ("raygenShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingTableBuffer" ::: VkBuffer) -> ("missShaderBindingOffset" ::: VkDeviceSize) -> ("missShaderBindingStride" ::: VkDeviceSize) -> ("hitShaderBindingTableBuffer" ::: VkBuffer) -> ("hitShaderBindingOffset" ::: VkDeviceSize) -> ("hitShaderBindingStride" ::: VkDeviceSize) -> ("callableShaderBindingTableBuffer" ::: VkBuffer) -> ("callableShaderBindingOffset" ::: VkDeviceSize) -> ("callableShaderBindingStride" ::: VkDeviceSize) -> ("width" ::: Word32) -> ("height" ::: Word32) -> ("depth" ::: Word32) -> IO ())
getRayTracingShaderGroupHandlesNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
getRayTracingShaderGroupHandlesNV deviceCmds = mkVkGetRayTracingShaderGroupHandlesNV (pVkGetRayTracingShaderGroupHandlesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRayTracingShaderGroupHandlesNV
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("firstGroup" ::: Word32) -> ("groupCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
getAccelerationStructureHandleNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
getAccelerationStructureHandleNV deviceCmds = mkVkGetAccelerationStructureHandleNV (pVkGetAccelerationStructureHandleNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetAccelerationStructureHandleNV
  :: FunPtr (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("accelerationStructure" ::: VkAccelerationStructureNV) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> IO VkResult)
createRayTracingPipelinesNV :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
createRayTracingPipelinesNV deviceCmds = mkVkCreateRayTracingPipelinesNV (pVkCreateRayTracingPipelinesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateRayTracingPipelinesNV
  :: FunPtr (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("createInfoCount" ::: Word32) -> ("pCreateInfos" ::: Ptr VkRayTracingPipelineCreateInfoNV) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelines" ::: Ptr VkPipeline) -> IO VkResult)
getImageDrmFormatModifierPropertiesEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult)
getImageDrmFormatModifierPropertiesEXT deviceCmds = mkVkGetImageDrmFormatModifierPropertiesEXT (pVkGetImageDrmFormatModifierPropertiesEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageDrmFormatModifierPropertiesEXT
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("pProperties" ::: Ptr VkImageDrmFormatModifierPropertiesEXT) -> IO VkResult)
getBufferDeviceAddressEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress)
getBufferDeviceAddressEXT deviceCmds = mkVkGetBufferDeviceAddressEXT (pVkGetBufferDeviceAddressEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetBufferDeviceAddressEXT
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress)
getImageViewHandleNVX :: DeviceCmds -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32)
getImageViewHandleNVX deviceCmds = mkVkGetImageViewHandleNVX (pVkGetImageViewHandleNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetImageViewHandleNVX
  :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32) -> (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkImageViewHandleInfoNVX) -> IO Word32)
getDeviceGroupSurfacePresentModes2EXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
getDeviceGroupSurfacePresentModes2EXT deviceCmds = mkVkGetDeviceGroupSurfacePresentModes2EXT (pVkGetDeviceGroupSurfacePresentModes2EXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceGroupSurfacePresentModes2EXT
  :: FunPtr (("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pModes" ::: Ptr VkDeviceGroupPresentModeFlagsKHR) -> IO VkResult)
#if VK_USE_PLATFORM_WIN32_KHR
acquireFullScreenExclusiveModeEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
acquireFullScreenExclusiveModeEXT deviceCmds = mkVkAcquireFullScreenExclusiveModeEXT (pVkAcquireFullScreenExclusiveModeEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireFullScreenExclusiveModeEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
releaseFullScreenExclusiveModeEXT :: DeviceCmds -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
releaseFullScreenExclusiveModeEXT deviceCmds = mkVkReleaseFullScreenExclusiveModeEXT (pVkReleaseFullScreenExclusiveModeEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseFullScreenExclusiveModeEXT
  :: FunPtr (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult) -> (("device" ::: VkDevice) -> ("swapchain" ::: VkSwapchainKHR) -> IO VkResult)
#endif

-- * Instance commands
destroyInstance :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyInstance deviceCmds = mkVkDestroyInstance (pVkDestroyInstance deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyInstance
  :: FunPtr (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
enumeratePhysicalDevices :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult)
enumeratePhysicalDevices deviceCmds = mkVkEnumeratePhysicalDevices (pVkEnumeratePhysicalDevices deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDevices
  :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceCount" ::: Ptr Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> IO VkResult)
getInstanceProcAddr :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
getInstanceProcAddr deviceCmds = mkVkGetInstanceProcAddr (pVkGetInstanceProcAddr deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetInstanceProcAddr
  :: FunPtr (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction) -> (("instance" ::: VkInstance) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
getPhysicalDeviceProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ())
getPhysicalDeviceProperties deviceCmds = mkVkGetPhysicalDeviceProperties (pVkGetPhysicalDeviceProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties) -> IO ())
getPhysicalDeviceQueueFamilyProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ())
getPhysicalDeviceQueueFamilyProperties deviceCmds = mkVkGetPhysicalDeviceQueueFamilyProperties (pVkGetPhysicalDeviceQueueFamilyProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties) -> IO ())
getPhysicalDeviceMemoryProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ())
getPhysicalDeviceMemoryProperties deviceCmds = mkVkGetPhysicalDeviceMemoryProperties (pVkGetPhysicalDeviceMemoryProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties) -> IO ())
getPhysicalDeviceFeatures :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ())
getPhysicalDeviceFeatures deviceCmds = mkVkGetPhysicalDeviceFeatures (pVkGetPhysicalDeviceFeatures deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures) -> IO ())
getPhysicalDeviceFormatProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ())
getPhysicalDeviceFormatProperties deviceCmds = mkVkGetPhysicalDeviceFormatProperties (pVkGetPhysicalDeviceFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties) -> IO ())
getPhysicalDeviceImageFormatProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult)
getPhysicalDeviceImageFormatProperties deviceCmds = mkVkGetPhysicalDeviceImageFormatProperties (pVkGetPhysicalDeviceImageFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties) -> IO VkResult)
createDevice :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult)
createDevice deviceCmds = mkVkCreateDevice (pVkCreateDevice deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDevice
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pCreateInfo" ::: Ptr VkDeviceCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pDevice" ::: Ptr VkDevice) -> IO VkResult)
enumerateDeviceLayerProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)
enumerateDeviceLayerProperties deviceCmds = mkVkEnumerateDeviceLayerProperties (pVkEnumerateDeviceLayerProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceLayerProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkLayerProperties) -> IO VkResult)
enumerateDeviceExtensionProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)
enumerateDeviceExtensionProperties deviceCmds = mkVkEnumerateDeviceExtensionProperties (pVkEnumerateDeviceExtensionProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumerateDeviceExtensionProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pLayerName" ::: Ptr CChar) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkExtensionProperties) -> IO VkResult)
getPhysicalDeviceSparseImageFormatProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ())
getPhysicalDeviceSparseImageFormatProperties deviceCmds = mkVkGetPhysicalDeviceSparseImageFormatProperties (pVkGetPhysicalDeviceSparseImageFormatProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSparseImageFormatProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("samples" ::: VkSampleCountFlagBits) -> ("usage" ::: VkImageUsageFlags) -> ("tiling" ::: VkImageTiling) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties) -> IO ())
#if VK_USE_PLATFORM_ANDROID_KHR
createAndroidSurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createAndroidSurfaceKHR deviceCmds = mkVkCreateAndroidSurfaceKHR (pVkCreateAndroidSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateAndroidSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
getPhysicalDeviceDisplayPropertiesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult)
getPhysicalDeviceDisplayPropertiesKHR deviceCmds = mkVkGetPhysicalDeviceDisplayPropertiesKHR (pVkGetPhysicalDeviceDisplayPropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPropertiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult)
getPhysicalDeviceDisplayPlanePropertiesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult)
getPhysicalDeviceDisplayPlanePropertiesKHR deviceCmds = mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR (pVkGetPhysicalDeviceDisplayPlanePropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlanePropertiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult)
getDisplayPlaneSupportedDisplaysKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult)
getDisplayPlaneSupportedDisplaysKHR deviceCmds = mkVkGetDisplayPlaneSupportedDisplaysKHR (pVkGetDisplayPlaneSupportedDisplaysKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneSupportedDisplaysKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult)
getDisplayModePropertiesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult)
getDisplayModePropertiesKHR deviceCmds = mkVkGetDisplayModePropertiesKHR (pVkGetDisplayModePropertiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModePropertiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult)
createDisplayModeKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult)
createDisplayModeKHR deviceCmds = mkVkCreateDisplayModeKHR (pVkCreateDisplayModeKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayModeKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult)
getDisplayPlaneCapabilitiesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult)
getDisplayPlaneCapabilitiesKHR deviceCmds = mkVkGetDisplayPlaneCapabilitiesKHR (pVkGetDisplayPlaneCapabilitiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilitiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult)
createDisplayPlaneSurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createDisplayPlaneSurfaceKHR deviceCmds = mkVkCreateDisplayPlaneSurfaceKHR (pVkCreateDisplayPlaneSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDisplayPlaneSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
destroySurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroySurfaceKHR deviceCmds = mkVkDestroySurfaceKHR (pVkDestroySurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroySurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
getPhysicalDeviceSurfaceSupportKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult)
getPhysicalDeviceSurfaceSupportKHR deviceCmds = mkVkGetPhysicalDeviceSurfaceSupportKHR (pVkGetPhysicalDeviceSurfaceSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult)
getPhysicalDeviceSurfaceCapabilitiesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult)
getPhysicalDeviceSurfaceCapabilitiesKHR deviceCmds = mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR (pVkGetPhysicalDeviceSurfaceCapabilitiesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilitiesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult)
getPhysicalDeviceSurfaceFormatsKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult)
getPhysicalDeviceSurfaceFormatsKHR deviceCmds = mkVkGetPhysicalDeviceSurfaceFormatsKHR (pVkGetPhysicalDeviceSurfaceFormatsKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormatsKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult)
getPhysicalDeviceSurfacePresentModesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
getPhysicalDeviceSurfacePresentModesKHR deviceCmds = mkVkGetPhysicalDeviceSurfacePresentModesKHR (pVkGetPhysicalDeviceSurfacePresentModesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#if VK_USE_PLATFORM_VI_NN
createViSurfaceNN :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createViSurfaceNN deviceCmds = mkVkCreateViSurfaceNN (pVkCreateViSurfaceNN deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateViSurfaceNN
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_WAYLAND_KHR
createWaylandSurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createWaylandSurfaceKHR deviceCmds = mkVkCreateWaylandSurfaceKHR (pVkCreateWaylandSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWaylandSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
getPhysicalDeviceWaylandPresentationSupportKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32)
getPhysicalDeviceWaylandPresentationSupportKHR deviceCmds = mkVkGetPhysicalDeviceWaylandPresentationSupportKHR (pVkGetPhysicalDeviceWaylandPresentationSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceWaylandPresentationSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_WIN32_KHR
createWin32SurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createWin32SurfaceKHR deviceCmds = mkVkCreateWin32SurfaceKHR (pVkCreateWin32SurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateWin32SurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
getPhysicalDeviceWin32PresentationSupportKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32)
getPhysicalDeviceWin32PresentationSupportKHR deviceCmds = mkVkGetPhysicalDeviceWin32PresentationSupportKHR (pVkGetPhysicalDeviceWin32PresentationSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceWin32PresentationSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_XLIB_KHR
createXlibSurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createXlibSurfaceKHR deviceCmds = mkVkCreateXlibSurfaceKHR (pVkCreateXlibSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXlibSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
getPhysicalDeviceXlibPresentationSupportKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32)
getPhysicalDeviceXlibPresentationSupportKHR deviceCmds = mkVkGetPhysicalDeviceXlibPresentationSupportKHR (pVkGetPhysicalDeviceXlibPresentationSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceXlibPresentationSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_XCB_KHR
createXcbSurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createXcbSurfaceKHR deviceCmds = mkVkCreateXcbSurfaceKHR (pVkCreateXcbSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateXcbSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
getPhysicalDeviceXcbPresentationSupportKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32)
getPhysicalDeviceXcbPresentationSupportKHR deviceCmds = mkVkGetPhysicalDeviceXcbPresentationSupportKHR (pVkGetPhysicalDeviceXcbPresentationSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceXcbPresentationSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32)
#endif
#if VK_USE_PLATFORM_FUCHSIA
createImagePipeSurfaceFUCHSIA :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createImagePipeSurfaceFUCHSIA deviceCmds = mkVkCreateImagePipeSurfaceFUCHSIA (pVkCreateImagePipeSurfaceFUCHSIA deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateImagePipeSurfaceFUCHSIA
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_GGP
createStreamDescriptorSurfaceGGP :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createStreamDescriptorSurfaceGGP deviceCmds = mkVkCreateStreamDescriptorSurfaceGGP (pVkCreateStreamDescriptorSurfaceGGP deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateStreamDescriptorSurfaceGGP
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkStreamDescriptorSurfaceCreateInfoGGP) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
createDebugReportCallbackEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult)
createDebugReportCallbackEXT deviceCmds = mkVkCreateDebugReportCallbackEXT (pVkCreateDebugReportCallbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugReportCallbackEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugReportCallbackCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pCallback" ::: Ptr VkDebugReportCallbackEXT) -> IO VkResult)
destroyDebugReportCallbackEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyDebugReportCallbackEXT deviceCmds = mkVkDestroyDebugReportCallbackEXT (pVkDestroyDebugReportCallbackEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugReportCallbackEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("callback" ::: VkDebugReportCallbackEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
debugReportMessageEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ())
debugReportMessageEXT deviceCmds = mkVkDebugReportMessageEXT (pVkDebugReportMessageEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDebugReportMessageEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ()) -> (("instance" ::: VkInstance) -> ("flags" ::: VkDebugReportFlagsEXT) -> ("objectType" ::: VkDebugReportObjectTypeEXT) -> ("object" ::: Word64) -> ("location" ::: CSize) -> ("messageCode" ::: Int32) -> ("pLayerPrefix" ::: Ptr CChar) -> ("pMessage" ::: Ptr CChar) -> IO ())
getPhysicalDeviceExternalImageFormatPropertiesNV :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult)
getPhysicalDeviceExternalImageFormatPropertiesNV deviceCmds = mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV (pVkGetPhysicalDeviceExternalImageFormatPropertiesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalImageFormatPropertiesNV
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("type" ::: VkImageType) -> ("tiling" ::: VkImageTiling) -> ("usage" ::: VkImageUsageFlags) -> ("flags" ::: VkImageCreateFlags) -> ("externalHandleType" ::: VkExternalMemoryHandleTypeFlagsNV) -> ("pExternalImageFormatProperties" ::: Ptr VkExternalImageFormatPropertiesNV) -> IO VkResult)
getPhysicalDeviceGeneratedCommandsPropertiesNVX :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ())
getPhysicalDeviceGeneratedCommandsPropertiesNVX deviceCmds = mkVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX (pVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkDeviceGeneratedCommandsFeaturesNVX) -> ("pLimits" ::: Ptr VkDeviceGeneratedCommandsLimitsNVX) -> IO ())
getPhysicalDeviceFeatures2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ())
getPhysicalDeviceFeatures2 deviceCmds = mkVkGetPhysicalDeviceFeatures2 (pVkGetPhysicalDeviceFeatures2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFeatures2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFeatures" ::: Ptr VkPhysicalDeviceFeatures2) -> IO ())
getPhysicalDeviceProperties2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ())
getPhysicalDeviceProperties2 deviceCmds = mkVkGetPhysicalDeviceProperties2 (pVkGetPhysicalDeviceProperties2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceProperties2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pProperties" ::: Ptr VkPhysicalDeviceProperties2) -> IO ())
getPhysicalDeviceFormatProperties2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ())
getPhysicalDeviceFormatProperties2 deviceCmds = mkVkGetPhysicalDeviceFormatProperties2 (pVkGetPhysicalDeviceFormatProperties2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceFormatProperties2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("format" ::: VkFormat) -> ("pFormatProperties" ::: Ptr VkFormatProperties2) -> IO ())
getPhysicalDeviceImageFormatProperties2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult)
getPhysicalDeviceImageFormatProperties2 deviceCmds = mkVkGetPhysicalDeviceImageFormatProperties2 (pVkGetPhysicalDeviceImageFormatProperties2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceImageFormatProperties2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pImageFormatInfo" ::: Ptr VkPhysicalDeviceImageFormatInfo2) -> ("pImageFormatProperties" ::: Ptr VkImageFormatProperties2) -> IO VkResult)
getPhysicalDeviceQueueFamilyProperties2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ())
getPhysicalDeviceQueueFamilyProperties2 deviceCmds = mkVkGetPhysicalDeviceQueueFamilyProperties2 (pVkGetPhysicalDeviceQueueFamilyProperties2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceQueueFamilyProperties2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pQueueFamilyPropertyCount" ::: Ptr Word32) -> ("pQueueFamilyProperties" ::: Ptr VkQueueFamilyProperties2) -> IO ())
getPhysicalDeviceMemoryProperties2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ())
getPhysicalDeviceMemoryProperties2 deviceCmds = mkVkGetPhysicalDeviceMemoryProperties2 (pVkGetPhysicalDeviceMemoryProperties2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMemoryProperties2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pMemoryProperties" ::: Ptr VkPhysicalDeviceMemoryProperties2) -> IO ())
getPhysicalDeviceSparseImageFormatProperties2 :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ())
getPhysicalDeviceSparseImageFormatProperties2 deviceCmds = mkVkGetPhysicalDeviceSparseImageFormatProperties2 (pVkGetPhysicalDeviceSparseImageFormatProperties2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSparseImageFormatProperties2
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pFormatInfo" ::: Ptr VkPhysicalDeviceSparseImageFormatInfo2) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkSparseImageFormatProperties2) -> IO ())
getPhysicalDeviceExternalBufferProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ())
getPhysicalDeviceExternalBufferProperties deviceCmds = mkVkGetPhysicalDeviceExternalBufferProperties (pVkGetPhysicalDeviceExternalBufferProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalBufferProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalBufferInfo" ::: Ptr VkPhysicalDeviceExternalBufferInfo) -> ("pExternalBufferProperties" ::: Ptr VkExternalBufferProperties) -> IO ())
getPhysicalDeviceExternalSemaphoreProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ())
getPhysicalDeviceExternalSemaphoreProperties deviceCmds = mkVkGetPhysicalDeviceExternalSemaphoreProperties (pVkGetPhysicalDeviceExternalSemaphoreProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalSemaphoreProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalSemaphoreInfo" ::: Ptr VkPhysicalDeviceExternalSemaphoreInfo) -> ("pExternalSemaphoreProperties" ::: Ptr VkExternalSemaphoreProperties) -> IO ())
getPhysicalDeviceExternalFenceProperties :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ())
getPhysicalDeviceExternalFenceProperties deviceCmds = mkVkGetPhysicalDeviceExternalFenceProperties (pVkGetPhysicalDeviceExternalFenceProperties deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceExternalFenceProperties
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pExternalFenceInfo" ::: Ptr VkPhysicalDeviceExternalFenceInfo) -> ("pExternalFenceProperties" ::: Ptr VkExternalFenceProperties) -> IO ())
releaseDisplayEXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
releaseDisplayEXT deviceCmds = mkVkReleaseDisplayEXT (pVkReleaseDisplayEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkReleaseDisplayEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
#if VK_USE_PLATFORM_XLIB_XRANDR_EXT
acquireXlibDisplayEXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
acquireXlibDisplayEXT deviceCmds = mkVkAcquireXlibDisplayEXT (pVkAcquireXlibDisplayEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireXlibDisplayEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
getRandROutputDisplayEXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult)
getRandROutputDisplayEXT deviceCmds = mkVkGetRandROutputDisplayEXT (pVkGetRandROutputDisplayEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetRandROutputDisplayEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult)
#endif
getPhysicalDeviceSurfaceCapabilities2EXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult)
getPhysicalDeviceSurfaceCapabilities2EXT deviceCmds = mkVkGetPhysicalDeviceSurfaceCapabilities2EXT (pVkGetPhysicalDeviceSurfaceCapabilities2EXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2EXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult)
enumeratePhysicalDeviceGroups :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult)
enumeratePhysicalDeviceGroups deviceCmds = mkVkEnumeratePhysicalDeviceGroups (pVkEnumeratePhysicalDeviceGroups deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceGroups
  :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult)
getPhysicalDevicePresentRectanglesKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult)
getPhysicalDevicePresentRectanglesKHR deviceCmds = mkVkGetPhysicalDevicePresentRectanglesKHR (pVkGetPhysicalDevicePresentRectanglesKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDevicePresentRectanglesKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult)
#if VK_USE_PLATFORM_IOS_MVK
createIOSSurfaceMVK :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createIOSSurfaceMVK deviceCmds = mkVkCreateIOSSurfaceMVK (pVkCreateIOSSurfaceMVK deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIOSSurfaceMVK
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_MACOS_MVK
createMacOSSurfaceMVK :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createMacOSSurfaceMVK deviceCmds = mkVkCreateMacOSSurfaceMVK (pVkCreateMacOSSurfaceMVK deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMacOSSurfaceMVK
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if VK_USE_PLATFORM_METAL_EXT
createMetalSurfaceEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createMetalSurfaceEXT deviceCmds = mkVkCreateMetalSurfaceEXT (pVkCreateMetalSurfaceEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMetalSurfaceEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMetalSurfaceCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
getPhysicalDeviceMultisamplePropertiesEXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ())
getPhysicalDeviceMultisamplePropertiesEXT deviceCmds = mkVkGetPhysicalDeviceMultisamplePropertiesEXT (pVkGetPhysicalDeviceMultisamplePropertiesEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMultisamplePropertiesEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ()) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ())
getPhysicalDeviceSurfaceCapabilities2KHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2KHR) -> IO VkResult)
getPhysicalDeviceSurfaceCapabilities2KHR deviceCmds = mkVkGetPhysicalDeviceSurfaceCapabilities2KHR (pVkGetPhysicalDeviceSurfaceCapabilities2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceCapabilities2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2KHR) -> IO VkResult)
getPhysicalDeviceSurfaceFormats2KHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormat2KHR) -> IO VkResult)
getPhysicalDeviceSurfaceFormats2KHR deviceCmds = mkVkGetPhysicalDeviceSurfaceFormats2KHR (pVkGetPhysicalDeviceSurfaceFormats2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfaceFormats2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormat2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormat2KHR) -> IO VkResult)
getPhysicalDeviceDisplayProperties2KHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult)
getPhysicalDeviceDisplayProperties2KHR deviceCmds = mkVkGetPhysicalDeviceDisplayProperties2KHR (pVkGetPhysicalDeviceDisplayProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayProperties2KHR) -> IO VkResult)
getPhysicalDeviceDisplayPlaneProperties2KHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult)
getPhysicalDeviceDisplayPlaneProperties2KHR deviceCmds = mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR (pVkGetPhysicalDeviceDisplayPlaneProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceDisplayPlaneProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlaneProperties2KHR) -> IO VkResult)
getDisplayModeProperties2KHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult)
getDisplayModeProperties2KHR deviceCmds = mkVkGetDisplayModeProperties2KHR (pVkGetDisplayModeProperties2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayModeProperties2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModeProperties2KHR) -> IO VkResult)
getDisplayPlaneCapabilities2KHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult)
getDisplayPlaneCapabilities2KHR deviceCmds = mkVkGetDisplayPlaneCapabilities2KHR (pVkGetDisplayPlaneCapabilities2KHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDisplayPlaneCapabilities2KHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pDisplayPlaneInfo" ::: Ptr VkDisplayPlaneInfo2KHR) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilities2KHR) -> IO VkResult)
getPhysicalDeviceCalibrateableTimeDomainsEXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult)
getPhysicalDeviceCalibrateableTimeDomainsEXT deviceCmds = mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT (pVkGetPhysicalDeviceCalibrateableTimeDomainsEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCalibrateableTimeDomainsEXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pTimeDomainCount" ::: Ptr Word32) -> ("pTimeDomains" ::: Ptr VkTimeDomainEXT) -> IO VkResult)
createDebugUtilsMessengerEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult)
createDebugUtilsMessengerEXT deviceCmds = mkVkCreateDebugUtilsMessengerEXT (pVkCreateDebugUtilsMessengerEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateDebugUtilsMessengerEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult)
destroyDebugUtilsMessengerEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
destroyDebugUtilsMessengerEXT deviceCmds = mkVkDestroyDebugUtilsMessengerEXT (pVkDestroyDebugUtilsMessengerEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyDebugUtilsMessengerEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
submitDebugUtilsMessageEXT :: InstanceCmds -> (("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ())
submitDebugUtilsMessageEXT deviceCmds = mkVkSubmitDebugUtilsMessageEXT (pVkSubmitDebugUtilsMessageEXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkSubmitDebugUtilsMessageEXT
  :: FunPtr (("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ()) -> (("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ())
getPhysicalDeviceCooperativeMatrixPropertiesNV :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult)
getPhysicalDeviceCooperativeMatrixPropertiesNV deviceCmds = mkVkGetPhysicalDeviceCooperativeMatrixPropertiesNV (pVkGetPhysicalDeviceCooperativeMatrixPropertiesNV deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceCooperativeMatrixPropertiesNV
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkCooperativeMatrixPropertiesNV) -> IO VkResult)
#if VK_USE_PLATFORM_WIN32_KHR
getPhysicalDeviceSurfacePresentModes2EXT :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
getPhysicalDeviceSurfacePresentModes2EXT deviceCmds = mkVkGetPhysicalDeviceSurfacePresentModes2EXT (pVkGetPhysicalDeviceSurfacePresentModes2EXT deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceSurfacePresentModes2EXT
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#endif
