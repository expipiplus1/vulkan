{-# language Strict #-}
{-# language CPP #-}
{-# language CPP #-}
{-# language ForeignFunctionInterface #-}
{-# language MagicHash #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Dynamic
  ( DeviceCmds(..)
  , InstanceCmds(..)
  , initDeviceCmds
  , initInstanceCmds
  , destroyInstance
  , hasDestroyInstance
  , enumeratePhysicalDevices
  , hasEnumeratePhysicalDevices
  , getDeviceProcAddr
  , hasGetDeviceProcAddr
  , getInstanceProcAddr
  , hasGetInstanceProcAddr
  , getPhysicalDeviceProperties
  , hasGetPhysicalDeviceProperties
  , getPhysicalDeviceQueueFamilyProperties
  , hasGetPhysicalDeviceQueueFamilyProperties
  , getPhysicalDeviceMemoryProperties
  , hasGetPhysicalDeviceMemoryProperties
  , getPhysicalDeviceFeatures
  , hasGetPhysicalDeviceFeatures
  , getPhysicalDeviceFormatProperties
  , hasGetPhysicalDeviceFormatProperties
  , getPhysicalDeviceImageFormatProperties
  , hasGetPhysicalDeviceImageFormatProperties
  , createDevice
  , hasCreateDevice
  , destroyDevice
  , hasDestroyDevice
  , enumerateDeviceLayerProperties
  , hasEnumerateDeviceLayerProperties
  , enumerateDeviceExtensionProperties
  , hasEnumerateDeviceExtensionProperties
  , getDeviceQueue
  , hasGetDeviceQueue
  , queueSubmit
  , hasQueueSubmit
  , queueWaitIdle
  , hasQueueWaitIdle
  , deviceWaitIdle
  , hasDeviceWaitIdle
  , allocateMemory
  , hasAllocateMemory
  , freeMemory
  , hasFreeMemory
  , mapMemory
  , hasMapMemory
  , unmapMemory
  , hasUnmapMemory
  , flushMappedMemoryRanges
  , hasFlushMappedMemoryRanges
  , invalidateMappedMemoryRanges
  , hasInvalidateMappedMemoryRanges
  , getDeviceMemoryCommitment
  , hasGetDeviceMemoryCommitment
  , getBufferMemoryRequirements
  , hasGetBufferMemoryRequirements
  , bindBufferMemory
  , hasBindBufferMemory
  , getImageMemoryRequirements
  , hasGetImageMemoryRequirements
  , bindImageMemory
  , hasBindImageMemory
  , getImageSparseMemoryRequirements
  , hasGetImageSparseMemoryRequirements
  , getPhysicalDeviceSparseImageFormatProperties
  , hasGetPhysicalDeviceSparseImageFormatProperties
  , queueBindSparse
  , hasQueueBindSparse
  , createFence
  , hasCreateFence
  , destroyFence
  , hasDestroyFence
  , resetFences
  , hasResetFences
  , getFenceStatus
  , hasGetFenceStatus
  , waitForFences
  , hasWaitForFences
  , createSemaphore
  , hasCreateSemaphore
  , destroySemaphore
  , hasDestroySemaphore
  , createEvent
  , hasCreateEvent
  , destroyEvent
  , hasDestroyEvent
  , getEventStatus
  , hasGetEventStatus
  , setEvent
  , hasSetEvent
  , resetEvent
  , hasResetEvent
  , createQueryPool
  , hasCreateQueryPool
  , destroyQueryPool
  , hasDestroyQueryPool
  , getQueryPoolResults
  , hasGetQueryPoolResults
  , createBuffer
  , hasCreateBuffer
  , destroyBuffer
  , hasDestroyBuffer
  , createBufferView
  , hasCreateBufferView
  , destroyBufferView
  , hasDestroyBufferView
  , createImage
  , hasCreateImage
  , destroyImage
  , hasDestroyImage
  , getImageSubresourceLayout
  , hasGetImageSubresourceLayout
  , createImageView
  , hasCreateImageView
  , destroyImageView
  , hasDestroyImageView
  , createShaderModule
  , hasCreateShaderModule
  , destroyShaderModule
  , hasDestroyShaderModule
  , createPipelineCache
  , hasCreatePipelineCache
  , destroyPipelineCache
  , hasDestroyPipelineCache
  , getPipelineCacheData
  , hasGetPipelineCacheData
  , mergePipelineCaches
  , hasMergePipelineCaches
  , createGraphicsPipelines
  , hasCreateGraphicsPipelines
  , createComputePipelines
  , hasCreateComputePipelines
  , destroyPipeline
  , hasDestroyPipeline
  , createPipelineLayout
  , hasCreatePipelineLayout
  , destroyPipelineLayout
  , hasDestroyPipelineLayout
  , createSampler
  , hasCreateSampler
  , destroySampler
  , hasDestroySampler
  , createDescriptorSetLayout
  , hasCreateDescriptorSetLayout
  , destroyDescriptorSetLayout
  , hasDestroyDescriptorSetLayout
  , createDescriptorPool
  , hasCreateDescriptorPool
  , destroyDescriptorPool
  , hasDestroyDescriptorPool
  , resetDescriptorPool
  , hasResetDescriptorPool
  , allocateDescriptorSets
  , hasAllocateDescriptorSets
  , freeDescriptorSets
  , hasFreeDescriptorSets
  , updateDescriptorSets
  , hasUpdateDescriptorSets
  , createFramebuffer
  , hasCreateFramebuffer
  , destroyFramebuffer
  , hasDestroyFramebuffer
  , createRenderPass
  , hasCreateRenderPass
  , destroyRenderPass
  , hasDestroyRenderPass
  , getRenderAreaGranularity
  , hasGetRenderAreaGranularity
  , createCommandPool
  , hasCreateCommandPool
  , destroyCommandPool
  , hasDestroyCommandPool
  , resetCommandPool
  , hasResetCommandPool
  , allocateCommandBuffers
  , hasAllocateCommandBuffers
  , freeCommandBuffers
  , hasFreeCommandBuffers
  , beginCommandBuffer
  , hasBeginCommandBuffer
  , endCommandBuffer
  , hasEndCommandBuffer
  , resetCommandBuffer
  , hasResetCommandBuffer
  , cmdBindPipeline
  , hasCmdBindPipeline
  , cmdSetViewport
  , hasCmdSetViewport
  , cmdSetScissor
  , hasCmdSetScissor
  , cmdSetLineWidth
  , hasCmdSetLineWidth
  , cmdSetDepthBias
  , hasCmdSetDepthBias
  , cmdSetBlendConstants
  , hasCmdSetBlendConstants
  , cmdSetDepthBounds
  , hasCmdSetDepthBounds
  , cmdSetStencilCompareMask
  , hasCmdSetStencilCompareMask
  , cmdSetStencilWriteMask
  , hasCmdSetStencilWriteMask
  , cmdSetStencilReference
  , hasCmdSetStencilReference
  , cmdBindDescriptorSets
  , hasCmdBindDescriptorSets
  , cmdBindIndexBuffer
  , hasCmdBindIndexBuffer
  , cmdBindVertexBuffers
  , hasCmdBindVertexBuffers
  , cmdDraw
  , hasCmdDraw
  , cmdDrawIndexed
  , hasCmdDrawIndexed
  , cmdDrawIndirect
  , hasCmdDrawIndirect
  , cmdDrawIndexedIndirect
  , hasCmdDrawIndexedIndirect
  , cmdDispatch
  , hasCmdDispatch
  , cmdDispatchIndirect
  , hasCmdDispatchIndirect
  , cmdCopyBuffer
  , hasCmdCopyBuffer
  , cmdCopyImage
  , hasCmdCopyImage
  , cmdBlitImage
  , hasCmdBlitImage
  , cmdCopyBufferToImage
  , hasCmdCopyBufferToImage
  , cmdCopyImageToBuffer
  , hasCmdCopyImageToBuffer
  , cmdUpdateBuffer
  , hasCmdUpdateBuffer
  , cmdFillBuffer
  , hasCmdFillBuffer
  , cmdClearColorImage
  , hasCmdClearColorImage
  , cmdClearDepthStencilImage
  , hasCmdClearDepthStencilImage
  , cmdClearAttachments
  , hasCmdClearAttachments
  , cmdResolveImage
  , hasCmdResolveImage
  , cmdSetEvent
  , hasCmdSetEvent
  , cmdResetEvent
  , hasCmdResetEvent
  , cmdWaitEvents
  , hasCmdWaitEvents
  , cmdPipelineBarrier
  , hasCmdPipelineBarrier
  , cmdBeginQuery
  , hasCmdBeginQuery
  , cmdEndQuery
  , hasCmdEndQuery
  , cmdResetQueryPool
  , hasCmdResetQueryPool
  , cmdWriteTimestamp
  , hasCmdWriteTimestamp
  , cmdCopyQueryPoolResults
  , hasCmdCopyQueryPoolResults
  , cmdPushConstants
  , hasCmdPushConstants
  , cmdBeginRenderPass
  , hasCmdBeginRenderPass
  , cmdNextSubpass
  , hasCmdNextSubpass
  , cmdEndRenderPass
  , hasCmdEndRenderPass
  , cmdExecuteCommands
  , hasCmdExecuteCommands
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , createAndroidSurfaceKHR
  , hasCreateAndroidSurfaceKHR
#endif
  , getPhysicalDeviceDisplayPropertiesKHR
  , hasGetPhysicalDeviceDisplayPropertiesKHR
  , getPhysicalDeviceDisplayPlanePropertiesKHR
  , hasGetPhysicalDeviceDisplayPlanePropertiesKHR
  , getDisplayPlaneSupportedDisplaysKHR
  , hasGetDisplayPlaneSupportedDisplaysKHR
  , getDisplayModePropertiesKHR
  , hasGetDisplayModePropertiesKHR
  , createDisplayModeKHR
  , hasCreateDisplayModeKHR
  , getDisplayPlaneCapabilitiesKHR
  , hasGetDisplayPlaneCapabilitiesKHR
  , createDisplayPlaneSurfaceKHR
  , hasCreateDisplayPlaneSurfaceKHR
  , createSharedSwapchainsKHR
  , hasCreateSharedSwapchainsKHR
#if defined(VK_USE_PLATFORM_MIR_KHR)
  , createMirSurfaceKHR
  , hasCreateMirSurfaceKHR
  , getPhysicalDeviceMirPresentationSupportKHR
  , hasGetPhysicalDeviceMirPresentationSupportKHR
#endif
  , destroySurfaceKHR
  , hasDestroySurfaceKHR
  , getPhysicalDeviceSurfaceSupportKHR
  , hasGetPhysicalDeviceSurfaceSupportKHR
  , getPhysicalDeviceSurfaceCapabilitiesKHR
  , hasGetPhysicalDeviceSurfaceCapabilitiesKHR
  , getPhysicalDeviceSurfaceFormatsKHR
  , hasGetPhysicalDeviceSurfaceFormatsKHR
  , getPhysicalDeviceSurfacePresentModesKHR
  , hasGetPhysicalDeviceSurfacePresentModesKHR
  , createSwapchainKHR
  , hasCreateSwapchainKHR
  , destroySwapchainKHR
  , hasDestroySwapchainKHR
  , getSwapchainImagesKHR
  , hasGetSwapchainImagesKHR
  , acquireNextImageKHR
  , hasAcquireNextImageKHR
  , queuePresentKHR
  , hasQueuePresentKHR
#if defined(VK_USE_PLATFORM_VI_NN)
  , createViSurfaceNN
  , hasCreateViSurfaceNN
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
  , createWaylandSurfaceKHR
  , hasCreateWaylandSurfaceKHR
  , getPhysicalDeviceWaylandPresentationSupportKHR
  , hasGetPhysicalDeviceWaylandPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , createWin32SurfaceKHR
  , hasCreateWin32SurfaceKHR
  , getPhysicalDeviceWin32PresentationSupportKHR
  , hasGetPhysicalDeviceWin32PresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
  , createXlibSurfaceKHR
  , hasCreateXlibSurfaceKHR
  , getPhysicalDeviceXlibPresentationSupportKHR
  , hasGetPhysicalDeviceXlibPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
  , createXcbSurfaceKHR
  , hasCreateXcbSurfaceKHR
  , getPhysicalDeviceXcbPresentationSupportKHR
  , hasGetPhysicalDeviceXcbPresentationSupportKHR
#endif
  , createDebugReportCallbackEXT
  , hasCreateDebugReportCallbackEXT
  , destroyDebugReportCallbackEXT
  , hasDestroyDebugReportCallbackEXT
  , debugReportMessageEXT
  , hasDebugReportMessageEXT
  , debugMarkerSetObjectNameEXT
  , hasDebugMarkerSetObjectNameEXT
  , debugMarkerSetObjectTagEXT
  , hasDebugMarkerSetObjectTagEXT
  , cmdDebugMarkerBeginEXT
  , hasCmdDebugMarkerBeginEXT
  , cmdDebugMarkerEndEXT
  , hasCmdDebugMarkerEndEXT
  , cmdDebugMarkerInsertEXT
  , hasCmdDebugMarkerInsertEXT
  , getPhysicalDeviceExternalImageFormatPropertiesNV
  , hasGetPhysicalDeviceExternalImageFormatPropertiesNV
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getMemoryWin32HandleNV
  , hasGetMemoryWin32HandleNV
#endif
  , cmdDrawIndirectCountAMD
  , hasCmdDrawIndirectCountAMD
  , cmdDrawIndexedIndirectCountAMD
  , hasCmdDrawIndexedIndirectCountAMD
  , cmdProcessCommandsNVX
  , hasCmdProcessCommandsNVX
  , cmdReserveSpaceForCommandsNVX
  , hasCmdReserveSpaceForCommandsNVX
  , createIndirectCommandsLayoutNVX
  , hasCreateIndirectCommandsLayoutNVX
  , destroyIndirectCommandsLayoutNVX
  , hasDestroyIndirectCommandsLayoutNVX
  , createObjectTableNVX
  , hasCreateObjectTableNVX
  , destroyObjectTableNVX
  , hasDestroyObjectTableNVX
  , registerObjectsNVX
  , hasRegisterObjectsNVX
  , unregisterObjectsNVX
  , hasUnregisterObjectsNVX
  , getPhysicalDeviceGeneratedCommandsPropertiesNVX
  , hasGetPhysicalDeviceGeneratedCommandsPropertiesNVX
  , getPhysicalDeviceFeatures2
  , hasGetPhysicalDeviceFeatures2
  , getPhysicalDeviceProperties2
  , hasGetPhysicalDeviceProperties2
  , getPhysicalDeviceFormatProperties2
  , hasGetPhysicalDeviceFormatProperties2
  , getPhysicalDeviceImageFormatProperties2
  , hasGetPhysicalDeviceImageFormatProperties2
  , getPhysicalDeviceQueueFamilyProperties2
  , hasGetPhysicalDeviceQueueFamilyProperties2
  , getPhysicalDeviceMemoryProperties2
  , hasGetPhysicalDeviceMemoryProperties2
  , getPhysicalDeviceSparseImageFormatProperties2
  , hasGetPhysicalDeviceSparseImageFormatProperties2
  , cmdPushDescriptorSetKHR
  , hasCmdPushDescriptorSetKHR
  , trimCommandPool
  , hasTrimCommandPool
  , getPhysicalDeviceExternalBufferProperties
  , hasGetPhysicalDeviceExternalBufferProperties
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getMemoryWin32HandleKHR
  , hasGetMemoryWin32HandleKHR
  , getMemoryWin32HandlePropertiesKHR
  , hasGetMemoryWin32HandlePropertiesKHR
#endif
  , getMemoryFdKHR
  , hasGetMemoryFdKHR
  , getMemoryFdPropertiesKHR
  , hasGetMemoryFdPropertiesKHR
  , getPhysicalDeviceExternalSemaphoreProperties
  , hasGetPhysicalDeviceExternalSemaphoreProperties
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getSemaphoreWin32HandleKHR
  , hasGetSemaphoreWin32HandleKHR
  , importSemaphoreWin32HandleKHR
  , hasImportSemaphoreWin32HandleKHR
#endif
  , getSemaphoreFdKHR
  , hasGetSemaphoreFdKHR
  , importSemaphoreFdKHR
  , hasImportSemaphoreFdKHR
  , getPhysicalDeviceExternalFenceProperties
  , hasGetPhysicalDeviceExternalFenceProperties
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  , getFenceWin32HandleKHR
  , hasGetFenceWin32HandleKHR
  , importFenceWin32HandleKHR
  , hasImportFenceWin32HandleKHR
#endif
  , getFenceFdKHR
  , hasGetFenceFdKHR
  , importFenceFdKHR
  , hasImportFenceFdKHR
  , releaseDisplayEXT
  , hasReleaseDisplayEXT
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
  , acquireXlibDisplayEXT
  , hasAcquireXlibDisplayEXT
  , getRandROutputDisplayEXT
  , hasGetRandROutputDisplayEXT
#endif
  , displayPowerControlEXT
  , hasDisplayPowerControlEXT
  , registerDeviceEventEXT
  , hasRegisterDeviceEventEXT
  , registerDisplayEventEXT
  , hasRegisterDisplayEventEXT
  , getSwapchainCounterEXT
  , hasGetSwapchainCounterEXT
  , getPhysicalDeviceSurfaceCapabilities2EXT
  , hasGetPhysicalDeviceSurfaceCapabilities2EXT
  , enumeratePhysicalDeviceGroups
  , hasEnumeratePhysicalDeviceGroups
  , getDeviceGroupPeerMemoryFeatures
  , hasGetDeviceGroupPeerMemoryFeatures
  , bindBufferMemory2
  , hasBindBufferMemory2
  , bindImageMemory2
  , hasBindImageMemory2
  , cmdSetDeviceMask
  , hasCmdSetDeviceMask
  , getDeviceGroupPresentCapabilitiesKHR
  , hasGetDeviceGroupPresentCapabilitiesKHR
  , getDeviceGroupSurfacePresentModesKHR
  , hasGetDeviceGroupSurfacePresentModesKHR
  , acquireNextImage2KHR
  , hasAcquireNextImage2KHR
  , cmdDispatchBase
  , hasCmdDispatchBase
  , getPhysicalDevicePresentRectanglesKHR
  , hasGetPhysicalDevicePresentRectanglesKHR
  , createDescriptorUpdateTemplate
  , hasCreateDescriptorUpdateTemplate
  , destroyDescriptorUpdateTemplate
  , hasDestroyDescriptorUpdateTemplate
  , updateDescriptorSetWithTemplate
  , hasUpdateDescriptorSetWithTemplate
  , cmdPushDescriptorSetWithTemplateKHR
  , hasCmdPushDescriptorSetWithTemplateKHR
  , setHdrMetadataEXT
  , hasSetHdrMetadataEXT
  , getSwapchainStatusKHR
  , hasGetSwapchainStatusKHR
  , getRefreshCycleDurationGOOGLE
  , hasGetRefreshCycleDurationGOOGLE
  , getPastPresentationTimingGOOGLE
  , hasGetPastPresentationTimingGOOGLE
#if defined(VK_USE_PLATFORM_IOS_MVK)
  , createIOSSurfaceMVK
  , hasCreateIOSSurfaceMVK
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
  , createMacOSSurfaceMVK
  , hasCreateMacOSSurfaceMVK
#endif
  , cmdSetViewportWScalingNV
  , hasCmdSetViewportWScalingNV
  , cmdSetDiscardRectangleEXT
  , hasCmdSetDiscardRectangleEXT
  , cmdSetSampleLocationsEXT
  , hasCmdSetSampleLocationsEXT
  , getPhysicalDeviceMultisamplePropertiesEXT
  , hasGetPhysicalDeviceMultisamplePropertiesEXT
  , getPhysicalDeviceSurfaceCapabilities2KHR
  , hasGetPhysicalDeviceSurfaceCapabilities2KHR
  , getPhysicalDeviceSurfaceFormats2KHR
  , hasGetPhysicalDeviceSurfaceFormats2KHR
  , getBufferMemoryRequirements2
  , hasGetBufferMemoryRequirements2
  , getImageMemoryRequirements2
  , hasGetImageMemoryRequirements2
  , getImageSparseMemoryRequirements2
  , hasGetImageSparseMemoryRequirements2
  , createSamplerYcbcrConversion
  , hasCreateSamplerYcbcrConversion
  , destroySamplerYcbcrConversion
  , hasDestroySamplerYcbcrConversion
  , getDeviceQueue2
  , hasGetDeviceQueue2
  , createValidationCacheEXT
  , hasCreateValidationCacheEXT
  , destroyValidationCacheEXT
  , hasDestroyValidationCacheEXT
  , getValidationCacheDataEXT
  , hasGetValidationCacheDataEXT
  , mergeValidationCachesEXT
  , hasMergeValidationCachesEXT
  , getDescriptorSetLayoutSupport
  , hasGetDescriptorSetLayoutSupport
  , getSwapchainGrallocUsageANDROID
  , hasGetSwapchainGrallocUsageANDROID
  , acquireImageANDROID
  , hasAcquireImageANDROID
  , queueSignalReleaseImageANDROID
  , hasQueueSignalReleaseImageANDROID
  , getShaderInfoAMD
  , hasGetShaderInfoAMD
  , setDebugUtilsObjectNameEXT
  , hasSetDebugUtilsObjectNameEXT
  , setDebugUtilsObjectTagEXT
  , hasSetDebugUtilsObjectTagEXT
  , queueBeginDebugUtilsLabelEXT
  , hasQueueBeginDebugUtilsLabelEXT
  , queueEndDebugUtilsLabelEXT
  , hasQueueEndDebugUtilsLabelEXT
  , queueInsertDebugUtilsLabelEXT
  , hasQueueInsertDebugUtilsLabelEXT
  , cmdBeginDebugUtilsLabelEXT
  , hasCmdBeginDebugUtilsLabelEXT
  , cmdEndDebugUtilsLabelEXT
  , hasCmdEndDebugUtilsLabelEXT
  , cmdInsertDebugUtilsLabelEXT
  , hasCmdInsertDebugUtilsLabelEXT
  , createDebugUtilsMessengerEXT
  , hasCreateDebugUtilsMessengerEXT
  , destroyDebugUtilsMessengerEXT
  , hasDestroyDebugUtilsMessengerEXT
  , submitDebugUtilsMessageEXT
  , hasSubmitDebugUtilsMessageEXT
  , getMemoryHostPointerPropertiesEXT
  , hasGetMemoryHostPointerPropertiesEXT
  , cmdWriteBufferMarkerAMD
  , hasCmdWriteBufferMarkerAMD
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  , getAndroidHardwareBufferPropertiesANDROID
  , hasGetAndroidHardwareBufferPropertiesANDROID
  , getMemoryAndroidHardwareBufferANDROID
  , hasGetMemoryAndroidHardwareBufferANDROID
#endif
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
  , nullFunPtr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import qualified GHC.Ptr
  ( Ptr(..)
  )


import Graphics.Vulkan.Core10.Buffer
  ( VkBufferCreateInfo(..)
  )
import Graphics.Vulkan.Core10.BufferView
  ( VkBufferViewCreateInfo(..)
  , VkBufferView
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferResetFlagBits(..)
  , VkQueryControlFlagBits(..)
  , VkCommandBufferResetFlags
  , VkQueryControlFlags
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
  , VkStencilFaceFlagBits(..)
  , VkSubpassContents(..)
  , VkStencilFaceFlags
  )
import Graphics.Vulkan.Core10.CommandPool
  ( VkCommandPoolCreateInfo(..)
  , VkCommandPoolResetFlagBits(..)
  , VkCommandPool
  , VkCommandPoolResetFlags
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
  )
import Graphics.Vulkan.Core10.Device
  ( VkDeviceCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkFormatProperties(..)
  , VkImageCreateFlagBits(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkImageUsageFlagBits(..)
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
  , vkGetDeviceProcAddr
  , vkGetInstanceProcAddr
  )
import Graphics.Vulkan.Core10.Event
  ( VkEventCreateInfo(..)
  , VkEvent
  )
import Graphics.Vulkan.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
  )
import Graphics.Vulkan.Core10.Fence
  ( VkFenceCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , VkSubresourceLayout(..)
  )
import Graphics.Vulkan.Core10.ImageView
  ( VkImageSubresourceRange(..)
  , VkImageViewCreateInfo(..)
  , VkImageView
  )
import Graphics.Vulkan.Core10.LayerDiscovery
  ( VkLayerProperties(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkMemoryMapFlags(..)
  , VkDeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  )
import Graphics.Vulkan.Core10.Pass
  ( VkDependencyFlagBits(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateInfo(..)
  , VkDependencyFlags
  , VkFramebuffer
  )
import Graphics.Vulkan.Core10.Pipeline
  ( VkComputePipelineCreateInfo(..)
  , VkExtent2D(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkRect2D(..)
  , VkShaderStageFlagBits(..)
  , VkViewport(..)
  , VkPipeline
  , VkPipelineLayout
  , VkRenderPass
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( VkPipelineCacheCreateInfo(..)
  , VkPipelineCache
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( VkPipelineLayoutCreateInfo(..)
  , VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import Graphics.Vulkan.Core10.Query
  ( VkQueryPoolCreateInfo(..)
  , VkQueryResultFlagBits(..)
  , VkQueryPool
  , VkQueryResultFlags
  )
import Graphics.Vulkan.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkSubmitInfo(..)
  , VkCommandBuffer
  , VkFence
  , VkPipelineStageFlags
  , VkQueue
  , VkSemaphore
  )
import Graphics.Vulkan.Core10.QueueSemaphore
  ( VkSemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Sampler
  ( VkFilter(..)
  , VkSamplerCreateInfo(..)
  , VkSampler
  )
import Graphics.Vulkan.Core10.Shader
  ( VkShaderModuleCreateInfo(..)
  , VkShaderModule
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageSubresource(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryRequirements(..)
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplate
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( VkPeerMemoryFeatureFlags
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkPhysicalDeviceGroupProperties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
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
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance1
  ( VkCommandPoolTrimFlags(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversion
  )
import Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( VkShaderInfoTypeAMD(..)
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , AHardwareBuffer
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import Graphics.Vulkan.Extensions.VK_EXT_acquire_xlib_display
  ( RROutput
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportFlagBitsEXT(..)
  , VkDebugReportObjectTypeEXT(..)
  , VkDebugReportCallbackEXT
  , VkDebugReportFlagsEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessageSeverityFlagBitsEXT(..)
  , VkDebugUtilsMessageTypeFlagBitsEXT(..)
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
  , VkDebugUtilsMessageTypeFlagsEXT
  , VkDebugUtilsMessengerEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkDisplayPowerInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagBitsEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( VkMemoryHostPointerPropertiesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( VkMultisamplePropertiesEXT(..)
  , VkSampleLocationsInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( VkValidationCacheCreateInfoEXT(..)
  , VkValidationCacheEXT
  )
import Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  )

#if defined(VK_USE_PLATFORM_MIR_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_mir_surface
  ( VkMirSurfaceCreateInfoKHR(..)
  , MirConnection
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkSurfaceKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateInfoKHR(..)
  , VkDeviceGroupPresentModeFlagsKHR
  , VkSwapchainKHR
  )

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR(..)
  , Wl_display
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_visualid_t
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , VisualID
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkObjectEntryTypeNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkIndirectCommandsLayoutNVX
  , VkObjectTableNVX
  )
import Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( VkViewportWScalingNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryHandleTypeFlagBitsNV(..)
  , VkExternalMemoryHandleTypeFlagsNV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( HANDLE
  )
#endif


data DeviceCmds = DeviceCmds
  { pVkGetDeviceProcAddr :: FunPtr (("device" ::: VkDevice) -> ("pName" ::: Ptr CChar) -> IO PFN_vkVoidFunction)
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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
, pVkGetMemoryWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkMemoryGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
, pVkGetMemoryWin32HandlePropertiesKHR :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("handle" ::: HANDLE) -> ("pMemoryWin32HandleProperties" ::: Ptr VkMemoryWin32HandlePropertiesKHR) -> IO VkResult)
#endif
, pVkGetMemoryFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkMemoryGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
, pVkGetMemoryFdPropertiesKHR :: FunPtr (("device" ::: VkDevice) -> ("handleType" ::: VkExternalMemoryHandleTypeFlagBits) -> ("fd" ::: CInt) -> ("pMemoryFdProperties" ::: Ptr VkMemoryFdPropertiesKHR) -> IO VkResult)
#if defined(VK_USE_PLATFORM_WIN32_KHR)
, pVkGetSemaphoreWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetWin32HandleInfo" ::: Ptr VkSemaphoreGetWin32HandleInfoKHR) -> ("pHandle" ::: Ptr HANDLE) -> IO VkResult)
, pVkImportSemaphoreWin32HandleKHR :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreWin32HandleInfo" ::: Ptr VkImportSemaphoreWin32HandleInfoKHR) -> IO VkResult)
#endif
, pVkGetSemaphoreFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pGetFdInfo" ::: Ptr VkSemaphoreGetFdInfoKHR) -> ("pFd" ::: Ptr CInt) -> IO VkResult)
, pVkImportSemaphoreFdKHR :: FunPtr (("device" ::: VkDevice) -> ("pImportSemaphoreFdInfo" ::: Ptr VkImportSemaphoreFdInfoKHR) -> IO VkResult)
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
, pVkGetSwapchainGrallocUsageANDROID :: FunPtr (("device" ::: VkDevice) -> ("format" ::: VkFormat) -> ("imageUsage" ::: VkImageUsageFlags) -> ("grallocUsage" ::: Ptr CInt) -> IO VkResult)
, pVkAcquireImageANDROID :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("nativeFenceFd" ::: CInt) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> IO VkResult)
, pVkQueueSignalReleaseImageANDROID :: FunPtr (("queue" ::: VkQueue) -> ("waitSemaphoreCount" ::: Word32) -> ("pWaitSemaphores" ::: Ptr VkSemaphore) -> ("image" ::: VkImage) -> ("pNativeFenceFd" ::: Ptr CInt) -> IO VkResult)
, pVkGetShaderInfoAMD :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult)
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
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
, pVkGetAndroidHardwareBufferPropertiesANDROID :: FunPtr (("device" ::: VkDevice) -> ("buffer" ::: Ptr AHardwareBuffer) -> ("pProperties" ::: Ptr VkAndroidHardwareBufferPropertiesANDROID) -> IO VkResult)
, pVkGetMemoryAndroidHardwareBufferANDROID :: FunPtr (("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID) -> ("pBuffer" ::: Ptr (Ptr AHardwareBuffer)) -> IO VkResult)
#endif
  }
  deriving (Show)

data InstanceCmds = InstanceCmds
  { pVkDestroyInstance :: FunPtr (("instance" ::: VkInstance) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
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
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
, pVkCreateAndroidSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkAndroidSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
, pVkGetPhysicalDeviceDisplayPropertiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPropertiesKHR) -> IO VkResult)
, pVkGetPhysicalDeviceDisplayPlanePropertiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayPlanePropertiesKHR) -> IO VkResult)
, pVkGetDisplayPlaneSupportedDisplaysKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("planeIndex" ::: Word32) -> ("pDisplayCount" ::: Ptr Word32) -> ("pDisplays" ::: Ptr VkDisplayKHR) -> IO VkResult)
, pVkGetDisplayModePropertiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pPropertyCount" ::: Ptr Word32) -> ("pProperties" ::: Ptr VkDisplayModePropertiesKHR) -> IO VkResult)
, pVkCreateDisplayModeKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("display" ::: VkDisplayKHR) -> ("pCreateInfo" ::: Ptr VkDisplayModeCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMode" ::: Ptr VkDisplayModeKHR) -> IO VkResult)
, pVkGetDisplayPlaneCapabilitiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("mode" ::: VkDisplayModeKHR) -> ("planeIndex" ::: Word32) -> ("pCapabilities" ::: Ptr VkDisplayPlaneCapabilitiesKHR) -> IO VkResult)
, pVkCreateDisplayPlaneSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDisplaySurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#if defined(VK_USE_PLATFORM_MIR_KHR)
, pVkCreateMirSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
, pVkGetPhysicalDeviceMirPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32)
#endif
, pVkDestroySurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("surface" ::: VkSurfaceKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
, pVkGetPhysicalDeviceSurfaceSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("surface" ::: VkSurfaceKHR) -> ("pSupported" ::: Ptr VkBool32) -> IO VkResult)
, pVkGetPhysicalDeviceSurfaceCapabilitiesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilitiesKHR) -> IO VkResult)
, pVkGetPhysicalDeviceSurfaceFormatsKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormatKHR) -> IO VkResult)
, pVkGetPhysicalDeviceSurfacePresentModesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pPresentModeCount" ::: Ptr Word32) -> ("pPresentModes" ::: Ptr VkPresentModeKHR) -> IO VkResult)
#if defined(VK_USE_PLATFORM_VI_NN)
, pVkCreateViSurfaceNN :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
, pVkCreateWaylandSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWaylandSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
, pVkGetPhysicalDeviceWaylandPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("display" ::: Ptr Wl_display) -> IO VkBool32)
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
, pVkCreateWin32SurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkWin32SurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
, pVkGetPhysicalDeviceWin32PresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> IO VkBool32)
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
, pVkCreateXlibSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXlibSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
, pVkGetPhysicalDeviceXlibPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("dpy" ::: Ptr Display) -> ("visualID" ::: VisualID) -> IO VkBool32)
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
, pVkCreateXcbSurfaceKHR :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkXcbSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
, pVkGetPhysicalDeviceXcbPresentationSupportKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr Xcb_connection_t) -> ("visual_id" ::: Xcb_visualid_t) -> IO VkBool32)
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
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
, pVkAcquireXlibDisplayEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("display" ::: VkDisplayKHR) -> IO VkResult)
, pVkGetRandROutputDisplayEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("dpy" ::: Ptr Display) -> ("rrOutput" ::: RROutput) -> ("pDisplay" ::: Ptr VkDisplayKHR) -> IO VkResult)
#endif
, pVkGetPhysicalDeviceSurfaceCapabilities2EXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2EXT) -> IO VkResult)
, pVkEnumeratePhysicalDeviceGroups :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult)
, pVkGetPhysicalDevicePresentRectanglesKHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("surface" ::: VkSurfaceKHR) -> ("pRectCount" ::: Ptr Word32) -> ("pRects" ::: Ptr VkRect2D) -> IO VkResult)
#if defined(VK_USE_PLATFORM_IOS_MVK)
, pVkCreateIOSSurfaceMVK :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
, pVkCreateMacOSSurfaceMVK :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
, pVkGetPhysicalDeviceMultisamplePropertiesEXT :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("samples" ::: VkSampleCountFlagBits) -> ("pMultisampleProperties" ::: Ptr VkMultisamplePropertiesEXT) -> IO ())
, pVkGetPhysicalDeviceSurfaceCapabilities2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceCapabilities" ::: Ptr VkSurfaceCapabilities2KHR) -> IO VkResult)
, pVkGetPhysicalDeviceSurfaceFormats2KHR :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("pSurfaceInfo" ::: Ptr VkPhysicalDeviceSurfaceInfo2KHR) -> ("pSurfaceFormatCount" ::: Ptr Word32) -> ("pSurfaceFormats" ::: Ptr VkSurfaceFormat2KHR) -> IO VkResult)
, pVkCreateDebugUtilsMessengerEXT :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkDebugUtilsMessengerCreateInfoEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pMessenger" ::: Ptr VkDebugUtilsMessengerEXT) -> IO VkResult)
, pVkDestroyDebugUtilsMessengerEXT :: FunPtr (("instance" ::: VkInstance) -> ("messenger" ::: VkDebugUtilsMessengerEXT) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
, pVkSubmitDebugUtilsMessageEXT :: FunPtr (("instance" ::: VkInstance) -> ("messageSeverity" ::: VkDebugUtilsMessageSeverityFlagBitsEXT) -> ("messageTypes" ::: VkDebugUtilsMessageTypeFlagsEXT) -> ("pCallbackData" ::: Ptr VkDebugUtilsMessengerCallbackDataEXT) -> IO ())
  }
  deriving (Show)

initDeviceCmds :: VkDevice -> IO DeviceCmds
initDeviceCmds handle = DeviceCmds
  <$> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceProcAddr\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDevice\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceQueue\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueSubmit\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueWaitIdle\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDeviceWaitIdle\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkAllocateMemory\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkFreeMemory\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkMapMemory\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkUnmapMemory\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkFlushMappedMemoryRanges\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkInvalidateMappedMemoryRanges\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceMemoryCommitment\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetBufferMemoryRequirements\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkBindBufferMemory\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetImageMemoryRequirements\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkBindImageMemory\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetImageSparseMemoryRequirements\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueBindSparse\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateFence\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyFence\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkResetFences\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetFenceStatus\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkWaitForFences\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateSemaphore\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroySemaphore\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateEvent\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyEvent\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetEventStatus\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkSetEvent\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkResetEvent\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateQueryPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyQueryPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetQueryPoolResults\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateBufferView\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyBufferView\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetImageSubresourceLayout\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateImageView\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyImageView\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateShaderModule\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyShaderModule\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreatePipelineCache\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyPipelineCache\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetPipelineCacheData\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkMergePipelineCaches\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateGraphicsPipelines\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateComputePipelines\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyPipeline\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreatePipelineLayout\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyPipelineLayout\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateSampler\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroySampler\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateDescriptorSetLayout\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDescriptorSetLayout\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateDescriptorPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDescriptorPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkResetDescriptorPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkAllocateDescriptorSets\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkFreeDescriptorSets\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkUpdateDescriptorSets\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateFramebuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyFramebuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateRenderPass\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyRenderPass\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetRenderAreaGranularity\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateCommandPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyCommandPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkResetCommandPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkAllocateCommandBuffers\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkFreeCommandBuffers\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkBeginCommandBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkEndCommandBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkResetCommandBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBindPipeline\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetViewport\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetScissor\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetLineWidth\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetDepthBias\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetBlendConstants\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetDepthBounds\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetStencilCompareMask\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetStencilWriteMask\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetStencilReference\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBindDescriptorSets\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBindIndexBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBindVertexBuffers\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDraw\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDrawIndexed\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDrawIndirect\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDrawIndexedIndirect\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDispatch\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDispatchIndirect\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdCopyBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdCopyImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBlitImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdCopyBufferToImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdCopyImageToBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdUpdateBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdFillBuffer\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdClearColorImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdClearDepthStencilImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdClearAttachments\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdResolveImage\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetEvent\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdResetEvent\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdWaitEvents\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdPipelineBarrier\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBeginQuery\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdEndQuery\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdResetQueryPool\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdWriteTimestamp\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdCopyQueryPoolResults\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdPushConstants\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBeginRenderPass\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdNextSubpass\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdEndRenderPass\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdExecuteCommands\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateSharedSwapchainsKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateSwapchainKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroySwapchainKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetSwapchainImagesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkAcquireNextImageKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueuePresentKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDebugMarkerSetObjectNameEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDebugMarkerSetObjectTagEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDebugMarkerBeginEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDebugMarkerEndEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDebugMarkerInsertEXT\NUL"#))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryWin32HandleNV\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDrawIndirectCountAMD\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDrawIndexedIndirectCountAMD\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdProcessCommandsNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdReserveSpaceForCommandsNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateIndirectCommandsLayoutNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyIndirectCommandsLayoutNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateObjectTableNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyObjectTableNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkRegisterObjectsNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkUnregisterObjectsNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdPushDescriptorSetKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkTrimCommandPool\NUL"#))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryWin32HandleKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryWin32HandlePropertiesKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryFdKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryFdPropertiesKHR\NUL"#))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetSemaphoreWin32HandleKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkImportSemaphoreWin32HandleKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetSemaphoreFdKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkImportSemaphoreFdKHR\NUL"#))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetFenceWin32HandleKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkImportFenceWin32HandleKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetFenceFdKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkImportFenceFdKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDisplayPowerControlEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkRegisterDeviceEventEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkRegisterDisplayEventEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetSwapchainCounterEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceGroupPeerMemoryFeatures\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkBindBufferMemory2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkBindImageMemory2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetDeviceMask\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceGroupPresentCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceGroupSurfacePresentModesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkAcquireNextImage2KHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdDispatchBase\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateDescriptorUpdateTemplate\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDescriptorUpdateTemplate\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkUpdateDescriptorSetWithTemplate\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdPushDescriptorSetWithTemplateKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkSetHdrMetadataEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetSwapchainStatusKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetRefreshCycleDurationGOOGLE\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetPastPresentationTimingGOOGLE\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetViewportWScalingNV\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetDiscardRectangleEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdSetSampleLocationsEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetBufferMemoryRequirements2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetImageMemoryRequirements2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetImageSparseMemoryRequirements2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateSamplerYcbcrConversion\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroySamplerYcbcrConversion\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDeviceQueue2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCreateValidationCacheEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkDestroyValidationCacheEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetValidationCacheDataEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkMergeValidationCachesEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetDescriptorSetLayoutSupport\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetSwapchainGrallocUsageANDROID\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkAcquireImageANDROID\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueSignalReleaseImageANDROID\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetShaderInfoAMD\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkSetDebugUtilsObjectNameEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkSetDebugUtilsObjectTagEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueBeginDebugUtilsLabelEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueEndDebugUtilsLabelEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkQueueInsertDebugUtilsLabelEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdBeginDebugUtilsLabelEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdEndDebugUtilsLabelEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdInsertDebugUtilsLabelEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryHostPointerPropertiesEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkCmdWriteBufferMarkerAMD\NUL"#))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetAndroidHardwareBufferPropertiesANDROID\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetDeviceProcAddr handle (GHC.Ptr.Ptr "vkGetMemoryAndroidHardwareBufferANDROID\NUL"#))
#endif

initInstanceCmds :: VkInstance -> IO InstanceCmds
initInstanceCmds handle = InstanceCmds
  <$> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroyInstance\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumeratePhysicalDevices\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetInstanceProcAddr\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceQueueFamilyProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMemoryProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFeatures\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFormatProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceImageFormatProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDevice\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumerateDeviceLayerProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumerateDeviceExtensionProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSparseImageFormatProperties\NUL"#))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateAndroidSurfaceKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceDisplayPlanePropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayPlaneSupportedDisplaysKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayModePropertiesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDisplayModeKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetDisplayPlaneCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDisplayPlaneSurfaceKHR\NUL"#))
#if defined(VK_USE_PLATFORM_MIR_KHR)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateMirSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMirPresentationSupportKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroySurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceSupportKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilitiesKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceFormatsKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfacePresentModesKHR\NUL"#))
#if defined(VK_USE_PLATFORM_VI_NN)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateViSurfaceNN\NUL"#))
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateWaylandSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceWaylandPresentationSupportKHR\NUL"#))
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateWin32SurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceWin32PresentationSupportKHR\NUL"#))
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateXlibSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceXlibPresentationSupportKHR\NUL"#))
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateXcbSurfaceKHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceXcbPresentationSupportKHR\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDebugReportCallbackEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDebugReportCallbackEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDebugReportMessageEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalImageFormatPropertiesNV\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceGeneratedCommandsPropertiesNVX\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFeatures2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceProperties2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceImageFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceQueueFamilyProperties2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMemoryProperties2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSparseImageFormatProperties2\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalBufferProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalSemaphoreProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceExternalFenceProperties\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkReleaseDisplayEXT\NUL"#))
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkAcquireXlibDisplayEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetRandROutputDisplayEXT\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilities2EXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkEnumeratePhysicalDeviceGroups\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDevicePresentRectanglesKHR\NUL"#))
#if defined(VK_USE_PLATFORM_IOS_MVK)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateIOSSurfaceMVK\NUL"#))
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateMacOSSurfaceMVK\NUL"#))
#endif
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceMultisamplePropertiesEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceCapabilities2KHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkGetPhysicalDeviceSurfaceFormats2KHR\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkCreateDebugUtilsMessengerEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkDestroyDebugUtilsMessengerEXT\NUL"#))
  <*> (castPtrToFunPtr <$> vkGetInstanceProcAddr handle (GHC.Ptr.Ptr "vkSubmitDebugUtilsMessageEXT\NUL"#))

hasGetDeviceProcAddr :: DeviceCmds -> Bool
hasGetDeviceProcAddr = (/= nullFunPtr) . pVkGetDeviceProcAddr
hasDestroyDevice :: DeviceCmds -> Bool
hasDestroyDevice = (/= nullFunPtr) . pVkDestroyDevice
hasGetDeviceQueue :: DeviceCmds -> Bool
hasGetDeviceQueue = (/= nullFunPtr) . pVkGetDeviceQueue
hasQueueSubmit :: DeviceCmds -> Bool
hasQueueSubmit = (/= nullFunPtr) . pVkQueueSubmit
hasQueueWaitIdle :: DeviceCmds -> Bool
hasQueueWaitIdle = (/= nullFunPtr) . pVkQueueWaitIdle
hasDeviceWaitIdle :: DeviceCmds -> Bool
hasDeviceWaitIdle = (/= nullFunPtr) . pVkDeviceWaitIdle
hasAllocateMemory :: DeviceCmds -> Bool
hasAllocateMemory = (/= nullFunPtr) . pVkAllocateMemory
hasFreeMemory :: DeviceCmds -> Bool
hasFreeMemory = (/= nullFunPtr) . pVkFreeMemory
hasMapMemory :: DeviceCmds -> Bool
hasMapMemory = (/= nullFunPtr) . pVkMapMemory
hasUnmapMemory :: DeviceCmds -> Bool
hasUnmapMemory = (/= nullFunPtr) . pVkUnmapMemory
hasFlushMappedMemoryRanges :: DeviceCmds -> Bool
hasFlushMappedMemoryRanges = (/= nullFunPtr) . pVkFlushMappedMemoryRanges
hasInvalidateMappedMemoryRanges :: DeviceCmds -> Bool
hasInvalidateMappedMemoryRanges = (/= nullFunPtr) . pVkInvalidateMappedMemoryRanges
hasGetDeviceMemoryCommitment :: DeviceCmds -> Bool
hasGetDeviceMemoryCommitment = (/= nullFunPtr) . pVkGetDeviceMemoryCommitment
hasGetBufferMemoryRequirements :: DeviceCmds -> Bool
hasGetBufferMemoryRequirements = (/= nullFunPtr) . pVkGetBufferMemoryRequirements
hasBindBufferMemory :: DeviceCmds -> Bool
hasBindBufferMemory = (/= nullFunPtr) . pVkBindBufferMemory
hasGetImageMemoryRequirements :: DeviceCmds -> Bool
hasGetImageMemoryRequirements = (/= nullFunPtr) . pVkGetImageMemoryRequirements
hasBindImageMemory :: DeviceCmds -> Bool
hasBindImageMemory = (/= nullFunPtr) . pVkBindImageMemory
hasGetImageSparseMemoryRequirements :: DeviceCmds -> Bool
hasGetImageSparseMemoryRequirements = (/= nullFunPtr) . pVkGetImageSparseMemoryRequirements
hasQueueBindSparse :: DeviceCmds -> Bool
hasQueueBindSparse = (/= nullFunPtr) . pVkQueueBindSparse
hasCreateFence :: DeviceCmds -> Bool
hasCreateFence = (/= nullFunPtr) . pVkCreateFence
hasDestroyFence :: DeviceCmds -> Bool
hasDestroyFence = (/= nullFunPtr) . pVkDestroyFence
hasResetFences :: DeviceCmds -> Bool
hasResetFences = (/= nullFunPtr) . pVkResetFences
hasGetFenceStatus :: DeviceCmds -> Bool
hasGetFenceStatus = (/= nullFunPtr) . pVkGetFenceStatus
hasWaitForFences :: DeviceCmds -> Bool
hasWaitForFences = (/= nullFunPtr) . pVkWaitForFences
hasCreateSemaphore :: DeviceCmds -> Bool
hasCreateSemaphore = (/= nullFunPtr) . pVkCreateSemaphore
hasDestroySemaphore :: DeviceCmds -> Bool
hasDestroySemaphore = (/= nullFunPtr) . pVkDestroySemaphore
hasCreateEvent :: DeviceCmds -> Bool
hasCreateEvent = (/= nullFunPtr) . pVkCreateEvent
hasDestroyEvent :: DeviceCmds -> Bool
hasDestroyEvent = (/= nullFunPtr) . pVkDestroyEvent
hasGetEventStatus :: DeviceCmds -> Bool
hasGetEventStatus = (/= nullFunPtr) . pVkGetEventStatus
hasSetEvent :: DeviceCmds -> Bool
hasSetEvent = (/= nullFunPtr) . pVkSetEvent
hasResetEvent :: DeviceCmds -> Bool
hasResetEvent = (/= nullFunPtr) . pVkResetEvent
hasCreateQueryPool :: DeviceCmds -> Bool
hasCreateQueryPool = (/= nullFunPtr) . pVkCreateQueryPool
hasDestroyQueryPool :: DeviceCmds -> Bool
hasDestroyQueryPool = (/= nullFunPtr) . pVkDestroyQueryPool
hasGetQueryPoolResults :: DeviceCmds -> Bool
hasGetQueryPoolResults = (/= nullFunPtr) . pVkGetQueryPoolResults
hasCreateBuffer :: DeviceCmds -> Bool
hasCreateBuffer = (/= nullFunPtr) . pVkCreateBuffer
hasDestroyBuffer :: DeviceCmds -> Bool
hasDestroyBuffer = (/= nullFunPtr) . pVkDestroyBuffer
hasCreateBufferView :: DeviceCmds -> Bool
hasCreateBufferView = (/= nullFunPtr) . pVkCreateBufferView
hasDestroyBufferView :: DeviceCmds -> Bool
hasDestroyBufferView = (/= nullFunPtr) . pVkDestroyBufferView
hasCreateImage :: DeviceCmds -> Bool
hasCreateImage = (/= nullFunPtr) . pVkCreateImage
hasDestroyImage :: DeviceCmds -> Bool
hasDestroyImage = (/= nullFunPtr) . pVkDestroyImage
hasGetImageSubresourceLayout :: DeviceCmds -> Bool
hasGetImageSubresourceLayout = (/= nullFunPtr) . pVkGetImageSubresourceLayout
hasCreateImageView :: DeviceCmds -> Bool
hasCreateImageView = (/= nullFunPtr) . pVkCreateImageView
hasDestroyImageView :: DeviceCmds -> Bool
hasDestroyImageView = (/= nullFunPtr) . pVkDestroyImageView
hasCreateShaderModule :: DeviceCmds -> Bool
hasCreateShaderModule = (/= nullFunPtr) . pVkCreateShaderModule
hasDestroyShaderModule :: DeviceCmds -> Bool
hasDestroyShaderModule = (/= nullFunPtr) . pVkDestroyShaderModule
hasCreatePipelineCache :: DeviceCmds -> Bool
hasCreatePipelineCache = (/= nullFunPtr) . pVkCreatePipelineCache
hasDestroyPipelineCache :: DeviceCmds -> Bool
hasDestroyPipelineCache = (/= nullFunPtr) . pVkDestroyPipelineCache
hasGetPipelineCacheData :: DeviceCmds -> Bool
hasGetPipelineCacheData = (/= nullFunPtr) . pVkGetPipelineCacheData
hasMergePipelineCaches :: DeviceCmds -> Bool
hasMergePipelineCaches = (/= nullFunPtr) . pVkMergePipelineCaches
hasCreateGraphicsPipelines :: DeviceCmds -> Bool
hasCreateGraphicsPipelines = (/= nullFunPtr) . pVkCreateGraphicsPipelines
hasCreateComputePipelines :: DeviceCmds -> Bool
hasCreateComputePipelines = (/= nullFunPtr) . pVkCreateComputePipelines
hasDestroyPipeline :: DeviceCmds -> Bool
hasDestroyPipeline = (/= nullFunPtr) . pVkDestroyPipeline
hasCreatePipelineLayout :: DeviceCmds -> Bool
hasCreatePipelineLayout = (/= nullFunPtr) . pVkCreatePipelineLayout
hasDestroyPipelineLayout :: DeviceCmds -> Bool
hasDestroyPipelineLayout = (/= nullFunPtr) . pVkDestroyPipelineLayout
hasCreateSampler :: DeviceCmds -> Bool
hasCreateSampler = (/= nullFunPtr) . pVkCreateSampler
hasDestroySampler :: DeviceCmds -> Bool
hasDestroySampler = (/= nullFunPtr) . pVkDestroySampler
hasCreateDescriptorSetLayout :: DeviceCmds -> Bool
hasCreateDescriptorSetLayout = (/= nullFunPtr) . pVkCreateDescriptorSetLayout
hasDestroyDescriptorSetLayout :: DeviceCmds -> Bool
hasDestroyDescriptorSetLayout = (/= nullFunPtr) . pVkDestroyDescriptorSetLayout
hasCreateDescriptorPool :: DeviceCmds -> Bool
hasCreateDescriptorPool = (/= nullFunPtr) . pVkCreateDescriptorPool
hasDestroyDescriptorPool :: DeviceCmds -> Bool
hasDestroyDescriptorPool = (/= nullFunPtr) . pVkDestroyDescriptorPool
hasResetDescriptorPool :: DeviceCmds -> Bool
hasResetDescriptorPool = (/= nullFunPtr) . pVkResetDescriptorPool
hasAllocateDescriptorSets :: DeviceCmds -> Bool
hasAllocateDescriptorSets = (/= nullFunPtr) . pVkAllocateDescriptorSets
hasFreeDescriptorSets :: DeviceCmds -> Bool
hasFreeDescriptorSets = (/= nullFunPtr) . pVkFreeDescriptorSets
hasUpdateDescriptorSets :: DeviceCmds -> Bool
hasUpdateDescriptorSets = (/= nullFunPtr) . pVkUpdateDescriptorSets
hasCreateFramebuffer :: DeviceCmds -> Bool
hasCreateFramebuffer = (/= nullFunPtr) . pVkCreateFramebuffer
hasDestroyFramebuffer :: DeviceCmds -> Bool
hasDestroyFramebuffer = (/= nullFunPtr) . pVkDestroyFramebuffer
hasCreateRenderPass :: DeviceCmds -> Bool
hasCreateRenderPass = (/= nullFunPtr) . pVkCreateRenderPass
hasDestroyRenderPass :: DeviceCmds -> Bool
hasDestroyRenderPass = (/= nullFunPtr) . pVkDestroyRenderPass
hasGetRenderAreaGranularity :: DeviceCmds -> Bool
hasGetRenderAreaGranularity = (/= nullFunPtr) . pVkGetRenderAreaGranularity
hasCreateCommandPool :: DeviceCmds -> Bool
hasCreateCommandPool = (/= nullFunPtr) . pVkCreateCommandPool
hasDestroyCommandPool :: DeviceCmds -> Bool
hasDestroyCommandPool = (/= nullFunPtr) . pVkDestroyCommandPool
hasResetCommandPool :: DeviceCmds -> Bool
hasResetCommandPool = (/= nullFunPtr) . pVkResetCommandPool
hasAllocateCommandBuffers :: DeviceCmds -> Bool
hasAllocateCommandBuffers = (/= nullFunPtr) . pVkAllocateCommandBuffers
hasFreeCommandBuffers :: DeviceCmds -> Bool
hasFreeCommandBuffers = (/= nullFunPtr) . pVkFreeCommandBuffers
hasBeginCommandBuffer :: DeviceCmds -> Bool
hasBeginCommandBuffer = (/= nullFunPtr) . pVkBeginCommandBuffer
hasEndCommandBuffer :: DeviceCmds -> Bool
hasEndCommandBuffer = (/= nullFunPtr) . pVkEndCommandBuffer
hasResetCommandBuffer :: DeviceCmds -> Bool
hasResetCommandBuffer = (/= nullFunPtr) . pVkResetCommandBuffer
hasCmdBindPipeline :: DeviceCmds -> Bool
hasCmdBindPipeline = (/= nullFunPtr) . pVkCmdBindPipeline
hasCmdSetViewport :: DeviceCmds -> Bool
hasCmdSetViewport = (/= nullFunPtr) . pVkCmdSetViewport
hasCmdSetScissor :: DeviceCmds -> Bool
hasCmdSetScissor = (/= nullFunPtr) . pVkCmdSetScissor
hasCmdSetLineWidth :: DeviceCmds -> Bool
hasCmdSetLineWidth = (/= nullFunPtr) . pVkCmdSetLineWidth
hasCmdSetDepthBias :: DeviceCmds -> Bool
hasCmdSetDepthBias = (/= nullFunPtr) . pVkCmdSetDepthBias
hasCmdSetBlendConstants :: DeviceCmds -> Bool
hasCmdSetBlendConstants = (/= nullFunPtr) . pVkCmdSetBlendConstants
hasCmdSetDepthBounds :: DeviceCmds -> Bool
hasCmdSetDepthBounds = (/= nullFunPtr) . pVkCmdSetDepthBounds
hasCmdSetStencilCompareMask :: DeviceCmds -> Bool
hasCmdSetStencilCompareMask = (/= nullFunPtr) . pVkCmdSetStencilCompareMask
hasCmdSetStencilWriteMask :: DeviceCmds -> Bool
hasCmdSetStencilWriteMask = (/= nullFunPtr) . pVkCmdSetStencilWriteMask
hasCmdSetStencilReference :: DeviceCmds -> Bool
hasCmdSetStencilReference = (/= nullFunPtr) . pVkCmdSetStencilReference
hasCmdBindDescriptorSets :: DeviceCmds -> Bool
hasCmdBindDescriptorSets = (/= nullFunPtr) . pVkCmdBindDescriptorSets
hasCmdBindIndexBuffer :: DeviceCmds -> Bool
hasCmdBindIndexBuffer = (/= nullFunPtr) . pVkCmdBindIndexBuffer
hasCmdBindVertexBuffers :: DeviceCmds -> Bool
hasCmdBindVertexBuffers = (/= nullFunPtr) . pVkCmdBindVertexBuffers
hasCmdDraw :: DeviceCmds -> Bool
hasCmdDraw = (/= nullFunPtr) . pVkCmdDraw
hasCmdDrawIndexed :: DeviceCmds -> Bool
hasCmdDrawIndexed = (/= nullFunPtr) . pVkCmdDrawIndexed
hasCmdDrawIndirect :: DeviceCmds -> Bool
hasCmdDrawIndirect = (/= nullFunPtr) . pVkCmdDrawIndirect
hasCmdDrawIndexedIndirect :: DeviceCmds -> Bool
hasCmdDrawIndexedIndirect = (/= nullFunPtr) . pVkCmdDrawIndexedIndirect
hasCmdDispatch :: DeviceCmds -> Bool
hasCmdDispatch = (/= nullFunPtr) . pVkCmdDispatch
hasCmdDispatchIndirect :: DeviceCmds -> Bool
hasCmdDispatchIndirect = (/= nullFunPtr) . pVkCmdDispatchIndirect
hasCmdCopyBuffer :: DeviceCmds -> Bool
hasCmdCopyBuffer = (/= nullFunPtr) . pVkCmdCopyBuffer
hasCmdCopyImage :: DeviceCmds -> Bool
hasCmdCopyImage = (/= nullFunPtr) . pVkCmdCopyImage
hasCmdBlitImage :: DeviceCmds -> Bool
hasCmdBlitImage = (/= nullFunPtr) . pVkCmdBlitImage
hasCmdCopyBufferToImage :: DeviceCmds -> Bool
hasCmdCopyBufferToImage = (/= nullFunPtr) . pVkCmdCopyBufferToImage
hasCmdCopyImageToBuffer :: DeviceCmds -> Bool
hasCmdCopyImageToBuffer = (/= nullFunPtr) . pVkCmdCopyImageToBuffer
hasCmdUpdateBuffer :: DeviceCmds -> Bool
hasCmdUpdateBuffer = (/= nullFunPtr) . pVkCmdUpdateBuffer
hasCmdFillBuffer :: DeviceCmds -> Bool
hasCmdFillBuffer = (/= nullFunPtr) . pVkCmdFillBuffer
hasCmdClearColorImage :: DeviceCmds -> Bool
hasCmdClearColorImage = (/= nullFunPtr) . pVkCmdClearColorImage
hasCmdClearDepthStencilImage :: DeviceCmds -> Bool
hasCmdClearDepthStencilImage = (/= nullFunPtr) . pVkCmdClearDepthStencilImage
hasCmdClearAttachments :: DeviceCmds -> Bool
hasCmdClearAttachments = (/= nullFunPtr) . pVkCmdClearAttachments
hasCmdResolveImage :: DeviceCmds -> Bool
hasCmdResolveImage = (/= nullFunPtr) . pVkCmdResolveImage
hasCmdSetEvent :: DeviceCmds -> Bool
hasCmdSetEvent = (/= nullFunPtr) . pVkCmdSetEvent
hasCmdResetEvent :: DeviceCmds -> Bool
hasCmdResetEvent = (/= nullFunPtr) . pVkCmdResetEvent
hasCmdWaitEvents :: DeviceCmds -> Bool
hasCmdWaitEvents = (/= nullFunPtr) . pVkCmdWaitEvents
hasCmdPipelineBarrier :: DeviceCmds -> Bool
hasCmdPipelineBarrier = (/= nullFunPtr) . pVkCmdPipelineBarrier
hasCmdBeginQuery :: DeviceCmds -> Bool
hasCmdBeginQuery = (/= nullFunPtr) . pVkCmdBeginQuery
hasCmdEndQuery :: DeviceCmds -> Bool
hasCmdEndQuery = (/= nullFunPtr) . pVkCmdEndQuery
hasCmdResetQueryPool :: DeviceCmds -> Bool
hasCmdResetQueryPool = (/= nullFunPtr) . pVkCmdResetQueryPool
hasCmdWriteTimestamp :: DeviceCmds -> Bool
hasCmdWriteTimestamp = (/= nullFunPtr) . pVkCmdWriteTimestamp
hasCmdCopyQueryPoolResults :: DeviceCmds -> Bool
hasCmdCopyQueryPoolResults = (/= nullFunPtr) . pVkCmdCopyQueryPoolResults
hasCmdPushConstants :: DeviceCmds -> Bool
hasCmdPushConstants = (/= nullFunPtr) . pVkCmdPushConstants
hasCmdBeginRenderPass :: DeviceCmds -> Bool
hasCmdBeginRenderPass = (/= nullFunPtr) . pVkCmdBeginRenderPass
hasCmdNextSubpass :: DeviceCmds -> Bool
hasCmdNextSubpass = (/= nullFunPtr) . pVkCmdNextSubpass
hasCmdEndRenderPass :: DeviceCmds -> Bool
hasCmdEndRenderPass = (/= nullFunPtr) . pVkCmdEndRenderPass
hasCmdExecuteCommands :: DeviceCmds -> Bool
hasCmdExecuteCommands = (/= nullFunPtr) . pVkCmdExecuteCommands
hasCreateSharedSwapchainsKHR :: DeviceCmds -> Bool
hasCreateSharedSwapchainsKHR = (/= nullFunPtr) . pVkCreateSharedSwapchainsKHR
hasCreateSwapchainKHR :: DeviceCmds -> Bool
hasCreateSwapchainKHR = (/= nullFunPtr) . pVkCreateSwapchainKHR
hasDestroySwapchainKHR :: DeviceCmds -> Bool
hasDestroySwapchainKHR = (/= nullFunPtr) . pVkDestroySwapchainKHR
hasGetSwapchainImagesKHR :: DeviceCmds -> Bool
hasGetSwapchainImagesKHR = (/= nullFunPtr) . pVkGetSwapchainImagesKHR
hasAcquireNextImageKHR :: DeviceCmds -> Bool
hasAcquireNextImageKHR = (/= nullFunPtr) . pVkAcquireNextImageKHR
hasQueuePresentKHR :: DeviceCmds -> Bool
hasQueuePresentKHR = (/= nullFunPtr) . pVkQueuePresentKHR
hasDebugMarkerSetObjectNameEXT :: DeviceCmds -> Bool
hasDebugMarkerSetObjectNameEXT = (/= nullFunPtr) . pVkDebugMarkerSetObjectNameEXT
hasDebugMarkerSetObjectTagEXT :: DeviceCmds -> Bool
hasDebugMarkerSetObjectTagEXT = (/= nullFunPtr) . pVkDebugMarkerSetObjectTagEXT
hasCmdDebugMarkerBeginEXT :: DeviceCmds -> Bool
hasCmdDebugMarkerBeginEXT = (/= nullFunPtr) . pVkCmdDebugMarkerBeginEXT
hasCmdDebugMarkerEndEXT :: DeviceCmds -> Bool
hasCmdDebugMarkerEndEXT = (/= nullFunPtr) . pVkCmdDebugMarkerEndEXT
hasCmdDebugMarkerInsertEXT :: DeviceCmds -> Bool
hasCmdDebugMarkerInsertEXT = (/= nullFunPtr) . pVkCmdDebugMarkerInsertEXT
#if defined(VK_USE_PLATFORM_WIN32_KHR)
hasGetMemoryWin32HandleNV :: DeviceCmds -> Bool
hasGetMemoryWin32HandleNV = (/= nullFunPtr) . pVkGetMemoryWin32HandleNV
#endif
hasCmdDrawIndirectCountAMD :: DeviceCmds -> Bool
hasCmdDrawIndirectCountAMD = (/= nullFunPtr) . pVkCmdDrawIndirectCountAMD
hasCmdDrawIndexedIndirectCountAMD :: DeviceCmds -> Bool
hasCmdDrawIndexedIndirectCountAMD = (/= nullFunPtr) . pVkCmdDrawIndexedIndirectCountAMD
hasCmdProcessCommandsNVX :: DeviceCmds -> Bool
hasCmdProcessCommandsNVX = (/= nullFunPtr) . pVkCmdProcessCommandsNVX
hasCmdReserveSpaceForCommandsNVX :: DeviceCmds -> Bool
hasCmdReserveSpaceForCommandsNVX = (/= nullFunPtr) . pVkCmdReserveSpaceForCommandsNVX
hasCreateIndirectCommandsLayoutNVX :: DeviceCmds -> Bool
hasCreateIndirectCommandsLayoutNVX = (/= nullFunPtr) . pVkCreateIndirectCommandsLayoutNVX
hasDestroyIndirectCommandsLayoutNVX :: DeviceCmds -> Bool
hasDestroyIndirectCommandsLayoutNVX = (/= nullFunPtr) . pVkDestroyIndirectCommandsLayoutNVX
hasCreateObjectTableNVX :: DeviceCmds -> Bool
hasCreateObjectTableNVX = (/= nullFunPtr) . pVkCreateObjectTableNVX
hasDestroyObjectTableNVX :: DeviceCmds -> Bool
hasDestroyObjectTableNVX = (/= nullFunPtr) . pVkDestroyObjectTableNVX
hasRegisterObjectsNVX :: DeviceCmds -> Bool
hasRegisterObjectsNVX = (/= nullFunPtr) . pVkRegisterObjectsNVX
hasUnregisterObjectsNVX :: DeviceCmds -> Bool
hasUnregisterObjectsNVX = (/= nullFunPtr) . pVkUnregisterObjectsNVX
hasCmdPushDescriptorSetKHR :: DeviceCmds -> Bool
hasCmdPushDescriptorSetKHR = (/= nullFunPtr) . pVkCmdPushDescriptorSetKHR
hasTrimCommandPool :: DeviceCmds -> Bool
hasTrimCommandPool = (/= nullFunPtr) . pVkTrimCommandPool
#if defined(VK_USE_PLATFORM_WIN32_KHR)
hasGetMemoryWin32HandleKHR :: DeviceCmds -> Bool
hasGetMemoryWin32HandleKHR = (/= nullFunPtr) . pVkGetMemoryWin32HandleKHR
hasGetMemoryWin32HandlePropertiesKHR :: DeviceCmds -> Bool
hasGetMemoryWin32HandlePropertiesKHR = (/= nullFunPtr) . pVkGetMemoryWin32HandlePropertiesKHR
#endif
hasGetMemoryFdKHR :: DeviceCmds -> Bool
hasGetMemoryFdKHR = (/= nullFunPtr) . pVkGetMemoryFdKHR
hasGetMemoryFdPropertiesKHR :: DeviceCmds -> Bool
hasGetMemoryFdPropertiesKHR = (/= nullFunPtr) . pVkGetMemoryFdPropertiesKHR
#if defined(VK_USE_PLATFORM_WIN32_KHR)
hasGetSemaphoreWin32HandleKHR :: DeviceCmds -> Bool
hasGetSemaphoreWin32HandleKHR = (/= nullFunPtr) . pVkGetSemaphoreWin32HandleKHR
hasImportSemaphoreWin32HandleKHR :: DeviceCmds -> Bool
hasImportSemaphoreWin32HandleKHR = (/= nullFunPtr) . pVkImportSemaphoreWin32HandleKHR
#endif
hasGetSemaphoreFdKHR :: DeviceCmds -> Bool
hasGetSemaphoreFdKHR = (/= nullFunPtr) . pVkGetSemaphoreFdKHR
hasImportSemaphoreFdKHR :: DeviceCmds -> Bool
hasImportSemaphoreFdKHR = (/= nullFunPtr) . pVkImportSemaphoreFdKHR
#if defined(VK_USE_PLATFORM_WIN32_KHR)
hasGetFenceWin32HandleKHR :: DeviceCmds -> Bool
hasGetFenceWin32HandleKHR = (/= nullFunPtr) . pVkGetFenceWin32HandleKHR
hasImportFenceWin32HandleKHR :: DeviceCmds -> Bool
hasImportFenceWin32HandleKHR = (/= nullFunPtr) . pVkImportFenceWin32HandleKHR
#endif
hasGetFenceFdKHR :: DeviceCmds -> Bool
hasGetFenceFdKHR = (/= nullFunPtr) . pVkGetFenceFdKHR
hasImportFenceFdKHR :: DeviceCmds -> Bool
hasImportFenceFdKHR = (/= nullFunPtr) . pVkImportFenceFdKHR
hasDisplayPowerControlEXT :: DeviceCmds -> Bool
hasDisplayPowerControlEXT = (/= nullFunPtr) . pVkDisplayPowerControlEXT
hasRegisterDeviceEventEXT :: DeviceCmds -> Bool
hasRegisterDeviceEventEXT = (/= nullFunPtr) . pVkRegisterDeviceEventEXT
hasRegisterDisplayEventEXT :: DeviceCmds -> Bool
hasRegisterDisplayEventEXT = (/= nullFunPtr) . pVkRegisterDisplayEventEXT
hasGetSwapchainCounterEXT :: DeviceCmds -> Bool
hasGetSwapchainCounterEXT = (/= nullFunPtr) . pVkGetSwapchainCounterEXT
hasGetDeviceGroupPeerMemoryFeatures :: DeviceCmds -> Bool
hasGetDeviceGroupPeerMemoryFeatures = (/= nullFunPtr) . pVkGetDeviceGroupPeerMemoryFeatures
hasBindBufferMemory2 :: DeviceCmds -> Bool
hasBindBufferMemory2 = (/= nullFunPtr) . pVkBindBufferMemory2
hasBindImageMemory2 :: DeviceCmds -> Bool
hasBindImageMemory2 = (/= nullFunPtr) . pVkBindImageMemory2
hasCmdSetDeviceMask :: DeviceCmds -> Bool
hasCmdSetDeviceMask = (/= nullFunPtr) . pVkCmdSetDeviceMask
hasGetDeviceGroupPresentCapabilitiesKHR :: DeviceCmds -> Bool
hasGetDeviceGroupPresentCapabilitiesKHR = (/= nullFunPtr) . pVkGetDeviceGroupPresentCapabilitiesKHR
hasGetDeviceGroupSurfacePresentModesKHR :: DeviceCmds -> Bool
hasGetDeviceGroupSurfacePresentModesKHR = (/= nullFunPtr) . pVkGetDeviceGroupSurfacePresentModesKHR
hasAcquireNextImage2KHR :: DeviceCmds -> Bool
hasAcquireNextImage2KHR = (/= nullFunPtr) . pVkAcquireNextImage2KHR
hasCmdDispatchBase :: DeviceCmds -> Bool
hasCmdDispatchBase = (/= nullFunPtr) . pVkCmdDispatchBase
hasCreateDescriptorUpdateTemplate :: DeviceCmds -> Bool
hasCreateDescriptorUpdateTemplate = (/= nullFunPtr) . pVkCreateDescriptorUpdateTemplate
hasDestroyDescriptorUpdateTemplate :: DeviceCmds -> Bool
hasDestroyDescriptorUpdateTemplate = (/= nullFunPtr) . pVkDestroyDescriptorUpdateTemplate
hasUpdateDescriptorSetWithTemplate :: DeviceCmds -> Bool
hasUpdateDescriptorSetWithTemplate = (/= nullFunPtr) . pVkUpdateDescriptorSetWithTemplate
hasCmdPushDescriptorSetWithTemplateKHR :: DeviceCmds -> Bool
hasCmdPushDescriptorSetWithTemplateKHR = (/= nullFunPtr) . pVkCmdPushDescriptorSetWithTemplateKHR
hasSetHdrMetadataEXT :: DeviceCmds -> Bool
hasSetHdrMetadataEXT = (/= nullFunPtr) . pVkSetHdrMetadataEXT
hasGetSwapchainStatusKHR :: DeviceCmds -> Bool
hasGetSwapchainStatusKHR = (/= nullFunPtr) . pVkGetSwapchainStatusKHR
hasGetRefreshCycleDurationGOOGLE :: DeviceCmds -> Bool
hasGetRefreshCycleDurationGOOGLE = (/= nullFunPtr) . pVkGetRefreshCycleDurationGOOGLE
hasGetPastPresentationTimingGOOGLE :: DeviceCmds -> Bool
hasGetPastPresentationTimingGOOGLE = (/= nullFunPtr) . pVkGetPastPresentationTimingGOOGLE
hasCmdSetViewportWScalingNV :: DeviceCmds -> Bool
hasCmdSetViewportWScalingNV = (/= nullFunPtr) . pVkCmdSetViewportWScalingNV
hasCmdSetDiscardRectangleEXT :: DeviceCmds -> Bool
hasCmdSetDiscardRectangleEXT = (/= nullFunPtr) . pVkCmdSetDiscardRectangleEXT
hasCmdSetSampleLocationsEXT :: DeviceCmds -> Bool
hasCmdSetSampleLocationsEXT = (/= nullFunPtr) . pVkCmdSetSampleLocationsEXT
hasGetBufferMemoryRequirements2 :: DeviceCmds -> Bool
hasGetBufferMemoryRequirements2 = (/= nullFunPtr) . pVkGetBufferMemoryRequirements2
hasGetImageMemoryRequirements2 :: DeviceCmds -> Bool
hasGetImageMemoryRequirements2 = (/= nullFunPtr) . pVkGetImageMemoryRequirements2
hasGetImageSparseMemoryRequirements2 :: DeviceCmds -> Bool
hasGetImageSparseMemoryRequirements2 = (/= nullFunPtr) . pVkGetImageSparseMemoryRequirements2
hasCreateSamplerYcbcrConversion :: DeviceCmds -> Bool
hasCreateSamplerYcbcrConversion = (/= nullFunPtr) . pVkCreateSamplerYcbcrConversion
hasDestroySamplerYcbcrConversion :: DeviceCmds -> Bool
hasDestroySamplerYcbcrConversion = (/= nullFunPtr) . pVkDestroySamplerYcbcrConversion
hasGetDeviceQueue2 :: DeviceCmds -> Bool
hasGetDeviceQueue2 = (/= nullFunPtr) . pVkGetDeviceQueue2
hasCreateValidationCacheEXT :: DeviceCmds -> Bool
hasCreateValidationCacheEXT = (/= nullFunPtr) . pVkCreateValidationCacheEXT
hasDestroyValidationCacheEXT :: DeviceCmds -> Bool
hasDestroyValidationCacheEXT = (/= nullFunPtr) . pVkDestroyValidationCacheEXT
hasGetValidationCacheDataEXT :: DeviceCmds -> Bool
hasGetValidationCacheDataEXT = (/= nullFunPtr) . pVkGetValidationCacheDataEXT
hasMergeValidationCachesEXT :: DeviceCmds -> Bool
hasMergeValidationCachesEXT = (/= nullFunPtr) . pVkMergeValidationCachesEXT
hasGetDescriptorSetLayoutSupport :: DeviceCmds -> Bool
hasGetDescriptorSetLayoutSupport = (/= nullFunPtr) . pVkGetDescriptorSetLayoutSupport
hasGetSwapchainGrallocUsageANDROID :: DeviceCmds -> Bool
hasGetSwapchainGrallocUsageANDROID = (/= nullFunPtr) . pVkGetSwapchainGrallocUsageANDROID
hasAcquireImageANDROID :: DeviceCmds -> Bool
hasAcquireImageANDROID = (/= nullFunPtr) . pVkAcquireImageANDROID
hasQueueSignalReleaseImageANDROID :: DeviceCmds -> Bool
hasQueueSignalReleaseImageANDROID = (/= nullFunPtr) . pVkQueueSignalReleaseImageANDROID
hasGetShaderInfoAMD :: DeviceCmds -> Bool
hasGetShaderInfoAMD = (/= nullFunPtr) . pVkGetShaderInfoAMD
hasSetDebugUtilsObjectNameEXT :: DeviceCmds -> Bool
hasSetDebugUtilsObjectNameEXT = (/= nullFunPtr) . pVkSetDebugUtilsObjectNameEXT
hasSetDebugUtilsObjectTagEXT :: DeviceCmds -> Bool
hasSetDebugUtilsObjectTagEXT = (/= nullFunPtr) . pVkSetDebugUtilsObjectTagEXT
hasQueueBeginDebugUtilsLabelEXT :: DeviceCmds -> Bool
hasQueueBeginDebugUtilsLabelEXT = (/= nullFunPtr) . pVkQueueBeginDebugUtilsLabelEXT
hasQueueEndDebugUtilsLabelEXT :: DeviceCmds -> Bool
hasQueueEndDebugUtilsLabelEXT = (/= nullFunPtr) . pVkQueueEndDebugUtilsLabelEXT
hasQueueInsertDebugUtilsLabelEXT :: DeviceCmds -> Bool
hasQueueInsertDebugUtilsLabelEXT = (/= nullFunPtr) . pVkQueueInsertDebugUtilsLabelEXT
hasCmdBeginDebugUtilsLabelEXT :: DeviceCmds -> Bool
hasCmdBeginDebugUtilsLabelEXT = (/= nullFunPtr) . pVkCmdBeginDebugUtilsLabelEXT
hasCmdEndDebugUtilsLabelEXT :: DeviceCmds -> Bool
hasCmdEndDebugUtilsLabelEXT = (/= nullFunPtr) . pVkCmdEndDebugUtilsLabelEXT
hasCmdInsertDebugUtilsLabelEXT :: DeviceCmds -> Bool
hasCmdInsertDebugUtilsLabelEXT = (/= nullFunPtr) . pVkCmdInsertDebugUtilsLabelEXT
hasGetMemoryHostPointerPropertiesEXT :: DeviceCmds -> Bool
hasGetMemoryHostPointerPropertiesEXT = (/= nullFunPtr) . pVkGetMemoryHostPointerPropertiesEXT
hasCmdWriteBufferMarkerAMD :: DeviceCmds -> Bool
hasCmdWriteBufferMarkerAMD = (/= nullFunPtr) . pVkCmdWriteBufferMarkerAMD
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
hasGetAndroidHardwareBufferPropertiesANDROID :: DeviceCmds -> Bool
hasGetAndroidHardwareBufferPropertiesANDROID = (/= nullFunPtr) . pVkGetAndroidHardwareBufferPropertiesANDROID
hasGetMemoryAndroidHardwareBufferANDROID :: DeviceCmds -> Bool
hasGetMemoryAndroidHardwareBufferANDROID = (/= nullFunPtr) . pVkGetMemoryAndroidHardwareBufferANDROID
#endif

hasDestroyInstance :: InstanceCmds -> Bool
hasDestroyInstance = (/= nullFunPtr) . pVkDestroyInstance
hasEnumeratePhysicalDevices :: InstanceCmds -> Bool
hasEnumeratePhysicalDevices = (/= nullFunPtr) . pVkEnumeratePhysicalDevices
hasGetInstanceProcAddr :: InstanceCmds -> Bool
hasGetInstanceProcAddr = (/= nullFunPtr) . pVkGetInstanceProcAddr
hasGetPhysicalDeviceProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceProperties
hasGetPhysicalDeviceQueueFamilyProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceQueueFamilyProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceQueueFamilyProperties
hasGetPhysicalDeviceMemoryProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceMemoryProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceMemoryProperties
hasGetPhysicalDeviceFeatures :: InstanceCmds -> Bool
hasGetPhysicalDeviceFeatures = (/= nullFunPtr) . pVkGetPhysicalDeviceFeatures
hasGetPhysicalDeviceFormatProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceFormatProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceFormatProperties
hasGetPhysicalDeviceImageFormatProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceImageFormatProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceImageFormatProperties
hasCreateDevice :: InstanceCmds -> Bool
hasCreateDevice = (/= nullFunPtr) . pVkCreateDevice
hasEnumerateDeviceLayerProperties :: InstanceCmds -> Bool
hasEnumerateDeviceLayerProperties = (/= nullFunPtr) . pVkEnumerateDeviceLayerProperties
hasEnumerateDeviceExtensionProperties :: InstanceCmds -> Bool
hasEnumerateDeviceExtensionProperties = (/= nullFunPtr) . pVkEnumerateDeviceExtensionProperties
hasGetPhysicalDeviceSparseImageFormatProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceSparseImageFormatProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceSparseImageFormatProperties
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
hasCreateAndroidSurfaceKHR :: InstanceCmds -> Bool
hasCreateAndroidSurfaceKHR = (/= nullFunPtr) . pVkCreateAndroidSurfaceKHR
#endif
hasGetPhysicalDeviceDisplayPropertiesKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceDisplayPropertiesKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceDisplayPropertiesKHR
hasGetPhysicalDeviceDisplayPlanePropertiesKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceDisplayPlanePropertiesKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceDisplayPlanePropertiesKHR
hasGetDisplayPlaneSupportedDisplaysKHR :: InstanceCmds -> Bool
hasGetDisplayPlaneSupportedDisplaysKHR = (/= nullFunPtr) . pVkGetDisplayPlaneSupportedDisplaysKHR
hasGetDisplayModePropertiesKHR :: InstanceCmds -> Bool
hasGetDisplayModePropertiesKHR = (/= nullFunPtr) . pVkGetDisplayModePropertiesKHR
hasCreateDisplayModeKHR :: InstanceCmds -> Bool
hasCreateDisplayModeKHR = (/= nullFunPtr) . pVkCreateDisplayModeKHR
hasGetDisplayPlaneCapabilitiesKHR :: InstanceCmds -> Bool
hasGetDisplayPlaneCapabilitiesKHR = (/= nullFunPtr) . pVkGetDisplayPlaneCapabilitiesKHR
hasCreateDisplayPlaneSurfaceKHR :: InstanceCmds -> Bool
hasCreateDisplayPlaneSurfaceKHR = (/= nullFunPtr) . pVkCreateDisplayPlaneSurfaceKHR
#if defined(VK_USE_PLATFORM_MIR_KHR)
hasCreateMirSurfaceKHR :: InstanceCmds -> Bool
hasCreateMirSurfaceKHR = (/= nullFunPtr) . pVkCreateMirSurfaceKHR
hasGetPhysicalDeviceMirPresentationSupportKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceMirPresentationSupportKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceMirPresentationSupportKHR
#endif
hasDestroySurfaceKHR :: InstanceCmds -> Bool
hasDestroySurfaceKHR = (/= nullFunPtr) . pVkDestroySurfaceKHR
hasGetPhysicalDeviceSurfaceSupportKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfaceSupportKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfaceSupportKHR
hasGetPhysicalDeviceSurfaceCapabilitiesKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfaceCapabilitiesKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfaceCapabilitiesKHR
hasGetPhysicalDeviceSurfaceFormatsKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfaceFormatsKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfaceFormatsKHR
hasGetPhysicalDeviceSurfacePresentModesKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfacePresentModesKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfacePresentModesKHR
#if defined(VK_USE_PLATFORM_VI_NN)
hasCreateViSurfaceNN :: InstanceCmds -> Bool
hasCreateViSurfaceNN = (/= nullFunPtr) . pVkCreateViSurfaceNN
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
hasCreateWaylandSurfaceKHR :: InstanceCmds -> Bool
hasCreateWaylandSurfaceKHR = (/= nullFunPtr) . pVkCreateWaylandSurfaceKHR
hasGetPhysicalDeviceWaylandPresentationSupportKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceWaylandPresentationSupportKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceWaylandPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
hasCreateWin32SurfaceKHR :: InstanceCmds -> Bool
hasCreateWin32SurfaceKHR = (/= nullFunPtr) . pVkCreateWin32SurfaceKHR
hasGetPhysicalDeviceWin32PresentationSupportKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceWin32PresentationSupportKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceWin32PresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
hasCreateXlibSurfaceKHR :: InstanceCmds -> Bool
hasCreateXlibSurfaceKHR = (/= nullFunPtr) . pVkCreateXlibSurfaceKHR
hasGetPhysicalDeviceXlibPresentationSupportKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceXlibPresentationSupportKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceXlibPresentationSupportKHR
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
hasCreateXcbSurfaceKHR :: InstanceCmds -> Bool
hasCreateXcbSurfaceKHR = (/= nullFunPtr) . pVkCreateXcbSurfaceKHR
hasGetPhysicalDeviceXcbPresentationSupportKHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceXcbPresentationSupportKHR = (/= nullFunPtr) . pVkGetPhysicalDeviceXcbPresentationSupportKHR
#endif
hasCreateDebugReportCallbackEXT :: InstanceCmds -> Bool
hasCreateDebugReportCallbackEXT = (/= nullFunPtr) . pVkCreateDebugReportCallbackEXT
hasDestroyDebugReportCallbackEXT :: InstanceCmds -> Bool
hasDestroyDebugReportCallbackEXT = (/= nullFunPtr) . pVkDestroyDebugReportCallbackEXT
hasDebugReportMessageEXT :: InstanceCmds -> Bool
hasDebugReportMessageEXT = (/= nullFunPtr) . pVkDebugReportMessageEXT
hasGetPhysicalDeviceExternalImageFormatPropertiesNV :: InstanceCmds -> Bool
hasGetPhysicalDeviceExternalImageFormatPropertiesNV = (/= nullFunPtr) . pVkGetPhysicalDeviceExternalImageFormatPropertiesNV
hasGetPhysicalDeviceGeneratedCommandsPropertiesNVX :: InstanceCmds -> Bool
hasGetPhysicalDeviceGeneratedCommandsPropertiesNVX = (/= nullFunPtr) . pVkGetPhysicalDeviceGeneratedCommandsPropertiesNVX
hasGetPhysicalDeviceFeatures2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceFeatures2 = (/= nullFunPtr) . pVkGetPhysicalDeviceFeatures2
hasGetPhysicalDeviceProperties2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceProperties2 = (/= nullFunPtr) . pVkGetPhysicalDeviceProperties2
hasGetPhysicalDeviceFormatProperties2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceFormatProperties2 = (/= nullFunPtr) . pVkGetPhysicalDeviceFormatProperties2
hasGetPhysicalDeviceImageFormatProperties2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceImageFormatProperties2 = (/= nullFunPtr) . pVkGetPhysicalDeviceImageFormatProperties2
hasGetPhysicalDeviceQueueFamilyProperties2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceQueueFamilyProperties2 = (/= nullFunPtr) . pVkGetPhysicalDeviceQueueFamilyProperties2
hasGetPhysicalDeviceMemoryProperties2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceMemoryProperties2 = (/= nullFunPtr) . pVkGetPhysicalDeviceMemoryProperties2
hasGetPhysicalDeviceSparseImageFormatProperties2 :: InstanceCmds -> Bool
hasGetPhysicalDeviceSparseImageFormatProperties2 = (/= nullFunPtr) . pVkGetPhysicalDeviceSparseImageFormatProperties2
hasGetPhysicalDeviceExternalBufferProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceExternalBufferProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceExternalBufferProperties
hasGetPhysicalDeviceExternalSemaphoreProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceExternalSemaphoreProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceExternalSemaphoreProperties
hasGetPhysicalDeviceExternalFenceProperties :: InstanceCmds -> Bool
hasGetPhysicalDeviceExternalFenceProperties = (/= nullFunPtr) . pVkGetPhysicalDeviceExternalFenceProperties
hasReleaseDisplayEXT :: InstanceCmds -> Bool
hasReleaseDisplayEXT = (/= nullFunPtr) . pVkReleaseDisplayEXT
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
hasAcquireXlibDisplayEXT :: InstanceCmds -> Bool
hasAcquireXlibDisplayEXT = (/= nullFunPtr) . pVkAcquireXlibDisplayEXT
hasGetRandROutputDisplayEXT :: InstanceCmds -> Bool
hasGetRandROutputDisplayEXT = (/= nullFunPtr) . pVkGetRandROutputDisplayEXT
#endif
hasGetPhysicalDeviceSurfaceCapabilities2EXT :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfaceCapabilities2EXT = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfaceCapabilities2EXT
hasEnumeratePhysicalDeviceGroups :: InstanceCmds -> Bool
hasEnumeratePhysicalDeviceGroups = (/= nullFunPtr) . pVkEnumeratePhysicalDeviceGroups
hasGetPhysicalDevicePresentRectanglesKHR :: InstanceCmds -> Bool
hasGetPhysicalDevicePresentRectanglesKHR = (/= nullFunPtr) . pVkGetPhysicalDevicePresentRectanglesKHR
#if defined(VK_USE_PLATFORM_IOS_MVK)
hasCreateIOSSurfaceMVK :: InstanceCmds -> Bool
hasCreateIOSSurfaceMVK = (/= nullFunPtr) . pVkCreateIOSSurfaceMVK
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
hasCreateMacOSSurfaceMVK :: InstanceCmds -> Bool
hasCreateMacOSSurfaceMVK = (/= nullFunPtr) . pVkCreateMacOSSurfaceMVK
#endif
hasGetPhysicalDeviceMultisamplePropertiesEXT :: InstanceCmds -> Bool
hasGetPhysicalDeviceMultisamplePropertiesEXT = (/= nullFunPtr) . pVkGetPhysicalDeviceMultisamplePropertiesEXT
hasGetPhysicalDeviceSurfaceCapabilities2KHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfaceCapabilities2KHR = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfaceCapabilities2KHR
hasGetPhysicalDeviceSurfaceFormats2KHR :: InstanceCmds -> Bool
hasGetPhysicalDeviceSurfaceFormats2KHR = (/= nullFunPtr) . pVkGetPhysicalDeviceSurfaceFormats2KHR
hasCreateDebugUtilsMessengerEXT :: InstanceCmds -> Bool
hasCreateDebugUtilsMessengerEXT = (/= nullFunPtr) . pVkCreateDebugUtilsMessengerEXT
hasDestroyDebugUtilsMessengerEXT :: InstanceCmds -> Bool
hasDestroyDebugUtilsMessengerEXT = (/= nullFunPtr) . pVkDestroyDebugUtilsMessengerEXT
hasSubmitDebugUtilsMessageEXT :: InstanceCmds -> Bool
hasSubmitDebugUtilsMessageEXT = (/= nullFunPtr) . pVkSubmitDebugUtilsMessageEXT

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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
getSwapchainGrallocUsageANDROID :: DeviceCmds -> (("device" ::: VkDevice) -> ("format" ::: VkFormat) -> ("imageUsage" ::: VkImageUsageFlags) -> ("grallocUsage" ::: Ptr CInt) -> IO VkResult)
getSwapchainGrallocUsageANDROID deviceCmds = mkVkGetSwapchainGrallocUsageANDROID (pVkGetSwapchainGrallocUsageANDROID deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetSwapchainGrallocUsageANDROID
  :: FunPtr (("device" ::: VkDevice) -> ("format" ::: VkFormat) -> ("imageUsage" ::: VkImageUsageFlags) -> ("grallocUsage" ::: Ptr CInt) -> IO VkResult) -> (("device" ::: VkDevice) -> ("format" ::: VkFormat) -> ("imageUsage" ::: VkImageUsageFlags) -> ("grallocUsage" ::: Ptr CInt) -> IO VkResult)
acquireImageANDROID :: DeviceCmds -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("nativeFenceFd" ::: CInt) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> IO VkResult)
acquireImageANDROID deviceCmds = mkVkAcquireImageANDROID (pVkAcquireImageANDROID deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkAcquireImageANDROID
  :: FunPtr (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("nativeFenceFd" ::: CInt) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> IO VkResult) -> (("device" ::: VkDevice) -> ("image" ::: VkImage) -> ("nativeFenceFd" ::: CInt) -> ("semaphore" ::: VkSemaphore) -> ("fence" ::: VkFence) -> IO VkResult)
queueSignalReleaseImageANDROID :: DeviceCmds -> (("queue" ::: VkQueue) -> ("waitSemaphoreCount" ::: Word32) -> ("pWaitSemaphores" ::: Ptr VkSemaphore) -> ("image" ::: VkImage) -> ("pNativeFenceFd" ::: Ptr CInt) -> IO VkResult)
queueSignalReleaseImageANDROID deviceCmds = mkVkQueueSignalReleaseImageANDROID (pVkQueueSignalReleaseImageANDROID deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkQueueSignalReleaseImageANDROID
  :: FunPtr (("queue" ::: VkQueue) -> ("waitSemaphoreCount" ::: Word32) -> ("pWaitSemaphores" ::: Ptr VkSemaphore) -> ("image" ::: VkImage) -> ("pNativeFenceFd" ::: Ptr CInt) -> IO VkResult) -> (("queue" ::: VkQueue) -> ("waitSemaphoreCount" ::: Word32) -> ("pWaitSemaphores" ::: Ptr VkSemaphore) -> ("image" ::: VkImage) -> ("pNativeFenceFd" ::: Ptr CInt) -> IO VkResult)
getShaderInfoAMD :: DeviceCmds -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult)
getShaderInfoAMD deviceCmds = mkVkGetShaderInfoAMD (pVkGetShaderInfoAMD deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetShaderInfoAMD
  :: FunPtr (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pipeline" ::: VkPipeline) -> ("shaderStage" ::: VkShaderStageFlagBits) -> ("infoType" ::: VkShaderInfoTypeAMD) -> ("pInfoSize" ::: Ptr CSize) -> ("pInfo" ::: Ptr ()) -> IO VkResult)
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
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
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
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
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
#if defined(VK_USE_PLATFORM_MIR_KHR)
createMirSurfaceKHR :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createMirSurfaceKHR deviceCmds = mkVkCreateMirSurfaceKHR (pVkCreateMirSurfaceKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMirSurfaceKHR
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMirSurfaceCreateInfoKHR) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
getPhysicalDeviceMirPresentationSupportKHR :: InstanceCmds -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32)
getPhysicalDeviceMirPresentationSupportKHR deviceCmds = mkVkGetPhysicalDeviceMirPresentationSupportKHR (pVkGetPhysicalDeviceMirPresentationSupportKHR deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetPhysicalDeviceMirPresentationSupportKHR
  :: FunPtr (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32) -> (("physicalDevice" ::: VkPhysicalDevice) -> ("queueFamilyIndex" ::: Word32) -> ("connection" ::: Ptr MirConnection) -> IO VkBool32)
#endif
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
#if defined(VK_USE_PLATFORM_VI_NN)
createViSurfaceNN :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createViSurfaceNN deviceCmds = mkVkCreateViSurfaceNN (pVkCreateViSurfaceNN deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateViSurfaceNN
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkViSurfaceCreateInfoNN) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
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
#if defined(VK_USE_PLATFORM_WIN32_KHR)
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
#if defined(VK_USE_PLATFORM_XLIB_KHR)
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
#if defined(VK_USE_PLATFORM_XCB_KHR)
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
#if defined(VK_USE_PLATFORM_XLIB_XRANDR_EXT)
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
#if defined(VK_USE_PLATFORM_IOS_MVK)
createIOSSurfaceMVK :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createIOSSurfaceMVK deviceCmds = mkVkCreateIOSSurfaceMVK (pVkCreateIOSSurfaceMVK deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateIOSSurfaceMVK
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkIOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
createMacOSSurfaceMVK :: InstanceCmds -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
createMacOSSurfaceMVK deviceCmds = mkVkCreateMacOSSurfaceMVK (pVkCreateMacOSSurfaceMVK deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateMacOSSurfaceMVK
  :: FunPtr (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pCreateInfo" ::: Ptr VkMacOSSurfaceCreateInfoMVK) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pSurface" ::: Ptr VkSurfaceKHR) -> IO VkResult)
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
