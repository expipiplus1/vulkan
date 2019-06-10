{-# language Strict #-}
{-# language CPP #-}
{-# language FunctionalDependencies #-}
{-# language DefaultSignatures #-}
{-# language RecordWildCards #-}
{-# language InstanceSigs #-}
{-# language TypeApplications #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Marshal.ToCStruct
  ( ToCStruct(..)
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad.IO.Class
  ( liftIO
  )
import Control.Monad.Trans.Cont
  ( ContT(..)
  )
import Data.ByteString
  ( useAsCString
  )
import qualified Data.ByteString
  ( length
  )
import Data.ByteString.Unsafe
  ( unsafeUseAsCString
  )
import Data.Coerce
  ( coerce
  )
import Data.Either
  ( either
  )
import Data.Foldable
  ( traverse_
  )
import Data.Int
  ( Int32
  )
import Data.Maybe
  ( maybe
  )
import qualified Data.Vector
  ( imapM_
  , length
  , null
  , take
  )
import Data.Word
  ( Word32
  , Word64
  , Word8
  )
import Foreign.C.String
  ( CString
  )
import Foreign.C.Types
  ( CChar(..)
  , CFloat(..)
  , CInt(..)
  , CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( advancePtr
  , allocaArray
  )
import Foreign.Marshal.Utils
  ( maybeWith
  )
import Foreign.Ptr
  ( Ptr
  , nullPtr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , poke
  , pokeElemOff
  )
import GHC.IO.Exception
  ( IOErrorType(InvalidArgument)
  , IOException(..)
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateInfo(..)
  , VkSharingMode(..)
  , VkBufferCreateFlags
  , VkBufferUsageFlags
  )
import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateFlags(..)
  , VkBufferViewCreateInfo(..)
  , VkBufferView
  )
import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferInheritanceInfo(..)
  , VkCommandBufferLevel(..)
  , VkCommandBufferUsageFlags
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
  , VkClearValue(..)
  , VkDispatchIndirectCommand(..)
  , VkDrawIndexedIndirectCommand(..)
  , VkDrawIndirectCommand(..)
  , VkImageBlit(..)
  , VkImageCopy(..)
  , VkImageMemoryBarrier(..)
  , VkImageResolve(..)
  , VkImageSubresourceLayers(..)
  , VkIndexType(..)
  , VkMemoryBarrier(..)
  , VkRenderPassBeginInfo(..)
  , VkSubpassContents(..)
  )
import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateInfo(..)
  , VkCommandPool
  , VkCommandPoolCreateFlags
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorBufferInfo(..)
  , VkDescriptorImageInfo(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorPoolSize(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutBinding(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkDescriptorType(..)
  , VkWriteDescriptorSet(..)
  , VkDescriptorPool
  , VkDescriptorPoolCreateFlags
  , VkDescriptorSet
  , VkDescriptorSetLayoutCreateFlags
  )
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateFlags(..)
  , VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateInfo(..)
  , VkDeviceQueueCreateFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkApplicationInfo(..)
  , VkExtent3D(..)
  , VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkImageTiling(..)
  , VkImageType(..)
  , VkInstanceCreateFlags(..)
  , VkInstanceCreateInfo(..)
  , VkMemoryHeap(..)
  , VkMemoryType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkPhysicalDeviceType(..)
  , VkQueueFamilyProperties(..)
  , VkSampleCountFlagBits(..)
  , PFN_vkAllocationFunction
  , PFN_vkFreeFunction
  , PFN_vkInternalAllocationNotification
  , PFN_vkInternalFreeNotification
  , PFN_vkReallocationFunction
  , VkDeviceSize
  , VkFormatFeatureFlags
  , VkImageCreateFlags
  , VkImageUsageFlags
  , VkMemoryHeapFlags
  , VkMemoryPropertyFlags
  , VkPhysicalDevice
  , VkQueueFlags
  , VkSampleCountFlags
  , pattern VK_MAX_MEMORY_HEAPS
  , pattern VK_MAX_MEMORY_TYPES
  , pattern VK_MAX_PHYSICAL_DEVICE_NAME_SIZE
  , pattern VK_UUID_SIZE
  )
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateFlags(..)
  , VkEventCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
  , pattern VK_MAX_EXTENSION_NAME_SIZE
  )
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateInfo(..)
  , VkFenceCreateFlags
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkImageLayout(..)
  , VkSubresourceLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  , VkComponentSwizzle(..)
  , VkImageSubresourceRange(..)
  , VkImageViewCreateInfo(..)
  , VkImageViewType(..)
  , VkImageView
  , VkImageViewCreateFlags
  )
import Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties(..)
  , pattern VK_MAX_DESCRIPTION_SIZE
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  , VkDeviceMemory
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  , VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAttachmentDescription(..)
  , VkAttachmentLoadOp(..)
  , VkAttachmentReference(..)
  , VkAttachmentStoreOp(..)
  , VkFramebufferCreateFlags(..)
  , VkFramebufferCreateInfo(..)
  , VkPipelineBindPoint(..)
  , VkRenderPassCreateFlags(..)
  , VkRenderPassCreateInfo(..)
  , VkSubpassDependency(..)
  , VkSubpassDescription(..)
  , VkAccessFlags
  , VkAttachmentDescriptionFlags
  , VkDependencyFlags
  , VkFramebuffer
  , VkSubpassDescriptionFlags
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkBlendFactor(..)
  , VkBlendOp(..)
  , VkCompareOp(..)
  , VkComputePipelineCreateInfo(..)
  , VkDynamicState(..)
  , VkExtent2D(..)
  , VkFrontFace(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkLogicOp(..)
  , VkOffset2D(..)
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateFlags(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineDepthStencilStateCreateFlags(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineDynamicStateCreateFlags(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateFlags(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineMultisampleStateCreateFlags(..)
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateFlags(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineShaderStageCreateFlags(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkPipelineTessellationStateCreateFlags(..)
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineVertexInputStateCreateFlags(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineViewportStateCreateFlags(..)
  , VkPipelineViewportStateCreateInfo(..)
  , VkPolygonMode(..)
  , VkPrimitiveTopology(..)
  , VkRect2D(..)
  , VkShaderStageFlagBits(..)
  , VkSpecializationInfo(..)
  , VkSpecializationMapEntry(..)
  , VkStencilOp(..)
  , VkStencilOpState(..)
  , VkVertexInputAttributeDescription(..)
  , VkVertexInputBindingDescription(..)
  , VkVertexInputRate(..)
  , VkViewport(..)
  , VkColorComponentFlags
  , VkCullModeFlags
  , VkPipeline
  , VkPipelineCreateFlags
  , VkPipelineLayout
  , VkRenderPass
  , VkSampleMask
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags(..)
  , VkPipelineCacheCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateFlags(..)
  , VkPipelineLayoutCreateInfo(..)
  , VkPushConstantRange(..)
  , VkDescriptorSetLayout
  , VkShaderStageFlags
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPoolCreateFlags(..)
  , VkQueryPoolCreateInfo(..)
  , VkQueryType(..)
  , VkQueryPipelineStatisticFlags
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkPipelineStageFlagBits(..)
  , VkSubmitInfo(..)
  , VkCommandBuffer
  , VkFence
  , VkPipelineStageFlags
  , VkSemaphore
  )
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateFlags(..)
  , VkSemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor(..)
  , VkFilter(..)
  , VkSamplerAddressMode(..)
  , VkSamplerCreateInfo(..)
  , VkSamplerMipmapMode(..)
  , VkSampler
  , VkSamplerCreateFlags
  )
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateFlags(..)
  , VkShaderModuleCreateInfo(..)
  , VkShaderModule
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageAspectFlagBits(..)
  , VkImageSubresource(..)
  , VkOffset3D(..)
  , VkSparseBufferMemoryBindInfo(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryBind(..)
  , VkSparseImageMemoryBindInfo(..)
  , VkSparseImageMemoryRequirements(..)
  , VkSparseImageOpaqueMemoryBindInfo(..)
  , VkSparseMemoryBind(..)
  , VkImageAspectFlags
  , VkSparseImageFormatFlags
  , VkSparseMemoryBindFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkProtectedSubmitInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkPhysicalDeviceSubgroupProperties(..)
  , VkSubgroupFeatureFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage
  ( VkPhysicalDevice16BitStorageFeatures(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateFlags(..)
  , VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplateEntry(..)
  , VkDescriptorUpdateTemplateType(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagsInfo(..)
  , VkMemoryAllocateFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , VkFenceImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceHandleTypeFlagBits(..)
  , VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , VkExternalFenceFeatureFlags
  , VkExternalFenceHandleTypeFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkExternalMemoryHandleTypeFlagBits(..)
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkPhysicalDeviceIDProperties(..)
  , VkExternalMemoryFeatureFlags
  , VkExternalMemoryHandleTypeFlags
  , pattern VK_LUID_SIZE
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo(..)
  , VkSemaphoreImportFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreHandleTypeFlagBits(..)
  , VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , VkExternalSemaphoreFeatureFlags
  , VkExternalSemaphoreHandleTypeFlags
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
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
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkInputAttachmentAspectReference(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkPointClippingBehavior(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , VkTessellationDomainOrigin(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkBindImagePlaneMemoryInfo(..)
  , VkChromaLocation(..)
  , VkImagePlaneMemoryRequirementsInfo(..)
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversionImageFormatProperties(..)
  , VkSamplerYcbcrConversionInfo(..)
  , VkSamplerYcbcrModelConversion(..)
  , VkSamplerYcbcrRange(..)
  , VkSamplerYcbcrConversion
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParametersFeatures(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointersFeatures(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , VkSwapchainDisplayNativeHdrCreateInfoAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkDeviceMemoryOverallocationCreateInfoAMD(..)
  , VkMemoryOverallocationBehaviorAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( VkPipelineRasterizationStateRasterizationOrderAMD(..)
  , VkRasterizationOrderAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( VkPhysicalDeviceShaderCorePropertiesAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( VkTextureLODGatherFormatPropertiesAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkExternalFormatANDROID(..)
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , AHardwareBuffer
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( VkImageViewASTCDecodeModeEXT(..)
  , VkPhysicalDeviceASTCDecodeFeaturesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkBlendOverlapEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , VkDeviceAddress
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , VkTimeDomainEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , VkConditionalRenderingFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT(..)
  , VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , VkDebugReportObjectTypeEXT(..)
  , PFN_vkDebugReportCallbackEXT
  , VkDebugReportFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCallbackDataFlagsEXT(..)
  , VkDebugUtilsMessengerCreateFlagsEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
  , PFN_vkDebugUtilsMessengerCallbackEXT
  , VkDebugUtilsMessageSeverityFlagsEXT
  , VkDebugUtilsMessageTypeFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable
  ( VkPhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , VkPipelineRasterizationDepthClipStateCreateFlagsEXT(..)
  , VkPipelineRasterizationDepthClipStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , VkDescriptorBindingFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkDiscardRectangleModeEXT(..)
  , VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , VkPipelineDiscardRectangleStateCreateFlagsEXT(..)
  , VkPipelineDiscardRectangleStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDeviceEventTypeEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkDisplayEventTypeEXT(..)
  , VkDisplayPowerInfoEXT(..)
  , VkDisplayPowerStateEXT(..)
  , VkSwapchainCounterCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , VkSurfaceCounterFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( VkFilterCubicImageViewImageFormatPropertiesEXT(..)
  , VkPhysicalDeviceImageViewImageFormatInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkFullScreenExclusiveEXT(..)
  , VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , HMONITOR
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkDeviceQueueGlobalPriorityCreateInfoEXT(..)
  , VkQueueGlobalPriorityEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , VkXYColorEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateFlagsEXT(..)
  , VkHeadlessSurfaceCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT(..)
  , VkDrmFormatModifierPropertiesListEXT(..)
  , VkImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , VkImageDrmFormatModifierListCreateInfoEXT(..)
  , VkImageDrmFormatModifierPropertiesEXT(..)
  , VkPhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( VkDescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , VkPhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , VkPhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , VkWriteDescriptorSetInlineUniformBlockEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( VkMemoryPriorityAllocateInfoEXT(..)
  , VkPhysicalDeviceMemoryPriorityFeaturesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateFlagsEXT(..)
  , VkMetalSurfaceCreateInfoEXT(..)
  , CAMetalLayer
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( VkPhysicalDevicePCIBusInfoPropertiesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , VkPipelineCreationFeedbackEXT(..)
  , VkPipelineCreationFeedbackFlagsEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkAttachmentSampleLocationsEXT(..)
  , VkMultisamplePropertiesEXT(..)
  , VkPhysicalDeviceSampleLocationsPropertiesEXT(..)
  , VkPipelineSampleLocationsStateCreateInfoEXT(..)
  , VkRenderPassSampleLocationsBeginInfoEXT(..)
  , VkSampleLocationEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , VkSubpassSampleLocationsEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , VkSamplerReductionModeCreateInfoEXT(..)
  , VkSamplerReductionModeEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( VkPhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( VkImageStencilUsageCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , VkPipelineRasterizationStateStreamCreateFlagsEXT(..)
  , VkPipelineRasterizationStateStreamCreateInfoEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateFlagsEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , VkValidationCacheEXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeatureDisableEXT(..)
  , VkValidationFeatureEnableEXT(..)
  , VkValidationFeaturesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationCheckEXT(..)
  , VkValidationFlagsEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VkVertexInputBindingDivisorDescriptionEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( VkPhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  )
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateFlagsFUCHSIA(..)
  , VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  , Zx_handle_t
  )
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( VkPresentFrameTokenGGP(..)
  , GgpFrameToken
  )
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateFlagsGGP(..)
  , VkStreamDescriptorSurfaceCreateInfoGGP(..)
  , GgpStreamDescriptor
  )
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( VkPhysicalDevice8BitStorageFeaturesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateFlagsKHR(..)
  , VkAndroidSurfaceCreateInfoKHR(..)
  , ANativeWindow
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentDescription2KHR(..)
  , VkAttachmentReference2KHR(..)
  , VkRenderPassCreateInfo2KHR(..)
  , VkSubpassBeginInfoKHR(..)
  , VkSubpassDependency2KHR(..)
  , VkSubpassDescription2KHR(..)
  , VkSubpassEndInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkPhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , VkResolveModeFlagBitsKHR(..)
  , VkSubpassDescriptionDepthStencilResolveKHR(..)
  , VkResolveModeFlagsKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateFlagsKHR(..)
  , VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneAlphaFlagBitsKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateFlagsKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , VkDisplayKHR
  , VkDisplayModeKHR
  , VkDisplayPlaneAlphaFlagsKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkConformanceVersionKHR(..)
  , VkDriverIdKHR(..)
  , VkPhysicalDeviceDriverPropertiesKHR(..)
  , pattern VK_MAX_DRIVER_INFO_SIZE_KHR
  , pattern VK_MAX_DRIVER_NAME_SIZE_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , LPCWSTR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( VkImageFormatListCreateInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( VkPresentRegionKHR(..)
  , VkPresentRegionsKHR(..)
  , VkRectLayerKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( VkPhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( VkPhysicalDeviceFloat16Int8FeaturesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkColorSpaceKHR(..)
  , VkCompositeAlphaFlagBitsKHR(..)
  , VkPresentModeKHR(..)
  , VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  , VkSurfaceTransformFlagBitsKHR(..)
  , VkCompositeAlphaFlagsKHR
  , VkSurfaceKHR
  , VkSurfaceTransformFlagsKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( VkSurfaceProtectedCapabilitiesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupPresentModeFlagBitsKHR(..)
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , VkImageSwapchainCreateInfoKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateInfoKHR(..)
  , VkDeviceGroupPresentModeFlagsKHR
  , VkSwapchainCreateFlagsKHR
  , VkSwapchainKHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( VkPhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateFlagsKHR(..)
  , VkWaylandSurfaceCreateInfoKHR(..)
  , Wl_display
  , Wl_surface
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateFlagsKHR(..)
  , VkWin32SurfaceCreateInfoKHR(..)
  , HINSTANCE
  , HWND
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateFlagsKHR(..)
  , VkXcbSurfaceCreateInfoKHR(..)
  , Xcb_connection_t
  , Xcb_window_t
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( Display(..)
  , VkXlibSurfaceCreateFlagsKHR(..)
  , VkXlibSurfaceCreateInfoKHR(..)
  , Window
  )
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateFlagsMVK(..)
  , VkIOSSurfaceCreateInfoMVK(..)
  )
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateFlagsMVK(..)
  , VkMacOSSurfaceCreateInfoMVK(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateFlagsNN(..)
  , VkViSurfaceCreateInfoNN(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkIndirectCommandsLayoutTokenNVX(..)
  , VkIndirectCommandsTokenNVX(..)
  , VkIndirectCommandsTokenTypeNVX(..)
  , VkObjectEntryTypeNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableDescriptorSetEntryNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkObjectTableIndexBufferEntryNVX(..)
  , VkObjectTablePipelineEntryNVX(..)
  , VkObjectTablePushConstantEntryNVX(..)
  , VkObjectTableVertexBufferEntryNVX(..)
  , VkIndirectCommandsLayoutNVX
  , VkIndirectCommandsLayoutUsageFlagsNVX
  , VkObjectEntryUsageFlagsNVX
  , VkObjectTableNVX
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , VkViewportWScalingNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( VkPhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkComponentTypeNV(..)
  , VkCooperativeMatrixPropertiesNV(..)
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , VkScopeNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( VkPhysicalDeviceCornerSampledImageFeaturesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , VkQueueFamilyCheckpointPropertiesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( VkExportMemoryAllocateInfoNV(..)
  , VkExternalMemoryImageCreateInfoNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  , VkExternalMemoryFeatureFlagsNV
  , VkExternalMemoryHandleTypeFlagsNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  , DWORD
  , HANDLE
  , SECURITY_ATTRIBUTES
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateFlagsNV(..)
  , VkPipelineCoverageToColorStateCreateInfoNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric
  ( VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkCoverageModulationModeNV(..)
  , VkPipelineCoverageModulationStateCreateFlagsNV(..)
  , VkPipelineCoverageModulationStateCreateInfoNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkDrawMeshTasksIndirectCommandNV(..)
  , VkPhysicalDeviceMeshShaderFeaturesNV(..)
  , VkPhysicalDeviceMeshShaderPropertiesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV(..)
  , VkAccelerationStructureInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsTypeNV(..)
  , VkAccelerationStructureTypeNV(..)
  , VkBindAccelerationStructureMemoryInfoNV(..)
  , VkGeometryAABBNV(..)
  , VkGeometryDataNV(..)
  , VkGeometryNV(..)
  , VkGeometryTrianglesNV(..)
  , VkGeometryTypeNV(..)
  , VkPhysicalDeviceRayTracingPropertiesNV(..)
  , VkRayTracingPipelineCreateInfoNV(..)
  , VkRayTracingShaderGroupCreateInfoNV(..)
  , VkRayTracingShaderGroupTypeNV(..)
  , VkWriteDescriptorSetAccelerationStructureNV(..)
  , VkAccelerationStructureNV
  , VkBuildAccelerationStructureFlagsNV
  , VkGeometryFlagsNV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , VkPipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( VkPhysicalDeviceShaderImageFootprintFeaturesNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleLocationNV(..)
  , VkCoarseSampleOrderCustomNV(..)
  , VkCoarseSampleOrderTypeNV(..)
  , VkPhysicalDeviceShadingRateImageFeaturesNV(..)
  , VkPhysicalDeviceShadingRateImagePropertiesNV(..)
  , VkPipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , VkPipelineViewportShadingRateImageStateCreateInfoNV(..)
  , VkShadingRatePaletteEntryNV(..)
  , VkShadingRatePaletteNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateFlagsNV(..)
  , VkPipelineViewportSwizzleStateCreateInfoNV(..)
  , VkViewportCoordinateSwizzleNV(..)
  , VkViewportSwizzleNV(..)
  )
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
  )
import Graphics.Vulkan.Core10.Buffer
  ( BufferCreateInfo(..)
  )
import Graphics.Vulkan.Core10.BufferView
  ( BufferViewCreateInfo(..)
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( CommandBufferAllocateInfo(..)
  , CommandBufferBeginInfo(..)
  , CommandBufferInheritanceInfo(..)
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( BufferMemoryBarrier(..)
  , ImageMemoryBarrier(..)
  , MemoryBarrier(..)
  , RenderPassBeginInfo(..)
  , BufferCopy(..)
  , BufferImageCopy(..)
  , ClearAttachment(..)
  , ClearColorValue(..)
  , ClearDepthStencilValue(..)
  , ClearRect(..)
  , ClearValue(..)
  , DispatchIndirectCommand(..)
  , DrawIndexedIndirectCommand(..)
  , DrawIndirectCommand(..)
  , ImageBlit(..)
  , ImageCopy(..)
  , ImageResolve(..)
  , ImageSubresourceLayers(..)
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPoolCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Core
  ( boolToBool32
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
  , pattern STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_APPLICATION_INFO
  , pattern STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  , pattern STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  , pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  , pattern STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_NV
  , pattern STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern STRUCTURE_TYPE_HDR_METADATA_EXT
  , pattern STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
  , pattern STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  , pattern STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  , pattern STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  , pattern STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  , pattern STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  , pattern STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  , pattern STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  , pattern STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  , pattern STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  , pattern STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  , pattern STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  , pattern STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
  , pattern STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  , pattern STRUCTURE_TYPE_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  , pattern STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  , pattern STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
  , pattern STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  , pattern STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  , pattern STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  , pattern STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  , pattern STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  , pattern STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  , pattern STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  , pattern STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  , pattern STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( CopyDescriptorSet(..)
  , DescriptorPoolCreateInfo(..)
  , DescriptorSetAllocateInfo(..)
  , DescriptorSetLayoutCreateInfo(..)
  , WriteDescriptorSet(..)
  , DescriptorBufferInfo(..)
  , DescriptorImageInfo(..)
  , DescriptorPoolSize(..)
  , DescriptorSetLayoutBinding(..)
  )
import Graphics.Vulkan.Core10.Device
  ( DeviceCreateInfo(..)
  , DeviceQueueCreateInfo(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ApplicationInfo(..)
  , InstanceCreateInfo(..)
  , AllocationCallbacks(..)
  , Extent3D(..)
  , FormatProperties(..)
  , ImageFormatProperties(..)
  , MemoryHeap(..)
  , MemoryType(..)
  , PhysicalDevice(..)
  , PhysicalDeviceFeatures(..)
  , PhysicalDeviceLimits(..)
  , PhysicalDeviceMemoryProperties(..)
  , PhysicalDeviceProperties(..)
  , PhysicalDeviceSparseProperties(..)
  , QueueFamilyProperties(..)
  )
import Graphics.Vulkan.Core10.Event
  ( EventCreateInfo(..)
  )
import Graphics.Vulkan.Core10.ExtensionDiscovery
  ( ExtensionProperties(..)
  )
import Graphics.Vulkan.Core10.Fence
  ( FenceCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Image
  ( ImageCreateInfo(..)
  , SubresourceLayout(..)
  )
import Graphics.Vulkan.Core10.ImageView
  ( ImageViewCreateInfo(..)
  , ComponentMapping(..)
  , ImageSubresourceRange(..)
  )
import Graphics.Vulkan.Core10.LayerDiscovery
  ( LayerProperties(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( MappedMemoryRange(..)
  , MemoryAllocateInfo(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( MemoryRequirements(..)
  )
import Graphics.Vulkan.Core10.Pass
  ( FramebufferCreateInfo(..)
  , RenderPassCreateInfo(..)
  , AttachmentDescription(..)
  , AttachmentReference(..)
  , SubpassDependency(..)
  , SubpassDescription(..)
  )
import Graphics.Vulkan.Core10.Pipeline
  ( ComputePipelineCreateInfo(..)
  , GraphicsPipelineCreateInfo(..)
  , PipelineColorBlendStateCreateInfo(..)
  , PipelineDepthStencilStateCreateInfo(..)
  , PipelineDynamicStateCreateInfo(..)
  , PipelineInputAssemblyStateCreateInfo(..)
  , PipelineMultisampleStateCreateInfo(..)
  , PipelineRasterizationStateCreateInfo(..)
  , PipelineShaderStageCreateInfo(..)
  , PipelineTessellationStateCreateInfo(..)
  , PipelineVertexInputStateCreateInfo(..)
  , PipelineViewportStateCreateInfo(..)
  , Extent2D(..)
  , Offset2D(..)
  , PipelineColorBlendAttachmentState(..)
  , Rect2D(..)
  , SpecializationInfo(..)
  , SpecializationMapEntry(..)
  , StencilOpState(..)
  , VertexInputAttributeDescription(..)
  , VertexInputBindingDescription(..)
  , Viewport(..)
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCacheCreateInfo(..)
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( PipelineLayoutCreateInfo(..)
  , PushConstantRange(..)
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPoolCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Queue
  ( SubmitInfo(..)
  , CommandBuffer(..)
  )
import Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Sampler
  ( SamplerCreateInfo(..)
  )
import Graphics.Vulkan.Core10.Shader
  ( ShaderModuleCreateInfo(..)
  )
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( BindSparseInfo(..)
  , ImageSubresource(..)
  , Offset3D(..)
  , SparseBufferMemoryBindInfo(..)
  , SparseImageFormatProperties(..)
  , SparseImageMemoryBind(..)
  , SparseImageMemoryBindInfo(..)
  , SparseImageMemoryRequirements(..)
  , SparseImageOpaqueMemoryBindInfo(..)
  , SparseMemoryBind(..)
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( DeviceQueueInfo2(..)
  , PhysicalDeviceProtectedMemoryFeatures(..)
  , PhysicalDeviceProtectedMemoryProperties(..)
  , ProtectedSubmitInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( PhysicalDeviceSubgroupProperties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeatures(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( BindBufferMemoryInfo(..)
  , BindImageMemoryInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfo(..)
  , MemoryDedicatedRequirements(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateInfo(..)
  , DescriptorUpdateTemplateEntry(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( DeviceGroupBindSparseInfo(..)
  , DeviceGroupCommandBufferBeginInfo(..)
  , DeviceGroupRenderPassBeginInfo(..)
  , DeviceGroupSubmitInfo(..)
  , MemoryAllocateFlagsInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( BindBufferMemoryDeviceGroupInfo(..)
  , BindImageMemoryDeviceGroupInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfo(..)
  , PhysicalDeviceGroupProperties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( ExportFenceCreateInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceProperties(..)
  , PhysicalDeviceExternalFenceInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( ExportMemoryAllocateInfo(..)
  , ExternalMemoryBufferCreateInfo(..)
  , ExternalMemoryImageCreateInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalBufferProperties(..)
  , ExternalImageFormatProperties(..)
  , PhysicalDeviceExternalBufferInfo(..)
  , PhysicalDeviceExternalImageFormatInfo(..)
  , PhysicalDeviceIDProperties(..)
  , ExternalMemoryProperties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreProperties(..)
  , PhysicalDeviceExternalSemaphoreInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( BufferMemoryRequirementsInfo2(..)
  , ImageMemoryRequirementsInfo2(..)
  , ImageSparseMemoryRequirementsInfo2(..)
  , MemoryRequirements2(..)
  , SparseImageMemoryRequirements2(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2
  ( FormatProperties2(..)
  , ImageFormatProperties2(..)
  , PhysicalDeviceFeatures2(..)
  , PhysicalDeviceImageFormatInfo2(..)
  , PhysicalDeviceMemoryProperties2(..)
  , PhysicalDeviceProperties2(..)
  , PhysicalDeviceSparseImageFormatInfo2(..)
  , QueueFamilyProperties2(..)
  , SparseImageFormatProperties2(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( ImageViewUsageCreateInfo(..)
  , PhysicalDevicePointClippingProperties(..)
  , PipelineTessellationDomainOriginStateCreateInfo(..)
  , RenderPassInputAttachmentAspectCreateInfo(..)
  , InputAttachmentAspectReference(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( DescriptorSetLayoutSupport(..)
  , PhysicalDeviceMaintenance3Properties(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeatures(..)
  , PhysicalDeviceMultiviewProperties(..)
  , RenderPassMultiviewCreateInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( BindImagePlaneMemoryInfo(..)
  , ImagePlaneMemoryRequirementsInfo(..)
  , PhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , SamplerYcbcrConversionCreateInfo(..)
  , SamplerYcbcrConversionImageFormatProperties(..)
  , SamplerYcbcrConversionInfo(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( PhysicalDeviceShaderDrawParametersFeatures(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointersFeatures(..)
  )
import Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr
  ( DisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , SwapchainDisplayNativeHdrCreateInfoAMD(..)
  )
import Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( DeviceMemoryOverallocationCreateInfoAMD(..)
  )
import Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( PipelineRasterizationStateRasterizationOrderAMD(..)
  )
import Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( PhysicalDeviceShaderCorePropertiesAMD(..)
  )
import Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( ShaderResourceUsageAMD(..)
  , ShaderStatisticsInfoAMD(..)
  )
import Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  ( TextureLODGatherFormatPropertiesAMD(..)
  )
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AndroidHardwareBufferFormatPropertiesANDROID(..)
  , AndroidHardwareBufferPropertiesANDROID(..)
  , AndroidHardwareBufferUsageANDROID(..)
  , ExternalFormatANDROID(..)
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode
  ( ImageViewASTCDecodeModeEXT(..)
  , PhysicalDeviceASTCDecodeFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( BufferDeviceAddressCreateInfoEXT(..)
  , BufferDeviceAddressInfoEXT(..)
  , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( CalibratedTimestampInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( CommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , ConditionalRenderingBeginInfoEXT(..)
  , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , PipelineRasterizationConservativeStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( DebugMarkerMarkerInfoEXT(..)
  , DebugMarkerObjectNameInfoEXT(..)
  , DebugMarkerObjectTagInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportCallbackCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( DebugUtilsLabelEXT(..)
  , DebugUtilsMessengerCallbackDataEXT(..)
  , DebugUtilsMessengerCreateInfoEXT(..)
  , DebugUtilsObjectNameInfoEXT(..)
  , DebugUtilsObjectTagInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable
  ( PhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , PhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , PhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( PhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , PipelineDiscardRectangleStateCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( DeviceEventInfoEXT(..)
  , DisplayEventInfoEXT(..)
  , DisplayPowerInfoEXT(..)
  , SwapchainCounterCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCapabilities2EXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( ImportMemoryHostPointerInfoEXT(..)
  , MemoryHostPointerPropertiesEXT(..)
  , PhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_filter_cubic
  ( FilterCubicImageViewImageFormatPropertiesEXT(..)
  , PhysicalDeviceImageViewImageFormatInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , RenderPassFragmentDensityMapCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( SurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , SurfaceFullScreenExclusiveInfoEXT(..)
  , SurfaceFullScreenExclusiveWin32InfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( DeviceQueueGlobalPriorityCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  ( HdrMetadataEXT(..)
  , XYColorEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_headless_surface
  ( HeadlessSurfaceCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_host_query_reset
  ( PhysicalDeviceHostQueryResetFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier
  ( DrmFormatModifierPropertiesListEXT(..)
  , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , ImageDrmFormatModifierListCreateInfoEXT(..)
  , ImageDrmFormatModifierPropertiesEXT(..)
  , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , DrmFormatModifierPropertiesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block
  ( DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , WriteDescriptorSetInlineUniformBlockEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_memory_budget
  ( PhysicalDeviceMemoryBudgetPropertiesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_memory_priority
  ( MemoryPriorityAllocateInfoEXT(..)
  , PhysicalDeviceMemoryPriorityFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( MetalSurfaceCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_pci_bus_info
  ( PhysicalDevicePCIBusInfoPropertiesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( PipelineCreationFeedbackCreateInfoEXT(..)
  , PipelineCreationFeedbackEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( MultisamplePropertiesEXT(..)
  , PhysicalDeviceSampleLocationsPropertiesEXT(..)
  , PipelineSampleLocationsStateCreateInfoEXT(..)
  , RenderPassSampleLocationsBeginInfoEXT(..)
  , SampleLocationsInfoEXT(..)
  , AttachmentSampleLocationsEXT(..)
  , SampleLocationEXT(..)
  , SubpassSampleLocationsEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( PhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , SamplerReductionModeCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_scalar_block_layout
  ( PhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_separate_stencil_usage
  ( ImageStencilUsageCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_transform_feedback
  ( PhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , PipelineRasterizationStateStreamCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( ShaderModuleValidationCacheCreateInfoEXT(..)
  , ValidationCacheCreateInfoEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_features
  ( ValidationFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationFlagsEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( PhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , PipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VertexInputBindingDivisorDescriptionEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays
  ( PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  )
import Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( ImagePipeSurfaceCreateInfoFUCHSIA(..)
  )
import Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( PresentFrameTokenGGP(..)
  )
import Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( StreamDescriptorSurfaceCreateInfoGGP(..)
  )
import Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( PresentTimesInfoGOOGLE(..)
  , PastPresentationTimingGOOGLE(..)
  , PresentTimeGOOGLE(..)
  , RefreshCycleDurationGOOGLE(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_8bit_storage
  ( PhysicalDevice8BitStorageFeaturesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( AndroidSurfaceCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( AttachmentDescription2KHR(..)
  , AttachmentReference2KHR(..)
  , RenderPassCreateInfo2KHR(..)
  , SubpassBeginInfoKHR(..)
  , SubpassDependency2KHR(..)
  , SubpassDescription2KHR(..)
  , SubpassEndInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( PhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , SubpassDescriptionDepthStencilResolveKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayModeCreateInfoKHR(..)
  , DisplaySurfaceCreateInfoKHR(..)
  , DisplayModeParametersKHR(..)
  , DisplayModePropertiesKHR(..)
  , DisplayPlaneCapabilitiesKHR(..)
  , DisplayPlanePropertiesKHR(..)
  , DisplayPropertiesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( DisplayPresentInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( PhysicalDeviceDriverPropertiesKHR(..)
  , ConformanceVersionKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( FenceGetFdInfoKHR(..)
  , ImportFenceFdInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( ExportFenceWin32HandleInfoKHR(..)
  , FenceGetWin32HandleInfoKHR(..)
  , ImportFenceWin32HandleInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( ImportMemoryFdInfoKHR(..)
  , MemoryFdPropertiesKHR(..)
  , MemoryGetFdInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( ExportMemoryWin32HandleInfoKHR(..)
  , ImportMemoryWin32HandleInfoKHR(..)
  , MemoryGetWin32HandleInfoKHR(..)
  , MemoryWin32HandlePropertiesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( ImportSemaphoreFdInfoKHR(..)
  , SemaphoreGetFdInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( D3D12FenceSubmitInfoKHR(..)
  , ExportSemaphoreWin32HandleInfoKHR(..)
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , SemaphoreGetWin32HandleInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2
  ( DisplayModeProperties2KHR(..)
  , DisplayPlaneCapabilities2KHR(..)
  , DisplayPlaneInfo2KHR(..)
  , DisplayPlaneProperties2KHR(..)
  , DisplayProperties2KHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  , SurfaceCapabilities2KHR(..)
  , SurfaceFormat2KHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( ImageFormatListCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  ( PresentRegionsKHR(..)
  , PresentRegionKHR(..)
  , RectLayerKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( PhysicalDevicePushDescriptorPropertiesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_shader_atomic_int64
  ( PhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_shader_float16_int8
  ( PhysicalDeviceFloat16Int8FeaturesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls
  ( PhysicalDeviceFloatControlsPropertiesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( SharedPresentSurfaceCapabilitiesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface_protected_capabilities
  ( SurfaceProtectedCapabilitiesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_swapchain
  ( AcquireNextImageInfoKHR(..)
  , BindImageMemorySwapchainInfoKHR(..)
  , DeviceGroupPresentCapabilitiesKHR(..)
  , DeviceGroupPresentInfoKHR(..)
  , DeviceGroupSwapchainCreateInfoKHR(..)
  , ImageSwapchainCreateInfoKHR(..)
  , PresentInfoKHR(..)
  , SwapchainCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_vulkan_memory_model
  ( PhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( WaylandSurfaceCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( Win32SurfaceCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( XcbSurfaceCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( XlibSurfaceCreateInfoKHR(..)
  )
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( IOSSurfaceCreateInfoMVK(..)
  )
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateInfoMVK(..)
  )
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( ViSurfaceCreateInfoNN(..)
  )
import Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( CmdProcessCommandsInfoNVX(..)
  , CmdReserveSpaceForCommandsInfoNVX(..)
  , DeviceGeneratedCommandsFeaturesNVX(..)
  , DeviceGeneratedCommandsLimitsNVX(..)
  , IndirectCommandsLayoutCreateInfoNVX(..)
  , ObjectTableCreateInfoNVX(..)
  , IndirectCommandsLayoutTokenNVX(..)
  , IndirectCommandsTokenNVX(..)
  , ObjectTableDescriptorSetEntryNVX(..)
  , ObjectTableEntryNVX(..)
  , ObjectTableIndexBufferEntryNVX(..)
  , ObjectTablePipelineEntryNVX(..)
  , ObjectTablePushConstantEntryNVX(..)
  , ObjectTableVertexBufferEntryNVX(..)
  )
import Graphics.Vulkan.Extensions.VK_NVX_image_view_handle
  ( ImageViewHandleInfoNVX(..)
  )
import Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( PipelineViewportWScalingStateCreateInfoNV(..)
  , ViewportWScalingNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives
  ( PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix
  ( CooperativeMatrixPropertiesNV(..)
  , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image
  ( PhysicalDeviceCornerSampledImageFeaturesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( DedicatedAllocationBufferCreateInfoNV(..)
  , DedicatedAllocationImageCreateInfoNV(..)
  , DedicatedAllocationMemoryAllocateInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints
  ( CheckpointDataNV(..)
  , QueueFamilyCheckpointPropertiesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory
  ( ExportMemoryAllocateInfoNV(..)
  , ExternalMemoryImageCreateInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalImageFormatPropertiesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( ExportMemoryWin32HandleInfoNV(..)
  , ImportMemoryWin32HandleInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric
  ( PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( PipelineCoverageModulationStateCreateInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_mesh_shader
  ( PhysicalDeviceMeshShaderFeaturesNV(..)
  , PhysicalDeviceMeshShaderPropertiesNV(..)
  , DrawMeshTasksIndirectCommandNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_ray_tracing
  ( AccelerationStructureCreateInfoNV(..)
  , AccelerationStructureInfoNV(..)
  , AccelerationStructureMemoryRequirementsInfoNV(..)
  , BindAccelerationStructureMemoryInfoNV(..)
  , GeometryAABBNV(..)
  , GeometryNV(..)
  , GeometryTrianglesNV(..)
  , PhysicalDeviceRayTracingPropertiesNV(..)
  , RayTracingPipelineCreateInfoNV(..)
  , RayTracingShaderGroupCreateInfoNV(..)
  , WriteDescriptorSetAccelerationStructureNV(..)
  , GeometryDataNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test
  ( PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive
  ( PhysicalDeviceExclusiveScissorFeaturesNV(..)
  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint
  ( PhysicalDeviceShaderImageFootprintFeaturesNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( PhysicalDeviceShadingRateImageFeaturesNV(..)
  , PhysicalDeviceShadingRateImagePropertiesNV(..)
  , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , PipelineViewportShadingRateImageStateCreateInfoNV(..)
  , CoarseSampleLocationNV(..)
  , CoarseSampleOrderCustomNV(..)
  , ShadingRatePaletteNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateInfoNV(..)
  , ViewportSwizzleNV(..)
  )
import Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoNV(..)
  )
import Graphics.Vulkan.Marshal.Utils
  ( pokeFixedLengthByteString
  , pokeFixedLengthNullTerminatedByteString
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( withSomeVkStruct
  )


-- | A class for types which can be marshalled into a C style
-- structure.
class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
  -- | Allocates a C type structure and all dependencies and passes
  -- it to a continuation. The space is deallocated when this
  -- continuation returns and the C type structure must not be
  -- returned out of it.
  withCStruct :: marshalled -> (Ptr c -> IO a) -> IO a
  default withCStruct :: Storable c => marshalled -> (Ptr c -> IO a) -> IO a
  withCStruct x f = alloca $ \p -> pokeCStruct p x (f p)

  -- | Write a C type struct into some existing memory and run a
  -- continuation. The pointed to structure is not necessarily valid
  -- outside the continuation as additional allocations may have been
  -- made.
  pokeCStruct :: Ptr c -> marshalled -> IO a -> IO a


instance ToCStruct Offset2D VkOffset2D where
  pokeCStruct :: Ptr VkOffset2D -> Offset2D -> IO a -> IO a
  pokeCStruct p Offset2D{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Int32) x
      poke (p `plusPtr` 4 :: Ptr Int32) y


instance ToCStruct Offset3D VkOffset3D where
  pokeCStruct :: Ptr VkOffset3D -> Offset3D -> IO a -> IO a
  pokeCStruct p Offset3D{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Int32) x
      poke (p `plusPtr` 4 :: Ptr Int32) y
      poke (p `plusPtr` 8 :: Ptr Int32) z


instance ToCStruct Extent2D VkExtent2D where
  pokeCStruct :: Ptr VkExtent2D -> Extent2D -> IO a -> IO a
  pokeCStruct p Extent2D{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) width
      poke (p `plusPtr` 4 :: Ptr Word32) height


instance ToCStruct Extent3D VkExtent3D where
  pokeCStruct :: Ptr VkExtent3D -> Extent3D -> IO a -> IO a
  pokeCStruct p Extent3D{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) width
      poke (p `plusPtr` 4 :: Ptr Word32) height
      poke (p `plusPtr` 8 :: Ptr Word32) depth


instance ToCStruct Viewport VkViewport where
  pokeCStruct :: Ptr VkViewport -> Viewport -> IO a -> IO a
  pokeCStruct p Viewport{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr CFloat) (CFloat x :: CFloat)
      poke (p `plusPtr` 4 :: Ptr CFloat) (CFloat y :: CFloat)
      poke (p `plusPtr` 8 :: Ptr CFloat) (CFloat width :: CFloat)
      poke (p `plusPtr` 12 :: Ptr CFloat) (CFloat height :: CFloat)
      poke (p `plusPtr` 16 :: Ptr CFloat) (CFloat minDepth :: CFloat)
      poke (p `plusPtr` 20 :: Ptr CFloat) (CFloat maxDepth :: CFloat)


instance ToCStruct Rect2D VkRect2D where
  pokeCStruct :: Ptr VkRect2D -> Rect2D -> IO a -> IO a
  pokeCStruct p Rect2D{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) offset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 8) extent . ($ ())
    


instance ToCStruct ClearRect VkClearRect where
  pokeCStruct :: Ptr VkClearRect -> ClearRect -> IO a -> IO a
  pokeCStruct p ClearRect{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) rect . ($ ())
    liftIO $ do
      poke (p `plusPtr` 16 :: Ptr Word32) baseArrayLayer
      poke (p `plusPtr` 20 :: Ptr Word32) layerCount


instance ToCStruct ComponentMapping VkComponentMapping where
  pokeCStruct :: Ptr VkComponentMapping -> ComponentMapping -> IO a -> IO a
  pokeCStruct p ComponentMapping{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkComponentSwizzle) r
      poke (p `plusPtr` 4 :: Ptr VkComponentSwizzle) g
      poke (p `plusPtr` 8 :: Ptr VkComponentSwizzle) b
      poke (p `plusPtr` 12 :: Ptr VkComponentSwizzle) a


instance ToCStruct PhysicalDeviceProperties VkPhysicalDeviceProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceProperties -> PhysicalDeviceProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceProperties{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 296) limits . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 800) sparseProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) apiVersion
      poke (p `plusPtr` 4 :: Ptr Word32) driverVersion
      poke (p `plusPtr` 8 :: Ptr Word32) vendorID
      poke (p `plusPtr` 12 :: Ptr Word32) deviceID
      poke (p `plusPtr` 16 :: Ptr VkPhysicalDeviceType) deviceType
      pokeFixedLengthNullTerminatedByteString VK_MAX_PHYSICAL_DEVICE_NAME_SIZE (p `plusPtr` 20) deviceName
      pokeFixedLengthByteString VK_UUID_SIZE (p `plusPtr` 276) pipelineCacheUUID


instance ToCStruct ExtensionProperties VkExtensionProperties where
  pokeCStruct :: Ptr VkExtensionProperties -> ExtensionProperties -> IO a -> IO a
  pokeCStruct p ExtensionProperties{..} = (. const) . runContT $    
    liftIO $ do
      pokeFixedLengthNullTerminatedByteString VK_MAX_EXTENSION_NAME_SIZE (p `plusPtr` 0) extensionName
      poke (p `plusPtr` 256 :: Ptr Word32) specVersion


instance ToCStruct LayerProperties VkLayerProperties where
  pokeCStruct :: Ptr VkLayerProperties -> LayerProperties -> IO a -> IO a
  pokeCStruct p LayerProperties{..} = (. const) . runContT $    
    liftIO $ do
      pokeFixedLengthNullTerminatedByteString VK_MAX_EXTENSION_NAME_SIZE (p `plusPtr` 0) layerName
      poke (p `plusPtr` 256 :: Ptr Word32) specVersion
      poke (p `plusPtr` 260 :: Ptr Word32) implementationVersion
      pokeFixedLengthNullTerminatedByteString VK_MAX_DESCRIPTION_SIZE (p `plusPtr` 264) description


instance ToCStruct ApplicationInfo VkApplicationInfo where
  pokeCStruct :: Ptr VkApplicationInfo -> ApplicationInfo -> IO a -> IO a
  pokeCStruct p ApplicationInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pApplicationName <- ContT $ maybeWith useAsCString applicationName
    pEngineName <- ContT $ maybeWith useAsCString engineName
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_APPLICATION_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr CChar)) pApplicationName
      poke (p `plusPtr` 24 :: Ptr Word32) applicationVersion
      poke (p `plusPtr` 32 :: Ptr (Ptr CChar)) pEngineName
      poke (p `plusPtr` 40 :: Ptr Word32) engineVersion
      poke (p `plusPtr` 44 :: Ptr Word32) apiVersion


instance ToCStruct AllocationCallbacks VkAllocationCallbacks where
  pokeCStruct :: Ptr VkAllocationCallbacks -> AllocationCallbacks -> IO a -> IO a
  pokeCStruct p AllocationCallbacks{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr (Ptr ())) (userData :: Ptr ())
      poke (p `plusPtr` 8 :: Ptr PFN_vkAllocationFunction) pfnAllocation
      poke (p `plusPtr` 16 :: Ptr PFN_vkReallocationFunction) pfnReallocation
      poke (p `plusPtr` 24 :: Ptr PFN_vkFreeFunction) pfnFree
      poke (p `plusPtr` 32 :: Ptr PFN_vkInternalAllocationNotification) pfnInternalAllocation
      poke (p `plusPtr` 40 :: Ptr PFN_vkInternalFreeNotification) pfnInternalFree


instance ToCStruct DeviceQueueCreateInfo VkDeviceQueueCreateInfo where
  pokeCStruct :: Ptr VkDeviceQueueCreateInfo -> DeviceQueueCreateInfo -> IO a -> IO a
  pokeCStruct p DeviceQueueCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pQueuePriorities <- ContT $ allocaArray @CFloat (Data.Vector.length queuePriorities)
    liftIO $ do
      Data.Vector.imapM_ (\i -> pokeElemOff pQueuePriorities i . CFloat) queuePriorities
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceQueueCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) queueFamilyIndex
      poke (p `plusPtr` 24 :: Ptr Word32) (fromIntegral $ Data.Vector.length queuePriorities :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr CFloat)) pQueuePriorities


instance ToCStruct DeviceCreateInfo VkDeviceCreateInfo where
  pokeCStruct :: Ptr VkDeviceCreateInfo -> DeviceCreateInfo -> IO a -> IO a
  pokeCStruct p DeviceCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pQueueCreateInfos <- ContT $ allocaArray @VkDeviceQueueCreateInfo (Data.Vector.length queueCreateInfos)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pQueueCreateInfos `advancePtr` i) s . ($ ())) queueCreateInfos
    ppEnabledLayerNames <- ContT $ allocaArray @(Ptr CChar) (Data.Vector.length enabledLayerNames)
    Data.Vector.imapM_ (\i bs -> ContT (useAsCString bs) >>= (liftIO . pokeElemOff ppEnabledLayerNames i)) enabledLayerNames
    ppEnabledExtensionNames <- ContT $ allocaArray @(Ptr CChar) (Data.Vector.length enabledExtensionNames)
    Data.Vector.imapM_ (\i bs -> ContT (useAsCString bs) >>= (liftIO . pokeElemOff ppEnabledExtensionNames i)) enabledExtensionNames
    pEnabledFeatures <- ContT $ maybeWith withCStruct enabledFeatures
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length queueCreateInfos :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDeviceQueueCreateInfo)) pQueueCreateInfos
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length enabledLayerNames :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar))) ppEnabledLayerNames
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length enabledExtensionNames :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar))) ppEnabledExtensionNames
      poke (p `plusPtr` 64 :: Ptr (Ptr VkPhysicalDeviceFeatures)) pEnabledFeatures


instance ToCStruct InstanceCreateInfo VkInstanceCreateInfo where
  pokeCStruct :: Ptr VkInstanceCreateInfo -> InstanceCreateInfo -> IO a -> IO a
  pokeCStruct p InstanceCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pApplicationInfo <- ContT $ maybeWith withCStruct applicationInfo
    ppEnabledLayerNames <- ContT $ allocaArray @(Ptr CChar) (Data.Vector.length enabledLayerNames)
    Data.Vector.imapM_ (\i bs -> ContT (useAsCString bs) >>= (liftIO . pokeElemOff ppEnabledLayerNames i)) enabledLayerNames
    ppEnabledExtensionNames <- ContT $ allocaArray @(Ptr CChar) (Data.Vector.length enabledExtensionNames)
    Data.Vector.imapM_ (\i bs -> ContT (useAsCString bs) >>= (liftIO . pokeElemOff ppEnabledExtensionNames i)) enabledExtensionNames
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_INSTANCE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkInstanceCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr VkApplicationInfo)) pApplicationInfo
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length enabledLayerNames :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr (Ptr CChar))) ppEnabledLayerNames
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length enabledExtensionNames :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr (Ptr CChar))) ppEnabledExtensionNames


instance ToCStruct QueueFamilyProperties VkQueueFamilyProperties where
  pokeCStruct :: Ptr VkQueueFamilyProperties -> QueueFamilyProperties -> IO a -> IO a
  pokeCStruct p QueueFamilyProperties{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 12) minImageTransferGranularity . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkQueueFlags) queueFlags
      poke (p `plusPtr` 4 :: Ptr Word32) queueCount
      poke (p `plusPtr` 8 :: Ptr Word32) timestampValidBits


instance ToCStruct PhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceMemoryProperties -> PhysicalDeviceMemoryProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMemoryProperties{..} = (. const) . runContT $ do
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct ((p `plusPtr` 4) `advancePtr` i) s . ($ ())) (Data.Vector.take VK_MAX_MEMORY_TYPES memoryTypes)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct ((p `plusPtr` 264) `advancePtr` i) s . ($ ())) (Data.Vector.take VK_MAX_MEMORY_HEAPS memoryHeaps)
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) (fromIntegral $ Data.Vector.length memoryTypes :: Word32)
      poke (p `plusPtr` 260 :: Ptr Word32) (fromIntegral $ Data.Vector.length memoryHeaps :: Word32)


instance ToCStruct MemoryAllocateInfo VkMemoryAllocateInfo where
  pokeCStruct :: Ptr VkMemoryAllocateInfo -> MemoryAllocateInfo -> IO a -> IO a
  pokeCStruct p MemoryAllocateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) allocationSize
      poke (p `plusPtr` 24 :: Ptr Word32) memoryTypeIndex


instance ToCStruct MemoryRequirements VkMemoryRequirements where
  pokeCStruct :: Ptr VkMemoryRequirements -> MemoryRequirements -> IO a -> IO a
  pokeCStruct p MemoryRequirements{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDeviceSize) size
      poke (p `plusPtr` 8 :: Ptr VkDeviceSize) alignment
      poke (p `plusPtr` 16 :: Ptr Word32) memoryTypeBits


instance ToCStruct SparseImageFormatProperties VkSparseImageFormatProperties where
  pokeCStruct :: Ptr VkSparseImageFormatProperties -> SparseImageFormatProperties -> IO a -> IO a
  pokeCStruct p SparseImageFormatProperties{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 4) imageGranularity . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImageAspectFlags) aspectMask
      poke (p `plusPtr` 16 :: Ptr VkSparseImageFormatFlags) flags


instance ToCStruct SparseImageMemoryRequirements VkSparseImageMemoryRequirements where
  pokeCStruct :: Ptr VkSparseImageMemoryRequirements -> SparseImageMemoryRequirements -> IO a -> IO a
  pokeCStruct p SparseImageMemoryRequirements{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) formatProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 20 :: Ptr Word32) imageMipTailFirstLod
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) imageMipTailSize
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) imageMipTailOffset
      poke (p `plusPtr` 40 :: Ptr VkDeviceSize) imageMipTailStride


instance ToCStruct MemoryType VkMemoryType where
  pokeCStruct :: Ptr VkMemoryType -> MemoryType -> IO a -> IO a
  pokeCStruct p MemoryType{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkMemoryPropertyFlags) propertyFlags
      poke (p `plusPtr` 4 :: Ptr Word32) heapIndex


instance ToCStruct MemoryHeap VkMemoryHeap where
  pokeCStruct :: Ptr VkMemoryHeap -> MemoryHeap -> IO a -> IO a
  pokeCStruct p MemoryHeap{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDeviceSize) size
      poke (p `plusPtr` 8 :: Ptr VkMemoryHeapFlags) flags


instance ToCStruct MappedMemoryRange VkMappedMemoryRange where
  pokeCStruct :: Ptr VkMappedMemoryRange -> MappedMemoryRange -> IO a -> IO a
  pokeCStruct p MappedMemoryRange{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MAPPED_MEMORY_RANGE :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) offset
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) size


instance ToCStruct FormatProperties VkFormatProperties where
  pokeCStruct :: Ptr VkFormatProperties -> FormatProperties -> IO a -> IO a
  pokeCStruct p FormatProperties{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkFormatFeatureFlags) linearTilingFeatures
      poke (p `plusPtr` 4 :: Ptr VkFormatFeatureFlags) optimalTilingFeatures
      poke (p `plusPtr` 8 :: Ptr VkFormatFeatureFlags) bufferFeatures


instance ToCStruct ImageFormatProperties VkImageFormatProperties where
  pokeCStruct :: Ptr VkImageFormatProperties -> ImageFormatProperties -> IO a -> IO a
  pokeCStruct p ImageFormatProperties{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) maxExtent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 12 :: Ptr Word32) maxMipLevels
      poke (p `plusPtr` 16 :: Ptr Word32) maxArrayLayers
      poke (p `plusPtr` 20 :: Ptr VkSampleCountFlags) sampleCounts
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) maxResourceSize


instance ToCStruct DescriptorBufferInfo VkDescriptorBufferInfo where
  pokeCStruct :: Ptr VkDescriptorBufferInfo -> DescriptorBufferInfo -> IO a -> IO a
  pokeCStruct p DescriptorBufferInfo{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 8 :: Ptr VkDeviceSize) offset
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) range


instance ToCStruct DescriptorImageInfo VkDescriptorImageInfo where
  pokeCStruct :: Ptr VkDescriptorImageInfo -> DescriptorImageInfo -> IO a -> IO a
  pokeCStruct p DescriptorImageInfo{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkSampler) sampler
      poke (p `plusPtr` 8 :: Ptr VkImageView) imageView
      poke (p `plusPtr` 16 :: Ptr VkImageLayout) imageLayout


instance ToCStruct WriteDescriptorSet VkWriteDescriptorSet where
  pokeCStruct :: Ptr VkWriteDescriptorSet -> WriteDescriptorSet -> IO a -> IO a
  pokeCStruct p WriteDescriptorSet{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pImageInfo <- ContT $ allocaArray @VkDescriptorImageInfo (Data.Vector.length imageInfo)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pImageInfo `advancePtr` i) s . ($ ())) imageInfo
    pBufferInfo <- ContT $ allocaArray @VkDescriptorBufferInfo (Data.Vector.length bufferInfo)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pBufferInfo `advancePtr` i) s . ($ ())) bufferInfo
    pTexelBufferView <- ContT $ allocaArray @VkBufferView (Data.Vector.length texelBufferView)
    liftIO $ do
      descriptorCount <- let l = Data.Vector.length imageInfo in if l == Data.Vector.length bufferInfo && l == Data.Vector.length texelBufferView then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "imageInfo, bufferInfo and texelBufferView must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pTexelBufferView) texelBufferView
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDescriptorSet) dstSet
      poke (p `plusPtr` 24 :: Ptr Word32) dstBinding
      poke (p `plusPtr` 28 :: Ptr Word32) dstArrayElement
      poke (p `plusPtr` 32 :: Ptr Word32) descriptorCount
      poke (p `plusPtr` 36 :: Ptr VkDescriptorType) descriptorType
      poke (p `plusPtr` 40 :: Ptr (Ptr VkDescriptorImageInfo)) pImageInfo
      poke (p `plusPtr` 48 :: Ptr (Ptr VkDescriptorBufferInfo)) pBufferInfo
      poke (p `plusPtr` 56 :: Ptr (Ptr VkBufferView)) pTexelBufferView


instance ToCStruct CopyDescriptorSet VkCopyDescriptorSet where
  pokeCStruct :: Ptr VkCopyDescriptorSet -> CopyDescriptorSet -> IO a -> IO a
  pokeCStruct p CopyDescriptorSet{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COPY_DESCRIPTOR_SET :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDescriptorSet) srcSet
      poke (p `plusPtr` 24 :: Ptr Word32) srcBinding
      poke (p `plusPtr` 28 :: Ptr Word32) srcArrayElement
      poke (p `plusPtr` 32 :: Ptr VkDescriptorSet) dstSet
      poke (p `plusPtr` 40 :: Ptr Word32) dstBinding
      poke (p `plusPtr` 44 :: Ptr Word32) dstArrayElement
      poke (p `plusPtr` 48 :: Ptr Word32) descriptorCount


instance ToCStruct BufferCreateInfo VkBufferCreateInfo where
  pokeCStruct :: Ptr VkBufferCreateInfo -> BufferCreateInfo -> IO a -> IO a
  pokeCStruct p BufferCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pQueueFamilyIndices <- ContT $ allocaArray @Word32 (Data.Vector.length queueFamilyIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pQueueFamilyIndices) queueFamilyIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BUFFER_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBufferCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) size
      poke (p `plusPtr` 32 :: Ptr VkBufferUsageFlags) usage
      poke (p `plusPtr` 36 :: Ptr VkSharingMode) sharingMode
      poke (p `plusPtr` 40 :: Ptr Word32) (fromIntegral $ Data.Vector.length queueFamilyIndices :: Word32)
      poke (p `plusPtr` 48 :: Ptr (Ptr Word32)) pQueueFamilyIndices


instance ToCStruct BufferViewCreateInfo VkBufferViewCreateInfo where
  pokeCStruct :: Ptr VkBufferViewCreateInfo -> BufferViewCreateInfo -> IO a -> IO a
  pokeCStruct p BufferViewCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBufferViewCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 32 :: Ptr VkFormat) format
      poke (p `plusPtr` 40 :: Ptr VkDeviceSize) offset
      poke (p `plusPtr` 48 :: Ptr VkDeviceSize) range


instance ToCStruct ImageSubresource VkImageSubresource where
  pokeCStruct :: Ptr VkImageSubresource -> ImageSubresource -> IO a -> IO a
  pokeCStruct p ImageSubresource{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImageAspectFlags) aspectMask
      poke (p `plusPtr` 4 :: Ptr Word32) mipLevel
      poke (p `plusPtr` 8 :: Ptr Word32) arrayLayer


instance ToCStruct ImageSubresourceLayers VkImageSubresourceLayers where
  pokeCStruct :: Ptr VkImageSubresourceLayers -> ImageSubresourceLayers -> IO a -> IO a
  pokeCStruct p ImageSubresourceLayers{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImageAspectFlags) aspectMask
      poke (p `plusPtr` 4 :: Ptr Word32) mipLevel
      poke (p `plusPtr` 8 :: Ptr Word32) baseArrayLayer
      poke (p `plusPtr` 12 :: Ptr Word32) layerCount


instance ToCStruct ImageSubresourceRange VkImageSubresourceRange where
  pokeCStruct :: Ptr VkImageSubresourceRange -> ImageSubresourceRange -> IO a -> IO a
  pokeCStruct p ImageSubresourceRange{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImageAspectFlags) aspectMask
      poke (p `plusPtr` 4 :: Ptr Word32) baseMipLevel
      poke (p `plusPtr` 8 :: Ptr Word32) levelCount
      poke (p `plusPtr` 12 :: Ptr Word32) baseArrayLayer
      poke (p `plusPtr` 16 :: Ptr Word32) layerCount


instance ToCStruct MemoryBarrier VkMemoryBarrier where
  pokeCStruct :: Ptr VkMemoryBarrier -> MemoryBarrier -> IO a -> IO a
  pokeCStruct p MemoryBarrier{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_BARRIER :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAccessFlags) srcAccessMask
      poke (p `plusPtr` 20 :: Ptr VkAccessFlags) dstAccessMask


instance ToCStruct BufferMemoryBarrier VkBufferMemoryBarrier where
  pokeCStruct :: Ptr VkBufferMemoryBarrier -> BufferMemoryBarrier -> IO a -> IO a
  pokeCStruct p BufferMemoryBarrier{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAccessFlags) srcAccessMask
      poke (p `plusPtr` 20 :: Ptr VkAccessFlags) dstAccessMask
      poke (p `plusPtr` 24 :: Ptr Word32) srcQueueFamilyIndex
      poke (p `plusPtr` 28 :: Ptr Word32) dstQueueFamilyIndex
      poke (p `plusPtr` 32 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 40 :: Ptr VkDeviceSize) offset
      poke (p `plusPtr` 48 :: Ptr VkDeviceSize) size


instance ToCStruct ImageMemoryBarrier VkImageMemoryBarrier where
  pokeCStruct :: Ptr VkImageMemoryBarrier -> ImageMemoryBarrier -> IO a -> IO a
  pokeCStruct p ImageMemoryBarrier{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 48) subresourceRange . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAccessFlags) srcAccessMask
      poke (p `plusPtr` 20 :: Ptr VkAccessFlags) dstAccessMask
      poke (p `plusPtr` 24 :: Ptr VkImageLayout) oldLayout
      poke (p `plusPtr` 28 :: Ptr VkImageLayout) newLayout
      poke (p `plusPtr` 32 :: Ptr Word32) srcQueueFamilyIndex
      poke (p `plusPtr` 36 :: Ptr Word32) dstQueueFamilyIndex
      poke (p `plusPtr` 40 :: Ptr VkImage) image


instance ToCStruct ImageCreateInfo VkImageCreateInfo where
  pokeCStruct :: Ptr VkImageCreateInfo -> ImageCreateInfo -> IO a -> IO a
  pokeCStruct p ImageCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 28) extent . ($ ())
    pQueueFamilyIndices <- ContT $ allocaArray @Word32 (Data.Vector.length queueFamilyIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pQueueFamilyIndices) queueFamilyIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkImageType) imageType
      poke (p `plusPtr` 24 :: Ptr VkFormat) format
      poke (p `plusPtr` 40 :: Ptr Word32) mipLevels
      poke (p `plusPtr` 44 :: Ptr Word32) arrayLayers
      poke (p `plusPtr` 48 :: Ptr VkSampleCountFlagBits) samples
      poke (p `plusPtr` 52 :: Ptr VkImageTiling) tiling
      poke (p `plusPtr` 56 :: Ptr VkImageUsageFlags) usage
      poke (p `plusPtr` 60 :: Ptr VkSharingMode) sharingMode
      poke (p `plusPtr` 64 :: Ptr Word32) (fromIntegral $ Data.Vector.length queueFamilyIndices :: Word32)
      poke (p `plusPtr` 72 :: Ptr (Ptr Word32)) pQueueFamilyIndices
      poke (p `plusPtr` 80 :: Ptr VkImageLayout) initialLayout


instance ToCStruct SubresourceLayout VkSubresourceLayout where
  pokeCStruct :: Ptr VkSubresourceLayout -> SubresourceLayout -> IO a -> IO a
  pokeCStruct p SubresourceLayout{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDeviceSize) offset
      poke (p `plusPtr` 8 :: Ptr VkDeviceSize) size
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) rowPitch
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) arrayPitch
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) depthPitch


instance ToCStruct ImageViewCreateInfo VkImageViewCreateInfo where
  pokeCStruct :: Ptr VkImageViewCreateInfo -> ImageViewCreateInfo -> IO a -> IO a
  pokeCStruct p ImageViewCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 40) components . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 56) subresourceRange . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageViewCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr VkImage) image
      poke (p `plusPtr` 32 :: Ptr VkImageViewType) viewType
      poke (p `plusPtr` 36 :: Ptr VkFormat) format


instance ToCStruct BufferCopy VkBufferCopy where
  pokeCStruct :: Ptr VkBufferCopy -> BufferCopy -> IO a -> IO a
  pokeCStruct p BufferCopy{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDeviceSize) srcOffset
      poke (p `plusPtr` 8 :: Ptr VkDeviceSize) dstOffset
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) size


instance ToCStruct SparseMemoryBind VkSparseMemoryBind where
  pokeCStruct :: Ptr VkSparseMemoryBind -> SparseMemoryBind -> IO a -> IO a
  pokeCStruct p SparseMemoryBind{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDeviceSize) resourceOffset
      poke (p `plusPtr` 8 :: Ptr VkDeviceSize) size
      poke (p `plusPtr` 16 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) memoryOffset
      poke (p `plusPtr` 32 :: Ptr VkSparseMemoryBindFlags) flags


instance ToCStruct SparseImageMemoryBind VkSparseImageMemoryBind where
  pokeCStruct :: Ptr VkSparseImageMemoryBind -> SparseImageMemoryBind -> IO a -> IO a
  pokeCStruct p SparseImageMemoryBind{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) subresource . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 12) offset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 24) extent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 40 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 48 :: Ptr VkDeviceSize) memoryOffset
      poke (p `plusPtr` 56 :: Ptr VkSparseMemoryBindFlags) flags


instance ToCStruct SparseBufferMemoryBindInfo VkSparseBufferMemoryBindInfo where
  pokeCStruct :: Ptr VkSparseBufferMemoryBindInfo -> SparseBufferMemoryBindInfo -> IO a -> IO a
  pokeCStruct p SparseBufferMemoryBindInfo{..} = (. const) . runContT $ do
    pBinds <- ContT $ allocaArray @VkSparseMemoryBind (Data.Vector.length binds)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pBinds `advancePtr` i) s . ($ ())) binds
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 8 :: Ptr Word32) (fromIntegral $ Data.Vector.length binds :: Word32)
      poke (p `plusPtr` 16 :: Ptr (Ptr VkSparseMemoryBind)) pBinds


instance ToCStruct SparseImageOpaqueMemoryBindInfo VkSparseImageOpaqueMemoryBindInfo where
  pokeCStruct :: Ptr VkSparseImageOpaqueMemoryBindInfo -> SparseImageOpaqueMemoryBindInfo -> IO a -> IO a
  pokeCStruct p SparseImageOpaqueMemoryBindInfo{..} = (. const) . runContT $ do
    pBinds <- ContT $ allocaArray @VkSparseMemoryBind (Data.Vector.length binds)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pBinds `advancePtr` i) s . ($ ())) binds
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImage) image
      poke (p `plusPtr` 8 :: Ptr Word32) (fromIntegral $ Data.Vector.length binds :: Word32)
      poke (p `plusPtr` 16 :: Ptr (Ptr VkSparseMemoryBind)) pBinds


instance ToCStruct SparseImageMemoryBindInfo VkSparseImageMemoryBindInfo where
  pokeCStruct :: Ptr VkSparseImageMemoryBindInfo -> SparseImageMemoryBindInfo -> IO a -> IO a
  pokeCStruct p SparseImageMemoryBindInfo{..} = (. const) . runContT $ do
    pBinds <- ContT $ allocaArray @VkSparseImageMemoryBind (Data.Vector.length binds)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pBinds `advancePtr` i) s . ($ ())) binds
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImage) image
      poke (p `plusPtr` 8 :: Ptr Word32) (fromIntegral $ Data.Vector.length binds :: Word32)
      poke (p `plusPtr` 16 :: Ptr (Ptr VkSparseImageMemoryBind)) pBinds


instance ToCStruct BindSparseInfo VkBindSparseInfo where
  pokeCStruct :: Ptr VkBindSparseInfo -> BindSparseInfo -> IO a -> IO a
  pokeCStruct p BindSparseInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pWaitSemaphores <- ContT $ allocaArray @VkSemaphore (Data.Vector.length waitSemaphores)
    pBufferBinds <- ContT $ allocaArray @VkSparseBufferMemoryBindInfo (Data.Vector.length bufferBinds)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pBufferBinds `advancePtr` i) s . ($ ())) bufferBinds
    pImageOpaqueBinds <- ContT $ allocaArray @VkSparseImageOpaqueMemoryBindInfo (Data.Vector.length imageOpaqueBinds)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pImageOpaqueBinds `advancePtr` i) s . ($ ())) imageOpaqueBinds
    pImageBinds <- ContT $ allocaArray @VkSparseImageMemoryBindInfo (Data.Vector.length imageBinds)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pImageBinds `advancePtr` i) s . ($ ())) imageBinds
    pSignalSemaphores <- ContT $ allocaArray @VkSemaphore (Data.Vector.length signalSemaphores)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pWaitSemaphores) waitSemaphores
      Data.Vector.imapM_ (pokeElemOff pSignalSemaphores) signalSemaphores
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_SPARSE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length waitSemaphores :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkSemaphore)) pWaitSemaphores
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length bufferBinds :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkSparseBufferMemoryBindInfo)) pBufferBinds
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length imageOpaqueBinds :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr VkSparseImageOpaqueMemoryBindInfo)) pImageOpaqueBinds
      poke (p `plusPtr` 64 :: Ptr Word32) (fromIntegral $ Data.Vector.length imageBinds :: Word32)
      poke (p `plusPtr` 72 :: Ptr (Ptr VkSparseImageMemoryBindInfo)) pImageBinds
      poke (p `plusPtr` 80 :: Ptr Word32) (fromIntegral $ Data.Vector.length signalSemaphores :: Word32)
      poke (p `plusPtr` 88 :: Ptr (Ptr VkSemaphore)) pSignalSemaphores


instance ToCStruct ImageCopy VkImageCopy where
  pokeCStruct :: Ptr VkImageCopy -> ImageCopy -> IO a -> IO a
  pokeCStruct p ImageCopy{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) srcSubresource . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 16) srcOffset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 28) dstSubresource . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 44) dstOffset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 56) extent . ($ ())
    


instance ToCStruct ImageBlit VkImageBlit where
  pokeCStruct :: Ptr VkImageBlit -> ImageBlit -> IO a -> IO a
  pokeCStruct p ImageBlit{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) srcSubresource . ($ ())
    case srcOffsets of
      (srcOffsets1, srcOffsets2) -> do
        ContT $ pokeCStruct ((p `plusPtr` 16) `advancePtr` 0) srcOffsets1 . ($ ())
        ContT $ pokeCStruct ((p `plusPtr` 16) `advancePtr` 1) srcOffsets2 . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 40) dstSubresource . ($ ())
    case dstOffsets of
      (dstOffsets1, dstOffsets2) -> do
        ContT $ pokeCStruct ((p `plusPtr` 56) `advancePtr` 0) dstOffsets1 . ($ ())
        ContT $ pokeCStruct ((p `plusPtr` 56) `advancePtr` 1) dstOffsets2 . ($ ())
    


instance ToCStruct BufferImageCopy VkBufferImageCopy where
  pokeCStruct :: Ptr VkBufferImageCopy -> BufferImageCopy -> IO a -> IO a
  pokeCStruct p BufferImageCopy{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 16) imageSubresource . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 32) imageOffset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 44) imageExtent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDeviceSize) bufferOffset
      poke (p `plusPtr` 8 :: Ptr Word32) bufferRowLength
      poke (p `plusPtr` 12 :: Ptr Word32) bufferImageHeight


instance ToCStruct ImageResolve VkImageResolve where
  pokeCStruct :: Ptr VkImageResolve -> ImageResolve -> IO a -> IO a
  pokeCStruct p ImageResolve{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) srcSubresource . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 16) srcOffset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 28) dstSubresource . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 44) dstOffset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 56) extent . ($ ())
    


instance ToCStruct ShaderModuleCreateInfo VkShaderModuleCreateInfo where
  pokeCStruct :: Ptr VkShaderModuleCreateInfo -> ShaderModuleCreateInfo -> IO a -> IO a
  pokeCStruct p ShaderModuleCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pCode <- ContT $ (unsafeUseAsCString code) . (. coerce @CString @(Ptr Word32))
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkShaderModuleCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr CSize) (fromIntegral $ Data.ByteString.length code :: CSize)
      poke (p `plusPtr` 32 :: Ptr (Ptr Word32)) pCode


instance ToCStruct DescriptorSetLayoutBinding VkDescriptorSetLayoutBinding where
  pokeCStruct :: Ptr VkDescriptorSetLayoutBinding -> DescriptorSetLayoutBinding -> IO a -> IO a
  pokeCStruct p DescriptorSetLayoutBinding{..} = (. const) . runContT $ do
    pImmutableSamplers <- ContT $ either (const ($ nullPtr)) (allocaArray @VkSampler . Data.Vector.length) immutableSamplers
    liftIO $ do
      either (const (pure ())) (Data.Vector.imapM_ (pokeElemOff pImmutableSamplers)) immutableSamplers
      poke (p `plusPtr` 0 :: Ptr Word32) binding
      poke (p `plusPtr` 4 :: Ptr VkDescriptorType) descriptorType
      poke (p `plusPtr` 8 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) immutableSamplers :: Word32)
      poke (p `plusPtr` 12 :: Ptr VkShaderStageFlags) stageFlags
      poke (p `plusPtr` 16 :: Ptr (Ptr VkSampler)) pImmutableSamplers


instance ToCStruct DescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo where
  pokeCStruct :: Ptr VkDescriptorSetLayoutCreateInfo -> DescriptorSetLayoutCreateInfo -> IO a -> IO a
  pokeCStruct p DescriptorSetLayoutCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pBindings <- ContT $ allocaArray @VkDescriptorSetLayoutBinding (Data.Vector.length bindings)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pBindings `advancePtr` i) s . ($ ())) bindings
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDescriptorSetLayoutCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length bindings :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDescriptorSetLayoutBinding)) pBindings


instance ToCStruct DescriptorPoolSize VkDescriptorPoolSize where
  pokeCStruct :: Ptr VkDescriptorPoolSize -> DescriptorPoolSize -> IO a -> IO a
  pokeCStruct p DescriptorPoolSize{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDescriptorType) type'
      poke (p `plusPtr` 4 :: Ptr Word32) descriptorCount


instance ToCStruct DescriptorPoolCreateInfo VkDescriptorPoolCreateInfo where
  pokeCStruct :: Ptr VkDescriptorPoolCreateInfo -> DescriptorPoolCreateInfo -> IO a -> IO a
  pokeCStruct p DescriptorPoolCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pPoolSizes <- ContT $ allocaArray @VkDescriptorPoolSize (Data.Vector.length poolSizes)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pPoolSizes `advancePtr` i) s . ($ ())) poolSizes
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDescriptorPoolCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) maxSets
      poke (p `plusPtr` 24 :: Ptr Word32) (fromIntegral $ Data.Vector.length poolSizes :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkDescriptorPoolSize)) pPoolSizes


instance ToCStruct DescriptorSetAllocateInfo VkDescriptorSetAllocateInfo where
  pokeCStruct :: Ptr VkDescriptorSetAllocateInfo -> DescriptorSetAllocateInfo -> IO a -> IO a
  pokeCStruct p DescriptorSetAllocateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pSetLayouts <- ContT $ allocaArray @VkDescriptorSetLayout (Data.Vector.length setLayouts)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pSetLayouts) setLayouts
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDescriptorPool) descriptorPool
      poke (p `plusPtr` 24 :: Ptr Word32) (fromIntegral $ Data.Vector.length setLayouts :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkDescriptorSetLayout)) pSetLayouts


instance ToCStruct SpecializationMapEntry VkSpecializationMapEntry where
  pokeCStruct :: Ptr VkSpecializationMapEntry -> SpecializationMapEntry -> IO a -> IO a
  pokeCStruct p SpecializationMapEntry{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) constantID
      poke (p `plusPtr` 4 :: Ptr Word32) offset
      poke (p `plusPtr` 8 :: Ptr CSize) size


instance ToCStruct SpecializationInfo VkSpecializationInfo where
  pokeCStruct :: Ptr VkSpecializationInfo -> SpecializationInfo -> IO a -> IO a
  pokeCStruct p SpecializationInfo{..} = (. const) . runContT $ do
    pMapEntries <- ContT $ allocaArray @VkSpecializationMapEntry (Data.Vector.length mapEntries)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pMapEntries `advancePtr` i) s . ($ ())) mapEntries
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) (fromIntegral $ Data.Vector.length mapEntries :: Word32)
      poke (p `plusPtr` 8 :: Ptr (Ptr VkSpecializationMapEntry)) pMapEntries
      poke (p `plusPtr` 16 :: Ptr CSize) dataSize
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (data' :: Ptr ())


instance ToCStruct PipelineShaderStageCreateInfo VkPipelineShaderStageCreateInfo where
  pokeCStruct :: Ptr VkPipelineShaderStageCreateInfo -> PipelineShaderStageCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineShaderStageCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pName <- ContT $ useAsCString name
    pSpecializationInfo <- ContT $ maybeWith withCStruct specializationInfo
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineShaderStageCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkShaderStageFlagBits) stage
      poke (p `plusPtr` 24 :: Ptr VkShaderModule) module'
      poke (p `plusPtr` 32 :: Ptr (Ptr CChar)) pName
      poke (p `plusPtr` 40 :: Ptr (Ptr VkSpecializationInfo)) pSpecializationInfo


instance ToCStruct ComputePipelineCreateInfo VkComputePipelineCreateInfo where
  pokeCStruct :: Ptr VkComputePipelineCreateInfo -> ComputePipelineCreateInfo -> IO a -> IO a
  pokeCStruct p ComputePipelineCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 24) stage . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineCreateFlags) flags
      poke (p `plusPtr` 72 :: Ptr VkPipelineLayout) layout
      poke (p `plusPtr` 80 :: Ptr VkPipeline) basePipelineHandle
      poke (p `plusPtr` 88 :: Ptr Int32) basePipelineIndex


instance ToCStruct VertexInputBindingDescription VkVertexInputBindingDescription where
  pokeCStruct :: Ptr VkVertexInputBindingDescription -> VertexInputBindingDescription -> IO a -> IO a
  pokeCStruct p VertexInputBindingDescription{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) binding
      poke (p `plusPtr` 4 :: Ptr Word32) stride
      poke (p `plusPtr` 8 :: Ptr VkVertexInputRate) inputRate


instance ToCStruct VertexInputAttributeDescription VkVertexInputAttributeDescription where
  pokeCStruct :: Ptr VkVertexInputAttributeDescription -> VertexInputAttributeDescription -> IO a -> IO a
  pokeCStruct p VertexInputAttributeDescription{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) location
      poke (p `plusPtr` 4 :: Ptr Word32) binding
      poke (p `plusPtr` 8 :: Ptr VkFormat) format
      poke (p `plusPtr` 12 :: Ptr Word32) offset


instance ToCStruct PipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineVertexInputStateCreateInfo -> PipelineVertexInputStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineVertexInputStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pVertexBindingDescriptions <- ContT $ allocaArray @VkVertexInputBindingDescription (Data.Vector.length vertexBindingDescriptions)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pVertexBindingDescriptions `advancePtr` i) s . ($ ())) vertexBindingDescriptions
    pVertexAttributeDescriptions <- ContT $ allocaArray @VkVertexInputAttributeDescription (Data.Vector.length vertexAttributeDescriptions)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pVertexAttributeDescriptions `advancePtr` i) s . ($ ())) vertexAttributeDescriptions
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineVertexInputStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length vertexBindingDescriptions :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkVertexInputBindingDescription)) pVertexBindingDescriptions
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length vertexAttributeDescriptions :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkVertexInputAttributeDescription)) pVertexAttributeDescriptions


instance ToCStruct PipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineInputAssemblyStateCreateInfo -> PipelineInputAssemblyStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineInputAssemblyStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineInputAssemblyStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkPrimitiveTopology) topology
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 primitiveRestartEnable :: VkBool32)


instance ToCStruct PipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineTessellationStateCreateInfo -> PipelineTessellationStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineTessellationStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineTessellationStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) patchControlPoints


instance ToCStruct PipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineViewportStateCreateInfo -> PipelineViewportStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineViewportStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pViewports <- ContT $ either (const ($ nullPtr)) (allocaArray @VkViewport . Data.Vector.length) viewports
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pViewports `advancePtr` i) s . ($ ()))) viewports
    pScissors <- ContT $ either (const ($ nullPtr)) (allocaArray @VkRect2D . Data.Vector.length) scissors
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pScissors `advancePtr` i) s . ($ ()))) scissors
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineViewportStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) viewports :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkViewport)) pViewports
      poke (p `plusPtr` 32 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) scissors :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkRect2D)) pScissors


instance ToCStruct PipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineRasterizationStateCreateInfo -> PipelineRasterizationStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineRasterizationStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineRasterizationStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 depthClampEnable :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 rasterizerDiscardEnable :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkPolygonMode) polygonMode
      poke (p `plusPtr` 32 :: Ptr VkCullModeFlags) cullMode
      poke (p `plusPtr` 36 :: Ptr VkFrontFace) frontFace
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 depthBiasEnable :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr CFloat) (CFloat depthBiasConstantFactor :: CFloat)
      poke (p `plusPtr` 48 :: Ptr CFloat) (CFloat depthBiasClamp :: CFloat)
      poke (p `plusPtr` 52 :: Ptr CFloat) (CFloat depthBiasSlopeFactor :: CFloat)
      poke (p `plusPtr` 56 :: Ptr CFloat) (CFloat lineWidth :: CFloat)


instance ToCStruct PipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineMultisampleStateCreateInfo -> PipelineMultisampleStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineMultisampleStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pSampleMask <- ContT $ either (const ($ nullPtr)) (allocaArray @VkSampleMask . Data.Vector.length) sampleMask
    liftIO $ do
      either (const (pure ())) (Data.Vector.imapM_ (pokeElemOff pSampleMask)) sampleMask
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineMultisampleStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkSampleCountFlagBits) rasterizationSamples
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 sampleShadingEnable :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr CFloat) (CFloat minSampleShading :: CFloat)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkSampleMask)) pSampleMask
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 alphaToCoverageEnable :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 alphaToOneEnable :: VkBool32)


instance ToCStruct PipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState where
  pokeCStruct :: Ptr VkPipelineColorBlendAttachmentState -> PipelineColorBlendAttachmentState -> IO a -> IO a
  pokeCStruct p PipelineColorBlendAttachmentState{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkBool32) (boolToBool32 blendEnable :: VkBool32)
      poke (p `plusPtr` 4 :: Ptr VkBlendFactor) srcColorBlendFactor
      poke (p `plusPtr` 8 :: Ptr VkBlendFactor) dstColorBlendFactor
      poke (p `plusPtr` 12 :: Ptr VkBlendOp) colorBlendOp
      poke (p `plusPtr` 16 :: Ptr VkBlendFactor) srcAlphaBlendFactor
      poke (p `plusPtr` 20 :: Ptr VkBlendFactor) dstAlphaBlendFactor
      poke (p `plusPtr` 24 :: Ptr VkBlendOp) alphaBlendOp
      poke (p `plusPtr` 28 :: Ptr VkColorComponentFlags) colorWriteMask


instance ToCStruct PipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineColorBlendStateCreateInfo -> PipelineColorBlendStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineColorBlendStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAttachments <- ContT $ allocaArray @VkPipelineColorBlendAttachmentState (Data.Vector.length attachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pAttachments `advancePtr` i) s . ($ ())) attachments
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineColorBlendStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 logicOpEnable :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkLogicOp) logicOp
      poke (p `plusPtr` 28 :: Ptr Word32) (fromIntegral $ Data.Vector.length attachments :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkPipelineColorBlendAttachmentState)) pAttachments
      case blendConstants of
        (blendConstants1, blendConstants2, blendConstants3, blendConstants4) -> do
          pokeElemOff @CFloat (p `plusPtr` 40) 0 (CFloat blendConstants1)
          pokeElemOff @CFloat (p `plusPtr` 40) 1 (CFloat blendConstants2)
          pokeElemOff @CFloat (p `plusPtr` 40) 2 (CFloat blendConstants3)
          pokeElemOff @CFloat (p `plusPtr` 40) 3 (CFloat blendConstants4)


instance ToCStruct PipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineDynamicStateCreateInfo -> PipelineDynamicStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineDynamicStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDynamicStates <- ContT $ allocaArray @VkDynamicState (Data.Vector.length dynamicStates)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDynamicStates) dynamicStates
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineDynamicStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length dynamicStates :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDynamicState)) pDynamicStates


instance ToCStruct StencilOpState VkStencilOpState where
  pokeCStruct :: Ptr VkStencilOpState -> StencilOpState -> IO a -> IO a
  pokeCStruct p StencilOpState{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStencilOp) failOp
      poke (p `plusPtr` 4 :: Ptr VkStencilOp) passOp
      poke (p `plusPtr` 8 :: Ptr VkStencilOp) depthFailOp
      poke (p `plusPtr` 12 :: Ptr VkCompareOp) compareOp
      poke (p `plusPtr` 16 :: Ptr Word32) compareMask
      poke (p `plusPtr` 20 :: Ptr Word32) writeMask
      poke (p `plusPtr` 24 :: Ptr Word32) reference


instance ToCStruct PipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineDepthStencilStateCreateInfo -> PipelineDepthStencilStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineDepthStencilStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 40) front . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 68) back . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineDepthStencilStateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 depthTestEnable :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 depthWriteEnable :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkCompareOp) depthCompareOp
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 depthBoundsTestEnable :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 stencilTestEnable :: VkBool32)
      poke (p `plusPtr` 96 :: Ptr CFloat) (CFloat minDepthBounds :: CFloat)
      poke (p `plusPtr` 100 :: Ptr CFloat) (CFloat maxDepthBounds :: CFloat)


instance ToCStruct GraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo where
  pokeCStruct :: Ptr VkGraphicsPipelineCreateInfo -> GraphicsPipelineCreateInfo -> IO a -> IO a
  pokeCStruct p GraphicsPipelineCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pStages <- ContT $ allocaArray @VkPipelineShaderStageCreateInfo (Data.Vector.length stages)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pStages `advancePtr` i) s . ($ ())) stages
    pVertexInputState <- ContT $ maybeWith withCStruct vertexInputState
    pInputAssemblyState <- ContT $ maybeWith withCStruct inputAssemblyState
    pTessellationState <- ContT $ maybeWith withCStruct tessellationState
    pViewportState <- ContT $ maybeWith withCStruct viewportState
    pRasterizationState <- ContT $ withCStruct rasterizationState
    pMultisampleState <- ContT $ maybeWith withCStruct multisampleState
    pDepthStencilState <- ContT $ maybeWith withCStruct depthStencilState
    pColorBlendState <- ContT $ maybeWith withCStruct colorBlendState
    pDynamicState <- ContT $ maybeWith withCStruct dynamicState
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length stages :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkPipelineShaderStageCreateInfo)) pStages
      poke (p `plusPtr` 32 :: Ptr (Ptr VkPipelineVertexInputStateCreateInfo)) pVertexInputState
      poke (p `plusPtr` 40 :: Ptr (Ptr VkPipelineInputAssemblyStateCreateInfo)) pInputAssemblyState
      poke (p `plusPtr` 48 :: Ptr (Ptr VkPipelineTessellationStateCreateInfo)) pTessellationState
      poke (p `plusPtr` 56 :: Ptr (Ptr VkPipelineViewportStateCreateInfo)) pViewportState
      poke (p `plusPtr` 64 :: Ptr (Ptr VkPipelineRasterizationStateCreateInfo)) pRasterizationState
      poke (p `plusPtr` 72 :: Ptr (Ptr VkPipelineMultisampleStateCreateInfo)) pMultisampleState
      poke (p `plusPtr` 80 :: Ptr (Ptr VkPipelineDepthStencilStateCreateInfo)) pDepthStencilState
      poke (p `plusPtr` 88 :: Ptr (Ptr VkPipelineColorBlendStateCreateInfo)) pColorBlendState
      poke (p `plusPtr` 96 :: Ptr (Ptr VkPipelineDynamicStateCreateInfo)) pDynamicState
      poke (p `plusPtr` 104 :: Ptr VkPipelineLayout) layout
      poke (p `plusPtr` 112 :: Ptr VkRenderPass) renderPass
      poke (p `plusPtr` 120 :: Ptr Word32) subpass
      poke (p `plusPtr` 128 :: Ptr VkPipeline) basePipelineHandle
      poke (p `plusPtr` 136 :: Ptr Int32) basePipelineIndex


instance ToCStruct PipelineCacheCreateInfo VkPipelineCacheCreateInfo where
  pokeCStruct :: Ptr VkPipelineCacheCreateInfo -> PipelineCacheCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineCacheCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pInitialData <- ContT $ (unsafeUseAsCString initialData) . (. coerce @CString @(Ptr ()))
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineCacheCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr CSize) (fromIntegral $ Data.ByteString.length initialData :: CSize)
      poke (p `plusPtr` 32 :: Ptr (Ptr ())) pInitialData


instance ToCStruct PushConstantRange VkPushConstantRange where
  pokeCStruct :: Ptr VkPushConstantRange -> PushConstantRange -> IO a -> IO a
  pokeCStruct p PushConstantRange{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkShaderStageFlags) stageFlags
      poke (p `plusPtr` 4 :: Ptr Word32) offset
      poke (p `plusPtr` 8 :: Ptr Word32) size


instance ToCStruct PipelineLayoutCreateInfo VkPipelineLayoutCreateInfo where
  pokeCStruct :: Ptr VkPipelineLayoutCreateInfo -> PipelineLayoutCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineLayoutCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pSetLayouts <- ContT $ allocaArray @VkDescriptorSetLayout (Data.Vector.length setLayouts)
    pPushConstantRanges <- ContT $ allocaArray @VkPushConstantRange (Data.Vector.length pushConstantRanges)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pPushConstantRanges `advancePtr` i) s . ($ ())) pushConstantRanges
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pSetLayouts) setLayouts
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineLayoutCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length setLayouts :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDescriptorSetLayout)) pSetLayouts
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length pushConstantRanges :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkPushConstantRange)) pPushConstantRanges


instance ToCStruct SamplerCreateInfo VkSamplerCreateInfo where
  pokeCStruct :: Ptr VkSamplerCreateInfo -> SamplerCreateInfo -> IO a -> IO a
  pokeCStruct p SamplerCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SAMPLER_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSamplerCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkFilter) magFilter
      poke (p `plusPtr` 24 :: Ptr VkFilter) minFilter
      poke (p `plusPtr` 28 :: Ptr VkSamplerMipmapMode) mipmapMode
      poke (p `plusPtr` 32 :: Ptr VkSamplerAddressMode) addressModeU
      poke (p `plusPtr` 36 :: Ptr VkSamplerAddressMode) addressModeV
      poke (p `plusPtr` 40 :: Ptr VkSamplerAddressMode) addressModeW
      poke (p `plusPtr` 44 :: Ptr CFloat) (CFloat mipLodBias :: CFloat)
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 anisotropyEnable :: VkBool32)
      poke (p `plusPtr` 52 :: Ptr CFloat) (CFloat maxAnisotropy :: CFloat)
      poke (p `plusPtr` 56 :: Ptr VkBool32) (boolToBool32 compareEnable :: VkBool32)
      poke (p `plusPtr` 60 :: Ptr VkCompareOp) compareOp
      poke (p `plusPtr` 64 :: Ptr CFloat) (CFloat minLod :: CFloat)
      poke (p `plusPtr` 68 :: Ptr CFloat) (CFloat maxLod :: CFloat)
      poke (p `plusPtr` 72 :: Ptr VkBorderColor) borderColor
      poke (p `plusPtr` 76 :: Ptr VkBool32) (boolToBool32 unnormalizedCoordinates :: VkBool32)


instance ToCStruct CommandPoolCreateInfo VkCommandPoolCreateInfo where
  pokeCStruct :: Ptr VkCommandPoolCreateInfo -> CommandPoolCreateInfo -> IO a -> IO a
  pokeCStruct p CommandPoolCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkCommandPoolCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) queueFamilyIndex


instance ToCStruct CommandBufferAllocateInfo VkCommandBufferAllocateInfo where
  pokeCStruct :: Ptr VkCommandBufferAllocateInfo -> CommandBufferAllocateInfo -> IO a -> IO a
  pokeCStruct p CommandBufferAllocateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkCommandPool) commandPool
      poke (p `plusPtr` 24 :: Ptr VkCommandBufferLevel) level
      poke (p `plusPtr` 28 :: Ptr Word32) commandBufferCount


instance ToCStruct CommandBufferInheritanceInfo VkCommandBufferInheritanceInfo where
  pokeCStruct :: Ptr VkCommandBufferInheritanceInfo -> CommandBufferInheritanceInfo -> IO a -> IO a
  pokeCStruct p CommandBufferInheritanceInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkRenderPass) renderPass
      poke (p `plusPtr` 24 :: Ptr Word32) subpass
      poke (p `plusPtr` 32 :: Ptr VkFramebuffer) framebuffer
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 occlusionQueryEnable :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkQueryControlFlags) queryFlags
      poke (p `plusPtr` 48 :: Ptr VkQueryPipelineStatisticFlags) pipelineStatistics


instance ToCStruct CommandBufferBeginInfo VkCommandBufferBeginInfo where
  pokeCStruct :: Ptr VkCommandBufferBeginInfo -> CommandBufferBeginInfo -> IO a -> IO a
  pokeCStruct p CommandBufferBeginInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pInheritanceInfo <- ContT $ maybeWith withCStruct inheritanceInfo
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkCommandBufferUsageFlags) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr VkCommandBufferInheritanceInfo)) pInheritanceInfo


instance ToCStruct RenderPassBeginInfo VkRenderPassBeginInfo where
  pokeCStruct :: Ptr VkRenderPassBeginInfo -> RenderPassBeginInfo -> IO a -> IO a
  pokeCStruct p RenderPassBeginInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 32) renderArea . ($ ())
    pClearValues <- ContT $ allocaArray @VkClearValue (Data.Vector.length clearValues)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pClearValues `advancePtr` i) s . ($ ())) clearValues
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkRenderPass) renderPass
      poke (p `plusPtr` 24 :: Ptr VkFramebuffer) framebuffer
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length clearValues :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr VkClearValue)) pClearValues


instance ToCStruct ClearDepthStencilValue VkClearDepthStencilValue where
  pokeCStruct :: Ptr VkClearDepthStencilValue -> ClearDepthStencilValue -> IO a -> IO a
  pokeCStruct p ClearDepthStencilValue{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr CFloat) (CFloat depth :: CFloat)
      poke (p `plusPtr` 4 :: Ptr Word32) stencil


instance ToCStruct ClearAttachment VkClearAttachment where
  pokeCStruct :: Ptr VkClearAttachment -> ClearAttachment -> IO a -> IO a
  pokeCStruct p ClearAttachment{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 8) clearValue . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkImageAspectFlags) aspectMask
      poke (p `plusPtr` 4 :: Ptr Word32) colorAttachment


instance ToCStruct AttachmentDescription VkAttachmentDescription where
  pokeCStruct :: Ptr VkAttachmentDescription -> AttachmentDescription -> IO a -> IO a
  pokeCStruct p AttachmentDescription{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkAttachmentDescriptionFlags) flags
      poke (p `plusPtr` 4 :: Ptr VkFormat) format
      poke (p `plusPtr` 8 :: Ptr VkSampleCountFlagBits) samples
      poke (p `plusPtr` 12 :: Ptr VkAttachmentLoadOp) loadOp
      poke (p `plusPtr` 16 :: Ptr VkAttachmentStoreOp) storeOp
      poke (p `plusPtr` 20 :: Ptr VkAttachmentLoadOp) stencilLoadOp
      poke (p `plusPtr` 24 :: Ptr VkAttachmentStoreOp) stencilStoreOp
      poke (p `plusPtr` 28 :: Ptr VkImageLayout) initialLayout
      poke (p `plusPtr` 32 :: Ptr VkImageLayout) finalLayout


instance ToCStruct AttachmentReference VkAttachmentReference where
  pokeCStruct :: Ptr VkAttachmentReference -> AttachmentReference -> IO a -> IO a
  pokeCStruct p AttachmentReference{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) attachment
      poke (p `plusPtr` 4 :: Ptr VkImageLayout) layout


instance ToCStruct SubpassDescription VkSubpassDescription where
  pokeCStruct :: Ptr VkSubpassDescription -> SubpassDescription -> IO a -> IO a
  pokeCStruct p SubpassDescription{..} = (. const) . runContT $ do
    pInputAttachments <- ContT $ allocaArray @VkAttachmentReference (Data.Vector.length inputAttachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pInputAttachments `advancePtr` i) s . ($ ())) inputAttachments
    pColorAttachments <- ContT $ allocaArray @VkAttachmentReference (Data.Vector.length colorAttachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pColorAttachments `advancePtr` i) s . ($ ())) colorAttachments
    pResolveAttachments <- ContT $ either (const ($ nullPtr)) (allocaArray @VkAttachmentReference . Data.Vector.length) resolveAttachments
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pResolveAttachments `advancePtr` i) s . ($ ()))) resolveAttachments
    pDepthStencilAttachment <- ContT $ maybeWith withCStruct depthStencilAttachment
    pPreserveAttachments <- ContT $ allocaArray @Word32 (Data.Vector.length preserveAttachments)
    liftIO $ do
      colorAttachmentCount <- let l = Data.Vector.length colorAttachments in if (l == either fromIntegral Data.Vector.length resolveAttachments) then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "colorAttachments and resolveAttachments must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pPreserveAttachments) preserveAttachments
      poke (p `plusPtr` 0 :: Ptr VkSubpassDescriptionFlags) flags
      poke (p `plusPtr` 4 :: Ptr VkPipelineBindPoint) pipelineBindPoint
      poke (p `plusPtr` 8 :: Ptr Word32) (fromIntegral $ Data.Vector.length inputAttachments :: Word32)
      poke (p `plusPtr` 16 :: Ptr (Ptr VkAttachmentReference)) pInputAttachments
      poke (p `plusPtr` 24 :: Ptr Word32) colorAttachmentCount
      poke (p `plusPtr` 32 :: Ptr (Ptr VkAttachmentReference)) pColorAttachments
      poke (p `plusPtr` 40 :: Ptr (Ptr VkAttachmentReference)) pResolveAttachments
      poke (p `plusPtr` 48 :: Ptr (Ptr VkAttachmentReference)) pDepthStencilAttachment
      poke (p `plusPtr` 56 :: Ptr Word32) (fromIntegral $ Data.Vector.length preserveAttachments :: Word32)
      poke (p `plusPtr` 64 :: Ptr (Ptr Word32)) pPreserveAttachments


instance ToCStruct SubpassDependency VkSubpassDependency where
  pokeCStruct :: Ptr VkSubpassDependency -> SubpassDependency -> IO a -> IO a
  pokeCStruct p SubpassDependency{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) srcSubpass
      poke (p `plusPtr` 4 :: Ptr Word32) dstSubpass
      poke (p `plusPtr` 8 :: Ptr VkPipelineStageFlags) srcStageMask
      poke (p `plusPtr` 12 :: Ptr VkPipelineStageFlags) dstStageMask
      poke (p `plusPtr` 16 :: Ptr VkAccessFlags) srcAccessMask
      poke (p `plusPtr` 20 :: Ptr VkAccessFlags) dstAccessMask
      poke (p `plusPtr` 24 :: Ptr VkDependencyFlags) dependencyFlags


instance ToCStruct RenderPassCreateInfo VkRenderPassCreateInfo where
  pokeCStruct :: Ptr VkRenderPassCreateInfo -> RenderPassCreateInfo -> IO a -> IO a
  pokeCStruct p RenderPassCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAttachments <- ContT $ allocaArray @VkAttachmentDescription (Data.Vector.length attachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pAttachments `advancePtr` i) s . ($ ())) attachments
    pSubpasses <- ContT $ allocaArray @VkSubpassDescription (Data.Vector.length subpasses)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pSubpasses `advancePtr` i) s . ($ ())) subpasses
    pDependencies <- ContT $ allocaArray @VkSubpassDependency (Data.Vector.length dependencies)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pDependencies `advancePtr` i) s . ($ ())) dependencies
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkRenderPassCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length attachments :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkAttachmentDescription)) pAttachments
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length subpasses :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkSubpassDescription)) pSubpasses
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length dependencies :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr VkSubpassDependency)) pDependencies


instance ToCStruct EventCreateInfo VkEventCreateInfo where
  pokeCStruct :: Ptr VkEventCreateInfo -> EventCreateInfo -> IO a -> IO a
  pokeCStruct p EventCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EVENT_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkEventCreateFlags) flags


instance ToCStruct FenceCreateInfo VkFenceCreateInfo where
  pokeCStruct :: Ptr VkFenceCreateInfo -> FenceCreateInfo -> IO a -> IO a
  pokeCStruct p FenceCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_FENCE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFenceCreateFlags) flags


instance ToCStruct PhysicalDeviceFeatures VkPhysicalDeviceFeatures where
  pokeCStruct :: Ptr VkPhysicalDeviceFeatures -> PhysicalDeviceFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFeatures{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkBool32) (boolToBool32 robustBufferAccess :: VkBool32)
      poke (p `plusPtr` 4 :: Ptr VkBool32) (boolToBool32 fullDrawIndexUint32 :: VkBool32)
      poke (p `plusPtr` 8 :: Ptr VkBool32) (boolToBool32 imageCubeArray :: VkBool32)
      poke (p `plusPtr` 12 :: Ptr VkBool32) (boolToBool32 independentBlend :: VkBool32)
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 geometryShader :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 tessellationShader :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 sampleRateShading :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 dualSrcBlend :: VkBool32)
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 logicOp :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 multiDrawIndirect :: VkBool32)
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 drawIndirectFirstInstance :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 depthClamp :: VkBool32)
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 depthBiasClamp :: VkBool32)
      poke (p `plusPtr` 52 :: Ptr VkBool32) (boolToBool32 fillModeNonSolid :: VkBool32)
      poke (p `plusPtr` 56 :: Ptr VkBool32) (boolToBool32 depthBounds :: VkBool32)
      poke (p `plusPtr` 60 :: Ptr VkBool32) (boolToBool32 wideLines :: VkBool32)
      poke (p `plusPtr` 64 :: Ptr VkBool32) (boolToBool32 largePoints :: VkBool32)
      poke (p `plusPtr` 68 :: Ptr VkBool32) (boolToBool32 alphaToOne :: VkBool32)
      poke (p `plusPtr` 72 :: Ptr VkBool32) (boolToBool32 multiViewport :: VkBool32)
      poke (p `plusPtr` 76 :: Ptr VkBool32) (boolToBool32 samplerAnisotropy :: VkBool32)
      poke (p `plusPtr` 80 :: Ptr VkBool32) (boolToBool32 textureCompressionETC2 :: VkBool32)
      poke (p `plusPtr` 84 :: Ptr VkBool32) (boolToBool32 textureCompressionASTC_LDR :: VkBool32)
      poke (p `plusPtr` 88 :: Ptr VkBool32) (boolToBool32 textureCompressionBC :: VkBool32)
      poke (p `plusPtr` 92 :: Ptr VkBool32) (boolToBool32 occlusionQueryPrecise :: VkBool32)
      poke (p `plusPtr` 96 :: Ptr VkBool32) (boolToBool32 pipelineStatisticsQuery :: VkBool32)
      poke (p `plusPtr` 100 :: Ptr VkBool32) (boolToBool32 vertexPipelineStoresAndAtomics :: VkBool32)
      poke (p `plusPtr` 104 :: Ptr VkBool32) (boolToBool32 fragmentStoresAndAtomics :: VkBool32)
      poke (p `plusPtr` 108 :: Ptr VkBool32) (boolToBool32 shaderTessellationAndGeometryPointSize :: VkBool32)
      poke (p `plusPtr` 112 :: Ptr VkBool32) (boolToBool32 shaderImageGatherExtended :: VkBool32)
      poke (p `plusPtr` 116 :: Ptr VkBool32) (boolToBool32 shaderStorageImageExtendedFormats :: VkBool32)
      poke (p `plusPtr` 120 :: Ptr VkBool32) (boolToBool32 shaderStorageImageMultisample :: VkBool32)
      poke (p `plusPtr` 124 :: Ptr VkBool32) (boolToBool32 shaderStorageImageReadWithoutFormat :: VkBool32)
      poke (p `plusPtr` 128 :: Ptr VkBool32) (boolToBool32 shaderStorageImageWriteWithoutFormat :: VkBool32)
      poke (p `plusPtr` 132 :: Ptr VkBool32) (boolToBool32 shaderUniformBufferArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 136 :: Ptr VkBool32) (boolToBool32 shaderSampledImageArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 140 :: Ptr VkBool32) (boolToBool32 shaderStorageBufferArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 144 :: Ptr VkBool32) (boolToBool32 shaderStorageImageArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 148 :: Ptr VkBool32) (boolToBool32 shaderClipDistance :: VkBool32)
      poke (p `plusPtr` 152 :: Ptr VkBool32) (boolToBool32 shaderCullDistance :: VkBool32)
      poke (p `plusPtr` 156 :: Ptr VkBool32) (boolToBool32 shaderFloat64 :: VkBool32)
      poke (p `plusPtr` 160 :: Ptr VkBool32) (boolToBool32 shaderInt64 :: VkBool32)
      poke (p `plusPtr` 164 :: Ptr VkBool32) (boolToBool32 shaderInt16 :: VkBool32)
      poke (p `plusPtr` 168 :: Ptr VkBool32) (boolToBool32 shaderResourceResidency :: VkBool32)
      poke (p `plusPtr` 172 :: Ptr VkBool32) (boolToBool32 shaderResourceMinLod :: VkBool32)
      poke (p `plusPtr` 176 :: Ptr VkBool32) (boolToBool32 sparseBinding :: VkBool32)
      poke (p `plusPtr` 180 :: Ptr VkBool32) (boolToBool32 sparseResidencyBuffer :: VkBool32)
      poke (p `plusPtr` 184 :: Ptr VkBool32) (boolToBool32 sparseResidencyImage2D :: VkBool32)
      poke (p `plusPtr` 188 :: Ptr VkBool32) (boolToBool32 sparseResidencyImage3D :: VkBool32)
      poke (p `plusPtr` 192 :: Ptr VkBool32) (boolToBool32 sparseResidency2Samples :: VkBool32)
      poke (p `plusPtr` 196 :: Ptr VkBool32) (boolToBool32 sparseResidency4Samples :: VkBool32)
      poke (p `plusPtr` 200 :: Ptr VkBool32) (boolToBool32 sparseResidency8Samples :: VkBool32)
      poke (p `plusPtr` 204 :: Ptr VkBool32) (boolToBool32 sparseResidency16Samples :: VkBool32)
      poke (p `plusPtr` 208 :: Ptr VkBool32) (boolToBool32 sparseResidencyAliased :: VkBool32)
      poke (p `plusPtr` 212 :: Ptr VkBool32) (boolToBool32 variableMultisampleRate :: VkBool32)
      poke (p `plusPtr` 216 :: Ptr VkBool32) (boolToBool32 inheritedQueries :: VkBool32)


instance ToCStruct PhysicalDeviceSparseProperties VkPhysicalDeviceSparseProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceSparseProperties -> PhysicalDeviceSparseProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSparseProperties{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkBool32) (boolToBool32 residencyStandard2DBlockShape :: VkBool32)
      poke (p `plusPtr` 4 :: Ptr VkBool32) (boolToBool32 residencyStandard2DMultisampleBlockShape :: VkBool32)
      poke (p `plusPtr` 8 :: Ptr VkBool32) (boolToBool32 residencyStandard3DBlockShape :: VkBool32)
      poke (p `plusPtr` 12 :: Ptr VkBool32) (boolToBool32 residencyAlignedMipSize :: VkBool32)
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 residencyNonResidentStrict :: VkBool32)


instance ToCStruct PhysicalDeviceLimits VkPhysicalDeviceLimits where
  pokeCStruct :: Ptr VkPhysicalDeviceLimits -> PhysicalDeviceLimits -> IO a -> IO a
  pokeCStruct p PhysicalDeviceLimits{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) maxImageDimension1D
      poke (p `plusPtr` 4 :: Ptr Word32) maxImageDimension2D
      poke (p `plusPtr` 8 :: Ptr Word32) maxImageDimension3D
      poke (p `plusPtr` 12 :: Ptr Word32) maxImageDimensionCube
      poke (p `plusPtr` 16 :: Ptr Word32) maxImageArrayLayers
      poke (p `plusPtr` 20 :: Ptr Word32) maxTexelBufferElements
      poke (p `plusPtr` 24 :: Ptr Word32) maxUniformBufferRange
      poke (p `plusPtr` 28 :: Ptr Word32) maxStorageBufferRange
      poke (p `plusPtr` 32 :: Ptr Word32) maxPushConstantsSize
      poke (p `plusPtr` 36 :: Ptr Word32) maxMemoryAllocationCount
      poke (p `plusPtr` 40 :: Ptr Word32) maxSamplerAllocationCount
      poke (p `plusPtr` 48 :: Ptr VkDeviceSize) bufferImageGranularity
      poke (p `plusPtr` 56 :: Ptr VkDeviceSize) sparseAddressSpaceSize
      poke (p `plusPtr` 64 :: Ptr Word32) maxBoundDescriptorSets
      poke (p `plusPtr` 68 :: Ptr Word32) maxPerStageDescriptorSamplers
      poke (p `plusPtr` 72 :: Ptr Word32) maxPerStageDescriptorUniformBuffers
      poke (p `plusPtr` 76 :: Ptr Word32) maxPerStageDescriptorStorageBuffers
      poke (p `plusPtr` 80 :: Ptr Word32) maxPerStageDescriptorSampledImages
      poke (p `plusPtr` 84 :: Ptr Word32) maxPerStageDescriptorStorageImages
      poke (p `plusPtr` 88 :: Ptr Word32) maxPerStageDescriptorInputAttachments
      poke (p `plusPtr` 92 :: Ptr Word32) maxPerStageResources
      poke (p `plusPtr` 96 :: Ptr Word32) maxDescriptorSetSamplers
      poke (p `plusPtr` 100 :: Ptr Word32) maxDescriptorSetUniformBuffers
      poke (p `plusPtr` 104 :: Ptr Word32) maxDescriptorSetUniformBuffersDynamic
      poke (p `plusPtr` 108 :: Ptr Word32) maxDescriptorSetStorageBuffers
      poke (p `plusPtr` 112 :: Ptr Word32) maxDescriptorSetStorageBuffersDynamic
      poke (p `plusPtr` 116 :: Ptr Word32) maxDescriptorSetSampledImages
      poke (p `plusPtr` 120 :: Ptr Word32) maxDescriptorSetStorageImages
      poke (p `plusPtr` 124 :: Ptr Word32) maxDescriptorSetInputAttachments
      poke (p `plusPtr` 128 :: Ptr Word32) maxVertexInputAttributes
      poke (p `plusPtr` 132 :: Ptr Word32) maxVertexInputBindings
      poke (p `plusPtr` 136 :: Ptr Word32) maxVertexInputAttributeOffset
      poke (p `plusPtr` 140 :: Ptr Word32) maxVertexInputBindingStride
      poke (p `plusPtr` 144 :: Ptr Word32) maxVertexOutputComponents
      poke (p `plusPtr` 148 :: Ptr Word32) maxTessellationGenerationLevel
      poke (p `plusPtr` 152 :: Ptr Word32) maxTessellationPatchSize
      poke (p `plusPtr` 156 :: Ptr Word32) maxTessellationControlPerVertexInputComponents
      poke (p `plusPtr` 160 :: Ptr Word32) maxTessellationControlPerVertexOutputComponents
      poke (p `plusPtr` 164 :: Ptr Word32) maxTessellationControlPerPatchOutputComponents
      poke (p `plusPtr` 168 :: Ptr Word32) maxTessellationControlTotalOutputComponents
      poke (p `plusPtr` 172 :: Ptr Word32) maxTessellationEvaluationInputComponents
      poke (p `plusPtr` 176 :: Ptr Word32) maxTessellationEvaluationOutputComponents
      poke (p `plusPtr` 180 :: Ptr Word32) maxGeometryShaderInvocations
      poke (p `plusPtr` 184 :: Ptr Word32) maxGeometryInputComponents
      poke (p `plusPtr` 188 :: Ptr Word32) maxGeometryOutputComponents
      poke (p `plusPtr` 192 :: Ptr Word32) maxGeometryOutputVertices
      poke (p `plusPtr` 196 :: Ptr Word32) maxGeometryTotalOutputComponents
      poke (p `plusPtr` 200 :: Ptr Word32) maxFragmentInputComponents
      poke (p `plusPtr` 204 :: Ptr Word32) maxFragmentOutputAttachments
      poke (p `plusPtr` 208 :: Ptr Word32) maxFragmentDualSrcAttachments
      poke (p `plusPtr` 212 :: Ptr Word32) maxFragmentCombinedOutputResources
      poke (p `plusPtr` 216 :: Ptr Word32) maxComputeSharedMemorySize
      case maxComputeWorkGroupCount of
        (maxComputeWorkGroupCount1, maxComputeWorkGroupCount2, maxComputeWorkGroupCount3) -> do
          pokeElemOff @Word32 (p `plusPtr` 220) 0 maxComputeWorkGroupCount1
          pokeElemOff @Word32 (p `plusPtr` 220) 1 maxComputeWorkGroupCount2
          pokeElemOff @Word32 (p `plusPtr` 220) 2 maxComputeWorkGroupCount3
      poke (p `plusPtr` 232 :: Ptr Word32) maxComputeWorkGroupInvocations
      case maxComputeWorkGroupSize of
        (maxComputeWorkGroupSize1, maxComputeWorkGroupSize2, maxComputeWorkGroupSize3) -> do
          pokeElemOff @Word32 (p `plusPtr` 236) 0 maxComputeWorkGroupSize1
          pokeElemOff @Word32 (p `plusPtr` 236) 1 maxComputeWorkGroupSize2
          pokeElemOff @Word32 (p `plusPtr` 236) 2 maxComputeWorkGroupSize3
      poke (p `plusPtr` 248 :: Ptr Word32) subPixelPrecisionBits
      poke (p `plusPtr` 252 :: Ptr Word32) subTexelPrecisionBits
      poke (p `plusPtr` 256 :: Ptr Word32) mipmapPrecisionBits
      poke (p `plusPtr` 260 :: Ptr Word32) maxDrawIndexedIndexValue
      poke (p `plusPtr` 264 :: Ptr Word32) maxDrawIndirectCount
      poke (p `plusPtr` 268 :: Ptr CFloat) (CFloat maxSamplerLodBias :: CFloat)
      poke (p `plusPtr` 272 :: Ptr CFloat) (CFloat maxSamplerAnisotropy :: CFloat)
      poke (p `plusPtr` 276 :: Ptr Word32) maxViewports
      case maxViewportDimensions of
        (maxViewportDimensions1, maxViewportDimensions2) -> do
          pokeElemOff @Word32 (p `plusPtr` 280) 0 maxViewportDimensions1
          pokeElemOff @Word32 (p `plusPtr` 280) 1 maxViewportDimensions2
      case viewportBoundsRange of
        (viewportBoundsRange1, viewportBoundsRange2) -> do
          pokeElemOff @CFloat (p `plusPtr` 288) 0 (CFloat viewportBoundsRange1)
          pokeElemOff @CFloat (p `plusPtr` 288) 1 (CFloat viewportBoundsRange2)
      poke (p `plusPtr` 296 :: Ptr Word32) viewportSubPixelBits
      poke (p `plusPtr` 304 :: Ptr CSize) minMemoryMapAlignment
      poke (p `plusPtr` 312 :: Ptr VkDeviceSize) minTexelBufferOffsetAlignment
      poke (p `plusPtr` 320 :: Ptr VkDeviceSize) minUniformBufferOffsetAlignment
      poke (p `plusPtr` 328 :: Ptr VkDeviceSize) minStorageBufferOffsetAlignment
      poke (p `plusPtr` 336 :: Ptr Int32) minTexelOffset
      poke (p `plusPtr` 340 :: Ptr Word32) maxTexelOffset
      poke (p `plusPtr` 344 :: Ptr Int32) minTexelGatherOffset
      poke (p `plusPtr` 348 :: Ptr Word32) maxTexelGatherOffset
      poke (p `plusPtr` 352 :: Ptr CFloat) (CFloat minInterpolationOffset :: CFloat)
      poke (p `plusPtr` 356 :: Ptr CFloat) (CFloat maxInterpolationOffset :: CFloat)
      poke (p `plusPtr` 360 :: Ptr Word32) subPixelInterpolationOffsetBits
      poke (p `plusPtr` 364 :: Ptr Word32) maxFramebufferWidth
      poke (p `plusPtr` 368 :: Ptr Word32) maxFramebufferHeight
      poke (p `plusPtr` 372 :: Ptr Word32) maxFramebufferLayers
      poke (p `plusPtr` 376 :: Ptr VkSampleCountFlags) framebufferColorSampleCounts
      poke (p `plusPtr` 380 :: Ptr VkSampleCountFlags) framebufferDepthSampleCounts
      poke (p `plusPtr` 384 :: Ptr VkSampleCountFlags) framebufferStencilSampleCounts
      poke (p `plusPtr` 388 :: Ptr VkSampleCountFlags) framebufferNoAttachmentsSampleCounts
      poke (p `plusPtr` 392 :: Ptr Word32) maxColorAttachments
      poke (p `plusPtr` 396 :: Ptr VkSampleCountFlags) sampledImageColorSampleCounts
      poke (p `plusPtr` 400 :: Ptr VkSampleCountFlags) sampledImageIntegerSampleCounts
      poke (p `plusPtr` 404 :: Ptr VkSampleCountFlags) sampledImageDepthSampleCounts
      poke (p `plusPtr` 408 :: Ptr VkSampleCountFlags) sampledImageStencilSampleCounts
      poke (p `plusPtr` 412 :: Ptr VkSampleCountFlags) storageImageSampleCounts
      poke (p `plusPtr` 416 :: Ptr Word32) maxSampleMaskWords
      poke (p `plusPtr` 420 :: Ptr VkBool32) (boolToBool32 timestampComputeAndGraphics :: VkBool32)
      poke (p `plusPtr` 424 :: Ptr CFloat) (CFloat timestampPeriod :: CFloat)
      poke (p `plusPtr` 428 :: Ptr Word32) maxClipDistances
      poke (p `plusPtr` 432 :: Ptr Word32) maxCullDistances
      poke (p `plusPtr` 436 :: Ptr Word32) maxCombinedClipAndCullDistances
      poke (p `plusPtr` 440 :: Ptr Word32) discreteQueuePriorities
      case pointSizeRange of
        (pointSizeRange1, pointSizeRange2) -> do
          pokeElemOff @CFloat (p `plusPtr` 444) 0 (CFloat pointSizeRange1)
          pokeElemOff @CFloat (p `plusPtr` 444) 1 (CFloat pointSizeRange2)
      case lineWidthRange of
        (lineWidthRange1, lineWidthRange2) -> do
          pokeElemOff @CFloat (p `plusPtr` 452) 0 (CFloat lineWidthRange1)
          pokeElemOff @CFloat (p `plusPtr` 452) 1 (CFloat lineWidthRange2)
      poke (p `plusPtr` 460 :: Ptr CFloat) (CFloat pointSizeGranularity :: CFloat)
      poke (p `plusPtr` 464 :: Ptr CFloat) (CFloat lineWidthGranularity :: CFloat)
      poke (p `plusPtr` 468 :: Ptr VkBool32) (boolToBool32 strictLines :: VkBool32)
      poke (p `plusPtr` 472 :: Ptr VkBool32) (boolToBool32 standardSampleLocations :: VkBool32)
      poke (p `plusPtr` 480 :: Ptr VkDeviceSize) optimalBufferCopyOffsetAlignment
      poke (p `plusPtr` 488 :: Ptr VkDeviceSize) optimalBufferCopyRowPitchAlignment
      poke (p `plusPtr` 496 :: Ptr VkDeviceSize) nonCoherentAtomSize


instance ToCStruct SemaphoreCreateInfo VkSemaphoreCreateInfo where
  pokeCStruct :: Ptr VkSemaphoreCreateInfo -> SemaphoreCreateInfo -> IO a -> IO a
  pokeCStruct p SemaphoreCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSemaphoreCreateFlags) flags


instance ToCStruct QueryPoolCreateInfo VkQueryPoolCreateInfo where
  pokeCStruct :: Ptr VkQueryPoolCreateInfo -> QueryPoolCreateInfo -> IO a -> IO a
  pokeCStruct p QueryPoolCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkQueryPoolCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkQueryType) queryType
      poke (p `plusPtr` 24 :: Ptr Word32) queryCount
      poke (p `plusPtr` 28 :: Ptr VkQueryPipelineStatisticFlags) pipelineStatistics


instance ToCStruct FramebufferCreateInfo VkFramebufferCreateInfo where
  pokeCStruct :: Ptr VkFramebufferCreateInfo -> FramebufferCreateInfo -> IO a -> IO a
  pokeCStruct p FramebufferCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAttachments <- ContT $ allocaArray @VkImageView (Data.Vector.length attachments)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pAttachments) attachments
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFramebufferCreateFlags) flags
      poke (p `plusPtr` 24 :: Ptr VkRenderPass) renderPass
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length attachments :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkImageView)) pAttachments
      poke (p `plusPtr` 48 :: Ptr Word32) width
      poke (p `plusPtr` 52 :: Ptr Word32) height
      poke (p `plusPtr` 56 :: Ptr Word32) layers


instance ToCStruct DrawIndirectCommand VkDrawIndirectCommand where
  pokeCStruct :: Ptr VkDrawIndirectCommand -> DrawIndirectCommand -> IO a -> IO a
  pokeCStruct p DrawIndirectCommand{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) vertexCount
      poke (p `plusPtr` 4 :: Ptr Word32) instanceCount
      poke (p `plusPtr` 8 :: Ptr Word32) firstVertex
      poke (p `plusPtr` 12 :: Ptr Word32) firstInstance


instance ToCStruct DrawIndexedIndirectCommand VkDrawIndexedIndirectCommand where
  pokeCStruct :: Ptr VkDrawIndexedIndirectCommand -> DrawIndexedIndirectCommand -> IO a -> IO a
  pokeCStruct p DrawIndexedIndirectCommand{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) indexCount
      poke (p `plusPtr` 4 :: Ptr Word32) instanceCount
      poke (p `plusPtr` 8 :: Ptr Word32) firstIndex
      poke (p `plusPtr` 12 :: Ptr Int32) vertexOffset
      poke (p `plusPtr` 16 :: Ptr Word32) firstInstance


instance ToCStruct DispatchIndirectCommand VkDispatchIndirectCommand where
  pokeCStruct :: Ptr VkDispatchIndirectCommand -> DispatchIndirectCommand -> IO a -> IO a
  pokeCStruct p DispatchIndirectCommand{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) x
      poke (p `plusPtr` 4 :: Ptr Word32) y
      poke (p `plusPtr` 8 :: Ptr Word32) z


instance ToCStruct SubmitInfo VkSubmitInfo where
  pokeCStruct :: Ptr VkSubmitInfo -> SubmitInfo -> IO a -> IO a
  pokeCStruct p SubmitInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pWaitSemaphores <- ContT $ allocaArray @VkSemaphore (Data.Vector.length waitSemaphores)
    pWaitDstStageMask <- ContT $ allocaArray @VkPipelineStageFlags (Data.Vector.length waitDstStageMask)
    pCommandBuffers <- ContT $ allocaArray @VkCommandBuffer (Data.Vector.length commandBuffers)
    pSignalSemaphores <- ContT $ allocaArray @VkSemaphore (Data.Vector.length signalSemaphores)
    liftIO $ do
      waitSemaphoreCount <- let l = Data.Vector.length waitSemaphores in if l == Data.Vector.length waitDstStageMask then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "waitSemaphores and waitDstStageMask must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pWaitSemaphores) waitSemaphores
      Data.Vector.imapM_ (pokeElemOff pWaitDstStageMask) waitDstStageMask
      Data.Vector.imapM_ (\i -> pokeElemOff pCommandBuffers i . commandBufferHandle) commandBuffers
      Data.Vector.imapM_ (pokeElemOff pSignalSemaphores) signalSemaphores
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SUBMIT_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) waitSemaphoreCount
      poke (p `plusPtr` 24 :: Ptr (Ptr VkSemaphore)) pWaitSemaphores
      poke (p `plusPtr` 32 :: Ptr (Ptr VkPipelineStageFlags)) pWaitDstStageMask
      poke (p `plusPtr` 40 :: Ptr Word32) (fromIntegral $ Data.Vector.length commandBuffers :: Word32)
      poke (p `plusPtr` 48 :: Ptr (Ptr VkCommandBuffer)) pCommandBuffers
      poke (p `plusPtr` 56 :: Ptr Word32) (fromIntegral $ Data.Vector.length signalSemaphores :: Word32)
      poke (p `plusPtr` 64 :: Ptr (Ptr VkSemaphore)) pSignalSemaphores


instance ToCStruct DisplayPropertiesKHR VkDisplayPropertiesKHR where
  pokeCStruct :: Ptr VkDisplayPropertiesKHR -> DisplayPropertiesKHR -> IO a -> IO a
  pokeCStruct p DisplayPropertiesKHR{..} = (. const) . runContT $ do
    displayName <- ContT $ useAsCString displayName
    ContT $ pokeCStruct (p `plusPtr` 16) physicalDimensions . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 24) physicalResolution . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDisplayKHR) display
      poke (p `plusPtr` 8 :: Ptr (Ptr CChar)) displayName
      poke (p `plusPtr` 32 :: Ptr VkSurfaceTransformFlagsKHR) supportedTransforms
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 planeReorderPossible :: VkBool32)
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 persistentContent :: VkBool32)


instance ToCStruct DisplayPlanePropertiesKHR VkDisplayPlanePropertiesKHR where
  pokeCStruct :: Ptr VkDisplayPlanePropertiesKHR -> DisplayPlanePropertiesKHR -> IO a -> IO a
  pokeCStruct p DisplayPlanePropertiesKHR{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDisplayKHR) currentDisplay
      poke (p `plusPtr` 8 :: Ptr Word32) currentStackIndex


instance ToCStruct DisplayModeParametersKHR VkDisplayModeParametersKHR where
  pokeCStruct :: Ptr VkDisplayModeParametersKHR -> DisplayModeParametersKHR -> IO a -> IO a
  pokeCStruct p DisplayModeParametersKHR{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) visibleRegion . ($ ())
    liftIO $ do
      poke (p `plusPtr` 8 :: Ptr Word32) refreshRate


instance ToCStruct DisplayModePropertiesKHR VkDisplayModePropertiesKHR where
  pokeCStruct :: Ptr VkDisplayModePropertiesKHR -> DisplayModePropertiesKHR -> IO a -> IO a
  pokeCStruct p DisplayModePropertiesKHR{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 8) parameters . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDisplayModeKHR) displayMode


instance ToCStruct DisplayModeCreateInfoKHR VkDisplayModeCreateInfoKHR where
  pokeCStruct :: Ptr VkDisplayModeCreateInfoKHR -> DisplayModeCreateInfoKHR -> IO a -> IO a
  pokeCStruct p DisplayModeCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 20) parameters . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDisplayModeCreateFlagsKHR) flags


instance ToCStruct DisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR where
  pokeCStruct :: Ptr VkDisplayPlaneCapabilitiesKHR -> DisplayPlaneCapabilitiesKHR -> IO a -> IO a
  pokeCStruct p DisplayPlaneCapabilitiesKHR{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 4) minSrcPosition . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 12) maxSrcPosition . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 20) minSrcExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 28) maxSrcExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 36) minDstPosition . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 44) maxDstPosition . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 52) minDstExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 60) maxDstExtent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkDisplayPlaneAlphaFlagsKHR) supportedAlpha


instance ToCStruct DisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR where
  pokeCStruct :: Ptr VkDisplaySurfaceCreateInfoKHR -> DisplaySurfaceCreateInfoKHR -> IO a -> IO a
  pokeCStruct p DisplaySurfaceCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 52) imageExtent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDisplaySurfaceCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr VkDisplayModeKHR) displayMode
      poke (p `plusPtr` 32 :: Ptr Word32) planeIndex
      poke (p `plusPtr` 36 :: Ptr Word32) planeStackIndex
      poke (p `plusPtr` 40 :: Ptr VkSurfaceTransformFlagBitsKHR) transform
      poke (p `plusPtr` 44 :: Ptr CFloat) (CFloat globalAlpha :: CFloat)
      poke (p `plusPtr` 48 :: Ptr VkDisplayPlaneAlphaFlagBitsKHR) alphaMode


instance ToCStruct DisplayPresentInfoKHR VkDisplayPresentInfoKHR where
  pokeCStruct :: Ptr VkDisplayPresentInfoKHR -> DisplayPresentInfoKHR -> IO a -> IO a
  pokeCStruct p DisplayPresentInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) srcRect . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 32) dstRect . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 persistent :: VkBool32)


instance ToCStruct SurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR where
  pokeCStruct :: Ptr VkSurfaceCapabilitiesKHR -> SurfaceCapabilitiesKHR -> IO a -> IO a
  pokeCStruct p SurfaceCapabilitiesKHR{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 8) currentExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 16) minImageExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 24) maxImageExtent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) minImageCount
      poke (p `plusPtr` 4 :: Ptr Word32) maxImageCount
      poke (p `plusPtr` 32 :: Ptr Word32) maxImageArrayLayers
      poke (p `plusPtr` 36 :: Ptr VkSurfaceTransformFlagsKHR) supportedTransforms
      poke (p `plusPtr` 40 :: Ptr VkSurfaceTransformFlagBitsKHR) currentTransform
      poke (p `plusPtr` 44 :: Ptr VkCompositeAlphaFlagsKHR) supportedCompositeAlpha
      poke (p `plusPtr` 48 :: Ptr VkImageUsageFlags) supportedUsageFlags


instance ToCStruct AndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR where
  pokeCStruct :: Ptr VkAndroidSurfaceCreateInfoKHR -> AndroidSurfaceCreateInfoKHR -> IO a -> IO a
  pokeCStruct p AndroidSurfaceCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAndroidSurfaceCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr ANativeWindow)) (window :: Ptr ANativeWindow)


instance ToCStruct ViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN where
  pokeCStruct :: Ptr VkViSurfaceCreateInfoNN -> ViSurfaceCreateInfoNN -> IO a -> IO a
  pokeCStruct p ViSurfaceCreateInfoNN{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkViSurfaceCreateFlagsNN) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (window :: Ptr ())


instance ToCStruct WaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR where
  pokeCStruct :: Ptr VkWaylandSurfaceCreateInfoKHR -> WaylandSurfaceCreateInfoKHR -> IO a -> IO a
  pokeCStruct p WaylandSurfaceCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkWaylandSurfaceCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr Wl_display)) (display :: Ptr Wl_display)
      poke (p `plusPtr` 32 :: Ptr (Ptr Wl_surface)) (surface :: Ptr Wl_surface)


instance ToCStruct Win32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR where
  pokeCStruct :: Ptr VkWin32SurfaceCreateInfoKHR -> Win32SurfaceCreateInfoKHR -> IO a -> IO a
  pokeCStruct p Win32SurfaceCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkWin32SurfaceCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr HINSTANCE) hinstance
      poke (p `plusPtr` 32 :: Ptr HWND) hwnd


instance ToCStruct XlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR where
  pokeCStruct :: Ptr VkXlibSurfaceCreateInfoKHR -> XlibSurfaceCreateInfoKHR -> IO a -> IO a
  pokeCStruct p XlibSurfaceCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkXlibSurfaceCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr Display)) (dpy :: Ptr Display)
      poke (p `plusPtr` 32 :: Ptr Window) window


instance ToCStruct XcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR where
  pokeCStruct :: Ptr VkXcbSurfaceCreateInfoKHR -> XcbSurfaceCreateInfoKHR -> IO a -> IO a
  pokeCStruct p XcbSurfaceCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkXcbSurfaceCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr Xcb_connection_t)) (connection :: Ptr Xcb_connection_t)
      poke (p `plusPtr` 32 :: Ptr Xcb_window_t) window


instance ToCStruct ImagePipeSurfaceCreateInfoFUCHSIA VkImagePipeSurfaceCreateInfoFUCHSIA where
  pokeCStruct :: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA -> ImagePipeSurfaceCreateInfoFUCHSIA -> IO a -> IO a
  pokeCStruct p ImagePipeSurfaceCreateInfoFUCHSIA{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImagePipeSurfaceCreateFlagsFUCHSIA) flags
      poke (p `plusPtr` 20 :: Ptr Zx_handle_t) imagePipeHandle


instance ToCStruct StreamDescriptorSurfaceCreateInfoGGP VkStreamDescriptorSurfaceCreateInfoGGP where
  pokeCStruct :: Ptr VkStreamDescriptorSurfaceCreateInfoGGP -> StreamDescriptorSurfaceCreateInfoGGP -> IO a -> IO a
  pokeCStruct p StreamDescriptorSurfaceCreateInfoGGP{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkStreamDescriptorSurfaceCreateFlagsGGP) flags
      poke (p `plusPtr` 20 :: Ptr GgpStreamDescriptor) streamDescriptor


instance ToCStruct SurfaceFormatKHR VkSurfaceFormatKHR where
  pokeCStruct :: Ptr VkSurfaceFormatKHR -> SurfaceFormatKHR -> IO a -> IO a
  pokeCStruct p SurfaceFormatKHR{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkFormat) format
      poke (p `plusPtr` 4 :: Ptr VkColorSpaceKHR) colorSpace


instance ToCStruct SwapchainCreateInfoKHR VkSwapchainCreateInfoKHR where
  pokeCStruct :: Ptr VkSwapchainCreateInfoKHR -> SwapchainCreateInfoKHR -> IO a -> IO a
  pokeCStruct p SwapchainCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 44) imageExtent . ($ ())
    pQueueFamilyIndices <- ContT $ allocaArray @Word32 (Data.Vector.length queueFamilyIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pQueueFamilyIndices) queueFamilyIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSwapchainCreateFlagsKHR) flags
      poke (p `plusPtr` 24 :: Ptr VkSurfaceKHR) surface
      poke (p `plusPtr` 32 :: Ptr Word32) minImageCount
      poke (p `plusPtr` 36 :: Ptr VkFormat) imageFormat
      poke (p `plusPtr` 40 :: Ptr VkColorSpaceKHR) imageColorSpace
      poke (p `plusPtr` 52 :: Ptr Word32) imageArrayLayers
      poke (p `plusPtr` 56 :: Ptr VkImageUsageFlags) imageUsage
      poke (p `plusPtr` 60 :: Ptr VkSharingMode) imageSharingMode
      poke (p `plusPtr` 64 :: Ptr Word32) (fromIntegral $ Data.Vector.length queueFamilyIndices :: Word32)
      poke (p `plusPtr` 72 :: Ptr (Ptr Word32)) pQueueFamilyIndices
      poke (p `plusPtr` 80 :: Ptr VkSurfaceTransformFlagBitsKHR) preTransform
      poke (p `plusPtr` 84 :: Ptr VkCompositeAlphaFlagBitsKHR) compositeAlpha
      poke (p `plusPtr` 88 :: Ptr VkPresentModeKHR) presentMode
      poke (p `plusPtr` 92 :: Ptr VkBool32) (boolToBool32 clipped :: VkBool32)
      poke (p `plusPtr` 96 :: Ptr VkSwapchainKHR) oldSwapchain


instance ToCStruct PresentInfoKHR VkPresentInfoKHR where
  pokeCStruct :: Ptr VkPresentInfoKHR -> PresentInfoKHR -> IO a -> IO a
  pokeCStruct p PresentInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pWaitSemaphores <- ContT $ allocaArray @VkSemaphore (Data.Vector.length waitSemaphores)
    pSwapchains <- ContT $ allocaArray @VkSwapchainKHR (Data.Vector.length swapchains)
    pImageIndices <- ContT $ allocaArray @Word32 (Data.Vector.length imageIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pWaitSemaphores) waitSemaphores
      swapchainCount <- let l = Data.Vector.length swapchains in if l == Data.Vector.length imageIndices then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "swapchains and imageIndices must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pSwapchains) swapchains
      Data.Vector.imapM_ (pokeElemOff pImageIndices) imageIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PRESENT_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length waitSemaphores :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkSemaphore)) pWaitSemaphores
      poke (p `plusPtr` 32 :: Ptr Word32) swapchainCount
      poke (p `plusPtr` 40 :: Ptr (Ptr VkSwapchainKHR)) pSwapchains
      poke (p `plusPtr` 48 :: Ptr (Ptr Word32)) pImageIndices
      poke (p `plusPtr` 56 :: Ptr (Ptr VkResult)) (results :: Ptr VkResult)


instance ToCStruct DebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT where
  pokeCStruct :: Ptr VkDebugReportCallbackCreateInfoEXT -> DebugReportCallbackCreateInfoEXT -> IO a -> IO a
  pokeCStruct p DebugReportCallbackCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDebugReportFlagsEXT) flags
      poke (p `plusPtr` 24 :: Ptr PFN_vkDebugReportCallbackEXT) pfnCallback
      poke (p `plusPtr` 32 :: Ptr (Ptr ())) (userData :: Ptr ())


instance ToCStruct ValidationFlagsEXT VkValidationFlagsEXT where
  pokeCStruct :: Ptr VkValidationFlagsEXT -> ValidationFlagsEXT -> IO a -> IO a
  pokeCStruct p ValidationFlagsEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDisabledValidationChecks <- ContT $ allocaArray @VkValidationCheckEXT (Data.Vector.length disabledValidationChecks)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDisabledValidationChecks) disabledValidationChecks
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_VALIDATION_FLAGS_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length disabledValidationChecks :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkValidationCheckEXT)) pDisabledValidationChecks


instance ToCStruct ValidationFeaturesEXT VkValidationFeaturesEXT where
  pokeCStruct :: Ptr VkValidationFeaturesEXT -> ValidationFeaturesEXT -> IO a -> IO a
  pokeCStruct p ValidationFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pEnabledValidationFeatures <- ContT $ allocaArray @VkValidationFeatureEnableEXT (Data.Vector.length enabledValidationFeatures)
    pDisabledValidationFeatures <- ContT $ allocaArray @VkValidationFeatureDisableEXT (Data.Vector.length disabledValidationFeatures)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pEnabledValidationFeatures) enabledValidationFeatures
      Data.Vector.imapM_ (pokeElemOff pDisabledValidationFeatures) disabledValidationFeatures
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_VALIDATION_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length enabledValidationFeatures :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkValidationFeatureEnableEXT)) pEnabledValidationFeatures
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length disabledValidationFeatures :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkValidationFeatureDisableEXT)) pDisabledValidationFeatures


instance ToCStruct PipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD where
  pokeCStruct :: Ptr VkPipelineRasterizationStateRasterizationOrderAMD -> PipelineRasterizationStateRasterizationOrderAMD -> IO a -> IO a
  pokeCStruct p PipelineRasterizationStateRasterizationOrderAMD{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkRasterizationOrderAMD) rasterizationOrder


instance ToCStruct DebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT where
  pokeCStruct :: Ptr VkDebugMarkerObjectNameInfoEXT -> DebugMarkerObjectNameInfoEXT -> IO a -> IO a
  pokeCStruct p DebugMarkerObjectNameInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pObjectName <- ContT $ useAsCString objectName
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDebugReportObjectTypeEXT) objectType
      poke (p `plusPtr` 24 :: Ptr Word64) object
      poke (p `plusPtr` 32 :: Ptr (Ptr CChar)) pObjectName


instance ToCStruct DebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT where
  pokeCStruct :: Ptr VkDebugMarkerObjectTagInfoEXT -> DebugMarkerObjectTagInfoEXT -> IO a -> IO a
  pokeCStruct p DebugMarkerObjectTagInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDebugReportObjectTypeEXT) objectType
      poke (p `plusPtr` 24 :: Ptr Word64) object
      poke (p `plusPtr` 32 :: Ptr Word64) tagName
      poke (p `plusPtr` 40 :: Ptr CSize) tagSize
      poke (p `plusPtr` 48 :: Ptr (Ptr ())) (tag :: Ptr ())


instance ToCStruct DebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT where
  pokeCStruct :: Ptr VkDebugMarkerMarkerInfoEXT -> DebugMarkerMarkerInfoEXT -> IO a -> IO a
  pokeCStruct p DebugMarkerMarkerInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pMarkerName <- ContT $ useAsCString markerName
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr CChar)) pMarkerName
      case color of
        (color1, color2, color3, color4) -> do
          pokeElemOff @CFloat (p `plusPtr` 24) 0 (CFloat color1)
          pokeElemOff @CFloat (p `plusPtr` 24) 1 (CFloat color2)
          pokeElemOff @CFloat (p `plusPtr` 24) 2 (CFloat color3)
          pokeElemOff @CFloat (p `plusPtr` 24) 3 (CFloat color4)


instance ToCStruct DedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV where
  pokeCStruct :: Ptr VkDedicatedAllocationImageCreateInfoNV -> DedicatedAllocationImageCreateInfoNV -> IO a -> IO a
  pokeCStruct p DedicatedAllocationImageCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 dedicatedAllocation :: VkBool32)


instance ToCStruct DedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV where
  pokeCStruct :: Ptr VkDedicatedAllocationBufferCreateInfoNV -> DedicatedAllocationBufferCreateInfoNV -> IO a -> IO a
  pokeCStruct p DedicatedAllocationBufferCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 dedicatedAllocation :: VkBool32)


instance ToCStruct DedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV where
  pokeCStruct :: Ptr VkDedicatedAllocationMemoryAllocateInfoNV -> DedicatedAllocationMemoryAllocateInfoNV -> IO a -> IO a
  pokeCStruct p DedicatedAllocationMemoryAllocateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImage) image
      poke (p `plusPtr` 24 :: Ptr VkBuffer) buffer


instance ToCStruct ExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV where
  pokeCStruct :: Ptr VkExternalImageFormatPropertiesNV -> ExternalImageFormatPropertiesNV -> IO a -> IO a
  pokeCStruct p ExternalImageFormatPropertiesNV{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) imageFormatProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 32 :: Ptr VkExternalMemoryFeatureFlagsNV) externalMemoryFeatures
      poke (p `plusPtr` 36 :: Ptr VkExternalMemoryHandleTypeFlagsNV) exportFromImportedHandleTypes
      poke (p `plusPtr` 40 :: Ptr VkExternalMemoryHandleTypeFlagsNV) compatibleHandleTypes


instance ToCStruct ExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV where
  pokeCStruct :: Ptr VkExternalMemoryImageCreateInfoNV -> ExternalMemoryImageCreateInfoNV -> IO a -> IO a
  pokeCStruct p ExternalMemoryImageCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagsNV) handleTypes


instance ToCStruct ExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV where
  pokeCStruct :: Ptr VkExportMemoryAllocateInfoNV -> ExportMemoryAllocateInfoNV -> IO a -> IO a
  pokeCStruct p ExportMemoryAllocateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagsNV) handleTypes


instance ToCStruct ImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV where
  pokeCStruct :: Ptr VkImportMemoryWin32HandleInfoNV -> ImportMemoryWin32HandleInfoNV -> IO a -> IO a
  pokeCStruct p ImportMemoryWin32HandleInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagsNV) handleType
      poke (p `plusPtr` 24 :: Ptr HANDLE) handle


instance ToCStruct ExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV where
  pokeCStruct :: Ptr VkExportMemoryWin32HandleInfoNV -> ExportMemoryWin32HandleInfoNV -> IO a -> IO a
  pokeCStruct p ExportMemoryWin32HandleInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)) (attributes :: Ptr SECURITY_ATTRIBUTES)
      poke (p `plusPtr` 24 :: Ptr DWORD) dwAccess


instance ToCStruct Win32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV where
  pokeCStruct :: Ptr VkWin32KeyedMutexAcquireReleaseInfoNV -> Win32KeyedMutexAcquireReleaseInfoNV -> IO a -> IO a
  pokeCStruct p Win32KeyedMutexAcquireReleaseInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAcquireSyncs <- ContT $ allocaArray @VkDeviceMemory (Data.Vector.length acquireSyncs)
    pAcquireKeys <- ContT $ allocaArray @Word64 (Data.Vector.length acquireKeys)
    pAcquireTimeoutMilliseconds <- ContT $ allocaArray @Word32 (Data.Vector.length acquireTimeoutMilliseconds)
    pReleaseSyncs <- ContT $ allocaArray @VkDeviceMemory (Data.Vector.length releaseSyncs)
    pReleaseKeys <- ContT $ allocaArray @Word64 (Data.Vector.length releaseKeys)
    liftIO $ do
      acquireCount <- let l = Data.Vector.length acquireSyncs in if l == Data.Vector.length acquireKeys && l == Data.Vector.length acquireTimeoutMilliseconds then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "acquireSyncs, acquireKeys and acquireTimeoutMilliseconds must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pAcquireSyncs) acquireSyncs
      Data.Vector.imapM_ (pokeElemOff pAcquireKeys) acquireKeys
      Data.Vector.imapM_ (pokeElemOff pAcquireTimeoutMilliseconds) acquireTimeoutMilliseconds
      releaseCount <- let l = Data.Vector.length releaseSyncs in if l == Data.Vector.length releaseKeys then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "releaseSyncs and releaseKeys must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pReleaseSyncs) releaseSyncs
      Data.Vector.imapM_ (pokeElemOff pReleaseKeys) releaseKeys
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) acquireCount
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDeviceMemory)) pAcquireSyncs
      poke (p `plusPtr` 32 :: Ptr (Ptr Word64)) pAcquireKeys
      poke (p `plusPtr` 40 :: Ptr (Ptr Word32)) pAcquireTimeoutMilliseconds
      poke (p `plusPtr` 48 :: Ptr Word32) releaseCount
      poke (p `plusPtr` 56 :: Ptr (Ptr VkDeviceMemory)) pReleaseSyncs
      poke (p `plusPtr` 64 :: Ptr (Ptr Word64)) pReleaseKeys


instance ToCStruct DeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX where
  pokeCStruct :: Ptr VkDeviceGeneratedCommandsFeaturesNVX -> DeviceGeneratedCommandsFeaturesNVX -> IO a -> IO a
  pokeCStruct p DeviceGeneratedCommandsFeaturesNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 computeBindingPointSupport :: VkBool32)


instance ToCStruct DeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX where
  pokeCStruct :: Ptr VkDeviceGeneratedCommandsLimitsNVX -> DeviceGeneratedCommandsLimitsNVX -> IO a -> IO a
  pokeCStruct p DeviceGeneratedCommandsLimitsNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxIndirectCommandsLayoutTokenCount
      poke (p `plusPtr` 20 :: Ptr Word32) maxObjectEntryCounts
      poke (p `plusPtr` 24 :: Ptr Word32) minSequenceCountBufferOffsetAlignment
      poke (p `plusPtr` 28 :: Ptr Word32) minSequenceIndexBufferOffsetAlignment
      poke (p `plusPtr` 32 :: Ptr Word32) minCommandsTokenBufferOffsetAlignment


instance ToCStruct IndirectCommandsTokenNVX VkIndirectCommandsTokenNVX where
  pokeCStruct :: Ptr VkIndirectCommandsTokenNVX -> IndirectCommandsTokenNVX -> IO a -> IO a
  pokeCStruct p IndirectCommandsTokenNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkIndirectCommandsTokenTypeNVX) tokenType
      poke (p `plusPtr` 8 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) offset


instance ToCStruct IndirectCommandsLayoutTokenNVX VkIndirectCommandsLayoutTokenNVX where
  pokeCStruct :: Ptr VkIndirectCommandsLayoutTokenNVX -> IndirectCommandsLayoutTokenNVX -> IO a -> IO a
  pokeCStruct p IndirectCommandsLayoutTokenNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkIndirectCommandsTokenTypeNVX) tokenType
      poke (p `plusPtr` 4 :: Ptr Word32) bindingUnit
      poke (p `plusPtr` 8 :: Ptr Word32) dynamicCount
      poke (p `plusPtr` 12 :: Ptr Word32) divisor


instance ToCStruct IndirectCommandsLayoutCreateInfoNVX VkIndirectCommandsLayoutCreateInfoNVX where
  pokeCStruct :: Ptr VkIndirectCommandsLayoutCreateInfoNVX -> IndirectCommandsLayoutCreateInfoNVX -> IO a -> IO a
  pokeCStruct p IndirectCommandsLayoutCreateInfoNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pTokens <- ContT $ allocaArray @VkIndirectCommandsLayoutTokenNVX (Data.Vector.length tokens)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pTokens `advancePtr` i) s . ($ ())) tokens
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineBindPoint) pipelineBindPoint
      poke (p `plusPtr` 20 :: Ptr VkIndirectCommandsLayoutUsageFlagsNVX) flags
      poke (p `plusPtr` 24 :: Ptr Word32) (fromIntegral $ Data.Vector.length tokens :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkIndirectCommandsLayoutTokenNVX)) pTokens


instance ToCStruct CmdProcessCommandsInfoNVX VkCmdProcessCommandsInfoNVX where
  pokeCStruct :: Ptr VkCmdProcessCommandsInfoNVX -> CmdProcessCommandsInfoNVX -> IO a -> IO a
  pokeCStruct p CmdProcessCommandsInfoNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pIndirectCommandsTokens <- ContT $ allocaArray @VkIndirectCommandsTokenNVX (Data.Vector.length indirectCommandsTokens)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pIndirectCommandsTokens `advancePtr` i) s . ($ ())) indirectCommandsTokens
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkObjectTableNVX) objectTable
      poke (p `plusPtr` 24 :: Ptr VkIndirectCommandsLayoutNVX) indirectCommandsLayout
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length indirectCommandsTokens :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkIndirectCommandsTokenNVX)) pIndirectCommandsTokens
      poke (p `plusPtr` 48 :: Ptr Word32) maxSequencesCount
      poke (p `plusPtr` 56 :: Ptr VkCommandBuffer) (maybe zero commandBufferHandle targetCommandBuffer :: VkCommandBuffer)
      poke (p `plusPtr` 64 :: Ptr VkBuffer) sequencesCountBuffer
      poke (p `plusPtr` 72 :: Ptr VkDeviceSize) sequencesCountOffset
      poke (p `plusPtr` 80 :: Ptr VkBuffer) sequencesIndexBuffer
      poke (p `plusPtr` 88 :: Ptr VkDeviceSize) sequencesIndexOffset


instance ToCStruct CmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX where
  pokeCStruct :: Ptr VkCmdReserveSpaceForCommandsInfoNVX -> CmdReserveSpaceForCommandsInfoNVX -> IO a -> IO a
  pokeCStruct p CmdReserveSpaceForCommandsInfoNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkObjectTableNVX) objectTable
      poke (p `plusPtr` 24 :: Ptr VkIndirectCommandsLayoutNVX) indirectCommandsLayout
      poke (p `plusPtr` 32 :: Ptr Word32) maxSequencesCount


instance ToCStruct ObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX where
  pokeCStruct :: Ptr VkObjectTableCreateInfoNVX -> ObjectTableCreateInfoNVX -> IO a -> IO a
  pokeCStruct p ObjectTableCreateInfoNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pObjectEntryTypes <- ContT $ allocaArray @VkObjectEntryTypeNVX (Data.Vector.length objectEntryTypes)
    pObjectEntryCounts <- ContT $ allocaArray @Word32 (Data.Vector.length objectEntryCounts)
    pObjectEntryUsageFlags <- ContT $ allocaArray @VkObjectEntryUsageFlagsNVX (Data.Vector.length objectEntryUsageFlags)
    liftIO $ do
      objectCount <- let l = Data.Vector.length objectEntryTypes in if l == Data.Vector.length objectEntryCounts && l == Data.Vector.length objectEntryUsageFlags then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "objectEntryTypes, objectEntryCounts and objectEntryUsageFlags must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pObjectEntryTypes) objectEntryTypes
      Data.Vector.imapM_ (pokeElemOff pObjectEntryCounts) objectEntryCounts
      Data.Vector.imapM_ (pokeElemOff pObjectEntryUsageFlags) objectEntryUsageFlags
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) objectCount
      poke (p `plusPtr` 24 :: Ptr (Ptr VkObjectEntryTypeNVX)) pObjectEntryTypes
      poke (p `plusPtr` 32 :: Ptr (Ptr Word32)) pObjectEntryCounts
      poke (p `plusPtr` 40 :: Ptr (Ptr VkObjectEntryUsageFlagsNVX)) pObjectEntryUsageFlags
      poke (p `plusPtr` 48 :: Ptr Word32) maxUniformBuffersPerDescriptor
      poke (p `plusPtr` 52 :: Ptr Word32) maxStorageBuffersPerDescriptor
      poke (p `plusPtr` 56 :: Ptr Word32) maxStorageImagesPerDescriptor
      poke (p `plusPtr` 60 :: Ptr Word32) maxSampledImagesPerDescriptor
      poke (p `plusPtr` 64 :: Ptr Word32) maxPipelineLayouts


instance ToCStruct ObjectTableEntryNVX VkObjectTableEntryNVX where
  pokeCStruct :: Ptr VkObjectTableEntryNVX -> ObjectTableEntryNVX -> IO a -> IO a
  pokeCStruct p ObjectTableEntryNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkObjectEntryTypeNVX) type'
      poke (p `plusPtr` 4 :: Ptr VkObjectEntryUsageFlagsNVX) flags


instance ToCStruct ObjectTablePipelineEntryNVX VkObjectTablePipelineEntryNVX where
  pokeCStruct :: Ptr VkObjectTablePipelineEntryNVX -> ObjectTablePipelineEntryNVX -> IO a -> IO a
  pokeCStruct p ObjectTablePipelineEntryNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkObjectEntryTypeNVX) type'
      poke (p `plusPtr` 4 :: Ptr VkObjectEntryUsageFlagsNVX) flags
      poke (p `plusPtr` 8 :: Ptr VkPipeline) pipeline


instance ToCStruct ObjectTableDescriptorSetEntryNVX VkObjectTableDescriptorSetEntryNVX where
  pokeCStruct :: Ptr VkObjectTableDescriptorSetEntryNVX -> ObjectTableDescriptorSetEntryNVX -> IO a -> IO a
  pokeCStruct p ObjectTableDescriptorSetEntryNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkObjectEntryTypeNVX) type'
      poke (p `plusPtr` 4 :: Ptr VkObjectEntryUsageFlagsNVX) flags
      poke (p `plusPtr` 8 :: Ptr VkPipelineLayout) pipelineLayout
      poke (p `plusPtr` 16 :: Ptr VkDescriptorSet) descriptorSet


instance ToCStruct ObjectTableVertexBufferEntryNVX VkObjectTableVertexBufferEntryNVX where
  pokeCStruct :: Ptr VkObjectTableVertexBufferEntryNVX -> ObjectTableVertexBufferEntryNVX -> IO a -> IO a
  pokeCStruct p ObjectTableVertexBufferEntryNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkObjectEntryTypeNVX) type'
      poke (p `plusPtr` 4 :: Ptr VkObjectEntryUsageFlagsNVX) flags
      poke (p `plusPtr` 8 :: Ptr VkBuffer) buffer


instance ToCStruct ObjectTableIndexBufferEntryNVX VkObjectTableIndexBufferEntryNVX where
  pokeCStruct :: Ptr VkObjectTableIndexBufferEntryNVX -> ObjectTableIndexBufferEntryNVX -> IO a -> IO a
  pokeCStruct p ObjectTableIndexBufferEntryNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkObjectEntryTypeNVX) type'
      poke (p `plusPtr` 4 :: Ptr VkObjectEntryUsageFlagsNVX) flags
      poke (p `plusPtr` 8 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 16 :: Ptr VkIndexType) indexType


instance ToCStruct ObjectTablePushConstantEntryNVX VkObjectTablePushConstantEntryNVX where
  pokeCStruct :: Ptr VkObjectTablePushConstantEntryNVX -> ObjectTablePushConstantEntryNVX -> IO a -> IO a
  pokeCStruct p ObjectTablePushConstantEntryNVX{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkObjectEntryTypeNVX) type'
      poke (p `plusPtr` 4 :: Ptr VkObjectEntryUsageFlagsNVX) flags
      poke (p `plusPtr` 8 :: Ptr VkPipelineLayout) pipelineLayout
      poke (p `plusPtr` 16 :: Ptr VkShaderStageFlags) stageFlags


instance ToCStruct PhysicalDeviceFeatures2 VkPhysicalDeviceFeatures2 where
  pokeCStruct :: Ptr VkPhysicalDeviceFeatures2 -> PhysicalDeviceFeatures2 -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFeatures2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) features . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceProperties2 VkPhysicalDeviceProperties2 where
  pokeCStruct :: Ptr VkPhysicalDeviceProperties2 -> PhysicalDeviceProperties2 -> IO a -> IO a
  pokeCStruct p PhysicalDeviceProperties2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) properties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct FormatProperties2 VkFormatProperties2 where
  pokeCStruct :: Ptr VkFormatProperties2 -> FormatProperties2 -> IO a -> IO a
  pokeCStruct p FormatProperties2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) formatProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_FORMAT_PROPERTIES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct ImageFormatProperties2 VkImageFormatProperties2 where
  pokeCStruct :: Ptr VkImageFormatProperties2 -> ImageFormatProperties2 -> IO a -> IO a
  pokeCStruct p ImageFormatProperties2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) imageFormatProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 where
  pokeCStruct :: Ptr VkPhysicalDeviceImageFormatInfo2 -> PhysicalDeviceImageFormatInfo2 -> IO a -> IO a
  pokeCStruct p PhysicalDeviceImageFormatInfo2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFormat) format
      poke (p `plusPtr` 20 :: Ptr VkImageType) type'
      poke (p `plusPtr` 24 :: Ptr VkImageTiling) tiling
      poke (p `plusPtr` 28 :: Ptr VkImageUsageFlags) usage
      poke (p `plusPtr` 32 :: Ptr VkImageCreateFlags) flags


instance ToCStruct QueueFamilyProperties2 VkQueueFamilyProperties2 where
  pokeCStruct :: Ptr VkQueueFamilyProperties2 -> QueueFamilyProperties2 -> IO a -> IO a
  pokeCStruct p QueueFamilyProperties2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) queueFamilyProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 where
  pokeCStruct :: Ptr VkPhysicalDeviceMemoryProperties2 -> PhysicalDeviceMemoryProperties2 -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMemoryProperties2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) memoryProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct SparseImageFormatProperties2 VkSparseImageFormatProperties2 where
  pokeCStruct :: Ptr VkSparseImageFormatProperties2 -> SparseImageFormatProperties2 -> IO a -> IO a
  pokeCStruct p SparseImageFormatProperties2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) properties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 where
  pokeCStruct :: Ptr VkPhysicalDeviceSparseImageFormatInfo2 -> PhysicalDeviceSparseImageFormatInfo2 -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSparseImageFormatInfo2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFormat) format
      poke (p `plusPtr` 20 :: Ptr VkImageType) type'
      poke (p `plusPtr` 24 :: Ptr VkSampleCountFlagBits) samples
      poke (p `plusPtr` 28 :: Ptr VkImageUsageFlags) usage
      poke (p `plusPtr` 32 :: Ptr VkImageTiling) tiling


instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR where
  pokeCStruct :: Ptr VkPhysicalDevicePushDescriptorPropertiesKHR -> PhysicalDevicePushDescriptorPropertiesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDevicePushDescriptorPropertiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxPushDescriptors


instance ToCStruct ConformanceVersionKHR VkConformanceVersionKHR where
  pokeCStruct :: Ptr VkConformanceVersionKHR -> ConformanceVersionKHR -> IO a -> IO a
  pokeCStruct p ConformanceVersionKHR{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word8) major
      poke (p `plusPtr` 1 :: Ptr Word8) minor
      poke (p `plusPtr` 2 :: Ptr Word8) subminor
      poke (p `plusPtr` 3 :: Ptr Word8) patch


instance ToCStruct PhysicalDeviceDriverPropertiesKHR VkPhysicalDeviceDriverPropertiesKHR where
  pokeCStruct :: Ptr VkPhysicalDeviceDriverPropertiesKHR -> PhysicalDeviceDriverPropertiesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDriverPropertiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 532) conformanceVersion . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDriverIdKHR) driverID
      pokeFixedLengthNullTerminatedByteString VK_MAX_DRIVER_NAME_SIZE_KHR (p `plusPtr` 20) driverName
      pokeFixedLengthNullTerminatedByteString VK_MAX_DRIVER_INFO_SIZE_KHR (p `plusPtr` 276) driverInfo


instance ToCStruct PresentRegionsKHR VkPresentRegionsKHR where
  pokeCStruct :: Ptr VkPresentRegionsKHR -> PresentRegionsKHR -> IO a -> IO a
  pokeCStruct p PresentRegionsKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pRegions <- ContT $ either (const ($ nullPtr)) (allocaArray @VkPresentRegionKHR . Data.Vector.length) regions
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pRegions `advancePtr` i) s . ($ ()))) regions
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PRESENT_REGIONS_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) regions :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkPresentRegionKHR)) pRegions


instance ToCStruct PresentRegionKHR VkPresentRegionKHR where
  pokeCStruct :: Ptr VkPresentRegionKHR -> PresentRegionKHR -> IO a -> IO a
  pokeCStruct p PresentRegionKHR{..} = (. const) . runContT $ do
    pRectangles <- ContT $ either (const ($ nullPtr)) (allocaArray @VkRectLayerKHR . Data.Vector.length) rectangles
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pRectangles `advancePtr` i) s . ($ ()))) rectangles
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) rectangles :: Word32)
      poke (p `plusPtr` 8 :: Ptr (Ptr VkRectLayerKHR)) pRectangles


instance ToCStruct RectLayerKHR VkRectLayerKHR where
  pokeCStruct :: Ptr VkRectLayerKHR -> RectLayerKHR -> IO a -> IO a
  pokeCStruct p RectLayerKHR{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) offset . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 8) extent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 16 :: Ptr Word32) layer


instance ToCStruct PhysicalDeviceVariablePointersFeatures VkPhysicalDeviceVariablePointersFeatures where
  pokeCStruct :: Ptr VkPhysicalDeviceVariablePointersFeatures -> PhysicalDeviceVariablePointersFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDeviceVariablePointersFeatures{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 variablePointersStorageBuffer :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 variablePointers :: VkBool32)


instance ToCStruct ExternalMemoryProperties VkExternalMemoryProperties where
  pokeCStruct :: Ptr VkExternalMemoryProperties -> ExternalMemoryProperties -> IO a -> IO a
  pokeCStruct p ExternalMemoryProperties{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkExternalMemoryFeatureFlags) externalMemoryFeatures
      poke (p `plusPtr` 4 :: Ptr VkExternalMemoryHandleTypeFlags) exportFromImportedHandleTypes
      poke (p `plusPtr` 8 :: Ptr VkExternalMemoryHandleTypeFlags) compatibleHandleTypes


instance ToCStruct PhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo where
  pokeCStruct :: Ptr VkPhysicalDeviceExternalImageFormatInfo -> PhysicalDeviceExternalImageFormatInfo -> IO a -> IO a
  pokeCStruct p PhysicalDeviceExternalImageFormatInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType


instance ToCStruct ExternalImageFormatProperties VkExternalImageFormatProperties where
  pokeCStruct :: Ptr VkExternalImageFormatProperties -> ExternalImageFormatProperties -> IO a -> IO a
  pokeCStruct p ExternalImageFormatProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) externalMemoryProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo where
  pokeCStruct :: Ptr VkPhysicalDeviceExternalBufferInfo -> PhysicalDeviceExternalBufferInfo -> IO a -> IO a
  pokeCStruct p PhysicalDeviceExternalBufferInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBufferCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkBufferUsageFlags) usage
      poke (p `plusPtr` 24 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType


instance ToCStruct ExternalBufferProperties VkExternalBufferProperties where
  pokeCStruct :: Ptr VkExternalBufferProperties -> ExternalBufferProperties -> IO a -> IO a
  pokeCStruct p ExternalBufferProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) externalMemoryProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceIDProperties VkPhysicalDeviceIDProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceIDProperties -> PhysicalDeviceIDProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceIDProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      pokeFixedLengthByteString VK_UUID_SIZE (p `plusPtr` 16) deviceUUID
      pokeFixedLengthByteString VK_UUID_SIZE (p `plusPtr` 32) driverUUID
      pokeFixedLengthByteString VK_LUID_SIZE (p `plusPtr` 48) deviceLUID
      poke (p `plusPtr` 56 :: Ptr Word32) deviceNodeMask
      poke (p `plusPtr` 60 :: Ptr VkBool32) (boolToBool32 deviceLUIDValid :: VkBool32)


instance ToCStruct ExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo where
  pokeCStruct :: Ptr VkExternalMemoryImageCreateInfo -> ExternalMemoryImageCreateInfo -> IO a -> IO a
  pokeCStruct p ExternalMemoryImageCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlags) handleTypes


instance ToCStruct ExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo where
  pokeCStruct :: Ptr VkExternalMemoryBufferCreateInfo -> ExternalMemoryBufferCreateInfo -> IO a -> IO a
  pokeCStruct p ExternalMemoryBufferCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlags) handleTypes


instance ToCStruct ExportMemoryAllocateInfo VkExportMemoryAllocateInfo where
  pokeCStruct :: Ptr VkExportMemoryAllocateInfo -> ExportMemoryAllocateInfo -> IO a -> IO a
  pokeCStruct p ExportMemoryAllocateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlags) handleTypes


instance ToCStruct ImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkImportMemoryWin32HandleInfoKHR -> ImportMemoryWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p ImportMemoryWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType
      poke (p `plusPtr` 24 :: Ptr HANDLE) handle
      poke (p `plusPtr` 32 :: Ptr LPCWSTR) name


instance ToCStruct ExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkExportMemoryWin32HandleInfoKHR -> ExportMemoryWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p ExportMemoryWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)) (attributes :: Ptr SECURITY_ATTRIBUTES)
      poke (p `plusPtr` 24 :: Ptr DWORD) dwAccess
      poke (p `plusPtr` 32 :: Ptr LPCWSTR) name


instance ToCStruct MemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR where
  pokeCStruct :: Ptr VkMemoryWin32HandlePropertiesKHR -> MemoryWin32HandlePropertiesKHR -> IO a -> IO a
  pokeCStruct p MemoryWin32HandlePropertiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) memoryTypeBits


instance ToCStruct MemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkMemoryGetWin32HandleInfoKHR -> MemoryGetWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p MemoryGetWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 24 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType


instance ToCStruct ImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR where
  pokeCStruct :: Ptr VkImportMemoryFdInfoKHR -> ImportMemoryFdInfoKHR -> IO a -> IO a
  pokeCStruct p ImportMemoryFdInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType
      poke (p `plusPtr` 20 :: Ptr CInt) fd


instance ToCStruct MemoryFdPropertiesKHR VkMemoryFdPropertiesKHR where
  pokeCStruct :: Ptr VkMemoryFdPropertiesKHR -> MemoryFdPropertiesKHR -> IO a -> IO a
  pokeCStruct p MemoryFdPropertiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) memoryTypeBits


instance ToCStruct MemoryGetFdInfoKHR VkMemoryGetFdInfoKHR where
  pokeCStruct :: Ptr VkMemoryGetFdInfoKHR -> MemoryGetFdInfoKHR -> IO a -> IO a
  pokeCStruct p MemoryGetFdInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 24 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType


instance ToCStruct Win32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR where
  pokeCStruct :: Ptr VkWin32KeyedMutexAcquireReleaseInfoKHR -> Win32KeyedMutexAcquireReleaseInfoKHR -> IO a -> IO a
  pokeCStruct p Win32KeyedMutexAcquireReleaseInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAcquireSyncs <- ContT $ allocaArray @VkDeviceMemory (Data.Vector.length acquireSyncs)
    pAcquireKeys <- ContT $ allocaArray @Word64 (Data.Vector.length acquireKeys)
    pAcquireTimeouts <- ContT $ allocaArray @Word32 (Data.Vector.length acquireTimeouts)
    pReleaseSyncs <- ContT $ allocaArray @VkDeviceMemory (Data.Vector.length releaseSyncs)
    pReleaseKeys <- ContT $ allocaArray @Word64 (Data.Vector.length releaseKeys)
    liftIO $ do
      acquireCount <- let l = Data.Vector.length acquireSyncs in if l == Data.Vector.length acquireKeys && l == Data.Vector.length acquireTimeouts then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "acquireSyncs, acquireKeys and acquireTimeouts must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pAcquireSyncs) acquireSyncs
      Data.Vector.imapM_ (pokeElemOff pAcquireKeys) acquireKeys
      Data.Vector.imapM_ (pokeElemOff pAcquireTimeouts) acquireTimeouts
      releaseCount <- let l = Data.Vector.length releaseSyncs in if l == Data.Vector.length releaseKeys then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "releaseSyncs and releaseKeys must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pReleaseSyncs) releaseSyncs
      Data.Vector.imapM_ (pokeElemOff pReleaseKeys) releaseKeys
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) acquireCount
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDeviceMemory)) pAcquireSyncs
      poke (p `plusPtr` 32 :: Ptr (Ptr Word64)) pAcquireKeys
      poke (p `plusPtr` 40 :: Ptr (Ptr Word32)) pAcquireTimeouts
      poke (p `plusPtr` 48 :: Ptr Word32) releaseCount
      poke (p `plusPtr` 56 :: Ptr (Ptr VkDeviceMemory)) pReleaseSyncs
      poke (p `plusPtr` 64 :: Ptr (Ptr Word64)) pReleaseKeys


instance ToCStruct PhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo where
  pokeCStruct :: Ptr VkPhysicalDeviceExternalSemaphoreInfo -> PhysicalDeviceExternalSemaphoreInfo -> IO a -> IO a
  pokeCStruct p PhysicalDeviceExternalSemaphoreInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalSemaphoreHandleTypeFlagBits) handleType


instance ToCStruct ExternalSemaphoreProperties VkExternalSemaphoreProperties where
  pokeCStruct :: Ptr VkExternalSemaphoreProperties -> ExternalSemaphoreProperties -> IO a -> IO a
  pokeCStruct p ExternalSemaphoreProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalSemaphoreHandleTypeFlags) exportFromImportedHandleTypes
      poke (p `plusPtr` 20 :: Ptr VkExternalSemaphoreHandleTypeFlags) compatibleHandleTypes
      poke (p `plusPtr` 24 :: Ptr VkExternalSemaphoreFeatureFlags) externalSemaphoreFeatures


instance ToCStruct ExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo where
  pokeCStruct :: Ptr VkExportSemaphoreCreateInfo -> ExportSemaphoreCreateInfo -> IO a -> IO a
  pokeCStruct p ExportSemaphoreCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalSemaphoreHandleTypeFlags) handleTypes


instance ToCStruct ImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkImportSemaphoreWin32HandleInfoKHR -> ImportSemaphoreWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p ImportSemaphoreWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSemaphore) semaphore
      poke (p `plusPtr` 24 :: Ptr VkSemaphoreImportFlags) flags
      poke (p `plusPtr` 28 :: Ptr VkExternalSemaphoreHandleTypeFlagBits) handleType
      poke (p `plusPtr` 32 :: Ptr HANDLE) handle
      poke (p `plusPtr` 40 :: Ptr LPCWSTR) name


instance ToCStruct ExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkExportSemaphoreWin32HandleInfoKHR -> ExportSemaphoreWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p ExportSemaphoreWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)) (attributes :: Ptr SECURITY_ATTRIBUTES)
      poke (p `plusPtr` 24 :: Ptr DWORD) dwAccess
      poke (p `plusPtr` 32 :: Ptr LPCWSTR) name


instance ToCStruct D3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR where
  pokeCStruct :: Ptr VkD3D12FenceSubmitInfoKHR -> D3D12FenceSubmitInfoKHR -> IO a -> IO a
  pokeCStruct p D3D12FenceSubmitInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pWaitSemaphoreValues <- ContT $ either (const ($ nullPtr)) (allocaArray @Word64 . Data.Vector.length) waitSemaphoreValues
    pSignalSemaphoreValues <- ContT $ either (const ($ nullPtr)) (allocaArray @Word64 . Data.Vector.length) signalSemaphoreValues
    liftIO $ do
      either (const (pure ())) (Data.Vector.imapM_ (pokeElemOff pWaitSemaphoreValues)) waitSemaphoreValues
      either (const (pure ())) (Data.Vector.imapM_ (pokeElemOff pSignalSemaphoreValues)) signalSemaphoreValues
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) waitSemaphoreValues :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word64)) pWaitSemaphoreValues
      poke (p `plusPtr` 32 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) signalSemaphoreValues :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr Word64)) pSignalSemaphoreValues


instance ToCStruct SemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkSemaphoreGetWin32HandleInfoKHR -> SemaphoreGetWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p SemaphoreGetWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSemaphore) semaphore
      poke (p `plusPtr` 24 :: Ptr VkExternalSemaphoreHandleTypeFlagBits) handleType


instance ToCStruct ImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR where
  pokeCStruct :: Ptr VkImportSemaphoreFdInfoKHR -> ImportSemaphoreFdInfoKHR -> IO a -> IO a
  pokeCStruct p ImportSemaphoreFdInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSemaphore) semaphore
      poke (p `plusPtr` 24 :: Ptr VkSemaphoreImportFlags) flags
      poke (p `plusPtr` 28 :: Ptr VkExternalSemaphoreHandleTypeFlagBits) handleType
      poke (p `plusPtr` 32 :: Ptr CInt) fd


instance ToCStruct SemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR where
  pokeCStruct :: Ptr VkSemaphoreGetFdInfoKHR -> SemaphoreGetFdInfoKHR -> IO a -> IO a
  pokeCStruct p SemaphoreGetFdInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSemaphore) semaphore
      poke (p `plusPtr` 24 :: Ptr VkExternalSemaphoreHandleTypeFlagBits) handleType


instance ToCStruct PhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo where
  pokeCStruct :: Ptr VkPhysicalDeviceExternalFenceInfo -> PhysicalDeviceExternalFenceInfo -> IO a -> IO a
  pokeCStruct p PhysicalDeviceExternalFenceInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalFenceHandleTypeFlagBits) handleType


instance ToCStruct ExternalFenceProperties VkExternalFenceProperties where
  pokeCStruct :: Ptr VkExternalFenceProperties -> ExternalFenceProperties -> IO a -> IO a
  pokeCStruct p ExternalFenceProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalFenceHandleTypeFlags) exportFromImportedHandleTypes
      poke (p `plusPtr` 20 :: Ptr VkExternalFenceHandleTypeFlags) compatibleHandleTypes
      poke (p `plusPtr` 24 :: Ptr VkExternalFenceFeatureFlags) externalFenceFeatures


instance ToCStruct ExportFenceCreateInfo VkExportFenceCreateInfo where
  pokeCStruct :: Ptr VkExportFenceCreateInfo -> ExportFenceCreateInfo -> IO a -> IO a
  pokeCStruct p ExportFenceCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalFenceHandleTypeFlags) handleTypes


instance ToCStruct ImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkImportFenceWin32HandleInfoKHR -> ImportFenceWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p ImportFenceWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFence) fence
      poke (p `plusPtr` 24 :: Ptr VkFenceImportFlags) flags
      poke (p `plusPtr` 28 :: Ptr VkExternalFenceHandleTypeFlagBits) handleType
      poke (p `plusPtr` 32 :: Ptr HANDLE) handle
      poke (p `plusPtr` 40 :: Ptr LPCWSTR) name


instance ToCStruct ExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkExportFenceWin32HandleInfoKHR -> ExportFenceWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p ExportFenceWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr SECURITY_ATTRIBUTES)) (attributes :: Ptr SECURITY_ATTRIBUTES)
      poke (p `plusPtr` 24 :: Ptr DWORD) dwAccess
      poke (p `plusPtr` 32 :: Ptr LPCWSTR) name


instance ToCStruct FenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR where
  pokeCStruct :: Ptr VkFenceGetWin32HandleInfoKHR -> FenceGetWin32HandleInfoKHR -> IO a -> IO a
  pokeCStruct p FenceGetWin32HandleInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFence) fence
      poke (p `plusPtr` 24 :: Ptr VkExternalFenceHandleTypeFlagBits) handleType


instance ToCStruct ImportFenceFdInfoKHR VkImportFenceFdInfoKHR where
  pokeCStruct :: Ptr VkImportFenceFdInfoKHR -> ImportFenceFdInfoKHR -> IO a -> IO a
  pokeCStruct p ImportFenceFdInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFence) fence
      poke (p `plusPtr` 24 :: Ptr VkFenceImportFlags) flags
      poke (p `plusPtr` 28 :: Ptr VkExternalFenceHandleTypeFlagBits) handleType
      poke (p `plusPtr` 32 :: Ptr CInt) fd


instance ToCStruct FenceGetFdInfoKHR VkFenceGetFdInfoKHR where
  pokeCStruct :: Ptr VkFenceGetFdInfoKHR -> FenceGetFdInfoKHR -> IO a -> IO a
  pokeCStruct p FenceGetFdInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFence) fence
      poke (p `plusPtr` 24 :: Ptr VkExternalFenceHandleTypeFlagBits) handleType


instance ToCStruct PhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures where
  pokeCStruct :: Ptr VkPhysicalDeviceMultiviewFeatures -> PhysicalDeviceMultiviewFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMultiviewFeatures{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 multiview :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 multiviewGeometryShader :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 multiviewTessellationShader :: VkBool32)


instance ToCStruct PhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceMultiviewProperties -> PhysicalDeviceMultiviewProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMultiviewProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxMultiviewViewCount
      poke (p `plusPtr` 20 :: Ptr Word32) maxMultiviewInstanceIndex


instance ToCStruct RenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo where
  pokeCStruct :: Ptr VkRenderPassMultiviewCreateInfo -> RenderPassMultiviewCreateInfo -> IO a -> IO a
  pokeCStruct p RenderPassMultiviewCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pViewMasks <- ContT $ allocaArray @Word32 (Data.Vector.length viewMasks)
    pViewOffsets <- ContT $ allocaArray @Int32 (Data.Vector.length viewOffsets)
    pCorrelationMasks <- ContT $ allocaArray @Word32 (Data.Vector.length correlationMasks)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pViewMasks) viewMasks
      Data.Vector.imapM_ (pokeElemOff pViewOffsets) viewOffsets
      Data.Vector.imapM_ (pokeElemOff pCorrelationMasks) correlationMasks
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length viewMasks :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word32)) pViewMasks
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length viewOffsets :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr Int32)) pViewOffsets
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length correlationMasks :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr Word32)) pCorrelationMasks


instance ToCStruct SurfaceCapabilities2EXT VkSurfaceCapabilities2EXT where
  pokeCStruct :: Ptr VkSurfaceCapabilities2EXT -> SurfaceCapabilities2EXT -> IO a -> IO a
  pokeCStruct p SurfaceCapabilities2EXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 24) currentExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 32) minImageExtent . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 40) maxImageExtent . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) minImageCount
      poke (p `plusPtr` 20 :: Ptr Word32) maxImageCount
      poke (p `plusPtr` 48 :: Ptr Word32) maxImageArrayLayers
      poke (p `plusPtr` 52 :: Ptr VkSurfaceTransformFlagsKHR) supportedTransforms
      poke (p `plusPtr` 56 :: Ptr VkSurfaceTransformFlagBitsKHR) currentTransform
      poke (p `plusPtr` 60 :: Ptr VkCompositeAlphaFlagsKHR) supportedCompositeAlpha
      poke (p `plusPtr` 64 :: Ptr VkImageUsageFlags) supportedUsageFlags
      poke (p `plusPtr` 68 :: Ptr VkSurfaceCounterFlagsEXT) supportedSurfaceCounters


instance ToCStruct DisplayPowerInfoEXT VkDisplayPowerInfoEXT where
  pokeCStruct :: Ptr VkDisplayPowerInfoEXT -> DisplayPowerInfoEXT -> IO a -> IO a
  pokeCStruct p DisplayPowerInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDisplayPowerStateEXT) powerState


instance ToCStruct DeviceEventInfoEXT VkDeviceEventInfoEXT where
  pokeCStruct :: Ptr VkDeviceEventInfoEXT -> DeviceEventInfoEXT -> IO a -> IO a
  pokeCStruct p DeviceEventInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceEventTypeEXT) deviceEvent


instance ToCStruct DisplayEventInfoEXT VkDisplayEventInfoEXT where
  pokeCStruct :: Ptr VkDisplayEventInfoEXT -> DisplayEventInfoEXT -> IO a -> IO a
  pokeCStruct p DisplayEventInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDisplayEventTypeEXT) displayEvent


instance ToCStruct SwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT where
  pokeCStruct :: Ptr VkSwapchainCounterCreateInfoEXT -> SwapchainCounterCreateInfoEXT -> IO a -> IO a
  pokeCStruct p SwapchainCounterCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSurfaceCounterFlagsEXT) surfaceCounters


instance ToCStruct PhysicalDeviceGroupProperties VkPhysicalDeviceGroupProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceGroupProperties -> PhysicalDeviceGroupProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceGroupProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length physicalDevices :: Word32)
      Data.Vector.imapM_ (\i -> pokeElemOff (p `plusPtr` 24) i . physicalDeviceHandle) (Data.Vector.take VK_MAX_DEVICE_GROUP_SIZE physicalDevices)
      poke (p `plusPtr` 280 :: Ptr VkBool32) (boolToBool32 subsetAllocation :: VkBool32)


instance ToCStruct MemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo where
  pokeCStruct :: Ptr VkMemoryAllocateFlagsInfo -> MemoryAllocateFlagsInfo -> IO a -> IO a
  pokeCStruct p MemoryAllocateFlagsInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkMemoryAllocateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) deviceMask


instance ToCStruct BindBufferMemoryInfo VkBindBufferMemoryInfo where
  pokeCStruct :: Ptr VkBindBufferMemoryInfo -> BindBufferMemoryInfo -> IO a -> IO a
  pokeCStruct p BindBufferMemoryInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 24 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) memoryOffset


instance ToCStruct BindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo where
  pokeCStruct :: Ptr VkBindBufferMemoryDeviceGroupInfo -> BindBufferMemoryDeviceGroupInfo -> IO a -> IO a
  pokeCStruct p BindBufferMemoryDeviceGroupInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDeviceIndices <- ContT $ allocaArray @Word32 (Data.Vector.length deviceIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDeviceIndices) deviceIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length deviceIndices :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word32)) pDeviceIndices


instance ToCStruct BindImageMemoryInfo VkBindImageMemoryInfo where
  pokeCStruct :: Ptr VkBindImageMemoryInfo -> BindImageMemoryInfo -> IO a -> IO a
  pokeCStruct p BindImageMemoryInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImage) image
      poke (p `plusPtr` 24 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) memoryOffset


instance ToCStruct BindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo where
  pokeCStruct :: Ptr VkBindImageMemoryDeviceGroupInfo -> BindImageMemoryDeviceGroupInfo -> IO a -> IO a
  pokeCStruct p BindImageMemoryDeviceGroupInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDeviceIndices <- ContT $ allocaArray @Word32 (Data.Vector.length deviceIndices)
    pSplitInstanceBindRegions <- ContT $ allocaArray @VkRect2D (Data.Vector.length splitInstanceBindRegions)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pSplitInstanceBindRegions `advancePtr` i) s . ($ ())) splitInstanceBindRegions
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDeviceIndices) deviceIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length deviceIndices :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word32)) pDeviceIndices
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length splitInstanceBindRegions :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkRect2D)) pSplitInstanceBindRegions


instance ToCStruct DeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo where
  pokeCStruct :: Ptr VkDeviceGroupRenderPassBeginInfo -> DeviceGroupRenderPassBeginInfo -> IO a -> IO a
  pokeCStruct p DeviceGroupRenderPassBeginInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDeviceRenderAreas <- ContT $ allocaArray @VkRect2D (Data.Vector.length deviceRenderAreas)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pDeviceRenderAreas `advancePtr` i) s . ($ ())) deviceRenderAreas
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) deviceMask
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length deviceRenderAreas :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkRect2D)) pDeviceRenderAreas


instance ToCStruct DeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo where
  pokeCStruct :: Ptr VkDeviceGroupCommandBufferBeginInfo -> DeviceGroupCommandBufferBeginInfo -> IO a -> IO a
  pokeCStruct p DeviceGroupCommandBufferBeginInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) deviceMask


instance ToCStruct DeviceGroupSubmitInfo VkDeviceGroupSubmitInfo where
  pokeCStruct :: Ptr VkDeviceGroupSubmitInfo -> DeviceGroupSubmitInfo -> IO a -> IO a
  pokeCStruct p DeviceGroupSubmitInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pWaitSemaphoreDeviceIndices <- ContT $ allocaArray @Word32 (Data.Vector.length waitSemaphoreDeviceIndices)
    pCommandBufferDeviceMasks <- ContT $ allocaArray @Word32 (Data.Vector.length commandBufferDeviceMasks)
    pSignalSemaphoreDeviceIndices <- ContT $ allocaArray @Word32 (Data.Vector.length signalSemaphoreDeviceIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pWaitSemaphoreDeviceIndices) waitSemaphoreDeviceIndices
      Data.Vector.imapM_ (pokeElemOff pCommandBufferDeviceMasks) commandBufferDeviceMasks
      Data.Vector.imapM_ (pokeElemOff pSignalSemaphoreDeviceIndices) signalSemaphoreDeviceIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length waitSemaphoreDeviceIndices :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word32)) pWaitSemaphoreDeviceIndices
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length commandBufferDeviceMasks :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr Word32)) pCommandBufferDeviceMasks
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length signalSemaphoreDeviceIndices :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr Word32)) pSignalSemaphoreDeviceIndices


instance ToCStruct DeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo where
  pokeCStruct :: Ptr VkDeviceGroupBindSparseInfo -> DeviceGroupBindSparseInfo -> IO a -> IO a
  pokeCStruct p DeviceGroupBindSparseInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) resourceDeviceIndex
      poke (p `plusPtr` 20 :: Ptr Word32) memoryDeviceIndex


instance ToCStruct DeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR where
  pokeCStruct :: Ptr VkDeviceGroupPresentCapabilitiesKHR -> DeviceGroupPresentCapabilitiesKHR -> IO a -> IO a
  pokeCStruct p DeviceGroupPresentCapabilitiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      Data.Vector.imapM_ (pokeElemOff (p `plusPtr` 16)) (Data.Vector.take VK_MAX_DEVICE_GROUP_SIZE presentMask)
      poke (p `plusPtr` 144 :: Ptr VkDeviceGroupPresentModeFlagsKHR) modes


instance ToCStruct ImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR where
  pokeCStruct :: Ptr VkImageSwapchainCreateInfoKHR -> ImageSwapchainCreateInfoKHR -> IO a -> IO a
  pokeCStruct p ImageSwapchainCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSwapchainKHR) swapchain


instance ToCStruct BindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR where
  pokeCStruct :: Ptr VkBindImageMemorySwapchainInfoKHR -> BindImageMemorySwapchainInfoKHR -> IO a -> IO a
  pokeCStruct p BindImageMemorySwapchainInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSwapchainKHR) swapchain
      poke (p `plusPtr` 24 :: Ptr Word32) imageIndex


instance ToCStruct AcquireNextImageInfoKHR VkAcquireNextImageInfoKHR where
  pokeCStruct :: Ptr VkAcquireNextImageInfoKHR -> AcquireNextImageInfoKHR -> IO a -> IO a
  pokeCStruct p AcquireNextImageInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSwapchainKHR) swapchain
      poke (p `plusPtr` 24 :: Ptr Word64) timeout
      poke (p `plusPtr` 32 :: Ptr VkSemaphore) semaphore
      poke (p `plusPtr` 40 :: Ptr VkFence) fence
      poke (p `plusPtr` 48 :: Ptr Word32) deviceMask


instance ToCStruct DeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR where
  pokeCStruct :: Ptr VkDeviceGroupPresentInfoKHR -> DeviceGroupPresentInfoKHR -> IO a -> IO a
  pokeCStruct p DeviceGroupPresentInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDeviceMasks <- ContT $ allocaArray @Word32 (Data.Vector.length deviceMasks)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDeviceMasks) deviceMasks
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length deviceMasks :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word32)) pDeviceMasks
      poke (p `plusPtr` 32 :: Ptr VkDeviceGroupPresentModeFlagBitsKHR) mode


instance ToCStruct DeviceGroupDeviceCreateInfo VkDeviceGroupDeviceCreateInfo where
  pokeCStruct :: Ptr VkDeviceGroupDeviceCreateInfo -> DeviceGroupDeviceCreateInfo -> IO a -> IO a
  pokeCStruct p DeviceGroupDeviceCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pPhysicalDevices <- ContT $ allocaArray @VkPhysicalDevice (Data.Vector.length physicalDevices)
    liftIO $ do
      Data.Vector.imapM_ (\i -> pokeElemOff pPhysicalDevices i . physicalDeviceHandle) physicalDevices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length physicalDevices :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkPhysicalDevice)) pPhysicalDevices


instance ToCStruct DeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR where
  pokeCStruct :: Ptr VkDeviceGroupSwapchainCreateInfoKHR -> DeviceGroupSwapchainCreateInfoKHR -> IO a -> IO a
  pokeCStruct p DeviceGroupSwapchainCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceGroupPresentModeFlagsKHR) modes


instance ToCStruct DescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry where
  pokeCStruct :: Ptr VkDescriptorUpdateTemplateEntry -> DescriptorUpdateTemplateEntry -> IO a -> IO a
  pokeCStruct p DescriptorUpdateTemplateEntry{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) dstBinding
      poke (p `plusPtr` 4 :: Ptr Word32) dstArrayElement
      poke (p `plusPtr` 8 :: Ptr Word32) descriptorCount
      poke (p `plusPtr` 12 :: Ptr VkDescriptorType) descriptorType
      poke (p `plusPtr` 16 :: Ptr CSize) offset
      poke (p `plusPtr` 24 :: Ptr CSize) stride


instance ToCStruct DescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo where
  pokeCStruct :: Ptr VkDescriptorUpdateTemplateCreateInfo -> DescriptorUpdateTemplateCreateInfo -> IO a -> IO a
  pokeCStruct p DescriptorUpdateTemplateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDescriptorUpdateEntries <- ContT $ allocaArray @VkDescriptorUpdateTemplateEntry (Data.Vector.length descriptorUpdateEntries)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pDescriptorUpdateEntries `advancePtr` i) s . ($ ())) descriptorUpdateEntries
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDescriptorUpdateTemplateCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length descriptorUpdateEntries :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDescriptorUpdateTemplateEntry)) pDescriptorUpdateEntries
      poke (p `plusPtr` 32 :: Ptr VkDescriptorUpdateTemplateType) templateType
      poke (p `plusPtr` 40 :: Ptr VkDescriptorSetLayout) descriptorSetLayout
      poke (p `plusPtr` 48 :: Ptr VkPipelineBindPoint) pipelineBindPoint
      poke (p `plusPtr` 56 :: Ptr VkPipelineLayout) pipelineLayout
      poke (p `plusPtr` 64 :: Ptr Word32) set


instance ToCStruct XYColorEXT VkXYColorEXT where
  pokeCStruct :: Ptr VkXYColorEXT -> XYColorEXT -> IO a -> IO a
  pokeCStruct p XYColorEXT{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr CFloat) (CFloat x :: CFloat)
      poke (p `plusPtr` 4 :: Ptr CFloat) (CFloat y :: CFloat)


instance ToCStruct HdrMetadataEXT VkHdrMetadataEXT where
  pokeCStruct :: Ptr VkHdrMetadataEXT -> HdrMetadataEXT -> IO a -> IO a
  pokeCStruct p HdrMetadataEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) displayPrimaryRed . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 24) displayPrimaryGreen . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 32) displayPrimaryBlue . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 40) whitePoint . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_HDR_METADATA_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 48 :: Ptr CFloat) (CFloat maxLuminance :: CFloat)
      poke (p `plusPtr` 52 :: Ptr CFloat) (CFloat minLuminance :: CFloat)
      poke (p `plusPtr` 56 :: Ptr CFloat) (CFloat maxContentLightLevel :: CFloat)
      poke (p `plusPtr` 60 :: Ptr CFloat) (CFloat maxFrameAverageLightLevel :: CFloat)


instance ToCStruct DisplayNativeHdrSurfaceCapabilitiesAMD VkDisplayNativeHdrSurfaceCapabilitiesAMD where
  pokeCStruct :: Ptr VkDisplayNativeHdrSurfaceCapabilitiesAMD -> DisplayNativeHdrSurfaceCapabilitiesAMD -> IO a -> IO a
  pokeCStruct p DisplayNativeHdrSurfaceCapabilitiesAMD{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 localDimmingSupport :: VkBool32)


instance ToCStruct SwapchainDisplayNativeHdrCreateInfoAMD VkSwapchainDisplayNativeHdrCreateInfoAMD where
  pokeCStruct :: Ptr VkSwapchainDisplayNativeHdrCreateInfoAMD -> SwapchainDisplayNativeHdrCreateInfoAMD -> IO a -> IO a
  pokeCStruct p SwapchainDisplayNativeHdrCreateInfoAMD{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 localDimmingEnable :: VkBool32)


instance ToCStruct RefreshCycleDurationGOOGLE VkRefreshCycleDurationGOOGLE where
  pokeCStruct :: Ptr VkRefreshCycleDurationGOOGLE -> RefreshCycleDurationGOOGLE -> IO a -> IO a
  pokeCStruct p RefreshCycleDurationGOOGLE{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word64) refreshDuration


instance ToCStruct PastPresentationTimingGOOGLE VkPastPresentationTimingGOOGLE where
  pokeCStruct :: Ptr VkPastPresentationTimingGOOGLE -> PastPresentationTimingGOOGLE -> IO a -> IO a
  pokeCStruct p PastPresentationTimingGOOGLE{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) presentID
      poke (p `plusPtr` 8 :: Ptr Word64) desiredPresentTime
      poke (p `plusPtr` 16 :: Ptr Word64) actualPresentTime
      poke (p `plusPtr` 24 :: Ptr Word64) earliestPresentTime
      poke (p `plusPtr` 32 :: Ptr Word64) presentMargin


instance ToCStruct PresentTimesInfoGOOGLE VkPresentTimesInfoGOOGLE where
  pokeCStruct :: Ptr VkPresentTimesInfoGOOGLE -> PresentTimesInfoGOOGLE -> IO a -> IO a
  pokeCStruct p PresentTimesInfoGOOGLE{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pTimes <- ContT $ either (const ($ nullPtr)) (allocaArray @VkPresentTimeGOOGLE . Data.Vector.length) times
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pTimes `advancePtr` i) s . ($ ()))) times
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) times :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkPresentTimeGOOGLE)) pTimes


instance ToCStruct PresentTimeGOOGLE VkPresentTimeGOOGLE where
  pokeCStruct :: Ptr VkPresentTimeGOOGLE -> PresentTimeGOOGLE -> IO a -> IO a
  pokeCStruct p PresentTimeGOOGLE{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) presentID
      poke (p `plusPtr` 8 :: Ptr Word64) desiredPresentTime


instance ToCStruct IOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK where
  pokeCStruct :: Ptr VkIOSSurfaceCreateInfoMVK -> IOSSurfaceCreateInfoMVK -> IO a -> IO a
  pokeCStruct p IOSSurfaceCreateInfoMVK{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkIOSSurfaceCreateFlagsMVK) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (view :: Ptr ())


instance ToCStruct MacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK where
  pokeCStruct :: Ptr VkMacOSSurfaceCreateInfoMVK -> MacOSSurfaceCreateInfoMVK -> IO a -> IO a
  pokeCStruct p MacOSSurfaceCreateInfoMVK{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkMacOSSurfaceCreateFlagsMVK) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (view :: Ptr ())


instance ToCStruct MetalSurfaceCreateInfoEXT VkMetalSurfaceCreateInfoEXT where
  pokeCStruct :: Ptr VkMetalSurfaceCreateInfoEXT -> MetalSurfaceCreateInfoEXT -> IO a -> IO a
  pokeCStruct p MetalSurfaceCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkMetalSurfaceCreateFlagsEXT) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr CAMetalLayer)) (layer :: Ptr CAMetalLayer)


instance ToCStruct ViewportWScalingNV VkViewportWScalingNV where
  pokeCStruct :: Ptr VkViewportWScalingNV -> ViewportWScalingNV -> IO a -> IO a
  pokeCStruct p ViewportWScalingNV{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr CFloat) (CFloat xcoeff :: CFloat)
      poke (p `plusPtr` 4 :: Ptr CFloat) (CFloat ycoeff :: CFloat)


instance ToCStruct PipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineViewportWScalingStateCreateInfoNV -> PipelineViewportWScalingStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineViewportWScalingStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pViewportWScalings <- ContT $ either (const ($ nullPtr)) (allocaArray @VkViewportWScalingNV . Data.Vector.length) viewportWScalings
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pViewportWScalings `advancePtr` i) s . ($ ()))) viewportWScalings
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 viewportWScalingEnable :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) viewportWScalings :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkViewportWScalingNV)) pViewportWScalings


instance ToCStruct ViewportSwizzleNV VkViewportSwizzleNV where
  pokeCStruct :: Ptr VkViewportSwizzleNV -> ViewportSwizzleNV -> IO a -> IO a
  pokeCStruct p ViewportSwizzleNV{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkViewportCoordinateSwizzleNV) x
      poke (p `plusPtr` 4 :: Ptr VkViewportCoordinateSwizzleNV) y
      poke (p `plusPtr` 8 :: Ptr VkViewportCoordinateSwizzleNV) z
      poke (p `plusPtr` 12 :: Ptr VkViewportCoordinateSwizzleNV) w


instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineViewportSwizzleStateCreateInfoNV -> PipelineViewportSwizzleStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineViewportSwizzleStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pViewportSwizzles <- ContT $ allocaArray @VkViewportSwizzleNV (Data.Vector.length viewportSwizzles)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pViewportSwizzles `advancePtr` i) s . ($ ())) viewportSwizzles
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineViewportSwizzleStateCreateFlagsNV) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length viewportSwizzles :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkViewportSwizzleNV)) pViewportSwizzles


instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceDiscardRectanglePropertiesEXT -> PhysicalDeviceDiscardRectanglePropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDiscardRectanglePropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxDiscardRectangles


instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineDiscardRectangleStateCreateInfoEXT -> PipelineDiscardRectangleStateCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineDiscardRectangleStateCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDiscardRectangles <- ContT $ either (const ($ nullPtr)) (allocaArray @VkRect2D . Data.Vector.length) discardRectangles
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pDiscardRectangles `advancePtr` i) s . ($ ()))) discardRectangles
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineDiscardRectangleStateCreateFlagsEXT) flags
      poke (p `plusPtr` 20 :: Ptr VkDiscardRectangleModeEXT) discardRectangleMode
      poke (p `plusPtr` 24 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) discardRectangles :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkRect2D)) pDiscardRectangles


instance ToCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  pokeCStruct :: Ptr VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 perViewPositionAllComponents :: VkBool32)


instance ToCStruct InputAttachmentAspectReference VkInputAttachmentAspectReference where
  pokeCStruct :: Ptr VkInputAttachmentAspectReference -> InputAttachmentAspectReference -> IO a -> IO a
  pokeCStruct p InputAttachmentAspectReference{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) subpass
      poke (p `plusPtr` 4 :: Ptr Word32) inputAttachmentIndex
      poke (p `plusPtr` 8 :: Ptr VkImageAspectFlags) aspectMask


instance ToCStruct RenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo where
  pokeCStruct :: Ptr VkRenderPassInputAttachmentAspectCreateInfo -> RenderPassInputAttachmentAspectCreateInfo -> IO a -> IO a
  pokeCStruct p RenderPassInputAttachmentAspectCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAspectReferences <- ContT $ allocaArray @VkInputAttachmentAspectReference (Data.Vector.length aspectReferences)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pAspectReferences `advancePtr` i) s . ($ ())) aspectReferences
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length aspectReferences :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkInputAttachmentAspectReference)) pAspectReferences


instance ToCStruct PhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR where
  pokeCStruct :: Ptr VkPhysicalDeviceSurfaceInfo2KHR -> PhysicalDeviceSurfaceInfo2KHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSurfaceInfo2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSurfaceKHR) surface


instance ToCStruct SurfaceCapabilities2KHR VkSurfaceCapabilities2KHR where
  pokeCStruct :: Ptr VkSurfaceCapabilities2KHR -> SurfaceCapabilities2KHR -> IO a -> IO a
  pokeCStruct p SurfaceCapabilities2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) surfaceCapabilities . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct SurfaceFormat2KHR VkSurfaceFormat2KHR where
  pokeCStruct :: Ptr VkSurfaceFormat2KHR -> SurfaceFormat2KHR -> IO a -> IO a
  pokeCStruct p SurfaceFormat2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) surfaceFormat . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct DisplayProperties2KHR VkDisplayProperties2KHR where
  pokeCStruct :: Ptr VkDisplayProperties2KHR -> DisplayProperties2KHR -> IO a -> IO a
  pokeCStruct p DisplayProperties2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) displayProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct DisplayPlaneProperties2KHR VkDisplayPlaneProperties2KHR where
  pokeCStruct :: Ptr VkDisplayPlaneProperties2KHR -> DisplayPlaneProperties2KHR -> IO a -> IO a
  pokeCStruct p DisplayPlaneProperties2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) displayPlaneProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct DisplayModeProperties2KHR VkDisplayModeProperties2KHR where
  pokeCStruct :: Ptr VkDisplayModeProperties2KHR -> DisplayModeProperties2KHR -> IO a -> IO a
  pokeCStruct p DisplayModeProperties2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) displayModeProperties . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct DisplayPlaneInfo2KHR VkDisplayPlaneInfo2KHR where
  pokeCStruct :: Ptr VkDisplayPlaneInfo2KHR -> DisplayPlaneInfo2KHR -> IO a -> IO a
  pokeCStruct p DisplayPlaneInfo2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDisplayModeKHR) mode
      poke (p `plusPtr` 24 :: Ptr Word32) planeIndex


instance ToCStruct DisplayPlaneCapabilities2KHR VkDisplayPlaneCapabilities2KHR where
  pokeCStruct :: Ptr VkDisplayPlaneCapabilities2KHR -> DisplayPlaneCapabilities2KHR -> IO a -> IO a
  pokeCStruct p DisplayPlaneCapabilities2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) capabilities . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct SharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR where
  pokeCStruct :: Ptr VkSharedPresentSurfaceCapabilitiesKHR -> SharedPresentSurfaceCapabilitiesKHR -> IO a -> IO a
  pokeCStruct p SharedPresentSurfaceCapabilitiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageUsageFlags) sharedPresentSupportedUsageFlags


instance ToCStruct PhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures where
  pokeCStruct :: Ptr VkPhysicalDevice16BitStorageFeatures -> PhysicalDevice16BitStorageFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDevice16BitStorageFeatures{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 storageBuffer16BitAccess :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 uniformAndStorageBuffer16BitAccess :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 storagePushConstant16 :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 storageInputOutput16 :: VkBool32)


instance ToCStruct PhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceSubgroupProperties -> PhysicalDeviceSubgroupProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSubgroupProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) subgroupSize
      poke (p `plusPtr` 20 :: Ptr VkShaderStageFlags) supportedStages
      poke (p `plusPtr` 24 :: Ptr VkSubgroupFeatureFlags) supportedOperations
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 quadOperationsInAllStages :: VkBool32)


instance ToCStruct BufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 where
  pokeCStruct :: Ptr VkBufferMemoryRequirementsInfo2 -> BufferMemoryRequirementsInfo2 -> IO a -> IO a
  pokeCStruct p BufferMemoryRequirementsInfo2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBuffer) buffer


instance ToCStruct ImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 where
  pokeCStruct :: Ptr VkImageMemoryRequirementsInfo2 -> ImageMemoryRequirementsInfo2 -> IO a -> IO a
  pokeCStruct p ImageMemoryRequirementsInfo2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImage) image


instance ToCStruct ImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 where
  pokeCStruct :: Ptr VkImageSparseMemoryRequirementsInfo2 -> ImageSparseMemoryRequirementsInfo2 -> IO a -> IO a
  pokeCStruct p ImageSparseMemoryRequirementsInfo2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImage) image


instance ToCStruct MemoryRequirements2 VkMemoryRequirements2 where
  pokeCStruct :: Ptr VkMemoryRequirements2 -> MemoryRequirements2 -> IO a -> IO a
  pokeCStruct p MemoryRequirements2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) memoryRequirements . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct SparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 where
  pokeCStruct :: Ptr VkSparseImageMemoryRequirements2 -> SparseImageMemoryRequirements2 -> IO a -> IO a
  pokeCStruct p SparseImageMemoryRequirements2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) memoryRequirements . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties where
  pokeCStruct :: Ptr VkPhysicalDevicePointClippingProperties -> PhysicalDevicePointClippingProperties -> IO a -> IO a
  pokeCStruct p PhysicalDevicePointClippingProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPointClippingBehavior) pointClippingBehavior


instance ToCStruct MemoryDedicatedRequirements VkMemoryDedicatedRequirements where
  pokeCStruct :: Ptr VkMemoryDedicatedRequirements -> MemoryDedicatedRequirements -> IO a -> IO a
  pokeCStruct p MemoryDedicatedRequirements{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 prefersDedicatedAllocation :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 requiresDedicatedAllocation :: VkBool32)


instance ToCStruct MemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo where
  pokeCStruct :: Ptr VkMemoryDedicatedAllocateInfo -> MemoryDedicatedAllocateInfo -> IO a -> IO a
  pokeCStruct p MemoryDedicatedAllocateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImage) image
      poke (p `plusPtr` 24 :: Ptr VkBuffer) buffer


instance ToCStruct ImageViewUsageCreateInfo VkImageViewUsageCreateInfo where
  pokeCStruct :: Ptr VkImageViewUsageCreateInfo -> ImageViewUsageCreateInfo -> IO a -> IO a
  pokeCStruct p ImageViewUsageCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageUsageFlags) usage


instance ToCStruct PipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo where
  pokeCStruct :: Ptr VkPipelineTessellationDomainOriginStateCreateInfo -> PipelineTessellationDomainOriginStateCreateInfo -> IO a -> IO a
  pokeCStruct p PipelineTessellationDomainOriginStateCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkTessellationDomainOrigin) domainOrigin


instance ToCStruct SamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo where
  pokeCStruct :: Ptr VkSamplerYcbcrConversionInfo -> SamplerYcbcrConversionInfo -> IO a -> IO a
  pokeCStruct p SamplerYcbcrConversionInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSamplerYcbcrConversion) conversion


instance ToCStruct SamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo where
  pokeCStruct :: Ptr VkSamplerYcbcrConversionCreateInfo -> SamplerYcbcrConversionCreateInfo -> IO a -> IO a
  pokeCStruct p SamplerYcbcrConversionCreateInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 28) components . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFormat) format
      poke (p `plusPtr` 20 :: Ptr VkSamplerYcbcrModelConversion) ycbcrModel
      poke (p `plusPtr` 24 :: Ptr VkSamplerYcbcrRange) ycbcrRange
      poke (p `plusPtr` 44 :: Ptr VkChromaLocation) xChromaOffset
      poke (p `plusPtr` 48 :: Ptr VkChromaLocation) yChromaOffset
      poke (p `plusPtr` 52 :: Ptr VkFilter) chromaFilter
      poke (p `plusPtr` 56 :: Ptr VkBool32) (boolToBool32 forceExplicitReconstruction :: VkBool32)


instance ToCStruct BindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo where
  pokeCStruct :: Ptr VkBindImagePlaneMemoryInfo -> BindImagePlaneMemoryInfo -> IO a -> IO a
  pokeCStruct p BindImagePlaneMemoryInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageAspectFlagBits) planeAspect


instance ToCStruct ImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo where
  pokeCStruct :: Ptr VkImagePlaneMemoryRequirementsInfo -> ImagePlaneMemoryRequirementsInfo -> IO a -> IO a
  pokeCStruct p ImagePlaneMemoryRequirementsInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageAspectFlagBits) planeAspect


instance ToCStruct PhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  pokeCStruct :: Ptr VkPhysicalDeviceSamplerYcbcrConversionFeatures -> PhysicalDeviceSamplerYcbcrConversionFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSamplerYcbcrConversionFeatures{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 samplerYcbcrConversion :: VkBool32)


instance ToCStruct SamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties where
  pokeCStruct :: Ptr VkSamplerYcbcrConversionImageFormatProperties -> SamplerYcbcrConversionImageFormatProperties -> IO a -> IO a
  pokeCStruct p SamplerYcbcrConversionImageFormatProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) combinedImageSamplerDescriptorCount


instance ToCStruct TextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD where
  pokeCStruct :: Ptr VkTextureLODGatherFormatPropertiesAMD -> TextureLODGatherFormatPropertiesAMD -> IO a -> IO a
  pokeCStruct p TextureLODGatherFormatPropertiesAMD{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 supportsTextureGatherLODBiasAMD :: VkBool32)


instance ToCStruct ConditionalRenderingBeginInfoEXT VkConditionalRenderingBeginInfoEXT where
  pokeCStruct :: Ptr VkConditionalRenderingBeginInfoEXT -> ConditionalRenderingBeginInfoEXT -> IO a -> IO a
  pokeCStruct p ConditionalRenderingBeginInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBuffer) buffer
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) offset
      poke (p `plusPtr` 32 :: Ptr VkConditionalRenderingFlagsEXT) flags


instance ToCStruct ProtectedSubmitInfo VkProtectedSubmitInfo where
  pokeCStruct :: Ptr VkProtectedSubmitInfo -> ProtectedSubmitInfo -> IO a -> IO a
  pokeCStruct p ProtectedSubmitInfo{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 protectedSubmit :: VkBool32)


instance ToCStruct PhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures where
  pokeCStruct :: Ptr VkPhysicalDeviceProtectedMemoryFeatures -> PhysicalDeviceProtectedMemoryFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDeviceProtectedMemoryFeatures{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 protectedMemory :: VkBool32)


instance ToCStruct PhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties where
  pokeCStruct :: Ptr VkPhysicalDeviceProtectedMemoryProperties -> PhysicalDeviceProtectedMemoryProperties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceProtectedMemoryProperties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 protectedNoFault :: VkBool32)


instance ToCStruct DeviceQueueInfo2 VkDeviceQueueInfo2 where
  pokeCStruct :: Ptr VkDeviceQueueInfo2 -> DeviceQueueInfo2 -> IO a -> IO a
  pokeCStruct p DeviceQueueInfo2{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceQueueCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) queueFamilyIndex
      poke (p `plusPtr` 24 :: Ptr Word32) queueIndex


instance ToCStruct PipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineCoverageToColorStateCreateInfoNV -> PipelineCoverageToColorStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineCoverageToColorStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineCoverageToColorStateCreateFlagsNV) flags
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 coverageToColorEnable :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr Word32) coverageToColorLocation


instance ToCStruct PhysicalDeviceSamplerFilterMinmaxPropertiesEXT VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> PhysicalDeviceSamplerFilterMinmaxPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSamplerFilterMinmaxPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 filterMinmaxSingleComponentFormats :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 filterMinmaxImageComponentMapping :: VkBool32)


instance ToCStruct SampleLocationEXT VkSampleLocationEXT where
  pokeCStruct :: Ptr VkSampleLocationEXT -> SampleLocationEXT -> IO a -> IO a
  pokeCStruct p SampleLocationEXT{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr CFloat) (CFloat x :: CFloat)
      poke (p `plusPtr` 4 :: Ptr CFloat) (CFloat y :: CFloat)


instance ToCStruct SampleLocationsInfoEXT VkSampleLocationsInfoEXT where
  pokeCStruct :: Ptr VkSampleLocationsInfoEXT -> SampleLocationsInfoEXT -> IO a -> IO a
  pokeCStruct p SampleLocationsInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 20) sampleLocationGridSize . ($ ())
    pSampleLocations <- ContT $ allocaArray @VkSampleLocationEXT (Data.Vector.length sampleLocations)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pSampleLocations `advancePtr` i) s . ($ ())) sampleLocations
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSampleCountFlagBits) sampleLocationsPerPixel
      poke (p `plusPtr` 28 :: Ptr Word32) (fromIntegral $ Data.Vector.length sampleLocations :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkSampleLocationEXT)) pSampleLocations


instance ToCStruct AttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT where
  pokeCStruct :: Ptr VkAttachmentSampleLocationsEXT -> AttachmentSampleLocationsEXT -> IO a -> IO a
  pokeCStruct p AttachmentSampleLocationsEXT{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 8) sampleLocationsInfo . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) attachmentIndex


instance ToCStruct SubpassSampleLocationsEXT VkSubpassSampleLocationsEXT where
  pokeCStruct :: Ptr VkSubpassSampleLocationsEXT -> SubpassSampleLocationsEXT -> IO a -> IO a
  pokeCStruct p SubpassSampleLocationsEXT{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 8) sampleLocationsInfo . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) subpassIndex


instance ToCStruct RenderPassSampleLocationsBeginInfoEXT VkRenderPassSampleLocationsBeginInfoEXT where
  pokeCStruct :: Ptr VkRenderPassSampleLocationsBeginInfoEXT -> RenderPassSampleLocationsBeginInfoEXT -> IO a -> IO a
  pokeCStruct p RenderPassSampleLocationsBeginInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAttachmentInitialSampleLocations <- ContT $ allocaArray @VkAttachmentSampleLocationsEXT (Data.Vector.length attachmentInitialSampleLocations)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pAttachmentInitialSampleLocations `advancePtr` i) s . ($ ())) attachmentInitialSampleLocations
    pPostSubpassSampleLocations <- ContT $ allocaArray @VkSubpassSampleLocationsEXT (Data.Vector.length postSubpassSampleLocations)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pPostSubpassSampleLocations `advancePtr` i) s . ($ ())) postSubpassSampleLocations
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length attachmentInitialSampleLocations :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkAttachmentSampleLocationsEXT)) pAttachmentInitialSampleLocations
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length postSubpassSampleLocations :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkSubpassSampleLocationsEXT)) pPostSubpassSampleLocations


instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineSampleLocationsStateCreateInfoEXT -> PipelineSampleLocationsStateCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineSampleLocationsStateCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 24) sampleLocationsInfo . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 sampleLocationsEnable :: VkBool32)


instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceSampleLocationsPropertiesEXT -> PhysicalDeviceSampleLocationsPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceSampleLocationsPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 20) maxSampleLocationGridSize . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSampleCountFlags) sampleLocationSampleCounts
      case sampleLocationCoordinateRange of
        (sampleLocationCoordinateRange1, sampleLocationCoordinateRange2) -> do
          pokeElemOff @CFloat (p `plusPtr` 28) 0 (CFloat sampleLocationCoordinateRange1)
          pokeElemOff @CFloat (p `plusPtr` 28) 1 (CFloat sampleLocationCoordinateRange2)
      poke (p `plusPtr` 36 :: Ptr Word32) sampleLocationSubPixelBits
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 variableSampleLocations :: VkBool32)


instance ToCStruct MultisamplePropertiesEXT VkMultisamplePropertiesEXT where
  pokeCStruct :: Ptr VkMultisamplePropertiesEXT -> MultisamplePropertiesEXT -> IO a -> IO a
  pokeCStruct p MultisamplePropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) maxSampleLocationGridSize . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct SamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT where
  pokeCStruct :: Ptr VkSamplerReductionModeCreateInfoEXT -> SamplerReductionModeCreateInfoEXT -> IO a -> IO a
  pokeCStruct p SamplerReductionModeCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSamplerReductionModeEXT) reductionMode


instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT -> PhysicalDeviceBlendOperationAdvancedFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceBlendOperationAdvancedFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 advancedBlendCoherentOperations :: VkBool32)


instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT -> PhysicalDeviceBlendOperationAdvancedPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceBlendOperationAdvancedPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) advancedBlendMaxColorAttachments
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 advancedBlendIndependentBlend :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 advancedBlendNonPremultipliedSrcColor :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 advancedBlendNonPremultipliedDstColor :: VkBool32)
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 advancedBlendCorrelatedOverlap :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 advancedBlendAllOperations :: VkBool32)


instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineColorBlendAdvancedStateCreateInfoEXT -> PipelineColorBlendAdvancedStateCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineColorBlendAdvancedStateCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 srcPremultiplied :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 dstPremultiplied :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBlendOverlapEXT) blendOverlap


instance ToCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT VkPhysicalDeviceInlineUniformBlockFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceInlineUniformBlockFeaturesEXT -> PhysicalDeviceInlineUniformBlockFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceInlineUniformBlockFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 inlineUniformBlock :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 descriptorBindingInlineUniformBlockUpdateAfterBind :: VkBool32)


instance ToCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT VkPhysicalDeviceInlineUniformBlockPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceInlineUniformBlockPropertiesEXT -> PhysicalDeviceInlineUniformBlockPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceInlineUniformBlockPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxInlineUniformBlockSize
      poke (p `plusPtr` 20 :: Ptr Word32) maxPerStageDescriptorInlineUniformBlocks
      poke (p `plusPtr` 24 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindInlineUniformBlocks
      poke (p `plusPtr` 28 :: Ptr Word32) maxDescriptorSetInlineUniformBlocks
      poke (p `plusPtr` 32 :: Ptr Word32) maxDescriptorSetUpdateAfterBindInlineUniformBlocks


instance ToCStruct WriteDescriptorSetInlineUniformBlockEXT VkWriteDescriptorSetInlineUniformBlockEXT where
  pokeCStruct :: Ptr VkWriteDescriptorSetInlineUniformBlockEXT -> WriteDescriptorSetInlineUniformBlockEXT -> IO a -> IO a
  pokeCStruct p WriteDescriptorSetInlineUniformBlockEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) dataSize
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (data' :: Ptr ())


instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT VkDescriptorPoolInlineUniformBlockCreateInfoEXT where
  pokeCStruct :: Ptr VkDescriptorPoolInlineUniformBlockCreateInfoEXT -> DescriptorPoolInlineUniformBlockCreateInfoEXT -> IO a -> IO a
  pokeCStruct p DescriptorPoolInlineUniformBlockCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxInlineUniformBlockBindings


instance ToCStruct PipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineCoverageModulationStateCreateInfoNV -> PipelineCoverageModulationStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineCoverageModulationStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pCoverageModulationTable <- ContT $ either (const ($ nullPtr)) (allocaArray @CFloat . Data.Vector.length) coverageModulationTable
    liftIO $ do
      either (const (pure ())) (Data.Vector.imapM_ (\i -> pokeElemOff pCoverageModulationTable i . CFloat)) coverageModulationTable
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineCoverageModulationStateCreateFlagsNV) flags
      poke (p `plusPtr` 20 :: Ptr VkCoverageModulationModeNV) coverageModulationMode
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 coverageModulationTableEnable :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) coverageModulationTable :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr CFloat)) pCoverageModulationTable


instance ToCStruct ImageFormatListCreateInfoKHR VkImageFormatListCreateInfoKHR where
  pokeCStruct :: Ptr VkImageFormatListCreateInfoKHR -> ImageFormatListCreateInfoKHR -> IO a -> IO a
  pokeCStruct p ImageFormatListCreateInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pViewFormats <- ContT $ allocaArray @VkFormat (Data.Vector.length viewFormats)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pViewFormats) viewFormats
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length viewFormats :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkFormat)) pViewFormats


instance ToCStruct ValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT where
  pokeCStruct :: Ptr VkValidationCacheCreateInfoEXT -> ValidationCacheCreateInfoEXT -> IO a -> IO a
  pokeCStruct p ValidationCacheCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pInitialData <- ContT $ (unsafeUseAsCString initialData) . (. coerce @CString @(Ptr ()))
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkValidationCacheCreateFlagsEXT) flags
      poke (p `plusPtr` 24 :: Ptr CSize) (fromIntegral $ Data.ByteString.length initialData :: CSize)
      poke (p `plusPtr` 32 :: Ptr (Ptr ())) pInitialData


instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT where
  pokeCStruct :: Ptr VkShaderModuleValidationCacheCreateInfoEXT -> ShaderModuleValidationCacheCreateInfoEXT -> IO a -> IO a
  pokeCStruct p ShaderModuleValidationCacheCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkValidationCacheEXT) validationCache


instance ToCStruct PhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties where
  pokeCStruct :: Ptr VkPhysicalDeviceMaintenance3Properties -> PhysicalDeviceMaintenance3Properties -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMaintenance3Properties{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxPerSetDescriptors
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) maxMemoryAllocationSize


instance ToCStruct DescriptorSetLayoutSupport VkDescriptorSetLayoutSupport where
  pokeCStruct :: Ptr VkDescriptorSetLayoutSupport -> DescriptorSetLayoutSupport -> IO a -> IO a
  pokeCStruct p DescriptorSetLayoutSupport{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 supported :: VkBool32)


instance ToCStruct PhysicalDeviceShaderDrawParametersFeatures VkPhysicalDeviceShaderDrawParametersFeatures where
  pokeCStruct :: Ptr VkPhysicalDeviceShaderDrawParametersFeatures -> PhysicalDeviceShaderDrawParametersFeatures -> IO a -> IO a
  pokeCStruct p PhysicalDeviceShaderDrawParametersFeatures{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 shaderDrawParameters :: VkBool32)


instance ToCStruct PhysicalDeviceFloat16Int8FeaturesKHR VkPhysicalDeviceFloat16Int8FeaturesKHR where
  pokeCStruct :: Ptr VkPhysicalDeviceFloat16Int8FeaturesKHR -> PhysicalDeviceFloat16Int8FeaturesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFloat16Int8FeaturesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 shaderFloat16 :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 shaderInt8 :: VkBool32)


instance ToCStruct PhysicalDeviceFloatControlsPropertiesKHR VkPhysicalDeviceFloatControlsPropertiesKHR where
  pokeCStruct :: Ptr VkPhysicalDeviceFloatControlsPropertiesKHR -> PhysicalDeviceFloatControlsPropertiesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFloatControlsPropertiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 separateDenormSettings :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 separateRoundingModeSettings :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 shaderSignedZeroInfNanPreserveFloat16 :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 shaderSignedZeroInfNanPreserveFloat32 :: VkBool32)
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 shaderSignedZeroInfNanPreserveFloat64 :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 shaderDenormPreserveFloat16 :: VkBool32)
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 shaderDenormPreserveFloat32 :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 shaderDenormPreserveFloat64 :: VkBool32)
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 shaderDenormFlushToZeroFloat16 :: VkBool32)
      poke (p `plusPtr` 52 :: Ptr VkBool32) (boolToBool32 shaderDenormFlushToZeroFloat32 :: VkBool32)
      poke (p `plusPtr` 56 :: Ptr VkBool32) (boolToBool32 shaderDenormFlushToZeroFloat64 :: VkBool32)
      poke (p `plusPtr` 60 :: Ptr VkBool32) (boolToBool32 shaderRoundingModeRTEFloat16 :: VkBool32)
      poke (p `plusPtr` 64 :: Ptr VkBool32) (boolToBool32 shaderRoundingModeRTEFloat32 :: VkBool32)
      poke (p `plusPtr` 68 :: Ptr VkBool32) (boolToBool32 shaderRoundingModeRTEFloat64 :: VkBool32)
      poke (p `plusPtr` 72 :: Ptr VkBool32) (boolToBool32 shaderRoundingModeRTZFloat16 :: VkBool32)
      poke (p `plusPtr` 76 :: Ptr VkBool32) (boolToBool32 shaderRoundingModeRTZFloat32 :: VkBool32)
      poke (p `plusPtr` 80 :: Ptr VkBool32) (boolToBool32 shaderRoundingModeRTZFloat64 :: VkBool32)


instance ToCStruct PhysicalDeviceHostQueryResetFeaturesEXT VkPhysicalDeviceHostQueryResetFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceHostQueryResetFeaturesEXT -> PhysicalDeviceHostQueryResetFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceHostQueryResetFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 hostQueryReset :: VkBool32)


instance ToCStruct ShaderResourceUsageAMD VkShaderResourceUsageAMD where
  pokeCStruct :: Ptr VkShaderResourceUsageAMD -> ShaderResourceUsageAMD -> IO a -> IO a
  pokeCStruct p ShaderResourceUsageAMD{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) numUsedVgprs
      poke (p `plusPtr` 4 :: Ptr Word32) numUsedSgprs
      poke (p `plusPtr` 8 :: Ptr Word32) ldsSizePerLocalWorkGroup
      poke (p `plusPtr` 16 :: Ptr CSize) ldsUsageSizeInBytes
      poke (p `plusPtr` 24 :: Ptr CSize) scratchMemUsageInBytes


instance ToCStruct ShaderStatisticsInfoAMD VkShaderStatisticsInfoAMD where
  pokeCStruct :: Ptr VkShaderStatisticsInfoAMD -> ShaderStatisticsInfoAMD -> IO a -> IO a
  pokeCStruct p ShaderStatisticsInfoAMD{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 8) resourceUsage . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkShaderStageFlags) shaderStageMask
      poke (p `plusPtr` 40 :: Ptr Word32) numPhysicalVgprs
      poke (p `plusPtr` 44 :: Ptr Word32) numPhysicalSgprs
      poke (p `plusPtr` 48 :: Ptr Word32) numAvailableVgprs
      poke (p `plusPtr` 52 :: Ptr Word32) numAvailableSgprs
      case computeWorkGroupSize of
        (computeWorkGroupSize1, computeWorkGroupSize2, computeWorkGroupSize3) -> do
          pokeElemOff @Word32 (p `plusPtr` 56) 0 computeWorkGroupSize1
          pokeElemOff @Word32 (p `plusPtr` 56) 1 computeWorkGroupSize2
          pokeElemOff @Word32 (p `plusPtr` 56) 2 computeWorkGroupSize3


instance ToCStruct DeviceQueueGlobalPriorityCreateInfoEXT VkDeviceQueueGlobalPriorityCreateInfoEXT where
  pokeCStruct :: Ptr VkDeviceQueueGlobalPriorityCreateInfoEXT -> DeviceQueueGlobalPriorityCreateInfoEXT -> IO a -> IO a
  pokeCStruct p DeviceQueueGlobalPriorityCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkQueueGlobalPriorityEXT) globalPriority


instance ToCStruct DebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT where
  pokeCStruct :: Ptr VkDebugUtilsObjectNameInfoEXT -> DebugUtilsObjectNameInfoEXT -> IO a -> IO a
  pokeCStruct p DebugUtilsObjectNameInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pObjectName <- ContT $ maybeWith useAsCString objectName
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkObjectType) objectType
      poke (p `plusPtr` 24 :: Ptr Word64) objectHandle
      poke (p `plusPtr` 32 :: Ptr (Ptr CChar)) pObjectName


instance ToCStruct DebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT where
  pokeCStruct :: Ptr VkDebugUtilsObjectTagInfoEXT -> DebugUtilsObjectTagInfoEXT -> IO a -> IO a
  pokeCStruct p DebugUtilsObjectTagInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkObjectType) objectType
      poke (p `plusPtr` 24 :: Ptr Word64) objectHandle
      poke (p `plusPtr` 32 :: Ptr Word64) tagName
      poke (p `plusPtr` 40 :: Ptr CSize) tagSize
      poke (p `plusPtr` 48 :: Ptr (Ptr ())) (tag :: Ptr ())


instance ToCStruct DebugUtilsLabelEXT VkDebugUtilsLabelEXT where
  pokeCStruct :: Ptr VkDebugUtilsLabelEXT -> DebugUtilsLabelEXT -> IO a -> IO a
  pokeCStruct p DebugUtilsLabelEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pLabelName <- ContT $ useAsCString labelName
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr CChar)) pLabelName
      case color of
        (color1, color2, color3, color4) -> do
          pokeElemOff @CFloat (p `plusPtr` 24) 0 (CFloat color1)
          pokeElemOff @CFloat (p `plusPtr` 24) 1 (CFloat color2)
          pokeElemOff @CFloat (p `plusPtr` 24) 2 (CFloat color3)
          pokeElemOff @CFloat (p `plusPtr` 24) 3 (CFloat color4)


instance ToCStruct DebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT where
  pokeCStruct :: Ptr VkDebugUtilsMessengerCreateInfoEXT -> DebugUtilsMessengerCreateInfoEXT -> IO a -> IO a
  pokeCStruct p DebugUtilsMessengerCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDebugUtilsMessengerCreateFlagsEXT) flags
      poke (p `plusPtr` 20 :: Ptr VkDebugUtilsMessageSeverityFlagsEXT) messageSeverity
      poke (p `plusPtr` 24 :: Ptr VkDebugUtilsMessageTypeFlagsEXT) messageType
      poke (p `plusPtr` 32 :: Ptr PFN_vkDebugUtilsMessengerCallbackEXT) pfnUserCallback
      poke (p `plusPtr` 40 :: Ptr (Ptr ())) (userData :: Ptr ())


instance ToCStruct DebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT where
  pokeCStruct :: Ptr VkDebugUtilsMessengerCallbackDataEXT -> DebugUtilsMessengerCallbackDataEXT -> IO a -> IO a
  pokeCStruct p DebugUtilsMessengerCallbackDataEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pMessageIdName <- ContT $ maybeWith useAsCString messageIdName
    pMessage <- ContT $ useAsCString message
    pQueueLabels <- ContT $ allocaArray @VkDebugUtilsLabelEXT (Data.Vector.length queueLabels)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pQueueLabels `advancePtr` i) s . ($ ())) queueLabels
    pCmdBufLabels <- ContT $ allocaArray @VkDebugUtilsLabelEXT (Data.Vector.length cmdBufLabels)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pCmdBufLabels `advancePtr` i) s . ($ ())) cmdBufLabels
    pObjects <- ContT $ allocaArray @VkDebugUtilsObjectNameInfoEXT (Data.Vector.length objects)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pObjects `advancePtr` i) s . ($ ())) objects
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDebugUtilsMessengerCallbackDataFlagsEXT) flags
      poke (p `plusPtr` 24 :: Ptr (Ptr CChar)) pMessageIdName
      poke (p `plusPtr` 32 :: Ptr Int32) messageIdNumber
      poke (p `plusPtr` 40 :: Ptr (Ptr CChar)) pMessage
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length queueLabels :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr VkDebugUtilsLabelEXT)) pQueueLabels
      poke (p `plusPtr` 64 :: Ptr Word32) (fromIntegral $ Data.Vector.length cmdBufLabels :: Word32)
      poke (p `plusPtr` 72 :: Ptr (Ptr VkDebugUtilsLabelEXT)) pCmdBufLabels
      poke (p `plusPtr` 80 :: Ptr Word32) (fromIntegral $ Data.Vector.length objects :: Word32)
      poke (p `plusPtr` 88 :: Ptr (Ptr VkDebugUtilsObjectNameInfoEXT)) pObjects


instance ToCStruct ImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT where
  pokeCStruct :: Ptr VkImportMemoryHostPointerInfoEXT -> ImportMemoryHostPointerInfoEXT -> IO a -> IO a
  pokeCStruct p ImportMemoryHostPointerInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkExternalMemoryHandleTypeFlagBits) handleType
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (hostPointer :: Ptr ())


instance ToCStruct MemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT where
  pokeCStruct :: Ptr VkMemoryHostPointerPropertiesEXT -> MemoryHostPointerPropertiesEXT -> IO a -> IO a
  pokeCStruct p MemoryHostPointerPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) memoryTypeBits


instance ToCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceExternalMemoryHostPropertiesEXT -> PhysicalDeviceExternalMemoryHostPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceExternalMemoryHostPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) minImportedHostPointerAlignment


instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceConservativeRasterizationPropertiesEXT -> PhysicalDeviceConservativeRasterizationPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceConservativeRasterizationPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr CFloat) (CFloat primitiveOverestimationSize :: CFloat)
      poke (p `plusPtr` 20 :: Ptr CFloat) (CFloat maxExtraPrimitiveOverestimationSize :: CFloat)
      poke (p `plusPtr` 24 :: Ptr CFloat) (CFloat extraPrimitiveOverestimationSizeGranularity :: CFloat)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 primitiveUnderestimation :: VkBool32)
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 conservativePointAndLineRasterization :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 degenerateTrianglesRasterized :: VkBool32)
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 degenerateLinesRasterized :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 fullyCoveredFragmentShaderInputVariable :: VkBool32)
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 conservativeRasterizationPostDepthCoverage :: VkBool32)


instance ToCStruct CalibratedTimestampInfoEXT VkCalibratedTimestampInfoEXT where
  pokeCStruct :: Ptr VkCalibratedTimestampInfoEXT -> CalibratedTimestampInfoEXT -> IO a -> IO a
  pokeCStruct p CalibratedTimestampInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkTimeDomainEXT) timeDomain


instance ToCStruct PhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD where
  pokeCStruct :: Ptr VkPhysicalDeviceShaderCorePropertiesAMD -> PhysicalDeviceShaderCorePropertiesAMD -> IO a -> IO a
  pokeCStruct p PhysicalDeviceShaderCorePropertiesAMD{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) shaderEngineCount
      poke (p `plusPtr` 20 :: Ptr Word32) shaderArraysPerEngineCount
      poke (p `plusPtr` 24 :: Ptr Word32) computeUnitsPerShaderArray
      poke (p `plusPtr` 28 :: Ptr Word32) simdPerComputeUnit
      poke (p `plusPtr` 32 :: Ptr Word32) wavefrontsPerSimd
      poke (p `plusPtr` 36 :: Ptr Word32) wavefrontSize
      poke (p `plusPtr` 40 :: Ptr Word32) sgprsPerSimd
      poke (p `plusPtr` 44 :: Ptr Word32) minSgprAllocation
      poke (p `plusPtr` 48 :: Ptr Word32) maxSgprAllocation
      poke (p `plusPtr` 52 :: Ptr Word32) sgprAllocationGranularity
      poke (p `plusPtr` 56 :: Ptr Word32) vgprsPerSimd
      poke (p `plusPtr` 60 :: Ptr Word32) minVgprAllocation
      poke (p `plusPtr` 64 :: Ptr Word32) maxVgprAllocation
      poke (p `plusPtr` 68 :: Ptr Word32) vgprAllocationGranularity


instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineRasterizationConservativeStateCreateInfoEXT -> PipelineRasterizationConservativeStateCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineRasterizationConservativeStateCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineRasterizationConservativeStateCreateFlagsEXT) flags
      poke (p `plusPtr` 20 :: Ptr VkConservativeRasterizationModeEXT) conservativeRasterizationMode
      poke (p `plusPtr` 24 :: Ptr CFloat) (CFloat extraPrimitiveOverestimationSize :: CFloat)


instance ToCStruct PhysicalDeviceDescriptorIndexingFeaturesEXT VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceDescriptorIndexingFeaturesEXT -> PhysicalDeviceDescriptorIndexingFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDescriptorIndexingFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 shaderInputAttachmentArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 shaderUniformTexelBufferArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 shaderStorageTexelBufferArrayDynamicIndexing :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 shaderUniformBufferArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 shaderSampledImageArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 shaderStorageBufferArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 shaderStorageImageArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 shaderInputAttachmentArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 shaderUniformTexelBufferArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 52 :: Ptr VkBool32) (boolToBool32 shaderStorageTexelBufferArrayNonUniformIndexing :: VkBool32)
      poke (p `plusPtr` 56 :: Ptr VkBool32) (boolToBool32 descriptorBindingUniformBufferUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 60 :: Ptr VkBool32) (boolToBool32 descriptorBindingSampledImageUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 64 :: Ptr VkBool32) (boolToBool32 descriptorBindingStorageImageUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 68 :: Ptr VkBool32) (boolToBool32 descriptorBindingStorageBufferUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 72 :: Ptr VkBool32) (boolToBool32 descriptorBindingUniformTexelBufferUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 76 :: Ptr VkBool32) (boolToBool32 descriptorBindingStorageTexelBufferUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 80 :: Ptr VkBool32) (boolToBool32 descriptorBindingUpdateUnusedWhilePending :: VkBool32)
      poke (p `plusPtr` 84 :: Ptr VkBool32) (boolToBool32 descriptorBindingPartiallyBound :: VkBool32)
      poke (p `plusPtr` 88 :: Ptr VkBool32) (boolToBool32 descriptorBindingVariableDescriptorCount :: VkBool32)
      poke (p `plusPtr` 92 :: Ptr VkBool32) (boolToBool32 runtimeDescriptorArray :: VkBool32)


instance ToCStruct PhysicalDeviceDescriptorIndexingPropertiesEXT VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceDescriptorIndexingPropertiesEXT -> PhysicalDeviceDescriptorIndexingPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDescriptorIndexingPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxUpdateAfterBindDescriptorsInAllPools
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 shaderUniformBufferArrayNonUniformIndexingNative :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 shaderSampledImageArrayNonUniformIndexingNative :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 shaderStorageBufferArrayNonUniformIndexingNative :: VkBool32)
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 shaderStorageImageArrayNonUniformIndexingNative :: VkBool32)
      poke (p `plusPtr` 36 :: Ptr VkBool32) (boolToBool32 shaderInputAttachmentArrayNonUniformIndexingNative :: VkBool32)
      poke (p `plusPtr` 40 :: Ptr VkBool32) (boolToBool32 robustBufferAccessUpdateAfterBind :: VkBool32)
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 quadDivergentImplicitLod :: VkBool32)
      poke (p `plusPtr` 48 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindSamplers
      poke (p `plusPtr` 52 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindUniformBuffers
      poke (p `plusPtr` 56 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindStorageBuffers
      poke (p `plusPtr` 60 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindSampledImages
      poke (p `plusPtr` 64 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindStorageImages
      poke (p `plusPtr` 68 :: Ptr Word32) maxPerStageDescriptorUpdateAfterBindInputAttachments
      poke (p `plusPtr` 72 :: Ptr Word32) maxPerStageUpdateAfterBindResources
      poke (p `plusPtr` 76 :: Ptr Word32) maxDescriptorSetUpdateAfterBindSamplers
      poke (p `plusPtr` 80 :: Ptr Word32) maxDescriptorSetUpdateAfterBindUniformBuffers
      poke (p `plusPtr` 84 :: Ptr Word32) maxDescriptorSetUpdateAfterBindUniformBuffersDynamic
      poke (p `plusPtr` 88 :: Ptr Word32) maxDescriptorSetUpdateAfterBindStorageBuffers
      poke (p `plusPtr` 92 :: Ptr Word32) maxDescriptorSetUpdateAfterBindStorageBuffersDynamic
      poke (p `plusPtr` 96 :: Ptr Word32) maxDescriptorSetUpdateAfterBindSampledImages
      poke (p `plusPtr` 100 :: Ptr Word32) maxDescriptorSetUpdateAfterBindStorageImages
      poke (p `plusPtr` 104 :: Ptr Word32) maxDescriptorSetUpdateAfterBindInputAttachments


instance ToCStruct DescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  pokeCStruct :: Ptr VkDescriptorSetLayoutBindingFlagsCreateInfoEXT -> DescriptorSetLayoutBindingFlagsCreateInfoEXT -> IO a -> IO a
  pokeCStruct p DescriptorSetLayoutBindingFlagsCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pBindingFlags <- ContT $ either (const ($ nullPtr)) (allocaArray @VkDescriptorBindingFlagsEXT . Data.Vector.length) bindingFlags
    liftIO $ do
      either (const (pure ())) (Data.Vector.imapM_ (pokeElemOff pBindingFlags)) bindingFlags
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) bindingFlags :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDescriptorBindingFlagsEXT)) pBindingFlags


instance ToCStruct DescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  pokeCStruct :: Ptr VkDescriptorSetVariableDescriptorCountAllocateInfoEXT -> DescriptorSetVariableDescriptorCountAllocateInfoEXT -> IO a -> IO a
  pokeCStruct p DescriptorSetVariableDescriptorCountAllocateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDescriptorCounts <- ContT $ allocaArray @Word32 (Data.Vector.length descriptorCounts)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDescriptorCounts) descriptorCounts
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length descriptorCounts :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word32)) pDescriptorCounts


instance ToCStruct DescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  pokeCStruct :: Ptr VkDescriptorSetVariableDescriptorCountLayoutSupportEXT -> DescriptorSetVariableDescriptorCountLayoutSupportEXT -> IO a -> IO a
  pokeCStruct p DescriptorSetVariableDescriptorCountLayoutSupportEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxVariableDescriptorCount


instance ToCStruct AttachmentDescription2KHR VkAttachmentDescription2KHR where
  pokeCStruct :: Ptr VkAttachmentDescription2KHR -> AttachmentDescription2KHR -> IO a -> IO a
  pokeCStruct p AttachmentDescription2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAttachmentDescriptionFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkFormat) format
      poke (p `plusPtr` 24 :: Ptr VkSampleCountFlagBits) samples
      poke (p `plusPtr` 28 :: Ptr VkAttachmentLoadOp) loadOp
      poke (p `plusPtr` 32 :: Ptr VkAttachmentStoreOp) storeOp
      poke (p `plusPtr` 36 :: Ptr VkAttachmentLoadOp) stencilLoadOp
      poke (p `plusPtr` 40 :: Ptr VkAttachmentStoreOp) stencilStoreOp
      poke (p `plusPtr` 44 :: Ptr VkImageLayout) initialLayout
      poke (p `plusPtr` 48 :: Ptr VkImageLayout) finalLayout


instance ToCStruct AttachmentReference2KHR VkAttachmentReference2KHR where
  pokeCStruct :: Ptr VkAttachmentReference2KHR -> AttachmentReference2KHR -> IO a -> IO a
  pokeCStruct p AttachmentReference2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) attachment
      poke (p `plusPtr` 20 :: Ptr VkImageLayout) layout
      poke (p `plusPtr` 24 :: Ptr VkImageAspectFlags) aspectMask


instance ToCStruct SubpassDescription2KHR VkSubpassDescription2KHR where
  pokeCStruct :: Ptr VkSubpassDescription2KHR -> SubpassDescription2KHR -> IO a -> IO a
  pokeCStruct p SubpassDescription2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pInputAttachments <- ContT $ allocaArray @VkAttachmentReference2KHR (Data.Vector.length inputAttachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pInputAttachments `advancePtr` i) s . ($ ())) inputAttachments
    pColorAttachments <- ContT $ allocaArray @VkAttachmentReference2KHR (Data.Vector.length colorAttachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pColorAttachments `advancePtr` i) s . ($ ())) colorAttachments
    pResolveAttachments <- ContT $ either (const ($ nullPtr)) (allocaArray @VkAttachmentReference2KHR . Data.Vector.length) resolveAttachments
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pResolveAttachments `advancePtr` i) s . ($ ()))) resolveAttachments
    pDepthStencilAttachment <- ContT $ maybeWith withCStruct depthStencilAttachment
    pPreserveAttachments <- ContT $ allocaArray @Word32 (Data.Vector.length preserveAttachments)
    liftIO $ do
      colorAttachmentCount <- let l = Data.Vector.length colorAttachments in if (l == either fromIntegral Data.Vector.length resolveAttachments) then pure (fromIntegral l :: Word32) else throwIO $ IOError Nothing InvalidArgument "" "colorAttachments and resolveAttachments must have the same length" Nothing Nothing
      Data.Vector.imapM_ (pokeElemOff pPreserveAttachments) preserveAttachments
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSubpassDescriptionFlags) flags
      poke (p `plusPtr` 20 :: Ptr VkPipelineBindPoint) pipelineBindPoint
      poke (p `plusPtr` 24 :: Ptr Word32) viewMask
      poke (p `plusPtr` 28 :: Ptr Word32) (fromIntegral $ Data.Vector.length inputAttachments :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkAttachmentReference2KHR)) pInputAttachments
      poke (p `plusPtr` 40 :: Ptr Word32) colorAttachmentCount
      poke (p `plusPtr` 48 :: Ptr (Ptr VkAttachmentReference2KHR)) pColorAttachments
      poke (p `plusPtr` 56 :: Ptr (Ptr VkAttachmentReference2KHR)) pResolveAttachments
      poke (p `plusPtr` 64 :: Ptr (Ptr VkAttachmentReference2KHR)) pDepthStencilAttachment
      poke (p `plusPtr` 72 :: Ptr Word32) (fromIntegral $ Data.Vector.length preserveAttachments :: Word32)
      poke (p `plusPtr` 80 :: Ptr (Ptr Word32)) pPreserveAttachments


instance ToCStruct SubpassDependency2KHR VkSubpassDependency2KHR where
  pokeCStruct :: Ptr VkSubpassDependency2KHR -> SubpassDependency2KHR -> IO a -> IO a
  pokeCStruct p SubpassDependency2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) srcSubpass
      poke (p `plusPtr` 20 :: Ptr Word32) dstSubpass
      poke (p `plusPtr` 24 :: Ptr VkPipelineStageFlags) srcStageMask
      poke (p `plusPtr` 28 :: Ptr VkPipelineStageFlags) dstStageMask
      poke (p `plusPtr` 32 :: Ptr VkAccessFlags) srcAccessMask
      poke (p `plusPtr` 36 :: Ptr VkAccessFlags) dstAccessMask
      poke (p `plusPtr` 40 :: Ptr VkDependencyFlags) dependencyFlags
      poke (p `plusPtr` 44 :: Ptr Int32) viewOffset


instance ToCStruct RenderPassCreateInfo2KHR VkRenderPassCreateInfo2KHR where
  pokeCStruct :: Ptr VkRenderPassCreateInfo2KHR -> RenderPassCreateInfo2KHR -> IO a -> IO a
  pokeCStruct p RenderPassCreateInfo2KHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAttachments <- ContT $ allocaArray @VkAttachmentDescription2KHR (Data.Vector.length attachments)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pAttachments `advancePtr` i) s . ($ ())) attachments
    pSubpasses <- ContT $ allocaArray @VkSubpassDescription2KHR (Data.Vector.length subpasses)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pSubpasses `advancePtr` i) s . ($ ())) subpasses
    pDependencies <- ContT $ allocaArray @VkSubpassDependency2KHR (Data.Vector.length dependencies)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pDependencies `advancePtr` i) s . ($ ())) dependencies
    pCorrelatedViewMasks <- ContT $ allocaArray @Word32 (Data.Vector.length correlatedViewMasks)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pCorrelatedViewMasks) correlatedViewMasks
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkRenderPassCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length attachments :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkAttachmentDescription2KHR)) pAttachments
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length subpasses :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkSubpassDescription2KHR)) pSubpasses
      poke (p `plusPtr` 48 :: Ptr Word32) (fromIntegral $ Data.Vector.length dependencies :: Word32)
      poke (p `plusPtr` 56 :: Ptr (Ptr VkSubpassDependency2KHR)) pDependencies
      poke (p `plusPtr` 64 :: Ptr Word32) (fromIntegral $ Data.Vector.length correlatedViewMasks :: Word32)
      poke (p `plusPtr` 72 :: Ptr (Ptr Word32)) pCorrelatedViewMasks


instance ToCStruct SubpassBeginInfoKHR VkSubpassBeginInfoKHR where
  pokeCStruct :: Ptr VkSubpassBeginInfoKHR -> SubpassBeginInfoKHR -> IO a -> IO a
  pokeCStruct p SubpassBeginInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkSubpassContents) contents


instance ToCStruct SubpassEndInfoKHR VkSubpassEndInfoKHR where
  pokeCStruct :: Ptr VkSubpassEndInfoKHR -> SubpassEndInfoKHR -> IO a -> IO a
  pokeCStruct p SubpassEndInfoKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SUBPASS_END_INFO_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct VertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT where
  pokeCStruct :: Ptr VkVertexInputBindingDivisorDescriptionEXT -> VertexInputBindingDivisorDescriptionEXT -> IO a -> IO a
  pokeCStruct p VertexInputBindingDivisorDescriptionEXT{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) binding
      poke (p `plusPtr` 4 :: Ptr Word32) divisor


instance ToCStruct PipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineVertexInputDivisorStateCreateInfoEXT -> PipelineVertexInputDivisorStateCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineVertexInputDivisorStateCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pVertexBindingDivisors <- ContT $ allocaArray @VkVertexInputBindingDivisorDescriptionEXT (Data.Vector.length vertexBindingDivisors)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pVertexBindingDivisors `advancePtr` i) s . ($ ())) vertexBindingDivisors
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length vertexBindingDivisors :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkVertexInputBindingDivisorDescriptionEXT)) pVertexBindingDivisors


instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT -> PhysicalDeviceVertexAttributeDivisorPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxVertexAttribDivisor


instance ToCStruct PhysicalDevicePCIBusInfoPropertiesEXT VkPhysicalDevicePCIBusInfoPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDevicePCIBusInfoPropertiesEXT -> PhysicalDevicePCIBusInfoPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDevicePCIBusInfoPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) pciDomain
      poke (p `plusPtr` 20 :: Ptr Word32) pciBus
      poke (p `plusPtr` 24 :: Ptr Word32) pciDevice
      poke (p `plusPtr` 28 :: Ptr Word32) pciFunction


instance ToCStruct ImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID where
  pokeCStruct :: Ptr VkImportAndroidHardwareBufferInfoANDROID -> ImportAndroidHardwareBufferInfoANDROID -> IO a -> IO a
  pokeCStruct p ImportAndroidHardwareBufferInfoANDROID{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr AHardwareBuffer)) (buffer :: Ptr AHardwareBuffer)


instance ToCStruct AndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID where
  pokeCStruct :: Ptr VkAndroidHardwareBufferUsageANDROID -> AndroidHardwareBufferUsageANDROID -> IO a -> IO a
  pokeCStruct p AndroidHardwareBufferUsageANDROID{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word64) androidHardwareBufferUsage


instance ToCStruct AndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID where
  pokeCStruct :: Ptr VkAndroidHardwareBufferPropertiesANDROID -> AndroidHardwareBufferPropertiesANDROID -> IO a -> IO a
  pokeCStruct p AndroidHardwareBufferPropertiesANDROID{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) allocationSize
      poke (p `plusPtr` 24 :: Ptr Word32) memoryTypeBits


instance ToCStruct MemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID where
  pokeCStruct :: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID -> MemoryGetAndroidHardwareBufferInfoANDROID -> IO a -> IO a
  pokeCStruct p MemoryGetAndroidHardwareBufferInfoANDROID{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceMemory) memory


instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID where
  pokeCStruct :: Ptr VkAndroidHardwareBufferFormatPropertiesANDROID -> AndroidHardwareBufferFormatPropertiesANDROID -> IO a -> IO a
  pokeCStruct p AndroidHardwareBufferFormatPropertiesANDROID{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 36) samplerYcbcrConversionComponents . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFormat) format
      poke (p `plusPtr` 24 :: Ptr Word64) externalFormat
      poke (p `plusPtr` 32 :: Ptr VkFormatFeatureFlags) formatFeatures
      poke (p `plusPtr` 52 :: Ptr VkSamplerYcbcrModelConversion) suggestedYcbcrModel
      poke (p `plusPtr` 56 :: Ptr VkSamplerYcbcrRange) suggestedYcbcrRange
      poke (p `plusPtr` 60 :: Ptr VkChromaLocation) suggestedXChromaOffset
      poke (p `plusPtr` 64 :: Ptr VkChromaLocation) suggestedYChromaOffset


instance ToCStruct CommandBufferInheritanceConditionalRenderingInfoEXT VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  pokeCStruct :: Ptr VkCommandBufferInheritanceConditionalRenderingInfoEXT -> CommandBufferInheritanceConditionalRenderingInfoEXT -> IO a -> IO a
  pokeCStruct p CommandBufferInheritanceConditionalRenderingInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 conditionalRenderingEnable :: VkBool32)


instance ToCStruct ExternalFormatANDROID VkExternalFormatANDROID where
  pokeCStruct :: Ptr VkExternalFormatANDROID -> ExternalFormatANDROID -> IO a -> IO a
  pokeCStruct p ExternalFormatANDROID{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word64) externalFormat


instance ToCStruct PhysicalDevice8BitStorageFeaturesKHR VkPhysicalDevice8BitStorageFeaturesKHR where
  pokeCStruct :: Ptr VkPhysicalDevice8BitStorageFeaturesKHR -> PhysicalDevice8BitStorageFeaturesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDevice8BitStorageFeaturesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 storageBuffer8BitAccess :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 uniformAndStorageBuffer8BitAccess :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 storagePushConstant8 :: VkBool32)


instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceConditionalRenderingFeaturesEXT -> PhysicalDeviceConditionalRenderingFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceConditionalRenderingFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 conditionalRendering :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 inheritedConditionalRendering :: VkBool32)


instance ToCStruct PhysicalDeviceVulkanMemoryModelFeaturesKHR VkPhysicalDeviceVulkanMemoryModelFeaturesKHR where
  pokeCStruct :: Ptr VkPhysicalDeviceVulkanMemoryModelFeaturesKHR -> PhysicalDeviceVulkanMemoryModelFeaturesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceVulkanMemoryModelFeaturesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 vulkanMemoryModel :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 vulkanMemoryModelDeviceScope :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 vulkanMemoryModelAvailabilityVisibilityChains :: VkBool32)


instance ToCStruct PhysicalDeviceShaderAtomicInt64FeaturesKHR VkPhysicalDeviceShaderAtomicInt64FeaturesKHR where
  pokeCStruct :: Ptr VkPhysicalDeviceShaderAtomicInt64FeaturesKHR -> PhysicalDeviceShaderAtomicInt64FeaturesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceShaderAtomicInt64FeaturesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 shaderBufferInt64Atomics :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 shaderSharedInt64Atomics :: VkBool32)


instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT -> PhysicalDeviceVertexAttributeDivisorFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceVertexAttributeDivisorFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 vertexAttributeInstanceRateDivisor :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 vertexAttributeInstanceRateZeroDivisor :: VkBool32)


instance ToCStruct QueueFamilyCheckpointPropertiesNV VkQueueFamilyCheckpointPropertiesNV where
  pokeCStruct :: Ptr VkQueueFamilyCheckpointPropertiesNV -> QueueFamilyCheckpointPropertiesNV -> IO a -> IO a
  pokeCStruct p QueueFamilyCheckpointPropertiesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineStageFlags) checkpointExecutionStageMask


instance ToCStruct CheckpointDataNV VkCheckpointDataNV where
  pokeCStruct :: Ptr VkCheckpointDataNV -> CheckpointDataNV -> IO a -> IO a
  pokeCStruct p CheckpointDataNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_CHECKPOINT_DATA_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineStageFlagBits) stage
      poke (p `plusPtr` 24 :: Ptr (Ptr ())) (checkpointMarker :: Ptr ())


instance ToCStruct PhysicalDeviceDepthStencilResolvePropertiesKHR VkPhysicalDeviceDepthStencilResolvePropertiesKHR where
  pokeCStruct :: Ptr VkPhysicalDeviceDepthStencilResolvePropertiesKHR -> PhysicalDeviceDepthStencilResolvePropertiesKHR -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDepthStencilResolvePropertiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkResolveModeFlagsKHR) supportedDepthResolveModes
      poke (p `plusPtr` 20 :: Ptr VkResolveModeFlagsKHR) supportedStencilResolveModes
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 independentResolveNone :: VkBool32)
      poke (p `plusPtr` 28 :: Ptr VkBool32) (boolToBool32 independentResolve :: VkBool32)


instance ToCStruct SubpassDescriptionDepthStencilResolveKHR VkSubpassDescriptionDepthStencilResolveKHR where
  pokeCStruct :: Ptr VkSubpassDescriptionDepthStencilResolveKHR -> SubpassDescriptionDepthStencilResolveKHR -> IO a -> IO a
  pokeCStruct p SubpassDescriptionDepthStencilResolveKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDepthStencilResolveAttachment <- ContT $ maybeWith withCStruct depthStencilResolveAttachment
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkResolveModeFlagBitsKHR) depthResolveMode
      poke (p `plusPtr` 20 :: Ptr VkResolveModeFlagBitsKHR) stencilResolveMode
      poke (p `plusPtr` 24 :: Ptr (Ptr VkAttachmentReference2KHR)) pDepthStencilResolveAttachment


instance ToCStruct ImageViewASTCDecodeModeEXT VkImageViewASTCDecodeModeEXT where
  pokeCStruct :: Ptr VkImageViewASTCDecodeModeEXT -> ImageViewASTCDecodeModeEXT -> IO a -> IO a
  pokeCStruct p ImageViewASTCDecodeModeEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFormat) decodeMode


instance ToCStruct PhysicalDeviceASTCDecodeFeaturesEXT VkPhysicalDeviceASTCDecodeFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceASTCDecodeFeaturesEXT -> PhysicalDeviceASTCDecodeFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceASTCDecodeFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 decodeModeSharedExponent :: VkBool32)


instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT VkPhysicalDeviceTransformFeedbackFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceTransformFeedbackFeaturesEXT -> PhysicalDeviceTransformFeedbackFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceTransformFeedbackFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 transformFeedback :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 geometryStreams :: VkBool32)


instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT VkPhysicalDeviceTransformFeedbackPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceTransformFeedbackPropertiesEXT -> PhysicalDeviceTransformFeedbackPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceTransformFeedbackPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxTransformFeedbackStreams
      poke (p `plusPtr` 20 :: Ptr Word32) maxTransformFeedbackBuffers
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) maxTransformFeedbackBufferSize
      poke (p `plusPtr` 32 :: Ptr Word32) maxTransformFeedbackStreamDataSize
      poke (p `plusPtr` 36 :: Ptr Word32) maxTransformFeedbackBufferDataSize
      poke (p `plusPtr` 40 :: Ptr Word32) maxTransformFeedbackBufferDataStride
      poke (p `plusPtr` 44 :: Ptr VkBool32) (boolToBool32 transformFeedbackQueries :: VkBool32)
      poke (p `plusPtr` 48 :: Ptr VkBool32) (boolToBool32 transformFeedbackStreamsLinesTriangles :: VkBool32)
      poke (p `plusPtr` 52 :: Ptr VkBool32) (boolToBool32 transformFeedbackRasterizationStreamSelect :: VkBool32)
      poke (p `plusPtr` 56 :: Ptr VkBool32) (boolToBool32 transformFeedbackDraw :: VkBool32)


instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT VkPipelineRasterizationStateStreamCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineRasterizationStateStreamCreateInfoEXT -> PipelineRasterizationStateStreamCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineRasterizationStateStreamCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineRasterizationStateStreamCreateFlagsEXT) flags
      poke (p `plusPtr` 20 :: Ptr Word32) rasterizationStream


instance ToCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV -> PhysicalDeviceRepresentativeFragmentTestFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceRepresentativeFragmentTestFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 representativeFragmentTest :: VkBool32)


instance ToCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV VkPipelineRepresentativeFragmentTestStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineRepresentativeFragmentTestStateCreateInfoNV -> PipelineRepresentativeFragmentTestStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineRepresentativeFragmentTestStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 representativeFragmentTestEnable :: VkBool32)


instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV VkPhysicalDeviceExclusiveScissorFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceExclusiveScissorFeaturesNV -> PhysicalDeviceExclusiveScissorFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceExclusiveScissorFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 exclusiveScissor :: VkBool32)


instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV VkPipelineViewportExclusiveScissorStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineViewportExclusiveScissorStateCreateInfoNV -> PipelineViewportExclusiveScissorStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineViewportExclusiveScissorStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pExclusiveScissors <- ContT $ either (const ($ nullPtr)) (allocaArray @VkRect2D . Data.Vector.length) exclusiveScissors
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pExclusiveScissors `advancePtr` i) s . ($ ()))) exclusiveScissors
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) exclusiveScissors :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkRect2D)) pExclusiveScissors


instance ToCStruct PhysicalDeviceCornerSampledImageFeaturesNV VkPhysicalDeviceCornerSampledImageFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceCornerSampledImageFeaturesNV -> PhysicalDeviceCornerSampledImageFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceCornerSampledImageFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 cornerSampledImage :: VkBool32)


instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV VkPhysicalDeviceComputeShaderDerivativesFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceComputeShaderDerivativesFeaturesNV -> PhysicalDeviceComputeShaderDerivativesFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceComputeShaderDerivativesFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 computeDerivativeGroupQuads :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 computeDerivativeGroupLinear :: VkBool32)


instance ToCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesNV VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV -> PhysicalDeviceFragmentShaderBarycentricFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFragmentShaderBarycentricFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 fragmentShaderBarycentric :: VkBool32)


instance ToCStruct PhysicalDeviceShaderImageFootprintFeaturesNV VkPhysicalDeviceShaderImageFootprintFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceShaderImageFootprintFeaturesNV -> PhysicalDeviceShaderImageFootprintFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceShaderImageFootprintFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 imageFootprint :: VkBool32)


instance ToCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 dedicatedAllocationImageAliasing :: VkBool32)


instance ToCStruct ShadingRatePaletteNV VkShadingRatePaletteNV where
  pokeCStruct :: Ptr VkShadingRatePaletteNV -> ShadingRatePaletteNV -> IO a -> IO a
  pokeCStruct p ShadingRatePaletteNV{..} = (. const) . runContT $ do
    pShadingRatePaletteEntries <- ContT $ allocaArray @VkShadingRatePaletteEntryNV (Data.Vector.length shadingRatePaletteEntries)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pShadingRatePaletteEntries) shadingRatePaletteEntries
      poke (p `plusPtr` 0 :: Ptr Word32) (fromIntegral $ Data.Vector.length shadingRatePaletteEntries :: Word32)
      poke (p `plusPtr` 8 :: Ptr (Ptr VkShadingRatePaletteEntryNV)) pShadingRatePaletteEntries


instance ToCStruct PipelineViewportShadingRateImageStateCreateInfoNV VkPipelineViewportShadingRateImageStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineViewportShadingRateImageStateCreateInfoNV -> PipelineViewportShadingRateImageStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineViewportShadingRateImageStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pShadingRatePalettes <- ContT $ either (const ($ nullPtr)) (allocaArray @VkShadingRatePaletteNV . Data.Vector.length) shadingRatePalettes
    either (const (pure ())) (Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pShadingRatePalettes `advancePtr` i) s . ($ ()))) shadingRatePalettes
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 shadingRateImageEnable :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr Word32) (either id (fromIntegral . Data.Vector.length) shadingRatePalettes :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkShadingRatePaletteNV)) pShadingRatePalettes


instance ToCStruct PhysicalDeviceShadingRateImageFeaturesNV VkPhysicalDeviceShadingRateImageFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceShadingRateImageFeaturesNV -> PhysicalDeviceShadingRateImageFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceShadingRateImageFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 shadingRateImage :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 shadingRateCoarseSampleOrder :: VkBool32)


instance ToCStruct PhysicalDeviceShadingRateImagePropertiesNV VkPhysicalDeviceShadingRateImagePropertiesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceShadingRateImagePropertiesNV -> PhysicalDeviceShadingRateImagePropertiesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceShadingRateImagePropertiesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) shadingRateTexelSize . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 24 :: Ptr Word32) shadingRatePaletteSize
      poke (p `plusPtr` 28 :: Ptr Word32) shadingRateMaxCoarseSamples


instance ToCStruct CoarseSampleLocationNV VkCoarseSampleLocationNV where
  pokeCStruct :: Ptr VkCoarseSampleLocationNV -> CoarseSampleLocationNV -> IO a -> IO a
  pokeCStruct p CoarseSampleLocationNV{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) pixelX
      poke (p `plusPtr` 4 :: Ptr Word32) pixelY
      poke (p `plusPtr` 8 :: Ptr Word32) sample


instance ToCStruct CoarseSampleOrderCustomNV VkCoarseSampleOrderCustomNV where
  pokeCStruct :: Ptr VkCoarseSampleOrderCustomNV -> CoarseSampleOrderCustomNV -> IO a -> IO a
  pokeCStruct p CoarseSampleOrderCustomNV{..} = (. const) . runContT $ do
    pSampleLocations <- ContT $ allocaArray @VkCoarseSampleLocationNV (Data.Vector.length sampleLocations)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pSampleLocations `advancePtr` i) s . ($ ())) sampleLocations
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkShadingRatePaletteEntryNV) shadingRate
      poke (p `plusPtr` 4 :: Ptr Word32) sampleCount
      poke (p `plusPtr` 8 :: Ptr Word32) (fromIntegral $ Data.Vector.length sampleLocations :: Word32)
      poke (p `plusPtr` 16 :: Ptr (Ptr VkCoarseSampleLocationNV)) pSampleLocations


instance ToCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV VkPipelineViewportCoarseSampleOrderStateCreateInfoNV where
  pokeCStruct :: Ptr VkPipelineViewportCoarseSampleOrderStateCreateInfoNV -> PipelineViewportCoarseSampleOrderStateCreateInfoNV -> IO a -> IO a
  pokeCStruct p PipelineViewportCoarseSampleOrderStateCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pCustomSampleOrders <- ContT $ allocaArray @VkCoarseSampleOrderCustomNV (Data.Vector.length customSampleOrders)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pCustomSampleOrders `advancePtr` i) s . ($ ())) customSampleOrders
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkCoarseSampleOrderTypeNV) sampleOrderType
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length customSampleOrders :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkCoarseSampleOrderCustomNV)) pCustomSampleOrders


instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV VkPhysicalDeviceMeshShaderFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceMeshShaderFeaturesNV -> PhysicalDeviceMeshShaderFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMeshShaderFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 taskShader :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 meshShader :: VkBool32)


instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV VkPhysicalDeviceMeshShaderPropertiesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceMeshShaderPropertiesNV -> PhysicalDeviceMeshShaderPropertiesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMeshShaderPropertiesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) maxDrawMeshTasksCount
      poke (p `plusPtr` 20 :: Ptr Word32) maxTaskWorkGroupInvocations
      case maxTaskWorkGroupSize of
        (maxTaskWorkGroupSize1, maxTaskWorkGroupSize2, maxTaskWorkGroupSize3) -> do
          pokeElemOff @Word32 (p `plusPtr` 24) 0 maxTaskWorkGroupSize1
          pokeElemOff @Word32 (p `plusPtr` 24) 1 maxTaskWorkGroupSize2
          pokeElemOff @Word32 (p `plusPtr` 24) 2 maxTaskWorkGroupSize3
      poke (p `plusPtr` 36 :: Ptr Word32) maxTaskTotalMemorySize
      poke (p `plusPtr` 40 :: Ptr Word32) maxTaskOutputCount
      poke (p `plusPtr` 44 :: Ptr Word32) maxMeshWorkGroupInvocations
      case maxMeshWorkGroupSize of
        (maxMeshWorkGroupSize1, maxMeshWorkGroupSize2, maxMeshWorkGroupSize3) -> do
          pokeElemOff @Word32 (p `plusPtr` 48) 0 maxMeshWorkGroupSize1
          pokeElemOff @Word32 (p `plusPtr` 48) 1 maxMeshWorkGroupSize2
          pokeElemOff @Word32 (p `plusPtr` 48) 2 maxMeshWorkGroupSize3
      poke (p `plusPtr` 60 :: Ptr Word32) maxMeshTotalMemorySize
      poke (p `plusPtr` 64 :: Ptr Word32) maxMeshOutputVertices
      poke (p `plusPtr` 68 :: Ptr Word32) maxMeshOutputPrimitives
      poke (p `plusPtr` 72 :: Ptr Word32) maxMeshMultiviewViewCount
      poke (p `plusPtr` 76 :: Ptr Word32) meshOutputPerVertexGranularity
      poke (p `plusPtr` 80 :: Ptr Word32) meshOutputPerPrimitiveGranularity


instance ToCStruct DrawMeshTasksIndirectCommandNV VkDrawMeshTasksIndirectCommandNV where
  pokeCStruct :: Ptr VkDrawMeshTasksIndirectCommandNV -> DrawMeshTasksIndirectCommandNV -> IO a -> IO a
  pokeCStruct p DrawMeshTasksIndirectCommandNV{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word32) taskCount
      poke (p `plusPtr` 4 :: Ptr Word32) firstTask


instance ToCStruct RayTracingShaderGroupCreateInfoNV VkRayTracingShaderGroupCreateInfoNV where
  pokeCStruct :: Ptr VkRayTracingShaderGroupCreateInfoNV -> RayTracingShaderGroupCreateInfoNV -> IO a -> IO a
  pokeCStruct p RayTracingShaderGroupCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkRayTracingShaderGroupTypeNV) type'
      poke (p `plusPtr` 20 :: Ptr Word32) generalShader
      poke (p `plusPtr` 24 :: Ptr Word32) closestHitShader
      poke (p `plusPtr` 28 :: Ptr Word32) anyHitShader
      poke (p `plusPtr` 32 :: Ptr Word32) intersectionShader


instance ToCStruct RayTracingPipelineCreateInfoNV VkRayTracingPipelineCreateInfoNV where
  pokeCStruct :: Ptr VkRayTracingPipelineCreateInfoNV -> RayTracingPipelineCreateInfoNV -> IO a -> IO a
  pokeCStruct p RayTracingPipelineCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pStages <- ContT $ allocaArray @VkPipelineShaderStageCreateInfo (Data.Vector.length stages)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pStages `advancePtr` i) s . ($ ())) stages
    pGroups <- ContT $ allocaArray @VkRayTracingShaderGroupCreateInfoNV (Data.Vector.length groups)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pGroups `advancePtr` i) s . ($ ())) groups
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineCreateFlags) flags
      poke (p `plusPtr` 20 :: Ptr Word32) (fromIntegral $ Data.Vector.length stages :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkPipelineShaderStageCreateInfo)) pStages
      poke (p `plusPtr` 32 :: Ptr Word32) (fromIntegral $ Data.Vector.length groups :: Word32)
      poke (p `plusPtr` 40 :: Ptr (Ptr VkRayTracingShaderGroupCreateInfoNV)) pGroups
      poke (p `plusPtr` 48 :: Ptr Word32) maxRecursionDepth
      poke (p `plusPtr` 56 :: Ptr VkPipelineLayout) layout
      poke (p `plusPtr` 64 :: Ptr VkPipeline) basePipelineHandle
      poke (p `plusPtr` 72 :: Ptr Int32) basePipelineIndex


instance ToCStruct GeometryTrianglesNV VkGeometryTrianglesNV where
  pokeCStruct :: Ptr VkGeometryTrianglesNV -> GeometryTrianglesNV -> IO a -> IO a
  pokeCStruct p GeometryTrianglesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBuffer) vertexData
      poke (p `plusPtr` 24 :: Ptr VkDeviceSize) vertexOffset
      poke (p `plusPtr` 32 :: Ptr Word32) vertexCount
      poke (p `plusPtr` 40 :: Ptr VkDeviceSize) vertexStride
      poke (p `plusPtr` 48 :: Ptr VkFormat) vertexFormat
      poke (p `plusPtr` 56 :: Ptr VkBuffer) indexData
      poke (p `plusPtr` 64 :: Ptr VkDeviceSize) indexOffset
      poke (p `plusPtr` 72 :: Ptr Word32) indexCount
      poke (p `plusPtr` 76 :: Ptr VkIndexType) indexType
      poke (p `plusPtr` 80 :: Ptr VkBuffer) transformData
      poke (p `plusPtr` 88 :: Ptr VkDeviceSize) transformOffset


instance ToCStruct GeometryAABBNV VkGeometryAABBNV where
  pokeCStruct :: Ptr VkGeometryAABBNV -> GeometryAABBNV -> IO a -> IO a
  pokeCStruct p GeometryAABBNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_GEOMETRY_AABB_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBuffer) aabbData
      poke (p `plusPtr` 24 :: Ptr Word32) numAABBs
      poke (p `plusPtr` 28 :: Ptr Word32) stride
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) offset


instance ToCStruct GeometryDataNV VkGeometryDataNV where
  pokeCStruct :: Ptr VkGeometryDataNV -> GeometryDataNV -> IO a -> IO a
  pokeCStruct p GeometryDataNV{..} = (. const) . runContT $ do
    ContT $ pokeCStruct (p `plusPtr` 0) triangles . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 96) aabbs . ($ ())
    


instance ToCStruct GeometryNV VkGeometryNV where
  pokeCStruct :: Ptr VkGeometryNV -> GeometryNV -> IO a -> IO a
  pokeCStruct p GeometryNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 24) geometry . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_GEOMETRY_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkGeometryTypeNV) geometryType
      poke (p `plusPtr` 160 :: Ptr VkGeometryFlagsNV) flags


instance ToCStruct AccelerationStructureInfoNV VkAccelerationStructureInfoNV where
  pokeCStruct :: Ptr VkAccelerationStructureInfoNV -> AccelerationStructureInfoNV -> IO a -> IO a
  pokeCStruct p AccelerationStructureInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pGeometries <- ContT $ allocaArray @VkGeometryNV (Data.Vector.length geometries)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pGeometries `advancePtr` i) s . ($ ())) geometries
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAccelerationStructureTypeNV) type'
      poke (p `plusPtr` 20 :: Ptr VkBuildAccelerationStructureFlagsNV) flags
      poke (p `plusPtr` 24 :: Ptr Word32) instanceCount
      poke (p `plusPtr` 28 :: Ptr Word32) (fromIntegral $ Data.Vector.length geometries :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkGeometryNV)) pGeometries


instance ToCStruct AccelerationStructureCreateInfoNV VkAccelerationStructureCreateInfoNV where
  pokeCStruct :: Ptr VkAccelerationStructureCreateInfoNV -> AccelerationStructureCreateInfoNV -> IO a -> IO a
  pokeCStruct p AccelerationStructureCreateInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 24) info . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceSize) compactedSize


instance ToCStruct BindAccelerationStructureMemoryInfoNV VkBindAccelerationStructureMemoryInfoNV where
  pokeCStruct :: Ptr VkBindAccelerationStructureMemoryInfoNV -> BindAccelerationStructureMemoryInfoNV -> IO a -> IO a
  pokeCStruct p BindAccelerationStructureMemoryInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDeviceIndices <- ContT $ allocaArray @Word32 (Data.Vector.length deviceIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDeviceIndices) deviceIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAccelerationStructureNV) accelerationStructure
      poke (p `plusPtr` 24 :: Ptr VkDeviceMemory) memory
      poke (p `plusPtr` 32 :: Ptr VkDeviceSize) memoryOffset
      poke (p `plusPtr` 40 :: Ptr Word32) (fromIntegral $ Data.Vector.length deviceIndices :: Word32)
      poke (p `plusPtr` 48 :: Ptr (Ptr Word32)) pDeviceIndices


instance ToCStruct WriteDescriptorSetAccelerationStructureNV VkWriteDescriptorSetAccelerationStructureNV where
  pokeCStruct :: Ptr VkWriteDescriptorSetAccelerationStructureNV -> WriteDescriptorSetAccelerationStructureNV -> IO a -> IO a
  pokeCStruct p WriteDescriptorSetAccelerationStructureNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pAccelerationStructures <- ContT $ allocaArray @VkAccelerationStructureNV (Data.Vector.length accelerationStructures)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pAccelerationStructures) accelerationStructures
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length accelerationStructures :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr VkAccelerationStructureNV)) pAccelerationStructures


instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV VkAccelerationStructureMemoryRequirementsInfoNV where
  pokeCStruct :: Ptr VkAccelerationStructureMemoryRequirementsInfoNV -> AccelerationStructureMemoryRequirementsInfoNV -> IO a -> IO a
  pokeCStruct p AccelerationStructureMemoryRequirementsInfoNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkAccelerationStructureMemoryRequirementsTypeNV) type'
      poke (p `plusPtr` 24 :: Ptr VkAccelerationStructureNV) accelerationStructure


instance ToCStruct PhysicalDeviceRayTracingPropertiesNV VkPhysicalDeviceRayTracingPropertiesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceRayTracingPropertiesNV -> PhysicalDeviceRayTracingPropertiesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceRayTracingPropertiesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) shaderGroupHandleSize
      poke (p `plusPtr` 20 :: Ptr Word32) maxRecursionDepth
      poke (p `plusPtr` 24 :: Ptr Word32) maxShaderGroupStride
      poke (p `plusPtr` 28 :: Ptr Word32) shaderGroupBaseAlignment
      poke (p `plusPtr` 32 :: Ptr Word64) maxGeometryCount
      poke (p `plusPtr` 40 :: Ptr Word64) maxInstanceCount
      poke (p `plusPtr` 48 :: Ptr Word64) maxTriangleCount
      poke (p `plusPtr` 56 :: Ptr Word32) maxDescriptorSetAccelerationStructures


instance ToCStruct DrmFormatModifierPropertiesListEXT VkDrmFormatModifierPropertiesListEXT where
  pokeCStruct :: Ptr VkDrmFormatModifierPropertiesListEXT -> DrmFormatModifierPropertiesListEXT -> IO a -> IO a
  pokeCStruct p DrmFormatModifierPropertiesListEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) drmFormatModifierCount
      poke (p `plusPtr` 24 :: Ptr (Ptr VkDrmFormatModifierPropertiesEXT)) (drmFormatModifierProperties :: Ptr VkDrmFormatModifierPropertiesEXT)


instance ToCStruct DrmFormatModifierPropertiesEXT VkDrmFormatModifierPropertiesEXT where
  pokeCStruct :: Ptr VkDrmFormatModifierPropertiesEXT -> DrmFormatModifierPropertiesEXT -> IO a -> IO a
  pokeCStruct p DrmFormatModifierPropertiesEXT{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr Word64) drmFormatModifier
      poke (p `plusPtr` 8 :: Ptr Word32) drmFormatModifierPlaneCount
      poke (p `plusPtr` 12 :: Ptr VkFormatFeatureFlags) drmFormatModifierTilingFeatures


instance ToCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT VkPhysicalDeviceImageDrmFormatModifierInfoEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceImageDrmFormatModifierInfoEXT -> PhysicalDeviceImageDrmFormatModifierInfoEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceImageDrmFormatModifierInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pQueueFamilyIndices <- ContT $ allocaArray @Word32 (Data.Vector.length queueFamilyIndices)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pQueueFamilyIndices) queueFamilyIndices
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word64) drmFormatModifier
      poke (p `plusPtr` 24 :: Ptr VkSharingMode) sharingMode
      poke (p `plusPtr` 28 :: Ptr Word32) (fromIntegral $ Data.Vector.length queueFamilyIndices :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr Word32)) pQueueFamilyIndices


instance ToCStruct ImageDrmFormatModifierListCreateInfoEXT VkImageDrmFormatModifierListCreateInfoEXT where
  pokeCStruct :: Ptr VkImageDrmFormatModifierListCreateInfoEXT -> ImageDrmFormatModifierListCreateInfoEXT -> IO a -> IO a
  pokeCStruct p ImageDrmFormatModifierListCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pDrmFormatModifiers <- ContT $ allocaArray @Word64 (Data.Vector.length drmFormatModifiers)
    liftIO $ do
      Data.Vector.imapM_ (pokeElemOff pDrmFormatModifiers) drmFormatModifiers
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) (fromIntegral $ Data.Vector.length drmFormatModifiers :: Word32)
      poke (p `plusPtr` 24 :: Ptr (Ptr Word64)) pDrmFormatModifiers


instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT VkImageDrmFormatModifierExplicitCreateInfoEXT where
  pokeCStruct :: Ptr VkImageDrmFormatModifierExplicitCreateInfoEXT -> ImageDrmFormatModifierExplicitCreateInfoEXT -> IO a -> IO a
  pokeCStruct p ImageDrmFormatModifierExplicitCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    pPlaneLayouts <- ContT $ allocaArray @VkSubresourceLayout (Data.Vector.length planeLayouts)
    Data.Vector.imapM_ (\i s -> ContT $ pokeCStruct (pPlaneLayouts `advancePtr` i) s . ($ ())) planeLayouts
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word64) drmFormatModifier
      poke (p `plusPtr` 24 :: Ptr Word32) (fromIntegral $ Data.Vector.length planeLayouts :: Word32)
      poke (p `plusPtr` 32 :: Ptr (Ptr VkSubresourceLayout)) pPlaneLayouts


instance ToCStruct ImageDrmFormatModifierPropertiesEXT VkImageDrmFormatModifierPropertiesEXT where
  pokeCStruct :: Ptr VkImageDrmFormatModifierPropertiesEXT -> ImageDrmFormatModifierPropertiesEXT -> IO a -> IO a
  pokeCStruct p ImageDrmFormatModifierPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word64) drmFormatModifier


instance ToCStruct ImageStencilUsageCreateInfoEXT VkImageStencilUsageCreateInfoEXT where
  pokeCStruct :: Ptr VkImageStencilUsageCreateInfoEXT -> ImageStencilUsageCreateInfoEXT -> IO a -> IO a
  pokeCStruct p ImageStencilUsageCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageUsageFlags) stencilUsage


instance ToCStruct DeviceMemoryOverallocationCreateInfoAMD VkDeviceMemoryOverallocationCreateInfoAMD where
  pokeCStruct :: Ptr VkDeviceMemoryOverallocationCreateInfoAMD -> DeviceMemoryOverallocationCreateInfoAMD -> IO a -> IO a
  pokeCStruct p DeviceMemoryOverallocationCreateInfoAMD{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkMemoryOverallocationBehaviorAMD) overallocationBehavior


instance ToCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceFragmentDensityMapFeaturesEXT -> PhysicalDeviceFragmentDensityMapFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFragmentDensityMapFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 fragmentDensityMap :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 fragmentDensityMapDynamic :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 fragmentDensityMapNonSubsampledImages :: VkBool32)


instance ToCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceFragmentDensityMapPropertiesEXT -> PhysicalDeviceFragmentDensityMapPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceFragmentDensityMapPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) minFragmentDensityTexelSize . ($ ())
    ContT $ pokeCStruct (p `plusPtr` 24) maxFragmentDensityTexelSize . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 32 :: Ptr VkBool32) (boolToBool32 fragmentDensityInvocations :: VkBool32)


instance ToCStruct RenderPassFragmentDensityMapCreateInfoEXT VkRenderPassFragmentDensityMapCreateInfoEXT where
  pokeCStruct :: Ptr VkRenderPassFragmentDensityMapCreateInfoEXT -> RenderPassFragmentDensityMapCreateInfoEXT -> IO a -> IO a
  pokeCStruct p RenderPassFragmentDensityMapCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    ContT $ pokeCStruct (p `plusPtr` 16) fragmentDensityMapAttachment . ($ ())
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext


instance ToCStruct PhysicalDeviceScalarBlockLayoutFeaturesEXT VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceScalarBlockLayoutFeaturesEXT -> PhysicalDeviceScalarBlockLayoutFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceScalarBlockLayoutFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 scalarBlockLayout :: VkBool32)


instance ToCStruct SurfaceProtectedCapabilitiesKHR VkSurfaceProtectedCapabilitiesKHR where
  pokeCStruct :: Ptr VkSurfaceProtectedCapabilitiesKHR -> SurfaceProtectedCapabilitiesKHR -> IO a -> IO a
  pokeCStruct p SurfaceProtectedCapabilitiesKHR{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 supportsProtected :: VkBool32)


instance ToCStruct PhysicalDeviceDepthClipEnableFeaturesEXT VkPhysicalDeviceDepthClipEnableFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceDepthClipEnableFeaturesEXT -> PhysicalDeviceDepthClipEnableFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceDepthClipEnableFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 depthClipEnable :: VkBool32)


instance ToCStruct PipelineRasterizationDepthClipStateCreateInfoEXT VkPipelineRasterizationDepthClipStateCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineRasterizationDepthClipStateCreateInfoEXT -> PipelineRasterizationDepthClipStateCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineRasterizationDepthClipStateCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkPipelineRasterizationDepthClipStateCreateFlagsEXT) flags
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 depthClipEnable :: VkBool32)


instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT VkPhysicalDeviceMemoryBudgetPropertiesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceMemoryBudgetPropertiesEXT -> PhysicalDeviceMemoryBudgetPropertiesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMemoryBudgetPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      Data.Vector.imapM_ (pokeElemOff (p `plusPtr` 16)) (Data.Vector.take VK_MAX_MEMORY_HEAPS heapBudget)
      Data.Vector.imapM_ (pokeElemOff (p `plusPtr` 144)) (Data.Vector.take VK_MAX_MEMORY_HEAPS heapUsage)


instance ToCStruct PhysicalDeviceMemoryPriorityFeaturesEXT VkPhysicalDeviceMemoryPriorityFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceMemoryPriorityFeaturesEXT -> PhysicalDeviceMemoryPriorityFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceMemoryPriorityFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 memoryPriority :: VkBool32)


instance ToCStruct MemoryPriorityAllocateInfoEXT VkMemoryPriorityAllocateInfoEXT where
  pokeCStruct :: Ptr VkMemoryPriorityAllocateInfoEXT -> MemoryPriorityAllocateInfoEXT -> IO a -> IO a
  pokeCStruct p MemoryPriorityAllocateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr CFloat) (CFloat priority :: CFloat)


instance ToCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceBufferDeviceAddressFeaturesEXT -> PhysicalDeviceBufferDeviceAddressFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceBufferDeviceAddressFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 bufferDeviceAddress :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 bufferDeviceAddressCaptureReplay :: VkBool32)
      poke (p `plusPtr` 24 :: Ptr VkBool32) (boolToBool32 bufferDeviceAddressMultiDevice :: VkBool32)


instance ToCStruct BufferDeviceAddressInfoEXT VkBufferDeviceAddressInfoEXT where
  pokeCStruct :: Ptr VkBufferDeviceAddressInfoEXT -> BufferDeviceAddressInfoEXT -> IO a -> IO a
  pokeCStruct p BufferDeviceAddressInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBuffer) buffer


instance ToCStruct BufferDeviceAddressCreateInfoEXT VkBufferDeviceAddressCreateInfoEXT where
  pokeCStruct :: Ptr VkBufferDeviceAddressCreateInfoEXT -> BufferDeviceAddressCreateInfoEXT -> IO a -> IO a
  pokeCStruct p BufferDeviceAddressCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkDeviceAddress) deviceAddress


instance ToCStruct PhysicalDeviceImageViewImageFormatInfoEXT VkPhysicalDeviceImageViewImageFormatInfoEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceImageViewImageFormatInfoEXT -> PhysicalDeviceImageViewImageFormatInfoEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceImageViewImageFormatInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageViewType) imageViewType


instance ToCStruct FilterCubicImageViewImageFormatPropertiesEXT VkFilterCubicImageViewImageFormatPropertiesEXT where
  pokeCStruct :: Ptr VkFilterCubicImageViewImageFormatPropertiesEXT -> FilterCubicImageViewImageFormatPropertiesEXT -> IO a -> IO a
  pokeCStruct p FilterCubicImageViewImageFormatPropertiesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 filterCubic :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 filterCubicMinmax :: VkBool32)


instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesNV VkPhysicalDeviceCooperativeMatrixFeaturesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceCooperativeMatrixFeaturesNV -> PhysicalDeviceCooperativeMatrixFeaturesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceCooperativeMatrixFeaturesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 cooperativeMatrix :: VkBool32)
      poke (p `plusPtr` 20 :: Ptr VkBool32) (boolToBool32 cooperativeMatrixRobustBufferAccess :: VkBool32)


instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesNV VkPhysicalDeviceCooperativeMatrixPropertiesNV where
  pokeCStruct :: Ptr VkPhysicalDeviceCooperativeMatrixPropertiesNV -> PhysicalDeviceCooperativeMatrixPropertiesNV -> IO a -> IO a
  pokeCStruct p PhysicalDeviceCooperativeMatrixPropertiesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkShaderStageFlags) cooperativeMatrixSupportedStages


instance ToCStruct CooperativeMatrixPropertiesNV VkCooperativeMatrixPropertiesNV where
  pokeCStruct :: Ptr VkCooperativeMatrixPropertiesNV -> CooperativeMatrixPropertiesNV -> IO a -> IO a
  pokeCStruct p CooperativeMatrixPropertiesNV{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr Word32) mSize
      poke (p `plusPtr` 20 :: Ptr Word32) nSize
      poke (p `plusPtr` 24 :: Ptr Word32) kSize
      poke (p `plusPtr` 28 :: Ptr VkComponentTypeNV) aType
      poke (p `plusPtr` 32 :: Ptr VkComponentTypeNV) bType
      poke (p `plusPtr` 36 :: Ptr VkComponentTypeNV) cType
      poke (p `plusPtr` 40 :: Ptr VkComponentTypeNV) dType
      poke (p `plusPtr` 44 :: Ptr VkScopeNV) scope


instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT VkPhysicalDeviceYcbcrImageArraysFeaturesEXT where
  pokeCStruct :: Ptr VkPhysicalDeviceYcbcrImageArraysFeaturesEXT -> PhysicalDeviceYcbcrImageArraysFeaturesEXT -> IO a -> IO a
  pokeCStruct p PhysicalDeviceYcbcrImageArraysFeaturesEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 ycbcrImageArrays :: VkBool32)


instance ToCStruct ImageViewHandleInfoNVX VkImageViewHandleInfoNVX where
  pokeCStruct :: Ptr VkImageViewHandleInfoNVX -> ImageViewHandleInfoNVX -> IO a -> IO a
  pokeCStruct p ImageViewHandleInfoNVX{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkImageView) imageView
      poke (p `plusPtr` 24 :: Ptr VkDescriptorType) descriptorType
      poke (p `plusPtr` 32 :: Ptr VkSampler) sampler


instance ToCStruct PresentFrameTokenGGP VkPresentFrameTokenGGP where
  pokeCStruct :: Ptr VkPresentFrameTokenGGP -> PresentFrameTokenGGP -> IO a -> IO a
  pokeCStruct p PresentFrameTokenGGP{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr GgpFrameToken) frameToken


instance ToCStruct PipelineCreationFeedbackEXT VkPipelineCreationFeedbackEXT where
  pokeCStruct :: Ptr VkPipelineCreationFeedbackEXT -> PipelineCreationFeedbackEXT -> IO a -> IO a
  pokeCStruct p PipelineCreationFeedbackEXT{..} = (. const) . runContT $    
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkPipelineCreationFeedbackFlagsEXT) flags
      poke (p `plusPtr` 8 :: Ptr Word64) duration


instance ToCStruct PipelineCreationFeedbackCreateInfoEXT VkPipelineCreationFeedbackCreateInfoEXT where
  pokeCStruct :: Ptr VkPipelineCreationFeedbackCreateInfoEXT -> PipelineCreationFeedbackCreateInfoEXT -> IO a -> IO a
  pokeCStruct p PipelineCreationFeedbackCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr (Ptr VkPipelineCreationFeedbackEXT)) (pipelineCreationFeedback :: Ptr VkPipelineCreationFeedbackEXT)
      poke (p `plusPtr` 24 :: Ptr Word32) pipelineStageCreationFeedbackCount
      poke (p `plusPtr` 32 :: Ptr (Ptr VkPipelineCreationFeedbackEXT)) (pipelineStageCreationFeedbacks :: Ptr VkPipelineCreationFeedbackEXT)


instance ToCStruct SurfaceFullScreenExclusiveInfoEXT VkSurfaceFullScreenExclusiveInfoEXT where
  pokeCStruct :: Ptr VkSurfaceFullScreenExclusiveInfoEXT -> SurfaceFullScreenExclusiveInfoEXT -> IO a -> IO a
  pokeCStruct p SurfaceFullScreenExclusiveInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkFullScreenExclusiveEXT) fullScreenExclusive


instance ToCStruct SurfaceFullScreenExclusiveWin32InfoEXT VkSurfaceFullScreenExclusiveWin32InfoEXT where
  pokeCStruct :: Ptr VkSurfaceFullScreenExclusiveWin32InfoEXT -> SurfaceFullScreenExclusiveWin32InfoEXT -> IO a -> IO a
  pokeCStruct p SurfaceFullScreenExclusiveWin32InfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr HMONITOR) hmonitor


instance ToCStruct SurfaceCapabilitiesFullScreenExclusiveEXT VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  pokeCStruct :: Ptr VkSurfaceCapabilitiesFullScreenExclusiveEXT -> SurfaceCapabilitiesFullScreenExclusiveEXT -> IO a -> IO a
  pokeCStruct p SurfaceCapabilitiesFullScreenExclusiveEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkBool32) (boolToBool32 fullScreenExclusiveSupported :: VkBool32)


instance ToCStruct HeadlessSurfaceCreateInfoEXT VkHeadlessSurfaceCreateInfoEXT where
  pokeCStruct :: Ptr VkHeadlessSurfaceCreateInfoEXT -> HeadlessSurfaceCreateInfoEXT -> IO a -> IO a
  pokeCStruct p HeadlessSurfaceCreateInfoEXT{..} = (. const) . runContT $ do
    pNext <- ContT $ maybeWith withSomeVkStruct next
    liftIO $ do
      poke (p `plusPtr` 0 :: Ptr VkStructureType) (STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT :: VkStructureType)
      poke (p `plusPtr` 8 :: Ptr (Ptr ())) pNext
      poke (p `plusPtr` 16 :: Ptr VkHeadlessSurfaceCreateFlagsEXT) flags


instance ToCStruct ClearColorValue VkClearColorValue where
  pokeCStruct :: Ptr VkClearColorValue -> ClearColorValue -> IO a -> IO a
  pokeCStruct p u = (. const) . runContT $ case u of
    Float32 float32 ->     
      liftIO $ do
        case float32 of
          (float321, float322, float323, float324) -> do
            pokeElemOff @CFloat (p `plusPtr` 0) 0 (CFloat float321)
            pokeElemOff @CFloat (p `plusPtr` 0) 1 (CFloat float322)
            pokeElemOff @CFloat (p `plusPtr` 0) 2 (CFloat float323)
            pokeElemOff @CFloat (p `plusPtr` 0) 3 (CFloat float324)
    Int32 int32 ->     
      liftIO $ do
        case int32 of
          (int321, int322, int323, int324) -> do
            pokeElemOff @Int32 (p `plusPtr` 0) 0 int321
            pokeElemOff @Int32 (p `plusPtr` 0) 1 int322
            pokeElemOff @Int32 (p `plusPtr` 0) 2 int323
            pokeElemOff @Int32 (p `plusPtr` 0) 3 int324
    Uint32 uint32 ->     
      liftIO $ do
        case uint32 of
          (uint321, uint322, uint323, uint324) -> do
            pokeElemOff @Word32 (p `plusPtr` 0) 0 uint321
            pokeElemOff @Word32 (p `plusPtr` 0) 1 uint322
            pokeElemOff @Word32 (p `plusPtr` 0) 2 uint323
            pokeElemOff @Word32 (p `plusPtr` 0) 3 uint324


instance ToCStruct ClearValue VkClearValue where
  pokeCStruct :: Ptr VkClearValue -> ClearValue -> IO a -> IO a
  pokeCStruct p u = (. const) . runContT $ case u of
    Color color ->  do
        ContT $ pokeCStruct (p `plusPtr` 0) color . ($ ())
      
    DepthStencil depthStencil ->  do
        ContT $ pokeCStruct (p `plusPtr` 0) depthStencil . ($ ())
      
