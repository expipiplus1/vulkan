{-# language Strict #-}
{-# language CPP #-}
{-# language FunctionalDependencies #-}
{-# language DataKinds #-}
{-# language ExplicitNamespaces #-}
{-# language FlexibleContexts #-}
{-# language GADTs #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language StandaloneDeriving #-}
{-# language TypeApplications #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Marshal.SomeVkStruct
  ( ToCStruct(..)
  , FromCStruct(..)
  , SomeVkStruct(..)
  , HasPNext(..)
  , fromSomeVkStruct
  , fromSomeVkStructChain
  , withSomeVkStruct
  , withCStructPtr
  , fromCStructPtr
  , fromCStructPtrElem
  , peekVkStruct
  ) where

import Control.Applicative
  ( (<|>)
  )
import Control.Exception
  ( throwIO
  )
import Data.Type.Equality
  ( (:~:)(Refl)
  )
import Data.Typeable
  ( Typeable
  , cast
  , eqT
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Ptr
  ( Ptr
  , castPtr
  )
import Foreign.Storable
  ( Storable
  , peek
  , peekElemOff
  , poke
  )
import GHC.IO.Exception
  ( IOErrorType(InvalidArgument)
  , IOException(..)
  )


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferInheritanceInfo(..)
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
  , VkMemoryBarrier(..)
  , VkRenderPassBeginInfo(..)
  )
import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkStructureType(..)
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
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
  , VkWriteDescriptorSet(..)
  )
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkApplicationInfo(..)
  , VkExtent3D(..)
  , VkFormatProperties(..)
  , VkImageFormatProperties(..)
  , VkInstanceCreateInfo(..)
  , VkMemoryHeap(..)
  , VkMemoryType(..)
  , VkPhysicalDeviceFeatures(..)
  , VkPhysicalDeviceLimits(..)
  , VkPhysicalDeviceMemoryProperties(..)
  , VkPhysicalDeviceProperties(..)
  , VkPhysicalDeviceSparseProperties(..)
  , VkQueueFamilyProperties(..)
  )
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.ExtensionDiscovery
  ( VkExtensionProperties(..)
  )
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  , VkSubresourceLayout(..)
  )
import Graphics.Vulkan.C.Core10.ImageView
  ( VkComponentMapping(..)
  , VkImageSubresourceRange(..)
  , VkImageViewCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.LayerDiscovery
  ( VkLayerProperties(..)
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkMemoryRequirements(..)
  )
import Graphics.Vulkan.C.Core10.Pass
  ( VkAttachmentDescription(..)
  , VkAttachmentReference(..)
  , VkFramebufferCreateInfo(..)
  , VkRenderPassCreateInfo(..)
  , VkSubpassDependency(..)
  , VkSubpassDescription(..)
  )
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkComputePipelineCreateInfo(..)
  , VkExtent2D(..)
  , VkGraphicsPipelineCreateInfo(..)
  , VkOffset2D(..)
  , VkPipelineColorBlendAttachmentState(..)
  , VkPipelineColorBlendStateCreateInfo(..)
  , VkPipelineDepthStencilStateCreateInfo(..)
  , VkPipelineDynamicStateCreateInfo(..)
  , VkPipelineInputAssemblyStateCreateInfo(..)
  , VkPipelineMultisampleStateCreateInfo(..)
  , VkPipelineRasterizationStateCreateInfo(..)
  , VkPipelineShaderStageCreateInfo(..)
  , VkPipelineTessellationStateCreateInfo(..)
  , VkPipelineVertexInputStateCreateInfo(..)
  , VkPipelineViewportStateCreateInfo(..)
  , VkRect2D(..)
  , VkSpecializationInfo(..)
  , VkSpecializationMapEntry(..)
  , VkStencilOpState(..)
  , VkVertexInputAttributeDescription(..)
  , VkVertexInputBindingDescription(..)
  , VkViewport(..)
  )
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateInfo(..)
  , VkPushConstantRange(..)
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPoolCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkSubmitInfo(..)
  )
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSamplerCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateInfo(..)
  )
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  , VkImageSubresource(..)
  , VkOffset3D(..)
  , VkSparseBufferMemoryBindInfo(..)
  , VkSparseImageFormatProperties(..)
  , VkSparseImageMemoryBind(..)
  , VkSparseImageMemoryBindInfo(..)
  , VkSparseImageMemoryRequirements(..)
  , VkSparseImageOpaqueMemoryBindInfo(..)
  , VkSparseMemoryBind(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkProtectedSubmitInfo(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkPhysicalDeviceSubgroupProperties(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage
  ( VkPhysicalDevice16BitStorageFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateInfo(..)
  , VkDescriptorUpdateTemplateEntry(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group
  ( VkDeviceGroupBindSparseInfo(..)
  , VkDeviceGroupCommandBufferBeginInfo(..)
  , VkDeviceGroupRenderPassBeginInfo(..)
  , VkDeviceGroupSubmitInfo(..)
  , VkMemoryAllocateFlagsInfo(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkExternalMemoryProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkPhysicalDeviceIDProperties(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( VkBufferMemoryRequirementsInfo2(..)
  , VkImageMemoryRequirementsInfo2(..)
  , VkImageSparseMemoryRequirementsInfo2(..)
  , VkMemoryRequirements2(..)
  , VkSparseImageMemoryRequirements2(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2
  , pattern VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2
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
  , pattern VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2
  , pattern VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkInputAttachmentAspectReference(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( VkBindImagePlaneMemoryInfo(..)
  , VkImagePlaneMemoryRequirementsInfo(..)
  , VkPhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , VkSamplerYcbcrConversionCreateInfo(..)
  , VkSamplerYcbcrConversionImageFormatProperties(..)
  , VkSamplerYcbcrConversionInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParameterFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointerFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( VkPipelineRasterizationStateRasterizationOrderAMD(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( VkPhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_info
  ( VkShaderResourceUsageAMD(..)
  , VkShaderStatisticsInfoAMD(..)
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( VkTextureLODGatherFormatPropertiesAMD(..)
  , pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkExternalFormatANDROID(..)
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_utils
  ( VkDebugUtilsLabelEXT(..)
  , VkDebugUtilsMessengerCallbackDataEXT(..)
  , VkDebugUtilsMessengerCreateInfoEXT(..)
  , VkDebugUtilsObjectNameInfoEXT(..)
  , VkDebugUtilsObjectTagInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_descriptor_indexing
  ( VkDescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , VkDescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , VkPhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , VkPhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , VkPipelineDiscardRectangleStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_control
  ( VkDeviceEventInfoEXT(..)
  , VkDisplayEventInfoEXT(..)
  , VkDisplayPowerInfoEXT(..)
  , VkSwapchainCounterCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkDeviceQueueGlobalPriorityCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , VkXYColorEXT(..)
  , pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT
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
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , VkSamplerReductionModeCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VkVertexInputBindingDivisorDescriptionEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  , pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateInfoKHR(..)
  , VkDisplayModeParametersKHR(..)
  , VkDisplayModePropertiesKHR(..)
  , VkDisplayPlaneCapabilitiesKHR(..)
  , VkDisplayPlanePropertiesKHR(..)
  , VkDisplayPropertiesKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( VkImageFormatListCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( VkPresentRegionKHR(..)
  , VkPresentRegionsKHR(..)
  , VkRectLayerKHR(..)
  , pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  )

#if defined(VK_USE_PLATFORM_MIR_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_mir_surface
  ( VkMirSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_swapchain
  ( VkAcquireNextImageInfoKHR(..)
  , VkBindImageMemorySwapchainInfoKHR(..)
  , VkDeviceGroupPresentCapabilitiesKHR(..)
  , VkDeviceGroupPresentInfoKHR(..)
  , VkDeviceGroupSwapchainCreateInfoKHR(..)
  , VkImageSwapchainCreateInfoKHR(..)
  , VkPresentInfoKHR(..)
  , VkSwapchainCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PRESENT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR
  )

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( VkXlibSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK(..)
  , pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK(..)
  , pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN(..)
  , pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdProcessCommandsInfoNVX(..)
  , VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkIndirectCommandsLayoutTokenNVX(..)
  , VkIndirectCommandsTokenNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , VkObjectTableDescriptorSetEntryNVX(..)
  , VkObjectTableEntryNVX(..)
  , VkObjectTableIndexBufferEntryNVX(..)
  , VkObjectTablePipelineEntryNVX(..)
  , VkObjectTablePushConstantEntryNVX(..)
  , VkObjectTableVertexBufferEntryNVX(..)
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  )
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  )
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , VkViewportWScalingNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( VkExportMemoryAllocateInfoNV(..)
  , VkExternalMemoryImageCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_capabilities
  ( VkExternalImageFormatPropertiesNV(..)
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkPipelineCoverageModulationStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateInfoNV(..)
  , VkViewportSwizzleNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  )
#endif
import Graphics.Vulkan.Core10.Buffer
  ( BufferCreateInfo(..)
  , fromCStructBufferCreateInfo
  , withCStructBufferCreateInfo
  )
import Graphics.Vulkan.Core10.BufferView
  ( BufferViewCreateInfo(..)
  , fromCStructBufferViewCreateInfo
  , withCStructBufferViewCreateInfo
  )
import Graphics.Vulkan.Core10.CommandBuffer
  ( CommandBufferAllocateInfo(..)
  , CommandBufferBeginInfo(..)
  , CommandBufferInheritanceInfo(..)
  , fromCStructCommandBufferAllocateInfo
  , fromCStructCommandBufferBeginInfo
  , fromCStructCommandBufferInheritanceInfo
  , withCStructCommandBufferAllocateInfo
  , withCStructCommandBufferBeginInfo
  , withCStructCommandBufferInheritanceInfo
  )
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( BufferCopy(..)
  , BufferImageCopy(..)
  , BufferMemoryBarrier(..)
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
  , ImageMemoryBarrier(..)
  , ImageResolve(..)
  , ImageSubresourceLayers(..)
  , MemoryBarrier(..)
  , RenderPassBeginInfo(..)
  , fromCStructBufferCopy
  , fromCStructBufferImageCopy
  , fromCStructBufferMemoryBarrier
  , fromCStructClearDepthStencilValue
  , fromCStructClearRect
  , fromCStructDispatchIndirectCommand
  , fromCStructDrawIndexedIndirectCommand
  , fromCStructDrawIndirectCommand
  , fromCStructImageBlit
  , fromCStructImageCopy
  , fromCStructImageMemoryBarrier
  , fromCStructImageResolve
  , fromCStructImageSubresourceLayers
  , fromCStructMemoryBarrier
  , withCStructBufferCopy
  , withCStructBufferImageCopy
  , withCStructBufferMemoryBarrier
  , withCStructClearAttachment
  , withCStructClearColorValue
  , withCStructClearDepthStencilValue
  , withCStructClearRect
  , withCStructClearValue
  , withCStructDispatchIndirectCommand
  , withCStructDrawIndexedIndirectCommand
  , withCStructDrawIndirectCommand
  , withCStructImageBlit
  , withCStructImageCopy
  , withCStructImageMemoryBarrier
  , withCStructImageResolve
  , withCStructImageSubresourceLayers
  , withCStructMemoryBarrier
  , withCStructRenderPassBeginInfo
  )
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPoolCreateInfo(..)
  , fromCStructCommandPoolCreateInfo
  , withCStructCommandPoolCreateInfo
  )
import Graphics.Vulkan.Core10.DescriptorSet
  ( CopyDescriptorSet(..)
  , DescriptorBufferInfo(..)
  , DescriptorImageInfo(..)
  , DescriptorPoolCreateInfo(..)
  , DescriptorPoolSize(..)
  , DescriptorSetAllocateInfo(..)
  , DescriptorSetLayoutBinding(..)
  , DescriptorSetLayoutCreateInfo(..)
  , WriteDescriptorSet(..)
  , fromCStructCopyDescriptorSet
  , fromCStructDescriptorBufferInfo
  , fromCStructDescriptorImageInfo
  , fromCStructDescriptorPoolCreateInfo
  , fromCStructDescriptorPoolSize
  , fromCStructDescriptorSetAllocateInfo
  , fromCStructDescriptorSetLayoutBinding
  , fromCStructDescriptorSetLayoutCreateInfo
  , fromCStructWriteDescriptorSet
  , withCStructCopyDescriptorSet
  , withCStructDescriptorBufferInfo
  , withCStructDescriptorImageInfo
  , withCStructDescriptorPoolCreateInfo
  , withCStructDescriptorPoolSize
  , withCStructDescriptorSetAllocateInfo
  , withCStructDescriptorSetLayoutBinding
  , withCStructDescriptorSetLayoutCreateInfo
  , withCStructWriteDescriptorSet
  )
import Graphics.Vulkan.Core10.Device
  ( DeviceCreateInfo(..)
  , DeviceQueueCreateInfo(..)
  , fromCStructDeviceCreateInfo
  , fromCStructDeviceQueueCreateInfo
  , withCStructDeviceCreateInfo
  , withCStructDeviceQueueCreateInfo
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , ApplicationInfo(..)
  , Extent3D(..)
  , FormatProperties(..)
  , ImageFormatProperties(..)
  , InstanceCreateInfo(..)
  , MemoryHeap(..)
  , MemoryType(..)
  , PhysicalDeviceFeatures(..)
  , PhysicalDeviceLimits(..)
  , PhysicalDeviceMemoryProperties(..)
  , PhysicalDeviceProperties(..)
  , PhysicalDeviceSparseProperties(..)
  , QueueFamilyProperties(..)
  , fromCStructAllocationCallbacks
  , fromCStructApplicationInfo
  , fromCStructExtent3D
  , fromCStructFormatProperties
  , fromCStructImageFormatProperties
  , fromCStructInstanceCreateInfo
  , fromCStructMemoryHeap
  , fromCStructMemoryType
  , fromCStructPhysicalDeviceFeatures
  , fromCStructPhysicalDeviceLimits
  , fromCStructPhysicalDeviceMemoryProperties
  , fromCStructPhysicalDeviceProperties
  , fromCStructPhysicalDeviceSparseProperties
  , fromCStructQueueFamilyProperties
  , withCStructAllocationCallbacks
  , withCStructApplicationInfo
  , withCStructExtent3D
  , withCStructFormatProperties
  , withCStructImageFormatProperties
  , withCStructInstanceCreateInfo
  , withCStructMemoryHeap
  , withCStructMemoryType
  , withCStructPhysicalDeviceFeatures
  , withCStructPhysicalDeviceLimits
  , withCStructPhysicalDeviceMemoryProperties
  , withCStructPhysicalDeviceProperties
  , withCStructPhysicalDeviceSparseProperties
  , withCStructQueueFamilyProperties
  )
import Graphics.Vulkan.Core10.Event
  ( EventCreateInfo(..)
  , fromCStructEventCreateInfo
  , withCStructEventCreateInfo
  )
import Graphics.Vulkan.Core10.ExtensionDiscovery
  ( ExtensionProperties(..)
  , fromCStructExtensionProperties
  , withCStructExtensionProperties
  )
import Graphics.Vulkan.Core10.Fence
  ( FenceCreateInfo(..)
  , fromCStructFenceCreateInfo
  , withCStructFenceCreateInfo
  )
import Graphics.Vulkan.Core10.Image
  ( ImageCreateInfo(..)
  , SubresourceLayout(..)
  , fromCStructImageCreateInfo
  , fromCStructSubresourceLayout
  , withCStructImageCreateInfo
  , withCStructSubresourceLayout
  )
import Graphics.Vulkan.Core10.ImageView
  ( ComponentMapping(..)
  , ImageSubresourceRange(..)
  , ImageViewCreateInfo(..)
  , fromCStructComponentMapping
  , fromCStructImageSubresourceRange
  , fromCStructImageViewCreateInfo
  , withCStructComponentMapping
  , withCStructImageSubresourceRange
  , withCStructImageViewCreateInfo
  )
import Graphics.Vulkan.Core10.LayerDiscovery
  ( LayerProperties(..)
  , fromCStructLayerProperties
  , withCStructLayerProperties
  )
import Graphics.Vulkan.Core10.Memory
  ( MappedMemoryRange(..)
  , MemoryAllocateInfo(..)
  , fromCStructMappedMemoryRange
  , fromCStructMemoryAllocateInfo
  , withCStructMappedMemoryRange
  , withCStructMemoryAllocateInfo
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( MemoryRequirements(..)
  , fromCStructMemoryRequirements
  , withCStructMemoryRequirements
  )
import Graphics.Vulkan.Core10.Pass
  ( AttachmentDescription(..)
  , AttachmentReference(..)
  , FramebufferCreateInfo(..)
  , RenderPassCreateInfo(..)
  , SubpassDependency(..)
  , SubpassDescription(..)
  , fromCStructAttachmentDescription
  , fromCStructAttachmentReference
  , fromCStructFramebufferCreateInfo
  , fromCStructRenderPassCreateInfo
  , fromCStructSubpassDependency
  , fromCStructSubpassDescription
  , withCStructAttachmentDescription
  , withCStructAttachmentReference
  , withCStructFramebufferCreateInfo
  , withCStructRenderPassCreateInfo
  , withCStructSubpassDependency
  , withCStructSubpassDescription
  )
import Graphics.Vulkan.Core10.Pipeline
  ( ComputePipelineCreateInfo(..)
  , Extent2D(..)
  , GraphicsPipelineCreateInfo(..)
  , Offset2D(..)
  , PipelineColorBlendAttachmentState(..)
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
  , Rect2D(..)
  , SpecializationInfo(..)
  , SpecializationMapEntry(..)
  , StencilOpState(..)
  , VertexInputAttributeDescription(..)
  , VertexInputBindingDescription(..)
  , Viewport(..)
  , fromCStructComputePipelineCreateInfo
  , fromCStructExtent2D
  , fromCStructGraphicsPipelineCreateInfo
  , fromCStructOffset2D
  , fromCStructPipelineColorBlendAttachmentState
  , fromCStructPipelineColorBlendStateCreateInfo
  , fromCStructPipelineDepthStencilStateCreateInfo
  , fromCStructPipelineDynamicStateCreateInfo
  , fromCStructPipelineInputAssemblyStateCreateInfo
  , fromCStructPipelineMultisampleStateCreateInfo
  , fromCStructPipelineRasterizationStateCreateInfo
  , fromCStructPipelineShaderStageCreateInfo
  , fromCStructPipelineTessellationStateCreateInfo
  , fromCStructPipelineVertexInputStateCreateInfo
  , fromCStructPipelineViewportStateCreateInfo
  , fromCStructRect2D
  , fromCStructSpecializationInfo
  , fromCStructSpecializationMapEntry
  , fromCStructStencilOpState
  , fromCStructVertexInputAttributeDescription
  , fromCStructVertexInputBindingDescription
  , fromCStructViewport
  , withCStructComputePipelineCreateInfo
  , withCStructExtent2D
  , withCStructGraphicsPipelineCreateInfo
  , withCStructOffset2D
  , withCStructPipelineColorBlendAttachmentState
  , withCStructPipelineColorBlendStateCreateInfo
  , withCStructPipelineDepthStencilStateCreateInfo
  , withCStructPipelineDynamicStateCreateInfo
  , withCStructPipelineInputAssemblyStateCreateInfo
  , withCStructPipelineMultisampleStateCreateInfo
  , withCStructPipelineRasterizationStateCreateInfo
  , withCStructPipelineShaderStageCreateInfo
  , withCStructPipelineTessellationStateCreateInfo
  , withCStructPipelineVertexInputStateCreateInfo
  , withCStructPipelineViewportStateCreateInfo
  , withCStructRect2D
  , withCStructSpecializationInfo
  , withCStructSpecializationMapEntry
  , withCStructStencilOpState
  , withCStructVertexInputAttributeDescription
  , withCStructVertexInputBindingDescription
  , withCStructViewport
  )
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCacheCreateInfo(..)
  , fromCStructPipelineCacheCreateInfo
  , withCStructPipelineCacheCreateInfo
  )
import Graphics.Vulkan.Core10.PipelineLayout
  ( PipelineLayoutCreateInfo(..)
  , PushConstantRange(..)
  , fromCStructPipelineLayoutCreateInfo
  , fromCStructPushConstantRange
  , withCStructPipelineLayoutCreateInfo
  , withCStructPushConstantRange
  )
import Graphics.Vulkan.Core10.Query
  ( QueryPoolCreateInfo(..)
  , fromCStructQueryPoolCreateInfo
  , withCStructQueryPoolCreateInfo
  )
import Graphics.Vulkan.Core10.Queue
  ( SubmitInfo(..)
  , withCStructSubmitInfo
  )
import Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateInfo(..)
  , fromCStructSemaphoreCreateInfo
  , withCStructSemaphoreCreateInfo
  )
import Graphics.Vulkan.Core10.Sampler
  ( SamplerCreateInfo(..)
  , fromCStructSamplerCreateInfo
  , withCStructSamplerCreateInfo
  )
import Graphics.Vulkan.Core10.Shader
  ( ShaderModuleCreateInfo(..)
  , fromCStructShaderModuleCreateInfo
  , withCStructShaderModuleCreateInfo
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
  , fromCStructBindSparseInfo
  , fromCStructImageSubresource
  , fromCStructOffset3D
  , fromCStructSparseBufferMemoryBindInfo
  , fromCStructSparseImageFormatProperties
  , fromCStructSparseImageMemoryBind
  , fromCStructSparseImageMemoryBindInfo
  , fromCStructSparseImageMemoryRequirements
  , fromCStructSparseImageOpaqueMemoryBindInfo
  , fromCStructSparseMemoryBind
  , withCStructBindSparseInfo
  , withCStructImageSubresource
  , withCStructOffset3D
  , withCStructSparseBufferMemoryBindInfo
  , withCStructSparseImageFormatProperties
  , withCStructSparseImageMemoryBind
  , withCStructSparseImageMemoryBindInfo
  , withCStructSparseImageMemoryRequirements
  , withCStructSparseImageOpaqueMemoryBindInfo
  , withCStructSparseMemoryBind
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( DeviceQueueInfo2(..)
  , PhysicalDeviceProtectedMemoryFeatures(..)
  , PhysicalDeviceProtectedMemoryProperties(..)
  , ProtectedSubmitInfo(..)
  , fromCStructDeviceQueueInfo2
  , fromCStructPhysicalDeviceProtectedMemoryFeatures
  , fromCStructPhysicalDeviceProtectedMemoryProperties
  , fromCStructProtectedSubmitInfo
  , withCStructDeviceQueueInfo2
  , withCStructPhysicalDeviceProtectedMemoryFeatures
  , withCStructPhysicalDeviceProtectedMemoryProperties
  , withCStructProtectedSubmitInfo
  )
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( PhysicalDeviceSubgroupProperties(..)
  , fromCStructPhysicalDeviceSubgroupProperties
  , withCStructPhysicalDeviceSubgroupProperties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeatures(..)
  , fromCStructPhysicalDevice16BitStorageFeatures
  , withCStructPhysicalDevice16BitStorageFeatures
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( BindBufferMemoryInfo(..)
  , BindImageMemoryInfo(..)
  , fromCStructBindBufferMemoryInfo
  , fromCStructBindImageMemoryInfo
  , withCStructBindBufferMemoryInfo
  , withCStructBindImageMemoryInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfo(..)
  , MemoryDedicatedRequirements(..)
  , fromCStructMemoryDedicatedAllocateInfo
  , fromCStructMemoryDedicatedRequirements
  , withCStructMemoryDedicatedAllocateInfo
  , withCStructMemoryDedicatedRequirements
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateInfo(..)
  , DescriptorUpdateTemplateEntry(..)
  , fromCStructDescriptorUpdateTemplateCreateInfo
  , fromCStructDescriptorUpdateTemplateEntry
  , withCStructDescriptorUpdateTemplateCreateInfo
  , withCStructDescriptorUpdateTemplateEntry
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( DeviceGroupBindSparseInfo(..)
  , DeviceGroupCommandBufferBeginInfo(..)
  , DeviceGroupRenderPassBeginInfo(..)
  , DeviceGroupSubmitInfo(..)
  , MemoryAllocateFlagsInfo(..)
  , fromCStructDeviceGroupBindSparseInfo
  , fromCStructDeviceGroupCommandBufferBeginInfo
  , fromCStructDeviceGroupRenderPassBeginInfo
  , fromCStructDeviceGroupSubmitInfo
  , fromCStructMemoryAllocateFlagsInfo
  , withCStructDeviceGroupBindSparseInfo
  , withCStructDeviceGroupCommandBufferBeginInfo
  , withCStructDeviceGroupRenderPassBeginInfo
  , withCStructDeviceGroupSubmitInfo
  , withCStructMemoryAllocateFlagsInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( BindBufferMemoryDeviceGroupInfo(..)
  , BindImageMemoryDeviceGroupInfo(..)
  , fromCStructBindBufferMemoryDeviceGroupInfo
  , fromCStructBindImageMemoryDeviceGroupInfo
  , withCStructBindBufferMemoryDeviceGroupInfo
  , withCStructBindImageMemoryDeviceGroupInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfo(..)
  , PhysicalDeviceGroupProperties(..)
  , withCStructDeviceGroupDeviceCreateInfo
  , withCStructPhysicalDeviceGroupProperties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( ExportFenceCreateInfo(..)
  , fromCStructExportFenceCreateInfo
  , withCStructExportFenceCreateInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceProperties(..)
  , PhysicalDeviceExternalFenceInfo(..)
  , fromCStructExternalFenceProperties
  , fromCStructPhysicalDeviceExternalFenceInfo
  , withCStructExternalFenceProperties
  , withCStructPhysicalDeviceExternalFenceInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( ExportMemoryAllocateInfo(..)
  , ExternalMemoryBufferCreateInfo(..)
  , ExternalMemoryImageCreateInfo(..)
  , fromCStructExportMemoryAllocateInfo
  , fromCStructExternalMemoryBufferCreateInfo
  , fromCStructExternalMemoryImageCreateInfo
  , withCStructExportMemoryAllocateInfo
  , withCStructExternalMemoryBufferCreateInfo
  , withCStructExternalMemoryImageCreateInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalBufferProperties(..)
  , ExternalImageFormatProperties(..)
  , ExternalMemoryProperties(..)
  , PhysicalDeviceExternalBufferInfo(..)
  , PhysicalDeviceExternalImageFormatInfo(..)
  , PhysicalDeviceIDProperties(..)
  , fromCStructExternalBufferProperties
  , fromCStructExternalImageFormatProperties
  , fromCStructExternalMemoryProperties
  , fromCStructPhysicalDeviceExternalBufferInfo
  , fromCStructPhysicalDeviceExternalImageFormatInfo
  , fromCStructPhysicalDeviceIDProperties
  , withCStructExternalBufferProperties
  , withCStructExternalImageFormatProperties
  , withCStructExternalMemoryProperties
  , withCStructPhysicalDeviceExternalBufferInfo
  , withCStructPhysicalDeviceExternalImageFormatInfo
  , withCStructPhysicalDeviceIDProperties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfo(..)
  , fromCStructExportSemaphoreCreateInfo
  , withCStructExportSemaphoreCreateInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreProperties(..)
  , PhysicalDeviceExternalSemaphoreInfo(..)
  , fromCStructExternalSemaphoreProperties
  , fromCStructPhysicalDeviceExternalSemaphoreInfo
  , withCStructExternalSemaphoreProperties
  , withCStructPhysicalDeviceExternalSemaphoreInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( BufferMemoryRequirementsInfo2(..)
  , ImageMemoryRequirementsInfo2(..)
  , ImageSparseMemoryRequirementsInfo2(..)
  , MemoryRequirements2(..)
  , SparseImageMemoryRequirements2(..)
  , fromCStructBufferMemoryRequirementsInfo2
  , fromCStructImageMemoryRequirementsInfo2
  , fromCStructImageSparseMemoryRequirementsInfo2
  , fromCStructMemoryRequirements2
  , fromCStructSparseImageMemoryRequirements2
  , withCStructBufferMemoryRequirementsInfo2
  , withCStructImageMemoryRequirementsInfo2
  , withCStructImageSparseMemoryRequirementsInfo2
  , withCStructMemoryRequirements2
  , withCStructSparseImageMemoryRequirements2
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
  , fromCStructFormatProperties2
  , fromCStructImageFormatProperties2
  , fromCStructPhysicalDeviceFeatures2
  , fromCStructPhysicalDeviceImageFormatInfo2
  , fromCStructPhysicalDeviceMemoryProperties2
  , fromCStructPhysicalDeviceProperties2
  , fromCStructPhysicalDeviceSparseImageFormatInfo2
  , fromCStructQueueFamilyProperties2
  , fromCStructSparseImageFormatProperties2
  , withCStructFormatProperties2
  , withCStructImageFormatProperties2
  , withCStructPhysicalDeviceFeatures2
  , withCStructPhysicalDeviceImageFormatInfo2
  , withCStructPhysicalDeviceMemoryProperties2
  , withCStructPhysicalDeviceProperties2
  , withCStructPhysicalDeviceSparseImageFormatInfo2
  , withCStructQueueFamilyProperties2
  , withCStructSparseImageFormatProperties2
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( ImageViewUsageCreateInfo(..)
  , InputAttachmentAspectReference(..)
  , PhysicalDevicePointClippingProperties(..)
  , PipelineTessellationDomainOriginStateCreateInfo(..)
  , RenderPassInputAttachmentAspectCreateInfo(..)
  , fromCStructImageViewUsageCreateInfo
  , fromCStructInputAttachmentAspectReference
  , fromCStructPhysicalDevicePointClippingProperties
  , fromCStructPipelineTessellationDomainOriginStateCreateInfo
  , fromCStructRenderPassInputAttachmentAspectCreateInfo
  , withCStructImageViewUsageCreateInfo
  , withCStructInputAttachmentAspectReference
  , withCStructPhysicalDevicePointClippingProperties
  , withCStructPipelineTessellationDomainOriginStateCreateInfo
  , withCStructRenderPassInputAttachmentAspectCreateInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( DescriptorSetLayoutSupport(..)
  , PhysicalDeviceMaintenance3Properties(..)
  , fromCStructDescriptorSetLayoutSupport
  , fromCStructPhysicalDeviceMaintenance3Properties
  , withCStructDescriptorSetLayoutSupport
  , withCStructPhysicalDeviceMaintenance3Properties
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeatures(..)
  , PhysicalDeviceMultiviewProperties(..)
  , RenderPassMultiviewCreateInfo(..)
  , fromCStructPhysicalDeviceMultiviewFeatures
  , fromCStructPhysicalDeviceMultiviewProperties
  , fromCStructRenderPassMultiviewCreateInfo
  , withCStructPhysicalDeviceMultiviewFeatures
  , withCStructPhysicalDeviceMultiviewProperties
  , withCStructRenderPassMultiviewCreateInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( BindImagePlaneMemoryInfo(..)
  , ImagePlaneMemoryRequirementsInfo(..)
  , PhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , SamplerYcbcrConversionCreateInfo(..)
  , SamplerYcbcrConversionImageFormatProperties(..)
  , SamplerYcbcrConversionInfo(..)
  , fromCStructBindImagePlaneMemoryInfo
  , fromCStructImagePlaneMemoryRequirementsInfo
  , fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures
  , fromCStructSamplerYcbcrConversionCreateInfo
  , fromCStructSamplerYcbcrConversionImageFormatProperties
  , fromCStructSamplerYcbcrConversionInfo
  , withCStructBindImagePlaneMemoryInfo
  , withCStructImagePlaneMemoryRequirementsInfo
  , withCStructPhysicalDeviceSamplerYcbcrConversionFeatures
  , withCStructSamplerYcbcrConversionCreateInfo
  , withCStructSamplerYcbcrConversionImageFormatProperties
  , withCStructSamplerYcbcrConversionInfo
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( PhysicalDeviceShaderDrawParameterFeatures(..)
  , fromCStructPhysicalDeviceShaderDrawParameterFeatures
  , withCStructPhysicalDeviceShaderDrawParameterFeatures
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointerFeatures(..)
  , fromCStructPhysicalDeviceVariablePointerFeatures
  , withCStructPhysicalDeviceVariablePointerFeatures
  )
import Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( PipelineRasterizationStateRasterizationOrderAMD(..)
  , fromCStructPipelineRasterizationStateRasterizationOrderAMD
  , withCStructPipelineRasterizationStateRasterizationOrderAMD
  )
import Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( PhysicalDeviceShaderCorePropertiesAMD(..)
  , fromCStructPhysicalDeviceShaderCorePropertiesAMD
  , withCStructPhysicalDeviceShaderCorePropertiesAMD
  )
import Graphics.Vulkan.Extensions.VK_AMD_shader_info
  ( ShaderResourceUsageAMD(..)
  , ShaderStatisticsInfoAMD(..)
  , fromCStructShaderResourceUsageAMD
  , fromCStructShaderStatisticsInfoAMD
  , withCStructShaderResourceUsageAMD
  , withCStructShaderStatisticsInfoAMD
  )
import Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  ( TextureLODGatherFormatPropertiesAMD(..)
  , fromCStructTextureLODGatherFormatPropertiesAMD
  , withCStructTextureLODGatherFormatPropertiesAMD
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AndroidHardwareBufferFormatPropertiesANDROID(..)
  , AndroidHardwareBufferPropertiesANDROID(..)
  , AndroidHardwareBufferUsageANDROID(..)
  , ExternalFormatANDROID(..)
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  , fromCStructAndroidHardwareBufferFormatPropertiesANDROID
  , fromCStructAndroidHardwareBufferPropertiesANDROID
  , fromCStructAndroidHardwareBufferUsageANDROID
  , fromCStructExternalFormatANDROID
  , fromCStructImportAndroidHardwareBufferInfoANDROID
  , fromCStructMemoryGetAndroidHardwareBufferInfoANDROID
  , withCStructAndroidHardwareBufferFormatPropertiesANDROID
  , withCStructAndroidHardwareBufferPropertiesANDROID
  , withCStructAndroidHardwareBufferUsageANDROID
  , withCStructExternalFormatANDROID
  , withCStructImportAndroidHardwareBufferInfoANDROID
  , withCStructMemoryGetAndroidHardwareBufferInfoANDROID
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  , fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  , fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT
  , withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
  , withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
  , withCStructPipelineColorBlendAdvancedStateCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , PipelineRasterizationConservativeStateCreateInfoEXT(..)
  , fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
  , fromCStructPipelineRasterizationConservativeStateCreateInfoEXT
  , withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
  , withCStructPipelineRasterizationConservativeStateCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( DebugMarkerMarkerInfoEXT(..)
  , DebugMarkerObjectNameInfoEXT(..)
  , DebugMarkerObjectTagInfoEXT(..)
  , fromCStructDebugMarkerMarkerInfoEXT
  , fromCStructDebugMarkerObjectNameInfoEXT
  , fromCStructDebugMarkerObjectTagInfoEXT
  , withCStructDebugMarkerMarkerInfoEXT
  , withCStructDebugMarkerObjectNameInfoEXT
  , withCStructDebugMarkerObjectTagInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportCallbackCreateInfoEXT(..)
  , fromCStructDebugReportCallbackCreateInfoEXT
  , withCStructDebugReportCallbackCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( DebugUtilsLabelEXT(..)
  , DebugUtilsMessengerCallbackDataEXT(..)
  , DebugUtilsMessengerCreateInfoEXT(..)
  , DebugUtilsObjectNameInfoEXT(..)
  , DebugUtilsObjectTagInfoEXT(..)
  , fromCStructDebugUtilsLabelEXT
  , fromCStructDebugUtilsMessengerCallbackDataEXT
  , fromCStructDebugUtilsMessengerCreateInfoEXT
  , fromCStructDebugUtilsObjectNameInfoEXT
  , fromCStructDebugUtilsObjectTagInfoEXT
  , withCStructDebugUtilsLabelEXT
  , withCStructDebugUtilsMessengerCallbackDataEXT
  , withCStructDebugUtilsMessengerCreateInfoEXT
  , withCStructDebugUtilsObjectNameInfoEXT
  , withCStructDebugUtilsObjectTagInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , PhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , PhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  , fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
  , fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
  , fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
  , fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
  , fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
  , withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
  , withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
  , withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
  , withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
  , withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( PhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , PipelineDiscardRectangleStateCreateInfoEXT(..)
  , fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT
  , fromCStructPipelineDiscardRectangleStateCreateInfoEXT
  , withCStructPhysicalDeviceDiscardRectanglePropertiesEXT
  , withCStructPipelineDiscardRectangleStateCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( DeviceEventInfoEXT(..)
  , DisplayEventInfoEXT(..)
  , DisplayPowerInfoEXT(..)
  , SwapchainCounterCreateInfoEXT(..)
  , fromCStructDeviceEventInfoEXT
  , fromCStructDisplayEventInfoEXT
  , fromCStructDisplayPowerInfoEXT
  , fromCStructSwapchainCounterCreateInfoEXT
  , withCStructDeviceEventInfoEXT
  , withCStructDisplayEventInfoEXT
  , withCStructDisplayPowerInfoEXT
  , withCStructSwapchainCounterCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCapabilities2EXT(..)
  , fromCStructSurfaceCapabilities2EXT
  , withCStructSurfaceCapabilities2EXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( ImportMemoryHostPointerInfoEXT(..)
  , MemoryHostPointerPropertiesEXT(..)
  , PhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , fromCStructImportMemoryHostPointerInfoEXT
  , fromCStructMemoryHostPointerPropertiesEXT
  , fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
  , withCStructImportMemoryHostPointerInfoEXT
  , withCStructMemoryHostPointerPropertiesEXT
  , withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( DeviceQueueGlobalPriorityCreateInfoEXT(..)
  , fromCStructDeviceQueueGlobalPriorityCreateInfoEXT
  , withCStructDeviceQueueGlobalPriorityCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  ( HdrMetadataEXT(..)
  , XYColorEXT(..)
  , fromCStructHdrMetadataEXT
  , fromCStructXYColorEXT
  , withCStructHdrMetadataEXT
  , withCStructXYColorEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( AttachmentSampleLocationsEXT(..)
  , MultisamplePropertiesEXT(..)
  , PhysicalDeviceSampleLocationsPropertiesEXT(..)
  , PipelineSampleLocationsStateCreateInfoEXT(..)
  , RenderPassSampleLocationsBeginInfoEXT(..)
  , SampleLocationEXT(..)
  , SampleLocationsInfoEXT(..)
  , SubpassSampleLocationsEXT(..)
  , fromCStructAttachmentSampleLocationsEXT
  , fromCStructMultisamplePropertiesEXT
  , fromCStructPhysicalDeviceSampleLocationsPropertiesEXT
  , fromCStructPipelineSampleLocationsStateCreateInfoEXT
  , fromCStructRenderPassSampleLocationsBeginInfoEXT
  , fromCStructSampleLocationEXT
  , fromCStructSampleLocationsInfoEXT
  , fromCStructSubpassSampleLocationsEXT
  , withCStructAttachmentSampleLocationsEXT
  , withCStructMultisamplePropertiesEXT
  , withCStructPhysicalDeviceSampleLocationsPropertiesEXT
  , withCStructPipelineSampleLocationsStateCreateInfoEXT
  , withCStructRenderPassSampleLocationsBeginInfoEXT
  , withCStructSampleLocationEXT
  , withCStructSampleLocationsInfoEXT
  , withCStructSubpassSampleLocationsEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( PhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , SamplerReductionModeCreateInfoEXT(..)
  , fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  , fromCStructSamplerReductionModeCreateInfoEXT
  , withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
  , withCStructSamplerReductionModeCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( ShaderModuleValidationCacheCreateInfoEXT(..)
  , ValidationCacheCreateInfoEXT(..)
  , fromCStructShaderModuleValidationCacheCreateInfoEXT
  , fromCStructValidationCacheCreateInfoEXT
  , withCStructShaderModuleValidationCacheCreateInfoEXT
  , withCStructValidationCacheCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationFlagsEXT(..)
  , fromCStructValidationFlagsEXT
  , withCStructValidationFlagsEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , PipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VertexInputBindingDivisorDescriptionEXT(..)
  , fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  , fromCStructPipelineVertexInputDivisorStateCreateInfoEXT
  , fromCStructVertexInputBindingDivisorDescriptionEXT
  , withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  , withCStructPipelineVertexInputDivisorStateCreateInfoEXT
  , withCStructVertexInputBindingDivisorDescriptionEXT
  )
import Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( PastPresentationTimingGOOGLE(..)
  , PresentTimeGOOGLE(..)
  , PresentTimesInfoGOOGLE(..)
  , RefreshCycleDurationGOOGLE(..)
  , fromCStructPastPresentationTimingGOOGLE
  , fromCStructPresentTimeGOOGLE
  , fromCStructPresentTimesInfoGOOGLE
  , fromCStructRefreshCycleDurationGOOGLE
  , withCStructPastPresentationTimingGOOGLE
  , withCStructPresentTimeGOOGLE
  , withCStructPresentTimesInfoGOOGLE
  , withCStructRefreshCycleDurationGOOGLE
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( AndroidSurfaceCreateInfoKHR(..)
  , fromCStructAndroidSurfaceCreateInfoKHR
  , withCStructAndroidSurfaceCreateInfoKHR
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayModeCreateInfoKHR(..)
  , DisplayModeParametersKHR(..)
  , DisplayModePropertiesKHR(..)
  , DisplayPlaneCapabilitiesKHR(..)
  , DisplayPlanePropertiesKHR(..)
  , DisplayPropertiesKHR(..)
  , DisplaySurfaceCreateInfoKHR(..)
  , fromCStructDisplayModeCreateInfoKHR
  , fromCStructDisplayModeParametersKHR
  , fromCStructDisplayModePropertiesKHR
  , fromCStructDisplayPlaneCapabilitiesKHR
  , fromCStructDisplayPlanePropertiesKHR
  , fromCStructDisplayPropertiesKHR
  , fromCStructDisplaySurfaceCreateInfoKHR
  , withCStructDisplayModeCreateInfoKHR
  , withCStructDisplayModeParametersKHR
  , withCStructDisplayModePropertiesKHR
  , withCStructDisplayPlaneCapabilitiesKHR
  , withCStructDisplayPlanePropertiesKHR
  , withCStructDisplayPropertiesKHR
  , withCStructDisplaySurfaceCreateInfoKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( DisplayPresentInfoKHR(..)
  , fromCStructDisplayPresentInfoKHR
  , withCStructDisplayPresentInfoKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( FenceGetFdInfoKHR(..)
  , ImportFenceFdInfoKHR(..)
  , fromCStructFenceGetFdInfoKHR
  , fromCStructImportFenceFdInfoKHR
  , withCStructFenceGetFdInfoKHR
  , withCStructImportFenceFdInfoKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( ExportFenceWin32HandleInfoKHR(..)
  , FenceGetWin32HandleInfoKHR(..)
  , ImportFenceWin32HandleInfoKHR(..)
  , fromCStructExportFenceWin32HandleInfoKHR
  , fromCStructFenceGetWin32HandleInfoKHR
  , fromCStructImportFenceWin32HandleInfoKHR
  , withCStructExportFenceWin32HandleInfoKHR
  , withCStructFenceGetWin32HandleInfoKHR
  , withCStructImportFenceWin32HandleInfoKHR
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( ImportMemoryFdInfoKHR(..)
  , MemoryFdPropertiesKHR(..)
  , MemoryGetFdInfoKHR(..)
  , fromCStructImportMemoryFdInfoKHR
  , fromCStructMemoryFdPropertiesKHR
  , fromCStructMemoryGetFdInfoKHR
  , withCStructImportMemoryFdInfoKHR
  , withCStructMemoryFdPropertiesKHR
  , withCStructMemoryGetFdInfoKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( ExportMemoryWin32HandleInfoKHR(..)
  , ImportMemoryWin32HandleInfoKHR(..)
  , MemoryGetWin32HandleInfoKHR(..)
  , MemoryWin32HandlePropertiesKHR(..)
  , fromCStructExportMemoryWin32HandleInfoKHR
  , fromCStructImportMemoryWin32HandleInfoKHR
  , fromCStructMemoryGetWin32HandleInfoKHR
  , fromCStructMemoryWin32HandlePropertiesKHR
  , withCStructExportMemoryWin32HandleInfoKHR
  , withCStructImportMemoryWin32HandleInfoKHR
  , withCStructMemoryGetWin32HandleInfoKHR
  , withCStructMemoryWin32HandlePropertiesKHR
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( ImportSemaphoreFdInfoKHR(..)
  , SemaphoreGetFdInfoKHR(..)
  , fromCStructImportSemaphoreFdInfoKHR
  , fromCStructSemaphoreGetFdInfoKHR
  , withCStructImportSemaphoreFdInfoKHR
  , withCStructSemaphoreGetFdInfoKHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( D3D12FenceSubmitInfoKHR(..)
  , ExportSemaphoreWin32HandleInfoKHR(..)
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , SemaphoreGetWin32HandleInfoKHR(..)
  , fromCStructD3D12FenceSubmitInfoKHR
  , fromCStructExportSemaphoreWin32HandleInfoKHR
  , fromCStructImportSemaphoreWin32HandleInfoKHR
  , fromCStructSemaphoreGetWin32HandleInfoKHR
  , withCStructD3D12FenceSubmitInfoKHR
  , withCStructExportSemaphoreWin32HandleInfoKHR
  , withCStructImportSemaphoreWin32HandleInfoKHR
  , withCStructSemaphoreGetWin32HandleInfoKHR
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  , SurfaceCapabilities2KHR(..)
  , SurfaceFormat2KHR(..)
  , fromCStructPhysicalDeviceSurfaceInfo2KHR
  , fromCStructSurfaceCapabilities2KHR
  , fromCStructSurfaceFormat2KHR
  , withCStructPhysicalDeviceSurfaceInfo2KHR
  , withCStructSurfaceCapabilities2KHR
  , withCStructSurfaceFormat2KHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( ImageFormatListCreateInfoKHR(..)
  , fromCStructImageFormatListCreateInfoKHR
  , withCStructImageFormatListCreateInfoKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  ( PresentRegionKHR(..)
  , PresentRegionsKHR(..)
  , RectLayerKHR(..)
  , fromCStructPresentRegionKHR
  , fromCStructPresentRegionsKHR
  , fromCStructRectLayerKHR
  , withCStructPresentRegionKHR
  , withCStructPresentRegionsKHR
  , withCStructRectLayerKHR
  )

#if defined(VK_USE_PLATFORM_MIR_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_mir_surface
  ( MirSurfaceCreateInfoKHR(..)
  , fromCStructMirSurfaceCreateInfoKHR
  , withCStructMirSurfaceCreateInfoKHR
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( PhysicalDevicePushDescriptorPropertiesKHR(..)
  , fromCStructPhysicalDevicePushDescriptorPropertiesKHR
  , withCStructPhysicalDevicePushDescriptorPropertiesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( SharedPresentSurfaceCapabilitiesKHR(..)
  , fromCStructSharedPresentSurfaceCapabilitiesKHR
  , withCStructSharedPresentSurfaceCapabilitiesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_surface
  ( SurfaceCapabilitiesKHR(..)
  , SurfaceFormatKHR(..)
  , fromCStructSurfaceCapabilitiesKHR
  , fromCStructSurfaceFormatKHR
  , withCStructSurfaceCapabilitiesKHR
  , withCStructSurfaceFormatKHR
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
  , fromCStructAcquireNextImageInfoKHR
  , fromCStructBindImageMemorySwapchainInfoKHR
  , fromCStructDeviceGroupPresentCapabilitiesKHR
  , fromCStructDeviceGroupPresentInfoKHR
  , fromCStructDeviceGroupSwapchainCreateInfoKHR
  , fromCStructImageSwapchainCreateInfoKHR
  , fromCStructPresentInfoKHR
  , fromCStructSwapchainCreateInfoKHR
  , withCStructAcquireNextImageInfoKHR
  , withCStructBindImageMemorySwapchainInfoKHR
  , withCStructDeviceGroupPresentCapabilitiesKHR
  , withCStructDeviceGroupPresentInfoKHR
  , withCStructDeviceGroupSwapchainCreateInfoKHR
  , withCStructImageSwapchainCreateInfoKHR
  , withCStructPresentInfoKHR
  , withCStructSwapchainCreateInfoKHR
  )

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( WaylandSurfaceCreateInfoKHR(..)
  , fromCStructWaylandSurfaceCreateInfoKHR
  , withCStructWaylandSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoKHR(..)
  , fromCStructWin32KeyedMutexAcquireReleaseInfoKHR
  , withCStructWin32KeyedMutexAcquireReleaseInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( Win32SurfaceCreateInfoKHR(..)
  , fromCStructWin32SurfaceCreateInfoKHR
  , withCStructWin32SurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( XcbSurfaceCreateInfoKHR(..)
  , fromCStructXcbSurfaceCreateInfoKHR
  , withCStructXcbSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( XlibSurfaceCreateInfoKHR(..)
  , fromCStructXlibSurfaceCreateInfoKHR
  , withCStructXlibSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( IOSSurfaceCreateInfoMVK(..)
  , fromCStructIOSSurfaceCreateInfoMVK
  , withCStructIOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateInfoMVK(..)
  , fromCStructMacOSSurfaceCreateInfoMVK
  , withCStructMacOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( ViSurfaceCreateInfoNN(..)
  , fromCStructViSurfaceCreateInfoNN
  , withCStructViSurfaceCreateInfoNN
  )
#endif
import Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( CmdProcessCommandsInfoNVX(..)
  , CmdReserveSpaceForCommandsInfoNVX(..)
  , DeviceGeneratedCommandsFeaturesNVX(..)
  , DeviceGeneratedCommandsLimitsNVX(..)
  , IndirectCommandsLayoutCreateInfoNVX(..)
  , IndirectCommandsLayoutTokenNVX(..)
  , IndirectCommandsTokenNVX(..)
  , ObjectTableCreateInfoNVX(..)
  , ObjectTableDescriptorSetEntryNVX(..)
  , ObjectTableEntryNVX(..)
  , ObjectTableIndexBufferEntryNVX(..)
  , ObjectTablePipelineEntryNVX(..)
  , ObjectTablePushConstantEntryNVX(..)
  , ObjectTableVertexBufferEntryNVX(..)
  , fromCStructCmdReserveSpaceForCommandsInfoNVX
  , fromCStructDeviceGeneratedCommandsFeaturesNVX
  , fromCStructDeviceGeneratedCommandsLimitsNVX
  , fromCStructIndirectCommandsLayoutCreateInfoNVX
  , fromCStructIndirectCommandsLayoutTokenNVX
  , fromCStructIndirectCommandsTokenNVX
  , fromCStructObjectTableCreateInfoNVX
  , fromCStructObjectTableDescriptorSetEntryNVX
  , fromCStructObjectTableEntryNVX
  , fromCStructObjectTableIndexBufferEntryNVX
  , fromCStructObjectTablePipelineEntryNVX
  , fromCStructObjectTablePushConstantEntryNVX
  , fromCStructObjectTableVertexBufferEntryNVX
  , withCStructCmdProcessCommandsInfoNVX
  , withCStructCmdReserveSpaceForCommandsInfoNVX
  , withCStructDeviceGeneratedCommandsFeaturesNVX
  , withCStructDeviceGeneratedCommandsLimitsNVX
  , withCStructIndirectCommandsLayoutCreateInfoNVX
  , withCStructIndirectCommandsLayoutTokenNVX
  , withCStructIndirectCommandsTokenNVX
  , withCStructObjectTableCreateInfoNVX
  , withCStructObjectTableDescriptorSetEntryNVX
  , withCStructObjectTableEntryNVX
  , withCStructObjectTableIndexBufferEntryNVX
  , withCStructObjectTablePipelineEntryNVX
  , withCStructObjectTablePushConstantEntryNVX
  , withCStructObjectTableVertexBufferEntryNVX
  )
import Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  , withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
  )
import Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( PipelineViewportWScalingStateCreateInfoNV(..)
  , ViewportWScalingNV(..)
  , fromCStructPipelineViewportWScalingStateCreateInfoNV
  , fromCStructViewportWScalingNV
  , withCStructPipelineViewportWScalingStateCreateInfoNV
  , withCStructViewportWScalingNV
  )
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( DedicatedAllocationBufferCreateInfoNV(..)
  , DedicatedAllocationImageCreateInfoNV(..)
  , DedicatedAllocationMemoryAllocateInfoNV(..)
  , fromCStructDedicatedAllocationBufferCreateInfoNV
  , fromCStructDedicatedAllocationImageCreateInfoNV
  , fromCStructDedicatedAllocationMemoryAllocateInfoNV
  , withCStructDedicatedAllocationBufferCreateInfoNV
  , withCStructDedicatedAllocationImageCreateInfoNV
  , withCStructDedicatedAllocationMemoryAllocateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory
  ( ExportMemoryAllocateInfoNV(..)
  , ExternalMemoryImageCreateInfoNV(..)
  , fromCStructExportMemoryAllocateInfoNV
  , fromCStructExternalMemoryImageCreateInfoNV
  , withCStructExportMemoryAllocateInfoNV
  , withCStructExternalMemoryImageCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities
  ( ExternalImageFormatPropertiesNV(..)
  , fromCStructExternalImageFormatPropertiesNV
  , withCStructExternalImageFormatPropertiesNV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( ExportMemoryWin32HandleInfoNV(..)
  , ImportMemoryWin32HandleInfoNV(..)
  , fromCStructExportMemoryWin32HandleInfoNV
  , fromCStructImportMemoryWin32HandleInfoNV
  , withCStructExportMemoryWin32HandleInfoNV
  , withCStructImportMemoryWin32HandleInfoNV
  )
#endif
import Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateInfoNV(..)
  , fromCStructPipelineCoverageToColorStateCreateInfoNV
  , withCStructPipelineCoverageToColorStateCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( PipelineCoverageModulationStateCreateInfoNV(..)
  , fromCStructPipelineCoverageModulationStateCreateInfoNV
  , withCStructPipelineCoverageModulationStateCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateInfoNV(..)
  , ViewportSwizzleNV(..)
  , fromCStructPipelineViewportSwizzleStateCreateInfoNV
  , fromCStructViewportSwizzleNV
  , withCStructPipelineViewportSwizzleStateCreateInfoNV
  , withCStructViewportSwizzleNV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoNV(..)
  , fromCStructWin32KeyedMutexAcquireReleaseInfoNV
  , withCStructWin32KeyedMutexAcquireReleaseInfoNV
  )
#endif


class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
  withCStruct :: marshalled -> (c -> IO a) -> IO a

class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
  fromCStruct :: c -> IO marshalled

class HasPNext a where
  getPNext :: a -> Maybe SomeVkStruct

data SomeVkStruct where
  SomeVkStruct
    :: (ToCStruct a b, Storable b, Show a, Eq a, Typeable a, HasPNext a)
    => a
    -> SomeVkStruct

instance HasPNext SomeVkStruct where
  getPNext (SomeVkStruct a) = getPNext a

deriving instance Show SomeVkStruct

instance Eq SomeVkStruct where
  SomeVkStruct (a :: a) == SomeVkStruct (b :: b) = case eqT @a @b of
    Nothing   -> False
    Just Refl -> a == b

withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
withCStructPtr a f = withCStruct a (\c -> alloca (\p -> poke p c *> f p))

fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
fromCStructPtr p = fromCStruct =<< peek p

fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
fromSomeVkStruct (SomeVkStruct a) = cast a

fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
fromSomeVkStructChain a =
  fromSomeVkStruct a <|> (getPNext a >>= fromSomeVkStructChain)

withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
withSomeVkStruct (SomeVkStruct a) f = withCStructPtr a (f . castPtr)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------
instance ToCStruct Offset2D VkOffset2D where
  withCStruct = withCStructOffset2D
instance FromCStruct Offset2D VkOffset2D where
  fromCStruct = fromCStructOffset2D

instance ToCStruct Offset3D VkOffset3D where
  withCStruct = withCStructOffset3D
instance FromCStruct Offset3D VkOffset3D where
  fromCStruct = fromCStructOffset3D

instance ToCStruct Extent2D VkExtent2D where
  withCStruct = withCStructExtent2D
instance FromCStruct Extent2D VkExtent2D where
  fromCStruct = fromCStructExtent2D

instance ToCStruct Extent3D VkExtent3D where
  withCStruct = withCStructExtent3D
instance FromCStruct Extent3D VkExtent3D where
  fromCStruct = fromCStructExtent3D

instance ToCStruct Viewport VkViewport where
  withCStruct = withCStructViewport
instance FromCStruct Viewport VkViewport where
  fromCStruct = fromCStructViewport

instance ToCStruct Rect2D VkRect2D where
  withCStruct = withCStructRect2D
instance FromCStruct Rect2D VkRect2D where
  fromCStruct = fromCStructRect2D

instance ToCStruct ClearRect VkClearRect where
  withCStruct = withCStructClearRect
instance FromCStruct ClearRect VkClearRect where
  fromCStruct = fromCStructClearRect

instance ToCStruct ComponentMapping VkComponentMapping where
  withCStruct = withCStructComponentMapping
instance FromCStruct ComponentMapping VkComponentMapping where
  fromCStruct = fromCStructComponentMapping

instance ToCStruct PhysicalDeviceProperties VkPhysicalDeviceProperties where
  withCStruct = withCStructPhysicalDeviceProperties
instance FromCStruct PhysicalDeviceProperties VkPhysicalDeviceProperties where
  fromCStruct = fromCStructPhysicalDeviceProperties

instance ToCStruct ExtensionProperties VkExtensionProperties where
  withCStruct = withCStructExtensionProperties
instance FromCStruct ExtensionProperties VkExtensionProperties where
  fromCStruct = fromCStructExtensionProperties

instance ToCStruct LayerProperties VkLayerProperties where
  withCStruct = withCStructLayerProperties
instance FromCStruct LayerProperties VkLayerProperties where
  fromCStruct = fromCStructLayerProperties

instance ToCStruct ApplicationInfo VkApplicationInfo where
  withCStruct = withCStructApplicationInfo
instance FromCStruct ApplicationInfo VkApplicationInfo where
  fromCStruct = fromCStructApplicationInfo
instance HasPNext ApplicationInfo where
  getPNext a = vkPNext (a :: ApplicationInfo)
instance ToCStruct AllocationCallbacks VkAllocationCallbacks where
  withCStruct = withCStructAllocationCallbacks
instance FromCStruct AllocationCallbacks VkAllocationCallbacks where
  fromCStruct = fromCStructAllocationCallbacks

instance ToCStruct DeviceQueueCreateInfo VkDeviceQueueCreateInfo where
  withCStruct = withCStructDeviceQueueCreateInfo
instance FromCStruct DeviceQueueCreateInfo VkDeviceQueueCreateInfo where
  fromCStruct = fromCStructDeviceQueueCreateInfo
instance HasPNext DeviceQueueCreateInfo where
  getPNext a = vkPNext (a :: DeviceQueueCreateInfo)
instance ToCStruct DeviceCreateInfo VkDeviceCreateInfo where
  withCStruct = withCStructDeviceCreateInfo
instance FromCStruct DeviceCreateInfo VkDeviceCreateInfo where
  fromCStruct = fromCStructDeviceCreateInfo
instance HasPNext DeviceCreateInfo where
  getPNext a = vkPNext (a :: DeviceCreateInfo)
instance ToCStruct InstanceCreateInfo VkInstanceCreateInfo where
  withCStruct = withCStructInstanceCreateInfo
instance FromCStruct InstanceCreateInfo VkInstanceCreateInfo where
  fromCStruct = fromCStructInstanceCreateInfo
instance HasPNext InstanceCreateInfo where
  getPNext a = vkPNext (a :: InstanceCreateInfo)
instance ToCStruct QueueFamilyProperties VkQueueFamilyProperties where
  withCStruct = withCStructQueueFamilyProperties
instance FromCStruct QueueFamilyProperties VkQueueFamilyProperties where
  fromCStruct = fromCStructQueueFamilyProperties

instance ToCStruct PhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties where
  withCStruct = withCStructPhysicalDeviceMemoryProperties
instance FromCStruct PhysicalDeviceMemoryProperties VkPhysicalDeviceMemoryProperties where
  fromCStruct = fromCStructPhysicalDeviceMemoryProperties

instance ToCStruct MemoryAllocateInfo VkMemoryAllocateInfo where
  withCStruct = withCStructMemoryAllocateInfo
instance FromCStruct MemoryAllocateInfo VkMemoryAllocateInfo where
  fromCStruct = fromCStructMemoryAllocateInfo
instance HasPNext MemoryAllocateInfo where
  getPNext a = vkPNext (a :: MemoryAllocateInfo)
instance ToCStruct MemoryRequirements VkMemoryRequirements where
  withCStruct = withCStructMemoryRequirements
instance FromCStruct MemoryRequirements VkMemoryRequirements where
  fromCStruct = fromCStructMemoryRequirements

instance ToCStruct SparseImageFormatProperties VkSparseImageFormatProperties where
  withCStruct = withCStructSparseImageFormatProperties
instance FromCStruct SparseImageFormatProperties VkSparseImageFormatProperties where
  fromCStruct = fromCStructSparseImageFormatProperties

instance ToCStruct SparseImageMemoryRequirements VkSparseImageMemoryRequirements where
  withCStruct = withCStructSparseImageMemoryRequirements
instance FromCStruct SparseImageMemoryRequirements VkSparseImageMemoryRequirements where
  fromCStruct = fromCStructSparseImageMemoryRequirements

instance ToCStruct MemoryType VkMemoryType where
  withCStruct = withCStructMemoryType
instance FromCStruct MemoryType VkMemoryType where
  fromCStruct = fromCStructMemoryType

instance ToCStruct MemoryHeap VkMemoryHeap where
  withCStruct = withCStructMemoryHeap
instance FromCStruct MemoryHeap VkMemoryHeap where
  fromCStruct = fromCStructMemoryHeap

instance ToCStruct MappedMemoryRange VkMappedMemoryRange where
  withCStruct = withCStructMappedMemoryRange
instance FromCStruct MappedMemoryRange VkMappedMemoryRange where
  fromCStruct = fromCStructMappedMemoryRange
instance HasPNext MappedMemoryRange where
  getPNext a = vkPNext (a :: MappedMemoryRange)
instance ToCStruct FormatProperties VkFormatProperties where
  withCStruct = withCStructFormatProperties
instance FromCStruct FormatProperties VkFormatProperties where
  fromCStruct = fromCStructFormatProperties

instance ToCStruct ImageFormatProperties VkImageFormatProperties where
  withCStruct = withCStructImageFormatProperties
instance FromCStruct ImageFormatProperties VkImageFormatProperties where
  fromCStruct = fromCStructImageFormatProperties

instance ToCStruct DescriptorBufferInfo VkDescriptorBufferInfo where
  withCStruct = withCStructDescriptorBufferInfo
instance FromCStruct DescriptorBufferInfo VkDescriptorBufferInfo where
  fromCStruct = fromCStructDescriptorBufferInfo

instance ToCStruct DescriptorImageInfo VkDescriptorImageInfo where
  withCStruct = withCStructDescriptorImageInfo
instance FromCStruct DescriptorImageInfo VkDescriptorImageInfo where
  fromCStruct = fromCStructDescriptorImageInfo

instance ToCStruct WriteDescriptorSet VkWriteDescriptorSet where
  withCStruct = withCStructWriteDescriptorSet
instance FromCStruct WriteDescriptorSet VkWriteDescriptorSet where
  fromCStruct = fromCStructWriteDescriptorSet
instance HasPNext WriteDescriptorSet where
  getPNext a = vkPNext (a :: WriteDescriptorSet)
instance ToCStruct CopyDescriptorSet VkCopyDescriptorSet where
  withCStruct = withCStructCopyDescriptorSet
instance FromCStruct CopyDescriptorSet VkCopyDescriptorSet where
  fromCStruct = fromCStructCopyDescriptorSet
instance HasPNext CopyDescriptorSet where
  getPNext a = vkPNext (a :: CopyDescriptorSet)
instance ToCStruct BufferCreateInfo VkBufferCreateInfo where
  withCStruct = withCStructBufferCreateInfo
instance FromCStruct BufferCreateInfo VkBufferCreateInfo where
  fromCStruct = fromCStructBufferCreateInfo
instance HasPNext BufferCreateInfo where
  getPNext a = vkPNext (a :: BufferCreateInfo)
instance ToCStruct BufferViewCreateInfo VkBufferViewCreateInfo where
  withCStruct = withCStructBufferViewCreateInfo
instance FromCStruct BufferViewCreateInfo VkBufferViewCreateInfo where
  fromCStruct = fromCStructBufferViewCreateInfo
instance HasPNext BufferViewCreateInfo where
  getPNext a = vkPNext (a :: BufferViewCreateInfo)
instance ToCStruct ImageSubresource VkImageSubresource where
  withCStruct = withCStructImageSubresource
instance FromCStruct ImageSubresource VkImageSubresource where
  fromCStruct = fromCStructImageSubresource

instance ToCStruct ImageSubresourceLayers VkImageSubresourceLayers where
  withCStruct = withCStructImageSubresourceLayers
instance FromCStruct ImageSubresourceLayers VkImageSubresourceLayers where
  fromCStruct = fromCStructImageSubresourceLayers

instance ToCStruct ImageSubresourceRange VkImageSubresourceRange where
  withCStruct = withCStructImageSubresourceRange
instance FromCStruct ImageSubresourceRange VkImageSubresourceRange where
  fromCStruct = fromCStructImageSubresourceRange

instance ToCStruct MemoryBarrier VkMemoryBarrier where
  withCStruct = withCStructMemoryBarrier
instance FromCStruct MemoryBarrier VkMemoryBarrier where
  fromCStruct = fromCStructMemoryBarrier
instance HasPNext MemoryBarrier where
  getPNext a = vkPNext (a :: MemoryBarrier)
instance ToCStruct BufferMemoryBarrier VkBufferMemoryBarrier where
  withCStruct = withCStructBufferMemoryBarrier
instance FromCStruct BufferMemoryBarrier VkBufferMemoryBarrier where
  fromCStruct = fromCStructBufferMemoryBarrier
instance HasPNext BufferMemoryBarrier where
  getPNext a = vkPNext (a :: BufferMemoryBarrier)
instance ToCStruct ImageMemoryBarrier VkImageMemoryBarrier where
  withCStruct = withCStructImageMemoryBarrier
instance FromCStruct ImageMemoryBarrier VkImageMemoryBarrier where
  fromCStruct = fromCStructImageMemoryBarrier
instance HasPNext ImageMemoryBarrier where
  getPNext a = vkPNext (a :: ImageMemoryBarrier)
instance ToCStruct ImageCreateInfo VkImageCreateInfo where
  withCStruct = withCStructImageCreateInfo
instance FromCStruct ImageCreateInfo VkImageCreateInfo where
  fromCStruct = fromCStructImageCreateInfo
instance HasPNext ImageCreateInfo where
  getPNext a = vkPNext (a :: ImageCreateInfo)
instance ToCStruct SubresourceLayout VkSubresourceLayout where
  withCStruct = withCStructSubresourceLayout
instance FromCStruct SubresourceLayout VkSubresourceLayout where
  fromCStruct = fromCStructSubresourceLayout

instance ToCStruct ImageViewCreateInfo VkImageViewCreateInfo where
  withCStruct = withCStructImageViewCreateInfo
instance FromCStruct ImageViewCreateInfo VkImageViewCreateInfo where
  fromCStruct = fromCStructImageViewCreateInfo
instance HasPNext ImageViewCreateInfo where
  getPNext a = vkPNext (a :: ImageViewCreateInfo)
instance ToCStruct BufferCopy VkBufferCopy where
  withCStruct = withCStructBufferCopy
instance FromCStruct BufferCopy VkBufferCopy where
  fromCStruct = fromCStructBufferCopy

instance ToCStruct SparseMemoryBind VkSparseMemoryBind where
  withCStruct = withCStructSparseMemoryBind
instance FromCStruct SparseMemoryBind VkSparseMemoryBind where
  fromCStruct = fromCStructSparseMemoryBind

instance ToCStruct SparseImageMemoryBind VkSparseImageMemoryBind where
  withCStruct = withCStructSparseImageMemoryBind
instance FromCStruct SparseImageMemoryBind VkSparseImageMemoryBind where
  fromCStruct = fromCStructSparseImageMemoryBind

instance ToCStruct SparseBufferMemoryBindInfo VkSparseBufferMemoryBindInfo where
  withCStruct = withCStructSparseBufferMemoryBindInfo
instance FromCStruct SparseBufferMemoryBindInfo VkSparseBufferMemoryBindInfo where
  fromCStruct = fromCStructSparseBufferMemoryBindInfo

instance ToCStruct SparseImageOpaqueMemoryBindInfo VkSparseImageOpaqueMemoryBindInfo where
  withCStruct = withCStructSparseImageOpaqueMemoryBindInfo
instance FromCStruct SparseImageOpaqueMemoryBindInfo VkSparseImageOpaqueMemoryBindInfo where
  fromCStruct = fromCStructSparseImageOpaqueMemoryBindInfo

instance ToCStruct SparseImageMemoryBindInfo VkSparseImageMemoryBindInfo where
  withCStruct = withCStructSparseImageMemoryBindInfo
instance FromCStruct SparseImageMemoryBindInfo VkSparseImageMemoryBindInfo where
  fromCStruct = fromCStructSparseImageMemoryBindInfo

instance ToCStruct BindSparseInfo VkBindSparseInfo where
  withCStruct = withCStructBindSparseInfo
instance FromCStruct BindSparseInfo VkBindSparseInfo where
  fromCStruct = fromCStructBindSparseInfo
instance HasPNext BindSparseInfo where
  getPNext a = vkPNext (a :: BindSparseInfo)
instance ToCStruct ImageCopy VkImageCopy where
  withCStruct = withCStructImageCopy
instance FromCStruct ImageCopy VkImageCopy where
  fromCStruct = fromCStructImageCopy

instance ToCStruct ImageBlit VkImageBlit where
  withCStruct = withCStructImageBlit
instance FromCStruct ImageBlit VkImageBlit where
  fromCStruct = fromCStructImageBlit

instance ToCStruct BufferImageCopy VkBufferImageCopy where
  withCStruct = withCStructBufferImageCopy
instance FromCStruct BufferImageCopy VkBufferImageCopy where
  fromCStruct = fromCStructBufferImageCopy

instance ToCStruct ImageResolve VkImageResolve where
  withCStruct = withCStructImageResolve
instance FromCStruct ImageResolve VkImageResolve where
  fromCStruct = fromCStructImageResolve

instance ToCStruct ShaderModuleCreateInfo VkShaderModuleCreateInfo where
  withCStruct = withCStructShaderModuleCreateInfo
instance FromCStruct ShaderModuleCreateInfo VkShaderModuleCreateInfo where
  fromCStruct = fromCStructShaderModuleCreateInfo
instance HasPNext ShaderModuleCreateInfo where
  getPNext a = vkPNext (a :: ShaderModuleCreateInfo)
instance ToCStruct DescriptorSetLayoutBinding VkDescriptorSetLayoutBinding where
  withCStruct = withCStructDescriptorSetLayoutBinding
instance FromCStruct DescriptorSetLayoutBinding VkDescriptorSetLayoutBinding where
  fromCStruct = fromCStructDescriptorSetLayoutBinding

instance ToCStruct DescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo where
  withCStruct = withCStructDescriptorSetLayoutCreateInfo
instance FromCStruct DescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo where
  fromCStruct = fromCStructDescriptorSetLayoutCreateInfo
instance HasPNext DescriptorSetLayoutCreateInfo where
  getPNext a = vkPNext (a :: DescriptorSetLayoutCreateInfo)
instance ToCStruct DescriptorPoolSize VkDescriptorPoolSize where
  withCStruct = withCStructDescriptorPoolSize
instance FromCStruct DescriptorPoolSize VkDescriptorPoolSize where
  fromCStruct = fromCStructDescriptorPoolSize

instance ToCStruct DescriptorPoolCreateInfo VkDescriptorPoolCreateInfo where
  withCStruct = withCStructDescriptorPoolCreateInfo
instance FromCStruct DescriptorPoolCreateInfo VkDescriptorPoolCreateInfo where
  fromCStruct = fromCStructDescriptorPoolCreateInfo
instance HasPNext DescriptorPoolCreateInfo where
  getPNext a = vkPNext (a :: DescriptorPoolCreateInfo)
instance ToCStruct DescriptorSetAllocateInfo VkDescriptorSetAllocateInfo where
  withCStruct = withCStructDescriptorSetAllocateInfo
instance FromCStruct DescriptorSetAllocateInfo VkDescriptorSetAllocateInfo where
  fromCStruct = fromCStructDescriptorSetAllocateInfo
instance HasPNext DescriptorSetAllocateInfo where
  getPNext a = vkPNext (a :: DescriptorSetAllocateInfo)
instance ToCStruct SpecializationMapEntry VkSpecializationMapEntry where
  withCStruct = withCStructSpecializationMapEntry
instance FromCStruct SpecializationMapEntry VkSpecializationMapEntry where
  fromCStruct = fromCStructSpecializationMapEntry

instance ToCStruct SpecializationInfo VkSpecializationInfo where
  withCStruct = withCStructSpecializationInfo
instance FromCStruct SpecializationInfo VkSpecializationInfo where
  fromCStruct = fromCStructSpecializationInfo

instance ToCStruct PipelineShaderStageCreateInfo VkPipelineShaderStageCreateInfo where
  withCStruct = withCStructPipelineShaderStageCreateInfo
instance FromCStruct PipelineShaderStageCreateInfo VkPipelineShaderStageCreateInfo where
  fromCStruct = fromCStructPipelineShaderStageCreateInfo
instance HasPNext PipelineShaderStageCreateInfo where
  getPNext a = vkPNext (a :: PipelineShaderStageCreateInfo)
instance ToCStruct ComputePipelineCreateInfo VkComputePipelineCreateInfo where
  withCStruct = withCStructComputePipelineCreateInfo
instance FromCStruct ComputePipelineCreateInfo VkComputePipelineCreateInfo where
  fromCStruct = fromCStructComputePipelineCreateInfo
instance HasPNext ComputePipelineCreateInfo where
  getPNext a = vkPNext (a :: ComputePipelineCreateInfo)
instance ToCStruct VertexInputBindingDescription VkVertexInputBindingDescription where
  withCStruct = withCStructVertexInputBindingDescription
instance FromCStruct VertexInputBindingDescription VkVertexInputBindingDescription where
  fromCStruct = fromCStructVertexInputBindingDescription

instance ToCStruct VertexInputAttributeDescription VkVertexInputAttributeDescription where
  withCStruct = withCStructVertexInputAttributeDescription
instance FromCStruct VertexInputAttributeDescription VkVertexInputAttributeDescription where
  fromCStruct = fromCStructVertexInputAttributeDescription

instance ToCStruct PipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo where
  withCStruct = withCStructPipelineVertexInputStateCreateInfo
instance FromCStruct PipelineVertexInputStateCreateInfo VkPipelineVertexInputStateCreateInfo where
  fromCStruct = fromCStructPipelineVertexInputStateCreateInfo
instance HasPNext PipelineVertexInputStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineVertexInputStateCreateInfo)
instance ToCStruct PipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo where
  withCStruct = withCStructPipelineInputAssemblyStateCreateInfo
instance FromCStruct PipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo where
  fromCStruct = fromCStructPipelineInputAssemblyStateCreateInfo
instance HasPNext PipelineInputAssemblyStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineInputAssemblyStateCreateInfo)
instance ToCStruct PipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo where
  withCStruct = withCStructPipelineTessellationStateCreateInfo
instance FromCStruct PipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo where
  fromCStruct = fromCStructPipelineTessellationStateCreateInfo
instance HasPNext PipelineTessellationStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineTessellationStateCreateInfo)
instance ToCStruct PipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo where
  withCStruct = withCStructPipelineViewportStateCreateInfo
instance FromCStruct PipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo where
  fromCStruct = fromCStructPipelineViewportStateCreateInfo
instance HasPNext PipelineViewportStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineViewportStateCreateInfo)
instance ToCStruct PipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo where
  withCStruct = withCStructPipelineRasterizationStateCreateInfo
instance FromCStruct PipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo where
  fromCStruct = fromCStructPipelineRasterizationStateCreateInfo
instance HasPNext PipelineRasterizationStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineRasterizationStateCreateInfo)
instance ToCStruct PipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo where
  withCStruct = withCStructPipelineMultisampleStateCreateInfo
instance FromCStruct PipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo where
  fromCStruct = fromCStructPipelineMultisampleStateCreateInfo
instance HasPNext PipelineMultisampleStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineMultisampleStateCreateInfo)
instance ToCStruct PipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState where
  withCStruct = withCStructPipelineColorBlendAttachmentState
instance FromCStruct PipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState where
  fromCStruct = fromCStructPipelineColorBlendAttachmentState

instance ToCStruct PipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo where
  withCStruct = withCStructPipelineColorBlendStateCreateInfo
instance FromCStruct PipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo where
  fromCStruct = fromCStructPipelineColorBlendStateCreateInfo
instance HasPNext PipelineColorBlendStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineColorBlendStateCreateInfo)
instance ToCStruct PipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo where
  withCStruct = withCStructPipelineDynamicStateCreateInfo
instance FromCStruct PipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo where
  fromCStruct = fromCStructPipelineDynamicStateCreateInfo
instance HasPNext PipelineDynamicStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineDynamicStateCreateInfo)
instance ToCStruct StencilOpState VkStencilOpState where
  withCStruct = withCStructStencilOpState
instance FromCStruct StencilOpState VkStencilOpState where
  fromCStruct = fromCStructStencilOpState

instance ToCStruct PipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo where
  withCStruct = withCStructPipelineDepthStencilStateCreateInfo
instance FromCStruct PipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo where
  fromCStruct = fromCStructPipelineDepthStencilStateCreateInfo
instance HasPNext PipelineDepthStencilStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineDepthStencilStateCreateInfo)
instance ToCStruct GraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo where
  withCStruct = withCStructGraphicsPipelineCreateInfo
instance FromCStruct GraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo where
  fromCStruct = fromCStructGraphicsPipelineCreateInfo
instance HasPNext GraphicsPipelineCreateInfo where
  getPNext a = vkPNext (a :: GraphicsPipelineCreateInfo)
instance ToCStruct PipelineCacheCreateInfo VkPipelineCacheCreateInfo where
  withCStruct = withCStructPipelineCacheCreateInfo
instance FromCStruct PipelineCacheCreateInfo VkPipelineCacheCreateInfo where
  fromCStruct = fromCStructPipelineCacheCreateInfo
instance HasPNext PipelineCacheCreateInfo where
  getPNext a = vkPNext (a :: PipelineCacheCreateInfo)
instance ToCStruct PushConstantRange VkPushConstantRange where
  withCStruct = withCStructPushConstantRange
instance FromCStruct PushConstantRange VkPushConstantRange where
  fromCStruct = fromCStructPushConstantRange

instance ToCStruct PipelineLayoutCreateInfo VkPipelineLayoutCreateInfo where
  withCStruct = withCStructPipelineLayoutCreateInfo
instance FromCStruct PipelineLayoutCreateInfo VkPipelineLayoutCreateInfo where
  fromCStruct = fromCStructPipelineLayoutCreateInfo
instance HasPNext PipelineLayoutCreateInfo where
  getPNext a = vkPNext (a :: PipelineLayoutCreateInfo)
instance ToCStruct SamplerCreateInfo VkSamplerCreateInfo where
  withCStruct = withCStructSamplerCreateInfo
instance FromCStruct SamplerCreateInfo VkSamplerCreateInfo where
  fromCStruct = fromCStructSamplerCreateInfo
instance HasPNext SamplerCreateInfo where
  getPNext a = vkPNext (a :: SamplerCreateInfo)
instance ToCStruct CommandPoolCreateInfo VkCommandPoolCreateInfo where
  withCStruct = withCStructCommandPoolCreateInfo
instance FromCStruct CommandPoolCreateInfo VkCommandPoolCreateInfo where
  fromCStruct = fromCStructCommandPoolCreateInfo
instance HasPNext CommandPoolCreateInfo where
  getPNext a = vkPNext (a :: CommandPoolCreateInfo)
instance ToCStruct CommandBufferAllocateInfo VkCommandBufferAllocateInfo where
  withCStruct = withCStructCommandBufferAllocateInfo
instance FromCStruct CommandBufferAllocateInfo VkCommandBufferAllocateInfo where
  fromCStruct = fromCStructCommandBufferAllocateInfo
instance HasPNext CommandBufferAllocateInfo where
  getPNext a = vkPNext (a :: CommandBufferAllocateInfo)
instance ToCStruct CommandBufferInheritanceInfo VkCommandBufferInheritanceInfo where
  withCStruct = withCStructCommandBufferInheritanceInfo
instance FromCStruct CommandBufferInheritanceInfo VkCommandBufferInheritanceInfo where
  fromCStruct = fromCStructCommandBufferInheritanceInfo
instance HasPNext CommandBufferInheritanceInfo where
  getPNext a = vkPNext (a :: CommandBufferInheritanceInfo)
instance ToCStruct CommandBufferBeginInfo VkCommandBufferBeginInfo where
  withCStruct = withCStructCommandBufferBeginInfo
instance FromCStruct CommandBufferBeginInfo VkCommandBufferBeginInfo where
  fromCStruct = fromCStructCommandBufferBeginInfo
instance HasPNext CommandBufferBeginInfo where
  getPNext a = vkPNext (a :: CommandBufferBeginInfo)
instance ToCStruct RenderPassBeginInfo VkRenderPassBeginInfo where
  withCStruct = withCStructRenderPassBeginInfo
-- No FromCStruct instance for VkRenderPassBeginInfo as it contains a union type
instance HasPNext RenderPassBeginInfo where
  getPNext a = vkPNext (a :: RenderPassBeginInfo)
instance ToCStruct ClearDepthStencilValue VkClearDepthStencilValue where
  withCStruct = withCStructClearDepthStencilValue
instance FromCStruct ClearDepthStencilValue VkClearDepthStencilValue where
  fromCStruct = fromCStructClearDepthStencilValue

instance ToCStruct ClearAttachment VkClearAttachment where
  withCStruct = withCStructClearAttachment
-- No FromCStruct instance for VkClearAttachment as it contains a union type

instance ToCStruct AttachmentDescription VkAttachmentDescription where
  withCStruct = withCStructAttachmentDescription
instance FromCStruct AttachmentDescription VkAttachmentDescription where
  fromCStruct = fromCStructAttachmentDescription

instance ToCStruct AttachmentReference VkAttachmentReference where
  withCStruct = withCStructAttachmentReference
instance FromCStruct AttachmentReference VkAttachmentReference where
  fromCStruct = fromCStructAttachmentReference

instance ToCStruct SubpassDescription VkSubpassDescription where
  withCStruct = withCStructSubpassDescription
instance FromCStruct SubpassDescription VkSubpassDescription where
  fromCStruct = fromCStructSubpassDescription

instance ToCStruct SubpassDependency VkSubpassDependency where
  withCStruct = withCStructSubpassDependency
instance FromCStruct SubpassDependency VkSubpassDependency where
  fromCStruct = fromCStructSubpassDependency

instance ToCStruct RenderPassCreateInfo VkRenderPassCreateInfo where
  withCStruct = withCStructRenderPassCreateInfo
instance FromCStruct RenderPassCreateInfo VkRenderPassCreateInfo where
  fromCStruct = fromCStructRenderPassCreateInfo
instance HasPNext RenderPassCreateInfo where
  getPNext a = vkPNext (a :: RenderPassCreateInfo)
instance ToCStruct EventCreateInfo VkEventCreateInfo where
  withCStruct = withCStructEventCreateInfo
instance FromCStruct EventCreateInfo VkEventCreateInfo where
  fromCStruct = fromCStructEventCreateInfo
instance HasPNext EventCreateInfo where
  getPNext a = vkPNext (a :: EventCreateInfo)
instance ToCStruct FenceCreateInfo VkFenceCreateInfo where
  withCStruct = withCStructFenceCreateInfo
instance FromCStruct FenceCreateInfo VkFenceCreateInfo where
  fromCStruct = fromCStructFenceCreateInfo
instance HasPNext FenceCreateInfo where
  getPNext a = vkPNext (a :: FenceCreateInfo)
instance ToCStruct PhysicalDeviceFeatures VkPhysicalDeviceFeatures where
  withCStruct = withCStructPhysicalDeviceFeatures
instance FromCStruct PhysicalDeviceFeatures VkPhysicalDeviceFeatures where
  fromCStruct = fromCStructPhysicalDeviceFeatures

instance ToCStruct PhysicalDeviceSparseProperties VkPhysicalDeviceSparseProperties where
  withCStruct = withCStructPhysicalDeviceSparseProperties
instance FromCStruct PhysicalDeviceSparseProperties VkPhysicalDeviceSparseProperties where
  fromCStruct = fromCStructPhysicalDeviceSparseProperties

instance ToCStruct PhysicalDeviceLimits VkPhysicalDeviceLimits where
  withCStruct = withCStructPhysicalDeviceLimits
instance FromCStruct PhysicalDeviceLimits VkPhysicalDeviceLimits where
  fromCStruct = fromCStructPhysicalDeviceLimits

instance ToCStruct SemaphoreCreateInfo VkSemaphoreCreateInfo where
  withCStruct = withCStructSemaphoreCreateInfo
instance FromCStruct SemaphoreCreateInfo VkSemaphoreCreateInfo where
  fromCStruct = fromCStructSemaphoreCreateInfo
instance HasPNext SemaphoreCreateInfo where
  getPNext a = vkPNext (a :: SemaphoreCreateInfo)
instance ToCStruct QueryPoolCreateInfo VkQueryPoolCreateInfo where
  withCStruct = withCStructQueryPoolCreateInfo
instance FromCStruct QueryPoolCreateInfo VkQueryPoolCreateInfo where
  fromCStruct = fromCStructQueryPoolCreateInfo
instance HasPNext QueryPoolCreateInfo where
  getPNext a = vkPNext (a :: QueryPoolCreateInfo)
instance ToCStruct FramebufferCreateInfo VkFramebufferCreateInfo where
  withCStruct = withCStructFramebufferCreateInfo
instance FromCStruct FramebufferCreateInfo VkFramebufferCreateInfo where
  fromCStruct = fromCStructFramebufferCreateInfo
instance HasPNext FramebufferCreateInfo where
  getPNext a = vkPNext (a :: FramebufferCreateInfo)
instance ToCStruct DrawIndirectCommand VkDrawIndirectCommand where
  withCStruct = withCStructDrawIndirectCommand
instance FromCStruct DrawIndirectCommand VkDrawIndirectCommand where
  fromCStruct = fromCStructDrawIndirectCommand

instance ToCStruct DrawIndexedIndirectCommand VkDrawIndexedIndirectCommand where
  withCStruct = withCStructDrawIndexedIndirectCommand
instance FromCStruct DrawIndexedIndirectCommand VkDrawIndexedIndirectCommand where
  fromCStruct = fromCStructDrawIndexedIndirectCommand

instance ToCStruct DispatchIndirectCommand VkDispatchIndirectCommand where
  withCStruct = withCStructDispatchIndirectCommand
instance FromCStruct DispatchIndirectCommand VkDispatchIndirectCommand where
  fromCStruct = fromCStructDispatchIndirectCommand

instance ToCStruct SubmitInfo VkSubmitInfo where
  withCStruct = withCStructSubmitInfo
-- No FromCStruct instance for VkSubmitInfo as it contains a dispatchable handle
instance HasPNext SubmitInfo where
  getPNext a = vkPNext (a :: SubmitInfo)
instance ToCStruct DisplayPropertiesKHR VkDisplayPropertiesKHR where
  withCStruct = withCStructDisplayPropertiesKHR
instance FromCStruct DisplayPropertiesKHR VkDisplayPropertiesKHR where
  fromCStruct = fromCStructDisplayPropertiesKHR

instance ToCStruct DisplayPlanePropertiesKHR VkDisplayPlanePropertiesKHR where
  withCStruct = withCStructDisplayPlanePropertiesKHR
instance FromCStruct DisplayPlanePropertiesKHR VkDisplayPlanePropertiesKHR where
  fromCStruct = fromCStructDisplayPlanePropertiesKHR

instance ToCStruct DisplayModeParametersKHR VkDisplayModeParametersKHR where
  withCStruct = withCStructDisplayModeParametersKHR
instance FromCStruct DisplayModeParametersKHR VkDisplayModeParametersKHR where
  fromCStruct = fromCStructDisplayModeParametersKHR

instance ToCStruct DisplayModePropertiesKHR VkDisplayModePropertiesKHR where
  withCStruct = withCStructDisplayModePropertiesKHR
instance FromCStruct DisplayModePropertiesKHR VkDisplayModePropertiesKHR where
  fromCStruct = fromCStructDisplayModePropertiesKHR

instance ToCStruct DisplayModeCreateInfoKHR VkDisplayModeCreateInfoKHR where
  withCStruct = withCStructDisplayModeCreateInfoKHR
instance FromCStruct DisplayModeCreateInfoKHR VkDisplayModeCreateInfoKHR where
  fromCStruct = fromCStructDisplayModeCreateInfoKHR
instance HasPNext DisplayModeCreateInfoKHR where
  getPNext a = vkPNext (a :: DisplayModeCreateInfoKHR)
instance ToCStruct DisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR where
  withCStruct = withCStructDisplayPlaneCapabilitiesKHR
instance FromCStruct DisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR where
  fromCStruct = fromCStructDisplayPlaneCapabilitiesKHR

instance ToCStruct DisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR where
  withCStruct = withCStructDisplaySurfaceCreateInfoKHR
instance FromCStruct DisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR where
  fromCStruct = fromCStructDisplaySurfaceCreateInfoKHR
instance HasPNext DisplaySurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: DisplaySurfaceCreateInfoKHR)
instance ToCStruct DisplayPresentInfoKHR VkDisplayPresentInfoKHR where
  withCStruct = withCStructDisplayPresentInfoKHR
instance FromCStruct DisplayPresentInfoKHR VkDisplayPresentInfoKHR where
  fromCStruct = fromCStructDisplayPresentInfoKHR
instance HasPNext DisplayPresentInfoKHR where
  getPNext a = vkPNext (a :: DisplayPresentInfoKHR)
instance ToCStruct SurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR where
  withCStruct = withCStructSurfaceCapabilitiesKHR
instance FromCStruct SurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR where
  fromCStruct = fromCStructSurfaceCapabilitiesKHR


#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR where
  withCStruct = withCStructAndroidSurfaceCreateInfoKHR
instance FromCStruct AndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR where
  fromCStruct = fromCStructAndroidSurfaceCreateInfoKHR
instance HasPNext AndroidSurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: AndroidSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_MIR_KHR
instance ToCStruct MirSurfaceCreateInfoKHR VkMirSurfaceCreateInfoKHR where
  withCStruct = withCStructMirSurfaceCreateInfoKHR
instance FromCStruct MirSurfaceCreateInfoKHR VkMirSurfaceCreateInfoKHR where
  fromCStruct = fromCStructMirSurfaceCreateInfoKHR
instance HasPNext MirSurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: MirSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_VI_NN
instance ToCStruct ViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN where
  withCStruct = withCStructViSurfaceCreateInfoNN
instance FromCStruct ViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN where
  fromCStruct = fromCStructViSurfaceCreateInfoNN
instance HasPNext ViSurfaceCreateInfoNN where
  getPNext a = vkPNext (a :: ViSurfaceCreateInfoNN)
#endif

#if VK_USE_PLATFORM_WAYLAND_KHR
instance ToCStruct WaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR where
  withCStruct = withCStructWaylandSurfaceCreateInfoKHR
instance FromCStruct WaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR where
  fromCStruct = fromCStructWaylandSurfaceCreateInfoKHR
instance HasPNext WaylandSurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: WaylandSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct Win32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR where
  withCStruct = withCStructWin32SurfaceCreateInfoKHR
instance FromCStruct Win32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR where
  fromCStruct = fromCStructWin32SurfaceCreateInfoKHR
instance HasPNext Win32SurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: Win32SurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_XLIB_KHR
instance ToCStruct XlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR where
  withCStruct = withCStructXlibSurfaceCreateInfoKHR
instance FromCStruct XlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR where
  fromCStruct = fromCStructXlibSurfaceCreateInfoKHR
instance HasPNext XlibSurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: XlibSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_XCB_KHR
instance ToCStruct XcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR where
  withCStruct = withCStructXcbSurfaceCreateInfoKHR
instance FromCStruct XcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR where
  fromCStruct = fromCStructXcbSurfaceCreateInfoKHR
instance HasPNext XcbSurfaceCreateInfoKHR where
  getPNext a = vkPNext (a :: XcbSurfaceCreateInfoKHR)
#endif
instance ToCStruct SurfaceFormatKHR VkSurfaceFormatKHR where
  withCStruct = withCStructSurfaceFormatKHR
instance FromCStruct SurfaceFormatKHR VkSurfaceFormatKHR where
  fromCStruct = fromCStructSurfaceFormatKHR

instance ToCStruct SwapchainCreateInfoKHR VkSwapchainCreateInfoKHR where
  withCStruct = withCStructSwapchainCreateInfoKHR
instance FromCStruct SwapchainCreateInfoKHR VkSwapchainCreateInfoKHR where
  fromCStruct = fromCStructSwapchainCreateInfoKHR
instance HasPNext SwapchainCreateInfoKHR where
  getPNext a = vkPNext (a :: SwapchainCreateInfoKHR)
instance ToCStruct PresentInfoKHR VkPresentInfoKHR where
  withCStruct = withCStructPresentInfoKHR
instance FromCStruct PresentInfoKHR VkPresentInfoKHR where
  fromCStruct = fromCStructPresentInfoKHR
instance HasPNext PresentInfoKHR where
  getPNext a = vkPNext (a :: PresentInfoKHR)
instance ToCStruct DebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT where
  withCStruct = withCStructDebugReportCallbackCreateInfoEXT
instance FromCStruct DebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT where
  fromCStruct = fromCStructDebugReportCallbackCreateInfoEXT
instance HasPNext DebugReportCallbackCreateInfoEXT where
  getPNext a = vkPNext (a :: DebugReportCallbackCreateInfoEXT)
instance ToCStruct ValidationFlagsEXT VkValidationFlagsEXT where
  withCStruct = withCStructValidationFlagsEXT
instance FromCStruct ValidationFlagsEXT VkValidationFlagsEXT where
  fromCStruct = fromCStructValidationFlagsEXT
instance HasPNext ValidationFlagsEXT where
  getPNext a = vkPNext (a :: ValidationFlagsEXT)
instance ToCStruct PipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD where
  withCStruct = withCStructPipelineRasterizationStateRasterizationOrderAMD
instance FromCStruct PipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD where
  fromCStruct = fromCStructPipelineRasterizationStateRasterizationOrderAMD
instance HasPNext PipelineRasterizationStateRasterizationOrderAMD where
  getPNext a = vkPNext (a :: PipelineRasterizationStateRasterizationOrderAMD)
instance ToCStruct DebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT where
  withCStruct = withCStructDebugMarkerObjectNameInfoEXT
instance FromCStruct DebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT where
  fromCStruct = fromCStructDebugMarkerObjectNameInfoEXT
instance HasPNext DebugMarkerObjectNameInfoEXT where
  getPNext a = vkPNext (a :: DebugMarkerObjectNameInfoEXT)
instance ToCStruct DebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT where
  withCStruct = withCStructDebugMarkerObjectTagInfoEXT
instance FromCStruct DebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT where
  fromCStruct = fromCStructDebugMarkerObjectTagInfoEXT
instance HasPNext DebugMarkerObjectTagInfoEXT where
  getPNext a = vkPNext (a :: DebugMarkerObjectTagInfoEXT)
instance ToCStruct DebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT where
  withCStruct = withCStructDebugMarkerMarkerInfoEXT
instance FromCStruct DebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT where
  fromCStruct = fromCStructDebugMarkerMarkerInfoEXT
instance HasPNext DebugMarkerMarkerInfoEXT where
  getPNext a = vkPNext (a :: DebugMarkerMarkerInfoEXT)
instance ToCStruct DedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV where
  withCStruct = withCStructDedicatedAllocationImageCreateInfoNV
instance FromCStruct DedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV where
  fromCStruct = fromCStructDedicatedAllocationImageCreateInfoNV
instance HasPNext DedicatedAllocationImageCreateInfoNV where
  getPNext a = vkPNext (a :: DedicatedAllocationImageCreateInfoNV)
instance ToCStruct DedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV where
  withCStruct = withCStructDedicatedAllocationBufferCreateInfoNV
instance FromCStruct DedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV where
  fromCStruct = fromCStructDedicatedAllocationBufferCreateInfoNV
instance HasPNext DedicatedAllocationBufferCreateInfoNV where
  getPNext a = vkPNext (a :: DedicatedAllocationBufferCreateInfoNV)
instance ToCStruct DedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV where
  withCStruct = withCStructDedicatedAllocationMemoryAllocateInfoNV
instance FromCStruct DedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV where
  fromCStruct = fromCStructDedicatedAllocationMemoryAllocateInfoNV
instance HasPNext DedicatedAllocationMemoryAllocateInfoNV where
  getPNext a = vkPNext (a :: DedicatedAllocationMemoryAllocateInfoNV)
instance ToCStruct ExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV where
  withCStruct = withCStructExternalImageFormatPropertiesNV
instance FromCStruct ExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV where
  fromCStruct = fromCStructExternalImageFormatPropertiesNV

instance ToCStruct ExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV where
  withCStruct = withCStructExternalMemoryImageCreateInfoNV
instance FromCStruct ExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV where
  fromCStruct = fromCStructExternalMemoryImageCreateInfoNV
instance HasPNext ExternalMemoryImageCreateInfoNV where
  getPNext a = vkPNext (a :: ExternalMemoryImageCreateInfoNV)
instance ToCStruct ExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV where
  withCStruct = withCStructExportMemoryAllocateInfoNV
instance FromCStruct ExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV where
  fromCStruct = fromCStructExportMemoryAllocateInfoNV
instance HasPNext ExportMemoryAllocateInfoNV where
  getPNext a = vkPNext (a :: ExportMemoryAllocateInfoNV)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV where
  withCStruct = withCStructImportMemoryWin32HandleInfoNV
instance FromCStruct ImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV where
  fromCStruct = fromCStructImportMemoryWin32HandleInfoNV
instance HasPNext ImportMemoryWin32HandleInfoNV where
  getPNext a = vkPNext (a :: ImportMemoryWin32HandleInfoNV)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV where
  withCStruct = withCStructExportMemoryWin32HandleInfoNV
instance FromCStruct ExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV where
  fromCStruct = fromCStructExportMemoryWin32HandleInfoNV
instance HasPNext ExportMemoryWin32HandleInfoNV where
  getPNext a = vkPNext (a :: ExportMemoryWin32HandleInfoNV)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct Win32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV where
  withCStruct = withCStructWin32KeyedMutexAcquireReleaseInfoNV
instance FromCStruct Win32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV where
  fromCStruct = fromCStructWin32KeyedMutexAcquireReleaseInfoNV
instance HasPNext Win32KeyedMutexAcquireReleaseInfoNV where
  getPNext a = vkPNext (a :: Win32KeyedMutexAcquireReleaseInfoNV)
#endif
instance ToCStruct DeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX where
  withCStruct = withCStructDeviceGeneratedCommandsFeaturesNVX
instance FromCStruct DeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX where
  fromCStruct = fromCStructDeviceGeneratedCommandsFeaturesNVX
instance HasPNext DeviceGeneratedCommandsFeaturesNVX where
  getPNext a = vkPNext (a :: DeviceGeneratedCommandsFeaturesNVX)
instance ToCStruct DeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX where
  withCStruct = withCStructDeviceGeneratedCommandsLimitsNVX
instance FromCStruct DeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX where
  fromCStruct = fromCStructDeviceGeneratedCommandsLimitsNVX
instance HasPNext DeviceGeneratedCommandsLimitsNVX where
  getPNext a = vkPNext (a :: DeviceGeneratedCommandsLimitsNVX)
instance ToCStruct IndirectCommandsTokenNVX VkIndirectCommandsTokenNVX where
  withCStruct = withCStructIndirectCommandsTokenNVX
instance FromCStruct IndirectCommandsTokenNVX VkIndirectCommandsTokenNVX where
  fromCStruct = fromCStructIndirectCommandsTokenNVX

instance ToCStruct IndirectCommandsLayoutTokenNVX VkIndirectCommandsLayoutTokenNVX where
  withCStruct = withCStructIndirectCommandsLayoutTokenNVX
instance FromCStruct IndirectCommandsLayoutTokenNVX VkIndirectCommandsLayoutTokenNVX where
  fromCStruct = fromCStructIndirectCommandsLayoutTokenNVX

instance ToCStruct IndirectCommandsLayoutCreateInfoNVX VkIndirectCommandsLayoutCreateInfoNVX where
  withCStruct = withCStructIndirectCommandsLayoutCreateInfoNVX
instance FromCStruct IndirectCommandsLayoutCreateInfoNVX VkIndirectCommandsLayoutCreateInfoNVX where
  fromCStruct = fromCStructIndirectCommandsLayoutCreateInfoNVX
instance HasPNext IndirectCommandsLayoutCreateInfoNVX where
  getPNext a = vkPNext (a :: IndirectCommandsLayoutCreateInfoNVX)
instance ToCStruct CmdProcessCommandsInfoNVX VkCmdProcessCommandsInfoNVX where
  withCStruct = withCStructCmdProcessCommandsInfoNVX
-- No FromCStruct instance for VkCmdProcessCommandsInfoNVX as it contains a dispatchable handle
instance HasPNext CmdProcessCommandsInfoNVX where
  getPNext a = vkPNext (a :: CmdProcessCommandsInfoNVX)
instance ToCStruct CmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX where
  withCStruct = withCStructCmdReserveSpaceForCommandsInfoNVX
instance FromCStruct CmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX where
  fromCStruct = fromCStructCmdReserveSpaceForCommandsInfoNVX
instance HasPNext CmdReserveSpaceForCommandsInfoNVX where
  getPNext a = vkPNext (a :: CmdReserveSpaceForCommandsInfoNVX)
instance ToCStruct ObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX where
  withCStruct = withCStructObjectTableCreateInfoNVX
instance FromCStruct ObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX where
  fromCStruct = fromCStructObjectTableCreateInfoNVX
instance HasPNext ObjectTableCreateInfoNVX where
  getPNext a = vkPNext (a :: ObjectTableCreateInfoNVX)
instance ToCStruct ObjectTableEntryNVX VkObjectTableEntryNVX where
  withCStruct = withCStructObjectTableEntryNVX
instance FromCStruct ObjectTableEntryNVX VkObjectTableEntryNVX where
  fromCStruct = fromCStructObjectTableEntryNVX

instance ToCStruct ObjectTablePipelineEntryNVX VkObjectTablePipelineEntryNVX where
  withCStruct = withCStructObjectTablePipelineEntryNVX
instance FromCStruct ObjectTablePipelineEntryNVX VkObjectTablePipelineEntryNVX where
  fromCStruct = fromCStructObjectTablePipelineEntryNVX

instance ToCStruct ObjectTableDescriptorSetEntryNVX VkObjectTableDescriptorSetEntryNVX where
  withCStruct = withCStructObjectTableDescriptorSetEntryNVX
instance FromCStruct ObjectTableDescriptorSetEntryNVX VkObjectTableDescriptorSetEntryNVX where
  fromCStruct = fromCStructObjectTableDescriptorSetEntryNVX

instance ToCStruct ObjectTableVertexBufferEntryNVX VkObjectTableVertexBufferEntryNVX where
  withCStruct = withCStructObjectTableVertexBufferEntryNVX
instance FromCStruct ObjectTableVertexBufferEntryNVX VkObjectTableVertexBufferEntryNVX where
  fromCStruct = fromCStructObjectTableVertexBufferEntryNVX

instance ToCStruct ObjectTableIndexBufferEntryNVX VkObjectTableIndexBufferEntryNVX where
  withCStruct = withCStructObjectTableIndexBufferEntryNVX
instance FromCStruct ObjectTableIndexBufferEntryNVX VkObjectTableIndexBufferEntryNVX where
  fromCStruct = fromCStructObjectTableIndexBufferEntryNVX

instance ToCStruct ObjectTablePushConstantEntryNVX VkObjectTablePushConstantEntryNVX where
  withCStruct = withCStructObjectTablePushConstantEntryNVX
instance FromCStruct ObjectTablePushConstantEntryNVX VkObjectTablePushConstantEntryNVX where
  fromCStruct = fromCStructObjectTablePushConstantEntryNVX

instance ToCStruct PhysicalDeviceFeatures2 VkPhysicalDeviceFeatures2 where
  withCStruct = withCStructPhysicalDeviceFeatures2
instance FromCStruct PhysicalDeviceFeatures2 VkPhysicalDeviceFeatures2 where
  fromCStruct = fromCStructPhysicalDeviceFeatures2
instance HasPNext PhysicalDeviceFeatures2 where
  getPNext a = vkPNext (a :: PhysicalDeviceFeatures2)
instance ToCStruct PhysicalDeviceProperties2 VkPhysicalDeviceProperties2 where
  withCStruct = withCStructPhysicalDeviceProperties2
instance FromCStruct PhysicalDeviceProperties2 VkPhysicalDeviceProperties2 where
  fromCStruct = fromCStructPhysicalDeviceProperties2
instance HasPNext PhysicalDeviceProperties2 where
  getPNext a = vkPNext (a :: PhysicalDeviceProperties2)
instance ToCStruct FormatProperties2 VkFormatProperties2 where
  withCStruct = withCStructFormatProperties2
instance FromCStruct FormatProperties2 VkFormatProperties2 where
  fromCStruct = fromCStructFormatProperties2
instance HasPNext FormatProperties2 where
  getPNext a = vkPNext (a :: FormatProperties2)
instance ToCStruct ImageFormatProperties2 VkImageFormatProperties2 where
  withCStruct = withCStructImageFormatProperties2
instance FromCStruct ImageFormatProperties2 VkImageFormatProperties2 where
  fromCStruct = fromCStructImageFormatProperties2
instance HasPNext ImageFormatProperties2 where
  getPNext a = vkPNext (a :: ImageFormatProperties2)
instance ToCStruct PhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 where
  withCStruct = withCStructPhysicalDeviceImageFormatInfo2
instance FromCStruct PhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 where
  fromCStruct = fromCStructPhysicalDeviceImageFormatInfo2
instance HasPNext PhysicalDeviceImageFormatInfo2 where
  getPNext a = vkPNext (a :: PhysicalDeviceImageFormatInfo2)
instance ToCStruct QueueFamilyProperties2 VkQueueFamilyProperties2 where
  withCStruct = withCStructQueueFamilyProperties2
instance FromCStruct QueueFamilyProperties2 VkQueueFamilyProperties2 where
  fromCStruct = fromCStructQueueFamilyProperties2
instance HasPNext QueueFamilyProperties2 where
  getPNext a = vkPNext (a :: QueueFamilyProperties2)
instance ToCStruct PhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 where
  withCStruct = withCStructPhysicalDeviceMemoryProperties2
instance FromCStruct PhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 where
  fromCStruct = fromCStructPhysicalDeviceMemoryProperties2
instance HasPNext PhysicalDeviceMemoryProperties2 where
  getPNext a = vkPNext (a :: PhysicalDeviceMemoryProperties2)
instance ToCStruct SparseImageFormatProperties2 VkSparseImageFormatProperties2 where
  withCStruct = withCStructSparseImageFormatProperties2
instance FromCStruct SparseImageFormatProperties2 VkSparseImageFormatProperties2 where
  fromCStruct = fromCStructSparseImageFormatProperties2
instance HasPNext SparseImageFormatProperties2 where
  getPNext a = vkPNext (a :: SparseImageFormatProperties2)
instance ToCStruct PhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 where
  withCStruct = withCStructPhysicalDeviceSparseImageFormatInfo2
instance FromCStruct PhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 where
  fromCStruct = fromCStructPhysicalDeviceSparseImageFormatInfo2
instance HasPNext PhysicalDeviceSparseImageFormatInfo2 where
  getPNext a = vkPNext (a :: PhysicalDeviceSparseImageFormatInfo2)
instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR where
  withCStruct = withCStructPhysicalDevicePushDescriptorPropertiesKHR
instance FromCStruct PhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR where
  fromCStruct = fromCStructPhysicalDevicePushDescriptorPropertiesKHR
instance HasPNext PhysicalDevicePushDescriptorPropertiesKHR where
  getPNext a = vkPNext (a :: PhysicalDevicePushDescriptorPropertiesKHR)
instance ToCStruct PresentRegionsKHR VkPresentRegionsKHR where
  withCStruct = withCStructPresentRegionsKHR
instance FromCStruct PresentRegionsKHR VkPresentRegionsKHR where
  fromCStruct = fromCStructPresentRegionsKHR
instance HasPNext PresentRegionsKHR where
  getPNext a = vkPNext (a :: PresentRegionsKHR)
instance ToCStruct PresentRegionKHR VkPresentRegionKHR where
  withCStruct = withCStructPresentRegionKHR
instance FromCStruct PresentRegionKHR VkPresentRegionKHR where
  fromCStruct = fromCStructPresentRegionKHR

instance ToCStruct RectLayerKHR VkRectLayerKHR where
  withCStruct = withCStructRectLayerKHR
instance FromCStruct RectLayerKHR VkRectLayerKHR where
  fromCStruct = fromCStructRectLayerKHR

instance ToCStruct PhysicalDeviceVariablePointerFeatures VkPhysicalDeviceVariablePointerFeatures where
  withCStruct = withCStructPhysicalDeviceVariablePointerFeatures
instance FromCStruct PhysicalDeviceVariablePointerFeatures VkPhysicalDeviceVariablePointerFeatures where
  fromCStruct = fromCStructPhysicalDeviceVariablePointerFeatures
instance HasPNext PhysicalDeviceVariablePointerFeatures where
  getPNext a = vkPNext (a :: PhysicalDeviceVariablePointerFeatures)
instance ToCStruct ExternalMemoryProperties VkExternalMemoryProperties where
  withCStruct = withCStructExternalMemoryProperties
instance FromCStruct ExternalMemoryProperties VkExternalMemoryProperties where
  fromCStruct = fromCStructExternalMemoryProperties

instance ToCStruct PhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo where
  withCStruct = withCStructPhysicalDeviceExternalImageFormatInfo
instance FromCStruct PhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalImageFormatInfo
instance HasPNext PhysicalDeviceExternalImageFormatInfo where
  getPNext a = vkPNext (a :: PhysicalDeviceExternalImageFormatInfo)
instance ToCStruct ExternalImageFormatProperties VkExternalImageFormatProperties where
  withCStruct = withCStructExternalImageFormatProperties
instance FromCStruct ExternalImageFormatProperties VkExternalImageFormatProperties where
  fromCStruct = fromCStructExternalImageFormatProperties
instance HasPNext ExternalImageFormatProperties where
  getPNext a = vkPNext (a :: ExternalImageFormatProperties)
instance ToCStruct PhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo where
  withCStruct = withCStructPhysicalDeviceExternalBufferInfo
instance FromCStruct PhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalBufferInfo
instance HasPNext PhysicalDeviceExternalBufferInfo where
  getPNext a = vkPNext (a :: PhysicalDeviceExternalBufferInfo)
instance ToCStruct ExternalBufferProperties VkExternalBufferProperties where
  withCStruct = withCStructExternalBufferProperties
instance FromCStruct ExternalBufferProperties VkExternalBufferProperties where
  fromCStruct = fromCStructExternalBufferProperties
instance HasPNext ExternalBufferProperties where
  getPNext a = vkPNext (a :: ExternalBufferProperties)
instance ToCStruct PhysicalDeviceIDProperties VkPhysicalDeviceIDProperties where
  withCStruct = withCStructPhysicalDeviceIDProperties
instance FromCStruct PhysicalDeviceIDProperties VkPhysicalDeviceIDProperties where
  fromCStruct = fromCStructPhysicalDeviceIDProperties
instance HasPNext PhysicalDeviceIDProperties where
  getPNext a = vkPNext (a :: PhysicalDeviceIDProperties)
instance ToCStruct ExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo where
  withCStruct = withCStructExternalMemoryImageCreateInfo
instance FromCStruct ExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo where
  fromCStruct = fromCStructExternalMemoryImageCreateInfo
instance HasPNext ExternalMemoryImageCreateInfo where
  getPNext a = vkPNext (a :: ExternalMemoryImageCreateInfo)
instance ToCStruct ExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo where
  withCStruct = withCStructExternalMemoryBufferCreateInfo
instance FromCStruct ExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo where
  fromCStruct = fromCStructExternalMemoryBufferCreateInfo
instance HasPNext ExternalMemoryBufferCreateInfo where
  getPNext a = vkPNext (a :: ExternalMemoryBufferCreateInfo)
instance ToCStruct ExportMemoryAllocateInfo VkExportMemoryAllocateInfo where
  withCStruct = withCStructExportMemoryAllocateInfo
instance FromCStruct ExportMemoryAllocateInfo VkExportMemoryAllocateInfo where
  fromCStruct = fromCStructExportMemoryAllocateInfo
instance HasPNext ExportMemoryAllocateInfo where
  getPNext a = vkPNext (a :: ExportMemoryAllocateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR where
  withCStruct = withCStructImportMemoryWin32HandleInfoKHR
instance FromCStruct ImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR where
  fromCStruct = fromCStructImportMemoryWin32HandleInfoKHR
instance HasPNext ImportMemoryWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: ImportMemoryWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR where
  withCStruct = withCStructExportMemoryWin32HandleInfoKHR
instance FromCStruct ExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR where
  fromCStruct = fromCStructExportMemoryWin32HandleInfoKHR
instance HasPNext ExportMemoryWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: ExportMemoryWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct MemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR where
  withCStruct = withCStructMemoryWin32HandlePropertiesKHR
instance FromCStruct MemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR where
  fromCStruct = fromCStructMemoryWin32HandlePropertiesKHR
instance HasPNext MemoryWin32HandlePropertiesKHR where
  getPNext a = vkPNext (a :: MemoryWin32HandlePropertiesKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct MemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR where
  withCStruct = withCStructMemoryGetWin32HandleInfoKHR
instance FromCStruct MemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR where
  fromCStruct = fromCStructMemoryGetWin32HandleInfoKHR
instance HasPNext MemoryGetWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: MemoryGetWin32HandleInfoKHR)
#endif
instance ToCStruct ImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR where
  withCStruct = withCStructImportMemoryFdInfoKHR
instance FromCStruct ImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR where
  fromCStruct = fromCStructImportMemoryFdInfoKHR
instance HasPNext ImportMemoryFdInfoKHR where
  getPNext a = vkPNext (a :: ImportMemoryFdInfoKHR)
instance ToCStruct MemoryFdPropertiesKHR VkMemoryFdPropertiesKHR where
  withCStruct = withCStructMemoryFdPropertiesKHR
instance FromCStruct MemoryFdPropertiesKHR VkMemoryFdPropertiesKHR where
  fromCStruct = fromCStructMemoryFdPropertiesKHR
instance HasPNext MemoryFdPropertiesKHR where
  getPNext a = vkPNext (a :: MemoryFdPropertiesKHR)
instance ToCStruct MemoryGetFdInfoKHR VkMemoryGetFdInfoKHR where
  withCStruct = withCStructMemoryGetFdInfoKHR
instance FromCStruct MemoryGetFdInfoKHR VkMemoryGetFdInfoKHR where
  fromCStruct = fromCStructMemoryGetFdInfoKHR
instance HasPNext MemoryGetFdInfoKHR where
  getPNext a = vkPNext (a :: MemoryGetFdInfoKHR)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct Win32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR where
  withCStruct = withCStructWin32KeyedMutexAcquireReleaseInfoKHR
instance FromCStruct Win32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR where
  fromCStruct = fromCStructWin32KeyedMutexAcquireReleaseInfoKHR
instance HasPNext Win32KeyedMutexAcquireReleaseInfoKHR where
  getPNext a = vkPNext (a :: Win32KeyedMutexAcquireReleaseInfoKHR)
#endif
instance ToCStruct PhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo where
  withCStruct = withCStructPhysicalDeviceExternalSemaphoreInfo
instance FromCStruct PhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalSemaphoreInfo
instance HasPNext PhysicalDeviceExternalSemaphoreInfo where
  getPNext a = vkPNext (a :: PhysicalDeviceExternalSemaphoreInfo)
instance ToCStruct ExternalSemaphoreProperties VkExternalSemaphoreProperties where
  withCStruct = withCStructExternalSemaphoreProperties
instance FromCStruct ExternalSemaphoreProperties VkExternalSemaphoreProperties where
  fromCStruct = fromCStructExternalSemaphoreProperties
instance HasPNext ExternalSemaphoreProperties where
  getPNext a = vkPNext (a :: ExternalSemaphoreProperties)
instance ToCStruct ExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo where
  withCStruct = withCStructExportSemaphoreCreateInfo
instance FromCStruct ExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo where
  fromCStruct = fromCStructExportSemaphoreCreateInfo
instance HasPNext ExportSemaphoreCreateInfo where
  getPNext a = vkPNext (a :: ExportSemaphoreCreateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR where
  withCStruct = withCStructImportSemaphoreWin32HandleInfoKHR
instance FromCStruct ImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR where
  fromCStruct = fromCStructImportSemaphoreWin32HandleInfoKHR
instance HasPNext ImportSemaphoreWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: ImportSemaphoreWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR where
  withCStruct = withCStructExportSemaphoreWin32HandleInfoKHR
instance FromCStruct ExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR where
  fromCStruct = fromCStructExportSemaphoreWin32HandleInfoKHR
instance HasPNext ExportSemaphoreWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: ExportSemaphoreWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct D3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR where
  withCStruct = withCStructD3D12FenceSubmitInfoKHR
instance FromCStruct D3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR where
  fromCStruct = fromCStructD3D12FenceSubmitInfoKHR
instance HasPNext D3D12FenceSubmitInfoKHR where
  getPNext a = vkPNext (a :: D3D12FenceSubmitInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct SemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR where
  withCStruct = withCStructSemaphoreGetWin32HandleInfoKHR
instance FromCStruct SemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR where
  fromCStruct = fromCStructSemaphoreGetWin32HandleInfoKHR
instance HasPNext SemaphoreGetWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: SemaphoreGetWin32HandleInfoKHR)
#endif
instance ToCStruct ImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR where
  withCStruct = withCStructImportSemaphoreFdInfoKHR
instance FromCStruct ImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR where
  fromCStruct = fromCStructImportSemaphoreFdInfoKHR
instance HasPNext ImportSemaphoreFdInfoKHR where
  getPNext a = vkPNext (a :: ImportSemaphoreFdInfoKHR)
instance ToCStruct SemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR where
  withCStruct = withCStructSemaphoreGetFdInfoKHR
instance FromCStruct SemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR where
  fromCStruct = fromCStructSemaphoreGetFdInfoKHR
instance HasPNext SemaphoreGetFdInfoKHR where
  getPNext a = vkPNext (a :: SemaphoreGetFdInfoKHR)
instance ToCStruct PhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo where
  withCStruct = withCStructPhysicalDeviceExternalFenceInfo
instance FromCStruct PhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalFenceInfo
instance HasPNext PhysicalDeviceExternalFenceInfo where
  getPNext a = vkPNext (a :: PhysicalDeviceExternalFenceInfo)
instance ToCStruct ExternalFenceProperties VkExternalFenceProperties where
  withCStruct = withCStructExternalFenceProperties
instance FromCStruct ExternalFenceProperties VkExternalFenceProperties where
  fromCStruct = fromCStructExternalFenceProperties
instance HasPNext ExternalFenceProperties where
  getPNext a = vkPNext (a :: ExternalFenceProperties)
instance ToCStruct ExportFenceCreateInfo VkExportFenceCreateInfo where
  withCStruct = withCStructExportFenceCreateInfo
instance FromCStruct ExportFenceCreateInfo VkExportFenceCreateInfo where
  fromCStruct = fromCStructExportFenceCreateInfo
instance HasPNext ExportFenceCreateInfo where
  getPNext a = vkPNext (a :: ExportFenceCreateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR where
  withCStruct = withCStructImportFenceWin32HandleInfoKHR
instance FromCStruct ImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR where
  fromCStruct = fromCStructImportFenceWin32HandleInfoKHR
instance HasPNext ImportFenceWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: ImportFenceWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR where
  withCStruct = withCStructExportFenceWin32HandleInfoKHR
instance FromCStruct ExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR where
  fromCStruct = fromCStructExportFenceWin32HandleInfoKHR
instance HasPNext ExportFenceWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: ExportFenceWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct FenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR where
  withCStruct = withCStructFenceGetWin32HandleInfoKHR
instance FromCStruct FenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR where
  fromCStruct = fromCStructFenceGetWin32HandleInfoKHR
instance HasPNext FenceGetWin32HandleInfoKHR where
  getPNext a = vkPNext (a :: FenceGetWin32HandleInfoKHR)
#endif
instance ToCStruct ImportFenceFdInfoKHR VkImportFenceFdInfoKHR where
  withCStruct = withCStructImportFenceFdInfoKHR
instance FromCStruct ImportFenceFdInfoKHR VkImportFenceFdInfoKHR where
  fromCStruct = fromCStructImportFenceFdInfoKHR
instance HasPNext ImportFenceFdInfoKHR where
  getPNext a = vkPNext (a :: ImportFenceFdInfoKHR)
instance ToCStruct FenceGetFdInfoKHR VkFenceGetFdInfoKHR where
  withCStruct = withCStructFenceGetFdInfoKHR
instance FromCStruct FenceGetFdInfoKHR VkFenceGetFdInfoKHR where
  fromCStruct = fromCStructFenceGetFdInfoKHR
instance HasPNext FenceGetFdInfoKHR where
  getPNext a = vkPNext (a :: FenceGetFdInfoKHR)
instance ToCStruct PhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures where
  withCStruct = withCStructPhysicalDeviceMultiviewFeatures
instance FromCStruct PhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures where
  fromCStruct = fromCStructPhysicalDeviceMultiviewFeatures
instance HasPNext PhysicalDeviceMultiviewFeatures where
  getPNext a = vkPNext (a :: PhysicalDeviceMultiviewFeatures)
instance ToCStruct PhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties where
  withCStruct = withCStructPhysicalDeviceMultiviewProperties
instance FromCStruct PhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties where
  fromCStruct = fromCStructPhysicalDeviceMultiviewProperties
instance HasPNext PhysicalDeviceMultiviewProperties where
  getPNext a = vkPNext (a :: PhysicalDeviceMultiviewProperties)
instance ToCStruct RenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo where
  withCStruct = withCStructRenderPassMultiviewCreateInfo
instance FromCStruct RenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo where
  fromCStruct = fromCStructRenderPassMultiviewCreateInfo
instance HasPNext RenderPassMultiviewCreateInfo where
  getPNext a = vkPNext (a :: RenderPassMultiviewCreateInfo)
instance ToCStruct SurfaceCapabilities2EXT VkSurfaceCapabilities2EXT where
  withCStruct = withCStructSurfaceCapabilities2EXT
instance FromCStruct SurfaceCapabilities2EXT VkSurfaceCapabilities2EXT where
  fromCStruct = fromCStructSurfaceCapabilities2EXT
instance HasPNext SurfaceCapabilities2EXT where
  getPNext a = vkPNext (a :: SurfaceCapabilities2EXT)
instance ToCStruct DisplayPowerInfoEXT VkDisplayPowerInfoEXT where
  withCStruct = withCStructDisplayPowerInfoEXT
instance FromCStruct DisplayPowerInfoEXT VkDisplayPowerInfoEXT where
  fromCStruct = fromCStructDisplayPowerInfoEXT
instance HasPNext DisplayPowerInfoEXT where
  getPNext a = vkPNext (a :: DisplayPowerInfoEXT)
instance ToCStruct DeviceEventInfoEXT VkDeviceEventInfoEXT where
  withCStruct = withCStructDeviceEventInfoEXT
instance FromCStruct DeviceEventInfoEXT VkDeviceEventInfoEXT where
  fromCStruct = fromCStructDeviceEventInfoEXT
instance HasPNext DeviceEventInfoEXT where
  getPNext a = vkPNext (a :: DeviceEventInfoEXT)
instance ToCStruct DisplayEventInfoEXT VkDisplayEventInfoEXT where
  withCStruct = withCStructDisplayEventInfoEXT
instance FromCStruct DisplayEventInfoEXT VkDisplayEventInfoEXT where
  fromCStruct = fromCStructDisplayEventInfoEXT
instance HasPNext DisplayEventInfoEXT where
  getPNext a = vkPNext (a :: DisplayEventInfoEXT)
instance ToCStruct SwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT where
  withCStruct = withCStructSwapchainCounterCreateInfoEXT
instance FromCStruct SwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT where
  fromCStruct = fromCStructSwapchainCounterCreateInfoEXT
instance HasPNext SwapchainCounterCreateInfoEXT where
  getPNext a = vkPNext (a :: SwapchainCounterCreateInfoEXT)
instance ToCStruct PhysicalDeviceGroupProperties VkPhysicalDeviceGroupProperties where
  withCStruct = withCStructPhysicalDeviceGroupProperties
-- No FromCStruct instance for VkPhysicalDeviceGroupProperties as it contains a dispatchable handle
instance HasPNext PhysicalDeviceGroupProperties where
  getPNext a = vkPNext (a :: PhysicalDeviceGroupProperties)
instance ToCStruct MemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo where
  withCStruct = withCStructMemoryAllocateFlagsInfo
instance FromCStruct MemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo where
  fromCStruct = fromCStructMemoryAllocateFlagsInfo
instance HasPNext MemoryAllocateFlagsInfo where
  getPNext a = vkPNext (a :: MemoryAllocateFlagsInfo)
instance ToCStruct BindBufferMemoryInfo VkBindBufferMemoryInfo where
  withCStruct = withCStructBindBufferMemoryInfo
instance FromCStruct BindBufferMemoryInfo VkBindBufferMemoryInfo where
  fromCStruct = fromCStructBindBufferMemoryInfo
instance HasPNext BindBufferMemoryInfo where
  getPNext a = vkPNext (a :: BindBufferMemoryInfo)
instance ToCStruct BindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo where
  withCStruct = withCStructBindBufferMemoryDeviceGroupInfo
instance FromCStruct BindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo where
  fromCStruct = fromCStructBindBufferMemoryDeviceGroupInfo
instance HasPNext BindBufferMemoryDeviceGroupInfo where
  getPNext a = vkPNext (a :: BindBufferMemoryDeviceGroupInfo)
instance ToCStruct BindImageMemoryInfo VkBindImageMemoryInfo where
  withCStruct = withCStructBindImageMemoryInfo
instance FromCStruct BindImageMemoryInfo VkBindImageMemoryInfo where
  fromCStruct = fromCStructBindImageMemoryInfo
instance HasPNext BindImageMemoryInfo where
  getPNext a = vkPNext (a :: BindImageMemoryInfo)
instance ToCStruct BindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo where
  withCStruct = withCStructBindImageMemoryDeviceGroupInfo
instance FromCStruct BindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo where
  fromCStruct = fromCStructBindImageMemoryDeviceGroupInfo
instance HasPNext BindImageMemoryDeviceGroupInfo where
  getPNext a = vkPNext (a :: BindImageMemoryDeviceGroupInfo)
instance ToCStruct DeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo where
  withCStruct = withCStructDeviceGroupRenderPassBeginInfo
instance FromCStruct DeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo where
  fromCStruct = fromCStructDeviceGroupRenderPassBeginInfo
instance HasPNext DeviceGroupRenderPassBeginInfo where
  getPNext a = vkPNext (a :: DeviceGroupRenderPassBeginInfo)
instance ToCStruct DeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo where
  withCStruct = withCStructDeviceGroupCommandBufferBeginInfo
instance FromCStruct DeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo where
  fromCStruct = fromCStructDeviceGroupCommandBufferBeginInfo
instance HasPNext DeviceGroupCommandBufferBeginInfo where
  getPNext a = vkPNext (a :: DeviceGroupCommandBufferBeginInfo)
instance ToCStruct DeviceGroupSubmitInfo VkDeviceGroupSubmitInfo where
  withCStruct = withCStructDeviceGroupSubmitInfo
instance FromCStruct DeviceGroupSubmitInfo VkDeviceGroupSubmitInfo where
  fromCStruct = fromCStructDeviceGroupSubmitInfo
instance HasPNext DeviceGroupSubmitInfo where
  getPNext a = vkPNext (a :: DeviceGroupSubmitInfo)
instance ToCStruct DeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo where
  withCStruct = withCStructDeviceGroupBindSparseInfo
instance FromCStruct DeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo where
  fromCStruct = fromCStructDeviceGroupBindSparseInfo
instance HasPNext DeviceGroupBindSparseInfo where
  getPNext a = vkPNext (a :: DeviceGroupBindSparseInfo)
instance ToCStruct DeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR where
  withCStruct = withCStructDeviceGroupPresentCapabilitiesKHR
instance FromCStruct DeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR where
  fromCStruct = fromCStructDeviceGroupPresentCapabilitiesKHR
instance HasPNext DeviceGroupPresentCapabilitiesKHR where
  getPNext a = vkPNext (a :: DeviceGroupPresentCapabilitiesKHR)
instance ToCStruct ImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR where
  withCStruct = withCStructImageSwapchainCreateInfoKHR
instance FromCStruct ImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR where
  fromCStruct = fromCStructImageSwapchainCreateInfoKHR
instance HasPNext ImageSwapchainCreateInfoKHR where
  getPNext a = vkPNext (a :: ImageSwapchainCreateInfoKHR)
instance ToCStruct BindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR where
  withCStruct = withCStructBindImageMemorySwapchainInfoKHR
instance FromCStruct BindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR where
  fromCStruct = fromCStructBindImageMemorySwapchainInfoKHR
instance HasPNext BindImageMemorySwapchainInfoKHR where
  getPNext a = vkPNext (a :: BindImageMemorySwapchainInfoKHR)
instance ToCStruct AcquireNextImageInfoKHR VkAcquireNextImageInfoKHR where
  withCStruct = withCStructAcquireNextImageInfoKHR
instance FromCStruct AcquireNextImageInfoKHR VkAcquireNextImageInfoKHR where
  fromCStruct = fromCStructAcquireNextImageInfoKHR
instance HasPNext AcquireNextImageInfoKHR where
  getPNext a = vkPNext (a :: AcquireNextImageInfoKHR)
instance ToCStruct DeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR where
  withCStruct = withCStructDeviceGroupPresentInfoKHR
instance FromCStruct DeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR where
  fromCStruct = fromCStructDeviceGroupPresentInfoKHR
instance HasPNext DeviceGroupPresentInfoKHR where
  getPNext a = vkPNext (a :: DeviceGroupPresentInfoKHR)
instance ToCStruct DeviceGroupDeviceCreateInfo VkDeviceGroupDeviceCreateInfo where
  withCStruct = withCStructDeviceGroupDeviceCreateInfo
-- No FromCStruct instance for VkDeviceGroupDeviceCreateInfo as it contains a dispatchable handle
instance HasPNext DeviceGroupDeviceCreateInfo where
  getPNext a = vkPNext (a :: DeviceGroupDeviceCreateInfo)
instance ToCStruct DeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR where
  withCStruct = withCStructDeviceGroupSwapchainCreateInfoKHR
instance FromCStruct DeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR where
  fromCStruct = fromCStructDeviceGroupSwapchainCreateInfoKHR
instance HasPNext DeviceGroupSwapchainCreateInfoKHR where
  getPNext a = vkPNext (a :: DeviceGroupSwapchainCreateInfoKHR)
instance ToCStruct DescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry where
  withCStruct = withCStructDescriptorUpdateTemplateEntry
instance FromCStruct DescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry where
  fromCStruct = fromCStructDescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo where
  withCStruct = withCStructDescriptorUpdateTemplateCreateInfo
instance FromCStruct DescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo where
  fromCStruct = fromCStructDescriptorUpdateTemplateCreateInfo
instance HasPNext DescriptorUpdateTemplateCreateInfo where
  getPNext a = vkPNext (a :: DescriptorUpdateTemplateCreateInfo)
instance ToCStruct XYColorEXT VkXYColorEXT where
  withCStruct = withCStructXYColorEXT
instance FromCStruct XYColorEXT VkXYColorEXT where
  fromCStruct = fromCStructXYColorEXT

instance ToCStruct HdrMetadataEXT VkHdrMetadataEXT where
  withCStruct = withCStructHdrMetadataEXT
instance FromCStruct HdrMetadataEXT VkHdrMetadataEXT where
  fromCStruct = fromCStructHdrMetadataEXT
instance HasPNext HdrMetadataEXT where
  getPNext a = vkPNext (a :: HdrMetadataEXT)
instance ToCStruct RefreshCycleDurationGOOGLE VkRefreshCycleDurationGOOGLE where
  withCStruct = withCStructRefreshCycleDurationGOOGLE
instance FromCStruct RefreshCycleDurationGOOGLE VkRefreshCycleDurationGOOGLE where
  fromCStruct = fromCStructRefreshCycleDurationGOOGLE

instance ToCStruct PastPresentationTimingGOOGLE VkPastPresentationTimingGOOGLE where
  withCStruct = withCStructPastPresentationTimingGOOGLE
instance FromCStruct PastPresentationTimingGOOGLE VkPastPresentationTimingGOOGLE where
  fromCStruct = fromCStructPastPresentationTimingGOOGLE

instance ToCStruct PresentTimesInfoGOOGLE VkPresentTimesInfoGOOGLE where
  withCStruct = withCStructPresentTimesInfoGOOGLE
instance FromCStruct PresentTimesInfoGOOGLE VkPresentTimesInfoGOOGLE where
  fromCStruct = fromCStructPresentTimesInfoGOOGLE
instance HasPNext PresentTimesInfoGOOGLE where
  getPNext a = vkPNext (a :: PresentTimesInfoGOOGLE)
instance ToCStruct PresentTimeGOOGLE VkPresentTimeGOOGLE where
  withCStruct = withCStructPresentTimeGOOGLE
instance FromCStruct PresentTimeGOOGLE VkPresentTimeGOOGLE where
  fromCStruct = fromCStructPresentTimeGOOGLE


#if VK_USE_PLATFORM_IOS_MVK
instance ToCStruct IOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK where
  withCStruct = withCStructIOSSurfaceCreateInfoMVK
instance FromCStruct IOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK where
  fromCStruct = fromCStructIOSSurfaceCreateInfoMVK
instance HasPNext IOSSurfaceCreateInfoMVK where
  getPNext a = vkPNext (a :: IOSSurfaceCreateInfoMVK)
#endif

#if VK_USE_PLATFORM_MACOS_MVK
instance ToCStruct MacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK where
  withCStruct = withCStructMacOSSurfaceCreateInfoMVK
instance FromCStruct MacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK where
  fromCStruct = fromCStructMacOSSurfaceCreateInfoMVK
instance HasPNext MacOSSurfaceCreateInfoMVK where
  getPNext a = vkPNext (a :: MacOSSurfaceCreateInfoMVK)
#endif
instance ToCStruct ViewportWScalingNV VkViewportWScalingNV where
  withCStruct = withCStructViewportWScalingNV
instance FromCStruct ViewportWScalingNV VkViewportWScalingNV where
  fromCStruct = fromCStructViewportWScalingNV

instance ToCStruct PipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportWScalingStateCreateInfoNV
instance FromCStruct PipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportWScalingStateCreateInfoNV
instance HasPNext PipelineViewportWScalingStateCreateInfoNV where
  getPNext a = vkPNext (a :: PipelineViewportWScalingStateCreateInfoNV)
instance ToCStruct ViewportSwizzleNV VkViewportSwizzleNV where
  withCStruct = withCStructViewportSwizzleNV
instance FromCStruct ViewportSwizzleNV VkViewportSwizzleNV where
  fromCStruct = fromCStructViewportSwizzleNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportSwizzleStateCreateInfoNV
instance FromCStruct PipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportSwizzleStateCreateInfoNV
instance HasPNext PipelineViewportSwizzleStateCreateInfoNV where
  getPNext a = vkPNext (a :: PipelineViewportSwizzleStateCreateInfoNV)
instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  withCStruct = withCStructPhysicalDeviceDiscardRectanglePropertiesEXT
instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT
instance HasPNext PhysicalDeviceDiscardRectanglePropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceDiscardRectanglePropertiesEXT)
instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT where
  withCStruct = withCStructPipelineDiscardRectangleStateCreateInfoEXT
instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineDiscardRectangleStateCreateInfoEXT
instance HasPNext PipelineDiscardRectangleStateCreateInfoEXT where
  getPNext a = vkPNext (a :: PipelineDiscardRectangleStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  withCStruct = withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
instance FromCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  fromCStruct = fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
instance HasPNext PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  getPNext a = vkPNext (a :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
instance ToCStruct InputAttachmentAspectReference VkInputAttachmentAspectReference where
  withCStruct = withCStructInputAttachmentAspectReference
instance FromCStruct InputAttachmentAspectReference VkInputAttachmentAspectReference where
  fromCStruct = fromCStructInputAttachmentAspectReference

instance ToCStruct RenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo where
  withCStruct = withCStructRenderPassInputAttachmentAspectCreateInfo
instance FromCStruct RenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo where
  fromCStruct = fromCStructRenderPassInputAttachmentAspectCreateInfo
instance HasPNext RenderPassInputAttachmentAspectCreateInfo where
  getPNext a = vkPNext (a :: RenderPassInputAttachmentAspectCreateInfo)
instance ToCStruct PhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR where
  withCStruct = withCStructPhysicalDeviceSurfaceInfo2KHR
instance FromCStruct PhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR where
  fromCStruct = fromCStructPhysicalDeviceSurfaceInfo2KHR
instance HasPNext PhysicalDeviceSurfaceInfo2KHR where
  getPNext a = vkPNext (a :: PhysicalDeviceSurfaceInfo2KHR)
instance ToCStruct SurfaceCapabilities2KHR VkSurfaceCapabilities2KHR where
  withCStruct = withCStructSurfaceCapabilities2KHR
instance FromCStruct SurfaceCapabilities2KHR VkSurfaceCapabilities2KHR where
  fromCStruct = fromCStructSurfaceCapabilities2KHR
instance HasPNext SurfaceCapabilities2KHR where
  getPNext a = vkPNext (a :: SurfaceCapabilities2KHR)
instance ToCStruct SurfaceFormat2KHR VkSurfaceFormat2KHR where
  withCStruct = withCStructSurfaceFormat2KHR
instance FromCStruct SurfaceFormat2KHR VkSurfaceFormat2KHR where
  fromCStruct = fromCStructSurfaceFormat2KHR
instance HasPNext SurfaceFormat2KHR where
  getPNext a = vkPNext (a :: SurfaceFormat2KHR)
instance ToCStruct SharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR where
  withCStruct = withCStructSharedPresentSurfaceCapabilitiesKHR
instance FromCStruct SharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR where
  fromCStruct = fromCStructSharedPresentSurfaceCapabilitiesKHR
instance HasPNext SharedPresentSurfaceCapabilitiesKHR where
  getPNext a = vkPNext (a :: SharedPresentSurfaceCapabilitiesKHR)
instance ToCStruct PhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures where
  withCStruct = withCStructPhysicalDevice16BitStorageFeatures
instance FromCStruct PhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures where
  fromCStruct = fromCStructPhysicalDevice16BitStorageFeatures
instance HasPNext PhysicalDevice16BitStorageFeatures where
  getPNext a = vkPNext (a :: PhysicalDevice16BitStorageFeatures)
instance ToCStruct PhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties where
  withCStruct = withCStructPhysicalDeviceSubgroupProperties
instance FromCStruct PhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties where
  fromCStruct = fromCStructPhysicalDeviceSubgroupProperties
instance HasPNext PhysicalDeviceSubgroupProperties where
  getPNext a = vkPNext (a :: PhysicalDeviceSubgroupProperties)
instance ToCStruct BufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 where
  withCStruct = withCStructBufferMemoryRequirementsInfo2
instance FromCStruct BufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 where
  fromCStruct = fromCStructBufferMemoryRequirementsInfo2
instance HasPNext BufferMemoryRequirementsInfo2 where
  getPNext a = vkPNext (a :: BufferMemoryRequirementsInfo2)
instance ToCStruct ImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 where
  withCStruct = withCStructImageMemoryRequirementsInfo2
instance FromCStruct ImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 where
  fromCStruct = fromCStructImageMemoryRequirementsInfo2
instance HasPNext ImageMemoryRequirementsInfo2 where
  getPNext a = vkPNext (a :: ImageMemoryRequirementsInfo2)
instance ToCStruct ImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 where
  withCStruct = withCStructImageSparseMemoryRequirementsInfo2
instance FromCStruct ImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 where
  fromCStruct = fromCStructImageSparseMemoryRequirementsInfo2
instance HasPNext ImageSparseMemoryRequirementsInfo2 where
  getPNext a = vkPNext (a :: ImageSparseMemoryRequirementsInfo2)
instance ToCStruct MemoryRequirements2 VkMemoryRequirements2 where
  withCStruct = withCStructMemoryRequirements2
instance FromCStruct MemoryRequirements2 VkMemoryRequirements2 where
  fromCStruct = fromCStructMemoryRequirements2
instance HasPNext MemoryRequirements2 where
  getPNext a = vkPNext (a :: MemoryRequirements2)
instance ToCStruct SparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 where
  withCStruct = withCStructSparseImageMemoryRequirements2
instance FromCStruct SparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 where
  fromCStruct = fromCStructSparseImageMemoryRequirements2
instance HasPNext SparseImageMemoryRequirements2 where
  getPNext a = vkPNext (a :: SparseImageMemoryRequirements2)
instance ToCStruct PhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties where
  withCStruct = withCStructPhysicalDevicePointClippingProperties
instance FromCStruct PhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties where
  fromCStruct = fromCStructPhysicalDevicePointClippingProperties
instance HasPNext PhysicalDevicePointClippingProperties where
  getPNext a = vkPNext (a :: PhysicalDevicePointClippingProperties)
instance ToCStruct MemoryDedicatedRequirements VkMemoryDedicatedRequirements where
  withCStruct = withCStructMemoryDedicatedRequirements
instance FromCStruct MemoryDedicatedRequirements VkMemoryDedicatedRequirements where
  fromCStruct = fromCStructMemoryDedicatedRequirements
instance HasPNext MemoryDedicatedRequirements where
  getPNext a = vkPNext (a :: MemoryDedicatedRequirements)
instance ToCStruct MemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo where
  withCStruct = withCStructMemoryDedicatedAllocateInfo
instance FromCStruct MemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo where
  fromCStruct = fromCStructMemoryDedicatedAllocateInfo
instance HasPNext MemoryDedicatedAllocateInfo where
  getPNext a = vkPNext (a :: MemoryDedicatedAllocateInfo)
instance ToCStruct ImageViewUsageCreateInfo VkImageViewUsageCreateInfo where
  withCStruct = withCStructImageViewUsageCreateInfo
instance FromCStruct ImageViewUsageCreateInfo VkImageViewUsageCreateInfo where
  fromCStruct = fromCStructImageViewUsageCreateInfo
instance HasPNext ImageViewUsageCreateInfo where
  getPNext a = vkPNext (a :: ImageViewUsageCreateInfo)
instance ToCStruct PipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo where
  withCStruct = withCStructPipelineTessellationDomainOriginStateCreateInfo
instance FromCStruct PipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo where
  fromCStruct = fromCStructPipelineTessellationDomainOriginStateCreateInfo
instance HasPNext PipelineTessellationDomainOriginStateCreateInfo where
  getPNext a = vkPNext (a :: PipelineTessellationDomainOriginStateCreateInfo)
instance ToCStruct SamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo where
  withCStruct = withCStructSamplerYcbcrConversionInfo
instance FromCStruct SamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo where
  fromCStruct = fromCStructSamplerYcbcrConversionInfo
instance HasPNext SamplerYcbcrConversionInfo where
  getPNext a = vkPNext (a :: SamplerYcbcrConversionInfo)
instance ToCStruct SamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo where
  withCStruct = withCStructSamplerYcbcrConversionCreateInfo
instance FromCStruct SamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo where
  fromCStruct = fromCStructSamplerYcbcrConversionCreateInfo
instance HasPNext SamplerYcbcrConversionCreateInfo where
  getPNext a = vkPNext (a :: SamplerYcbcrConversionCreateInfo)
instance ToCStruct BindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo where
  withCStruct = withCStructBindImagePlaneMemoryInfo
instance FromCStruct BindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo where
  fromCStruct = fromCStructBindImagePlaneMemoryInfo
instance HasPNext BindImagePlaneMemoryInfo where
  getPNext a = vkPNext (a :: BindImagePlaneMemoryInfo)
instance ToCStruct ImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo where
  withCStruct = withCStructImagePlaneMemoryRequirementsInfo
instance FromCStruct ImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo where
  fromCStruct = fromCStructImagePlaneMemoryRequirementsInfo
instance HasPNext ImagePlaneMemoryRequirementsInfo where
  getPNext a = vkPNext (a :: ImagePlaneMemoryRequirementsInfo)
instance ToCStruct PhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  withCStruct = withCStructPhysicalDeviceSamplerYcbcrConversionFeatures
instance FromCStruct PhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  fromCStruct = fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures
instance HasPNext PhysicalDeviceSamplerYcbcrConversionFeatures where
  getPNext a = vkPNext (a :: PhysicalDeviceSamplerYcbcrConversionFeatures)
instance ToCStruct SamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties where
  withCStruct = withCStructSamplerYcbcrConversionImageFormatProperties
instance FromCStruct SamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties where
  fromCStruct = fromCStructSamplerYcbcrConversionImageFormatProperties
instance HasPNext SamplerYcbcrConversionImageFormatProperties where
  getPNext a = vkPNext (a :: SamplerYcbcrConversionImageFormatProperties)
instance ToCStruct TextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD where
  withCStruct = withCStructTextureLODGatherFormatPropertiesAMD
instance FromCStruct TextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD where
  fromCStruct = fromCStructTextureLODGatherFormatPropertiesAMD
instance HasPNext TextureLODGatherFormatPropertiesAMD where
  getPNext a = vkPNext (a :: TextureLODGatherFormatPropertiesAMD)
instance ToCStruct ProtectedSubmitInfo VkProtectedSubmitInfo where
  withCStruct = withCStructProtectedSubmitInfo
instance FromCStruct ProtectedSubmitInfo VkProtectedSubmitInfo where
  fromCStruct = fromCStructProtectedSubmitInfo
instance HasPNext ProtectedSubmitInfo where
  getPNext a = vkPNext (a :: ProtectedSubmitInfo)
instance ToCStruct PhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures where
  withCStruct = withCStructPhysicalDeviceProtectedMemoryFeatures
instance FromCStruct PhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures where
  fromCStruct = fromCStructPhysicalDeviceProtectedMemoryFeatures
instance HasPNext PhysicalDeviceProtectedMemoryFeatures where
  getPNext a = vkPNext (a :: PhysicalDeviceProtectedMemoryFeatures)
instance ToCStruct PhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties where
  withCStruct = withCStructPhysicalDeviceProtectedMemoryProperties
instance FromCStruct PhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties where
  fromCStruct = fromCStructPhysicalDeviceProtectedMemoryProperties
instance HasPNext PhysicalDeviceProtectedMemoryProperties where
  getPNext a = vkPNext (a :: PhysicalDeviceProtectedMemoryProperties)
instance ToCStruct DeviceQueueInfo2 VkDeviceQueueInfo2 where
  withCStruct = withCStructDeviceQueueInfo2
instance FromCStruct DeviceQueueInfo2 VkDeviceQueueInfo2 where
  fromCStruct = fromCStructDeviceQueueInfo2
instance HasPNext DeviceQueueInfo2 where
  getPNext a = vkPNext (a :: DeviceQueueInfo2)
instance ToCStruct PipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV where
  withCStruct = withCStructPipelineCoverageToColorStateCreateInfoNV
instance FromCStruct PipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV where
  fromCStruct = fromCStructPipelineCoverageToColorStateCreateInfoNV
instance HasPNext PipelineCoverageToColorStateCreateInfoNV where
  getPNext a = vkPNext (a :: PipelineCoverageToColorStateCreateInfoNV)
instance ToCStruct PhysicalDeviceSamplerFilterMinmaxPropertiesEXT VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
instance FromCStruct PhysicalDeviceSamplerFilterMinmaxPropertiesEXT VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
instance HasPNext PhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)
instance ToCStruct SampleLocationEXT VkSampleLocationEXT where
  withCStruct = withCStructSampleLocationEXT
instance FromCStruct SampleLocationEXT VkSampleLocationEXT where
  fromCStruct = fromCStructSampleLocationEXT

instance ToCStruct SampleLocationsInfoEXT VkSampleLocationsInfoEXT where
  withCStruct = withCStructSampleLocationsInfoEXT
instance FromCStruct SampleLocationsInfoEXT VkSampleLocationsInfoEXT where
  fromCStruct = fromCStructSampleLocationsInfoEXT
instance HasPNext SampleLocationsInfoEXT where
  getPNext a = vkPNext (a :: SampleLocationsInfoEXT)
instance ToCStruct AttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT where
  withCStruct = withCStructAttachmentSampleLocationsEXT
instance FromCStruct AttachmentSampleLocationsEXT VkAttachmentSampleLocationsEXT where
  fromCStruct = fromCStructAttachmentSampleLocationsEXT

instance ToCStruct SubpassSampleLocationsEXT VkSubpassSampleLocationsEXT where
  withCStruct = withCStructSubpassSampleLocationsEXT
instance FromCStruct SubpassSampleLocationsEXT VkSubpassSampleLocationsEXT where
  fromCStruct = fromCStructSubpassSampleLocationsEXT

instance ToCStruct RenderPassSampleLocationsBeginInfoEXT VkRenderPassSampleLocationsBeginInfoEXT where
  withCStruct = withCStructRenderPassSampleLocationsBeginInfoEXT
instance FromCStruct RenderPassSampleLocationsBeginInfoEXT VkRenderPassSampleLocationsBeginInfoEXT where
  fromCStruct = fromCStructRenderPassSampleLocationsBeginInfoEXT
instance HasPNext RenderPassSampleLocationsBeginInfoEXT where
  getPNext a = vkPNext (a :: RenderPassSampleLocationsBeginInfoEXT)
instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT where
  withCStruct = withCStructPipelineSampleLocationsStateCreateInfoEXT
instance FromCStruct PipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineSampleLocationsStateCreateInfoEXT
instance HasPNext PipelineSampleLocationsStateCreateInfoEXT where
  getPNext a = vkPNext (a :: PipelineSampleLocationsStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceSampleLocationsPropertiesEXT
instance FromCStruct PhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceSampleLocationsPropertiesEXT
instance HasPNext PhysicalDeviceSampleLocationsPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceSampleLocationsPropertiesEXT)
instance ToCStruct MultisamplePropertiesEXT VkMultisamplePropertiesEXT where
  withCStruct = withCStructMultisamplePropertiesEXT
instance FromCStruct MultisamplePropertiesEXT VkMultisamplePropertiesEXT where
  fromCStruct = fromCStructMultisamplePropertiesEXT
instance HasPNext MultisamplePropertiesEXT where
  getPNext a = vkPNext (a :: MultisamplePropertiesEXT)
instance ToCStruct SamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT where
  withCStruct = withCStructSamplerReductionModeCreateInfoEXT
instance FromCStruct SamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT where
  fromCStruct = fromCStructSamplerReductionModeCreateInfoEXT
instance HasPNext SamplerReductionModeCreateInfoEXT where
  getPNext a = vkPNext (a :: SamplerReductionModeCreateInfoEXT)
instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
instance FromCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
instance HasPNext PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
instance FromCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
instance HasPNext PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  withCStruct = withCStructPipelineColorBlendAdvancedStateCreateInfoEXT
instance FromCStruct PipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT
instance HasPNext PipelineColorBlendAdvancedStateCreateInfoEXT where
  getPNext a = vkPNext (a :: PipelineColorBlendAdvancedStateCreateInfoEXT)
instance ToCStruct PipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV where
  withCStruct = withCStructPipelineCoverageModulationStateCreateInfoNV
instance FromCStruct PipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV where
  fromCStruct = fromCStructPipelineCoverageModulationStateCreateInfoNV
instance HasPNext PipelineCoverageModulationStateCreateInfoNV where
  getPNext a = vkPNext (a :: PipelineCoverageModulationStateCreateInfoNV)
instance ToCStruct ImageFormatListCreateInfoKHR VkImageFormatListCreateInfoKHR where
  withCStruct = withCStructImageFormatListCreateInfoKHR
instance FromCStruct ImageFormatListCreateInfoKHR VkImageFormatListCreateInfoKHR where
  fromCStruct = fromCStructImageFormatListCreateInfoKHR
instance HasPNext ImageFormatListCreateInfoKHR where
  getPNext a = vkPNext (a :: ImageFormatListCreateInfoKHR)
instance ToCStruct ValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT where
  withCStruct = withCStructValidationCacheCreateInfoEXT
instance FromCStruct ValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT where
  fromCStruct = fromCStructValidationCacheCreateInfoEXT
instance HasPNext ValidationCacheCreateInfoEXT where
  getPNext a = vkPNext (a :: ValidationCacheCreateInfoEXT)
instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT where
  withCStruct = withCStructShaderModuleValidationCacheCreateInfoEXT
instance FromCStruct ShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT where
  fromCStruct = fromCStructShaderModuleValidationCacheCreateInfoEXT
instance HasPNext ShaderModuleValidationCacheCreateInfoEXT where
  getPNext a = vkPNext (a :: ShaderModuleValidationCacheCreateInfoEXT)
instance ToCStruct PhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties where
  withCStruct = withCStructPhysicalDeviceMaintenance3Properties
instance FromCStruct PhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties where
  fromCStruct = fromCStructPhysicalDeviceMaintenance3Properties
instance HasPNext PhysicalDeviceMaintenance3Properties where
  getPNext a = vkPNext (a :: PhysicalDeviceMaintenance3Properties)
instance ToCStruct DescriptorSetLayoutSupport VkDescriptorSetLayoutSupport where
  withCStruct = withCStructDescriptorSetLayoutSupport
instance FromCStruct DescriptorSetLayoutSupport VkDescriptorSetLayoutSupport where
  fromCStruct = fromCStructDescriptorSetLayoutSupport
instance HasPNext DescriptorSetLayoutSupport where
  getPNext a = vkPNext (a :: DescriptorSetLayoutSupport)
instance ToCStruct PhysicalDeviceShaderDrawParameterFeatures VkPhysicalDeviceShaderDrawParameterFeatures where
  withCStruct = withCStructPhysicalDeviceShaderDrawParameterFeatures
instance FromCStruct PhysicalDeviceShaderDrawParameterFeatures VkPhysicalDeviceShaderDrawParameterFeatures where
  fromCStruct = fromCStructPhysicalDeviceShaderDrawParameterFeatures
instance HasPNext PhysicalDeviceShaderDrawParameterFeatures where
  getPNext a = vkPNext (a :: PhysicalDeviceShaderDrawParameterFeatures)
instance ToCStruct ShaderResourceUsageAMD VkShaderResourceUsageAMD where
  withCStruct = withCStructShaderResourceUsageAMD
instance FromCStruct ShaderResourceUsageAMD VkShaderResourceUsageAMD where
  fromCStruct = fromCStructShaderResourceUsageAMD

instance ToCStruct ShaderStatisticsInfoAMD VkShaderStatisticsInfoAMD where
  withCStruct = withCStructShaderStatisticsInfoAMD
instance FromCStruct ShaderStatisticsInfoAMD VkShaderStatisticsInfoAMD where
  fromCStruct = fromCStructShaderStatisticsInfoAMD

instance ToCStruct DeviceQueueGlobalPriorityCreateInfoEXT VkDeviceQueueGlobalPriorityCreateInfoEXT where
  withCStruct = withCStructDeviceQueueGlobalPriorityCreateInfoEXT
instance FromCStruct DeviceQueueGlobalPriorityCreateInfoEXT VkDeviceQueueGlobalPriorityCreateInfoEXT where
  fromCStruct = fromCStructDeviceQueueGlobalPriorityCreateInfoEXT
instance HasPNext DeviceQueueGlobalPriorityCreateInfoEXT where
  getPNext a = vkPNext (a :: DeviceQueueGlobalPriorityCreateInfoEXT)
instance ToCStruct DebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT where
  withCStruct = withCStructDebugUtilsObjectNameInfoEXT
instance FromCStruct DebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT where
  fromCStruct = fromCStructDebugUtilsObjectNameInfoEXT
instance HasPNext DebugUtilsObjectNameInfoEXT where
  getPNext a = vkPNext (a :: DebugUtilsObjectNameInfoEXT)
instance ToCStruct DebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT where
  withCStruct = withCStructDebugUtilsObjectTagInfoEXT
instance FromCStruct DebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT where
  fromCStruct = fromCStructDebugUtilsObjectTagInfoEXT
instance HasPNext DebugUtilsObjectTagInfoEXT where
  getPNext a = vkPNext (a :: DebugUtilsObjectTagInfoEXT)
instance ToCStruct DebugUtilsLabelEXT VkDebugUtilsLabelEXT where
  withCStruct = withCStructDebugUtilsLabelEXT
instance FromCStruct DebugUtilsLabelEXT VkDebugUtilsLabelEXT where
  fromCStruct = fromCStructDebugUtilsLabelEXT
instance HasPNext DebugUtilsLabelEXT where
  getPNext a = vkPNext (a :: DebugUtilsLabelEXT)
instance ToCStruct DebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT where
  withCStruct = withCStructDebugUtilsMessengerCreateInfoEXT
instance FromCStruct DebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT where
  fromCStruct = fromCStructDebugUtilsMessengerCreateInfoEXT
instance HasPNext DebugUtilsMessengerCreateInfoEXT where
  getPNext a = vkPNext (a :: DebugUtilsMessengerCreateInfoEXT)
instance ToCStruct DebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT where
  withCStruct = withCStructDebugUtilsMessengerCallbackDataEXT
instance FromCStruct DebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT where
  fromCStruct = fromCStructDebugUtilsMessengerCallbackDataEXT
instance HasPNext DebugUtilsMessengerCallbackDataEXT where
  getPNext a = vkPNext (a :: DebugUtilsMessengerCallbackDataEXT)
instance ToCStruct ImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT where
  withCStruct = withCStructImportMemoryHostPointerInfoEXT
instance FromCStruct ImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT where
  fromCStruct = fromCStructImportMemoryHostPointerInfoEXT
instance HasPNext ImportMemoryHostPointerInfoEXT where
  getPNext a = vkPNext (a :: ImportMemoryHostPointerInfoEXT)
instance ToCStruct MemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT where
  withCStruct = withCStructMemoryHostPointerPropertiesEXT
instance FromCStruct MemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT where
  fromCStruct = fromCStructMemoryHostPointerPropertiesEXT
instance HasPNext MemoryHostPointerPropertiesEXT where
  getPNext a = vkPNext (a :: MemoryHostPointerPropertiesEXT)
instance ToCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
instance FromCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
instance HasPNext PhysicalDeviceExternalMemoryHostPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceExternalMemoryHostPropertiesEXT)
instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
instance HasPNext PhysicalDeviceConservativeRasterizationPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceConservativeRasterizationPropertiesEXT)
instance ToCStruct PhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD where
  withCStruct = withCStructPhysicalDeviceShaderCorePropertiesAMD
instance FromCStruct PhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD where
  fromCStruct = fromCStructPhysicalDeviceShaderCorePropertiesAMD
instance HasPNext PhysicalDeviceShaderCorePropertiesAMD where
  getPNext a = vkPNext (a :: PhysicalDeviceShaderCorePropertiesAMD)
instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT where
  withCStruct = withCStructPipelineRasterizationConservativeStateCreateInfoEXT
instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineRasterizationConservativeStateCreateInfoEXT
instance HasPNext PipelineRasterizationConservativeStateCreateInfoEXT where
  getPNext a = vkPNext (a :: PipelineRasterizationConservativeStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceDescriptorIndexingFeaturesEXT VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
instance FromCStruct PhysicalDeviceDescriptorIndexingFeaturesEXT VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
instance HasPNext PhysicalDeviceDescriptorIndexingFeaturesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceDescriptorIndexingFeaturesEXT)
instance ToCStruct PhysicalDeviceDescriptorIndexingPropertiesEXT VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
instance FromCStruct PhysicalDeviceDescriptorIndexingPropertiesEXT VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
instance HasPNext PhysicalDeviceDescriptorIndexingPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceDescriptorIndexingPropertiesEXT)
instance ToCStruct DescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  withCStruct = withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
instance FromCStruct DescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  fromCStruct = fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
instance HasPNext DescriptorSetLayoutBindingFlagsCreateInfoEXT where
  getPNext a = vkPNext (a :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)
instance ToCStruct DescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  withCStruct = withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
instance FromCStruct DescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  fromCStruct = fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
instance HasPNext DescriptorSetVariableDescriptorCountAllocateInfoEXT where
  getPNext a = vkPNext (a :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)
instance ToCStruct DescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  withCStruct = withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
instance FromCStruct DescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  fromCStruct = fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
instance HasPNext DescriptorSetVariableDescriptorCountLayoutSupportEXT where
  getPNext a = vkPNext (a :: DescriptorSetVariableDescriptorCountLayoutSupportEXT)
instance ToCStruct VertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT where
  withCStruct = withCStructVertexInputBindingDivisorDescriptionEXT
instance FromCStruct VertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT where
  fromCStruct = fromCStructVertexInputBindingDivisorDescriptionEXT

instance ToCStruct PipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT where
  withCStruct = withCStructPipelineVertexInputDivisorStateCreateInfoEXT
instance FromCStruct PipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineVertexInputDivisorStateCreateInfoEXT
instance HasPNext PipelineVertexInputDivisorStateCreateInfoEXT where
  getPNext a = vkPNext (a :: PipelineVertexInputDivisorStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
instance HasPNext PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  getPNext a = vkPNext (a :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT)

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct ImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID where
  withCStruct = withCStructImportAndroidHardwareBufferInfoANDROID
instance FromCStruct ImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID where
  fromCStruct = fromCStructImportAndroidHardwareBufferInfoANDROID
instance HasPNext ImportAndroidHardwareBufferInfoANDROID where
  getPNext a = vkPNext (a :: ImportAndroidHardwareBufferInfoANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID where
  withCStruct = withCStructAndroidHardwareBufferUsageANDROID
instance FromCStruct AndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID where
  fromCStruct = fromCStructAndroidHardwareBufferUsageANDROID
instance HasPNext AndroidHardwareBufferUsageANDROID where
  getPNext a = vkPNext (a :: AndroidHardwareBufferUsageANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID where
  withCStruct = withCStructAndroidHardwareBufferPropertiesANDROID
instance FromCStruct AndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID where
  fromCStruct = fromCStructAndroidHardwareBufferPropertiesANDROID
instance HasPNext AndroidHardwareBufferPropertiesANDROID where
  getPNext a = vkPNext (a :: AndroidHardwareBufferPropertiesANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct MemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID where
  withCStruct = withCStructMemoryGetAndroidHardwareBufferInfoANDROID
instance FromCStruct MemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID where
  fromCStruct = fromCStructMemoryGetAndroidHardwareBufferInfoANDROID
instance HasPNext MemoryGetAndroidHardwareBufferInfoANDROID where
  getPNext a = vkPNext (a :: MemoryGetAndroidHardwareBufferInfoANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID where
  withCStruct = withCStructAndroidHardwareBufferFormatPropertiesANDROID
instance FromCStruct AndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID where
  fromCStruct = fromCStructAndroidHardwareBufferFormatPropertiesANDROID
instance HasPNext AndroidHardwareBufferFormatPropertiesANDROID where
  getPNext a = vkPNext (a :: AndroidHardwareBufferFormatPropertiesANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct ExternalFormatANDROID VkExternalFormatANDROID where
  withCStruct = withCStructExternalFormatANDROID
instance FromCStruct ExternalFormatANDROID VkExternalFormatANDROID where
  fromCStruct = fromCStructExternalFormatANDROID
instance HasPNext ExternalFormatANDROID where
  getPNext a = vkPNext (a :: ExternalFormatANDROID)
#endif
instance ToCStruct ClearColorValue VkClearColorValue where
  withCStruct = withCStructClearColorValue
-- No FromCStruct instance for VkClearColorValue as it contains a union type

instance ToCStruct ClearValue VkClearValue where
  withCStruct = withCStructClearValue
-- No FromCStruct instance for VkClearValue as it contains a union type

-- | Read the @sType@ member of a Vulkan struct and marshal the struct into
-- a 'SomeVkStruct'
--
-- Make sure that you only pass this a pointer to a Vulkan struct with a
-- @sType@ member at offset 0 otherwise the behaviour is undefined.
--
-- - Throws an 'InvalidArgument' 'IOException' if given a pointer to a
--   struct with an unrecognised @sType@ member.
-- - Throws an 'InvalidArgument' 'IOException' if given a pointer to a
--   struct which can't be marshalled (those containing union types)
peekVkStruct :: Ptr SomeVkStruct -> IO SomeVkStruct
peekVkStruct p = do
  peek (castPtr p :: Ptr VkStructureType) >>= \case
    VK_STRUCTURE_TYPE_APPLICATION_INFO -> SomeVkStruct <$> (fromCStructApplicationInfo =<< peek (castPtr p :: Ptr VkApplicationInfo))
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO -> SomeVkStruct <$> (fromCStructDeviceQueueCreateInfo =<< peek (castPtr p :: Ptr VkDeviceQueueCreateInfo))
    VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO -> SomeVkStruct <$> (fromCStructDeviceCreateInfo =<< peek (castPtr p :: Ptr VkDeviceCreateInfo))
    VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO -> SomeVkStruct <$> (fromCStructInstanceCreateInfo =<< peek (castPtr p :: Ptr VkInstanceCreateInfo))
    VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO -> SomeVkStruct <$> (fromCStructMemoryAllocateInfo =<< peek (castPtr p :: Ptr VkMemoryAllocateInfo))
    VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE -> SomeVkStruct <$> (fromCStructMappedMemoryRange =<< peek (castPtr p :: Ptr VkMappedMemoryRange))
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET -> SomeVkStruct <$> (fromCStructWriteDescriptorSet =<< peek (castPtr p :: Ptr VkWriteDescriptorSet))
    VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET -> SomeVkStruct <$> (fromCStructCopyDescriptorSet =<< peek (castPtr p :: Ptr VkCopyDescriptorSet))
    VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO -> SomeVkStruct <$> (fromCStructBufferCreateInfo =<< peek (castPtr p :: Ptr VkBufferCreateInfo))
    VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO -> SomeVkStruct <$> (fromCStructBufferViewCreateInfo =<< peek (castPtr p :: Ptr VkBufferViewCreateInfo))
    VK_STRUCTURE_TYPE_MEMORY_BARRIER -> SomeVkStruct <$> (fromCStructMemoryBarrier =<< peek (castPtr p :: Ptr VkMemoryBarrier))
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER -> SomeVkStruct <$> (fromCStructBufferMemoryBarrier =<< peek (castPtr p :: Ptr VkBufferMemoryBarrier))
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER -> SomeVkStruct <$> (fromCStructImageMemoryBarrier =<< peek (castPtr p :: Ptr VkImageMemoryBarrier))
    VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO -> SomeVkStruct <$> (fromCStructImageCreateInfo =<< peek (castPtr p :: Ptr VkImageCreateInfo))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO -> SomeVkStruct <$> (fromCStructImageViewCreateInfo =<< peek (castPtr p :: Ptr VkImageViewCreateInfo))
    VK_STRUCTURE_TYPE_BIND_SPARSE_INFO -> SomeVkStruct <$> (fromCStructBindSparseInfo =<< peek (castPtr p :: Ptr VkBindSparseInfo))
    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO -> SomeVkStruct <$> (fromCStructShaderModuleCreateInfo =<< peek (castPtr p :: Ptr VkShaderModuleCreateInfo))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO -> SomeVkStruct <$> (fromCStructDescriptorSetLayoutCreateInfo =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutCreateInfo))
    VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO -> SomeVkStruct <$> (fromCStructDescriptorPoolCreateInfo =<< peek (castPtr p :: Ptr VkDescriptorPoolCreateInfo))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO -> SomeVkStruct <$> (fromCStructDescriptorSetAllocateInfo =<< peek (castPtr p :: Ptr VkDescriptorSetAllocateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineShaderStageCreateInfo =<< peek (castPtr p :: Ptr VkPipelineShaderStageCreateInfo))
    VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO -> SomeVkStruct <$> (fromCStructComputePipelineCreateInfo =<< peek (castPtr p :: Ptr VkComputePipelineCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineVertexInputStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineVertexInputStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineInputAssemblyStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineInputAssemblyStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineTessellationStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineTessellationStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineViewportStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineViewportStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineRasterizationStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineRasterizationStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineMultisampleStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineMultisampleStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineColorBlendStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineColorBlendStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineDynamicStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineDynamicStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineDepthStencilStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineDepthStencilStateCreateInfo))
    VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO -> SomeVkStruct <$> (fromCStructGraphicsPipelineCreateInfo =<< peek (castPtr p :: Ptr VkGraphicsPipelineCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineCacheCreateInfo =<< peek (castPtr p :: Ptr VkPipelineCacheCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineLayoutCreateInfo =<< peek (castPtr p :: Ptr VkPipelineLayoutCreateInfo))
    VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO -> SomeVkStruct <$> (fromCStructSamplerCreateInfo =<< peek (castPtr p :: Ptr VkSamplerCreateInfo))
    VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO -> SomeVkStruct <$> (fromCStructCommandPoolCreateInfo =<< peek (castPtr p :: Ptr VkCommandPoolCreateInfo))
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO -> SomeVkStruct <$> (fromCStructCommandBufferAllocateInfo =<< peek (castPtr p :: Ptr VkCommandBufferAllocateInfo))
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO -> SomeVkStruct <$> (fromCStructCommandBufferInheritanceInfo =<< peek (castPtr p :: Ptr VkCommandBufferInheritanceInfo))
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO -> SomeVkStruct <$> (fromCStructCommandBufferBeginInfo =<< peek (castPtr p :: Ptr VkCommandBufferBeginInfo))
    -- We are not able to marshal this type back into Haskell as we don't know which union component to use
    VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO) Nothing Nothing)
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO -> SomeVkStruct <$> (fromCStructRenderPassCreateInfo =<< peek (castPtr p :: Ptr VkRenderPassCreateInfo))
    VK_STRUCTURE_TYPE_EVENT_CREATE_INFO -> SomeVkStruct <$> (fromCStructEventCreateInfo =<< peek (castPtr p :: Ptr VkEventCreateInfo))
    VK_STRUCTURE_TYPE_FENCE_CREATE_INFO -> SomeVkStruct <$> (fromCStructFenceCreateInfo =<< peek (castPtr p :: Ptr VkFenceCreateInfo))
    VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO -> SomeVkStruct <$> (fromCStructSemaphoreCreateInfo =<< peek (castPtr p :: Ptr VkSemaphoreCreateInfo))
    VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO -> SomeVkStruct <$> (fromCStructQueryPoolCreateInfo =<< peek (castPtr p :: Ptr VkQueryPoolCreateInfo))
    VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO -> SomeVkStruct <$> (fromCStructFramebufferCreateInfo =<< peek (castPtr p :: Ptr VkFramebufferCreateInfo))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_SUBMIT_INFO -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_SUBMIT_INFO) Nothing Nothing)
    VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructDisplayModeCreateInfoKHR =<< peek (castPtr p :: Ptr VkDisplayModeCreateInfoKHR))
    VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructDisplaySurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkDisplaySurfaceCreateInfoKHR))
    VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR -> SomeVkStruct <$> (fromCStructDisplayPresentInfoKHR =<< peek (castPtr p :: Ptr VkDisplayPresentInfoKHR))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructAndroidSurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkAndroidSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_MIR_KHR)
    VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructMirSurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkMirSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_VI_NN)
    VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN -> SomeVkStruct <$> (fromCStructViSurfaceCreateInfoNN =<< peek (castPtr p :: Ptr VkViSurfaceCreateInfoNN))
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
    VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructWaylandSurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkWaylandSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructWin32SurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkWin32SurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
    VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructXlibSurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkXlibSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
    VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructXcbSurfaceCreateInfoKHR =<< peek (castPtr p :: Ptr VkXcbSurfaceCreateInfoKHR))
#endif
    VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructSwapchainCreateInfoKHR =<< peek (castPtr p :: Ptr VkSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_PRESENT_INFO_KHR -> SomeVkStruct <$> (fromCStructPresentInfoKHR =<< peek (castPtr p :: Ptr VkPresentInfoKHR))
    VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugReportCallbackCreateInfoEXT =<< peek (castPtr p :: Ptr VkDebugReportCallbackCreateInfoEXT))
    VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT -> SomeVkStruct <$> (fromCStructValidationFlagsEXT =<< peek (castPtr p :: Ptr VkValidationFlagsEXT))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD -> SomeVkStruct <$> (fromCStructPipelineRasterizationStateRasterizationOrderAMD =<< peek (castPtr p :: Ptr VkPipelineRasterizationStateRasterizationOrderAMD))
    VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugMarkerObjectNameInfoEXT =<< peek (castPtr p :: Ptr VkDebugMarkerObjectNameInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugMarkerObjectTagInfoEXT =<< peek (castPtr p :: Ptr VkDebugMarkerObjectTagInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugMarkerMarkerInfoEXT =<< peek (castPtr p :: Ptr VkDebugMarkerMarkerInfoEXT))
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructDedicatedAllocationImageCreateInfoNV =<< peek (castPtr p :: Ptr VkDedicatedAllocationImageCreateInfoNV))
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructDedicatedAllocationBufferCreateInfoNV =<< peek (castPtr p :: Ptr VkDedicatedAllocationBufferCreateInfoNV))
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV -> SomeVkStruct <$> (fromCStructDedicatedAllocationMemoryAllocateInfoNV =<< peek (castPtr p :: Ptr VkDedicatedAllocationMemoryAllocateInfoNV))
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructExternalMemoryImageCreateInfoNV =<< peek (castPtr p :: Ptr VkExternalMemoryImageCreateInfoNV))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV -> SomeVkStruct <$> (fromCStructExportMemoryAllocateInfoNV =<< peek (castPtr p :: Ptr VkExportMemoryAllocateInfoNV))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV -> SomeVkStruct <$> (fromCStructImportMemoryWin32HandleInfoNV =<< peek (castPtr p :: Ptr VkImportMemoryWin32HandleInfoNV))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV -> SomeVkStruct <$> (fromCStructExportMemoryWin32HandleInfoNV =<< peek (castPtr p :: Ptr VkExportMemoryWin32HandleInfoNV))
    VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV -> SomeVkStruct <$> (fromCStructWin32KeyedMutexAcquireReleaseInfoNV =<< peek (castPtr p :: Ptr VkWin32KeyedMutexAcquireReleaseInfoNV))
#endif
    VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX -> SomeVkStruct <$> (fromCStructDeviceGeneratedCommandsFeaturesNVX =<< peek (castPtr p :: Ptr VkDeviceGeneratedCommandsFeaturesNVX))
    VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX -> SomeVkStruct <$> (fromCStructDeviceGeneratedCommandsLimitsNVX =<< peek (castPtr p :: Ptr VkDeviceGeneratedCommandsLimitsNVX))
    VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX -> SomeVkStruct <$> (fromCStructIndirectCommandsLayoutCreateInfoNVX =<< peek (castPtr p :: Ptr VkIndirectCommandsLayoutCreateInfoNVX))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX) Nothing Nothing)
    VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX -> SomeVkStruct <$> (fromCStructCmdReserveSpaceForCommandsInfoNVX =<< peek (castPtr p :: Ptr VkCmdReserveSpaceForCommandsInfoNVX))
    VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX -> SomeVkStruct <$> (fromCStructObjectTableCreateInfoNVX =<< peek (castPtr p :: Ptr VkObjectTableCreateInfoNVX))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 -> SomeVkStruct <$> (fromCStructPhysicalDeviceFeatures2 =<< peek (castPtr p :: Ptr VkPhysicalDeviceFeatures2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 -> SomeVkStruct <$> (fromCStructPhysicalDeviceProperties2 =<< peek (castPtr p :: Ptr VkPhysicalDeviceProperties2))
    VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 -> SomeVkStruct <$> (fromCStructFormatProperties2 =<< peek (castPtr p :: Ptr VkFormatProperties2))
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 -> SomeVkStruct <$> (fromCStructImageFormatProperties2 =<< peek (castPtr p :: Ptr VkImageFormatProperties2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 -> SomeVkStruct <$> (fromCStructPhysicalDeviceImageFormatInfo2 =<< peek (castPtr p :: Ptr VkPhysicalDeviceImageFormatInfo2))
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 -> SomeVkStruct <$> (fromCStructQueueFamilyProperties2 =<< peek (castPtr p :: Ptr VkQueueFamilyProperties2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 -> SomeVkStruct <$> (fromCStructPhysicalDeviceMemoryProperties2 =<< peek (castPtr p :: Ptr VkPhysicalDeviceMemoryProperties2))
    VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 -> SomeVkStruct <$> (fromCStructSparseImageFormatProperties2 =<< peek (castPtr p :: Ptr VkSparseImageFormatProperties2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 -> SomeVkStruct <$> (fromCStructPhysicalDeviceSparseImageFormatInfo2 =<< peek (castPtr p :: Ptr VkPhysicalDeviceSparseImageFormatInfo2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDevicePushDescriptorPropertiesKHR =<< peek (castPtr p :: Ptr VkPhysicalDevicePushDescriptorPropertiesKHR))
    VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR -> SomeVkStruct <$> (fromCStructPresentRegionsKHR =<< peek (castPtr p :: Ptr VkPresentRegionsKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceVariablePointerFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceVariablePointerFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO -> SomeVkStruct <$> (fromCStructPhysicalDeviceExternalImageFormatInfo =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalImageFormatInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES -> SomeVkStruct <$> (fromCStructExternalImageFormatProperties =<< peek (castPtr p :: Ptr VkExternalImageFormatProperties))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO -> SomeVkStruct <$> (fromCStructPhysicalDeviceExternalBufferInfo =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalBufferInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES -> SomeVkStruct <$> (fromCStructExternalBufferProperties =<< peek (castPtr p :: Ptr VkExternalBufferProperties))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDeviceIDProperties =<< peek (castPtr p :: Ptr VkPhysicalDeviceIDProperties))
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO -> SomeVkStruct <$> (fromCStructExternalMemoryImageCreateInfo =<< peek (castPtr p :: Ptr VkExternalMemoryImageCreateInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO -> SomeVkStruct <$> (fromCStructExternalMemoryBufferCreateInfo =<< peek (castPtr p :: Ptr VkExternalMemoryBufferCreateInfo))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO -> SomeVkStruct <$> (fromCStructExportMemoryAllocateInfo =<< peek (castPtr p :: Ptr VkExportMemoryAllocateInfo))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructImportMemoryWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkImportMemoryWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructExportMemoryWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkExportMemoryWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR -> SomeVkStruct <$> (fromCStructMemoryWin32HandlePropertiesKHR =<< peek (castPtr p :: Ptr VkMemoryWin32HandlePropertiesKHR))
    VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructMemoryGetWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkMemoryGetWin32HandleInfoKHR))
#endif
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR -> SomeVkStruct <$> (fromCStructImportMemoryFdInfoKHR =<< peek (castPtr p :: Ptr VkImportMemoryFdInfoKHR))
    VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR -> SomeVkStruct <$> (fromCStructMemoryFdPropertiesKHR =<< peek (castPtr p :: Ptr VkMemoryFdPropertiesKHR))
    VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR -> SomeVkStruct <$> (fromCStructMemoryGetFdInfoKHR =<< peek (castPtr p :: Ptr VkMemoryGetFdInfoKHR))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR -> SomeVkStruct <$> (fromCStructWin32KeyedMutexAcquireReleaseInfoKHR =<< peek (castPtr p :: Ptr VkWin32KeyedMutexAcquireReleaseInfoKHR))
#endif
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO -> SomeVkStruct <$> (fromCStructPhysicalDeviceExternalSemaphoreInfo =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalSemaphoreInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES -> SomeVkStruct <$> (fromCStructExternalSemaphoreProperties =<< peek (castPtr p :: Ptr VkExternalSemaphoreProperties))
    VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO -> SomeVkStruct <$> (fromCStructExportSemaphoreCreateInfo =<< peek (castPtr p :: Ptr VkExportSemaphoreCreateInfo))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructImportSemaphoreWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkImportSemaphoreWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructExportSemaphoreWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkExportSemaphoreWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR -> SomeVkStruct <$> (fromCStructD3D12FenceSubmitInfoKHR =<< peek (castPtr p :: Ptr VkD3D12FenceSubmitInfoKHR))
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructSemaphoreGetWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkSemaphoreGetWin32HandleInfoKHR))
#endif
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR -> SomeVkStruct <$> (fromCStructImportSemaphoreFdInfoKHR =<< peek (castPtr p :: Ptr VkImportSemaphoreFdInfoKHR))
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR -> SomeVkStruct <$> (fromCStructSemaphoreGetFdInfoKHR =<< peek (castPtr p :: Ptr VkSemaphoreGetFdInfoKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO -> SomeVkStruct <$> (fromCStructPhysicalDeviceExternalFenceInfo =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalFenceInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES -> SomeVkStruct <$> (fromCStructExternalFenceProperties =<< peek (castPtr p :: Ptr VkExternalFenceProperties))
    VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO -> SomeVkStruct <$> (fromCStructExportFenceCreateInfo =<< peek (castPtr p :: Ptr VkExportFenceCreateInfo))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructImportFenceWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkImportFenceWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructExportFenceWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkExportFenceWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR -> SomeVkStruct <$> (fromCStructFenceGetWin32HandleInfoKHR =<< peek (castPtr p :: Ptr VkFenceGetWin32HandleInfoKHR))
#endif
    VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR -> SomeVkStruct <$> (fromCStructImportFenceFdInfoKHR =<< peek (castPtr p :: Ptr VkImportFenceFdInfoKHR))
    VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR -> SomeVkStruct <$> (fromCStructFenceGetFdInfoKHR =<< peek (castPtr p :: Ptr VkFenceGetFdInfoKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceMultiviewFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceMultiviewFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDeviceMultiviewProperties =<< peek (castPtr p :: Ptr VkPhysicalDeviceMultiviewProperties))
    VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO -> SomeVkStruct <$> (fromCStructRenderPassMultiviewCreateInfo =<< peek (castPtr p :: Ptr VkRenderPassMultiviewCreateInfo))
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT -> SomeVkStruct <$> (fromCStructSurfaceCapabilities2EXT =<< peek (castPtr p :: Ptr VkSurfaceCapabilities2EXT))
    VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT -> SomeVkStruct <$> (fromCStructDisplayPowerInfoEXT =<< peek (castPtr p :: Ptr VkDisplayPowerInfoEXT))
    VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT -> SomeVkStruct <$> (fromCStructDeviceEventInfoEXT =<< peek (castPtr p :: Ptr VkDeviceEventInfoEXT))
    VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT -> SomeVkStruct <$> (fromCStructDisplayEventInfoEXT =<< peek (castPtr p :: Ptr VkDisplayEventInfoEXT))
    VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructSwapchainCounterCreateInfoEXT =<< peek (castPtr p :: Ptr VkSwapchainCounterCreateInfoEXT))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES) Nothing Nothing)
    VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO -> SomeVkStruct <$> (fromCStructMemoryAllocateFlagsInfo =<< peek (castPtr p :: Ptr VkMemoryAllocateFlagsInfo))
    VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO -> SomeVkStruct <$> (fromCStructBindBufferMemoryInfo =<< peek (castPtr p :: Ptr VkBindBufferMemoryInfo))
    VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO -> SomeVkStruct <$> (fromCStructBindBufferMemoryDeviceGroupInfo =<< peek (castPtr p :: Ptr VkBindBufferMemoryDeviceGroupInfo))
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO -> SomeVkStruct <$> (fromCStructBindImageMemoryInfo =<< peek (castPtr p :: Ptr VkBindImageMemoryInfo))
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO -> SomeVkStruct <$> (fromCStructBindImageMemoryDeviceGroupInfo =<< peek (castPtr p :: Ptr VkBindImageMemoryDeviceGroupInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO -> SomeVkStruct <$> (fromCStructDeviceGroupRenderPassBeginInfo =<< peek (castPtr p :: Ptr VkDeviceGroupRenderPassBeginInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO -> SomeVkStruct <$> (fromCStructDeviceGroupCommandBufferBeginInfo =<< peek (castPtr p :: Ptr VkDeviceGroupCommandBufferBeginInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO -> SomeVkStruct <$> (fromCStructDeviceGroupSubmitInfo =<< peek (castPtr p :: Ptr VkDeviceGroupSubmitInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO -> SomeVkStruct <$> (fromCStructDeviceGroupBindSparseInfo =<< peek (castPtr p :: Ptr VkDeviceGroupBindSparseInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR -> SomeVkStruct <$> (fromCStructDeviceGroupPresentCapabilitiesKHR =<< peek (castPtr p :: Ptr VkDeviceGroupPresentCapabilitiesKHR))
    VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructImageSwapchainCreateInfoKHR =<< peek (castPtr p :: Ptr VkImageSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR -> SomeVkStruct <$> (fromCStructBindImageMemorySwapchainInfoKHR =<< peek (castPtr p :: Ptr VkBindImageMemorySwapchainInfoKHR))
    VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR -> SomeVkStruct <$> (fromCStructAcquireNextImageInfoKHR =<< peek (castPtr p :: Ptr VkAcquireNextImageInfoKHR))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR -> SomeVkStruct <$> (fromCStructDeviceGroupPresentInfoKHR =<< peek (castPtr p :: Ptr VkDeviceGroupPresentInfoKHR))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO) Nothing Nothing)
    VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructDeviceGroupSwapchainCreateInfoKHR =<< peek (castPtr p :: Ptr VkDeviceGroupSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructDescriptorUpdateTemplateCreateInfo =<< peek (castPtr p :: Ptr VkDescriptorUpdateTemplateCreateInfo))
    VK_STRUCTURE_TYPE_HDR_METADATA_EXT -> SomeVkStruct <$> (fromCStructHdrMetadataEXT =<< peek (castPtr p :: Ptr VkHdrMetadataEXT))
    VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE -> SomeVkStruct <$> (fromCStructPresentTimesInfoGOOGLE =<< peek (castPtr p :: Ptr VkPresentTimesInfoGOOGLE))
#if defined(VK_USE_PLATFORM_IOS_MVK)
    VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK -> SomeVkStruct <$> (fromCStructIOSSurfaceCreateInfoMVK =<< peek (castPtr p :: Ptr VkIOSSurfaceCreateInfoMVK))
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
    VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK -> SomeVkStruct <$> (fromCStructMacOSSurfaceCreateInfoMVK =<< peek (castPtr p :: Ptr VkMacOSSurfaceCreateInfoMVK))
#endif
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineViewportWScalingStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineViewportWScalingStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineViewportSwizzleStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineViewportSwizzleStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceDiscardRectanglePropertiesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineDiscardRectangleStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineDiscardRectangleStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX -> SomeVkStruct <$> (fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX =<< peek (castPtr p :: Ptr VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
    VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO -> SomeVkStruct <$> (fromCStructRenderPassInputAttachmentAspectCreateInfo =<< peek (castPtr p :: Ptr VkRenderPassInputAttachmentAspectCreateInfo))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceSurfaceInfo2KHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceSurfaceInfo2KHR))
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR -> SomeVkStruct <$> (fromCStructSurfaceCapabilities2KHR =<< peek (castPtr p :: Ptr VkSurfaceCapabilities2KHR))
    VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR -> SomeVkStruct <$> (fromCStructSurfaceFormat2KHR =<< peek (castPtr p :: Ptr VkSurfaceFormat2KHR))
    VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR -> SomeVkStruct <$> (fromCStructSharedPresentSurfaceCapabilitiesKHR =<< peek (castPtr p :: Ptr VkSharedPresentSurfaceCapabilitiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDevice16BitStorageFeatures =<< peek (castPtr p :: Ptr VkPhysicalDevice16BitStorageFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDeviceSubgroupProperties =<< peek (castPtr p :: Ptr VkPhysicalDeviceSubgroupProperties))
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 -> SomeVkStruct <$> (fromCStructBufferMemoryRequirementsInfo2 =<< peek (castPtr p :: Ptr VkBufferMemoryRequirementsInfo2))
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 -> SomeVkStruct <$> (fromCStructImageMemoryRequirementsInfo2 =<< peek (castPtr p :: Ptr VkImageMemoryRequirementsInfo2))
    VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 -> SomeVkStruct <$> (fromCStructImageSparseMemoryRequirementsInfo2 =<< peek (castPtr p :: Ptr VkImageSparseMemoryRequirementsInfo2))
    VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 -> SomeVkStruct <$> (fromCStructMemoryRequirements2 =<< peek (castPtr p :: Ptr VkMemoryRequirements2))
    VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 -> SomeVkStruct <$> (fromCStructSparseImageMemoryRequirements2 =<< peek (castPtr p :: Ptr VkSparseImageMemoryRequirements2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDevicePointClippingProperties =<< peek (castPtr p :: Ptr VkPhysicalDevicePointClippingProperties))
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS -> SomeVkStruct <$> (fromCStructMemoryDedicatedRequirements =<< peek (castPtr p :: Ptr VkMemoryDedicatedRequirements))
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO -> SomeVkStruct <$> (fromCStructMemoryDedicatedAllocateInfo =<< peek (castPtr p :: Ptr VkMemoryDedicatedAllocateInfo))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO -> SomeVkStruct <$> (fromCStructImageViewUsageCreateInfo =<< peek (castPtr p :: Ptr VkImageViewUsageCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO -> SomeVkStruct <$> (fromCStructPipelineTessellationDomainOriginStateCreateInfo =<< peek (castPtr p :: Ptr VkPipelineTessellationDomainOriginStateCreateInfo))
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO -> SomeVkStruct <$> (fromCStructSamplerYcbcrConversionInfo =<< peek (castPtr p :: Ptr VkSamplerYcbcrConversionInfo))
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO -> SomeVkStruct <$> (fromCStructSamplerYcbcrConversionCreateInfo =<< peek (castPtr p :: Ptr VkSamplerYcbcrConversionCreateInfo))
    VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO -> SomeVkStruct <$> (fromCStructBindImagePlaneMemoryInfo =<< peek (castPtr p :: Ptr VkBindImagePlaneMemoryInfo))
    VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO -> SomeVkStruct <$> (fromCStructImagePlaneMemoryRequirementsInfo =<< peek (castPtr p :: Ptr VkImagePlaneMemoryRequirementsInfo))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceSamplerYcbcrConversionFeatures))
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES -> SomeVkStruct <$> (fromCStructSamplerYcbcrConversionImageFormatProperties =<< peek (castPtr p :: Ptr VkSamplerYcbcrConversionImageFormatProperties))
    VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD -> SomeVkStruct <$> (fromCStructTextureLODGatherFormatPropertiesAMD =<< peek (castPtr p :: Ptr VkTextureLODGatherFormatPropertiesAMD))
    VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO -> SomeVkStruct <$> (fromCStructProtectedSubmitInfo =<< peek (castPtr p :: Ptr VkProtectedSubmitInfo))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceProtectedMemoryFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceProtectedMemoryFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDeviceProtectedMemoryProperties =<< peek (castPtr p :: Ptr VkPhysicalDeviceProtectedMemoryProperties))
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 -> SomeVkStruct <$> (fromCStructDeviceQueueInfo2 =<< peek (castPtr p :: Ptr VkDeviceQueueInfo2))
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineCoverageToColorStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineCoverageToColorStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
    VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT -> SomeVkStruct <$> (fromCStructSampleLocationsInfoEXT =<< peek (castPtr p :: Ptr VkSampleLocationsInfoEXT))
    VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT -> SomeVkStruct <$> (fromCStructRenderPassSampleLocationsBeginInfoEXT =<< peek (castPtr p :: Ptr VkRenderPassSampleLocationsBeginInfoEXT))
    VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineSampleLocationsStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineSampleLocationsStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceSampleLocationsPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceSampleLocationsPropertiesEXT))
    VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructMultisamplePropertiesEXT =<< peek (castPtr p :: Ptr VkMultisamplePropertiesEXT))
    VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructSamplerReductionModeCreateInfoEXT =<< peek (castPtr p :: Ptr VkSamplerReductionModeCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineColorBlendAdvancedStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineCoverageModulationStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineCoverageModulationStateCreateInfoNV))
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructImageFormatListCreateInfoKHR =<< peek (castPtr p :: Ptr VkImageFormatListCreateInfoKHR))
    VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructValidationCacheCreateInfoEXT =<< peek (castPtr p :: Ptr VkValidationCacheCreateInfoEXT))
    VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructShaderModuleValidationCacheCreateInfoEXT =<< peek (castPtr p :: Ptr VkShaderModuleValidationCacheCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDeviceMaintenance3Properties =<< peek (castPtr p :: Ptr VkPhysicalDeviceMaintenance3Properties))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT -> SomeVkStruct <$> (fromCStructDescriptorSetLayoutSupport =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutSupport))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceShaderDrawParameterFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderDrawParameterFeatures))
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDeviceQueueGlobalPriorityCreateInfoEXT =<< peek (castPtr p :: Ptr VkDeviceQueueGlobalPriorityCreateInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugUtilsObjectNameInfoEXT =<< peek (castPtr p :: Ptr VkDebugUtilsObjectNameInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugUtilsObjectTagInfoEXT =<< peek (castPtr p :: Ptr VkDebugUtilsObjectTagInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT -> SomeVkStruct <$> (fromCStructDebugUtilsLabelEXT =<< peek (castPtr p :: Ptr VkDebugUtilsLabelEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugUtilsMessengerCreateInfoEXT =<< peek (castPtr p :: Ptr VkDebugUtilsMessengerCreateInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT -> SomeVkStruct <$> (fromCStructDebugUtilsMessengerCallbackDataEXT =<< peek (castPtr p :: Ptr VkDebugUtilsMessengerCallbackDataEXT))
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT -> SomeVkStruct <$> (fromCStructImportMemoryHostPointerInfoEXT =<< peek (castPtr p :: Ptr VkImportMemoryHostPointerInfoEXT))
    VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructMemoryHostPointerPropertiesEXT =<< peek (castPtr p :: Ptr VkMemoryHostPointerPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD -> SomeVkStruct <$> (fromCStructPhysicalDeviceShaderCorePropertiesAMD =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderCorePropertiesAMD))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineRasterizationConservativeStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineRasterizationConservativeStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT =<< peek (castPtr p :: Ptr VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT -> SomeVkStruct <$> (fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT =<< peek (castPtr p :: Ptr VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineVertexInputDivisorStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineVertexInputDivisorStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> SomeVkStruct <$> (fromCStructImportAndroidHardwareBufferInfoANDROID =<< peek (castPtr p :: Ptr VkImportAndroidHardwareBufferInfoANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID -> SomeVkStruct <$> (fromCStructAndroidHardwareBufferUsageANDROID =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferUsageANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID -> SomeVkStruct <$> (fromCStructAndroidHardwareBufferPropertiesANDROID =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferPropertiesANDROID))
    VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> SomeVkStruct <$> (fromCStructMemoryGetAndroidHardwareBufferInfoANDROID =<< peek (castPtr p :: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID -> SomeVkStruct <$> (fromCStructAndroidHardwareBufferFormatPropertiesANDROID =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferFormatPropertiesANDROID))
    VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID -> SomeVkStruct <$> (fromCStructExternalFormatANDROID =<< peek (castPtr p :: Ptr VkExternalFormatANDROID))
#endif
    t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)
