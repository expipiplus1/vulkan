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
  , HasNext(..)
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
  ( VkPhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , VkSwapchainDisplayNativeHdrCreateInfoAMD(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  )
import Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkDeviceMemoryOverallocationCreateInfoAMD(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
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
  ( pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID
  , pattern VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID
  , pattern VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( VkAndroidHardwareBufferFormatPropertiesANDROID(..)
  , VkAndroidHardwareBufferPropertiesANDROID(..)
  , VkAndroidHardwareBufferUsageANDROID(..)
  , VkExternalFormatANDROID(..)
  , VkImportAndroidHardwareBufferInfoANDROID(..)
  , VkMemoryGetAndroidHardwareBufferInfoANDROID(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( VkImageViewASTCDecodeModeEXT(..)
  , VkPhysicalDeviceASTCDecodeFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable
  ( VkPhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , VkPipelineRasterizationDepthClipStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( VkFilterCubicImageViewImageFormatPropertiesEXT(..)
  , VkPhysicalDeviceImageViewImageFormatInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesEXT(..)
  , VkDrmFormatModifierPropertiesListEXT(..)
  , VkImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , VkImageDrmFormatModifierListCreateInfoEXT(..)
  , VkImageDrmFormatModifierPropertiesEXT(..)
  , VkPhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_inline_uniform_block
  ( VkDescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , VkPhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , VkPhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , VkWriteDescriptorSetInlineUniformBlockEXT(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( VkMemoryPriorityAllocateInfoEXT(..)
  , VkPhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  )

#if defined(VK_USE_PLATFORM_METAL_EXT)
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_METAL_EXT)
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateInfoEXT(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( VkPhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , VkPipelineCreationFeedbackEXT(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
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
import Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( VkPhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( VkImageStencilUsageCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , VkPipelineRasterizationStateStreamCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VkVertexInputBindingDivisorDescriptionEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( VkPhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  )

#if defined(VK_USE_PLATFORM_FUCHSIA)
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_FUCHSIA)
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( VkPresentFrameTokenGGP(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateInfoGGP(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPastPresentationTimingGOOGLE(..)
  , VkPresentTimeGOOGLE(..)
  , VkPresentTimesInfoGOOGLE(..)
  , VkRefreshCycleDurationGOOGLE(..)
  , pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( VkPhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_create_renderpass2
  ( VkAttachmentDescription2KHR(..)
  , VkAttachmentReference2KHR(..)
  , VkRenderPassCreateInfo2KHR(..)
  , VkSubpassBeginInfoKHR(..)
  , VkSubpassDependency2KHR(..)
  , VkSubpassDescription2KHR(..)
  , VkSubpassEndInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkPhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , VkSubpassDescriptionDepthStencilResolveKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  )
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
import Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkConformanceVersionKHR(..)
  , VkPhysicalDeviceDriverPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
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
  ( pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_win32
  ( VkExportMemoryWin32HandleInfoKHR(..)
  , VkImportMemoryWin32HandleInfoKHR(..)
  , VkMemoryGetWin32HandleInfoKHR(..)
  , VkMemoryWin32HandlePropertiesKHR(..)
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
  ( pattern VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_win32
  ( VkD3D12FenceSubmitInfoKHR(..)
  , VkExportSemaphoreWin32HandleInfoKHR(..)
  , VkImportSemaphoreWin32HandleInfoKHR(..)
  , VkSemaphoreGetWin32HandleInfoKHR(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_KHR_get_display_properties2
  ( VkDisplayModeProperties2KHR(..)
  , VkDisplayPlaneCapabilities2KHR(..)
  , VkDisplayPlaneInfo2KHR(..)
  , VkDisplayPlaneProperties2KHR(..)
  , VkDisplayProperties2KHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR
  )
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
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( VkPhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( VkPhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface
  ( VkSurfaceCapabilitiesKHR(..)
  , VkSurfaceFormatKHR(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( VkSurfaceProtectedCapabilitiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
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
import Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( VkPhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  )

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( VkXlibSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN(..)
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
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
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
import Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( VkPhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkCooperativeMatrixPropertiesNV(..)
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( VkPhysicalDeviceCornerSampledImageFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , VkQueueFamilyCheckpointPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
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
  ( pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric
  ( VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkPipelineCoverageModulationStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkDrawMeshTasksIndirectCommandNV(..)
  , VkPhysicalDeviceMeshShaderFeaturesNV(..)
  , VkPhysicalDeviceMeshShaderPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV(..)
  , VkAccelerationStructureInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsInfoNV(..)
  , VkBindAccelerationStructureMemoryInfoNV(..)
  , VkGeometryAABBNV(..)
  , VkGeometryDataNV(..)
  , VkGeometryNV(..)
  , VkGeometryTrianglesNV(..)
  , VkPhysicalDeviceRayTracingPropertiesNV(..)
  , VkRayTracingPipelineCreateInfoNV(..)
  , VkRayTracingShaderGroupCreateInfoNV(..)
  , VkWriteDescriptorSetAccelerationStructureNV(..)
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV
  , pattern VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_NV
  , pattern VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , VkPipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( VkPhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkCoarseSampleLocationNV(..)
  , VkCoarseSampleOrderCustomNV(..)
  , VkPhysicalDeviceShadingRateImageFeaturesNV(..)
  , VkPhysicalDeviceShadingRateImagePropertiesNV(..)
  , VkPipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , VkPipelineViewportShadingRateImageStateCreateInfoNV(..)
  , VkShadingRatePaletteNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  )
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateInfoNV(..)
  , VkViewportSwizzleNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  )

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
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
  ( PhysicalDeviceShaderDrawParametersFeatures(..)
  , fromCStructPhysicalDeviceShaderDrawParametersFeatures
  , withCStructPhysicalDeviceShaderDrawParametersFeatures
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointersFeatures(..)
  , fromCStructPhysicalDeviceVariablePointersFeatures
  , withCStructPhysicalDeviceVariablePointersFeatures
  )
import Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr
  ( DisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , SwapchainDisplayNativeHdrCreateInfoAMD(..)
  , fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD
  , fromCStructSwapchainDisplayNativeHdrCreateInfoAMD
  , withCStructDisplayNativeHdrSurfaceCapabilitiesAMD
  , withCStructSwapchainDisplayNativeHdrCreateInfoAMD
  )
import Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( DeviceMemoryOverallocationCreateInfoAMD(..)
  , fromCStructDeviceMemoryOverallocationCreateInfoAMD
  , withCStructDeviceMemoryOverallocationCreateInfoAMD
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
  ( withCStructAndroidHardwareBufferFormatPropertiesANDROID
  , withCStructAndroidHardwareBufferPropertiesANDROID
  , withCStructAndroidHardwareBufferUsageANDROID
  , withCStructExternalFormatANDROID
  , withCStructImportAndroidHardwareBufferInfoANDROID
  , withCStructMemoryGetAndroidHardwareBufferInfoANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( fromCStructAndroidHardwareBufferFormatPropertiesANDROID
  , fromCStructAndroidHardwareBufferPropertiesANDROID
  , fromCStructAndroidHardwareBufferUsageANDROID
  , fromCStructExternalFormatANDROID
  , fromCStructImportAndroidHardwareBufferInfoANDROID
  , fromCStructMemoryGetAndroidHardwareBufferInfoANDROID
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AndroidHardwareBufferFormatPropertiesANDROID(..)
  , AndroidHardwareBufferPropertiesANDROID(..)
  , AndroidHardwareBufferUsageANDROID(..)
  , ExternalFormatANDROID(..)
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode
  ( ImageViewASTCDecodeModeEXT(..)
  , PhysicalDeviceASTCDecodeFeaturesEXT(..)
  , fromCStructImageViewASTCDecodeModeEXT
  , fromCStructPhysicalDeviceASTCDecodeFeaturesEXT
  , withCStructImageViewASTCDecodeModeEXT
  , withCStructPhysicalDeviceASTCDecodeFeaturesEXT
  )
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
import Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( BufferDeviceAddressCreateInfoEXT(..)
  , BufferDeviceAddressInfoEXT(..)
  , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , fromCStructBufferDeviceAddressCreateInfoEXT
  , fromCStructBufferDeviceAddressInfoEXT
  , fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
  , withCStructBufferDeviceAddressCreateInfoEXT
  , withCStructBufferDeviceAddressInfoEXT
  , withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( CalibratedTimestampInfoEXT(..)
  , fromCStructCalibratedTimestampInfoEXT
  , withCStructCalibratedTimestampInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( CommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , ConditionalRenderingBeginInfoEXT(..)
  , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT
  , fromCStructConditionalRenderingBeginInfoEXT
  , fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT
  , withCStructCommandBufferInheritanceConditionalRenderingInfoEXT
  , withCStructConditionalRenderingBeginInfoEXT
  , withCStructPhysicalDeviceConditionalRenderingFeaturesEXT
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
import Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable
  ( PhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
  , fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT
  , fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT
  , withCStructPhysicalDeviceDepthClipEnableFeaturesEXT
  , withCStructPipelineRasterizationDepthClipStateCreateInfoEXT
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
import Graphics.Vulkan.Extensions.VK_EXT_filter_cubic
  ( FilterCubicImageViewImageFormatPropertiesEXT(..)
  , PhysicalDeviceImageViewImageFormatInfoEXT(..)
  , fromCStructFilterCubicImageViewImageFormatPropertiesEXT
  , fromCStructPhysicalDeviceImageViewImageFormatInfoEXT
  , withCStructFilterCubicImageViewImageFormatPropertiesEXT
  , withCStructPhysicalDeviceImageViewImageFormatInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , RenderPassFragmentDensityMapCreateInfoEXT(..)
  , fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
  , fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
  , fromCStructRenderPassFragmentDensityMapCreateInfoEXT
  , withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
  , withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
  , withCStructRenderPassFragmentDensityMapCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( SurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , SurfaceFullScreenExclusiveInfoEXT(..)
  , SurfaceFullScreenExclusiveWin32InfoEXT(..)
  , fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT
  , fromCStructSurfaceFullScreenExclusiveInfoEXT
  , fromCStructSurfaceFullScreenExclusiveWin32InfoEXT
  , withCStructSurfaceCapabilitiesFullScreenExclusiveEXT
  , withCStructSurfaceFullScreenExclusiveInfoEXT
  , withCStructSurfaceFullScreenExclusiveWin32InfoEXT
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
import Graphics.Vulkan.Extensions.VK_EXT_headless_surface
  ( HeadlessSurfaceCreateInfoEXT(..)
  , fromCStructHeadlessSurfaceCreateInfoEXT
  , withCStructHeadlessSurfaceCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_host_query_reset
  ( PhysicalDeviceHostQueryResetFeaturesEXT(..)
  , fromCStructPhysicalDeviceHostQueryResetFeaturesEXT
  , withCStructPhysicalDeviceHostQueryResetFeaturesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier
  ( DrmFormatModifierPropertiesEXT(..)
  , DrmFormatModifierPropertiesListEXT(..)
  , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , ImageDrmFormatModifierListCreateInfoEXT(..)
  , ImageDrmFormatModifierPropertiesEXT(..)
  , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  , fromCStructDrmFormatModifierPropertiesEXT
  , fromCStructDrmFormatModifierPropertiesListEXT
  , fromCStructImageDrmFormatModifierExplicitCreateInfoEXT
  , fromCStructImageDrmFormatModifierListCreateInfoEXT
  , fromCStructImageDrmFormatModifierPropertiesEXT
  , fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT
  , withCStructDrmFormatModifierPropertiesEXT
  , withCStructDrmFormatModifierPropertiesListEXT
  , withCStructImageDrmFormatModifierExplicitCreateInfoEXT
  , withCStructImageDrmFormatModifierListCreateInfoEXT
  , withCStructImageDrmFormatModifierPropertiesEXT
  , withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block
  ( DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , WriteDescriptorSetInlineUniformBlockEXT(..)
  , fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT
  , fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT
  , fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT
  , fromCStructWriteDescriptorSetInlineUniformBlockEXT
  , withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT
  , withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT
  , withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT
  , withCStructWriteDescriptorSetInlineUniformBlockEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_memory_budget
  ( PhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT
  , withCStructPhysicalDeviceMemoryBudgetPropertiesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_memory_priority
  ( MemoryPriorityAllocateInfoEXT(..)
  , PhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , fromCStructMemoryPriorityAllocateInfoEXT
  , fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT
  , withCStructMemoryPriorityAllocateInfoEXT
  , withCStructPhysicalDeviceMemoryPriorityFeaturesEXT
  )

#if defined(VK_USE_PLATFORM_METAL_EXT)
import Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( withCStructMetalSurfaceCreateInfoEXT
  )
#endif

#if defined(VK_USE_PLATFORM_METAL_EXT)
import Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( fromCStructMetalSurfaceCreateInfoEXT
  )
#endif

#if defined(VK_USE_PLATFORM_METAL_EXT)
import Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( MetalSurfaceCreateInfoEXT(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_pci_bus_info
  ( PhysicalDevicePCIBusInfoPropertiesEXT(..)
  , fromCStructPhysicalDevicePCIBusInfoPropertiesEXT
  , withCStructPhysicalDevicePCIBusInfoPropertiesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( PipelineCreationFeedbackCreateInfoEXT(..)
  , PipelineCreationFeedbackEXT(..)
  , fromCStructPipelineCreationFeedbackCreateInfoEXT
  , fromCStructPipelineCreationFeedbackEXT
  , withCStructPipelineCreationFeedbackCreateInfoEXT
  , withCStructPipelineCreationFeedbackEXT
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
import Graphics.Vulkan.Extensions.VK_EXT_scalar_block_layout
  ( PhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT
  , withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_separate_stencil_usage
  ( ImageStencilUsageCreateInfoEXT(..)
  , fromCStructImageStencilUsageCreateInfoEXT
  , withCStructImageStencilUsageCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_transform_feedback
  ( PhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , PipelineRasterizationStateStreamCreateInfoEXT(..)
  , fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT
  , fromCStructPhysicalDeviceTransformFeedbackPropertiesEXT
  , fromCStructPipelineRasterizationStateStreamCreateInfoEXT
  , withCStructPhysicalDeviceTransformFeedbackFeaturesEXT
  , withCStructPhysicalDeviceTransformFeedbackPropertiesEXT
  , withCStructPipelineRasterizationStateStreamCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( ShaderModuleValidationCacheCreateInfoEXT(..)
  , ValidationCacheCreateInfoEXT(..)
  , fromCStructShaderModuleValidationCacheCreateInfoEXT
  , fromCStructValidationCacheCreateInfoEXT
  , withCStructShaderModuleValidationCacheCreateInfoEXT
  , withCStructValidationCacheCreateInfoEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_features
  ( ValidationFeaturesEXT(..)
  , fromCStructValidationFeaturesEXT
  , withCStructValidationFeaturesEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationFlagsEXT(..)
  , fromCStructValidationFlagsEXT
  , withCStructValidationFlagsEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( PhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , PipelineVertexInputDivisorStateCreateInfoEXT(..)
  , VertexInputBindingDivisorDescriptionEXT(..)
  , fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT
  , fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  , fromCStructPipelineVertexInputDivisorStateCreateInfoEXT
  , fromCStructVertexInputBindingDivisorDescriptionEXT
  , withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT
  , withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
  , withCStructPipelineVertexInputDivisorStateCreateInfoEXT
  , withCStructVertexInputBindingDivisorDescriptionEXT
  )
import Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays
  ( PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
  , withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
  )

#if defined(VK_USE_PLATFORM_FUCHSIA)
import Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( withCStructImagePipeSurfaceCreateInfoFUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_FUCHSIA)
import Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( fromCStructImagePipeSurfaceCreateInfoFUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_FUCHSIA)
import Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( ImagePipeSurfaceCreateInfoFUCHSIA(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( withCStructPresentFrameTokenGGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( fromCStructPresentFrameTokenGGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( PresentFrameTokenGGP(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( withCStructStreamDescriptorSurfaceCreateInfoGGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( fromCStructStreamDescriptorSurfaceCreateInfoGGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( StreamDescriptorSurfaceCreateInfoGGP(..)
  )
#endif
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
import Graphics.Vulkan.Extensions.VK_KHR_8bit_storage
  ( PhysicalDevice8BitStorageFeaturesKHR(..)
  , fromCStructPhysicalDevice8BitStorageFeaturesKHR
  , withCStructPhysicalDevice8BitStorageFeaturesKHR
  )

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( withCStructAndroidSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( fromCStructAndroidSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_ANDROID_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( AndroidSurfaceCreateInfoKHR(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( AttachmentDescription2KHR(..)
  , AttachmentReference2KHR(..)
  , RenderPassCreateInfo2KHR(..)
  , SubpassBeginInfoKHR(..)
  , SubpassDependency2KHR(..)
  , SubpassDescription2KHR(..)
  , SubpassEndInfoKHR(..)
  , fromCStructAttachmentDescription2KHR
  , fromCStructAttachmentReference2KHR
  , fromCStructRenderPassCreateInfo2KHR
  , fromCStructSubpassBeginInfoKHR
  , fromCStructSubpassDependency2KHR
  , fromCStructSubpassDescription2KHR
  , fromCStructSubpassEndInfoKHR
  , withCStructAttachmentDescription2KHR
  , withCStructAttachmentReference2KHR
  , withCStructRenderPassCreateInfo2KHR
  , withCStructSubpassBeginInfoKHR
  , withCStructSubpassDependency2KHR
  , withCStructSubpassDescription2KHR
  , withCStructSubpassEndInfoKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( PhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , SubpassDescriptionDepthStencilResolveKHR(..)
  , fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
  , fromCStructSubpassDescriptionDepthStencilResolveKHR
  , withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
  , withCStructSubpassDescriptionDepthStencilResolveKHR
  )
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
import Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( ConformanceVersionKHR(..)
  , PhysicalDeviceDriverPropertiesKHR(..)
  , fromCStructConformanceVersionKHR
  , fromCStructPhysicalDeviceDriverPropertiesKHR
  , withCStructConformanceVersionKHR
  , withCStructPhysicalDeviceDriverPropertiesKHR
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
  ( withCStructExportFenceWin32HandleInfoKHR
  , withCStructFenceGetWin32HandleInfoKHR
  , withCStructImportFenceWin32HandleInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( fromCStructExportFenceWin32HandleInfoKHR
  , fromCStructFenceGetWin32HandleInfoKHR
  , fromCStructImportFenceWin32HandleInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( ExportFenceWin32HandleInfoKHR(..)
  , FenceGetWin32HandleInfoKHR(..)
  , ImportFenceWin32HandleInfoKHR(..)
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
  ( withCStructExportMemoryWin32HandleInfoKHR
  , withCStructImportMemoryWin32HandleInfoKHR
  , withCStructMemoryGetWin32HandleInfoKHR
  , withCStructMemoryWin32HandlePropertiesKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( fromCStructExportMemoryWin32HandleInfoKHR
  , fromCStructImportMemoryWin32HandleInfoKHR
  , fromCStructMemoryGetWin32HandleInfoKHR
  , fromCStructMemoryWin32HandlePropertiesKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( ExportMemoryWin32HandleInfoKHR(..)
  , ImportMemoryWin32HandleInfoKHR(..)
  , MemoryGetWin32HandleInfoKHR(..)
  , MemoryWin32HandlePropertiesKHR(..)
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
  ( withCStructD3D12FenceSubmitInfoKHR
  , withCStructExportSemaphoreWin32HandleInfoKHR
  , withCStructImportSemaphoreWin32HandleInfoKHR
  , withCStructSemaphoreGetWin32HandleInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( fromCStructD3D12FenceSubmitInfoKHR
  , fromCStructExportSemaphoreWin32HandleInfoKHR
  , fromCStructImportSemaphoreWin32HandleInfoKHR
  , fromCStructSemaphoreGetWin32HandleInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( D3D12FenceSubmitInfoKHR(..)
  , ExportSemaphoreWin32HandleInfoKHR(..)
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , SemaphoreGetWin32HandleInfoKHR(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2
  ( DisplayModeProperties2KHR(..)
  , DisplayPlaneCapabilities2KHR(..)
  , DisplayPlaneInfo2KHR(..)
  , DisplayPlaneProperties2KHR(..)
  , DisplayProperties2KHR(..)
  , fromCStructDisplayModeProperties2KHR
  , fromCStructDisplayPlaneCapabilities2KHR
  , fromCStructDisplayPlaneInfo2KHR
  , fromCStructDisplayPlaneProperties2KHR
  , fromCStructDisplayProperties2KHR
  , withCStructDisplayModeProperties2KHR
  , withCStructDisplayPlaneCapabilities2KHR
  , withCStructDisplayPlaneInfo2KHR
  , withCStructDisplayPlaneProperties2KHR
  , withCStructDisplayProperties2KHR
  )
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
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( PhysicalDevicePushDescriptorPropertiesKHR(..)
  , fromCStructPhysicalDevicePushDescriptorPropertiesKHR
  , withCStructPhysicalDevicePushDescriptorPropertiesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_shader_atomic_int64
  ( PhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR
  , withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_shader_float16_int8
  ( PhysicalDeviceFloat16Int8FeaturesKHR(..)
  , fromCStructPhysicalDeviceFloat16Int8FeaturesKHR
  , withCStructPhysicalDeviceFloat16Int8FeaturesKHR
  )
import Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls
  ( PhysicalDeviceFloatControlsPropertiesKHR(..)
  , fromCStructPhysicalDeviceFloatControlsPropertiesKHR
  , withCStructPhysicalDeviceFloatControlsPropertiesKHR
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
import Graphics.Vulkan.Extensions.VK_KHR_surface_protected_capabilities
  ( SurfaceProtectedCapabilitiesKHR(..)
  , fromCStructSurfaceProtectedCapabilitiesKHR
  , withCStructSurfaceProtectedCapabilitiesKHR
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
import Graphics.Vulkan.Extensions.VK_KHR_vulkan_memory_model
  ( PhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
  , withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
  )

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( withCStructWaylandSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( fromCStructWaylandSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( WaylandSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( withCStructWin32KeyedMutexAcquireReleaseInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( fromCStructWin32KeyedMutexAcquireReleaseInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( withCStructWin32SurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( fromCStructWin32SurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( Win32SurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( withCStructXcbSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( fromCStructXcbSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XCB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( XcbSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( withCStructXlibSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( fromCStructXlibSurfaceCreateInfoKHR
  )
#endif

#if defined(VK_USE_PLATFORM_XLIB_KHR)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( XlibSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( withCStructIOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( fromCStructIOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_IOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( IOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( withCStructMacOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( fromCStructMacOSSurfaceCreateInfoMVK
  )
#endif

#if defined(VK_USE_PLATFORM_MACOS_MVK)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( withCStructViSurfaceCreateInfoNN
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( fromCStructViSurfaceCreateInfoNN
  )
#endif

#if defined(VK_USE_PLATFORM_VI_NN)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( ViSurfaceCreateInfoNN(..)
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
import Graphics.Vulkan.Extensions.VK_NVX_image_view_handle
  ( ImageViewHandleInfoNVX(..)
  , fromCStructImageViewHandleInfoNVX
  , withCStructImageViewHandleInfoNVX
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
import Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives
  ( PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV
  , withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix
  ( CooperativeMatrixPropertiesNV(..)
  , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , fromCStructCooperativeMatrixPropertiesNV
  , fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV
  , fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV
  , withCStructCooperativeMatrixPropertiesNV
  , withCStructPhysicalDeviceCooperativeMatrixFeaturesNV
  , withCStructPhysicalDeviceCooperativeMatrixPropertiesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image
  ( PhysicalDeviceCornerSampledImageFeaturesNV(..)
  , fromCStructPhysicalDeviceCornerSampledImageFeaturesNV
  , withCStructPhysicalDeviceCornerSampledImageFeaturesNV
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
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  , withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints
  ( CheckpointDataNV(..)
  , QueueFamilyCheckpointPropertiesNV(..)
  , fromCStructCheckpointDataNV
  , fromCStructQueueFamilyCheckpointPropertiesNV
  , withCStructCheckpointDataNV
  , withCStructQueueFamilyCheckpointPropertiesNV
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
  ( withCStructExportMemoryWin32HandleInfoNV
  , withCStructImportMemoryWin32HandleInfoNV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( fromCStructExportMemoryWin32HandleInfoNV
  , fromCStructImportMemoryWin32HandleInfoNV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( ExportMemoryWin32HandleInfoNV(..)
  , ImportMemoryWin32HandleInfoNV(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateInfoNV(..)
  , fromCStructPipelineCoverageToColorStateCreateInfoNV
  , withCStructPipelineCoverageToColorStateCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric
  ( PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
  , withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( PipelineCoverageModulationStateCreateInfoNV(..)
  , fromCStructPipelineCoverageModulationStateCreateInfoNV
  , withCStructPipelineCoverageModulationStateCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_mesh_shader
  ( DrawMeshTasksIndirectCommandNV(..)
  , PhysicalDeviceMeshShaderFeaturesNV(..)
  , PhysicalDeviceMeshShaderPropertiesNV(..)
  , fromCStructDrawMeshTasksIndirectCommandNV
  , fromCStructPhysicalDeviceMeshShaderFeaturesNV
  , fromCStructPhysicalDeviceMeshShaderPropertiesNV
  , withCStructDrawMeshTasksIndirectCommandNV
  , withCStructPhysicalDeviceMeshShaderFeaturesNV
  , withCStructPhysicalDeviceMeshShaderPropertiesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_ray_tracing
  ( AccelerationStructureCreateInfoNV(..)
  , AccelerationStructureInfoNV(..)
  , AccelerationStructureMemoryRequirementsInfoNV(..)
  , BindAccelerationStructureMemoryInfoNV(..)
  , GeometryAABBNV(..)
  , GeometryDataNV(..)
  , GeometryNV(..)
  , GeometryTrianglesNV(..)
  , PhysicalDeviceRayTracingPropertiesNV(..)
  , RayTracingPipelineCreateInfoNV(..)
  , RayTracingShaderGroupCreateInfoNV(..)
  , WriteDescriptorSetAccelerationStructureNV(..)
  , fromCStructAccelerationStructureCreateInfoNV
  , fromCStructAccelerationStructureInfoNV
  , fromCStructAccelerationStructureMemoryRequirementsInfoNV
  , fromCStructBindAccelerationStructureMemoryInfoNV
  , fromCStructGeometryAABBNV
  , fromCStructGeometryDataNV
  , fromCStructGeometryNV
  , fromCStructGeometryTrianglesNV
  , fromCStructPhysicalDeviceRayTracingPropertiesNV
  , fromCStructRayTracingPipelineCreateInfoNV
  , fromCStructRayTracingShaderGroupCreateInfoNV
  , fromCStructWriteDescriptorSetAccelerationStructureNV
  , withCStructAccelerationStructureCreateInfoNV
  , withCStructAccelerationStructureInfoNV
  , withCStructAccelerationStructureMemoryRequirementsInfoNV
  , withCStructBindAccelerationStructureMemoryInfoNV
  , withCStructGeometryAABBNV
  , withCStructGeometryDataNV
  , withCStructGeometryNV
  , withCStructGeometryTrianglesNV
  , withCStructPhysicalDeviceRayTracingPropertiesNV
  , withCStructRayTracingPipelineCreateInfoNV
  , withCStructRayTracingShaderGroupCreateInfoNV
  , withCStructWriteDescriptorSetAccelerationStructureNV
  )
import Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test
  ( PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  , fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV
  , fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV
  , withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV
  , withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive
  ( PhysicalDeviceExclusiveScissorFeaturesNV(..)
  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , fromCStructPhysicalDeviceExclusiveScissorFeaturesNV
  , fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV
  , withCStructPhysicalDeviceExclusiveScissorFeaturesNV
  , withCStructPipelineViewportExclusiveScissorStateCreateInfoNV
  )
import Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint
  ( PhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV
  , withCStructPhysicalDeviceShaderImageFootprintFeaturesNV
  )
import Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( CoarseSampleLocationNV(..)
  , CoarseSampleOrderCustomNV(..)
  , PhysicalDeviceShadingRateImageFeaturesNV(..)
  , PhysicalDeviceShadingRateImagePropertiesNV(..)
  , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , PipelineViewportShadingRateImageStateCreateInfoNV(..)
  , ShadingRatePaletteNV(..)
  , fromCStructCoarseSampleLocationNV
  , fromCStructCoarseSampleOrderCustomNV
  , fromCStructPhysicalDeviceShadingRateImageFeaturesNV
  , fromCStructPhysicalDeviceShadingRateImagePropertiesNV
  , fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV
  , fromCStructPipelineViewportShadingRateImageStateCreateInfoNV
  , fromCStructShadingRatePaletteNV
  , withCStructCoarseSampleLocationNV
  , withCStructCoarseSampleOrderCustomNV
  , withCStructPhysicalDeviceShadingRateImageFeaturesNV
  , withCStructPhysicalDeviceShadingRateImagePropertiesNV
  , withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV
  , withCStructPipelineViewportShadingRateImageStateCreateInfoNV
  , withCStructShadingRatePaletteNV
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
  ( withCStructWin32KeyedMutexAcquireReleaseInfoNV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( fromCStructWin32KeyedMutexAcquireReleaseInfoNV
  )
#endif

#if defined(VK_USE_PLATFORM_WIN32_KHR)
import Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoNV(..)
  )
#endif


class ToCStruct marshalled c | marshalled -> c, c -> marshalled where
  withCStruct :: marshalled -> (c -> IO a) -> IO a

class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
  fromCStruct :: c -> IO marshalled

class HasNext a where
  getNext :: a -> Maybe SomeVkStruct

data SomeVkStruct where
  SomeVkStruct
    :: (ToCStruct a b, Storable b, Show a, Eq a, Typeable a, HasNext a)
    => a
    -> SomeVkStruct

instance HasNext SomeVkStruct where
  getNext (SomeVkStruct s) = getNext s

deriving instance Show SomeVkStruct

instance Eq SomeVkStruct where
  SomeVkStruct (s1 :: s1) == SomeVkStruct (s2 :: s2) = case eqT @s1 @s2 of
    Nothing   -> False
    Just Refl -> s1 == s2

withCStructPtr :: (Storable c, ToCStruct a c) => a -> (Ptr c -> IO b) -> IO b
withCStructPtr s f = withCStruct s (\c -> alloca (\p -> poke p c *> f p))

fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
fromCStructPtr p = fromCStruct =<< peek p

fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
fromSomeVkStruct (SomeVkStruct s) = cast s

fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
fromSomeVkStructChain s =
  fromSomeVkStruct s <|> (getNext s >>= fromSomeVkStructChain)

withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
withSomeVkStruct (SomeVkStruct s) f = withCStructPtr s (f . castPtr)

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
instance HasNext ApplicationInfo where
  getNext s = next (s :: ApplicationInfo)
instance ToCStruct AllocationCallbacks VkAllocationCallbacks where
  withCStruct = withCStructAllocationCallbacks
instance FromCStruct AllocationCallbacks VkAllocationCallbacks where
  fromCStruct = fromCStructAllocationCallbacks

instance ToCStruct DeviceQueueCreateInfo VkDeviceQueueCreateInfo where
  withCStruct = withCStructDeviceQueueCreateInfo
instance FromCStruct DeviceQueueCreateInfo VkDeviceQueueCreateInfo where
  fromCStruct = fromCStructDeviceQueueCreateInfo
instance HasNext DeviceQueueCreateInfo where
  getNext s = next (s :: DeviceQueueCreateInfo)
instance ToCStruct DeviceCreateInfo VkDeviceCreateInfo where
  withCStruct = withCStructDeviceCreateInfo
instance FromCStruct DeviceCreateInfo VkDeviceCreateInfo where
  fromCStruct = fromCStructDeviceCreateInfo
instance HasNext DeviceCreateInfo where
  getNext s = next (s :: DeviceCreateInfo)
instance ToCStruct InstanceCreateInfo VkInstanceCreateInfo where
  withCStruct = withCStructInstanceCreateInfo
instance FromCStruct InstanceCreateInfo VkInstanceCreateInfo where
  fromCStruct = fromCStructInstanceCreateInfo
instance HasNext InstanceCreateInfo where
  getNext s = next (s :: InstanceCreateInfo)
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
instance HasNext MemoryAllocateInfo where
  getNext s = next (s :: MemoryAllocateInfo)
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
instance HasNext MappedMemoryRange where
  getNext s = next (s :: MappedMemoryRange)
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
instance HasNext WriteDescriptorSet where
  getNext s = next (s :: WriteDescriptorSet)
instance ToCStruct CopyDescriptorSet VkCopyDescriptorSet where
  withCStruct = withCStructCopyDescriptorSet
instance FromCStruct CopyDescriptorSet VkCopyDescriptorSet where
  fromCStruct = fromCStructCopyDescriptorSet
instance HasNext CopyDescriptorSet where
  getNext s = next (s :: CopyDescriptorSet)
instance ToCStruct BufferCreateInfo VkBufferCreateInfo where
  withCStruct = withCStructBufferCreateInfo
instance FromCStruct BufferCreateInfo VkBufferCreateInfo where
  fromCStruct = fromCStructBufferCreateInfo
instance HasNext BufferCreateInfo where
  getNext s = next (s :: BufferCreateInfo)
instance ToCStruct BufferViewCreateInfo VkBufferViewCreateInfo where
  withCStruct = withCStructBufferViewCreateInfo
instance FromCStruct BufferViewCreateInfo VkBufferViewCreateInfo where
  fromCStruct = fromCStructBufferViewCreateInfo
instance HasNext BufferViewCreateInfo where
  getNext s = next (s :: BufferViewCreateInfo)
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
instance HasNext MemoryBarrier where
  getNext s = next (s :: MemoryBarrier)
instance ToCStruct BufferMemoryBarrier VkBufferMemoryBarrier where
  withCStruct = withCStructBufferMemoryBarrier
instance FromCStruct BufferMemoryBarrier VkBufferMemoryBarrier where
  fromCStruct = fromCStructBufferMemoryBarrier
instance HasNext BufferMemoryBarrier where
  getNext s = next (s :: BufferMemoryBarrier)
instance ToCStruct ImageMemoryBarrier VkImageMemoryBarrier where
  withCStruct = withCStructImageMemoryBarrier
instance FromCStruct ImageMemoryBarrier VkImageMemoryBarrier where
  fromCStruct = fromCStructImageMemoryBarrier
instance HasNext ImageMemoryBarrier where
  getNext s = next (s :: ImageMemoryBarrier)
instance ToCStruct ImageCreateInfo VkImageCreateInfo where
  withCStruct = withCStructImageCreateInfo
instance FromCStruct ImageCreateInfo VkImageCreateInfo where
  fromCStruct = fromCStructImageCreateInfo
instance HasNext ImageCreateInfo where
  getNext s = next (s :: ImageCreateInfo)
instance ToCStruct SubresourceLayout VkSubresourceLayout where
  withCStruct = withCStructSubresourceLayout
instance FromCStruct SubresourceLayout VkSubresourceLayout where
  fromCStruct = fromCStructSubresourceLayout

instance ToCStruct ImageViewCreateInfo VkImageViewCreateInfo where
  withCStruct = withCStructImageViewCreateInfo
instance FromCStruct ImageViewCreateInfo VkImageViewCreateInfo where
  fromCStruct = fromCStructImageViewCreateInfo
instance HasNext ImageViewCreateInfo where
  getNext s = next (s :: ImageViewCreateInfo)
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
instance HasNext BindSparseInfo where
  getNext s = next (s :: BindSparseInfo)
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
instance HasNext ShaderModuleCreateInfo where
  getNext s = next (s :: ShaderModuleCreateInfo)
instance ToCStruct DescriptorSetLayoutBinding VkDescriptorSetLayoutBinding where
  withCStruct = withCStructDescriptorSetLayoutBinding
instance FromCStruct DescriptorSetLayoutBinding VkDescriptorSetLayoutBinding where
  fromCStruct = fromCStructDescriptorSetLayoutBinding

instance ToCStruct DescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo where
  withCStruct = withCStructDescriptorSetLayoutCreateInfo
instance FromCStruct DescriptorSetLayoutCreateInfo VkDescriptorSetLayoutCreateInfo where
  fromCStruct = fromCStructDescriptorSetLayoutCreateInfo
instance HasNext DescriptorSetLayoutCreateInfo where
  getNext s = next (s :: DescriptorSetLayoutCreateInfo)
instance ToCStruct DescriptorPoolSize VkDescriptorPoolSize where
  withCStruct = withCStructDescriptorPoolSize
instance FromCStruct DescriptorPoolSize VkDescriptorPoolSize where
  fromCStruct = fromCStructDescriptorPoolSize

instance ToCStruct DescriptorPoolCreateInfo VkDescriptorPoolCreateInfo where
  withCStruct = withCStructDescriptorPoolCreateInfo
instance FromCStruct DescriptorPoolCreateInfo VkDescriptorPoolCreateInfo where
  fromCStruct = fromCStructDescriptorPoolCreateInfo
instance HasNext DescriptorPoolCreateInfo where
  getNext s = next (s :: DescriptorPoolCreateInfo)
instance ToCStruct DescriptorSetAllocateInfo VkDescriptorSetAllocateInfo where
  withCStruct = withCStructDescriptorSetAllocateInfo
instance FromCStruct DescriptorSetAllocateInfo VkDescriptorSetAllocateInfo where
  fromCStruct = fromCStructDescriptorSetAllocateInfo
instance HasNext DescriptorSetAllocateInfo where
  getNext s = next (s :: DescriptorSetAllocateInfo)
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
instance HasNext PipelineShaderStageCreateInfo where
  getNext s = next (s :: PipelineShaderStageCreateInfo)
instance ToCStruct ComputePipelineCreateInfo VkComputePipelineCreateInfo where
  withCStruct = withCStructComputePipelineCreateInfo
instance FromCStruct ComputePipelineCreateInfo VkComputePipelineCreateInfo where
  fromCStruct = fromCStructComputePipelineCreateInfo
instance HasNext ComputePipelineCreateInfo where
  getNext s = next (s :: ComputePipelineCreateInfo)
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
instance HasNext PipelineVertexInputStateCreateInfo where
  getNext s = next (s :: PipelineVertexInputStateCreateInfo)
instance ToCStruct PipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo where
  withCStruct = withCStructPipelineInputAssemblyStateCreateInfo
instance FromCStruct PipelineInputAssemblyStateCreateInfo VkPipelineInputAssemblyStateCreateInfo where
  fromCStruct = fromCStructPipelineInputAssemblyStateCreateInfo
instance HasNext PipelineInputAssemblyStateCreateInfo where
  getNext s = next (s :: PipelineInputAssemblyStateCreateInfo)
instance ToCStruct PipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo where
  withCStruct = withCStructPipelineTessellationStateCreateInfo
instance FromCStruct PipelineTessellationStateCreateInfo VkPipelineTessellationStateCreateInfo where
  fromCStruct = fromCStructPipelineTessellationStateCreateInfo
instance HasNext PipelineTessellationStateCreateInfo where
  getNext s = next (s :: PipelineTessellationStateCreateInfo)
instance ToCStruct PipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo where
  withCStruct = withCStructPipelineViewportStateCreateInfo
instance FromCStruct PipelineViewportStateCreateInfo VkPipelineViewportStateCreateInfo where
  fromCStruct = fromCStructPipelineViewportStateCreateInfo
instance HasNext PipelineViewportStateCreateInfo where
  getNext s = next (s :: PipelineViewportStateCreateInfo)
instance ToCStruct PipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo where
  withCStruct = withCStructPipelineRasterizationStateCreateInfo
instance FromCStruct PipelineRasterizationStateCreateInfo VkPipelineRasterizationStateCreateInfo where
  fromCStruct = fromCStructPipelineRasterizationStateCreateInfo
instance HasNext PipelineRasterizationStateCreateInfo where
  getNext s = next (s :: PipelineRasterizationStateCreateInfo)
instance ToCStruct PipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo where
  withCStruct = withCStructPipelineMultisampleStateCreateInfo
instance FromCStruct PipelineMultisampleStateCreateInfo VkPipelineMultisampleStateCreateInfo where
  fromCStruct = fromCStructPipelineMultisampleStateCreateInfo
instance HasNext PipelineMultisampleStateCreateInfo where
  getNext s = next (s :: PipelineMultisampleStateCreateInfo)
instance ToCStruct PipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState where
  withCStruct = withCStructPipelineColorBlendAttachmentState
instance FromCStruct PipelineColorBlendAttachmentState VkPipelineColorBlendAttachmentState where
  fromCStruct = fromCStructPipelineColorBlendAttachmentState

instance ToCStruct PipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo where
  withCStruct = withCStructPipelineColorBlendStateCreateInfo
instance FromCStruct PipelineColorBlendStateCreateInfo VkPipelineColorBlendStateCreateInfo where
  fromCStruct = fromCStructPipelineColorBlendStateCreateInfo
instance HasNext PipelineColorBlendStateCreateInfo where
  getNext s = next (s :: PipelineColorBlendStateCreateInfo)
instance ToCStruct PipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo where
  withCStruct = withCStructPipelineDynamicStateCreateInfo
instance FromCStruct PipelineDynamicStateCreateInfo VkPipelineDynamicStateCreateInfo where
  fromCStruct = fromCStructPipelineDynamicStateCreateInfo
instance HasNext PipelineDynamicStateCreateInfo where
  getNext s = next (s :: PipelineDynamicStateCreateInfo)
instance ToCStruct StencilOpState VkStencilOpState where
  withCStruct = withCStructStencilOpState
instance FromCStruct StencilOpState VkStencilOpState where
  fromCStruct = fromCStructStencilOpState

instance ToCStruct PipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo where
  withCStruct = withCStructPipelineDepthStencilStateCreateInfo
instance FromCStruct PipelineDepthStencilStateCreateInfo VkPipelineDepthStencilStateCreateInfo where
  fromCStruct = fromCStructPipelineDepthStencilStateCreateInfo
instance HasNext PipelineDepthStencilStateCreateInfo where
  getNext s = next (s :: PipelineDepthStencilStateCreateInfo)
instance ToCStruct GraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo where
  withCStruct = withCStructGraphicsPipelineCreateInfo
instance FromCStruct GraphicsPipelineCreateInfo VkGraphicsPipelineCreateInfo where
  fromCStruct = fromCStructGraphicsPipelineCreateInfo
instance HasNext GraphicsPipelineCreateInfo where
  getNext s = next (s :: GraphicsPipelineCreateInfo)
instance ToCStruct PipelineCacheCreateInfo VkPipelineCacheCreateInfo where
  withCStruct = withCStructPipelineCacheCreateInfo
instance FromCStruct PipelineCacheCreateInfo VkPipelineCacheCreateInfo where
  fromCStruct = fromCStructPipelineCacheCreateInfo
instance HasNext PipelineCacheCreateInfo where
  getNext s = next (s :: PipelineCacheCreateInfo)
instance ToCStruct PushConstantRange VkPushConstantRange where
  withCStruct = withCStructPushConstantRange
instance FromCStruct PushConstantRange VkPushConstantRange where
  fromCStruct = fromCStructPushConstantRange

instance ToCStruct PipelineLayoutCreateInfo VkPipelineLayoutCreateInfo where
  withCStruct = withCStructPipelineLayoutCreateInfo
instance FromCStruct PipelineLayoutCreateInfo VkPipelineLayoutCreateInfo where
  fromCStruct = fromCStructPipelineLayoutCreateInfo
instance HasNext PipelineLayoutCreateInfo where
  getNext s = next (s :: PipelineLayoutCreateInfo)
instance ToCStruct SamplerCreateInfo VkSamplerCreateInfo where
  withCStruct = withCStructSamplerCreateInfo
instance FromCStruct SamplerCreateInfo VkSamplerCreateInfo where
  fromCStruct = fromCStructSamplerCreateInfo
instance HasNext SamplerCreateInfo where
  getNext s = next (s :: SamplerCreateInfo)
instance ToCStruct CommandPoolCreateInfo VkCommandPoolCreateInfo where
  withCStruct = withCStructCommandPoolCreateInfo
instance FromCStruct CommandPoolCreateInfo VkCommandPoolCreateInfo where
  fromCStruct = fromCStructCommandPoolCreateInfo
instance HasNext CommandPoolCreateInfo where
  getNext s = next (s :: CommandPoolCreateInfo)
instance ToCStruct CommandBufferAllocateInfo VkCommandBufferAllocateInfo where
  withCStruct = withCStructCommandBufferAllocateInfo
instance FromCStruct CommandBufferAllocateInfo VkCommandBufferAllocateInfo where
  fromCStruct = fromCStructCommandBufferAllocateInfo
instance HasNext CommandBufferAllocateInfo where
  getNext s = next (s :: CommandBufferAllocateInfo)
instance ToCStruct CommandBufferInheritanceInfo VkCommandBufferInheritanceInfo where
  withCStruct = withCStructCommandBufferInheritanceInfo
instance FromCStruct CommandBufferInheritanceInfo VkCommandBufferInheritanceInfo where
  fromCStruct = fromCStructCommandBufferInheritanceInfo
instance HasNext CommandBufferInheritanceInfo where
  getNext s = next (s :: CommandBufferInheritanceInfo)
instance ToCStruct CommandBufferBeginInfo VkCommandBufferBeginInfo where
  withCStruct = withCStructCommandBufferBeginInfo
instance FromCStruct CommandBufferBeginInfo VkCommandBufferBeginInfo where
  fromCStruct = fromCStructCommandBufferBeginInfo
instance HasNext CommandBufferBeginInfo where
  getNext s = next (s :: CommandBufferBeginInfo)
instance ToCStruct RenderPassBeginInfo VkRenderPassBeginInfo where
  withCStruct = withCStructRenderPassBeginInfo
-- No FromCStruct instance for VkRenderPassBeginInfo as it contains a union type
instance HasNext RenderPassBeginInfo where
  getNext s = next (s :: RenderPassBeginInfo)
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
instance HasNext RenderPassCreateInfo where
  getNext s = next (s :: RenderPassCreateInfo)
instance ToCStruct EventCreateInfo VkEventCreateInfo where
  withCStruct = withCStructEventCreateInfo
instance FromCStruct EventCreateInfo VkEventCreateInfo where
  fromCStruct = fromCStructEventCreateInfo
instance HasNext EventCreateInfo where
  getNext s = next (s :: EventCreateInfo)
instance ToCStruct FenceCreateInfo VkFenceCreateInfo where
  withCStruct = withCStructFenceCreateInfo
instance FromCStruct FenceCreateInfo VkFenceCreateInfo where
  fromCStruct = fromCStructFenceCreateInfo
instance HasNext FenceCreateInfo where
  getNext s = next (s :: FenceCreateInfo)
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
instance HasNext SemaphoreCreateInfo where
  getNext s = next (s :: SemaphoreCreateInfo)
instance ToCStruct QueryPoolCreateInfo VkQueryPoolCreateInfo where
  withCStruct = withCStructQueryPoolCreateInfo
instance FromCStruct QueryPoolCreateInfo VkQueryPoolCreateInfo where
  fromCStruct = fromCStructQueryPoolCreateInfo
instance HasNext QueryPoolCreateInfo where
  getNext s = next (s :: QueryPoolCreateInfo)
instance ToCStruct FramebufferCreateInfo VkFramebufferCreateInfo where
  withCStruct = withCStructFramebufferCreateInfo
instance FromCStruct FramebufferCreateInfo VkFramebufferCreateInfo where
  fromCStruct = fromCStructFramebufferCreateInfo
instance HasNext FramebufferCreateInfo where
  getNext s = next (s :: FramebufferCreateInfo)
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
instance HasNext SubmitInfo where
  getNext s = next (s :: SubmitInfo)
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
instance HasNext DisplayModeCreateInfoKHR where
  getNext s = next (s :: DisplayModeCreateInfoKHR)
instance ToCStruct DisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR where
  withCStruct = withCStructDisplayPlaneCapabilitiesKHR
instance FromCStruct DisplayPlaneCapabilitiesKHR VkDisplayPlaneCapabilitiesKHR where
  fromCStruct = fromCStructDisplayPlaneCapabilitiesKHR

instance ToCStruct DisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR where
  withCStruct = withCStructDisplaySurfaceCreateInfoKHR
instance FromCStruct DisplaySurfaceCreateInfoKHR VkDisplaySurfaceCreateInfoKHR where
  fromCStruct = fromCStructDisplaySurfaceCreateInfoKHR
instance HasNext DisplaySurfaceCreateInfoKHR where
  getNext s = next (s :: DisplaySurfaceCreateInfoKHR)
instance ToCStruct DisplayPresentInfoKHR VkDisplayPresentInfoKHR where
  withCStruct = withCStructDisplayPresentInfoKHR
instance FromCStruct DisplayPresentInfoKHR VkDisplayPresentInfoKHR where
  fromCStruct = fromCStructDisplayPresentInfoKHR
instance HasNext DisplayPresentInfoKHR where
  getNext s = next (s :: DisplayPresentInfoKHR)
instance ToCStruct SurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR where
  withCStruct = withCStructSurfaceCapabilitiesKHR
instance FromCStruct SurfaceCapabilitiesKHR VkSurfaceCapabilitiesKHR where
  fromCStruct = fromCStructSurfaceCapabilitiesKHR


#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR where
  withCStruct = withCStructAndroidSurfaceCreateInfoKHR
instance FromCStruct AndroidSurfaceCreateInfoKHR VkAndroidSurfaceCreateInfoKHR where
  fromCStruct = fromCStructAndroidSurfaceCreateInfoKHR
instance HasNext AndroidSurfaceCreateInfoKHR where
  getNext s = next (s :: AndroidSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_VI_NN
instance ToCStruct ViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN where
  withCStruct = withCStructViSurfaceCreateInfoNN
instance FromCStruct ViSurfaceCreateInfoNN VkViSurfaceCreateInfoNN where
  fromCStruct = fromCStructViSurfaceCreateInfoNN
instance HasNext ViSurfaceCreateInfoNN where
  getNext s = next (s :: ViSurfaceCreateInfoNN)
#endif

#if VK_USE_PLATFORM_WAYLAND_KHR
instance ToCStruct WaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR where
  withCStruct = withCStructWaylandSurfaceCreateInfoKHR
instance FromCStruct WaylandSurfaceCreateInfoKHR VkWaylandSurfaceCreateInfoKHR where
  fromCStruct = fromCStructWaylandSurfaceCreateInfoKHR
instance HasNext WaylandSurfaceCreateInfoKHR where
  getNext s = next (s :: WaylandSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct Win32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR where
  withCStruct = withCStructWin32SurfaceCreateInfoKHR
instance FromCStruct Win32SurfaceCreateInfoKHR VkWin32SurfaceCreateInfoKHR where
  fromCStruct = fromCStructWin32SurfaceCreateInfoKHR
instance HasNext Win32SurfaceCreateInfoKHR where
  getNext s = next (s :: Win32SurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_XLIB_KHR
instance ToCStruct XlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR where
  withCStruct = withCStructXlibSurfaceCreateInfoKHR
instance FromCStruct XlibSurfaceCreateInfoKHR VkXlibSurfaceCreateInfoKHR where
  fromCStruct = fromCStructXlibSurfaceCreateInfoKHR
instance HasNext XlibSurfaceCreateInfoKHR where
  getNext s = next (s :: XlibSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_XCB_KHR
instance ToCStruct XcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR where
  withCStruct = withCStructXcbSurfaceCreateInfoKHR
instance FromCStruct XcbSurfaceCreateInfoKHR VkXcbSurfaceCreateInfoKHR where
  fromCStruct = fromCStructXcbSurfaceCreateInfoKHR
instance HasNext XcbSurfaceCreateInfoKHR where
  getNext s = next (s :: XcbSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_FUCHSIA
instance ToCStruct ImagePipeSurfaceCreateInfoFUCHSIA VkImagePipeSurfaceCreateInfoFUCHSIA where
  withCStruct = withCStructImagePipeSurfaceCreateInfoFUCHSIA
instance FromCStruct ImagePipeSurfaceCreateInfoFUCHSIA VkImagePipeSurfaceCreateInfoFUCHSIA where
  fromCStruct = fromCStructImagePipeSurfaceCreateInfoFUCHSIA
instance HasNext ImagePipeSurfaceCreateInfoFUCHSIA where
  getNext s = next (s :: ImagePipeSurfaceCreateInfoFUCHSIA)
#endif

#if VK_USE_PLATFORM_GGP
instance ToCStruct StreamDescriptorSurfaceCreateInfoGGP VkStreamDescriptorSurfaceCreateInfoGGP where
  withCStruct = withCStructStreamDescriptorSurfaceCreateInfoGGP
instance FromCStruct StreamDescriptorSurfaceCreateInfoGGP VkStreamDescriptorSurfaceCreateInfoGGP where
  fromCStruct = fromCStructStreamDescriptorSurfaceCreateInfoGGP
instance HasNext StreamDescriptorSurfaceCreateInfoGGP where
  getNext s = next (s :: StreamDescriptorSurfaceCreateInfoGGP)
#endif
instance ToCStruct SurfaceFormatKHR VkSurfaceFormatKHR where
  withCStruct = withCStructSurfaceFormatKHR
instance FromCStruct SurfaceFormatKHR VkSurfaceFormatKHR where
  fromCStruct = fromCStructSurfaceFormatKHR

instance ToCStruct SwapchainCreateInfoKHR VkSwapchainCreateInfoKHR where
  withCStruct = withCStructSwapchainCreateInfoKHR
instance FromCStruct SwapchainCreateInfoKHR VkSwapchainCreateInfoKHR where
  fromCStruct = fromCStructSwapchainCreateInfoKHR
instance HasNext SwapchainCreateInfoKHR where
  getNext s = next (s :: SwapchainCreateInfoKHR)
instance ToCStruct PresentInfoKHR VkPresentInfoKHR where
  withCStruct = withCStructPresentInfoKHR
instance FromCStruct PresentInfoKHR VkPresentInfoKHR where
  fromCStruct = fromCStructPresentInfoKHR
instance HasNext PresentInfoKHR where
  getNext s = next (s :: PresentInfoKHR)
instance ToCStruct DebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT where
  withCStruct = withCStructDebugReportCallbackCreateInfoEXT
instance FromCStruct DebugReportCallbackCreateInfoEXT VkDebugReportCallbackCreateInfoEXT where
  fromCStruct = fromCStructDebugReportCallbackCreateInfoEXT
instance HasNext DebugReportCallbackCreateInfoEXT where
  getNext s = next (s :: DebugReportCallbackCreateInfoEXT)
instance ToCStruct ValidationFlagsEXT VkValidationFlagsEXT where
  withCStruct = withCStructValidationFlagsEXT
instance FromCStruct ValidationFlagsEXT VkValidationFlagsEXT where
  fromCStruct = fromCStructValidationFlagsEXT
instance HasNext ValidationFlagsEXT where
  getNext s = next (s :: ValidationFlagsEXT)
instance ToCStruct ValidationFeaturesEXT VkValidationFeaturesEXT where
  withCStruct = withCStructValidationFeaturesEXT
instance FromCStruct ValidationFeaturesEXT VkValidationFeaturesEXT where
  fromCStruct = fromCStructValidationFeaturesEXT
instance HasNext ValidationFeaturesEXT where
  getNext s = next (s :: ValidationFeaturesEXT)
instance ToCStruct PipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD where
  withCStruct = withCStructPipelineRasterizationStateRasterizationOrderAMD
instance FromCStruct PipelineRasterizationStateRasterizationOrderAMD VkPipelineRasterizationStateRasterizationOrderAMD where
  fromCStruct = fromCStructPipelineRasterizationStateRasterizationOrderAMD
instance HasNext PipelineRasterizationStateRasterizationOrderAMD where
  getNext s = next (s :: PipelineRasterizationStateRasterizationOrderAMD)
instance ToCStruct DebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT where
  withCStruct = withCStructDebugMarkerObjectNameInfoEXT
instance FromCStruct DebugMarkerObjectNameInfoEXT VkDebugMarkerObjectNameInfoEXT where
  fromCStruct = fromCStructDebugMarkerObjectNameInfoEXT
instance HasNext DebugMarkerObjectNameInfoEXT where
  getNext s = next (s :: DebugMarkerObjectNameInfoEXT)
instance ToCStruct DebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT where
  withCStruct = withCStructDebugMarkerObjectTagInfoEXT
instance FromCStruct DebugMarkerObjectTagInfoEXT VkDebugMarkerObjectTagInfoEXT where
  fromCStruct = fromCStructDebugMarkerObjectTagInfoEXT
instance HasNext DebugMarkerObjectTagInfoEXT where
  getNext s = next (s :: DebugMarkerObjectTagInfoEXT)
instance ToCStruct DebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT where
  withCStruct = withCStructDebugMarkerMarkerInfoEXT
instance FromCStruct DebugMarkerMarkerInfoEXT VkDebugMarkerMarkerInfoEXT where
  fromCStruct = fromCStructDebugMarkerMarkerInfoEXT
instance HasNext DebugMarkerMarkerInfoEXT where
  getNext s = next (s :: DebugMarkerMarkerInfoEXT)
instance ToCStruct DedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV where
  withCStruct = withCStructDedicatedAllocationImageCreateInfoNV
instance FromCStruct DedicatedAllocationImageCreateInfoNV VkDedicatedAllocationImageCreateInfoNV where
  fromCStruct = fromCStructDedicatedAllocationImageCreateInfoNV
instance HasNext DedicatedAllocationImageCreateInfoNV where
  getNext s = next (s :: DedicatedAllocationImageCreateInfoNV)
instance ToCStruct DedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV where
  withCStruct = withCStructDedicatedAllocationBufferCreateInfoNV
instance FromCStruct DedicatedAllocationBufferCreateInfoNV VkDedicatedAllocationBufferCreateInfoNV where
  fromCStruct = fromCStructDedicatedAllocationBufferCreateInfoNV
instance HasNext DedicatedAllocationBufferCreateInfoNV where
  getNext s = next (s :: DedicatedAllocationBufferCreateInfoNV)
instance ToCStruct DedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV where
  withCStruct = withCStructDedicatedAllocationMemoryAllocateInfoNV
instance FromCStruct DedicatedAllocationMemoryAllocateInfoNV VkDedicatedAllocationMemoryAllocateInfoNV where
  fromCStruct = fromCStructDedicatedAllocationMemoryAllocateInfoNV
instance HasNext DedicatedAllocationMemoryAllocateInfoNV where
  getNext s = next (s :: DedicatedAllocationMemoryAllocateInfoNV)
instance ToCStruct ExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV where
  withCStruct = withCStructExternalImageFormatPropertiesNV
instance FromCStruct ExternalImageFormatPropertiesNV VkExternalImageFormatPropertiesNV where
  fromCStruct = fromCStructExternalImageFormatPropertiesNV

instance ToCStruct ExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV where
  withCStruct = withCStructExternalMemoryImageCreateInfoNV
instance FromCStruct ExternalMemoryImageCreateInfoNV VkExternalMemoryImageCreateInfoNV where
  fromCStruct = fromCStructExternalMemoryImageCreateInfoNV
instance HasNext ExternalMemoryImageCreateInfoNV where
  getNext s = next (s :: ExternalMemoryImageCreateInfoNV)
instance ToCStruct ExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV where
  withCStruct = withCStructExportMemoryAllocateInfoNV
instance FromCStruct ExportMemoryAllocateInfoNV VkExportMemoryAllocateInfoNV where
  fromCStruct = fromCStructExportMemoryAllocateInfoNV
instance HasNext ExportMemoryAllocateInfoNV where
  getNext s = next (s :: ExportMemoryAllocateInfoNV)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV where
  withCStruct = withCStructImportMemoryWin32HandleInfoNV
instance FromCStruct ImportMemoryWin32HandleInfoNV VkImportMemoryWin32HandleInfoNV where
  fromCStruct = fromCStructImportMemoryWin32HandleInfoNV
instance HasNext ImportMemoryWin32HandleInfoNV where
  getNext s = next (s :: ImportMemoryWin32HandleInfoNV)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV where
  withCStruct = withCStructExportMemoryWin32HandleInfoNV
instance FromCStruct ExportMemoryWin32HandleInfoNV VkExportMemoryWin32HandleInfoNV where
  fromCStruct = fromCStructExportMemoryWin32HandleInfoNV
instance HasNext ExportMemoryWin32HandleInfoNV where
  getNext s = next (s :: ExportMemoryWin32HandleInfoNV)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct Win32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV where
  withCStruct = withCStructWin32KeyedMutexAcquireReleaseInfoNV
instance FromCStruct Win32KeyedMutexAcquireReleaseInfoNV VkWin32KeyedMutexAcquireReleaseInfoNV where
  fromCStruct = fromCStructWin32KeyedMutexAcquireReleaseInfoNV
instance HasNext Win32KeyedMutexAcquireReleaseInfoNV where
  getNext s = next (s :: Win32KeyedMutexAcquireReleaseInfoNV)
#endif
instance ToCStruct DeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX where
  withCStruct = withCStructDeviceGeneratedCommandsFeaturesNVX
instance FromCStruct DeviceGeneratedCommandsFeaturesNVX VkDeviceGeneratedCommandsFeaturesNVX where
  fromCStruct = fromCStructDeviceGeneratedCommandsFeaturesNVX
instance HasNext DeviceGeneratedCommandsFeaturesNVX where
  getNext s = next (s :: DeviceGeneratedCommandsFeaturesNVX)
instance ToCStruct DeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX where
  withCStruct = withCStructDeviceGeneratedCommandsLimitsNVX
instance FromCStruct DeviceGeneratedCommandsLimitsNVX VkDeviceGeneratedCommandsLimitsNVX where
  fromCStruct = fromCStructDeviceGeneratedCommandsLimitsNVX
instance HasNext DeviceGeneratedCommandsLimitsNVX where
  getNext s = next (s :: DeviceGeneratedCommandsLimitsNVX)
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
instance HasNext IndirectCommandsLayoutCreateInfoNVX where
  getNext s = next (s :: IndirectCommandsLayoutCreateInfoNVX)
instance ToCStruct CmdProcessCommandsInfoNVX VkCmdProcessCommandsInfoNVX where
  withCStruct = withCStructCmdProcessCommandsInfoNVX
-- No FromCStruct instance for VkCmdProcessCommandsInfoNVX as it contains a dispatchable handle
instance HasNext CmdProcessCommandsInfoNVX where
  getNext s = next (s :: CmdProcessCommandsInfoNVX)
instance ToCStruct CmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX where
  withCStruct = withCStructCmdReserveSpaceForCommandsInfoNVX
instance FromCStruct CmdReserveSpaceForCommandsInfoNVX VkCmdReserveSpaceForCommandsInfoNVX where
  fromCStruct = fromCStructCmdReserveSpaceForCommandsInfoNVX
instance HasNext CmdReserveSpaceForCommandsInfoNVX where
  getNext s = next (s :: CmdReserveSpaceForCommandsInfoNVX)
instance ToCStruct ObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX where
  withCStruct = withCStructObjectTableCreateInfoNVX
instance FromCStruct ObjectTableCreateInfoNVX VkObjectTableCreateInfoNVX where
  fromCStruct = fromCStructObjectTableCreateInfoNVX
instance HasNext ObjectTableCreateInfoNVX where
  getNext s = next (s :: ObjectTableCreateInfoNVX)
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
instance HasNext PhysicalDeviceFeatures2 where
  getNext s = next (s :: PhysicalDeviceFeatures2)
instance ToCStruct PhysicalDeviceProperties2 VkPhysicalDeviceProperties2 where
  withCStruct = withCStructPhysicalDeviceProperties2
instance FromCStruct PhysicalDeviceProperties2 VkPhysicalDeviceProperties2 where
  fromCStruct = fromCStructPhysicalDeviceProperties2
instance HasNext PhysicalDeviceProperties2 where
  getNext s = next (s :: PhysicalDeviceProperties2)
instance ToCStruct FormatProperties2 VkFormatProperties2 where
  withCStruct = withCStructFormatProperties2
instance FromCStruct FormatProperties2 VkFormatProperties2 where
  fromCStruct = fromCStructFormatProperties2
instance HasNext FormatProperties2 where
  getNext s = next (s :: FormatProperties2)
instance ToCStruct ImageFormatProperties2 VkImageFormatProperties2 where
  withCStruct = withCStructImageFormatProperties2
instance FromCStruct ImageFormatProperties2 VkImageFormatProperties2 where
  fromCStruct = fromCStructImageFormatProperties2
instance HasNext ImageFormatProperties2 where
  getNext s = next (s :: ImageFormatProperties2)
instance ToCStruct PhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 where
  withCStruct = withCStructPhysicalDeviceImageFormatInfo2
instance FromCStruct PhysicalDeviceImageFormatInfo2 VkPhysicalDeviceImageFormatInfo2 where
  fromCStruct = fromCStructPhysicalDeviceImageFormatInfo2
instance HasNext PhysicalDeviceImageFormatInfo2 where
  getNext s = next (s :: PhysicalDeviceImageFormatInfo2)
instance ToCStruct QueueFamilyProperties2 VkQueueFamilyProperties2 where
  withCStruct = withCStructQueueFamilyProperties2
instance FromCStruct QueueFamilyProperties2 VkQueueFamilyProperties2 where
  fromCStruct = fromCStructQueueFamilyProperties2
instance HasNext QueueFamilyProperties2 where
  getNext s = next (s :: QueueFamilyProperties2)
instance ToCStruct PhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 where
  withCStruct = withCStructPhysicalDeviceMemoryProperties2
instance FromCStruct PhysicalDeviceMemoryProperties2 VkPhysicalDeviceMemoryProperties2 where
  fromCStruct = fromCStructPhysicalDeviceMemoryProperties2
instance HasNext PhysicalDeviceMemoryProperties2 where
  getNext s = next (s :: PhysicalDeviceMemoryProperties2)
instance ToCStruct SparseImageFormatProperties2 VkSparseImageFormatProperties2 where
  withCStruct = withCStructSparseImageFormatProperties2
instance FromCStruct SparseImageFormatProperties2 VkSparseImageFormatProperties2 where
  fromCStruct = fromCStructSparseImageFormatProperties2
instance HasNext SparseImageFormatProperties2 where
  getNext s = next (s :: SparseImageFormatProperties2)
instance ToCStruct PhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 where
  withCStruct = withCStructPhysicalDeviceSparseImageFormatInfo2
instance FromCStruct PhysicalDeviceSparseImageFormatInfo2 VkPhysicalDeviceSparseImageFormatInfo2 where
  fromCStruct = fromCStructPhysicalDeviceSparseImageFormatInfo2
instance HasNext PhysicalDeviceSparseImageFormatInfo2 where
  getNext s = next (s :: PhysicalDeviceSparseImageFormatInfo2)
instance ToCStruct PhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR where
  withCStruct = withCStructPhysicalDevicePushDescriptorPropertiesKHR
instance FromCStruct PhysicalDevicePushDescriptorPropertiesKHR VkPhysicalDevicePushDescriptorPropertiesKHR where
  fromCStruct = fromCStructPhysicalDevicePushDescriptorPropertiesKHR
instance HasNext PhysicalDevicePushDescriptorPropertiesKHR where
  getNext s = next (s :: PhysicalDevicePushDescriptorPropertiesKHR)
instance ToCStruct ConformanceVersionKHR VkConformanceVersionKHR where
  withCStruct = withCStructConformanceVersionKHR
instance FromCStruct ConformanceVersionKHR VkConformanceVersionKHR where
  fromCStruct = fromCStructConformanceVersionKHR

instance ToCStruct PhysicalDeviceDriverPropertiesKHR VkPhysicalDeviceDriverPropertiesKHR where
  withCStruct = withCStructPhysicalDeviceDriverPropertiesKHR
instance FromCStruct PhysicalDeviceDriverPropertiesKHR VkPhysicalDeviceDriverPropertiesKHR where
  fromCStruct = fromCStructPhysicalDeviceDriverPropertiesKHR
instance HasNext PhysicalDeviceDriverPropertiesKHR where
  getNext s = next (s :: PhysicalDeviceDriverPropertiesKHR)
instance ToCStruct PresentRegionsKHR VkPresentRegionsKHR where
  withCStruct = withCStructPresentRegionsKHR
instance FromCStruct PresentRegionsKHR VkPresentRegionsKHR where
  fromCStruct = fromCStructPresentRegionsKHR
instance HasNext PresentRegionsKHR where
  getNext s = next (s :: PresentRegionsKHR)
instance ToCStruct PresentRegionKHR VkPresentRegionKHR where
  withCStruct = withCStructPresentRegionKHR
instance FromCStruct PresentRegionKHR VkPresentRegionKHR where
  fromCStruct = fromCStructPresentRegionKHR

instance ToCStruct RectLayerKHR VkRectLayerKHR where
  withCStruct = withCStructRectLayerKHR
instance FromCStruct RectLayerKHR VkRectLayerKHR where
  fromCStruct = fromCStructRectLayerKHR

instance ToCStruct PhysicalDeviceVariablePointersFeatures VkPhysicalDeviceVariablePointersFeatures where
  withCStruct = withCStructPhysicalDeviceVariablePointersFeatures
instance FromCStruct PhysicalDeviceVariablePointersFeatures VkPhysicalDeviceVariablePointersFeatures where
  fromCStruct = fromCStructPhysicalDeviceVariablePointersFeatures
instance HasNext PhysicalDeviceVariablePointersFeatures where
  getNext s = next (s :: PhysicalDeviceVariablePointersFeatures)
instance ToCStruct ExternalMemoryProperties VkExternalMemoryProperties where
  withCStruct = withCStructExternalMemoryProperties
instance FromCStruct ExternalMemoryProperties VkExternalMemoryProperties where
  fromCStruct = fromCStructExternalMemoryProperties

instance ToCStruct PhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo where
  withCStruct = withCStructPhysicalDeviceExternalImageFormatInfo
instance FromCStruct PhysicalDeviceExternalImageFormatInfo VkPhysicalDeviceExternalImageFormatInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalImageFormatInfo
instance HasNext PhysicalDeviceExternalImageFormatInfo where
  getNext s = next (s :: PhysicalDeviceExternalImageFormatInfo)
instance ToCStruct ExternalImageFormatProperties VkExternalImageFormatProperties where
  withCStruct = withCStructExternalImageFormatProperties
instance FromCStruct ExternalImageFormatProperties VkExternalImageFormatProperties where
  fromCStruct = fromCStructExternalImageFormatProperties
instance HasNext ExternalImageFormatProperties where
  getNext s = next (s :: ExternalImageFormatProperties)
instance ToCStruct PhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo where
  withCStruct = withCStructPhysicalDeviceExternalBufferInfo
instance FromCStruct PhysicalDeviceExternalBufferInfo VkPhysicalDeviceExternalBufferInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalBufferInfo
instance HasNext PhysicalDeviceExternalBufferInfo where
  getNext s = next (s :: PhysicalDeviceExternalBufferInfo)
instance ToCStruct ExternalBufferProperties VkExternalBufferProperties where
  withCStruct = withCStructExternalBufferProperties
instance FromCStruct ExternalBufferProperties VkExternalBufferProperties where
  fromCStruct = fromCStructExternalBufferProperties
instance HasNext ExternalBufferProperties where
  getNext s = next (s :: ExternalBufferProperties)
instance ToCStruct PhysicalDeviceIDProperties VkPhysicalDeviceIDProperties where
  withCStruct = withCStructPhysicalDeviceIDProperties
instance FromCStruct PhysicalDeviceIDProperties VkPhysicalDeviceIDProperties where
  fromCStruct = fromCStructPhysicalDeviceIDProperties
instance HasNext PhysicalDeviceIDProperties where
  getNext s = next (s :: PhysicalDeviceIDProperties)
instance ToCStruct ExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo where
  withCStruct = withCStructExternalMemoryImageCreateInfo
instance FromCStruct ExternalMemoryImageCreateInfo VkExternalMemoryImageCreateInfo where
  fromCStruct = fromCStructExternalMemoryImageCreateInfo
instance HasNext ExternalMemoryImageCreateInfo where
  getNext s = next (s :: ExternalMemoryImageCreateInfo)
instance ToCStruct ExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo where
  withCStruct = withCStructExternalMemoryBufferCreateInfo
instance FromCStruct ExternalMemoryBufferCreateInfo VkExternalMemoryBufferCreateInfo where
  fromCStruct = fromCStructExternalMemoryBufferCreateInfo
instance HasNext ExternalMemoryBufferCreateInfo where
  getNext s = next (s :: ExternalMemoryBufferCreateInfo)
instance ToCStruct ExportMemoryAllocateInfo VkExportMemoryAllocateInfo where
  withCStruct = withCStructExportMemoryAllocateInfo
instance FromCStruct ExportMemoryAllocateInfo VkExportMemoryAllocateInfo where
  fromCStruct = fromCStructExportMemoryAllocateInfo
instance HasNext ExportMemoryAllocateInfo where
  getNext s = next (s :: ExportMemoryAllocateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR where
  withCStruct = withCStructImportMemoryWin32HandleInfoKHR
instance FromCStruct ImportMemoryWin32HandleInfoKHR VkImportMemoryWin32HandleInfoKHR where
  fromCStruct = fromCStructImportMemoryWin32HandleInfoKHR
instance HasNext ImportMemoryWin32HandleInfoKHR where
  getNext s = next (s :: ImportMemoryWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR where
  withCStruct = withCStructExportMemoryWin32HandleInfoKHR
instance FromCStruct ExportMemoryWin32HandleInfoKHR VkExportMemoryWin32HandleInfoKHR where
  fromCStruct = fromCStructExportMemoryWin32HandleInfoKHR
instance HasNext ExportMemoryWin32HandleInfoKHR where
  getNext s = next (s :: ExportMemoryWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct MemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR where
  withCStruct = withCStructMemoryWin32HandlePropertiesKHR
instance FromCStruct MemoryWin32HandlePropertiesKHR VkMemoryWin32HandlePropertiesKHR where
  fromCStruct = fromCStructMemoryWin32HandlePropertiesKHR
instance HasNext MemoryWin32HandlePropertiesKHR where
  getNext s = next (s :: MemoryWin32HandlePropertiesKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct MemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR where
  withCStruct = withCStructMemoryGetWin32HandleInfoKHR
instance FromCStruct MemoryGetWin32HandleInfoKHR VkMemoryGetWin32HandleInfoKHR where
  fromCStruct = fromCStructMemoryGetWin32HandleInfoKHR
instance HasNext MemoryGetWin32HandleInfoKHR where
  getNext s = next (s :: MemoryGetWin32HandleInfoKHR)
#endif
instance ToCStruct ImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR where
  withCStruct = withCStructImportMemoryFdInfoKHR
instance FromCStruct ImportMemoryFdInfoKHR VkImportMemoryFdInfoKHR where
  fromCStruct = fromCStructImportMemoryFdInfoKHR
instance HasNext ImportMemoryFdInfoKHR where
  getNext s = next (s :: ImportMemoryFdInfoKHR)
instance ToCStruct MemoryFdPropertiesKHR VkMemoryFdPropertiesKHR where
  withCStruct = withCStructMemoryFdPropertiesKHR
instance FromCStruct MemoryFdPropertiesKHR VkMemoryFdPropertiesKHR where
  fromCStruct = fromCStructMemoryFdPropertiesKHR
instance HasNext MemoryFdPropertiesKHR where
  getNext s = next (s :: MemoryFdPropertiesKHR)
instance ToCStruct MemoryGetFdInfoKHR VkMemoryGetFdInfoKHR where
  withCStruct = withCStructMemoryGetFdInfoKHR
instance FromCStruct MemoryGetFdInfoKHR VkMemoryGetFdInfoKHR where
  fromCStruct = fromCStructMemoryGetFdInfoKHR
instance HasNext MemoryGetFdInfoKHR where
  getNext s = next (s :: MemoryGetFdInfoKHR)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct Win32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR where
  withCStruct = withCStructWin32KeyedMutexAcquireReleaseInfoKHR
instance FromCStruct Win32KeyedMutexAcquireReleaseInfoKHR VkWin32KeyedMutexAcquireReleaseInfoKHR where
  fromCStruct = fromCStructWin32KeyedMutexAcquireReleaseInfoKHR
instance HasNext Win32KeyedMutexAcquireReleaseInfoKHR where
  getNext s = next (s :: Win32KeyedMutexAcquireReleaseInfoKHR)
#endif
instance ToCStruct PhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo where
  withCStruct = withCStructPhysicalDeviceExternalSemaphoreInfo
instance FromCStruct PhysicalDeviceExternalSemaphoreInfo VkPhysicalDeviceExternalSemaphoreInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalSemaphoreInfo
instance HasNext PhysicalDeviceExternalSemaphoreInfo where
  getNext s = next (s :: PhysicalDeviceExternalSemaphoreInfo)
instance ToCStruct ExternalSemaphoreProperties VkExternalSemaphoreProperties where
  withCStruct = withCStructExternalSemaphoreProperties
instance FromCStruct ExternalSemaphoreProperties VkExternalSemaphoreProperties where
  fromCStruct = fromCStructExternalSemaphoreProperties
instance HasNext ExternalSemaphoreProperties where
  getNext s = next (s :: ExternalSemaphoreProperties)
instance ToCStruct ExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo where
  withCStruct = withCStructExportSemaphoreCreateInfo
instance FromCStruct ExportSemaphoreCreateInfo VkExportSemaphoreCreateInfo where
  fromCStruct = fromCStructExportSemaphoreCreateInfo
instance HasNext ExportSemaphoreCreateInfo where
  getNext s = next (s :: ExportSemaphoreCreateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR where
  withCStruct = withCStructImportSemaphoreWin32HandleInfoKHR
instance FromCStruct ImportSemaphoreWin32HandleInfoKHR VkImportSemaphoreWin32HandleInfoKHR where
  fromCStruct = fromCStructImportSemaphoreWin32HandleInfoKHR
instance HasNext ImportSemaphoreWin32HandleInfoKHR where
  getNext s = next (s :: ImportSemaphoreWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR where
  withCStruct = withCStructExportSemaphoreWin32HandleInfoKHR
instance FromCStruct ExportSemaphoreWin32HandleInfoKHR VkExportSemaphoreWin32HandleInfoKHR where
  fromCStruct = fromCStructExportSemaphoreWin32HandleInfoKHR
instance HasNext ExportSemaphoreWin32HandleInfoKHR where
  getNext s = next (s :: ExportSemaphoreWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct D3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR where
  withCStruct = withCStructD3D12FenceSubmitInfoKHR
instance FromCStruct D3D12FenceSubmitInfoKHR VkD3D12FenceSubmitInfoKHR where
  fromCStruct = fromCStructD3D12FenceSubmitInfoKHR
instance HasNext D3D12FenceSubmitInfoKHR where
  getNext s = next (s :: D3D12FenceSubmitInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct SemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR where
  withCStruct = withCStructSemaphoreGetWin32HandleInfoKHR
instance FromCStruct SemaphoreGetWin32HandleInfoKHR VkSemaphoreGetWin32HandleInfoKHR where
  fromCStruct = fromCStructSemaphoreGetWin32HandleInfoKHR
instance HasNext SemaphoreGetWin32HandleInfoKHR where
  getNext s = next (s :: SemaphoreGetWin32HandleInfoKHR)
#endif
instance ToCStruct ImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR where
  withCStruct = withCStructImportSemaphoreFdInfoKHR
instance FromCStruct ImportSemaphoreFdInfoKHR VkImportSemaphoreFdInfoKHR where
  fromCStruct = fromCStructImportSemaphoreFdInfoKHR
instance HasNext ImportSemaphoreFdInfoKHR where
  getNext s = next (s :: ImportSemaphoreFdInfoKHR)
instance ToCStruct SemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR where
  withCStruct = withCStructSemaphoreGetFdInfoKHR
instance FromCStruct SemaphoreGetFdInfoKHR VkSemaphoreGetFdInfoKHR where
  fromCStruct = fromCStructSemaphoreGetFdInfoKHR
instance HasNext SemaphoreGetFdInfoKHR where
  getNext s = next (s :: SemaphoreGetFdInfoKHR)
instance ToCStruct PhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo where
  withCStruct = withCStructPhysicalDeviceExternalFenceInfo
instance FromCStruct PhysicalDeviceExternalFenceInfo VkPhysicalDeviceExternalFenceInfo where
  fromCStruct = fromCStructPhysicalDeviceExternalFenceInfo
instance HasNext PhysicalDeviceExternalFenceInfo where
  getNext s = next (s :: PhysicalDeviceExternalFenceInfo)
instance ToCStruct ExternalFenceProperties VkExternalFenceProperties where
  withCStruct = withCStructExternalFenceProperties
instance FromCStruct ExternalFenceProperties VkExternalFenceProperties where
  fromCStruct = fromCStructExternalFenceProperties
instance HasNext ExternalFenceProperties where
  getNext s = next (s :: ExternalFenceProperties)
instance ToCStruct ExportFenceCreateInfo VkExportFenceCreateInfo where
  withCStruct = withCStructExportFenceCreateInfo
instance FromCStruct ExportFenceCreateInfo VkExportFenceCreateInfo where
  fromCStruct = fromCStructExportFenceCreateInfo
instance HasNext ExportFenceCreateInfo where
  getNext s = next (s :: ExportFenceCreateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR where
  withCStruct = withCStructImportFenceWin32HandleInfoKHR
instance FromCStruct ImportFenceWin32HandleInfoKHR VkImportFenceWin32HandleInfoKHR where
  fromCStruct = fromCStructImportFenceWin32HandleInfoKHR
instance HasNext ImportFenceWin32HandleInfoKHR where
  getNext s = next (s :: ImportFenceWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct ExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR where
  withCStruct = withCStructExportFenceWin32HandleInfoKHR
instance FromCStruct ExportFenceWin32HandleInfoKHR VkExportFenceWin32HandleInfoKHR where
  fromCStruct = fromCStructExportFenceWin32HandleInfoKHR
instance HasNext ExportFenceWin32HandleInfoKHR where
  getNext s = next (s :: ExportFenceWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance ToCStruct FenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR where
  withCStruct = withCStructFenceGetWin32HandleInfoKHR
instance FromCStruct FenceGetWin32HandleInfoKHR VkFenceGetWin32HandleInfoKHR where
  fromCStruct = fromCStructFenceGetWin32HandleInfoKHR
instance HasNext FenceGetWin32HandleInfoKHR where
  getNext s = next (s :: FenceGetWin32HandleInfoKHR)
#endif
instance ToCStruct ImportFenceFdInfoKHR VkImportFenceFdInfoKHR where
  withCStruct = withCStructImportFenceFdInfoKHR
instance FromCStruct ImportFenceFdInfoKHR VkImportFenceFdInfoKHR where
  fromCStruct = fromCStructImportFenceFdInfoKHR
instance HasNext ImportFenceFdInfoKHR where
  getNext s = next (s :: ImportFenceFdInfoKHR)
instance ToCStruct FenceGetFdInfoKHR VkFenceGetFdInfoKHR where
  withCStruct = withCStructFenceGetFdInfoKHR
instance FromCStruct FenceGetFdInfoKHR VkFenceGetFdInfoKHR where
  fromCStruct = fromCStructFenceGetFdInfoKHR
instance HasNext FenceGetFdInfoKHR where
  getNext s = next (s :: FenceGetFdInfoKHR)
instance ToCStruct PhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures where
  withCStruct = withCStructPhysicalDeviceMultiviewFeatures
instance FromCStruct PhysicalDeviceMultiviewFeatures VkPhysicalDeviceMultiviewFeatures where
  fromCStruct = fromCStructPhysicalDeviceMultiviewFeatures
instance HasNext PhysicalDeviceMultiviewFeatures where
  getNext s = next (s :: PhysicalDeviceMultiviewFeatures)
instance ToCStruct PhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties where
  withCStruct = withCStructPhysicalDeviceMultiviewProperties
instance FromCStruct PhysicalDeviceMultiviewProperties VkPhysicalDeviceMultiviewProperties where
  fromCStruct = fromCStructPhysicalDeviceMultiviewProperties
instance HasNext PhysicalDeviceMultiviewProperties where
  getNext s = next (s :: PhysicalDeviceMultiviewProperties)
instance ToCStruct RenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo where
  withCStruct = withCStructRenderPassMultiviewCreateInfo
instance FromCStruct RenderPassMultiviewCreateInfo VkRenderPassMultiviewCreateInfo where
  fromCStruct = fromCStructRenderPassMultiviewCreateInfo
instance HasNext RenderPassMultiviewCreateInfo where
  getNext s = next (s :: RenderPassMultiviewCreateInfo)
instance ToCStruct SurfaceCapabilities2EXT VkSurfaceCapabilities2EXT where
  withCStruct = withCStructSurfaceCapabilities2EXT
instance FromCStruct SurfaceCapabilities2EXT VkSurfaceCapabilities2EXT where
  fromCStruct = fromCStructSurfaceCapabilities2EXT
instance HasNext SurfaceCapabilities2EXT where
  getNext s = next (s :: SurfaceCapabilities2EXT)
instance ToCStruct DisplayPowerInfoEXT VkDisplayPowerInfoEXT where
  withCStruct = withCStructDisplayPowerInfoEXT
instance FromCStruct DisplayPowerInfoEXT VkDisplayPowerInfoEXT where
  fromCStruct = fromCStructDisplayPowerInfoEXT
instance HasNext DisplayPowerInfoEXT where
  getNext s = next (s :: DisplayPowerInfoEXT)
instance ToCStruct DeviceEventInfoEXT VkDeviceEventInfoEXT where
  withCStruct = withCStructDeviceEventInfoEXT
instance FromCStruct DeviceEventInfoEXT VkDeviceEventInfoEXT where
  fromCStruct = fromCStructDeviceEventInfoEXT
instance HasNext DeviceEventInfoEXT where
  getNext s = next (s :: DeviceEventInfoEXT)
instance ToCStruct DisplayEventInfoEXT VkDisplayEventInfoEXT where
  withCStruct = withCStructDisplayEventInfoEXT
instance FromCStruct DisplayEventInfoEXT VkDisplayEventInfoEXT where
  fromCStruct = fromCStructDisplayEventInfoEXT
instance HasNext DisplayEventInfoEXT where
  getNext s = next (s :: DisplayEventInfoEXT)
instance ToCStruct SwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT where
  withCStruct = withCStructSwapchainCounterCreateInfoEXT
instance FromCStruct SwapchainCounterCreateInfoEXT VkSwapchainCounterCreateInfoEXT where
  fromCStruct = fromCStructSwapchainCounterCreateInfoEXT
instance HasNext SwapchainCounterCreateInfoEXT where
  getNext s = next (s :: SwapchainCounterCreateInfoEXT)
instance ToCStruct PhysicalDeviceGroupProperties VkPhysicalDeviceGroupProperties where
  withCStruct = withCStructPhysicalDeviceGroupProperties
-- No FromCStruct instance for VkPhysicalDeviceGroupProperties as it contains a dispatchable handle
instance HasNext PhysicalDeviceGroupProperties where
  getNext s = next (s :: PhysicalDeviceGroupProperties)
instance ToCStruct MemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo where
  withCStruct = withCStructMemoryAllocateFlagsInfo
instance FromCStruct MemoryAllocateFlagsInfo VkMemoryAllocateFlagsInfo where
  fromCStruct = fromCStructMemoryAllocateFlagsInfo
instance HasNext MemoryAllocateFlagsInfo where
  getNext s = next (s :: MemoryAllocateFlagsInfo)
instance ToCStruct BindBufferMemoryInfo VkBindBufferMemoryInfo where
  withCStruct = withCStructBindBufferMemoryInfo
instance FromCStruct BindBufferMemoryInfo VkBindBufferMemoryInfo where
  fromCStruct = fromCStructBindBufferMemoryInfo
instance HasNext BindBufferMemoryInfo where
  getNext s = next (s :: BindBufferMemoryInfo)
instance ToCStruct BindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo where
  withCStruct = withCStructBindBufferMemoryDeviceGroupInfo
instance FromCStruct BindBufferMemoryDeviceGroupInfo VkBindBufferMemoryDeviceGroupInfo where
  fromCStruct = fromCStructBindBufferMemoryDeviceGroupInfo
instance HasNext BindBufferMemoryDeviceGroupInfo where
  getNext s = next (s :: BindBufferMemoryDeviceGroupInfo)
instance ToCStruct BindImageMemoryInfo VkBindImageMemoryInfo where
  withCStruct = withCStructBindImageMemoryInfo
instance FromCStruct BindImageMemoryInfo VkBindImageMemoryInfo where
  fromCStruct = fromCStructBindImageMemoryInfo
instance HasNext BindImageMemoryInfo where
  getNext s = next (s :: BindImageMemoryInfo)
instance ToCStruct BindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo where
  withCStruct = withCStructBindImageMemoryDeviceGroupInfo
instance FromCStruct BindImageMemoryDeviceGroupInfo VkBindImageMemoryDeviceGroupInfo where
  fromCStruct = fromCStructBindImageMemoryDeviceGroupInfo
instance HasNext BindImageMemoryDeviceGroupInfo where
  getNext s = next (s :: BindImageMemoryDeviceGroupInfo)
instance ToCStruct DeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo where
  withCStruct = withCStructDeviceGroupRenderPassBeginInfo
instance FromCStruct DeviceGroupRenderPassBeginInfo VkDeviceGroupRenderPassBeginInfo where
  fromCStruct = fromCStructDeviceGroupRenderPassBeginInfo
instance HasNext DeviceGroupRenderPassBeginInfo where
  getNext s = next (s :: DeviceGroupRenderPassBeginInfo)
instance ToCStruct DeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo where
  withCStruct = withCStructDeviceGroupCommandBufferBeginInfo
instance FromCStruct DeviceGroupCommandBufferBeginInfo VkDeviceGroupCommandBufferBeginInfo where
  fromCStruct = fromCStructDeviceGroupCommandBufferBeginInfo
instance HasNext DeviceGroupCommandBufferBeginInfo where
  getNext s = next (s :: DeviceGroupCommandBufferBeginInfo)
instance ToCStruct DeviceGroupSubmitInfo VkDeviceGroupSubmitInfo where
  withCStruct = withCStructDeviceGroupSubmitInfo
instance FromCStruct DeviceGroupSubmitInfo VkDeviceGroupSubmitInfo where
  fromCStruct = fromCStructDeviceGroupSubmitInfo
instance HasNext DeviceGroupSubmitInfo where
  getNext s = next (s :: DeviceGroupSubmitInfo)
instance ToCStruct DeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo where
  withCStruct = withCStructDeviceGroupBindSparseInfo
instance FromCStruct DeviceGroupBindSparseInfo VkDeviceGroupBindSparseInfo where
  fromCStruct = fromCStructDeviceGroupBindSparseInfo
instance HasNext DeviceGroupBindSparseInfo where
  getNext s = next (s :: DeviceGroupBindSparseInfo)
instance ToCStruct DeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR where
  withCStruct = withCStructDeviceGroupPresentCapabilitiesKHR
instance FromCStruct DeviceGroupPresentCapabilitiesKHR VkDeviceGroupPresentCapabilitiesKHR where
  fromCStruct = fromCStructDeviceGroupPresentCapabilitiesKHR
instance HasNext DeviceGroupPresentCapabilitiesKHR where
  getNext s = next (s :: DeviceGroupPresentCapabilitiesKHR)
instance ToCStruct ImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR where
  withCStruct = withCStructImageSwapchainCreateInfoKHR
instance FromCStruct ImageSwapchainCreateInfoKHR VkImageSwapchainCreateInfoKHR where
  fromCStruct = fromCStructImageSwapchainCreateInfoKHR
instance HasNext ImageSwapchainCreateInfoKHR where
  getNext s = next (s :: ImageSwapchainCreateInfoKHR)
instance ToCStruct BindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR where
  withCStruct = withCStructBindImageMemorySwapchainInfoKHR
instance FromCStruct BindImageMemorySwapchainInfoKHR VkBindImageMemorySwapchainInfoKHR where
  fromCStruct = fromCStructBindImageMemorySwapchainInfoKHR
instance HasNext BindImageMemorySwapchainInfoKHR where
  getNext s = next (s :: BindImageMemorySwapchainInfoKHR)
instance ToCStruct AcquireNextImageInfoKHR VkAcquireNextImageInfoKHR where
  withCStruct = withCStructAcquireNextImageInfoKHR
instance FromCStruct AcquireNextImageInfoKHR VkAcquireNextImageInfoKHR where
  fromCStruct = fromCStructAcquireNextImageInfoKHR
instance HasNext AcquireNextImageInfoKHR where
  getNext s = next (s :: AcquireNextImageInfoKHR)
instance ToCStruct DeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR where
  withCStruct = withCStructDeviceGroupPresentInfoKHR
instance FromCStruct DeviceGroupPresentInfoKHR VkDeviceGroupPresentInfoKHR where
  fromCStruct = fromCStructDeviceGroupPresentInfoKHR
instance HasNext DeviceGroupPresentInfoKHR where
  getNext s = next (s :: DeviceGroupPresentInfoKHR)
instance ToCStruct DeviceGroupDeviceCreateInfo VkDeviceGroupDeviceCreateInfo where
  withCStruct = withCStructDeviceGroupDeviceCreateInfo
-- No FromCStruct instance for VkDeviceGroupDeviceCreateInfo as it contains a dispatchable handle
instance HasNext DeviceGroupDeviceCreateInfo where
  getNext s = next (s :: DeviceGroupDeviceCreateInfo)
instance ToCStruct DeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR where
  withCStruct = withCStructDeviceGroupSwapchainCreateInfoKHR
instance FromCStruct DeviceGroupSwapchainCreateInfoKHR VkDeviceGroupSwapchainCreateInfoKHR where
  fromCStruct = fromCStructDeviceGroupSwapchainCreateInfoKHR
instance HasNext DeviceGroupSwapchainCreateInfoKHR where
  getNext s = next (s :: DeviceGroupSwapchainCreateInfoKHR)
instance ToCStruct DescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry where
  withCStruct = withCStructDescriptorUpdateTemplateEntry
instance FromCStruct DescriptorUpdateTemplateEntry VkDescriptorUpdateTemplateEntry where
  fromCStruct = fromCStructDescriptorUpdateTemplateEntry

instance ToCStruct DescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo where
  withCStruct = withCStructDescriptorUpdateTemplateCreateInfo
instance FromCStruct DescriptorUpdateTemplateCreateInfo VkDescriptorUpdateTemplateCreateInfo where
  fromCStruct = fromCStructDescriptorUpdateTemplateCreateInfo
instance HasNext DescriptorUpdateTemplateCreateInfo where
  getNext s = next (s :: DescriptorUpdateTemplateCreateInfo)
instance ToCStruct XYColorEXT VkXYColorEXT where
  withCStruct = withCStructXYColorEXT
instance FromCStruct XYColorEXT VkXYColorEXT where
  fromCStruct = fromCStructXYColorEXT

instance ToCStruct HdrMetadataEXT VkHdrMetadataEXT where
  withCStruct = withCStructHdrMetadataEXT
instance FromCStruct HdrMetadataEXT VkHdrMetadataEXT where
  fromCStruct = fromCStructHdrMetadataEXT
instance HasNext HdrMetadataEXT where
  getNext s = next (s :: HdrMetadataEXT)
instance ToCStruct DisplayNativeHdrSurfaceCapabilitiesAMD VkDisplayNativeHdrSurfaceCapabilitiesAMD where
  withCStruct = withCStructDisplayNativeHdrSurfaceCapabilitiesAMD
instance FromCStruct DisplayNativeHdrSurfaceCapabilitiesAMD VkDisplayNativeHdrSurfaceCapabilitiesAMD where
  fromCStruct = fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD
instance HasNext DisplayNativeHdrSurfaceCapabilitiesAMD where
  getNext s = next (s :: DisplayNativeHdrSurfaceCapabilitiesAMD)
instance ToCStruct SwapchainDisplayNativeHdrCreateInfoAMD VkSwapchainDisplayNativeHdrCreateInfoAMD where
  withCStruct = withCStructSwapchainDisplayNativeHdrCreateInfoAMD
instance FromCStruct SwapchainDisplayNativeHdrCreateInfoAMD VkSwapchainDisplayNativeHdrCreateInfoAMD where
  fromCStruct = fromCStructSwapchainDisplayNativeHdrCreateInfoAMD
instance HasNext SwapchainDisplayNativeHdrCreateInfoAMD where
  getNext s = next (s :: SwapchainDisplayNativeHdrCreateInfoAMD)
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
instance HasNext PresentTimesInfoGOOGLE where
  getNext s = next (s :: PresentTimesInfoGOOGLE)
instance ToCStruct PresentTimeGOOGLE VkPresentTimeGOOGLE where
  withCStruct = withCStructPresentTimeGOOGLE
instance FromCStruct PresentTimeGOOGLE VkPresentTimeGOOGLE where
  fromCStruct = fromCStructPresentTimeGOOGLE


#if VK_USE_PLATFORM_IOS_MVK
instance ToCStruct IOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK where
  withCStruct = withCStructIOSSurfaceCreateInfoMVK
instance FromCStruct IOSSurfaceCreateInfoMVK VkIOSSurfaceCreateInfoMVK where
  fromCStruct = fromCStructIOSSurfaceCreateInfoMVK
instance HasNext IOSSurfaceCreateInfoMVK where
  getNext s = next (s :: IOSSurfaceCreateInfoMVK)
#endif

#if VK_USE_PLATFORM_MACOS_MVK
instance ToCStruct MacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK where
  withCStruct = withCStructMacOSSurfaceCreateInfoMVK
instance FromCStruct MacOSSurfaceCreateInfoMVK VkMacOSSurfaceCreateInfoMVK where
  fromCStruct = fromCStructMacOSSurfaceCreateInfoMVK
instance HasNext MacOSSurfaceCreateInfoMVK where
  getNext s = next (s :: MacOSSurfaceCreateInfoMVK)
#endif

#if VK_USE_PLATFORM_METAL_EXT
instance ToCStruct MetalSurfaceCreateInfoEXT VkMetalSurfaceCreateInfoEXT where
  withCStruct = withCStructMetalSurfaceCreateInfoEXT
instance FromCStruct MetalSurfaceCreateInfoEXT VkMetalSurfaceCreateInfoEXT where
  fromCStruct = fromCStructMetalSurfaceCreateInfoEXT
instance HasNext MetalSurfaceCreateInfoEXT where
  getNext s = next (s :: MetalSurfaceCreateInfoEXT)
#endif
instance ToCStruct ViewportWScalingNV VkViewportWScalingNV where
  withCStruct = withCStructViewportWScalingNV
instance FromCStruct ViewportWScalingNV VkViewportWScalingNV where
  fromCStruct = fromCStructViewportWScalingNV

instance ToCStruct PipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportWScalingStateCreateInfoNV
instance FromCStruct PipelineViewportWScalingStateCreateInfoNV VkPipelineViewportWScalingStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportWScalingStateCreateInfoNV
instance HasNext PipelineViewportWScalingStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportWScalingStateCreateInfoNV)
instance ToCStruct ViewportSwizzleNV VkViewportSwizzleNV where
  withCStruct = withCStructViewportSwizzleNV
instance FromCStruct ViewportSwizzleNV VkViewportSwizzleNV where
  fromCStruct = fromCStructViewportSwizzleNV

instance ToCStruct PipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportSwizzleStateCreateInfoNV
instance FromCStruct PipelineViewportSwizzleStateCreateInfoNV VkPipelineViewportSwizzleStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportSwizzleStateCreateInfoNV
instance HasNext PipelineViewportSwizzleStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportSwizzleStateCreateInfoNV)
instance ToCStruct PhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  withCStruct = withCStructPhysicalDeviceDiscardRectanglePropertiesEXT
instance FromCStruct PhysicalDeviceDiscardRectanglePropertiesEXT VkPhysicalDeviceDiscardRectanglePropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceDiscardRectanglePropertiesEXT
instance HasNext PhysicalDeviceDiscardRectanglePropertiesEXT where
  getNext s = next (s :: PhysicalDeviceDiscardRectanglePropertiesEXT)
instance ToCStruct PipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT where
  withCStruct = withCStructPipelineDiscardRectangleStateCreateInfoEXT
instance FromCStruct PipelineDiscardRectangleStateCreateInfoEXT VkPipelineDiscardRectangleStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineDiscardRectangleStateCreateInfoEXT
instance HasNext PipelineDiscardRectangleStateCreateInfoEXT where
  getNext s = next (s :: PipelineDiscardRectangleStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  withCStruct = withCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
instance FromCStruct PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  fromCStruct = fromCStructPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX
instance HasNext PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  getNext s = next (s :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)
instance ToCStruct InputAttachmentAspectReference VkInputAttachmentAspectReference where
  withCStruct = withCStructInputAttachmentAspectReference
instance FromCStruct InputAttachmentAspectReference VkInputAttachmentAspectReference where
  fromCStruct = fromCStructInputAttachmentAspectReference

instance ToCStruct RenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo where
  withCStruct = withCStructRenderPassInputAttachmentAspectCreateInfo
instance FromCStruct RenderPassInputAttachmentAspectCreateInfo VkRenderPassInputAttachmentAspectCreateInfo where
  fromCStruct = fromCStructRenderPassInputAttachmentAspectCreateInfo
instance HasNext RenderPassInputAttachmentAspectCreateInfo where
  getNext s = next (s :: RenderPassInputAttachmentAspectCreateInfo)
instance ToCStruct PhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR where
  withCStruct = withCStructPhysicalDeviceSurfaceInfo2KHR
instance FromCStruct PhysicalDeviceSurfaceInfo2KHR VkPhysicalDeviceSurfaceInfo2KHR where
  fromCStruct = fromCStructPhysicalDeviceSurfaceInfo2KHR
instance HasNext PhysicalDeviceSurfaceInfo2KHR where
  getNext s = next (s :: PhysicalDeviceSurfaceInfo2KHR)
instance ToCStruct SurfaceCapabilities2KHR VkSurfaceCapabilities2KHR where
  withCStruct = withCStructSurfaceCapabilities2KHR
instance FromCStruct SurfaceCapabilities2KHR VkSurfaceCapabilities2KHR where
  fromCStruct = fromCStructSurfaceCapabilities2KHR
instance HasNext SurfaceCapabilities2KHR where
  getNext s = next (s :: SurfaceCapabilities2KHR)
instance ToCStruct SurfaceFormat2KHR VkSurfaceFormat2KHR where
  withCStruct = withCStructSurfaceFormat2KHR
instance FromCStruct SurfaceFormat2KHR VkSurfaceFormat2KHR where
  fromCStruct = fromCStructSurfaceFormat2KHR
instance HasNext SurfaceFormat2KHR where
  getNext s = next (s :: SurfaceFormat2KHR)
instance ToCStruct DisplayProperties2KHR VkDisplayProperties2KHR where
  withCStruct = withCStructDisplayProperties2KHR
instance FromCStruct DisplayProperties2KHR VkDisplayProperties2KHR where
  fromCStruct = fromCStructDisplayProperties2KHR
instance HasNext DisplayProperties2KHR where
  getNext s = next (s :: DisplayProperties2KHR)
instance ToCStruct DisplayPlaneProperties2KHR VkDisplayPlaneProperties2KHR where
  withCStruct = withCStructDisplayPlaneProperties2KHR
instance FromCStruct DisplayPlaneProperties2KHR VkDisplayPlaneProperties2KHR where
  fromCStruct = fromCStructDisplayPlaneProperties2KHR
instance HasNext DisplayPlaneProperties2KHR where
  getNext s = next (s :: DisplayPlaneProperties2KHR)
instance ToCStruct DisplayModeProperties2KHR VkDisplayModeProperties2KHR where
  withCStruct = withCStructDisplayModeProperties2KHR
instance FromCStruct DisplayModeProperties2KHR VkDisplayModeProperties2KHR where
  fromCStruct = fromCStructDisplayModeProperties2KHR
instance HasNext DisplayModeProperties2KHR where
  getNext s = next (s :: DisplayModeProperties2KHR)
instance ToCStruct DisplayPlaneInfo2KHR VkDisplayPlaneInfo2KHR where
  withCStruct = withCStructDisplayPlaneInfo2KHR
instance FromCStruct DisplayPlaneInfo2KHR VkDisplayPlaneInfo2KHR where
  fromCStruct = fromCStructDisplayPlaneInfo2KHR
instance HasNext DisplayPlaneInfo2KHR where
  getNext s = next (s :: DisplayPlaneInfo2KHR)
instance ToCStruct DisplayPlaneCapabilities2KHR VkDisplayPlaneCapabilities2KHR where
  withCStruct = withCStructDisplayPlaneCapabilities2KHR
instance FromCStruct DisplayPlaneCapabilities2KHR VkDisplayPlaneCapabilities2KHR where
  fromCStruct = fromCStructDisplayPlaneCapabilities2KHR
instance HasNext DisplayPlaneCapabilities2KHR where
  getNext s = next (s :: DisplayPlaneCapabilities2KHR)
instance ToCStruct SharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR where
  withCStruct = withCStructSharedPresentSurfaceCapabilitiesKHR
instance FromCStruct SharedPresentSurfaceCapabilitiesKHR VkSharedPresentSurfaceCapabilitiesKHR where
  fromCStruct = fromCStructSharedPresentSurfaceCapabilitiesKHR
instance HasNext SharedPresentSurfaceCapabilitiesKHR where
  getNext s = next (s :: SharedPresentSurfaceCapabilitiesKHR)
instance ToCStruct PhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures where
  withCStruct = withCStructPhysicalDevice16BitStorageFeatures
instance FromCStruct PhysicalDevice16BitStorageFeatures VkPhysicalDevice16BitStorageFeatures where
  fromCStruct = fromCStructPhysicalDevice16BitStorageFeatures
instance HasNext PhysicalDevice16BitStorageFeatures where
  getNext s = next (s :: PhysicalDevice16BitStorageFeatures)
instance ToCStruct PhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties where
  withCStruct = withCStructPhysicalDeviceSubgroupProperties
instance FromCStruct PhysicalDeviceSubgroupProperties VkPhysicalDeviceSubgroupProperties where
  fromCStruct = fromCStructPhysicalDeviceSubgroupProperties
instance HasNext PhysicalDeviceSubgroupProperties where
  getNext s = next (s :: PhysicalDeviceSubgroupProperties)
instance ToCStruct BufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 where
  withCStruct = withCStructBufferMemoryRequirementsInfo2
instance FromCStruct BufferMemoryRequirementsInfo2 VkBufferMemoryRequirementsInfo2 where
  fromCStruct = fromCStructBufferMemoryRequirementsInfo2
instance HasNext BufferMemoryRequirementsInfo2 where
  getNext s = next (s :: BufferMemoryRequirementsInfo2)
instance ToCStruct ImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 where
  withCStruct = withCStructImageMemoryRequirementsInfo2
instance FromCStruct ImageMemoryRequirementsInfo2 VkImageMemoryRequirementsInfo2 where
  fromCStruct = fromCStructImageMemoryRequirementsInfo2
instance HasNext ImageMemoryRequirementsInfo2 where
  getNext s = next (s :: ImageMemoryRequirementsInfo2)
instance ToCStruct ImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 where
  withCStruct = withCStructImageSparseMemoryRequirementsInfo2
instance FromCStruct ImageSparseMemoryRequirementsInfo2 VkImageSparseMemoryRequirementsInfo2 where
  fromCStruct = fromCStructImageSparseMemoryRequirementsInfo2
instance HasNext ImageSparseMemoryRequirementsInfo2 where
  getNext s = next (s :: ImageSparseMemoryRequirementsInfo2)
instance ToCStruct MemoryRequirements2 VkMemoryRequirements2 where
  withCStruct = withCStructMemoryRequirements2
instance FromCStruct MemoryRequirements2 VkMemoryRequirements2 where
  fromCStruct = fromCStructMemoryRequirements2
instance HasNext MemoryRequirements2 where
  getNext s = next (s :: MemoryRequirements2)
instance ToCStruct SparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 where
  withCStruct = withCStructSparseImageMemoryRequirements2
instance FromCStruct SparseImageMemoryRequirements2 VkSparseImageMemoryRequirements2 where
  fromCStruct = fromCStructSparseImageMemoryRequirements2
instance HasNext SparseImageMemoryRequirements2 where
  getNext s = next (s :: SparseImageMemoryRequirements2)
instance ToCStruct PhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties where
  withCStruct = withCStructPhysicalDevicePointClippingProperties
instance FromCStruct PhysicalDevicePointClippingProperties VkPhysicalDevicePointClippingProperties where
  fromCStruct = fromCStructPhysicalDevicePointClippingProperties
instance HasNext PhysicalDevicePointClippingProperties where
  getNext s = next (s :: PhysicalDevicePointClippingProperties)
instance ToCStruct MemoryDedicatedRequirements VkMemoryDedicatedRequirements where
  withCStruct = withCStructMemoryDedicatedRequirements
instance FromCStruct MemoryDedicatedRequirements VkMemoryDedicatedRequirements where
  fromCStruct = fromCStructMemoryDedicatedRequirements
instance HasNext MemoryDedicatedRequirements where
  getNext s = next (s :: MemoryDedicatedRequirements)
instance ToCStruct MemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo where
  withCStruct = withCStructMemoryDedicatedAllocateInfo
instance FromCStruct MemoryDedicatedAllocateInfo VkMemoryDedicatedAllocateInfo where
  fromCStruct = fromCStructMemoryDedicatedAllocateInfo
instance HasNext MemoryDedicatedAllocateInfo where
  getNext s = next (s :: MemoryDedicatedAllocateInfo)
instance ToCStruct ImageViewUsageCreateInfo VkImageViewUsageCreateInfo where
  withCStruct = withCStructImageViewUsageCreateInfo
instance FromCStruct ImageViewUsageCreateInfo VkImageViewUsageCreateInfo where
  fromCStruct = fromCStructImageViewUsageCreateInfo
instance HasNext ImageViewUsageCreateInfo where
  getNext s = next (s :: ImageViewUsageCreateInfo)
instance ToCStruct PipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo where
  withCStruct = withCStructPipelineTessellationDomainOriginStateCreateInfo
instance FromCStruct PipelineTessellationDomainOriginStateCreateInfo VkPipelineTessellationDomainOriginStateCreateInfo where
  fromCStruct = fromCStructPipelineTessellationDomainOriginStateCreateInfo
instance HasNext PipelineTessellationDomainOriginStateCreateInfo where
  getNext s = next (s :: PipelineTessellationDomainOriginStateCreateInfo)
instance ToCStruct SamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo where
  withCStruct = withCStructSamplerYcbcrConversionInfo
instance FromCStruct SamplerYcbcrConversionInfo VkSamplerYcbcrConversionInfo where
  fromCStruct = fromCStructSamplerYcbcrConversionInfo
instance HasNext SamplerYcbcrConversionInfo where
  getNext s = next (s :: SamplerYcbcrConversionInfo)
instance ToCStruct SamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo where
  withCStruct = withCStructSamplerYcbcrConversionCreateInfo
instance FromCStruct SamplerYcbcrConversionCreateInfo VkSamplerYcbcrConversionCreateInfo where
  fromCStruct = fromCStructSamplerYcbcrConversionCreateInfo
instance HasNext SamplerYcbcrConversionCreateInfo where
  getNext s = next (s :: SamplerYcbcrConversionCreateInfo)
instance ToCStruct BindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo where
  withCStruct = withCStructBindImagePlaneMemoryInfo
instance FromCStruct BindImagePlaneMemoryInfo VkBindImagePlaneMemoryInfo where
  fromCStruct = fromCStructBindImagePlaneMemoryInfo
instance HasNext BindImagePlaneMemoryInfo where
  getNext s = next (s :: BindImagePlaneMemoryInfo)
instance ToCStruct ImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo where
  withCStruct = withCStructImagePlaneMemoryRequirementsInfo
instance FromCStruct ImagePlaneMemoryRequirementsInfo VkImagePlaneMemoryRequirementsInfo where
  fromCStruct = fromCStructImagePlaneMemoryRequirementsInfo
instance HasNext ImagePlaneMemoryRequirementsInfo where
  getNext s = next (s :: ImagePlaneMemoryRequirementsInfo)
instance ToCStruct PhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  withCStruct = withCStructPhysicalDeviceSamplerYcbcrConversionFeatures
instance FromCStruct PhysicalDeviceSamplerYcbcrConversionFeatures VkPhysicalDeviceSamplerYcbcrConversionFeatures where
  fromCStruct = fromCStructPhysicalDeviceSamplerYcbcrConversionFeatures
instance HasNext PhysicalDeviceSamplerYcbcrConversionFeatures where
  getNext s = next (s :: PhysicalDeviceSamplerYcbcrConversionFeatures)
instance ToCStruct SamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties where
  withCStruct = withCStructSamplerYcbcrConversionImageFormatProperties
instance FromCStruct SamplerYcbcrConversionImageFormatProperties VkSamplerYcbcrConversionImageFormatProperties where
  fromCStruct = fromCStructSamplerYcbcrConversionImageFormatProperties
instance HasNext SamplerYcbcrConversionImageFormatProperties where
  getNext s = next (s :: SamplerYcbcrConversionImageFormatProperties)
instance ToCStruct TextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD where
  withCStruct = withCStructTextureLODGatherFormatPropertiesAMD
instance FromCStruct TextureLODGatherFormatPropertiesAMD VkTextureLODGatherFormatPropertiesAMD where
  fromCStruct = fromCStructTextureLODGatherFormatPropertiesAMD
instance HasNext TextureLODGatherFormatPropertiesAMD where
  getNext s = next (s :: TextureLODGatherFormatPropertiesAMD)
instance ToCStruct ConditionalRenderingBeginInfoEXT VkConditionalRenderingBeginInfoEXT where
  withCStruct = withCStructConditionalRenderingBeginInfoEXT
instance FromCStruct ConditionalRenderingBeginInfoEXT VkConditionalRenderingBeginInfoEXT where
  fromCStruct = fromCStructConditionalRenderingBeginInfoEXT
instance HasNext ConditionalRenderingBeginInfoEXT where
  getNext s = next (s :: ConditionalRenderingBeginInfoEXT)
instance ToCStruct ProtectedSubmitInfo VkProtectedSubmitInfo where
  withCStruct = withCStructProtectedSubmitInfo
instance FromCStruct ProtectedSubmitInfo VkProtectedSubmitInfo where
  fromCStruct = fromCStructProtectedSubmitInfo
instance HasNext ProtectedSubmitInfo where
  getNext s = next (s :: ProtectedSubmitInfo)
instance ToCStruct PhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures where
  withCStruct = withCStructPhysicalDeviceProtectedMemoryFeatures
instance FromCStruct PhysicalDeviceProtectedMemoryFeatures VkPhysicalDeviceProtectedMemoryFeatures where
  fromCStruct = fromCStructPhysicalDeviceProtectedMemoryFeatures
instance HasNext PhysicalDeviceProtectedMemoryFeatures where
  getNext s = next (s :: PhysicalDeviceProtectedMemoryFeatures)
instance ToCStruct PhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties where
  withCStruct = withCStructPhysicalDeviceProtectedMemoryProperties
instance FromCStruct PhysicalDeviceProtectedMemoryProperties VkPhysicalDeviceProtectedMemoryProperties where
  fromCStruct = fromCStructPhysicalDeviceProtectedMemoryProperties
instance HasNext PhysicalDeviceProtectedMemoryProperties where
  getNext s = next (s :: PhysicalDeviceProtectedMemoryProperties)
instance ToCStruct DeviceQueueInfo2 VkDeviceQueueInfo2 where
  withCStruct = withCStructDeviceQueueInfo2
instance FromCStruct DeviceQueueInfo2 VkDeviceQueueInfo2 where
  fromCStruct = fromCStructDeviceQueueInfo2
instance HasNext DeviceQueueInfo2 where
  getNext s = next (s :: DeviceQueueInfo2)
instance ToCStruct PipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV where
  withCStruct = withCStructPipelineCoverageToColorStateCreateInfoNV
instance FromCStruct PipelineCoverageToColorStateCreateInfoNV VkPipelineCoverageToColorStateCreateInfoNV where
  fromCStruct = fromCStructPipelineCoverageToColorStateCreateInfoNV
instance HasNext PipelineCoverageToColorStateCreateInfoNV where
  getNext s = next (s :: PipelineCoverageToColorStateCreateInfoNV)
instance ToCStruct PhysicalDeviceSamplerFilterMinmaxPropertiesEXT VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
instance FromCStruct PhysicalDeviceSamplerFilterMinmaxPropertiesEXT VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceSamplerFilterMinmaxPropertiesEXT
instance HasNext PhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)
instance ToCStruct SampleLocationEXT VkSampleLocationEXT where
  withCStruct = withCStructSampleLocationEXT
instance FromCStruct SampleLocationEXT VkSampleLocationEXT where
  fromCStruct = fromCStructSampleLocationEXT

instance ToCStruct SampleLocationsInfoEXT VkSampleLocationsInfoEXT where
  withCStruct = withCStructSampleLocationsInfoEXT
instance FromCStruct SampleLocationsInfoEXT VkSampleLocationsInfoEXT where
  fromCStruct = fromCStructSampleLocationsInfoEXT
instance HasNext SampleLocationsInfoEXT where
  getNext s = next (s :: SampleLocationsInfoEXT)
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
instance HasNext RenderPassSampleLocationsBeginInfoEXT where
  getNext s = next (s :: RenderPassSampleLocationsBeginInfoEXT)
instance ToCStruct PipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT where
  withCStruct = withCStructPipelineSampleLocationsStateCreateInfoEXT
instance FromCStruct PipelineSampleLocationsStateCreateInfoEXT VkPipelineSampleLocationsStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineSampleLocationsStateCreateInfoEXT
instance HasNext PipelineSampleLocationsStateCreateInfoEXT where
  getNext s = next (s :: PipelineSampleLocationsStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceSampleLocationsPropertiesEXT
instance FromCStruct PhysicalDeviceSampleLocationsPropertiesEXT VkPhysicalDeviceSampleLocationsPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceSampleLocationsPropertiesEXT
instance HasNext PhysicalDeviceSampleLocationsPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceSampleLocationsPropertiesEXT)
instance ToCStruct MultisamplePropertiesEXT VkMultisamplePropertiesEXT where
  withCStruct = withCStructMultisamplePropertiesEXT
instance FromCStruct MultisamplePropertiesEXT VkMultisamplePropertiesEXT where
  fromCStruct = fromCStructMultisamplePropertiesEXT
instance HasNext MultisamplePropertiesEXT where
  getNext s = next (s :: MultisamplePropertiesEXT)
instance ToCStruct SamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT where
  withCStruct = withCStructSamplerReductionModeCreateInfoEXT
instance FromCStruct SamplerReductionModeCreateInfoEXT VkSamplerReductionModeCreateInfoEXT where
  fromCStruct = fromCStructSamplerReductionModeCreateInfoEXT
instance HasNext SamplerReductionModeCreateInfoEXT where
  getNext s = next (s :: SamplerReductionModeCreateInfoEXT)
instance ToCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
instance FromCStruct PhysicalDeviceBlendOperationAdvancedFeaturesEXT VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceBlendOperationAdvancedFeaturesEXT
instance HasNext PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
instance ToCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
instance FromCStruct PhysicalDeviceBlendOperationAdvancedPropertiesEXT VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceBlendOperationAdvancedPropertiesEXT
instance HasNext PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
instance ToCStruct PipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  withCStruct = withCStructPipelineColorBlendAdvancedStateCreateInfoEXT
instance FromCStruct PipelineColorBlendAdvancedStateCreateInfoEXT VkPipelineColorBlendAdvancedStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineColorBlendAdvancedStateCreateInfoEXT
instance HasNext PipelineColorBlendAdvancedStateCreateInfoEXT where
  getNext s = next (s :: PipelineColorBlendAdvancedStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT VkPhysicalDeviceInlineUniformBlockFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceInlineUniformBlockFeaturesEXT
instance FromCStruct PhysicalDeviceInlineUniformBlockFeaturesEXT VkPhysicalDeviceInlineUniformBlockFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT
instance HasNext PhysicalDeviceInlineUniformBlockFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceInlineUniformBlockFeaturesEXT)
instance ToCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT VkPhysicalDeviceInlineUniformBlockPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceInlineUniformBlockPropertiesEXT
instance FromCStruct PhysicalDeviceInlineUniformBlockPropertiesEXT VkPhysicalDeviceInlineUniformBlockPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT
instance HasNext PhysicalDeviceInlineUniformBlockPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceInlineUniformBlockPropertiesEXT)
instance ToCStruct WriteDescriptorSetInlineUniformBlockEXT VkWriteDescriptorSetInlineUniformBlockEXT where
  withCStruct = withCStructWriteDescriptorSetInlineUniformBlockEXT
instance FromCStruct WriteDescriptorSetInlineUniformBlockEXT VkWriteDescriptorSetInlineUniformBlockEXT where
  fromCStruct = fromCStructWriteDescriptorSetInlineUniformBlockEXT
instance HasNext WriteDescriptorSetInlineUniformBlockEXT where
  getNext s = next (s :: WriteDescriptorSetInlineUniformBlockEXT)
instance ToCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT VkDescriptorPoolInlineUniformBlockCreateInfoEXT where
  withCStruct = withCStructDescriptorPoolInlineUniformBlockCreateInfoEXT
instance FromCStruct DescriptorPoolInlineUniformBlockCreateInfoEXT VkDescriptorPoolInlineUniformBlockCreateInfoEXT where
  fromCStruct = fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT
instance HasNext DescriptorPoolInlineUniformBlockCreateInfoEXT where
  getNext s = next (s :: DescriptorPoolInlineUniformBlockCreateInfoEXT)
instance ToCStruct PipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV where
  withCStruct = withCStructPipelineCoverageModulationStateCreateInfoNV
instance FromCStruct PipelineCoverageModulationStateCreateInfoNV VkPipelineCoverageModulationStateCreateInfoNV where
  fromCStruct = fromCStructPipelineCoverageModulationStateCreateInfoNV
instance HasNext PipelineCoverageModulationStateCreateInfoNV where
  getNext s = next (s :: PipelineCoverageModulationStateCreateInfoNV)
instance ToCStruct ImageFormatListCreateInfoKHR VkImageFormatListCreateInfoKHR where
  withCStruct = withCStructImageFormatListCreateInfoKHR
instance FromCStruct ImageFormatListCreateInfoKHR VkImageFormatListCreateInfoKHR where
  fromCStruct = fromCStructImageFormatListCreateInfoKHR
instance HasNext ImageFormatListCreateInfoKHR where
  getNext s = next (s :: ImageFormatListCreateInfoKHR)
instance ToCStruct ValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT where
  withCStruct = withCStructValidationCacheCreateInfoEXT
instance FromCStruct ValidationCacheCreateInfoEXT VkValidationCacheCreateInfoEXT where
  fromCStruct = fromCStructValidationCacheCreateInfoEXT
instance HasNext ValidationCacheCreateInfoEXT where
  getNext s = next (s :: ValidationCacheCreateInfoEXT)
instance ToCStruct ShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT where
  withCStruct = withCStructShaderModuleValidationCacheCreateInfoEXT
instance FromCStruct ShaderModuleValidationCacheCreateInfoEXT VkShaderModuleValidationCacheCreateInfoEXT where
  fromCStruct = fromCStructShaderModuleValidationCacheCreateInfoEXT
instance HasNext ShaderModuleValidationCacheCreateInfoEXT where
  getNext s = next (s :: ShaderModuleValidationCacheCreateInfoEXT)
instance ToCStruct PhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties where
  withCStruct = withCStructPhysicalDeviceMaintenance3Properties
instance FromCStruct PhysicalDeviceMaintenance3Properties VkPhysicalDeviceMaintenance3Properties where
  fromCStruct = fromCStructPhysicalDeviceMaintenance3Properties
instance HasNext PhysicalDeviceMaintenance3Properties where
  getNext s = next (s :: PhysicalDeviceMaintenance3Properties)
instance ToCStruct DescriptorSetLayoutSupport VkDescriptorSetLayoutSupport where
  withCStruct = withCStructDescriptorSetLayoutSupport
instance FromCStruct DescriptorSetLayoutSupport VkDescriptorSetLayoutSupport where
  fromCStruct = fromCStructDescriptorSetLayoutSupport
instance HasNext DescriptorSetLayoutSupport where
  getNext s = next (s :: DescriptorSetLayoutSupport)
instance ToCStruct PhysicalDeviceShaderDrawParametersFeatures VkPhysicalDeviceShaderDrawParametersFeatures where
  withCStruct = withCStructPhysicalDeviceShaderDrawParametersFeatures
instance FromCStruct PhysicalDeviceShaderDrawParametersFeatures VkPhysicalDeviceShaderDrawParametersFeatures where
  fromCStruct = fromCStructPhysicalDeviceShaderDrawParametersFeatures
instance HasNext PhysicalDeviceShaderDrawParametersFeatures where
  getNext s = next (s :: PhysicalDeviceShaderDrawParametersFeatures)
instance ToCStruct PhysicalDeviceFloat16Int8FeaturesKHR VkPhysicalDeviceFloat16Int8FeaturesKHR where
  withCStruct = withCStructPhysicalDeviceFloat16Int8FeaturesKHR
instance FromCStruct PhysicalDeviceFloat16Int8FeaturesKHR VkPhysicalDeviceFloat16Int8FeaturesKHR where
  fromCStruct = fromCStructPhysicalDeviceFloat16Int8FeaturesKHR
instance HasNext PhysicalDeviceFloat16Int8FeaturesKHR where
  getNext s = next (s :: PhysicalDeviceFloat16Int8FeaturesKHR)
instance ToCStruct PhysicalDeviceFloatControlsPropertiesKHR VkPhysicalDeviceFloatControlsPropertiesKHR where
  withCStruct = withCStructPhysicalDeviceFloatControlsPropertiesKHR
instance FromCStruct PhysicalDeviceFloatControlsPropertiesKHR VkPhysicalDeviceFloatControlsPropertiesKHR where
  fromCStruct = fromCStructPhysicalDeviceFloatControlsPropertiesKHR
instance HasNext PhysicalDeviceFloatControlsPropertiesKHR where
  getNext s = next (s :: PhysicalDeviceFloatControlsPropertiesKHR)
instance ToCStruct PhysicalDeviceHostQueryResetFeaturesEXT VkPhysicalDeviceHostQueryResetFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceHostQueryResetFeaturesEXT
instance FromCStruct PhysicalDeviceHostQueryResetFeaturesEXT VkPhysicalDeviceHostQueryResetFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceHostQueryResetFeaturesEXT
instance HasNext PhysicalDeviceHostQueryResetFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceHostQueryResetFeaturesEXT)
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
instance HasNext DeviceQueueGlobalPriorityCreateInfoEXT where
  getNext s = next (s :: DeviceQueueGlobalPriorityCreateInfoEXT)
instance ToCStruct DebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT where
  withCStruct = withCStructDebugUtilsObjectNameInfoEXT
instance FromCStruct DebugUtilsObjectNameInfoEXT VkDebugUtilsObjectNameInfoEXT where
  fromCStruct = fromCStructDebugUtilsObjectNameInfoEXT
instance HasNext DebugUtilsObjectNameInfoEXT where
  getNext s = next (s :: DebugUtilsObjectNameInfoEXT)
instance ToCStruct DebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT where
  withCStruct = withCStructDebugUtilsObjectTagInfoEXT
instance FromCStruct DebugUtilsObjectTagInfoEXT VkDebugUtilsObjectTagInfoEXT where
  fromCStruct = fromCStructDebugUtilsObjectTagInfoEXT
instance HasNext DebugUtilsObjectTagInfoEXT where
  getNext s = next (s :: DebugUtilsObjectTagInfoEXT)
instance ToCStruct DebugUtilsLabelEXT VkDebugUtilsLabelEXT where
  withCStruct = withCStructDebugUtilsLabelEXT
instance FromCStruct DebugUtilsLabelEXT VkDebugUtilsLabelEXT where
  fromCStruct = fromCStructDebugUtilsLabelEXT
instance HasNext DebugUtilsLabelEXT where
  getNext s = next (s :: DebugUtilsLabelEXT)
instance ToCStruct DebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT where
  withCStruct = withCStructDebugUtilsMessengerCreateInfoEXT
instance FromCStruct DebugUtilsMessengerCreateInfoEXT VkDebugUtilsMessengerCreateInfoEXT where
  fromCStruct = fromCStructDebugUtilsMessengerCreateInfoEXT
instance HasNext DebugUtilsMessengerCreateInfoEXT where
  getNext s = next (s :: DebugUtilsMessengerCreateInfoEXT)
instance ToCStruct DebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT where
  withCStruct = withCStructDebugUtilsMessengerCallbackDataEXT
instance FromCStruct DebugUtilsMessengerCallbackDataEXT VkDebugUtilsMessengerCallbackDataEXT where
  fromCStruct = fromCStructDebugUtilsMessengerCallbackDataEXT
instance HasNext DebugUtilsMessengerCallbackDataEXT where
  getNext s = next (s :: DebugUtilsMessengerCallbackDataEXT)
instance ToCStruct ImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT where
  withCStruct = withCStructImportMemoryHostPointerInfoEXT
instance FromCStruct ImportMemoryHostPointerInfoEXT VkImportMemoryHostPointerInfoEXT where
  fromCStruct = fromCStructImportMemoryHostPointerInfoEXT
instance HasNext ImportMemoryHostPointerInfoEXT where
  getNext s = next (s :: ImportMemoryHostPointerInfoEXT)
instance ToCStruct MemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT where
  withCStruct = withCStructMemoryHostPointerPropertiesEXT
instance FromCStruct MemoryHostPointerPropertiesEXT VkMemoryHostPointerPropertiesEXT where
  fromCStruct = fromCStructMemoryHostPointerPropertiesEXT
instance HasNext MemoryHostPointerPropertiesEXT where
  getNext s = next (s :: MemoryHostPointerPropertiesEXT)
instance ToCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
instance FromCStruct PhysicalDeviceExternalMemoryHostPropertiesEXT VkPhysicalDeviceExternalMemoryHostPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceExternalMemoryHostPropertiesEXT
instance HasNext PhysicalDeviceExternalMemoryHostPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceExternalMemoryHostPropertiesEXT)
instance ToCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
instance FromCStruct PhysicalDeviceConservativeRasterizationPropertiesEXT VkPhysicalDeviceConservativeRasterizationPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceConservativeRasterizationPropertiesEXT
instance HasNext PhysicalDeviceConservativeRasterizationPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceConservativeRasterizationPropertiesEXT)
instance ToCStruct CalibratedTimestampInfoEXT VkCalibratedTimestampInfoEXT where
  withCStruct = withCStructCalibratedTimestampInfoEXT
instance FromCStruct CalibratedTimestampInfoEXT VkCalibratedTimestampInfoEXT where
  fromCStruct = fromCStructCalibratedTimestampInfoEXT
instance HasNext CalibratedTimestampInfoEXT where
  getNext s = next (s :: CalibratedTimestampInfoEXT)
instance ToCStruct PhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD where
  withCStruct = withCStructPhysicalDeviceShaderCorePropertiesAMD
instance FromCStruct PhysicalDeviceShaderCorePropertiesAMD VkPhysicalDeviceShaderCorePropertiesAMD where
  fromCStruct = fromCStructPhysicalDeviceShaderCorePropertiesAMD
instance HasNext PhysicalDeviceShaderCorePropertiesAMD where
  getNext s = next (s :: PhysicalDeviceShaderCorePropertiesAMD)
instance ToCStruct PipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT where
  withCStruct = withCStructPipelineRasterizationConservativeStateCreateInfoEXT
instance FromCStruct PipelineRasterizationConservativeStateCreateInfoEXT VkPipelineRasterizationConservativeStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineRasterizationConservativeStateCreateInfoEXT
instance HasNext PipelineRasterizationConservativeStateCreateInfoEXT where
  getNext s = next (s :: PipelineRasterizationConservativeStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceDescriptorIndexingFeaturesEXT VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
instance FromCStruct PhysicalDeviceDescriptorIndexingFeaturesEXT VkPhysicalDeviceDescriptorIndexingFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT
instance HasNext PhysicalDeviceDescriptorIndexingFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceDescriptorIndexingFeaturesEXT)
instance ToCStruct PhysicalDeviceDescriptorIndexingPropertiesEXT VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
instance FromCStruct PhysicalDeviceDescriptorIndexingPropertiesEXT VkPhysicalDeviceDescriptorIndexingPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT
instance HasNext PhysicalDeviceDescriptorIndexingPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceDescriptorIndexingPropertiesEXT)
instance ToCStruct DescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  withCStruct = withCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
instance FromCStruct DescriptorSetLayoutBindingFlagsCreateInfoEXT VkDescriptorSetLayoutBindingFlagsCreateInfoEXT where
  fromCStruct = fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT
instance HasNext DescriptorSetLayoutBindingFlagsCreateInfoEXT where
  getNext s = next (s :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)
instance ToCStruct DescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  withCStruct = withCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
instance FromCStruct DescriptorSetVariableDescriptorCountAllocateInfoEXT VkDescriptorSetVariableDescriptorCountAllocateInfoEXT where
  fromCStruct = fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT
instance HasNext DescriptorSetVariableDescriptorCountAllocateInfoEXT where
  getNext s = next (s :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)
instance ToCStruct DescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  withCStruct = withCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
instance FromCStruct DescriptorSetVariableDescriptorCountLayoutSupportEXT VkDescriptorSetVariableDescriptorCountLayoutSupportEXT where
  fromCStruct = fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT
instance HasNext DescriptorSetVariableDescriptorCountLayoutSupportEXT where
  getNext s = next (s :: DescriptorSetVariableDescriptorCountLayoutSupportEXT)
instance ToCStruct AttachmentDescription2KHR VkAttachmentDescription2KHR where
  withCStruct = withCStructAttachmentDescription2KHR
instance FromCStruct AttachmentDescription2KHR VkAttachmentDescription2KHR where
  fromCStruct = fromCStructAttachmentDescription2KHR
instance HasNext AttachmentDescription2KHR where
  getNext s = next (s :: AttachmentDescription2KHR)
instance ToCStruct AttachmentReference2KHR VkAttachmentReference2KHR where
  withCStruct = withCStructAttachmentReference2KHR
instance FromCStruct AttachmentReference2KHR VkAttachmentReference2KHR where
  fromCStruct = fromCStructAttachmentReference2KHR
instance HasNext AttachmentReference2KHR where
  getNext s = next (s :: AttachmentReference2KHR)
instance ToCStruct SubpassDescription2KHR VkSubpassDescription2KHR where
  withCStruct = withCStructSubpassDescription2KHR
instance FromCStruct SubpassDescription2KHR VkSubpassDescription2KHR where
  fromCStruct = fromCStructSubpassDescription2KHR
instance HasNext SubpassDescription2KHR where
  getNext s = next (s :: SubpassDescription2KHR)
instance ToCStruct SubpassDependency2KHR VkSubpassDependency2KHR where
  withCStruct = withCStructSubpassDependency2KHR
instance FromCStruct SubpassDependency2KHR VkSubpassDependency2KHR where
  fromCStruct = fromCStructSubpassDependency2KHR
instance HasNext SubpassDependency2KHR where
  getNext s = next (s :: SubpassDependency2KHR)
instance ToCStruct RenderPassCreateInfo2KHR VkRenderPassCreateInfo2KHR where
  withCStruct = withCStructRenderPassCreateInfo2KHR
instance FromCStruct RenderPassCreateInfo2KHR VkRenderPassCreateInfo2KHR where
  fromCStruct = fromCStructRenderPassCreateInfo2KHR
instance HasNext RenderPassCreateInfo2KHR where
  getNext s = next (s :: RenderPassCreateInfo2KHR)
instance ToCStruct SubpassBeginInfoKHR VkSubpassBeginInfoKHR where
  withCStruct = withCStructSubpassBeginInfoKHR
instance FromCStruct SubpassBeginInfoKHR VkSubpassBeginInfoKHR where
  fromCStruct = fromCStructSubpassBeginInfoKHR
instance HasNext SubpassBeginInfoKHR where
  getNext s = next (s :: SubpassBeginInfoKHR)
instance ToCStruct SubpassEndInfoKHR VkSubpassEndInfoKHR where
  withCStruct = withCStructSubpassEndInfoKHR
instance FromCStruct SubpassEndInfoKHR VkSubpassEndInfoKHR where
  fromCStruct = fromCStructSubpassEndInfoKHR
instance HasNext SubpassEndInfoKHR where
  getNext s = next (s :: SubpassEndInfoKHR)
instance ToCStruct VertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT where
  withCStruct = withCStructVertexInputBindingDivisorDescriptionEXT
instance FromCStruct VertexInputBindingDivisorDescriptionEXT VkVertexInputBindingDivisorDescriptionEXT where
  fromCStruct = fromCStructVertexInputBindingDivisorDescriptionEXT

instance ToCStruct PipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT where
  withCStruct = withCStructPipelineVertexInputDivisorStateCreateInfoEXT
instance FromCStruct PipelineVertexInputDivisorStateCreateInfoEXT VkPipelineVertexInputDivisorStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineVertexInputDivisorStateCreateInfoEXT
instance HasNext PipelineVertexInputDivisorStateCreateInfoEXT where
  getNext s = next (s :: PipelineVertexInputDivisorStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
instance FromCStruct PhysicalDeviceVertexAttributeDivisorPropertiesEXT VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT
instance HasNext PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
instance ToCStruct PhysicalDevicePCIBusInfoPropertiesEXT VkPhysicalDevicePCIBusInfoPropertiesEXT where
  withCStruct = withCStructPhysicalDevicePCIBusInfoPropertiesEXT
instance FromCStruct PhysicalDevicePCIBusInfoPropertiesEXT VkPhysicalDevicePCIBusInfoPropertiesEXT where
  fromCStruct = fromCStructPhysicalDevicePCIBusInfoPropertiesEXT
instance HasNext PhysicalDevicePCIBusInfoPropertiesEXT where
  getNext s = next (s :: PhysicalDevicePCIBusInfoPropertiesEXT)

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct ImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID where
  withCStruct = withCStructImportAndroidHardwareBufferInfoANDROID
instance FromCStruct ImportAndroidHardwareBufferInfoANDROID VkImportAndroidHardwareBufferInfoANDROID where
  fromCStruct = fromCStructImportAndroidHardwareBufferInfoANDROID
instance HasNext ImportAndroidHardwareBufferInfoANDROID where
  getNext s = next (s :: ImportAndroidHardwareBufferInfoANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID where
  withCStruct = withCStructAndroidHardwareBufferUsageANDROID
instance FromCStruct AndroidHardwareBufferUsageANDROID VkAndroidHardwareBufferUsageANDROID where
  fromCStruct = fromCStructAndroidHardwareBufferUsageANDROID
instance HasNext AndroidHardwareBufferUsageANDROID where
  getNext s = next (s :: AndroidHardwareBufferUsageANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID where
  withCStruct = withCStructAndroidHardwareBufferPropertiesANDROID
instance FromCStruct AndroidHardwareBufferPropertiesANDROID VkAndroidHardwareBufferPropertiesANDROID where
  fromCStruct = fromCStructAndroidHardwareBufferPropertiesANDROID
instance HasNext AndroidHardwareBufferPropertiesANDROID where
  getNext s = next (s :: AndroidHardwareBufferPropertiesANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct MemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID where
  withCStruct = withCStructMemoryGetAndroidHardwareBufferInfoANDROID
instance FromCStruct MemoryGetAndroidHardwareBufferInfoANDROID VkMemoryGetAndroidHardwareBufferInfoANDROID where
  fromCStruct = fromCStructMemoryGetAndroidHardwareBufferInfoANDROID
instance HasNext MemoryGetAndroidHardwareBufferInfoANDROID where
  getNext s = next (s :: MemoryGetAndroidHardwareBufferInfoANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct AndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID where
  withCStruct = withCStructAndroidHardwareBufferFormatPropertiesANDROID
instance FromCStruct AndroidHardwareBufferFormatPropertiesANDROID VkAndroidHardwareBufferFormatPropertiesANDROID where
  fromCStruct = fromCStructAndroidHardwareBufferFormatPropertiesANDROID
instance HasNext AndroidHardwareBufferFormatPropertiesANDROID where
  getNext s = next (s :: AndroidHardwareBufferFormatPropertiesANDROID)
#endif
instance ToCStruct CommandBufferInheritanceConditionalRenderingInfoEXT VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  withCStruct = withCStructCommandBufferInheritanceConditionalRenderingInfoEXT
instance FromCStruct CommandBufferInheritanceConditionalRenderingInfoEXT VkCommandBufferInheritanceConditionalRenderingInfoEXT where
  fromCStruct = fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT
instance HasNext CommandBufferInheritanceConditionalRenderingInfoEXT where
  getNext s = next (s :: CommandBufferInheritanceConditionalRenderingInfoEXT)

#if VK_USE_PLATFORM_ANDROID_KHR
instance ToCStruct ExternalFormatANDROID VkExternalFormatANDROID where
  withCStruct = withCStructExternalFormatANDROID
instance FromCStruct ExternalFormatANDROID VkExternalFormatANDROID where
  fromCStruct = fromCStructExternalFormatANDROID
instance HasNext ExternalFormatANDROID where
  getNext s = next (s :: ExternalFormatANDROID)
#endif
instance ToCStruct PhysicalDevice8BitStorageFeaturesKHR VkPhysicalDevice8BitStorageFeaturesKHR where
  withCStruct = withCStructPhysicalDevice8BitStorageFeaturesKHR
instance FromCStruct PhysicalDevice8BitStorageFeaturesKHR VkPhysicalDevice8BitStorageFeaturesKHR where
  fromCStruct = fromCStructPhysicalDevice8BitStorageFeaturesKHR
instance HasNext PhysicalDevice8BitStorageFeaturesKHR where
  getNext s = next (s :: PhysicalDevice8BitStorageFeaturesKHR)
instance ToCStruct PhysicalDeviceConditionalRenderingFeaturesEXT VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceConditionalRenderingFeaturesEXT
instance FromCStruct PhysicalDeviceConditionalRenderingFeaturesEXT VkPhysicalDeviceConditionalRenderingFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT
instance HasNext PhysicalDeviceConditionalRenderingFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceConditionalRenderingFeaturesEXT)
instance ToCStruct PhysicalDeviceVulkanMemoryModelFeaturesKHR VkPhysicalDeviceVulkanMemoryModelFeaturesKHR where
  withCStruct = withCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
instance FromCStruct PhysicalDeviceVulkanMemoryModelFeaturesKHR VkPhysicalDeviceVulkanMemoryModelFeaturesKHR where
  fromCStruct = fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR
instance HasNext PhysicalDeviceVulkanMemoryModelFeaturesKHR where
  getNext s = next (s :: PhysicalDeviceVulkanMemoryModelFeaturesKHR)
instance ToCStruct PhysicalDeviceShaderAtomicInt64FeaturesKHR VkPhysicalDeviceShaderAtomicInt64FeaturesKHR where
  withCStruct = withCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR
instance FromCStruct PhysicalDeviceShaderAtomicInt64FeaturesKHR VkPhysicalDeviceShaderAtomicInt64FeaturesKHR where
  fromCStruct = fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR
instance HasNext PhysicalDeviceShaderAtomicInt64FeaturesKHR where
  getNext s = next (s :: PhysicalDeviceShaderAtomicInt64FeaturesKHR)
instance ToCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT
instance FromCStruct PhysicalDeviceVertexAttributeDivisorFeaturesEXT VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT
instance HasNext PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
instance ToCStruct QueueFamilyCheckpointPropertiesNV VkQueueFamilyCheckpointPropertiesNV where
  withCStruct = withCStructQueueFamilyCheckpointPropertiesNV
instance FromCStruct QueueFamilyCheckpointPropertiesNV VkQueueFamilyCheckpointPropertiesNV where
  fromCStruct = fromCStructQueueFamilyCheckpointPropertiesNV
instance HasNext QueueFamilyCheckpointPropertiesNV where
  getNext s = next (s :: QueueFamilyCheckpointPropertiesNV)
instance ToCStruct CheckpointDataNV VkCheckpointDataNV where
  withCStruct = withCStructCheckpointDataNV
instance FromCStruct CheckpointDataNV VkCheckpointDataNV where
  fromCStruct = fromCStructCheckpointDataNV
instance HasNext CheckpointDataNV where
  getNext s = next (s :: CheckpointDataNV)
instance ToCStruct PhysicalDeviceDepthStencilResolvePropertiesKHR VkPhysicalDeviceDepthStencilResolvePropertiesKHR where
  withCStruct = withCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
instance FromCStruct PhysicalDeviceDepthStencilResolvePropertiesKHR VkPhysicalDeviceDepthStencilResolvePropertiesKHR where
  fromCStruct = fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR
instance HasNext PhysicalDeviceDepthStencilResolvePropertiesKHR where
  getNext s = next (s :: PhysicalDeviceDepthStencilResolvePropertiesKHR)
instance ToCStruct SubpassDescriptionDepthStencilResolveKHR VkSubpassDescriptionDepthStencilResolveKHR where
  withCStruct = withCStructSubpassDescriptionDepthStencilResolveKHR
instance FromCStruct SubpassDescriptionDepthStencilResolveKHR VkSubpassDescriptionDepthStencilResolveKHR where
  fromCStruct = fromCStructSubpassDescriptionDepthStencilResolveKHR
instance HasNext SubpassDescriptionDepthStencilResolveKHR where
  getNext s = next (s :: SubpassDescriptionDepthStencilResolveKHR)
instance ToCStruct ImageViewASTCDecodeModeEXT VkImageViewASTCDecodeModeEXT where
  withCStruct = withCStructImageViewASTCDecodeModeEXT
instance FromCStruct ImageViewASTCDecodeModeEXT VkImageViewASTCDecodeModeEXT where
  fromCStruct = fromCStructImageViewASTCDecodeModeEXT
instance HasNext ImageViewASTCDecodeModeEXT where
  getNext s = next (s :: ImageViewASTCDecodeModeEXT)
instance ToCStruct PhysicalDeviceASTCDecodeFeaturesEXT VkPhysicalDeviceASTCDecodeFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceASTCDecodeFeaturesEXT
instance FromCStruct PhysicalDeviceASTCDecodeFeaturesEXT VkPhysicalDeviceASTCDecodeFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceASTCDecodeFeaturesEXT
instance HasNext PhysicalDeviceASTCDecodeFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceASTCDecodeFeaturesEXT)
instance ToCStruct PhysicalDeviceTransformFeedbackFeaturesEXT VkPhysicalDeviceTransformFeedbackFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceTransformFeedbackFeaturesEXT
instance FromCStruct PhysicalDeviceTransformFeedbackFeaturesEXT VkPhysicalDeviceTransformFeedbackFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT
instance HasNext PhysicalDeviceTransformFeedbackFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceTransformFeedbackFeaturesEXT)
instance ToCStruct PhysicalDeviceTransformFeedbackPropertiesEXT VkPhysicalDeviceTransformFeedbackPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceTransformFeedbackPropertiesEXT
instance FromCStruct PhysicalDeviceTransformFeedbackPropertiesEXT VkPhysicalDeviceTransformFeedbackPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceTransformFeedbackPropertiesEXT
instance HasNext PhysicalDeviceTransformFeedbackPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceTransformFeedbackPropertiesEXT)
instance ToCStruct PipelineRasterizationStateStreamCreateInfoEXT VkPipelineRasterizationStateStreamCreateInfoEXT where
  withCStruct = withCStructPipelineRasterizationStateStreamCreateInfoEXT
instance FromCStruct PipelineRasterizationStateStreamCreateInfoEXT VkPipelineRasterizationStateStreamCreateInfoEXT where
  fromCStruct = fromCStructPipelineRasterizationStateStreamCreateInfoEXT
instance HasNext PipelineRasterizationStateStreamCreateInfoEXT where
  getNext s = next (s :: PipelineRasterizationStateStreamCreateInfoEXT)
instance ToCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  withCStruct = withCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV
instance FromCStruct PhysicalDeviceRepresentativeFragmentTestFeaturesNV VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV
instance HasNext PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  getNext s = next (s :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
instance ToCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV VkPipelineRepresentativeFragmentTestStateCreateInfoNV where
  withCStruct = withCStructPipelineRepresentativeFragmentTestStateCreateInfoNV
instance FromCStruct PipelineRepresentativeFragmentTestStateCreateInfoNV VkPipelineRepresentativeFragmentTestStateCreateInfoNV where
  fromCStruct = fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV
instance HasNext PipelineRepresentativeFragmentTestStateCreateInfoNV where
  getNext s = next (s :: PipelineRepresentativeFragmentTestStateCreateInfoNV)
instance ToCStruct PhysicalDeviceExclusiveScissorFeaturesNV VkPhysicalDeviceExclusiveScissorFeaturesNV where
  withCStruct = withCStructPhysicalDeviceExclusiveScissorFeaturesNV
instance FromCStruct PhysicalDeviceExclusiveScissorFeaturesNV VkPhysicalDeviceExclusiveScissorFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceExclusiveScissorFeaturesNV
instance HasNext PhysicalDeviceExclusiveScissorFeaturesNV where
  getNext s = next (s :: PhysicalDeviceExclusiveScissorFeaturesNV)
instance ToCStruct PipelineViewportExclusiveScissorStateCreateInfoNV VkPipelineViewportExclusiveScissorStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportExclusiveScissorStateCreateInfoNV
instance FromCStruct PipelineViewportExclusiveScissorStateCreateInfoNV VkPipelineViewportExclusiveScissorStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV
instance HasNext PipelineViewportExclusiveScissorStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportExclusiveScissorStateCreateInfoNV)
instance ToCStruct PhysicalDeviceCornerSampledImageFeaturesNV VkPhysicalDeviceCornerSampledImageFeaturesNV where
  withCStruct = withCStructPhysicalDeviceCornerSampledImageFeaturesNV
instance FromCStruct PhysicalDeviceCornerSampledImageFeaturesNV VkPhysicalDeviceCornerSampledImageFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceCornerSampledImageFeaturesNV
instance HasNext PhysicalDeviceCornerSampledImageFeaturesNV where
  getNext s = next (s :: PhysicalDeviceCornerSampledImageFeaturesNV)
instance ToCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV VkPhysicalDeviceComputeShaderDerivativesFeaturesNV where
  withCStruct = withCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV
instance FromCStruct PhysicalDeviceComputeShaderDerivativesFeaturesNV VkPhysicalDeviceComputeShaderDerivativesFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV
instance HasNext PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  getNext s = next (s :: PhysicalDeviceComputeShaderDerivativesFeaturesNV)
instance ToCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesNV VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  withCStruct = withCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
instance FromCStruct PhysicalDeviceFragmentShaderBarycentricFeaturesNV VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV
instance HasNext PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  getNext s = next (s :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
instance ToCStruct PhysicalDeviceShaderImageFootprintFeaturesNV VkPhysicalDeviceShaderImageFootprintFeaturesNV where
  withCStruct = withCStructPhysicalDeviceShaderImageFootprintFeaturesNV
instance FromCStruct PhysicalDeviceShaderImageFootprintFeaturesNV VkPhysicalDeviceShaderImageFootprintFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV
instance HasNext PhysicalDeviceShaderImageFootprintFeaturesNV where
  getNext s = next (s :: PhysicalDeviceShaderImageFootprintFeaturesNV)
instance ToCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  withCStruct = withCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
instance FromCStruct PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV
instance HasNext PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  getNext s = next (s :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)
instance ToCStruct ShadingRatePaletteNV VkShadingRatePaletteNV where
  withCStruct = withCStructShadingRatePaletteNV
instance FromCStruct ShadingRatePaletteNV VkShadingRatePaletteNV where
  fromCStruct = fromCStructShadingRatePaletteNV

instance ToCStruct PipelineViewportShadingRateImageStateCreateInfoNV VkPipelineViewportShadingRateImageStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportShadingRateImageStateCreateInfoNV
instance FromCStruct PipelineViewportShadingRateImageStateCreateInfoNV VkPipelineViewportShadingRateImageStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportShadingRateImageStateCreateInfoNV
instance HasNext PipelineViewportShadingRateImageStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportShadingRateImageStateCreateInfoNV)
instance ToCStruct PhysicalDeviceShadingRateImageFeaturesNV VkPhysicalDeviceShadingRateImageFeaturesNV where
  withCStruct = withCStructPhysicalDeviceShadingRateImageFeaturesNV
instance FromCStruct PhysicalDeviceShadingRateImageFeaturesNV VkPhysicalDeviceShadingRateImageFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceShadingRateImageFeaturesNV
instance HasNext PhysicalDeviceShadingRateImageFeaturesNV where
  getNext s = next (s :: PhysicalDeviceShadingRateImageFeaturesNV)
instance ToCStruct PhysicalDeviceShadingRateImagePropertiesNV VkPhysicalDeviceShadingRateImagePropertiesNV where
  withCStruct = withCStructPhysicalDeviceShadingRateImagePropertiesNV
instance FromCStruct PhysicalDeviceShadingRateImagePropertiesNV VkPhysicalDeviceShadingRateImagePropertiesNV where
  fromCStruct = fromCStructPhysicalDeviceShadingRateImagePropertiesNV
instance HasNext PhysicalDeviceShadingRateImagePropertiesNV where
  getNext s = next (s :: PhysicalDeviceShadingRateImagePropertiesNV)
instance ToCStruct CoarseSampleLocationNV VkCoarseSampleLocationNV where
  withCStruct = withCStructCoarseSampleLocationNV
instance FromCStruct CoarseSampleLocationNV VkCoarseSampleLocationNV where
  fromCStruct = fromCStructCoarseSampleLocationNV

instance ToCStruct CoarseSampleOrderCustomNV VkCoarseSampleOrderCustomNV where
  withCStruct = withCStructCoarseSampleOrderCustomNV
instance FromCStruct CoarseSampleOrderCustomNV VkCoarseSampleOrderCustomNV where
  fromCStruct = fromCStructCoarseSampleOrderCustomNV

instance ToCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV VkPipelineViewportCoarseSampleOrderStateCreateInfoNV where
  withCStruct = withCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV
instance FromCStruct PipelineViewportCoarseSampleOrderStateCreateInfoNV VkPipelineViewportCoarseSampleOrderStateCreateInfoNV where
  fromCStruct = fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV
instance HasNext PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)
instance ToCStruct PhysicalDeviceMeshShaderFeaturesNV VkPhysicalDeviceMeshShaderFeaturesNV where
  withCStruct = withCStructPhysicalDeviceMeshShaderFeaturesNV
instance FromCStruct PhysicalDeviceMeshShaderFeaturesNV VkPhysicalDeviceMeshShaderFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceMeshShaderFeaturesNV
instance HasNext PhysicalDeviceMeshShaderFeaturesNV where
  getNext s = next (s :: PhysicalDeviceMeshShaderFeaturesNV)
instance ToCStruct PhysicalDeviceMeshShaderPropertiesNV VkPhysicalDeviceMeshShaderPropertiesNV where
  withCStruct = withCStructPhysicalDeviceMeshShaderPropertiesNV
instance FromCStruct PhysicalDeviceMeshShaderPropertiesNV VkPhysicalDeviceMeshShaderPropertiesNV where
  fromCStruct = fromCStructPhysicalDeviceMeshShaderPropertiesNV
instance HasNext PhysicalDeviceMeshShaderPropertiesNV where
  getNext s = next (s :: PhysicalDeviceMeshShaderPropertiesNV)
instance ToCStruct DrawMeshTasksIndirectCommandNV VkDrawMeshTasksIndirectCommandNV where
  withCStruct = withCStructDrawMeshTasksIndirectCommandNV
instance FromCStruct DrawMeshTasksIndirectCommandNV VkDrawMeshTasksIndirectCommandNV where
  fromCStruct = fromCStructDrawMeshTasksIndirectCommandNV

instance ToCStruct RayTracingShaderGroupCreateInfoNV VkRayTracingShaderGroupCreateInfoNV where
  withCStruct = withCStructRayTracingShaderGroupCreateInfoNV
instance FromCStruct RayTracingShaderGroupCreateInfoNV VkRayTracingShaderGroupCreateInfoNV where
  fromCStruct = fromCStructRayTracingShaderGroupCreateInfoNV
instance HasNext RayTracingShaderGroupCreateInfoNV where
  getNext s = next (s :: RayTracingShaderGroupCreateInfoNV)
instance ToCStruct RayTracingPipelineCreateInfoNV VkRayTracingPipelineCreateInfoNV where
  withCStruct = withCStructRayTracingPipelineCreateInfoNV
instance FromCStruct RayTracingPipelineCreateInfoNV VkRayTracingPipelineCreateInfoNV where
  fromCStruct = fromCStructRayTracingPipelineCreateInfoNV
instance HasNext RayTracingPipelineCreateInfoNV where
  getNext s = next (s :: RayTracingPipelineCreateInfoNV)
instance ToCStruct GeometryTrianglesNV VkGeometryTrianglesNV where
  withCStruct = withCStructGeometryTrianglesNV
instance FromCStruct GeometryTrianglesNV VkGeometryTrianglesNV where
  fromCStruct = fromCStructGeometryTrianglesNV
instance HasNext GeometryTrianglesNV where
  getNext s = next (s :: GeometryTrianglesNV)
instance ToCStruct GeometryAABBNV VkGeometryAABBNV where
  withCStruct = withCStructGeometryAABBNV
instance FromCStruct GeometryAABBNV VkGeometryAABBNV where
  fromCStruct = fromCStructGeometryAABBNV
instance HasNext GeometryAABBNV where
  getNext s = next (s :: GeometryAABBNV)
instance ToCStruct GeometryDataNV VkGeometryDataNV where
  withCStruct = withCStructGeometryDataNV
instance FromCStruct GeometryDataNV VkGeometryDataNV where
  fromCStruct = fromCStructGeometryDataNV

instance ToCStruct GeometryNV VkGeometryNV where
  withCStruct = withCStructGeometryNV
instance FromCStruct GeometryNV VkGeometryNV where
  fromCStruct = fromCStructGeometryNV
instance HasNext GeometryNV where
  getNext s = next (s :: GeometryNV)
instance ToCStruct AccelerationStructureInfoNV VkAccelerationStructureInfoNV where
  withCStruct = withCStructAccelerationStructureInfoNV
instance FromCStruct AccelerationStructureInfoNV VkAccelerationStructureInfoNV where
  fromCStruct = fromCStructAccelerationStructureInfoNV
instance HasNext AccelerationStructureInfoNV where
  getNext s = next (s :: AccelerationStructureInfoNV)
instance ToCStruct AccelerationStructureCreateInfoNV VkAccelerationStructureCreateInfoNV where
  withCStruct = withCStructAccelerationStructureCreateInfoNV
instance FromCStruct AccelerationStructureCreateInfoNV VkAccelerationStructureCreateInfoNV where
  fromCStruct = fromCStructAccelerationStructureCreateInfoNV
instance HasNext AccelerationStructureCreateInfoNV where
  getNext s = next (s :: AccelerationStructureCreateInfoNV)
instance ToCStruct BindAccelerationStructureMemoryInfoNV VkBindAccelerationStructureMemoryInfoNV where
  withCStruct = withCStructBindAccelerationStructureMemoryInfoNV
instance FromCStruct BindAccelerationStructureMemoryInfoNV VkBindAccelerationStructureMemoryInfoNV where
  fromCStruct = fromCStructBindAccelerationStructureMemoryInfoNV
instance HasNext BindAccelerationStructureMemoryInfoNV where
  getNext s = next (s :: BindAccelerationStructureMemoryInfoNV)
instance ToCStruct WriteDescriptorSetAccelerationStructureNV VkWriteDescriptorSetAccelerationStructureNV where
  withCStruct = withCStructWriteDescriptorSetAccelerationStructureNV
instance FromCStruct WriteDescriptorSetAccelerationStructureNV VkWriteDescriptorSetAccelerationStructureNV where
  fromCStruct = fromCStructWriteDescriptorSetAccelerationStructureNV
instance HasNext WriteDescriptorSetAccelerationStructureNV where
  getNext s = next (s :: WriteDescriptorSetAccelerationStructureNV)
instance ToCStruct AccelerationStructureMemoryRequirementsInfoNV VkAccelerationStructureMemoryRequirementsInfoNV where
  withCStruct = withCStructAccelerationStructureMemoryRequirementsInfoNV
instance FromCStruct AccelerationStructureMemoryRequirementsInfoNV VkAccelerationStructureMemoryRequirementsInfoNV where
  fromCStruct = fromCStructAccelerationStructureMemoryRequirementsInfoNV
instance HasNext AccelerationStructureMemoryRequirementsInfoNV where
  getNext s = next (s :: AccelerationStructureMemoryRequirementsInfoNV)
instance ToCStruct PhysicalDeviceRayTracingPropertiesNV VkPhysicalDeviceRayTracingPropertiesNV where
  withCStruct = withCStructPhysicalDeviceRayTracingPropertiesNV
instance FromCStruct PhysicalDeviceRayTracingPropertiesNV VkPhysicalDeviceRayTracingPropertiesNV where
  fromCStruct = fromCStructPhysicalDeviceRayTracingPropertiesNV
instance HasNext PhysicalDeviceRayTracingPropertiesNV where
  getNext s = next (s :: PhysicalDeviceRayTracingPropertiesNV)
instance ToCStruct DrmFormatModifierPropertiesListEXT VkDrmFormatModifierPropertiesListEXT where
  withCStruct = withCStructDrmFormatModifierPropertiesListEXT
instance FromCStruct DrmFormatModifierPropertiesListEXT VkDrmFormatModifierPropertiesListEXT where
  fromCStruct = fromCStructDrmFormatModifierPropertiesListEXT
instance HasNext DrmFormatModifierPropertiesListEXT where
  getNext s = next (s :: DrmFormatModifierPropertiesListEXT)
instance ToCStruct DrmFormatModifierPropertiesEXT VkDrmFormatModifierPropertiesEXT where
  withCStruct = withCStructDrmFormatModifierPropertiesEXT
instance FromCStruct DrmFormatModifierPropertiesEXT VkDrmFormatModifierPropertiesEXT where
  fromCStruct = fromCStructDrmFormatModifierPropertiesEXT

instance ToCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT VkPhysicalDeviceImageDrmFormatModifierInfoEXT where
  withCStruct = withCStructPhysicalDeviceImageDrmFormatModifierInfoEXT
instance FromCStruct PhysicalDeviceImageDrmFormatModifierInfoEXT VkPhysicalDeviceImageDrmFormatModifierInfoEXT where
  fromCStruct = fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT
instance HasNext PhysicalDeviceImageDrmFormatModifierInfoEXT where
  getNext s = next (s :: PhysicalDeviceImageDrmFormatModifierInfoEXT)
instance ToCStruct ImageDrmFormatModifierListCreateInfoEXT VkImageDrmFormatModifierListCreateInfoEXT where
  withCStruct = withCStructImageDrmFormatModifierListCreateInfoEXT
instance FromCStruct ImageDrmFormatModifierListCreateInfoEXT VkImageDrmFormatModifierListCreateInfoEXT where
  fromCStruct = fromCStructImageDrmFormatModifierListCreateInfoEXT
instance HasNext ImageDrmFormatModifierListCreateInfoEXT where
  getNext s = next (s :: ImageDrmFormatModifierListCreateInfoEXT)
instance ToCStruct ImageDrmFormatModifierExplicitCreateInfoEXT VkImageDrmFormatModifierExplicitCreateInfoEXT where
  withCStruct = withCStructImageDrmFormatModifierExplicitCreateInfoEXT
instance FromCStruct ImageDrmFormatModifierExplicitCreateInfoEXT VkImageDrmFormatModifierExplicitCreateInfoEXT where
  fromCStruct = fromCStructImageDrmFormatModifierExplicitCreateInfoEXT
instance HasNext ImageDrmFormatModifierExplicitCreateInfoEXT where
  getNext s = next (s :: ImageDrmFormatModifierExplicitCreateInfoEXT)
instance ToCStruct ImageDrmFormatModifierPropertiesEXT VkImageDrmFormatModifierPropertiesEXT where
  withCStruct = withCStructImageDrmFormatModifierPropertiesEXT
instance FromCStruct ImageDrmFormatModifierPropertiesEXT VkImageDrmFormatModifierPropertiesEXT where
  fromCStruct = fromCStructImageDrmFormatModifierPropertiesEXT
instance HasNext ImageDrmFormatModifierPropertiesEXT where
  getNext s = next (s :: ImageDrmFormatModifierPropertiesEXT)
instance ToCStruct ImageStencilUsageCreateInfoEXT VkImageStencilUsageCreateInfoEXT where
  withCStruct = withCStructImageStencilUsageCreateInfoEXT
instance FromCStruct ImageStencilUsageCreateInfoEXT VkImageStencilUsageCreateInfoEXT where
  fromCStruct = fromCStructImageStencilUsageCreateInfoEXT
instance HasNext ImageStencilUsageCreateInfoEXT where
  getNext s = next (s :: ImageStencilUsageCreateInfoEXT)
instance ToCStruct DeviceMemoryOverallocationCreateInfoAMD VkDeviceMemoryOverallocationCreateInfoAMD where
  withCStruct = withCStructDeviceMemoryOverallocationCreateInfoAMD
instance FromCStruct DeviceMemoryOverallocationCreateInfoAMD VkDeviceMemoryOverallocationCreateInfoAMD where
  fromCStruct = fromCStructDeviceMemoryOverallocationCreateInfoAMD
instance HasNext DeviceMemoryOverallocationCreateInfoAMD where
  getNext s = next (s :: DeviceMemoryOverallocationCreateInfoAMD)
instance ToCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
instance FromCStruct PhysicalDeviceFragmentDensityMapFeaturesEXT VkPhysicalDeviceFragmentDensityMapFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT
instance HasNext PhysicalDeviceFragmentDensityMapFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceFragmentDensityMapFeaturesEXT)
instance ToCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
instance FromCStruct PhysicalDeviceFragmentDensityMapPropertiesEXT VkPhysicalDeviceFragmentDensityMapPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT
instance HasNext PhysicalDeviceFragmentDensityMapPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceFragmentDensityMapPropertiesEXT)
instance ToCStruct RenderPassFragmentDensityMapCreateInfoEXT VkRenderPassFragmentDensityMapCreateInfoEXT where
  withCStruct = withCStructRenderPassFragmentDensityMapCreateInfoEXT
instance FromCStruct RenderPassFragmentDensityMapCreateInfoEXT VkRenderPassFragmentDensityMapCreateInfoEXT where
  fromCStruct = fromCStructRenderPassFragmentDensityMapCreateInfoEXT
instance HasNext RenderPassFragmentDensityMapCreateInfoEXT where
  getNext s = next (s :: RenderPassFragmentDensityMapCreateInfoEXT)
instance ToCStruct PhysicalDeviceScalarBlockLayoutFeaturesEXT VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT
instance FromCStruct PhysicalDeviceScalarBlockLayoutFeaturesEXT VkPhysicalDeviceScalarBlockLayoutFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT
instance HasNext PhysicalDeviceScalarBlockLayoutFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceScalarBlockLayoutFeaturesEXT)
instance ToCStruct SurfaceProtectedCapabilitiesKHR VkSurfaceProtectedCapabilitiesKHR where
  withCStruct = withCStructSurfaceProtectedCapabilitiesKHR
instance FromCStruct SurfaceProtectedCapabilitiesKHR VkSurfaceProtectedCapabilitiesKHR where
  fromCStruct = fromCStructSurfaceProtectedCapabilitiesKHR
instance HasNext SurfaceProtectedCapabilitiesKHR where
  getNext s = next (s :: SurfaceProtectedCapabilitiesKHR)
instance ToCStruct PhysicalDeviceDepthClipEnableFeaturesEXT VkPhysicalDeviceDepthClipEnableFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceDepthClipEnableFeaturesEXT
instance FromCStruct PhysicalDeviceDepthClipEnableFeaturesEXT VkPhysicalDeviceDepthClipEnableFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT
instance HasNext PhysicalDeviceDepthClipEnableFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceDepthClipEnableFeaturesEXT)
instance ToCStruct PipelineRasterizationDepthClipStateCreateInfoEXT VkPipelineRasterizationDepthClipStateCreateInfoEXT where
  withCStruct = withCStructPipelineRasterizationDepthClipStateCreateInfoEXT
instance FromCStruct PipelineRasterizationDepthClipStateCreateInfoEXT VkPipelineRasterizationDepthClipStateCreateInfoEXT where
  fromCStruct = fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT
instance HasNext PipelineRasterizationDepthClipStateCreateInfoEXT where
  getNext s = next (s :: PipelineRasterizationDepthClipStateCreateInfoEXT)
instance ToCStruct PhysicalDeviceMemoryBudgetPropertiesEXT VkPhysicalDeviceMemoryBudgetPropertiesEXT where
  withCStruct = withCStructPhysicalDeviceMemoryBudgetPropertiesEXT
instance FromCStruct PhysicalDeviceMemoryBudgetPropertiesEXT VkPhysicalDeviceMemoryBudgetPropertiesEXT where
  fromCStruct = fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT
instance HasNext PhysicalDeviceMemoryBudgetPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceMemoryBudgetPropertiesEXT)
instance ToCStruct PhysicalDeviceMemoryPriorityFeaturesEXT VkPhysicalDeviceMemoryPriorityFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceMemoryPriorityFeaturesEXT
instance FromCStruct PhysicalDeviceMemoryPriorityFeaturesEXT VkPhysicalDeviceMemoryPriorityFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT
instance HasNext PhysicalDeviceMemoryPriorityFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceMemoryPriorityFeaturesEXT)
instance ToCStruct MemoryPriorityAllocateInfoEXT VkMemoryPriorityAllocateInfoEXT where
  withCStruct = withCStructMemoryPriorityAllocateInfoEXT
instance FromCStruct MemoryPriorityAllocateInfoEXT VkMemoryPriorityAllocateInfoEXT where
  fromCStruct = fromCStructMemoryPriorityAllocateInfoEXT
instance HasNext MemoryPriorityAllocateInfoEXT where
  getNext s = next (s :: MemoryPriorityAllocateInfoEXT)
instance ToCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
instance FromCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT VkPhysicalDeviceBufferDeviceAddressFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT
instance HasNext PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceBufferDeviceAddressFeaturesEXT)
instance ToCStruct BufferDeviceAddressInfoEXT VkBufferDeviceAddressInfoEXT where
  withCStruct = withCStructBufferDeviceAddressInfoEXT
instance FromCStruct BufferDeviceAddressInfoEXT VkBufferDeviceAddressInfoEXT where
  fromCStruct = fromCStructBufferDeviceAddressInfoEXT
instance HasNext BufferDeviceAddressInfoEXT where
  getNext s = next (s :: BufferDeviceAddressInfoEXT)
instance ToCStruct BufferDeviceAddressCreateInfoEXT VkBufferDeviceAddressCreateInfoEXT where
  withCStruct = withCStructBufferDeviceAddressCreateInfoEXT
instance FromCStruct BufferDeviceAddressCreateInfoEXT VkBufferDeviceAddressCreateInfoEXT where
  fromCStruct = fromCStructBufferDeviceAddressCreateInfoEXT
instance HasNext BufferDeviceAddressCreateInfoEXT where
  getNext s = next (s :: BufferDeviceAddressCreateInfoEXT)
instance ToCStruct PhysicalDeviceImageViewImageFormatInfoEXT VkPhysicalDeviceImageViewImageFormatInfoEXT where
  withCStruct = withCStructPhysicalDeviceImageViewImageFormatInfoEXT
instance FromCStruct PhysicalDeviceImageViewImageFormatInfoEXT VkPhysicalDeviceImageViewImageFormatInfoEXT where
  fromCStruct = fromCStructPhysicalDeviceImageViewImageFormatInfoEXT
instance HasNext PhysicalDeviceImageViewImageFormatInfoEXT where
  getNext s = next (s :: PhysicalDeviceImageViewImageFormatInfoEXT)
instance ToCStruct FilterCubicImageViewImageFormatPropertiesEXT VkFilterCubicImageViewImageFormatPropertiesEXT where
  withCStruct = withCStructFilterCubicImageViewImageFormatPropertiesEXT
instance FromCStruct FilterCubicImageViewImageFormatPropertiesEXT VkFilterCubicImageViewImageFormatPropertiesEXT where
  fromCStruct = fromCStructFilterCubicImageViewImageFormatPropertiesEXT
instance HasNext FilterCubicImageViewImageFormatPropertiesEXT where
  getNext s = next (s :: FilterCubicImageViewImageFormatPropertiesEXT)
instance ToCStruct PhysicalDeviceCooperativeMatrixFeaturesNV VkPhysicalDeviceCooperativeMatrixFeaturesNV where
  withCStruct = withCStructPhysicalDeviceCooperativeMatrixFeaturesNV
instance FromCStruct PhysicalDeviceCooperativeMatrixFeaturesNV VkPhysicalDeviceCooperativeMatrixFeaturesNV where
  fromCStruct = fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV
instance HasNext PhysicalDeviceCooperativeMatrixFeaturesNV where
  getNext s = next (s :: PhysicalDeviceCooperativeMatrixFeaturesNV)
instance ToCStruct PhysicalDeviceCooperativeMatrixPropertiesNV VkPhysicalDeviceCooperativeMatrixPropertiesNV where
  withCStruct = withCStructPhysicalDeviceCooperativeMatrixPropertiesNV
instance FromCStruct PhysicalDeviceCooperativeMatrixPropertiesNV VkPhysicalDeviceCooperativeMatrixPropertiesNV where
  fromCStruct = fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV
instance HasNext PhysicalDeviceCooperativeMatrixPropertiesNV where
  getNext s = next (s :: PhysicalDeviceCooperativeMatrixPropertiesNV)
instance ToCStruct CooperativeMatrixPropertiesNV VkCooperativeMatrixPropertiesNV where
  withCStruct = withCStructCooperativeMatrixPropertiesNV
instance FromCStruct CooperativeMatrixPropertiesNV VkCooperativeMatrixPropertiesNV where
  fromCStruct = fromCStructCooperativeMatrixPropertiesNV
instance HasNext CooperativeMatrixPropertiesNV where
  getNext s = next (s :: CooperativeMatrixPropertiesNV)
instance ToCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT VkPhysicalDeviceYcbcrImageArraysFeaturesEXT where
  withCStruct = withCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
instance FromCStruct PhysicalDeviceYcbcrImageArraysFeaturesEXT VkPhysicalDeviceYcbcrImageArraysFeaturesEXT where
  fromCStruct = fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT
instance HasNext PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceYcbcrImageArraysFeaturesEXT)
instance ToCStruct ImageViewHandleInfoNVX VkImageViewHandleInfoNVX where
  withCStruct = withCStructImageViewHandleInfoNVX
instance FromCStruct ImageViewHandleInfoNVX VkImageViewHandleInfoNVX where
  fromCStruct = fromCStructImageViewHandleInfoNVX
instance HasNext ImageViewHandleInfoNVX where
  getNext s = next (s :: ImageViewHandleInfoNVX)

#if VK_USE_PLATFORM_GGP
instance ToCStruct PresentFrameTokenGGP VkPresentFrameTokenGGP where
  withCStruct = withCStructPresentFrameTokenGGP
instance FromCStruct PresentFrameTokenGGP VkPresentFrameTokenGGP where
  fromCStruct = fromCStructPresentFrameTokenGGP
instance HasNext PresentFrameTokenGGP where
  getNext s = next (s :: PresentFrameTokenGGP)
#endif
instance ToCStruct PipelineCreationFeedbackEXT VkPipelineCreationFeedbackEXT where
  withCStruct = withCStructPipelineCreationFeedbackEXT
instance FromCStruct PipelineCreationFeedbackEXT VkPipelineCreationFeedbackEXT where
  fromCStruct = fromCStructPipelineCreationFeedbackEXT

instance ToCStruct PipelineCreationFeedbackCreateInfoEXT VkPipelineCreationFeedbackCreateInfoEXT where
  withCStruct = withCStructPipelineCreationFeedbackCreateInfoEXT
instance FromCStruct PipelineCreationFeedbackCreateInfoEXT VkPipelineCreationFeedbackCreateInfoEXT where
  fromCStruct = fromCStructPipelineCreationFeedbackCreateInfoEXT
instance HasNext PipelineCreationFeedbackCreateInfoEXT where
  getNext s = next (s :: PipelineCreationFeedbackCreateInfoEXT)
instance ToCStruct SurfaceFullScreenExclusiveInfoEXT VkSurfaceFullScreenExclusiveInfoEXT where
  withCStruct = withCStructSurfaceFullScreenExclusiveInfoEXT
instance FromCStruct SurfaceFullScreenExclusiveInfoEXT VkSurfaceFullScreenExclusiveInfoEXT where
  fromCStruct = fromCStructSurfaceFullScreenExclusiveInfoEXT
instance HasNext SurfaceFullScreenExclusiveInfoEXT where
  getNext s = next (s :: SurfaceFullScreenExclusiveInfoEXT)
instance ToCStruct SurfaceFullScreenExclusiveWin32InfoEXT VkSurfaceFullScreenExclusiveWin32InfoEXT where
  withCStruct = withCStructSurfaceFullScreenExclusiveWin32InfoEXT
instance FromCStruct SurfaceFullScreenExclusiveWin32InfoEXT VkSurfaceFullScreenExclusiveWin32InfoEXT where
  fromCStruct = fromCStructSurfaceFullScreenExclusiveWin32InfoEXT
instance HasNext SurfaceFullScreenExclusiveWin32InfoEXT where
  getNext s = next (s :: SurfaceFullScreenExclusiveWin32InfoEXT)
instance ToCStruct SurfaceCapabilitiesFullScreenExclusiveEXT VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  withCStruct = withCStructSurfaceCapabilitiesFullScreenExclusiveEXT
instance FromCStruct SurfaceCapabilitiesFullScreenExclusiveEXT VkSurfaceCapabilitiesFullScreenExclusiveEXT where
  fromCStruct = fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT
instance HasNext SurfaceCapabilitiesFullScreenExclusiveEXT where
  getNext s = next (s :: SurfaceCapabilitiesFullScreenExclusiveEXT)
instance ToCStruct HeadlessSurfaceCreateInfoEXT VkHeadlessSurfaceCreateInfoEXT where
  withCStruct = withCStructHeadlessSurfaceCreateInfoEXT
instance FromCStruct HeadlessSurfaceCreateInfoEXT VkHeadlessSurfaceCreateInfoEXT where
  fromCStruct = fromCStructHeadlessSurfaceCreateInfoEXT
instance HasNext HeadlessSurfaceCreateInfoEXT where
  getNext s = next (s :: HeadlessSurfaceCreateInfoEXT)
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
#if defined(VK_USE_PLATFORM_FUCHSIA)
    VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA -> SomeVkStruct <$> (fromCStructImagePipeSurfaceCreateInfoFUCHSIA =<< peek (castPtr p :: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA))
#endif
#if defined(VK_USE_PLATFORM_GGP)
    VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP -> SomeVkStruct <$> (fromCStructStreamDescriptorSurfaceCreateInfoGGP =<< peek (castPtr p :: Ptr VkStreamDescriptorSurfaceCreateInfoGGP))
#endif
    VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructSwapchainCreateInfoKHR =<< peek (castPtr p :: Ptr VkSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_PRESENT_INFO_KHR -> SomeVkStruct <$> (fromCStructPresentInfoKHR =<< peek (castPtr p :: Ptr VkPresentInfoKHR))
    VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDebugReportCallbackCreateInfoEXT =<< peek (castPtr p :: Ptr VkDebugReportCallbackCreateInfoEXT))
    VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT -> SomeVkStruct <$> (fromCStructValidationFlagsEXT =<< peek (castPtr p :: Ptr VkValidationFlagsEXT))
    VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT -> SomeVkStruct <$> (fromCStructValidationFeaturesEXT =<< peek (castPtr p :: Ptr VkValidationFeaturesEXT))
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
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceDriverPropertiesKHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceDriverPropertiesKHR))
    VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR -> SomeVkStruct <$> (fromCStructPresentRegionsKHR =<< peek (castPtr p :: Ptr VkPresentRegionsKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceVariablePointersFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceVariablePointersFeatures))
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
    VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD -> SomeVkStruct <$> (fromCStructDisplayNativeHdrSurfaceCapabilitiesAMD =<< peek (castPtr p :: Ptr VkDisplayNativeHdrSurfaceCapabilitiesAMD))
    VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD -> SomeVkStruct <$> (fromCStructSwapchainDisplayNativeHdrCreateInfoAMD =<< peek (castPtr p :: Ptr VkSwapchainDisplayNativeHdrCreateInfoAMD))
    VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE -> SomeVkStruct <$> (fromCStructPresentTimesInfoGOOGLE =<< peek (castPtr p :: Ptr VkPresentTimesInfoGOOGLE))
#if defined(VK_USE_PLATFORM_IOS_MVK)
    VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK -> SomeVkStruct <$> (fromCStructIOSSurfaceCreateInfoMVK =<< peek (castPtr p :: Ptr VkIOSSurfaceCreateInfoMVK))
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
    VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK -> SomeVkStruct <$> (fromCStructMacOSSurfaceCreateInfoMVK =<< peek (castPtr p :: Ptr VkMacOSSurfaceCreateInfoMVK))
#endif
#if defined(VK_USE_PLATFORM_METAL_EXT)
    VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructMetalSurfaceCreateInfoEXT =<< peek (castPtr p :: Ptr VkMetalSurfaceCreateInfoEXT))
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
    VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR -> SomeVkStruct <$> (fromCStructDisplayProperties2KHR =<< peek (castPtr p :: Ptr VkDisplayProperties2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR -> SomeVkStruct <$> (fromCStructDisplayPlaneProperties2KHR =<< peek (castPtr p :: Ptr VkDisplayPlaneProperties2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR -> SomeVkStruct <$> (fromCStructDisplayModeProperties2KHR =<< peek (castPtr p :: Ptr VkDisplayModeProperties2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR -> SomeVkStruct <$> (fromCStructDisplayPlaneInfo2KHR =<< peek (castPtr p :: Ptr VkDisplayPlaneInfo2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR -> SomeVkStruct <$> (fromCStructDisplayPlaneCapabilities2KHR =<< peek (castPtr p :: Ptr VkDisplayPlaneCapabilities2KHR))
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
    VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT -> SomeVkStruct <$> (fromCStructConditionalRenderingBeginInfoEXT =<< peek (castPtr p :: Ptr VkConditionalRenderingBeginInfoEXT))
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
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceInlineUniformBlockFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceInlineUniformBlockFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceInlineUniformBlockPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT -> SomeVkStruct <$> (fromCStructWriteDescriptorSetInlineUniformBlockEXT =<< peek (castPtr p :: Ptr VkWriteDescriptorSetInlineUniformBlockEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDescriptorPoolInlineUniformBlockCreateInfoEXT =<< peek (castPtr p :: Ptr VkDescriptorPoolInlineUniformBlockCreateInfoEXT))
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineCoverageModulationStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineCoverageModulationStateCreateInfoNV))
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR -> SomeVkStruct <$> (fromCStructImageFormatListCreateInfoKHR =<< peek (castPtr p :: Ptr VkImageFormatListCreateInfoKHR))
    VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructValidationCacheCreateInfoEXT =<< peek (castPtr p :: Ptr VkValidationCacheCreateInfoEXT))
    VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructShaderModuleValidationCacheCreateInfoEXT =<< peek (castPtr p :: Ptr VkShaderModuleValidationCacheCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES -> SomeVkStruct <$> (fromCStructPhysicalDeviceMaintenance3Properties =<< peek (castPtr p :: Ptr VkPhysicalDeviceMaintenance3Properties))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT -> SomeVkStruct <$> (fromCStructDescriptorSetLayoutSupport =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutSupport))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES -> SomeVkStruct <$> (fromCStructPhysicalDeviceShaderDrawParametersFeatures =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderDrawParametersFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceFloat16Int8FeaturesKHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceFloat16Int8FeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceFloatControlsPropertiesKHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceFloatControlsPropertiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceHostQueryResetFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceHostQueryResetFeaturesEXT))
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
    VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT -> SomeVkStruct <$> (fromCStructCalibratedTimestampInfoEXT =<< peek (castPtr p :: Ptr VkCalibratedTimestampInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD -> SomeVkStruct <$> (fromCStructPhysicalDeviceShaderCorePropertiesAMD =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderCorePropertiesAMD))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineRasterizationConservativeStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineRasterizationConservativeStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceDescriptorIndexingFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceDescriptorIndexingPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDescriptorSetLayoutBindingFlagsCreateInfoEXT =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT -> SomeVkStruct <$> (fromCStructDescriptorSetVariableDescriptorCountAllocateInfoEXT =<< peek (castPtr p :: Ptr VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT -> SomeVkStruct <$> (fromCStructDescriptorSetVariableDescriptorCountLayoutSupportEXT =<< peek (castPtr p :: Ptr VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
    VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR -> SomeVkStruct <$> (fromCStructAttachmentDescription2KHR =<< peek (castPtr p :: Ptr VkAttachmentDescription2KHR))
    VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR -> SomeVkStruct <$> (fromCStructAttachmentReference2KHR =<< peek (castPtr p :: Ptr VkAttachmentReference2KHR))
    VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR -> SomeVkStruct <$> (fromCStructSubpassDescription2KHR =<< peek (castPtr p :: Ptr VkSubpassDescription2KHR))
    VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR -> SomeVkStruct <$> (fromCStructSubpassDependency2KHR =<< peek (castPtr p :: Ptr VkSubpassDependency2KHR))
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR -> SomeVkStruct <$> (fromCStructRenderPassCreateInfo2KHR =<< peek (castPtr p :: Ptr VkRenderPassCreateInfo2KHR))
    VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR -> SomeVkStruct <$> (fromCStructSubpassBeginInfoKHR =<< peek (castPtr p :: Ptr VkSubpassBeginInfoKHR))
    VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR -> SomeVkStruct <$> (fromCStructSubpassEndInfoKHR =<< peek (castPtr p :: Ptr VkSubpassEndInfoKHR))
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineVertexInputDivisorStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineVertexInputDivisorStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceVertexAttributeDivisorPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDevicePCIBusInfoPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDevicePCIBusInfoPropertiesEXT))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> SomeVkStruct <$> (fromCStructImportAndroidHardwareBufferInfoANDROID =<< peek (castPtr p :: Ptr VkImportAndroidHardwareBufferInfoANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID -> SomeVkStruct <$> (fromCStructAndroidHardwareBufferUsageANDROID =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferUsageANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID -> SomeVkStruct <$> (fromCStructAndroidHardwareBufferPropertiesANDROID =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferPropertiesANDROID))
    VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> SomeVkStruct <$> (fromCStructMemoryGetAndroidHardwareBufferInfoANDROID =<< peek (castPtr p :: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID -> SomeVkStruct <$> (fromCStructAndroidHardwareBufferFormatPropertiesANDROID =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferFormatPropertiesANDROID))
#endif
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT -> SomeVkStruct <$> (fromCStructCommandBufferInheritanceConditionalRenderingInfoEXT =<< peek (castPtr p :: Ptr VkCommandBufferInheritanceConditionalRenderingInfoEXT))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID -> SomeVkStruct <$> (fromCStructExternalFormatANDROID =<< peek (castPtr p :: Ptr VkExternalFormatANDROID))
#endif
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDevice8BitStorageFeaturesKHR =<< peek (castPtr p :: Ptr VkPhysicalDevice8BitStorageFeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceConditionalRenderingFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceConditionalRenderingFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceVulkanMemoryModelFeaturesKHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceShaderAtomicInt64FeaturesKHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderAtomicInt64FeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceVertexAttributeDivisorFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT))
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV -> SomeVkStruct <$> (fromCStructQueueFamilyCheckpointPropertiesNV =<< peek (castPtr p :: Ptr VkQueueFamilyCheckpointPropertiesNV))
    VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV -> SomeVkStruct <$> (fromCStructCheckpointDataNV =<< peek (castPtr p :: Ptr VkCheckpointDataNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR -> SomeVkStruct <$> (fromCStructPhysicalDeviceDepthStencilResolvePropertiesKHR =<< peek (castPtr p :: Ptr VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
    VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR -> SomeVkStruct <$> (fromCStructSubpassDescriptionDepthStencilResolveKHR =<< peek (castPtr p :: Ptr VkSubpassDescriptionDepthStencilResolveKHR))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT -> SomeVkStruct <$> (fromCStructImageViewASTCDecodeModeEXT =<< peek (castPtr p :: Ptr VkImageViewASTCDecodeModeEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceASTCDecodeFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceASTCDecodeFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceTransformFeedbackFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceTransformFeedbackFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceTransformFeedbackPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceTransformFeedbackPropertiesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineRasterizationStateStreamCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineRasterizationStateStreamCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceRepresentativeFragmentTestFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV))
    VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineRepresentativeFragmentTestStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineRepresentativeFragmentTestStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceExclusiveScissorFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceExclusiveScissorFeaturesNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineViewportExclusiveScissorStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineViewportExclusiveScissorStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceCornerSampledImageFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceCornerSampledImageFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceComputeShaderDerivativesFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceComputeShaderDerivativesFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceFragmentShaderBarycentricFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceShaderImageFootprintFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderImageFootprintFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineViewportShadingRateImageStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineViewportShadingRateImageStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceShadingRateImageFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceShadingRateImageFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceShadingRateImagePropertiesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceShadingRateImagePropertiesNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructPipelineViewportCoarseSampleOrderStateCreateInfoNV =<< peek (castPtr p :: Ptr VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceMeshShaderFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceMeshShaderFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceMeshShaderPropertiesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceMeshShaderPropertiesNV))
    VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructRayTracingShaderGroupCreateInfoNV =<< peek (castPtr p :: Ptr VkRayTracingShaderGroupCreateInfoNV))
    VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructRayTracingPipelineCreateInfoNV =<< peek (castPtr p :: Ptr VkRayTracingPipelineCreateInfoNV))
    VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV -> SomeVkStruct <$> (fromCStructGeometryTrianglesNV =<< peek (castPtr p :: Ptr VkGeometryTrianglesNV))
    VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV -> SomeVkStruct <$> (fromCStructGeometryAABBNV =<< peek (castPtr p :: Ptr VkGeometryAABBNV))
    VK_STRUCTURE_TYPE_GEOMETRY_NV -> SomeVkStruct <$> (fromCStructGeometryNV =<< peek (castPtr p :: Ptr VkGeometryNV))
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV -> SomeVkStruct <$> (fromCStructAccelerationStructureInfoNV =<< peek (castPtr p :: Ptr VkAccelerationStructureInfoNV))
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV -> SomeVkStruct <$> (fromCStructAccelerationStructureCreateInfoNV =<< peek (castPtr p :: Ptr VkAccelerationStructureCreateInfoNV))
    VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV -> SomeVkStruct <$> (fromCStructBindAccelerationStructureMemoryInfoNV =<< peek (castPtr p :: Ptr VkBindAccelerationStructureMemoryInfoNV))
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV -> SomeVkStruct <$> (fromCStructWriteDescriptorSetAccelerationStructureNV =<< peek (castPtr p :: Ptr VkWriteDescriptorSetAccelerationStructureNV))
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV -> SomeVkStruct <$> (fromCStructAccelerationStructureMemoryRequirementsInfoNV =<< peek (castPtr p :: Ptr VkAccelerationStructureMemoryRequirementsInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceRayTracingPropertiesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceRayTracingPropertiesNV))
    VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT -> SomeVkStruct <$> (fromCStructDrmFormatModifierPropertiesListEXT =<< peek (castPtr p :: Ptr VkDrmFormatModifierPropertiesListEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceImageDrmFormatModifierInfoEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructImageDrmFormatModifierListCreateInfoEXT =<< peek (castPtr p :: Ptr VkImageDrmFormatModifierListCreateInfoEXT))
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructImageDrmFormatModifierExplicitCreateInfoEXT =<< peek (castPtr p :: Ptr VkImageDrmFormatModifierExplicitCreateInfoEXT))
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructImageDrmFormatModifierPropertiesEXT =<< peek (castPtr p :: Ptr VkImageDrmFormatModifierPropertiesEXT))
    VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructImageStencilUsageCreateInfoEXT =<< peek (castPtr p :: Ptr VkImageStencilUsageCreateInfoEXT))
    VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD -> SomeVkStruct <$> (fromCStructDeviceMemoryOverallocationCreateInfoAMD =<< peek (castPtr p :: Ptr VkDeviceMemoryOverallocationCreateInfoAMD))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceFragmentDensityMapFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceFragmentDensityMapPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
    VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructRenderPassFragmentDensityMapCreateInfoEXT =<< peek (castPtr p :: Ptr VkRenderPassFragmentDensityMapCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceScalarBlockLayoutFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))
    VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR -> SomeVkStruct <$> (fromCStructSurfaceProtectedCapabilitiesKHR =<< peek (castPtr p :: Ptr VkSurfaceProtectedCapabilitiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceDepthClipEnableFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceDepthClipEnableFeaturesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineRasterizationDepthClipStateCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineRasterizationDepthClipStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceMemoryBudgetPropertiesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceMemoryBudgetPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceMemoryPriorityFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceMemoryPriorityFeaturesEXT))
    VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT -> SomeVkStruct <$> (fromCStructMemoryPriorityAllocateInfoEXT =<< peek (castPtr p :: Ptr VkMemoryPriorityAllocateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceBufferDeviceAddressFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
    VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT -> SomeVkStruct <$> (fromCStructBufferDeviceAddressInfoEXT =<< peek (castPtr p :: Ptr VkBufferDeviceAddressInfoEXT))
    VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructBufferDeviceAddressCreateInfoEXT =<< peek (castPtr p :: Ptr VkBufferDeviceAddressCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceImageViewImageFormatInfoEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceImageViewImageFormatInfoEXT))
    VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT -> SomeVkStruct <$> (fromCStructFilterCubicImageViewImageFormatPropertiesEXT =<< peek (castPtr p :: Ptr VkFilterCubicImageViewImageFormatPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceCooperativeMatrixFeaturesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceCooperativeMatrixFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV -> SomeVkStruct <$> (fromCStructPhysicalDeviceCooperativeMatrixPropertiesNV =<< peek (castPtr p :: Ptr VkPhysicalDeviceCooperativeMatrixPropertiesNV))
    VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV -> SomeVkStruct <$> (fromCStructCooperativeMatrixPropertiesNV =<< peek (castPtr p :: Ptr VkCooperativeMatrixPropertiesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT -> SomeVkStruct <$> (fromCStructPhysicalDeviceYcbcrImageArraysFeaturesEXT =<< peek (castPtr p :: Ptr VkPhysicalDeviceYcbcrImageArraysFeaturesEXT))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX -> SomeVkStruct <$> (fromCStructImageViewHandleInfoNVX =<< peek (castPtr p :: Ptr VkImageViewHandleInfoNVX))
#if defined(VK_USE_PLATFORM_GGP)
    VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP -> SomeVkStruct <$> (fromCStructPresentFrameTokenGGP =<< peek (castPtr p :: Ptr VkPresentFrameTokenGGP))
#endif
    VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructPipelineCreationFeedbackCreateInfoEXT =<< peek (castPtr p :: Ptr VkPipelineCreationFeedbackCreateInfoEXT))
    VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT -> SomeVkStruct <$> (fromCStructSurfaceFullScreenExclusiveInfoEXT =<< peek (castPtr p :: Ptr VkSurfaceFullScreenExclusiveInfoEXT))
    VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT -> SomeVkStruct <$> (fromCStructSurfaceFullScreenExclusiveWin32InfoEXT =<< peek (castPtr p :: Ptr VkSurfaceFullScreenExclusiveWin32InfoEXT))
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT -> SomeVkStruct <$> (fromCStructSurfaceCapabilitiesFullScreenExclusiveEXT =<< peek (castPtr p :: Ptr VkSurfaceCapabilitiesFullScreenExclusiveEXT))
    VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT -> SomeVkStruct <$> (fromCStructHeadlessSurfaceCreateInfoEXT =<< peek (castPtr p :: Ptr VkHeadlessSurfaceCreateInfoEXT))
    t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)
