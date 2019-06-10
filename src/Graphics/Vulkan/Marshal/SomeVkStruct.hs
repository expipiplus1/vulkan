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
{-# language DefaultSignatures #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Marshal.SomeVkStruct
  ( 
#if defined(VK_USE_PLATFORM_GGP)
  FromCStruct(..)
  , 
  SomeVkStruct(..)
  , HasNext(..)
  , fromSomeVkStruct
  , fromSomeVkStructChain
  , withSomeVkStruct
  , fromCStructPtr
  , fromCStructPtrElem
  , peekVkStruct
#endif
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



#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.BufferView
  ( VkBufferViewCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.CommandBuffer
  ( VkCommandBufferAllocateInfo(..)
  , VkCommandBufferBeginInfo(..)
  , VkCommandBufferInheritanceInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.CommandBufferBuilding
  ( VkBufferMemoryBarrier(..)
  , VkImageMemoryBarrier(..)
  , VkMemoryBarrier(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.DescriptorSet
  ( VkCopyDescriptorSet(..)
  , VkDescriptorPoolCreateInfo(..)
  , VkDescriptorSetAllocateInfo(..)
  , VkDescriptorSetLayoutCreateInfo(..)
  , VkWriteDescriptorSet(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceCreateInfo(..)
  , VkDeviceQueueCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkApplicationInfo(..)
  , VkInstanceCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Event
  ( VkEventCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Fence
  ( VkFenceCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Image
  ( VkImageCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.ImageView
  ( VkImageViewCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Memory
  ( VkMappedMemoryRange(..)
  , VkMemoryAllocateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Pass
  ( VkFramebufferCreateInfo(..)
  , VkRenderPassCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Pipeline
  ( VkComputePipelineCreateInfo(..)
  , VkGraphicsPipelineCreateInfo(..)
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
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.PipelineLayout
  ( VkPipelineLayoutCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPoolCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.QueueSemaphore
  ( VkSemaphoreCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Sampler
  ( VkSamplerCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Shader
  ( VkShaderModuleCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement
  ( VkBindSparseInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup
  ( VkPhysicalDeviceSubgroupProperties(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage
  ( VkPhysicalDevice16BitStorageFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( VkDescriptorUpdateTemplateCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( VkBindBufferMemoryDeviceGroupInfo(..)
  , VkBindImageMemoryDeviceGroupInfo(..)
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence
  ( VkExportFenceCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( VkExternalFenceProperties(..)
  , VkPhysicalDeviceExternalFenceInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalBufferProperties(..)
  , VkExternalImageFormatProperties(..)
  , VkPhysicalDeviceExternalBufferInfo(..)
  , VkPhysicalDeviceExternalImageFormatInfo(..)
  , VkPhysicalDeviceIDProperties(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore
  ( VkExportSemaphoreCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( VkExternalSemaphoreProperties(..)
  , VkPhysicalDeviceExternalSemaphoreInfo(..)
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2
  ( VkImageViewUsageCreateInfo(..)
  , VkPhysicalDevicePointClippingProperties(..)
  , VkPipelineTessellationDomainOriginStateCreateInfo(..)
  , VkRenderPassInputAttachmentAspectCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3
  ( VkDescriptorSetLayoutSupport(..)
  , VkPhysicalDeviceMaintenance3Properties(..)
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview
  ( VkPhysicalDeviceMultiviewFeatures(..)
  , VkPhysicalDeviceMultiviewProperties(..)
  , VkRenderPassMultiviewCreateInfo(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( VkPhysicalDeviceShaderDrawParametersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers
  ( VkPhysicalDeviceVariablePointersFeatures(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_AMD_display_native_hdr
  ( VkDisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , VkSwapchainDisplayNativeHdrCreateInfoAMD(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD
  , pattern VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_AMD_memory_overallocation_behavior
  ( VkDeviceMemoryOverallocationCreateInfoAMD(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( VkPipelineRasterizationStateRasterizationOrderAMD(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_AMD_shader_core_properties
  ( VkPhysicalDeviceShaderCorePropertiesAMD(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_AMD_texture_gather_bias_lod
  ( VkTextureLODGatherFormatPropertiesAMD(..)
  , pattern VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_astc_decode_mode
  ( VkImageViewASTCDecodeModeEXT(..)
  , VkPhysicalDeviceASTCDecodeFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_blend_operation_advanced
  ( VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , VkPipelineColorBlendAdvancedStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT(..)
  , VkBufferDeviceAddressInfoEXT(..)
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_calibrated_timestamps
  ( VkCalibratedTimestampInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_conditional_rendering
  ( VkCommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , VkConditionalRenderingBeginInfoEXT(..)
  , VkPhysicalDeviceConditionalRenderingFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkPhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , VkPipelineRasterizationConservativeStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_marker
  ( VkDebugMarkerMarkerInfoEXT(..)
  , VkDebugMarkerObjectNameInfoEXT(..)
  , VkDebugMarkerObjectTagInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_debug_report
  ( VkDebugReportCallbackCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_depth_clip_enable
  ( VkPhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , VkPipelineRasterizationDepthClipStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_discard_rectangles
  ( VkPhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , VkPipelineDiscardRectangleStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_display_surface_counter
  ( VkSurfaceCapabilities2EXT(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_external_memory_host
  ( VkImportMemoryHostPointerInfoEXT(..)
  , VkMemoryHostPointerPropertiesEXT(..)
  , VkPhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_filter_cubic
  ( VkFilterCubicImageViewImageFormatPropertiesEXT(..)
  , VkPhysicalDeviceImageViewImageFormatInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_fragment_density_map
  ( VkPhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , VkPhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , VkRenderPassFragmentDensityMapCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_full_screen_exclusive
  ( VkSurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , VkSurfaceFullScreenExclusiveInfoEXT(..)
  , VkSurfaceFullScreenExclusiveWin32InfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_global_priority
  ( VkDeviceQueueGlobalPriorityCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_hdr_metadata
  ( VkHdrMetadataEXT(..)
  , pattern VK_STRUCTURE_TYPE_HDR_METADATA_EXT
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_headless_surface
  ( VkHeadlessSurfaceCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_host_query_reset
  ( VkPhysicalDeviceHostQueryResetFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_image_drm_format_modifier
  ( VkDrmFormatModifierPropertiesListEXT(..)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_budget
  ( VkPhysicalDeviceMemoryBudgetPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( VkMemoryPriorityAllocateInfoEXT(..)
  , VkPhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_metal_surface
  ( VkMetalSurfaceCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_pci_bus_info
  ( VkPhysicalDevicePCIBusInfoPropertiesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT
  )
#endif
import Graphics.Vulkan.C.Extensions.VK_EXT_pipeline_creation_feedback
  ( VkPipelineCreationFeedbackCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_sample_locations
  ( VkMultisamplePropertiesEXT(..)
  , VkPhysicalDeviceSampleLocationsPropertiesEXT(..)
  , VkPipelineSampleLocationsStateCreateInfoEXT(..)
  , VkRenderPassSampleLocationsBeginInfoEXT(..)
  , VkSampleLocationsInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , VkSamplerReductionModeCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_scalar_block_layout
  ( VkPhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_separate_stencil_usage
  ( VkImageStencilUsageCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( VkPhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , VkPhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , VkPipelineRasterizationStateStreamCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_cache
  ( VkShaderModuleValidationCacheCreateInfoEXT(..)
  , VkValidationCacheCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_features
  ( VkValidationFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_validation_flags
  ( VkValidationFlagsEXT(..)
  , pattern VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_vertex_attribute_divisor
  ( VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , VkPipelineVertexInputDivisorStateCreateInfoEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_EXT_ycbcr_image_arrays
  ( VkPhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_FUCHSIA_imagepipe_surface
  ( VkImagePipeSurfaceCreateInfoFUCHSIA(..)
  , pattern VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_frame_token
  ( VkPresentFrameTokenGGP(..)
  , pattern VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GGP_stream_descriptor_surface
  ( VkStreamDescriptorSurfaceCreateInfoGGP(..)
  , pattern VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_GOOGLE_display_timing
  ( VkPresentTimesInfoGOOGLE(..)
  , pattern VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_8bit_storage
  ( VkPhysicalDevice8BitStorageFeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_android_surface
  ( VkAndroidSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_depth_stencil_resolve
  ( VkPhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , VkSubpassDescriptionDepthStencilResolveKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_display
  ( VkDisplayModeCreateInfoKHR(..)
  , VkDisplaySurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_display_swapchain
  ( VkDisplayPresentInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_driver_properties
  ( VkPhysicalDeviceDriverPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_fd
  ( VkFenceGetFdInfoKHR(..)
  , VkImportFenceFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_fence_win32
  ( VkExportFenceWin32HandleInfoKHR(..)
  , VkFenceGetWin32HandleInfoKHR(..)
  , VkImportFenceWin32HandleInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_memory_fd
  ( VkImportMemoryFdInfoKHR(..)
  , VkMemoryFdPropertiesKHR(..)
  , VkMemoryGetFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_external_semaphore_fd
  ( VkImportSemaphoreFdInfoKHR(..)
  , VkSemaphoreGetFdInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_get_surface_capabilities2
  ( VkPhysicalDeviceSurfaceInfo2KHR(..)
  , VkSurfaceCapabilities2KHR(..)
  , VkSurfaceFormat2KHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR
  , pattern VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_image_format_list
  ( VkImageFormatListCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_incremental_present
  ( VkPresentRegionsKHR(..)
  , pattern VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_push_descriptor
  ( VkPhysicalDevicePushDescriptorPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_atomic_int64
  ( VkPhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float16_int8
  ( VkPhysicalDeviceFloat16Int8FeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_shader_float_controls
  ( VkPhysicalDeviceFloatControlsPropertiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_shared_presentable_image
  ( VkSharedPresentSurfaceCapabilitiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_surface_protected_capabilities
  ( VkSurfaceProtectedCapabilitiesKHR(..)
  , pattern VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_vulkan_memory_model
  ( VkPhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_wayland_surface
  ( VkWaylandSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_win32_surface
  ( VkWin32SurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_xcb_surface
  ( VkXcbSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_KHR_xlib_surface
  ( VkXlibSurfaceCreateInfoKHR(..)
  , pattern VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_MVK_ios_surface
  ( VkIOSSurfaceCreateInfoMVK(..)
  , pattern VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_MVK_macos_surface
  ( VkMacOSSurfaceCreateInfoMVK(..)
  , pattern VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NN_vi_surface
  ( VkViSurfaceCreateInfoNN(..)
  , pattern VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NVX_device_generated_commands
  ( VkCmdReserveSpaceForCommandsInfoNVX(..)
  , VkDeviceGeneratedCommandsFeaturesNVX(..)
  , VkDeviceGeneratedCommandsLimitsNVX(..)
  , VkIndirectCommandsLayoutCreateInfoNVX(..)
  , VkObjectTableCreateInfoNVX(..)
  , pattern VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX
  , pattern VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX
  , pattern VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX
  , pattern VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NVX_image_view_handle
  ( VkImageViewHandleInfoNVX(..)
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NVX_multiview_per_view_attributes
  ( VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_clip_space_w_scaling
  ( VkPipelineViewportWScalingStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_compute_shader_derivatives
  ( VkPhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_cooperative_matrix
  ( VkCooperativeMatrixPropertiesNV(..)
  , VkPhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , VkPhysicalDeviceCooperativeMatrixPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_corner_sampled_image
  ( VkPhysicalDeviceCornerSampledImageFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_device_diagnostic_checkpoints
  ( VkCheckpointDataNV(..)
  , VkQueueFamilyCheckpointPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV
  , pattern VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory
  ( VkExportMemoryAllocateInfoNV(..)
  , VkExternalMemoryImageCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_external_memory_win32
  ( VkExportMemoryWin32HandleInfoNV(..)
  , VkImportMemoryWin32HandleInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_coverage_to_color
  ( VkPipelineCoverageToColorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_fragment_shader_barycentric
  ( VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkPipelineCoverageModulationStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_mesh_shader
  ( VkPhysicalDeviceMeshShaderFeaturesNV(..)
  , VkPhysicalDeviceMeshShaderPropertiesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( VkAccelerationStructureCreateInfoNV(..)
  , VkAccelerationStructureInfoNV(..)
  , VkAccelerationStructureMemoryRequirementsInfoNV(..)
  , VkBindAccelerationStructureMemoryInfoNV(..)
  , VkGeometryAABBNV(..)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_representative_fragment_test
  ( VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , VkPipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_scissor_exclusive
  ( VkPhysicalDeviceExclusiveScissorFeaturesNV(..)
  , VkPipelineViewportExclusiveScissorStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_shader_image_footprint
  ( VkPhysicalDeviceShaderImageFootprintFeaturesNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_shading_rate_image
  ( VkPhysicalDeviceShadingRateImageFeaturesNV(..)
  , VkPhysicalDeviceShadingRateImagePropertiesNV(..)
  , VkPipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , VkPipelineViewportShadingRateImageStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_viewport_swizzle
  ( VkPipelineViewportSwizzleStateCreateInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Extensions.VK_NV_win32_keyed_mutex
  ( VkWin32KeyedMutexAcquireReleaseInfoNV(..)
  , pattern VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Buffer
  ( BufferCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.BufferView
  ( BufferViewCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.CommandBuffer
  ( CommandBufferAllocateInfo(..)
  , CommandBufferBeginInfo(..)
  , CommandBufferInheritanceInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.CommandBufferBuilding
  ( BufferMemoryBarrier(..)
  , ImageMemoryBarrier(..)
  , MemoryBarrier(..)
  , RenderPassBeginInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.CommandPool
  ( CommandPoolCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DescriptorSet
  ( CopyDescriptorSet(..)
  , DescriptorPoolCreateInfo(..)
  , DescriptorSetAllocateInfo(..)
  , DescriptorSetLayoutCreateInfo(..)
  , WriteDescriptorSet(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Device
  ( DeviceCreateInfo(..)
  , DeviceQueueCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.DeviceInitialization
  ( ApplicationInfo(..)
  , InstanceCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Event
  ( EventCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Fence
  ( FenceCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Image
  ( ImageCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.ImageView
  ( ImageViewCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Memory
  ( MappedMemoryRange(..)
  , MemoryAllocateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Pass
  ( FramebufferCreateInfo(..)
  , RenderPassCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCacheCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.PipelineLayout
  ( PipelineLayoutCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Query
  ( QueryPoolCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Queue
  ( SubmitInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.QueueSemaphore
  ( SemaphoreCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Sampler
  ( SamplerCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.Shader
  ( ShaderModuleCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core10.SparseResourceMemoryManagement
  ( BindSparseInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory
  ( DeviceQueueInfo2(..)
  , PhysicalDeviceProtectedMemoryFeatures(..)
  , PhysicalDeviceProtectedMemoryProperties(..)
  , ProtectedSubmitInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup
  ( PhysicalDeviceSubgroupProperties(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage
  ( PhysicalDevice16BitStorageFeatures(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( BindBufferMemoryInfo(..)
  , BindImageMemoryInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( MemoryDedicatedAllocateInfo(..)
  , MemoryDedicatedRequirements(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template
  ( DescriptorUpdateTemplateCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group
  ( DeviceGroupBindSparseInfo(..)
  , DeviceGroupCommandBufferBeginInfo(..)
  , DeviceGroupRenderPassBeginInfo(..)
  , DeviceGroupSubmitInfo(..)
  , MemoryAllocateFlagsInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2
  ( BindBufferMemoryDeviceGroupInfo(..)
  , BindImageMemoryDeviceGroupInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfo(..)
  , PhysicalDeviceGroupProperties(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence
  ( ExportFenceCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities
  ( ExternalFenceProperties(..)
  , PhysicalDeviceExternalFenceInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
  ( ExportMemoryAllocateInfo(..)
  , ExternalMemoryBufferCreateInfo(..)
  , ExternalMemoryImageCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( ExternalBufferProperties(..)
  , ExternalImageFormatProperties(..)
  , PhysicalDeviceExternalBufferInfo(..)
  , PhysicalDeviceExternalImageFormatInfo(..)
  , PhysicalDeviceIDProperties(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore
  ( ExportSemaphoreCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities
  ( ExternalSemaphoreProperties(..)
  , PhysicalDeviceExternalSemaphoreInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2
  ( BufferMemoryRequirementsInfo2(..)
  , ImageMemoryRequirementsInfo2(..)
  , ImageSparseMemoryRequirementsInfo2(..)
  , MemoryRequirements2(..)
  , SparseImageMemoryRequirements2(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2
  ( ImageViewUsageCreateInfo(..)
  , PhysicalDevicePointClippingProperties(..)
  , PipelineTessellationDomainOriginStateCreateInfo(..)
  , RenderPassInputAttachmentAspectCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3
  ( DescriptorSetLayoutSupport(..)
  , PhysicalDeviceMaintenance3Properties(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview
  ( PhysicalDeviceMultiviewFeatures(..)
  , PhysicalDeviceMultiviewProperties(..)
  , RenderPassMultiviewCreateInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion
  ( BindImagePlaneMemoryInfo(..)
  , ImagePlaneMemoryRequirementsInfo(..)
  , PhysicalDeviceSamplerYcbcrConversionFeatures(..)
  , SamplerYcbcrConversionCreateInfo(..)
  , SamplerYcbcrConversionImageFormatProperties(..)
  , SamplerYcbcrConversionInfo(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters
  ( PhysicalDeviceShaderDrawParametersFeatures(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers
  ( PhysicalDeviceVariablePointersFeatures(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr
  ( DisplayNativeHdrSurfaceCapabilitiesAMD(..)
  , SwapchainDisplayNativeHdrCreateInfoAMD(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_AMD_memory_overallocation_behavior
  ( DeviceMemoryOverallocationCreateInfoAMD(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( PipelineRasterizationStateRasterizationOrderAMD(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties
  ( PhysicalDeviceShaderCorePropertiesAMD(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod
  ( TextureLODGatherFormatPropertiesAMD(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer
  ( AndroidHardwareBufferFormatPropertiesANDROID(..)
  , AndroidHardwareBufferPropertiesANDROID(..)
  , AndroidHardwareBufferUsageANDROID(..)
  , ExternalFormatANDROID(..)
  , ImportAndroidHardwareBufferInfoANDROID(..)
  , MemoryGetAndroidHardwareBufferInfoANDROID(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_astc_decode_mode
  ( ImageViewASTCDecodeModeEXT(..)
  , PhysicalDeviceASTCDecodeFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced
  ( PhysicalDeviceBlendOperationAdvancedFeaturesEXT(..)
  , PhysicalDeviceBlendOperationAdvancedPropertiesEXT(..)
  , PipelineColorBlendAdvancedStateCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_buffer_device_address
  ( BufferDeviceAddressCreateInfoEXT(..)
  , BufferDeviceAddressInfoEXT(..)
  , PhysicalDeviceBufferDeviceAddressFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_calibrated_timestamps
  ( CalibratedTimestampInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_conditional_rendering
  ( CommandBufferInheritanceConditionalRenderingInfoEXT(..)
  , ConditionalRenderingBeginInfoEXT(..)
  , PhysicalDeviceConditionalRenderingFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( PhysicalDeviceConservativeRasterizationPropertiesEXT(..)
  , PipelineRasterizationConservativeStateCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_debug_marker
  ( DebugMarkerMarkerInfoEXT(..)
  , DebugMarkerObjectNameInfoEXT(..)
  , DebugMarkerObjectTagInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_debug_report
  ( DebugReportCallbackCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_debug_utils
  ( DebugUtilsLabelEXT(..)
  , DebugUtilsMessengerCallbackDataEXT(..)
  , DebugUtilsMessengerCreateInfoEXT(..)
  , DebugUtilsObjectNameInfoEXT(..)
  , DebugUtilsObjectTagInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_depth_clip_enable
  ( PhysicalDeviceDepthClipEnableFeaturesEXT(..)
  , PipelineRasterizationDepthClipStateCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing
  ( DescriptorSetLayoutBindingFlagsCreateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountAllocateInfoEXT(..)
  , DescriptorSetVariableDescriptorCountLayoutSupportEXT(..)
  , PhysicalDeviceDescriptorIndexingFeaturesEXT(..)
  , PhysicalDeviceDescriptorIndexingPropertiesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles
  ( PhysicalDeviceDiscardRectanglePropertiesEXT(..)
  , PipelineDiscardRectangleStateCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_display_control
  ( DeviceEventInfoEXT(..)
  , DisplayEventInfoEXT(..)
  , DisplayPowerInfoEXT(..)
  , SwapchainCounterCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter
  ( SurfaceCapabilities2EXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_external_memory_host
  ( ImportMemoryHostPointerInfoEXT(..)
  , MemoryHostPointerPropertiesEXT(..)
  , PhysicalDeviceExternalMemoryHostPropertiesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_filter_cubic
  ( FilterCubicImageViewImageFormatPropertiesEXT(..)
  , PhysicalDeviceImageViewImageFormatInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( PhysicalDeviceFragmentDensityMapFeaturesEXT(..)
  , PhysicalDeviceFragmentDensityMapPropertiesEXT(..)
  , RenderPassFragmentDensityMapCreateInfoEXT(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_full_screen_exclusive
  ( SurfaceCapabilitiesFullScreenExclusiveEXT(..)
  , SurfaceFullScreenExclusiveInfoEXT(..)
  , SurfaceFullScreenExclusiveWin32InfoEXT(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_global_priority
  ( DeviceQueueGlobalPriorityCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata
  ( HdrMetadataEXT(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_headless_surface
  ( HeadlessSurfaceCreateInfoEXT(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_host_query_reset
  ( PhysicalDeviceHostQueryResetFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_image_drm_format_modifier
  ( DrmFormatModifierPropertiesListEXT(..)
  , ImageDrmFormatModifierExplicitCreateInfoEXT(..)
  , ImageDrmFormatModifierListCreateInfoEXT(..)
  , ImageDrmFormatModifierPropertiesEXT(..)
  , PhysicalDeviceImageDrmFormatModifierInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block
  ( DescriptorPoolInlineUniformBlockCreateInfoEXT(..)
  , PhysicalDeviceInlineUniformBlockFeaturesEXT(..)
  , PhysicalDeviceInlineUniformBlockPropertiesEXT(..)
  , WriteDescriptorSetInlineUniformBlockEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_memory_budget
  ( PhysicalDeviceMemoryBudgetPropertiesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_memory_priority
  ( MemoryPriorityAllocateInfoEXT(..)
  , PhysicalDeviceMemoryPriorityFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_metal_surface
  ( MetalSurfaceCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_pci_bus_info
  ( PhysicalDevicePCIBusInfoPropertiesEXT(..)
  )
#endif
import Graphics.Vulkan.Extensions.VK_EXT_pipeline_creation_feedback
  ( PipelineCreationFeedbackCreateInfoEXT(..)
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_sample_locations
  ( MultisamplePropertiesEXT(..)
  , PhysicalDeviceSampleLocationsPropertiesEXT(..)
  , PipelineSampleLocationsStateCreateInfoEXT(..)
  , RenderPassSampleLocationsBeginInfoEXT(..)
  , SampleLocationsInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( PhysicalDeviceSamplerFilterMinmaxPropertiesEXT(..)
  , SamplerReductionModeCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_scalar_block_layout
  ( PhysicalDeviceScalarBlockLayoutFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_separate_stencil_usage
  ( ImageStencilUsageCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_transform_feedback
  ( PhysicalDeviceTransformFeedbackFeaturesEXT(..)
  , PhysicalDeviceTransformFeedbackPropertiesEXT(..)
  , PipelineRasterizationStateStreamCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_validation_cache
  ( ShaderModuleValidationCacheCreateInfoEXT(..)
  , ValidationCacheCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_validation_features
  ( ValidationFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_validation_flags
  ( ValidationFlagsEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor
  ( PhysicalDeviceVertexAttributeDivisorFeaturesEXT(..)
  , PhysicalDeviceVertexAttributeDivisorPropertiesEXT(..)
  , PipelineVertexInputDivisorStateCreateInfoEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_EXT_ycbcr_image_arrays
  ( PhysicalDeviceYcbcrImageArraysFeaturesEXT(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_FUCHSIA_imagepipe_surface
  ( ImagePipeSurfaceCreateInfoFUCHSIA(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_frame_token
  ( PresentFrameTokenGGP(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GGP_stream_descriptor_surface
  ( StreamDescriptorSurfaceCreateInfoGGP(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing
  ( PresentTimesInfoGOOGLE(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_8bit_storage
  ( PhysicalDevice8BitStorageFeaturesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_android_surface
  ( AndroidSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_create_renderpass2
  ( AttachmentDescription2KHR(..)
  , AttachmentReference2KHR(..)
  , RenderPassCreateInfo2KHR(..)
  , SubpassBeginInfoKHR(..)
  , SubpassDependency2KHR(..)
  , SubpassDescription2KHR(..)
  , SubpassEndInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_depth_stencil_resolve
  ( PhysicalDeviceDepthStencilResolvePropertiesKHR(..)
  , SubpassDescriptionDepthStencilResolveKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_display
  ( DisplayModeCreateInfoKHR(..)
  , DisplaySurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_display_swapchain
  ( DisplayPresentInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_driver_properties
  ( PhysicalDeviceDriverPropertiesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd
  ( FenceGetFdInfoKHR(..)
  , ImportFenceFdInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32
  ( ExportFenceWin32HandleInfoKHR(..)
  , FenceGetWin32HandleInfoKHR(..)
  , ImportFenceWin32HandleInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd
  ( ImportMemoryFdInfoKHR(..)
  , MemoryFdPropertiesKHR(..)
  , MemoryGetFdInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32
  ( ExportMemoryWin32HandleInfoKHR(..)
  , ImportMemoryWin32HandleInfoKHR(..)
  , MemoryGetWin32HandleInfoKHR(..)
  , MemoryWin32HandlePropertiesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd
  ( ImportSemaphoreFdInfoKHR(..)
  , SemaphoreGetFdInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32
  ( D3D12FenceSubmitInfoKHR(..)
  , ExportSemaphoreWin32HandleInfoKHR(..)
  , ImportSemaphoreWin32HandleInfoKHR(..)
  , SemaphoreGetWin32HandleInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_get_display_properties2
  ( DisplayModeProperties2KHR(..)
  , DisplayPlaneCapabilities2KHR(..)
  , DisplayPlaneInfo2KHR(..)
  , DisplayPlaneProperties2KHR(..)
  , DisplayProperties2KHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2
  ( PhysicalDeviceSurfaceInfo2KHR(..)
  , SurfaceCapabilities2KHR(..)
  , SurfaceFormat2KHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_image_format_list
  ( ImageFormatListCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_incremental_present
  ( PresentRegionsKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_push_descriptor
  ( PhysicalDevicePushDescriptorPropertiesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_shader_atomic_int64
  ( PhysicalDeviceShaderAtomicInt64FeaturesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_shader_float16_int8
  ( PhysicalDeviceFloat16Int8FeaturesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_shader_float_controls
  ( PhysicalDeviceFloatControlsPropertiesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image
  ( SharedPresentSurfaceCapabilitiesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_surface_protected_capabilities
  ( SurfaceProtectedCapabilitiesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_vulkan_memory_model
  ( PhysicalDeviceVulkanMemoryModelFeaturesKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_wayland_surface
  ( WaylandSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_win32_surface
  ( Win32SurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_xcb_surface
  ( XcbSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_KHR_xlib_surface
  ( XlibSurfaceCreateInfoKHR(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_MVK_ios_surface
  ( IOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_MVK_macos_surface
  ( MacOSSurfaceCreateInfoMVK(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NN_vi_surface
  ( ViSurfaceCreateInfoNN(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands
  ( CmdProcessCommandsInfoNVX(..)
  , CmdReserveSpaceForCommandsInfoNVX(..)
  , DeviceGeneratedCommandsFeaturesNVX(..)
  , DeviceGeneratedCommandsLimitsNVX(..)
  , IndirectCommandsLayoutCreateInfoNVX(..)
  , ObjectTableCreateInfoNVX(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NVX_image_view_handle
  ( ImageViewHandleInfoNVX(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes
  ( PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling
  ( PipelineViewportWScalingStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_compute_shader_derivatives
  ( PhysicalDeviceComputeShaderDerivativesFeaturesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_cooperative_matrix
  ( CooperativeMatrixPropertiesNV(..)
  , PhysicalDeviceCooperativeMatrixFeaturesNV(..)
  , PhysicalDeviceCooperativeMatrixPropertiesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_corner_sampled_image
  ( PhysicalDeviceCornerSampledImageFeaturesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( DedicatedAllocationBufferCreateInfoNV(..)
  , DedicatedAllocationImageCreateInfoNV(..)
  , DedicatedAllocationMemoryAllocateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation_image_aliasing
  ( PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_device_diagnostic_checkpoints
  ( CheckpointDataNV(..)
  , QueueFamilyCheckpointPropertiesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_external_memory
  ( ExportMemoryAllocateInfoNV(..)
  , ExternalMemoryImageCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_external_memory_win32
  ( ExportMemoryWin32HandleInfoNV(..)
  , ImportMemoryWin32HandleInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color
  ( PipelineCoverageToColorStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_fragment_shader_barycentric
  ( PhysicalDeviceFragmentShaderBarycentricFeaturesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( PipelineCoverageModulationStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_mesh_shader
  ( PhysicalDeviceMeshShaderFeaturesNV(..)
  , PhysicalDeviceMeshShaderPropertiesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
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
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_representative_fragment_test
  ( PhysicalDeviceRepresentativeFragmentTestFeaturesNV(..)
  , PipelineRepresentativeFragmentTestStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_scissor_exclusive
  ( PhysicalDeviceExclusiveScissorFeaturesNV(..)
  , PipelineViewportExclusiveScissorStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_shader_image_footprint
  ( PhysicalDeviceShaderImageFootprintFeaturesNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_shading_rate_image
  ( PhysicalDeviceShadingRateImageFeaturesNV(..)
  , PhysicalDeviceShadingRateImagePropertiesNV(..)
  , PipelineViewportCoarseSampleOrderStateCreateInfoNV(..)
  , PipelineViewportShadingRateImageStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle
  ( PipelineViewportSwizzleStateCreateInfoNV(..)
  )
#endif

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex
  ( Win32KeyedMutexAcquireReleaseInfoNV(..)
  )
#endif
import Graphics.Vulkan.Marshal.ToCStruct
  ( ToCStruct(..)
  )


-- | A class for converting C type structures to the marshalled types
class FromCStruct marshalled c | marshalled -> c, c -> marshalled where
  -- | Read a C type structure and dependencies
  fromCStruct :: c -> IO marshalled

-- | A class for types which can be present in a @pNext@ chain
class HasNext a where
  getNext :: a -> Maybe SomeVkStruct

-- | A wrapper for holding any Vulkan struct, this is used to
-- implement structure @pNext@ chains.
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

fromCStructPtr :: (Storable c, FromCStruct a c) => Ptr c -> IO a
fromCStructPtr p = fromCStruct =<< peek p

fromCStructPtrElem :: (Storable c, FromCStruct a c) => Ptr c -> Int -> IO a
fromCStructPtrElem p o = fromCStruct =<< peekElemOff p o

-- | Convert a 'SomeVkStruct' to a structure of a known type if
-- possible.
fromSomeVkStruct :: Typeable a => SomeVkStruct -> Maybe a
fromSomeVkStruct (SomeVkStruct s) = cast s

-- | Search the whole pointer chain for a structure of a particular
-- type and return that if possible
fromSomeVkStructChain :: Typeable a => SomeVkStruct -> Maybe a
fromSomeVkStructChain s =
  fromSomeVkStruct s <|> (getNext s >>= fromSomeVkStructChain)

-- | Allocate space for the value contained in a 'SomeVkStruct' and
-- use that in continuation.
withSomeVkStruct :: SomeVkStruct -> (Ptr () -> IO a) -> IO a
withSomeVkStruct (SomeVkStruct s) f = withCStruct s (f . castPtr)

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------











instance HasNext ApplicationInfo where
  getNext s = next (s :: ApplicationInfo)

instance HasNext DeviceQueueCreateInfo where
  getNext s = next (s :: DeviceQueueCreateInfo)
instance HasNext DeviceCreateInfo where
  getNext s = next (s :: DeviceCreateInfo)
instance HasNext InstanceCreateInfo where
  getNext s = next (s :: InstanceCreateInfo)


instance HasNext MemoryAllocateInfo where
  getNext s = next (s :: MemoryAllocateInfo)





instance HasNext MappedMemoryRange where
  getNext s = next (s :: MappedMemoryRange)




instance HasNext WriteDescriptorSet where
  getNext s = next (s :: WriteDescriptorSet)
instance HasNext CopyDescriptorSet where
  getNext s = next (s :: CopyDescriptorSet)
instance HasNext BufferCreateInfo where
  getNext s = next (s :: BufferCreateInfo)
instance HasNext BufferViewCreateInfo where
  getNext s = next (s :: BufferViewCreateInfo)



instance HasNext MemoryBarrier where
  getNext s = next (s :: MemoryBarrier)
instance HasNext BufferMemoryBarrier where
  getNext s = next (s :: BufferMemoryBarrier)
instance HasNext ImageMemoryBarrier where
  getNext s = next (s :: ImageMemoryBarrier)
instance HasNext ImageCreateInfo where
  getNext s = next (s :: ImageCreateInfo)

instance HasNext ImageViewCreateInfo where
  getNext s = next (s :: ImageViewCreateInfo)






instance HasNext BindSparseInfo where
  getNext s = next (s :: BindSparseInfo)




instance HasNext ShaderModuleCreateInfo where
  getNext s = next (s :: ShaderModuleCreateInfo)

instance HasNext DescriptorSetLayoutCreateInfo where
  getNext s = next (s :: DescriptorSetLayoutCreateInfo)

instance HasNext DescriptorPoolCreateInfo where
  getNext s = next (s :: DescriptorPoolCreateInfo)
instance HasNext DescriptorSetAllocateInfo where
  getNext s = next (s :: DescriptorSetAllocateInfo)


instance HasNext PipelineShaderStageCreateInfo where
  getNext s = next (s :: PipelineShaderStageCreateInfo)
instance HasNext ComputePipelineCreateInfo where
  getNext s = next (s :: ComputePipelineCreateInfo)


instance HasNext PipelineVertexInputStateCreateInfo where
  getNext s = next (s :: PipelineVertexInputStateCreateInfo)
instance HasNext PipelineInputAssemblyStateCreateInfo where
  getNext s = next (s :: PipelineInputAssemblyStateCreateInfo)
instance HasNext PipelineTessellationStateCreateInfo where
  getNext s = next (s :: PipelineTessellationStateCreateInfo)
instance HasNext PipelineViewportStateCreateInfo where
  getNext s = next (s :: PipelineViewportStateCreateInfo)
instance HasNext PipelineRasterizationStateCreateInfo where
  getNext s = next (s :: PipelineRasterizationStateCreateInfo)
instance HasNext PipelineMultisampleStateCreateInfo where
  getNext s = next (s :: PipelineMultisampleStateCreateInfo)

instance HasNext PipelineColorBlendStateCreateInfo where
  getNext s = next (s :: PipelineColorBlendStateCreateInfo)
instance HasNext PipelineDynamicStateCreateInfo where
  getNext s = next (s :: PipelineDynamicStateCreateInfo)

instance HasNext PipelineDepthStencilStateCreateInfo where
  getNext s = next (s :: PipelineDepthStencilStateCreateInfo)
instance HasNext GraphicsPipelineCreateInfo where
  getNext s = next (s :: GraphicsPipelineCreateInfo)
instance HasNext PipelineCacheCreateInfo where
  getNext s = next (s :: PipelineCacheCreateInfo)

instance HasNext PipelineLayoutCreateInfo where
  getNext s = next (s :: PipelineLayoutCreateInfo)
instance HasNext SamplerCreateInfo where
  getNext s = next (s :: SamplerCreateInfo)
instance HasNext CommandPoolCreateInfo where
  getNext s = next (s :: CommandPoolCreateInfo)
instance HasNext CommandBufferAllocateInfo where
  getNext s = next (s :: CommandBufferAllocateInfo)
instance HasNext CommandBufferInheritanceInfo where
  getNext s = next (s :: CommandBufferInheritanceInfo)
instance HasNext CommandBufferBeginInfo where
  getNext s = next (s :: CommandBufferBeginInfo)
instance HasNext RenderPassBeginInfo where
  getNext s = next (s :: RenderPassBeginInfo)






instance HasNext RenderPassCreateInfo where
  getNext s = next (s :: RenderPassCreateInfo)
instance HasNext EventCreateInfo where
  getNext s = next (s :: EventCreateInfo)
instance HasNext FenceCreateInfo where
  getNext s = next (s :: FenceCreateInfo)



instance HasNext SemaphoreCreateInfo where
  getNext s = next (s :: SemaphoreCreateInfo)
instance HasNext QueryPoolCreateInfo where
  getNext s = next (s :: QueryPoolCreateInfo)
instance HasNext FramebufferCreateInfo where
  getNext s = next (s :: FramebufferCreateInfo)



instance HasNext SubmitInfo where
  getNext s = next (s :: SubmitInfo)




instance HasNext DisplayModeCreateInfoKHR where
  getNext s = next (s :: DisplayModeCreateInfoKHR)

instance HasNext DisplaySurfaceCreateInfoKHR where
  getNext s = next (s :: DisplaySurfaceCreateInfoKHR)
instance HasNext DisplayPresentInfoKHR where
  getNext s = next (s :: DisplayPresentInfoKHR)


#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext AndroidSurfaceCreateInfoKHR where
  getNext s = next (s :: AndroidSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_VI_NN
instance HasNext ViSurfaceCreateInfoNN where
  getNext s = next (s :: ViSurfaceCreateInfoNN)
#endif

#if VK_USE_PLATFORM_WAYLAND_KHR
instance HasNext WaylandSurfaceCreateInfoKHR where
  getNext s = next (s :: WaylandSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext Win32SurfaceCreateInfoKHR where
  getNext s = next (s :: Win32SurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_XLIB_KHR
instance HasNext XlibSurfaceCreateInfoKHR where
  getNext s = next (s :: XlibSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_XCB_KHR
instance HasNext XcbSurfaceCreateInfoKHR where
  getNext s = next (s :: XcbSurfaceCreateInfoKHR)
#endif

#if VK_USE_PLATFORM_FUCHSIA
instance HasNext ImagePipeSurfaceCreateInfoFUCHSIA where
  getNext s = next (s :: ImagePipeSurfaceCreateInfoFUCHSIA)
#endif

#if VK_USE_PLATFORM_GGP
instance HasNext StreamDescriptorSurfaceCreateInfoGGP where
  getNext s = next (s :: StreamDescriptorSurfaceCreateInfoGGP)
#endif

instance HasNext SwapchainCreateInfoKHR where
  getNext s = next (s :: SwapchainCreateInfoKHR)
instance HasNext PresentInfoKHR where
  getNext s = next (s :: PresentInfoKHR)
instance HasNext DebugReportCallbackCreateInfoEXT where
  getNext s = next (s :: DebugReportCallbackCreateInfoEXT)
instance HasNext ValidationFlagsEXT where
  getNext s = next (s :: ValidationFlagsEXT)
instance HasNext ValidationFeaturesEXT where
  getNext s = next (s :: ValidationFeaturesEXT)
instance HasNext PipelineRasterizationStateRasterizationOrderAMD where
  getNext s = next (s :: PipelineRasterizationStateRasterizationOrderAMD)
instance HasNext DebugMarkerObjectNameInfoEXT where
  getNext s = next (s :: DebugMarkerObjectNameInfoEXT)
instance HasNext DebugMarkerObjectTagInfoEXT where
  getNext s = next (s :: DebugMarkerObjectTagInfoEXT)
instance HasNext DebugMarkerMarkerInfoEXT where
  getNext s = next (s :: DebugMarkerMarkerInfoEXT)
instance HasNext DedicatedAllocationImageCreateInfoNV where
  getNext s = next (s :: DedicatedAllocationImageCreateInfoNV)
instance HasNext DedicatedAllocationBufferCreateInfoNV where
  getNext s = next (s :: DedicatedAllocationBufferCreateInfoNV)
instance HasNext DedicatedAllocationMemoryAllocateInfoNV where
  getNext s = next (s :: DedicatedAllocationMemoryAllocateInfoNV)

instance HasNext ExternalMemoryImageCreateInfoNV where
  getNext s = next (s :: ExternalMemoryImageCreateInfoNV)
instance HasNext ExportMemoryAllocateInfoNV where
  getNext s = next (s :: ExportMemoryAllocateInfoNV)

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ImportMemoryWin32HandleInfoNV where
  getNext s = next (s :: ImportMemoryWin32HandleInfoNV)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ExportMemoryWin32HandleInfoNV where
  getNext s = next (s :: ExportMemoryWin32HandleInfoNV)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext Win32KeyedMutexAcquireReleaseInfoNV where
  getNext s = next (s :: Win32KeyedMutexAcquireReleaseInfoNV)
#endif
instance HasNext DeviceGeneratedCommandsFeaturesNVX where
  getNext s = next (s :: DeviceGeneratedCommandsFeaturesNVX)
instance HasNext DeviceGeneratedCommandsLimitsNVX where
  getNext s = next (s :: DeviceGeneratedCommandsLimitsNVX)


instance HasNext IndirectCommandsLayoutCreateInfoNVX where
  getNext s = next (s :: IndirectCommandsLayoutCreateInfoNVX)
instance HasNext CmdProcessCommandsInfoNVX where
  getNext s = next (s :: CmdProcessCommandsInfoNVX)
instance HasNext CmdReserveSpaceForCommandsInfoNVX where
  getNext s = next (s :: CmdReserveSpaceForCommandsInfoNVX)
instance HasNext ObjectTableCreateInfoNVX where
  getNext s = next (s :: ObjectTableCreateInfoNVX)






instance HasNext PhysicalDeviceFeatures2 where
  getNext s = next (s :: PhysicalDeviceFeatures2)
instance HasNext PhysicalDeviceProperties2 where
  getNext s = next (s :: PhysicalDeviceProperties2)
instance HasNext FormatProperties2 where
  getNext s = next (s :: FormatProperties2)
instance HasNext ImageFormatProperties2 where
  getNext s = next (s :: ImageFormatProperties2)
instance HasNext PhysicalDeviceImageFormatInfo2 where
  getNext s = next (s :: PhysicalDeviceImageFormatInfo2)
instance HasNext QueueFamilyProperties2 where
  getNext s = next (s :: QueueFamilyProperties2)
instance HasNext PhysicalDeviceMemoryProperties2 where
  getNext s = next (s :: PhysicalDeviceMemoryProperties2)
instance HasNext SparseImageFormatProperties2 where
  getNext s = next (s :: SparseImageFormatProperties2)
instance HasNext PhysicalDeviceSparseImageFormatInfo2 where
  getNext s = next (s :: PhysicalDeviceSparseImageFormatInfo2)
instance HasNext PhysicalDevicePushDescriptorPropertiesKHR where
  getNext s = next (s :: PhysicalDevicePushDescriptorPropertiesKHR)

instance HasNext PhysicalDeviceDriverPropertiesKHR where
  getNext s = next (s :: PhysicalDeviceDriverPropertiesKHR)
instance HasNext PresentRegionsKHR where
  getNext s = next (s :: PresentRegionsKHR)


instance HasNext PhysicalDeviceVariablePointersFeatures where
  getNext s = next (s :: PhysicalDeviceVariablePointersFeatures)

instance HasNext PhysicalDeviceExternalImageFormatInfo where
  getNext s = next (s :: PhysicalDeviceExternalImageFormatInfo)
instance HasNext ExternalImageFormatProperties where
  getNext s = next (s :: ExternalImageFormatProperties)
instance HasNext PhysicalDeviceExternalBufferInfo where
  getNext s = next (s :: PhysicalDeviceExternalBufferInfo)
instance HasNext ExternalBufferProperties where
  getNext s = next (s :: ExternalBufferProperties)
instance HasNext PhysicalDeviceIDProperties where
  getNext s = next (s :: PhysicalDeviceIDProperties)
instance HasNext ExternalMemoryImageCreateInfo where
  getNext s = next (s :: ExternalMemoryImageCreateInfo)
instance HasNext ExternalMemoryBufferCreateInfo where
  getNext s = next (s :: ExternalMemoryBufferCreateInfo)
instance HasNext ExportMemoryAllocateInfo where
  getNext s = next (s :: ExportMemoryAllocateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ImportMemoryWin32HandleInfoKHR where
  getNext s = next (s :: ImportMemoryWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ExportMemoryWin32HandleInfoKHR where
  getNext s = next (s :: ExportMemoryWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext MemoryWin32HandlePropertiesKHR where
  getNext s = next (s :: MemoryWin32HandlePropertiesKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext MemoryGetWin32HandleInfoKHR where
  getNext s = next (s :: MemoryGetWin32HandleInfoKHR)
#endif
instance HasNext ImportMemoryFdInfoKHR where
  getNext s = next (s :: ImportMemoryFdInfoKHR)
instance HasNext MemoryFdPropertiesKHR where
  getNext s = next (s :: MemoryFdPropertiesKHR)
instance HasNext MemoryGetFdInfoKHR where
  getNext s = next (s :: MemoryGetFdInfoKHR)

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext Win32KeyedMutexAcquireReleaseInfoKHR where
  getNext s = next (s :: Win32KeyedMutexAcquireReleaseInfoKHR)
#endif
instance HasNext PhysicalDeviceExternalSemaphoreInfo where
  getNext s = next (s :: PhysicalDeviceExternalSemaphoreInfo)
instance HasNext ExternalSemaphoreProperties where
  getNext s = next (s :: ExternalSemaphoreProperties)
instance HasNext ExportSemaphoreCreateInfo where
  getNext s = next (s :: ExportSemaphoreCreateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ImportSemaphoreWin32HandleInfoKHR where
  getNext s = next (s :: ImportSemaphoreWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ExportSemaphoreWin32HandleInfoKHR where
  getNext s = next (s :: ExportSemaphoreWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext D3D12FenceSubmitInfoKHR where
  getNext s = next (s :: D3D12FenceSubmitInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext SemaphoreGetWin32HandleInfoKHR where
  getNext s = next (s :: SemaphoreGetWin32HandleInfoKHR)
#endif
instance HasNext ImportSemaphoreFdInfoKHR where
  getNext s = next (s :: ImportSemaphoreFdInfoKHR)
instance HasNext SemaphoreGetFdInfoKHR where
  getNext s = next (s :: SemaphoreGetFdInfoKHR)
instance HasNext PhysicalDeviceExternalFenceInfo where
  getNext s = next (s :: PhysicalDeviceExternalFenceInfo)
instance HasNext ExternalFenceProperties where
  getNext s = next (s :: ExternalFenceProperties)
instance HasNext ExportFenceCreateInfo where
  getNext s = next (s :: ExportFenceCreateInfo)

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ImportFenceWin32HandleInfoKHR where
  getNext s = next (s :: ImportFenceWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext ExportFenceWin32HandleInfoKHR where
  getNext s = next (s :: ExportFenceWin32HandleInfoKHR)
#endif

#if VK_USE_PLATFORM_WIN32_KHR
instance HasNext FenceGetWin32HandleInfoKHR where
  getNext s = next (s :: FenceGetWin32HandleInfoKHR)
#endif
instance HasNext ImportFenceFdInfoKHR where
  getNext s = next (s :: ImportFenceFdInfoKHR)
instance HasNext FenceGetFdInfoKHR where
  getNext s = next (s :: FenceGetFdInfoKHR)
instance HasNext PhysicalDeviceMultiviewFeatures where
  getNext s = next (s :: PhysicalDeviceMultiviewFeatures)
instance HasNext PhysicalDeviceMultiviewProperties where
  getNext s = next (s :: PhysicalDeviceMultiviewProperties)
instance HasNext RenderPassMultiviewCreateInfo where
  getNext s = next (s :: RenderPassMultiviewCreateInfo)
instance HasNext SurfaceCapabilities2EXT where
  getNext s = next (s :: SurfaceCapabilities2EXT)
instance HasNext DisplayPowerInfoEXT where
  getNext s = next (s :: DisplayPowerInfoEXT)
instance HasNext DeviceEventInfoEXT where
  getNext s = next (s :: DeviceEventInfoEXT)
instance HasNext DisplayEventInfoEXT where
  getNext s = next (s :: DisplayEventInfoEXT)
instance HasNext SwapchainCounterCreateInfoEXT where
  getNext s = next (s :: SwapchainCounterCreateInfoEXT)
instance HasNext PhysicalDeviceGroupProperties where
  getNext s = next (s :: PhysicalDeviceGroupProperties)
instance HasNext MemoryAllocateFlagsInfo where
  getNext s = next (s :: MemoryAllocateFlagsInfo)
instance HasNext BindBufferMemoryInfo where
  getNext s = next (s :: BindBufferMemoryInfo)
instance HasNext BindBufferMemoryDeviceGroupInfo where
  getNext s = next (s :: BindBufferMemoryDeviceGroupInfo)
instance HasNext BindImageMemoryInfo where
  getNext s = next (s :: BindImageMemoryInfo)
instance HasNext BindImageMemoryDeviceGroupInfo where
  getNext s = next (s :: BindImageMemoryDeviceGroupInfo)
instance HasNext DeviceGroupRenderPassBeginInfo where
  getNext s = next (s :: DeviceGroupRenderPassBeginInfo)
instance HasNext DeviceGroupCommandBufferBeginInfo where
  getNext s = next (s :: DeviceGroupCommandBufferBeginInfo)
instance HasNext DeviceGroupSubmitInfo where
  getNext s = next (s :: DeviceGroupSubmitInfo)
instance HasNext DeviceGroupBindSparseInfo where
  getNext s = next (s :: DeviceGroupBindSparseInfo)
instance HasNext DeviceGroupPresentCapabilitiesKHR where
  getNext s = next (s :: DeviceGroupPresentCapabilitiesKHR)
instance HasNext ImageSwapchainCreateInfoKHR where
  getNext s = next (s :: ImageSwapchainCreateInfoKHR)
instance HasNext BindImageMemorySwapchainInfoKHR where
  getNext s = next (s :: BindImageMemorySwapchainInfoKHR)
instance HasNext AcquireNextImageInfoKHR where
  getNext s = next (s :: AcquireNextImageInfoKHR)
instance HasNext DeviceGroupPresentInfoKHR where
  getNext s = next (s :: DeviceGroupPresentInfoKHR)
instance HasNext DeviceGroupDeviceCreateInfo where
  getNext s = next (s :: DeviceGroupDeviceCreateInfo)
instance HasNext DeviceGroupSwapchainCreateInfoKHR where
  getNext s = next (s :: DeviceGroupSwapchainCreateInfoKHR)

instance HasNext DescriptorUpdateTemplateCreateInfo where
  getNext s = next (s :: DescriptorUpdateTemplateCreateInfo)

instance HasNext HdrMetadataEXT where
  getNext s = next (s :: HdrMetadataEXT)
instance HasNext DisplayNativeHdrSurfaceCapabilitiesAMD where
  getNext s = next (s :: DisplayNativeHdrSurfaceCapabilitiesAMD)
instance HasNext SwapchainDisplayNativeHdrCreateInfoAMD where
  getNext s = next (s :: SwapchainDisplayNativeHdrCreateInfoAMD)


instance HasNext PresentTimesInfoGOOGLE where
  getNext s = next (s :: PresentTimesInfoGOOGLE)


#if VK_USE_PLATFORM_IOS_MVK
instance HasNext IOSSurfaceCreateInfoMVK where
  getNext s = next (s :: IOSSurfaceCreateInfoMVK)
#endif

#if VK_USE_PLATFORM_MACOS_MVK
instance HasNext MacOSSurfaceCreateInfoMVK where
  getNext s = next (s :: MacOSSurfaceCreateInfoMVK)
#endif

#if VK_USE_PLATFORM_METAL_EXT
instance HasNext MetalSurfaceCreateInfoEXT where
  getNext s = next (s :: MetalSurfaceCreateInfoEXT)
#endif

instance HasNext PipelineViewportWScalingStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportWScalingStateCreateInfoNV)

instance HasNext PipelineViewportSwizzleStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportSwizzleStateCreateInfoNV)
instance HasNext PhysicalDeviceDiscardRectanglePropertiesEXT where
  getNext s = next (s :: PhysicalDeviceDiscardRectanglePropertiesEXT)
instance HasNext PipelineDiscardRectangleStateCreateInfoEXT where
  getNext s = next (s :: PipelineDiscardRectangleStateCreateInfoEXT)
instance HasNext PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX where
  getNext s = next (s :: PhysicalDeviceMultiviewPerViewAttributesPropertiesNVX)

instance HasNext RenderPassInputAttachmentAspectCreateInfo where
  getNext s = next (s :: RenderPassInputAttachmentAspectCreateInfo)
instance HasNext PhysicalDeviceSurfaceInfo2KHR where
  getNext s = next (s :: PhysicalDeviceSurfaceInfo2KHR)
instance HasNext SurfaceCapabilities2KHR where
  getNext s = next (s :: SurfaceCapabilities2KHR)
instance HasNext SurfaceFormat2KHR where
  getNext s = next (s :: SurfaceFormat2KHR)
instance HasNext DisplayProperties2KHR where
  getNext s = next (s :: DisplayProperties2KHR)
instance HasNext DisplayPlaneProperties2KHR where
  getNext s = next (s :: DisplayPlaneProperties2KHR)
instance HasNext DisplayModeProperties2KHR where
  getNext s = next (s :: DisplayModeProperties2KHR)
instance HasNext DisplayPlaneInfo2KHR where
  getNext s = next (s :: DisplayPlaneInfo2KHR)
instance HasNext DisplayPlaneCapabilities2KHR where
  getNext s = next (s :: DisplayPlaneCapabilities2KHR)
instance HasNext SharedPresentSurfaceCapabilitiesKHR where
  getNext s = next (s :: SharedPresentSurfaceCapabilitiesKHR)
instance HasNext PhysicalDevice16BitStorageFeatures where
  getNext s = next (s :: PhysicalDevice16BitStorageFeatures)
instance HasNext PhysicalDeviceSubgroupProperties where
  getNext s = next (s :: PhysicalDeviceSubgroupProperties)
instance HasNext BufferMemoryRequirementsInfo2 where
  getNext s = next (s :: BufferMemoryRequirementsInfo2)
instance HasNext ImageMemoryRequirementsInfo2 where
  getNext s = next (s :: ImageMemoryRequirementsInfo2)
instance HasNext ImageSparseMemoryRequirementsInfo2 where
  getNext s = next (s :: ImageSparseMemoryRequirementsInfo2)
instance HasNext MemoryRequirements2 where
  getNext s = next (s :: MemoryRequirements2)
instance HasNext SparseImageMemoryRequirements2 where
  getNext s = next (s :: SparseImageMemoryRequirements2)
instance HasNext PhysicalDevicePointClippingProperties where
  getNext s = next (s :: PhysicalDevicePointClippingProperties)
instance HasNext MemoryDedicatedRequirements where
  getNext s = next (s :: MemoryDedicatedRequirements)
instance HasNext MemoryDedicatedAllocateInfo where
  getNext s = next (s :: MemoryDedicatedAllocateInfo)
instance HasNext ImageViewUsageCreateInfo where
  getNext s = next (s :: ImageViewUsageCreateInfo)
instance HasNext PipelineTessellationDomainOriginStateCreateInfo where
  getNext s = next (s :: PipelineTessellationDomainOriginStateCreateInfo)
instance HasNext SamplerYcbcrConversionInfo where
  getNext s = next (s :: SamplerYcbcrConversionInfo)
instance HasNext SamplerYcbcrConversionCreateInfo where
  getNext s = next (s :: SamplerYcbcrConversionCreateInfo)
instance HasNext BindImagePlaneMemoryInfo where
  getNext s = next (s :: BindImagePlaneMemoryInfo)
instance HasNext ImagePlaneMemoryRequirementsInfo where
  getNext s = next (s :: ImagePlaneMemoryRequirementsInfo)
instance HasNext PhysicalDeviceSamplerYcbcrConversionFeatures where
  getNext s = next (s :: PhysicalDeviceSamplerYcbcrConversionFeatures)
instance HasNext SamplerYcbcrConversionImageFormatProperties where
  getNext s = next (s :: SamplerYcbcrConversionImageFormatProperties)
instance HasNext TextureLODGatherFormatPropertiesAMD where
  getNext s = next (s :: TextureLODGatherFormatPropertiesAMD)
instance HasNext ConditionalRenderingBeginInfoEXT where
  getNext s = next (s :: ConditionalRenderingBeginInfoEXT)
instance HasNext ProtectedSubmitInfo where
  getNext s = next (s :: ProtectedSubmitInfo)
instance HasNext PhysicalDeviceProtectedMemoryFeatures where
  getNext s = next (s :: PhysicalDeviceProtectedMemoryFeatures)
instance HasNext PhysicalDeviceProtectedMemoryProperties where
  getNext s = next (s :: PhysicalDeviceProtectedMemoryProperties)
instance HasNext DeviceQueueInfo2 where
  getNext s = next (s :: DeviceQueueInfo2)
instance HasNext PipelineCoverageToColorStateCreateInfoNV where
  getNext s = next (s :: PipelineCoverageToColorStateCreateInfoNV)
instance HasNext PhysicalDeviceSamplerFilterMinmaxPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceSamplerFilterMinmaxPropertiesEXT)

instance HasNext SampleLocationsInfoEXT where
  getNext s = next (s :: SampleLocationsInfoEXT)


instance HasNext RenderPassSampleLocationsBeginInfoEXT where
  getNext s = next (s :: RenderPassSampleLocationsBeginInfoEXT)
instance HasNext PipelineSampleLocationsStateCreateInfoEXT where
  getNext s = next (s :: PipelineSampleLocationsStateCreateInfoEXT)
instance HasNext PhysicalDeviceSampleLocationsPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceSampleLocationsPropertiesEXT)
instance HasNext MultisamplePropertiesEXT where
  getNext s = next (s :: MultisamplePropertiesEXT)
instance HasNext SamplerReductionModeCreateInfoEXT where
  getNext s = next (s :: SamplerReductionModeCreateInfoEXT)
instance HasNext PhysicalDeviceBlendOperationAdvancedFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceBlendOperationAdvancedFeaturesEXT)
instance HasNext PhysicalDeviceBlendOperationAdvancedPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceBlendOperationAdvancedPropertiesEXT)
instance HasNext PipelineColorBlendAdvancedStateCreateInfoEXT where
  getNext s = next (s :: PipelineColorBlendAdvancedStateCreateInfoEXT)
instance HasNext PhysicalDeviceInlineUniformBlockFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceInlineUniformBlockFeaturesEXT)
instance HasNext PhysicalDeviceInlineUniformBlockPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceInlineUniformBlockPropertiesEXT)
instance HasNext WriteDescriptorSetInlineUniformBlockEXT where
  getNext s = next (s :: WriteDescriptorSetInlineUniformBlockEXT)
instance HasNext DescriptorPoolInlineUniformBlockCreateInfoEXT where
  getNext s = next (s :: DescriptorPoolInlineUniformBlockCreateInfoEXT)
instance HasNext PipelineCoverageModulationStateCreateInfoNV where
  getNext s = next (s :: PipelineCoverageModulationStateCreateInfoNV)
instance HasNext ImageFormatListCreateInfoKHR where
  getNext s = next (s :: ImageFormatListCreateInfoKHR)
instance HasNext ValidationCacheCreateInfoEXT where
  getNext s = next (s :: ValidationCacheCreateInfoEXT)
instance HasNext ShaderModuleValidationCacheCreateInfoEXT where
  getNext s = next (s :: ShaderModuleValidationCacheCreateInfoEXT)
instance HasNext PhysicalDeviceMaintenance3Properties where
  getNext s = next (s :: PhysicalDeviceMaintenance3Properties)
instance HasNext DescriptorSetLayoutSupport where
  getNext s = next (s :: DescriptorSetLayoutSupport)
instance HasNext PhysicalDeviceShaderDrawParametersFeatures where
  getNext s = next (s :: PhysicalDeviceShaderDrawParametersFeatures)
instance HasNext PhysicalDeviceFloat16Int8FeaturesKHR where
  getNext s = next (s :: PhysicalDeviceFloat16Int8FeaturesKHR)
instance HasNext PhysicalDeviceFloatControlsPropertiesKHR where
  getNext s = next (s :: PhysicalDeviceFloatControlsPropertiesKHR)
instance HasNext PhysicalDeviceHostQueryResetFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceHostQueryResetFeaturesEXT)


instance HasNext DeviceQueueGlobalPriorityCreateInfoEXT where
  getNext s = next (s :: DeviceQueueGlobalPriorityCreateInfoEXT)
instance HasNext DebugUtilsObjectNameInfoEXT where
  getNext s = next (s :: DebugUtilsObjectNameInfoEXT)
instance HasNext DebugUtilsObjectTagInfoEXT where
  getNext s = next (s :: DebugUtilsObjectTagInfoEXT)
instance HasNext DebugUtilsLabelEXT where
  getNext s = next (s :: DebugUtilsLabelEXT)
instance HasNext DebugUtilsMessengerCreateInfoEXT where
  getNext s = next (s :: DebugUtilsMessengerCreateInfoEXT)
instance HasNext DebugUtilsMessengerCallbackDataEXT where
  getNext s = next (s :: DebugUtilsMessengerCallbackDataEXT)
instance HasNext ImportMemoryHostPointerInfoEXT where
  getNext s = next (s :: ImportMemoryHostPointerInfoEXT)
instance HasNext MemoryHostPointerPropertiesEXT where
  getNext s = next (s :: MemoryHostPointerPropertiesEXT)
instance HasNext PhysicalDeviceExternalMemoryHostPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceExternalMemoryHostPropertiesEXT)
instance HasNext PhysicalDeviceConservativeRasterizationPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceConservativeRasterizationPropertiesEXT)
instance HasNext CalibratedTimestampInfoEXT where
  getNext s = next (s :: CalibratedTimestampInfoEXT)
instance HasNext PhysicalDeviceShaderCorePropertiesAMD where
  getNext s = next (s :: PhysicalDeviceShaderCorePropertiesAMD)
instance HasNext PipelineRasterizationConservativeStateCreateInfoEXT where
  getNext s = next (s :: PipelineRasterizationConservativeStateCreateInfoEXT)
instance HasNext PhysicalDeviceDescriptorIndexingFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceDescriptorIndexingFeaturesEXT)
instance HasNext PhysicalDeviceDescriptorIndexingPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceDescriptorIndexingPropertiesEXT)
instance HasNext DescriptorSetLayoutBindingFlagsCreateInfoEXT where
  getNext s = next (s :: DescriptorSetLayoutBindingFlagsCreateInfoEXT)
instance HasNext DescriptorSetVariableDescriptorCountAllocateInfoEXT where
  getNext s = next (s :: DescriptorSetVariableDescriptorCountAllocateInfoEXT)
instance HasNext DescriptorSetVariableDescriptorCountLayoutSupportEXT where
  getNext s = next (s :: DescriptorSetVariableDescriptorCountLayoutSupportEXT)
instance HasNext AttachmentDescription2KHR where
  getNext s = next (s :: AttachmentDescription2KHR)
instance HasNext AttachmentReference2KHR where
  getNext s = next (s :: AttachmentReference2KHR)
instance HasNext SubpassDescription2KHR where
  getNext s = next (s :: SubpassDescription2KHR)
instance HasNext SubpassDependency2KHR where
  getNext s = next (s :: SubpassDependency2KHR)
instance HasNext RenderPassCreateInfo2KHR where
  getNext s = next (s :: RenderPassCreateInfo2KHR)
instance HasNext SubpassBeginInfoKHR where
  getNext s = next (s :: SubpassBeginInfoKHR)
instance HasNext SubpassEndInfoKHR where
  getNext s = next (s :: SubpassEndInfoKHR)

instance HasNext PipelineVertexInputDivisorStateCreateInfoEXT where
  getNext s = next (s :: PipelineVertexInputDivisorStateCreateInfoEXT)
instance HasNext PhysicalDeviceVertexAttributeDivisorPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceVertexAttributeDivisorPropertiesEXT)
instance HasNext PhysicalDevicePCIBusInfoPropertiesEXT where
  getNext s = next (s :: PhysicalDevicePCIBusInfoPropertiesEXT)

#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext ImportAndroidHardwareBufferInfoANDROID where
  getNext s = next (s :: ImportAndroidHardwareBufferInfoANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext AndroidHardwareBufferUsageANDROID where
  getNext s = next (s :: AndroidHardwareBufferUsageANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext AndroidHardwareBufferPropertiesANDROID where
  getNext s = next (s :: AndroidHardwareBufferPropertiesANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext MemoryGetAndroidHardwareBufferInfoANDROID where
  getNext s = next (s :: MemoryGetAndroidHardwareBufferInfoANDROID)
#endif

#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext AndroidHardwareBufferFormatPropertiesANDROID where
  getNext s = next (s :: AndroidHardwareBufferFormatPropertiesANDROID)
#endif
instance HasNext CommandBufferInheritanceConditionalRenderingInfoEXT where
  getNext s = next (s :: CommandBufferInheritanceConditionalRenderingInfoEXT)

#if VK_USE_PLATFORM_ANDROID_KHR
instance HasNext ExternalFormatANDROID where
  getNext s = next (s :: ExternalFormatANDROID)
#endif
instance HasNext PhysicalDevice8BitStorageFeaturesKHR where
  getNext s = next (s :: PhysicalDevice8BitStorageFeaturesKHR)
instance HasNext PhysicalDeviceConditionalRenderingFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceConditionalRenderingFeaturesEXT)
instance HasNext PhysicalDeviceVulkanMemoryModelFeaturesKHR where
  getNext s = next (s :: PhysicalDeviceVulkanMemoryModelFeaturesKHR)
instance HasNext PhysicalDeviceShaderAtomicInt64FeaturesKHR where
  getNext s = next (s :: PhysicalDeviceShaderAtomicInt64FeaturesKHR)
instance HasNext PhysicalDeviceVertexAttributeDivisorFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceVertexAttributeDivisorFeaturesEXT)
instance HasNext QueueFamilyCheckpointPropertiesNV where
  getNext s = next (s :: QueueFamilyCheckpointPropertiesNV)
instance HasNext CheckpointDataNV where
  getNext s = next (s :: CheckpointDataNV)
instance HasNext PhysicalDeviceDepthStencilResolvePropertiesKHR where
  getNext s = next (s :: PhysicalDeviceDepthStencilResolvePropertiesKHR)
instance HasNext SubpassDescriptionDepthStencilResolveKHR where
  getNext s = next (s :: SubpassDescriptionDepthStencilResolveKHR)
instance HasNext ImageViewASTCDecodeModeEXT where
  getNext s = next (s :: ImageViewASTCDecodeModeEXT)
instance HasNext PhysicalDeviceASTCDecodeFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceASTCDecodeFeaturesEXT)
instance HasNext PhysicalDeviceTransformFeedbackFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceTransformFeedbackFeaturesEXT)
instance HasNext PhysicalDeviceTransformFeedbackPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceTransformFeedbackPropertiesEXT)
instance HasNext PipelineRasterizationStateStreamCreateInfoEXT where
  getNext s = next (s :: PipelineRasterizationStateStreamCreateInfoEXT)
instance HasNext PhysicalDeviceRepresentativeFragmentTestFeaturesNV where
  getNext s = next (s :: PhysicalDeviceRepresentativeFragmentTestFeaturesNV)
instance HasNext PipelineRepresentativeFragmentTestStateCreateInfoNV where
  getNext s = next (s :: PipelineRepresentativeFragmentTestStateCreateInfoNV)
instance HasNext PhysicalDeviceExclusiveScissorFeaturesNV where
  getNext s = next (s :: PhysicalDeviceExclusiveScissorFeaturesNV)
instance HasNext PipelineViewportExclusiveScissorStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportExclusiveScissorStateCreateInfoNV)
instance HasNext PhysicalDeviceCornerSampledImageFeaturesNV where
  getNext s = next (s :: PhysicalDeviceCornerSampledImageFeaturesNV)
instance HasNext PhysicalDeviceComputeShaderDerivativesFeaturesNV where
  getNext s = next (s :: PhysicalDeviceComputeShaderDerivativesFeaturesNV)
instance HasNext PhysicalDeviceFragmentShaderBarycentricFeaturesNV where
  getNext s = next (s :: PhysicalDeviceFragmentShaderBarycentricFeaturesNV)
instance HasNext PhysicalDeviceShaderImageFootprintFeaturesNV where
  getNext s = next (s :: PhysicalDeviceShaderImageFootprintFeaturesNV)
instance HasNext PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV where
  getNext s = next (s :: PhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV)

instance HasNext PipelineViewportShadingRateImageStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportShadingRateImageStateCreateInfoNV)
instance HasNext PhysicalDeviceShadingRateImageFeaturesNV where
  getNext s = next (s :: PhysicalDeviceShadingRateImageFeaturesNV)
instance HasNext PhysicalDeviceShadingRateImagePropertiesNV where
  getNext s = next (s :: PhysicalDeviceShadingRateImagePropertiesNV)


instance HasNext PipelineViewportCoarseSampleOrderStateCreateInfoNV where
  getNext s = next (s :: PipelineViewportCoarseSampleOrderStateCreateInfoNV)
instance HasNext PhysicalDeviceMeshShaderFeaturesNV where
  getNext s = next (s :: PhysicalDeviceMeshShaderFeaturesNV)
instance HasNext PhysicalDeviceMeshShaderPropertiesNV where
  getNext s = next (s :: PhysicalDeviceMeshShaderPropertiesNV)

instance HasNext RayTracingShaderGroupCreateInfoNV where
  getNext s = next (s :: RayTracingShaderGroupCreateInfoNV)
instance HasNext RayTracingPipelineCreateInfoNV where
  getNext s = next (s :: RayTracingPipelineCreateInfoNV)
instance HasNext GeometryTrianglesNV where
  getNext s = next (s :: GeometryTrianglesNV)
instance HasNext GeometryAABBNV where
  getNext s = next (s :: GeometryAABBNV)

instance HasNext GeometryNV where
  getNext s = next (s :: GeometryNV)
instance HasNext AccelerationStructureInfoNV where
  getNext s = next (s :: AccelerationStructureInfoNV)
instance HasNext AccelerationStructureCreateInfoNV where
  getNext s = next (s :: AccelerationStructureCreateInfoNV)
instance HasNext BindAccelerationStructureMemoryInfoNV where
  getNext s = next (s :: BindAccelerationStructureMemoryInfoNV)
instance HasNext WriteDescriptorSetAccelerationStructureNV where
  getNext s = next (s :: WriteDescriptorSetAccelerationStructureNV)
instance HasNext AccelerationStructureMemoryRequirementsInfoNV where
  getNext s = next (s :: AccelerationStructureMemoryRequirementsInfoNV)
instance HasNext PhysicalDeviceRayTracingPropertiesNV where
  getNext s = next (s :: PhysicalDeviceRayTracingPropertiesNV)
instance HasNext DrmFormatModifierPropertiesListEXT where
  getNext s = next (s :: DrmFormatModifierPropertiesListEXT)

instance HasNext PhysicalDeviceImageDrmFormatModifierInfoEXT where
  getNext s = next (s :: PhysicalDeviceImageDrmFormatModifierInfoEXT)
instance HasNext ImageDrmFormatModifierListCreateInfoEXT where
  getNext s = next (s :: ImageDrmFormatModifierListCreateInfoEXT)
instance HasNext ImageDrmFormatModifierExplicitCreateInfoEXT where
  getNext s = next (s :: ImageDrmFormatModifierExplicitCreateInfoEXT)
instance HasNext ImageDrmFormatModifierPropertiesEXT where
  getNext s = next (s :: ImageDrmFormatModifierPropertiesEXT)
instance HasNext ImageStencilUsageCreateInfoEXT where
  getNext s = next (s :: ImageStencilUsageCreateInfoEXT)
instance HasNext DeviceMemoryOverallocationCreateInfoAMD where
  getNext s = next (s :: DeviceMemoryOverallocationCreateInfoAMD)
instance HasNext PhysicalDeviceFragmentDensityMapFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceFragmentDensityMapFeaturesEXT)
instance HasNext PhysicalDeviceFragmentDensityMapPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceFragmentDensityMapPropertiesEXT)
instance HasNext RenderPassFragmentDensityMapCreateInfoEXT where
  getNext s = next (s :: RenderPassFragmentDensityMapCreateInfoEXT)
instance HasNext PhysicalDeviceScalarBlockLayoutFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceScalarBlockLayoutFeaturesEXT)
instance HasNext SurfaceProtectedCapabilitiesKHR where
  getNext s = next (s :: SurfaceProtectedCapabilitiesKHR)
instance HasNext PhysicalDeviceDepthClipEnableFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceDepthClipEnableFeaturesEXT)
instance HasNext PipelineRasterizationDepthClipStateCreateInfoEXT where
  getNext s = next (s :: PipelineRasterizationDepthClipStateCreateInfoEXT)
instance HasNext PhysicalDeviceMemoryBudgetPropertiesEXT where
  getNext s = next (s :: PhysicalDeviceMemoryBudgetPropertiesEXT)
instance HasNext PhysicalDeviceMemoryPriorityFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceMemoryPriorityFeaturesEXT)
instance HasNext MemoryPriorityAllocateInfoEXT where
  getNext s = next (s :: MemoryPriorityAllocateInfoEXT)
instance HasNext PhysicalDeviceBufferDeviceAddressFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceBufferDeviceAddressFeaturesEXT)
instance HasNext BufferDeviceAddressInfoEXT where
  getNext s = next (s :: BufferDeviceAddressInfoEXT)
instance HasNext BufferDeviceAddressCreateInfoEXT where
  getNext s = next (s :: BufferDeviceAddressCreateInfoEXT)
instance HasNext PhysicalDeviceImageViewImageFormatInfoEXT where
  getNext s = next (s :: PhysicalDeviceImageViewImageFormatInfoEXT)
instance HasNext FilterCubicImageViewImageFormatPropertiesEXT where
  getNext s = next (s :: FilterCubicImageViewImageFormatPropertiesEXT)
instance HasNext PhysicalDeviceCooperativeMatrixFeaturesNV where
  getNext s = next (s :: PhysicalDeviceCooperativeMatrixFeaturesNV)
instance HasNext PhysicalDeviceCooperativeMatrixPropertiesNV where
  getNext s = next (s :: PhysicalDeviceCooperativeMatrixPropertiesNV)
instance HasNext CooperativeMatrixPropertiesNV where
  getNext s = next (s :: CooperativeMatrixPropertiesNV)
instance HasNext PhysicalDeviceYcbcrImageArraysFeaturesEXT where
  getNext s = next (s :: PhysicalDeviceYcbcrImageArraysFeaturesEXT)
instance HasNext ImageViewHandleInfoNVX where
  getNext s = next (s :: ImageViewHandleInfoNVX)

#if VK_USE_PLATFORM_GGP
instance HasNext PresentFrameTokenGGP where
  getNext s = next (s :: PresentFrameTokenGGP)
#endif

instance HasNext PipelineCreationFeedbackCreateInfoEXT where
  getNext s = next (s :: PipelineCreationFeedbackCreateInfoEXT)
instance HasNext SurfaceFullScreenExclusiveInfoEXT where
  getNext s = next (s :: SurfaceFullScreenExclusiveInfoEXT)
instance HasNext SurfaceFullScreenExclusiveWin32InfoEXT where
  getNext s = next (s :: SurfaceFullScreenExclusiveWin32InfoEXT)
instance HasNext SurfaceCapabilitiesFullScreenExclusiveEXT where
  getNext s = next (s :: SurfaceCapabilitiesFullScreenExclusiveEXT)
instance HasNext HeadlessSurfaceCreateInfoEXT where
  getNext s = next (s :: HeadlessSurfaceCreateInfoEXT)



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
    VK_STRUCTURE_TYPE_APPLICATION_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkApplicationInfo))
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceQueueCreateInfo))
    VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceCreateInfo))
    VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkInstanceCreateInfo))
    VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryAllocateInfo))
    VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMappedMemoryRange))
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWriteDescriptorSet))
    VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCopyDescriptorSet))
    VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBufferCreateInfo))
    VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBufferViewCreateInfo))
    VK_STRUCTURE_TYPE_MEMORY_BARRIER -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryBarrier))
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBufferMemoryBarrier))
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageMemoryBarrier))
    VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageCreateInfo))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageViewCreateInfo))
    VK_STRUCTURE_TYPE_BIND_SPARSE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindSparseInfo))
    VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkShaderModuleCreateInfo))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutCreateInfo))
    VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorPoolCreateInfo))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorSetAllocateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineShaderStageCreateInfo))
    VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkComputePipelineCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineVertexInputStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineInputAssemblyStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineTessellationStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineViewportStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineRasterizationStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineMultisampleStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineColorBlendStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineDynamicStateCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineDepthStencilStateCreateInfo))
    VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkGraphicsPipelineCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineCacheCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineLayoutCreateInfo))
    VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSamplerCreateInfo))
    VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCommandPoolCreateInfo))
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCommandBufferAllocateInfo))
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCommandBufferInheritanceInfo))
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCommandBufferBeginInfo))
    -- We are not able to marshal this type back into Haskell as we don't know which union component to use
    VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing unions: " ++ show VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO) Nothing Nothing)
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRenderPassCreateInfo))
    VK_STRUCTURE_TYPE_EVENT_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkEventCreateInfo))
    VK_STRUCTURE_TYPE_FENCE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkFenceCreateInfo))
    VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSemaphoreCreateInfo))
    VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkQueryPoolCreateInfo))
    VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkFramebufferCreateInfo))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_SUBMIT_INFO -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_SUBMIT_INFO) Nothing Nothing)
    VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayModeCreateInfoKHR))
    VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplaySurfaceCreateInfoKHR))
    VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayPresentInfoKHR))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAndroidSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_VI_NN)
    VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkViSurfaceCreateInfoNN))
#endif
#if defined(VK_USE_PLATFORM_WAYLAND_KHR)
    VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWaylandSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWin32SurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_XLIB_KHR)
    VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkXlibSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_XCB_KHR)
    VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkXcbSurfaceCreateInfoKHR))
#endif
#if defined(VK_USE_PLATFORM_FUCHSIA)
    VK_STRUCTURE_TYPE_IMAGEPIPE_SURFACE_CREATE_INFO_FUCHSIA -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImagePipeSurfaceCreateInfoFUCHSIA))
#endif
#if defined(VK_USE_PLATFORM_GGP)
    VK_STRUCTURE_TYPE_STREAM_DESCRIPTOR_SURFACE_CREATE_INFO_GGP -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkStreamDescriptorSurfaceCreateInfoGGP))
#endif
    VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_PRESENT_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPresentInfoKHR))
    VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugReportCallbackCreateInfoEXT))
    VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkValidationFlagsEXT))
    VK_STRUCTURE_TYPE_VALIDATION_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkValidationFeaturesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineRasterizationStateRasterizationOrderAMD))
    VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugMarkerObjectNameInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugMarkerObjectTagInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugMarkerMarkerInfoEXT))
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDedicatedAllocationImageCreateInfoNV))
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDedicatedAllocationBufferCreateInfoNV))
    VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDedicatedAllocationMemoryAllocateInfoNV))
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalMemoryImageCreateInfoNV))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportMemoryAllocateInfoNV))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportMemoryWin32HandleInfoNV))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportMemoryWin32HandleInfoNV))
    VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWin32KeyedMutexAcquireReleaseInfoNV))
#endif
    VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGeneratedCommandsFeaturesNVX))
    VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGeneratedCommandsLimitsNVX))
    VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkIndirectCommandsLayoutCreateInfoNVX))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX) Nothing Nothing)
    VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCmdReserveSpaceForCommandsInfoNVX))
    VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkObjectTableCreateInfoNVX))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceFeatures2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceProperties2))
    VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkFormatProperties2))
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageFormatProperties2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceImageFormatInfo2))
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkQueueFamilyProperties2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMemoryProperties2))
    VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSparseImageFormatProperties2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceSparseImageFormatInfo2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDevicePushDescriptorPropertiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DRIVER_PROPERTIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDriverPropertiesKHR))
    VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPresentRegionsKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTERS_FEATURES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceVariablePointersFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalImageFormatInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalImageFormatProperties))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalBufferInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalBufferProperties))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceIDProperties))
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalMemoryImageCreateInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalMemoryBufferCreateInfo))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportMemoryAllocateInfo))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportMemoryWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportMemoryWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryWin32HandlePropertiesKHR))
    VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryGetWin32HandleInfoKHR))
#endif
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportMemoryFdInfoKHR))
    VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryFdPropertiesKHR))
    VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryGetFdInfoKHR))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWin32KeyedMutexAcquireReleaseInfoKHR))
#endif
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalSemaphoreInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalSemaphoreProperties))
    VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportSemaphoreCreateInfo))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportSemaphoreWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportSemaphoreWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkD3D12FenceSubmitInfoKHR))
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSemaphoreGetWin32HandleInfoKHR))
#endif
    VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportSemaphoreFdInfoKHR))
    VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSemaphoreGetFdInfoKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalFenceInfo))
    VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalFenceProperties))
    VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportFenceCreateInfo))
#if defined(VK_USE_PLATFORM_WIN32_KHR)
    VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportFenceWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExportFenceWin32HandleInfoKHR))
    VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkFenceGetWin32HandleInfoKHR))
#endif
    VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportFenceFdInfoKHR))
    VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkFenceGetFdInfoKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMultiviewFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMultiviewProperties))
    VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRenderPassMultiviewCreateInfo))
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceCapabilities2EXT))
    VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayPowerInfoEXT))
    VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceEventInfoEXT))
    VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayEventInfoEXT))
    VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSwapchainCounterCreateInfoEXT))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES) Nothing Nothing)
    VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryAllocateFlagsInfo))
    VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindBufferMemoryInfo))
    VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindBufferMemoryDeviceGroupInfo))
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindImageMemoryInfo))
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindImageMemoryDeviceGroupInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupRenderPassBeginInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupCommandBufferBeginInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupSubmitInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupBindSparseInfo))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupPresentCapabilitiesKHR))
    VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindImageMemorySwapchainInfoKHR))
    VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAcquireNextImageInfoKHR))
    VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupPresentInfoKHR))
    -- We are not able to marshal this type back into Haskell as we don't have the command table for it
    VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO -> throwIO (IOError Nothing InvalidArgument "" ("Unable to marshal Vulkan structure containing dispatchable handles: " ++ show VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO) Nothing Nothing)
    VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceGroupSwapchainCreateInfoKHR))
    VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorUpdateTemplateCreateInfo))
    VK_STRUCTURE_TYPE_HDR_METADATA_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkHdrMetadataEXT))
    VK_STRUCTURE_TYPE_DISPLAY_NATIVE_HDR_SURFACE_CAPABILITIES_AMD -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayNativeHdrSurfaceCapabilitiesAMD))
    VK_STRUCTURE_TYPE_SWAPCHAIN_DISPLAY_NATIVE_HDR_CREATE_INFO_AMD -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSwapchainDisplayNativeHdrCreateInfoAMD))
    VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPresentTimesInfoGOOGLE))
#if defined(VK_USE_PLATFORM_IOS_MVK)
    VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkIOSSurfaceCreateInfoMVK))
#endif
#if defined(VK_USE_PLATFORM_MACOS_MVK)
    VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMacOSSurfaceCreateInfoMVK))
#endif
#if defined(VK_USE_PLATFORM_METAL_EXT)
    VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMetalSurfaceCreateInfoEXT))
#endif
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineViewportWScalingStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineViewportSwizzleStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDiscardRectanglePropertiesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineDiscardRectangleStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX))
    VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRenderPassInputAttachmentAspectCreateInfo))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceSurfaceInfo2KHR))
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceCapabilities2KHR))
    VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceFormat2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PROPERTIES_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayProperties2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_PROPERTIES_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayPlaneProperties2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_MODE_PROPERTIES_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayModeProperties2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_INFO_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayPlaneInfo2KHR))
    VK_STRUCTURE_TYPE_DISPLAY_PLANE_CAPABILITIES_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDisplayPlaneCapabilities2KHR))
    VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSharedPresentSurfaceCapabilitiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDevice16BitStorageFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceSubgroupProperties))
    VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBufferMemoryRequirementsInfo2))
    VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageMemoryRequirementsInfo2))
    VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageSparseMemoryRequirementsInfo2))
    VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryRequirements2))
    VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSparseImageMemoryRequirements2))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDevicePointClippingProperties))
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryDedicatedRequirements))
    VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryDedicatedAllocateInfo))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageViewUsageCreateInfo))
    VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineTessellationDomainOriginStateCreateInfo))
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSamplerYcbcrConversionInfo))
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSamplerYcbcrConversionCreateInfo))
    VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindImagePlaneMemoryInfo))
    VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImagePlaneMemoryRequirementsInfo))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceSamplerYcbcrConversionFeatures))
    VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSamplerYcbcrConversionImageFormatProperties))
    VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkTextureLODGatherFormatPropertiesAMD))
    VK_STRUCTURE_TYPE_CONDITIONAL_RENDERING_BEGIN_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkConditionalRenderingBeginInfoEXT))
    VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkProtectedSubmitInfo))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceProtectedMemoryFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceProtectedMemoryProperties))
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceQueueInfo2))
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineCoverageToColorStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT))
    VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSampleLocationsInfoEXT))
    VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRenderPassSampleLocationsBeginInfoEXT))
    VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineSampleLocationsStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceSampleLocationsPropertiesEXT))
    VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMultisamplePropertiesEXT))
    VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSamplerReductionModeCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineColorBlendAdvancedStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceInlineUniformBlockFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_INLINE_UNIFORM_BLOCK_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceInlineUniformBlockPropertiesEXT))
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_INLINE_UNIFORM_BLOCK_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWriteDescriptorSetInlineUniformBlockEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_INLINE_UNIFORM_BLOCK_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorPoolInlineUniformBlockCreateInfoEXT))
    VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineCoverageModulationStateCreateInfoNV))
    VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageFormatListCreateInfoKHR))
    VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkValidationCacheCreateInfoEXT))
    VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkShaderModuleValidationCacheCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMaintenance3Properties))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutSupport))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETERS_FEATURES -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderDrawParametersFeatures))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT16_INT8_FEATURES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceFloat16Int8FeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FLOAT_CONTROLS_PROPERTIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceFloatControlsPropertiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_HOST_QUERY_RESET_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceHostQueryResetFeaturesEXT))
    VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceQueueGlobalPriorityCreateInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugUtilsObjectNameInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugUtilsObjectTagInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugUtilsLabelEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugUtilsMessengerCreateInfoEXT))
    VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDebugUtilsMessengerCallbackDataEXT))
    VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportMemoryHostPointerInfoEXT))
    VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryHostPointerPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceExternalMemoryHostPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceConservativeRasterizationPropertiesEXT))
    VK_STRUCTURE_TYPE_CALIBRATED_TIMESTAMP_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCalibratedTimestampInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderCorePropertiesAMD))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineRasterizationConservativeStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDescriptorIndexingFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDescriptorIndexingPropertiesEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorSetLayoutBindingFlagsCreateInfoEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorSetVariableDescriptorCountAllocateInfoEXT))
    VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDescriptorSetVariableDescriptorCountLayoutSupportEXT))
    VK_STRUCTURE_TYPE_ATTACHMENT_DESCRIPTION_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAttachmentDescription2KHR))
    VK_STRUCTURE_TYPE_ATTACHMENT_REFERENCE_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAttachmentReference2KHR))
    VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSubpassDescription2KHR))
    VK_STRUCTURE_TYPE_SUBPASS_DEPENDENCY_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSubpassDependency2KHR))
    VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO_2_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRenderPassCreateInfo2KHR))
    VK_STRUCTURE_TYPE_SUBPASS_BEGIN_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSubpassBeginInfoKHR))
    VK_STRUCTURE_TYPE_SUBPASS_END_INFO_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSubpassEndInfoKHR))
    VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineVertexInputDivisorStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PCI_BUS_INFO_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDevicePCIBusInfoPropertiesEXT))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImportAndroidHardwareBufferInfoANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferUsageANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferPropertiesANDROID))
    VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryGetAndroidHardwareBufferInfoANDROID))
    VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAndroidHardwareBufferFormatPropertiesANDROID))
#endif
    VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_CONDITIONAL_RENDERING_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCommandBufferInheritanceConditionalRenderingInfoEXT))
#if defined(VK_USE_PLATFORM_ANDROID_KHR)
    VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkExternalFormatANDROID))
#endif
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_8BIT_STORAGE_FEATURES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDevice8BitStorageFeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONDITIONAL_RENDERING_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceConditionalRenderingFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_MEMORY_MODEL_FEATURES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceVulkanMemoryModelFeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_ATOMIC_INT64_FEATURES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderAtomicInt64FeaturesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceVertexAttributeDivisorFeaturesEXT))
    VK_STRUCTURE_TYPE_QUEUE_FAMILY_CHECKPOINT_PROPERTIES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkQueueFamilyCheckpointPropertiesNV))
    VK_STRUCTURE_TYPE_CHECKPOINT_DATA_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCheckpointDataNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_STENCIL_RESOLVE_PROPERTIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDepthStencilResolvePropertiesKHR))
    VK_STRUCTURE_TYPE_SUBPASS_DESCRIPTION_DEPTH_STENCIL_RESOLVE_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSubpassDescriptionDepthStencilResolveKHR))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_ASTC_DECODE_MODE_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageViewASTCDecodeModeEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ASTC_DECODE_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceASTCDecodeFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceTransformFeedbackFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_TRANSFORM_FEEDBACK_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceTransformFeedbackPropertiesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_STREAM_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineRasterizationStateStreamCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_REPRESENTATIVE_FRAGMENT_TEST_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceRepresentativeFragmentTestFeaturesNV))
    VK_STRUCTURE_TYPE_PIPELINE_REPRESENTATIVE_FRAGMENT_TEST_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineRepresentativeFragmentTestStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXCLUSIVE_SCISSOR_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceExclusiveScissorFeaturesNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_EXCLUSIVE_SCISSOR_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineViewportExclusiveScissorStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CORNER_SAMPLED_IMAGE_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceCornerSampledImageFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COMPUTE_SHADER_DERIVATIVES_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceComputeShaderDerivativesFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_SHADER_BARYCENTRIC_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceFragmentShaderBarycentricFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_IMAGE_FOOTPRINT_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceShaderImageFootprintFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEDICATED_ALLOCATION_IMAGE_ALIASING_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDedicatedAllocationImageAliasingFeaturesNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SHADING_RATE_IMAGE_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineViewportShadingRateImageStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceShadingRateImageFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADING_RATE_IMAGE_PROPERTIES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceShadingRateImagePropertiesNV))
    VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_COARSE_SAMPLE_ORDER_STATE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineViewportCoarseSampleOrderStateCreateInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMeshShaderFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MESH_SHADER_PROPERTIES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMeshShaderPropertiesNV))
    VK_STRUCTURE_TYPE_RAY_TRACING_SHADER_GROUP_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRayTracingShaderGroupCreateInfoNV))
    VK_STRUCTURE_TYPE_RAY_TRACING_PIPELINE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRayTracingPipelineCreateInfoNV))
    VK_STRUCTURE_TYPE_GEOMETRY_TRIANGLES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkGeometryTrianglesNV))
    VK_STRUCTURE_TYPE_GEOMETRY_AABB_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkGeometryAABBNV))
    VK_STRUCTURE_TYPE_GEOMETRY_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkGeometryNV))
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAccelerationStructureInfoNV))
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_CREATE_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAccelerationStructureCreateInfoNV))
    VK_STRUCTURE_TYPE_BIND_ACCELERATION_STRUCTURE_MEMORY_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBindAccelerationStructureMemoryInfoNV))
    VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET_ACCELERATION_STRUCTURE_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkWriteDescriptorSetAccelerationStructureNV))
    VK_STRUCTURE_TYPE_ACCELERATION_STRUCTURE_MEMORY_REQUIREMENTS_INFO_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkAccelerationStructureMemoryRequirementsInfoNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_RAY_TRACING_PROPERTIES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceRayTracingPropertiesNV))
    VK_STRUCTURE_TYPE_DRM_FORMAT_MODIFIER_PROPERTIES_LIST_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDrmFormatModifierPropertiesListEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_DRM_FORMAT_MODIFIER_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceImageDrmFormatModifierInfoEXT))
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_LIST_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageDrmFormatModifierListCreateInfoEXT))
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_EXPLICIT_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageDrmFormatModifierExplicitCreateInfoEXT))
    VK_STRUCTURE_TYPE_IMAGE_DRM_FORMAT_MODIFIER_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageDrmFormatModifierPropertiesEXT))
    VK_STRUCTURE_TYPE_IMAGE_STENCIL_USAGE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageStencilUsageCreateInfoEXT))
    VK_STRUCTURE_TYPE_DEVICE_MEMORY_OVERALLOCATION_CREATE_INFO_AMD -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkDeviceMemoryOverallocationCreateInfoAMD))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceFragmentDensityMapFeaturesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FRAGMENT_DENSITY_MAP_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceFragmentDensityMapPropertiesEXT))
    VK_STRUCTURE_TYPE_RENDER_PASS_FRAGMENT_DENSITY_MAP_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkRenderPassFragmentDensityMapCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SCALAR_BLOCK_LAYOUT_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceScalarBlockLayoutFeaturesEXT))
    VK_STRUCTURE_TYPE_SURFACE_PROTECTED_CAPABILITIES_KHR -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceProtectedCapabilitiesKHR))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DEPTH_CLIP_ENABLE_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceDepthClipEnableFeaturesEXT))
    VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_DEPTH_CLIP_STATE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineRasterizationDepthClipStateCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMemoryBudgetPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceMemoryPriorityFeaturesEXT))
    VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkMemoryPriorityAllocateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BUFFER_DEVICE_ADDRESS_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceBufferDeviceAddressFeaturesEXT))
    VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBufferDeviceAddressInfoEXT))
    VK_STRUCTURE_TYPE_BUFFER_DEVICE_ADDRESS_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkBufferDeviceAddressCreateInfoEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_VIEW_IMAGE_FORMAT_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceImageViewImageFormatInfoEXT))
    VK_STRUCTURE_TYPE_FILTER_CUBIC_IMAGE_VIEW_IMAGE_FORMAT_PROPERTIES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkFilterCubicImageViewImageFormatPropertiesEXT))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_FEATURES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceCooperativeMatrixFeaturesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_COOPERATIVE_MATRIX_PROPERTIES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceCooperativeMatrixPropertiesNV))
    VK_STRUCTURE_TYPE_COOPERATIVE_MATRIX_PROPERTIES_NV -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkCooperativeMatrixPropertiesNV))
    VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_YCBCR_IMAGE_ARRAYS_FEATURES_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPhysicalDeviceYcbcrImageArraysFeaturesEXT))
    VK_STRUCTURE_TYPE_IMAGE_VIEW_HANDLE_INFO_NVX -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkImageViewHandleInfoNVX))
#if defined(VK_USE_PLATFORM_GGP)
    VK_STRUCTURE_TYPE_PRESENT_FRAME_TOKEN_GGP -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPresentFrameTokenGGP))
#endif
    VK_STRUCTURE_TYPE_PIPELINE_CREATION_FEEDBACK_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkPipelineCreationFeedbackCreateInfoEXT))
    VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceFullScreenExclusiveInfoEXT))
    VK_STRUCTURE_TYPE_SURFACE_FULL_SCREEN_EXCLUSIVE_WIN32_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceFullScreenExclusiveWin32InfoEXT))
    VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_FULL_SCREEN_EXCLUSIVE_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkSurfaceCapabilitiesFullScreenExclusiveEXT))
    VK_STRUCTURE_TYPE_HEADLESS_SURFACE_CREATE_INFO_EXT -> undefined -- SomeVkStruct <$> (fromCStruct =<< peek (castPtr p :: Ptr VkHeadlessSurfaceCreateInfoEXT))
    t -> throwIO (IOError Nothing InvalidArgument "" ("Unknown VkStructureType: " ++ show t) Nothing Nothing)
