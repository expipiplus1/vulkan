{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , pattern VK_FALSE
  , pattern VK_TRUE
  , VkFormat(..)
  , pattern VK_FORMAT_UNDEFINED
  , pattern VK_FORMAT_R4G4_UNORM_PACK8
  , pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16
  , pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16
  , pattern VK_FORMAT_R5G6B5_UNORM_PACK16
  , pattern VK_FORMAT_B5G6R5_UNORM_PACK16
  , pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16
  , pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16
  , pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16
  , pattern VK_FORMAT_R8_UNORM
  , pattern VK_FORMAT_R8_SNORM
  , pattern VK_FORMAT_R8_USCALED
  , pattern VK_FORMAT_R8_SSCALED
  , pattern VK_FORMAT_R8_UINT
  , pattern VK_FORMAT_R8_SINT
  , pattern VK_FORMAT_R8_SRGB
  , pattern VK_FORMAT_R8G8_UNORM
  , pattern VK_FORMAT_R8G8_SNORM
  , pattern VK_FORMAT_R8G8_USCALED
  , pattern VK_FORMAT_R8G8_SSCALED
  , pattern VK_FORMAT_R8G8_UINT
  , pattern VK_FORMAT_R8G8_SINT
  , pattern VK_FORMAT_R8G8_SRGB
  , pattern VK_FORMAT_R8G8B8_UNORM
  , pattern VK_FORMAT_R8G8B8_SNORM
  , pattern VK_FORMAT_R8G8B8_USCALED
  , pattern VK_FORMAT_R8G8B8_SSCALED
  , pattern VK_FORMAT_R8G8B8_UINT
  , pattern VK_FORMAT_R8G8B8_SINT
  , pattern VK_FORMAT_R8G8B8_SRGB
  , pattern VK_FORMAT_B8G8R8_UNORM
  , pattern VK_FORMAT_B8G8R8_SNORM
  , pattern VK_FORMAT_B8G8R8_USCALED
  , pattern VK_FORMAT_B8G8R8_SSCALED
  , pattern VK_FORMAT_B8G8R8_UINT
  , pattern VK_FORMAT_B8G8R8_SINT
  , pattern VK_FORMAT_B8G8R8_SRGB
  , pattern VK_FORMAT_R8G8B8A8_UNORM
  , pattern VK_FORMAT_R8G8B8A8_SNORM
  , pattern VK_FORMAT_R8G8B8A8_USCALED
  , pattern VK_FORMAT_R8G8B8A8_SSCALED
  , pattern VK_FORMAT_R8G8B8A8_UINT
  , pattern VK_FORMAT_R8G8B8A8_SINT
  , pattern VK_FORMAT_R8G8B8A8_SRGB
  , pattern VK_FORMAT_B8G8R8A8_UNORM
  , pattern VK_FORMAT_B8G8R8A8_SNORM
  , pattern VK_FORMAT_B8G8R8A8_USCALED
  , pattern VK_FORMAT_B8G8R8A8_SSCALED
  , pattern VK_FORMAT_B8G8R8A8_UINT
  , pattern VK_FORMAT_B8G8R8A8_SINT
  , pattern VK_FORMAT_B8G8R8A8_SRGB
  , pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_UINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UINT_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SINT_PACK32
  , pattern VK_FORMAT_R16_UNORM
  , pattern VK_FORMAT_R16_SNORM
  , pattern VK_FORMAT_R16_USCALED
  , pattern VK_FORMAT_R16_SSCALED
  , pattern VK_FORMAT_R16_UINT
  , pattern VK_FORMAT_R16_SINT
  , pattern VK_FORMAT_R16_SFLOAT
  , pattern VK_FORMAT_R16G16_UNORM
  , pattern VK_FORMAT_R16G16_SNORM
  , pattern VK_FORMAT_R16G16_USCALED
  , pattern VK_FORMAT_R16G16_SSCALED
  , pattern VK_FORMAT_R16G16_UINT
  , pattern VK_FORMAT_R16G16_SINT
  , pattern VK_FORMAT_R16G16_SFLOAT
  , pattern VK_FORMAT_R16G16B16_UNORM
  , pattern VK_FORMAT_R16G16B16_SNORM
  , pattern VK_FORMAT_R16G16B16_USCALED
  , pattern VK_FORMAT_R16G16B16_SSCALED
  , pattern VK_FORMAT_R16G16B16_UINT
  , pattern VK_FORMAT_R16G16B16_SINT
  , pattern VK_FORMAT_R16G16B16_SFLOAT
  , pattern VK_FORMAT_R16G16B16A16_UNORM
  , pattern VK_FORMAT_R16G16B16A16_SNORM
  , pattern VK_FORMAT_R16G16B16A16_USCALED
  , pattern VK_FORMAT_R16G16B16A16_SSCALED
  , pattern VK_FORMAT_R16G16B16A16_UINT
  , pattern VK_FORMAT_R16G16B16A16_SINT
  , pattern VK_FORMAT_R16G16B16A16_SFLOAT
  , pattern VK_FORMAT_R32_UINT
  , pattern VK_FORMAT_R32_SINT
  , pattern VK_FORMAT_R32_SFLOAT
  , pattern VK_FORMAT_R32G32_UINT
  , pattern VK_FORMAT_R32G32_SINT
  , pattern VK_FORMAT_R32G32_SFLOAT
  , pattern VK_FORMAT_R32G32B32_UINT
  , pattern VK_FORMAT_R32G32B32_SINT
  , pattern VK_FORMAT_R32G32B32_SFLOAT
  , pattern VK_FORMAT_R32G32B32A32_UINT
  , pattern VK_FORMAT_R32G32B32A32_SINT
  , pattern VK_FORMAT_R32G32B32A32_SFLOAT
  , pattern VK_FORMAT_R64_UINT
  , pattern VK_FORMAT_R64_SINT
  , pattern VK_FORMAT_R64_SFLOAT
  , pattern VK_FORMAT_R64G64_UINT
  , pattern VK_FORMAT_R64G64_SINT
  , pattern VK_FORMAT_R64G64_SFLOAT
  , pattern VK_FORMAT_R64G64B64_UINT
  , pattern VK_FORMAT_R64G64B64_SINT
  , pattern VK_FORMAT_R64G64B64_SFLOAT
  , pattern VK_FORMAT_R64G64B64A64_UINT
  , pattern VK_FORMAT_R64G64B64A64_SINT
  , pattern VK_FORMAT_R64G64B64A64_SFLOAT
  , pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32
  , pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
  , pattern VK_FORMAT_D16_UNORM
  , pattern VK_FORMAT_X8_D24_UNORM_PACK32
  , pattern VK_FORMAT_D32_SFLOAT
  , pattern VK_FORMAT_S8_UINT
  , pattern VK_FORMAT_D16_UNORM_S8_UINT
  , pattern VK_FORMAT_D24_UNORM_S8_UINT
  , pattern VK_FORMAT_D32_SFLOAT_S8_UINT
  , pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK
  , pattern VK_FORMAT_BC2_UNORM_BLOCK
  , pattern VK_FORMAT_BC2_SRGB_BLOCK
  , pattern VK_FORMAT_BC3_UNORM_BLOCK
  , pattern VK_FORMAT_BC3_SRGB_BLOCK
  , pattern VK_FORMAT_BC4_UNORM_BLOCK
  , pattern VK_FORMAT_BC4_SNORM_BLOCK
  , pattern VK_FORMAT_BC5_UNORM_BLOCK
  , pattern VK_FORMAT_BC5_SNORM_BLOCK
  , pattern VK_FORMAT_BC6H_UFLOAT_BLOCK
  , pattern VK_FORMAT_BC6H_SFLOAT_BLOCK
  , pattern VK_FORMAT_BC7_UNORM_BLOCK
  , pattern VK_FORMAT_BC7_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
  , pattern VK_FORMAT_EAC_R11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11_SNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK
  , VkResult(..)
  , pattern VK_SUCCESS
  , pattern VK_NOT_READY
  , pattern VK_TIMEOUT
  , pattern VK_EVENT_SET
  , pattern VK_EVENT_RESET
  , pattern VK_INCOMPLETE
  , pattern VK_ERROR_OUT_OF_HOST_MEMORY
  , pattern VK_ERROR_OUT_OF_DEVICE_MEMORY
  , pattern VK_ERROR_INITIALIZATION_FAILED
  , pattern VK_ERROR_DEVICE_LOST
  , pattern VK_ERROR_MEMORY_MAP_FAILED
  , pattern VK_ERROR_LAYER_NOT_PRESENT
  , pattern VK_ERROR_EXTENSION_NOT_PRESENT
  , pattern VK_ERROR_FEATURE_NOT_PRESENT
  , pattern VK_ERROR_INCOMPATIBLE_DRIVER
  , pattern VK_ERROR_TOO_MANY_OBJECTS
  , pattern VK_ERROR_FORMAT_NOT_SUPPORTED
  , pattern VK_ERROR_FRAGMENTED_POOL
  , VkStructureType(..)
  , pattern VK_STRUCTURE_TYPE_APPLICATION_INFO
  , pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  , pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
  , VkObjectType(..)
  , pattern VK_OBJECT_TYPE_UNKNOWN
  , pattern VK_OBJECT_TYPE_INSTANCE
  , pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE
  , pattern VK_OBJECT_TYPE_DEVICE
  , pattern VK_OBJECT_TYPE_QUEUE
  , pattern VK_OBJECT_TYPE_SEMAPHORE
  , pattern VK_OBJECT_TYPE_COMMAND_BUFFER
  , pattern VK_OBJECT_TYPE_FENCE
  , pattern VK_OBJECT_TYPE_DEVICE_MEMORY
  , pattern VK_OBJECT_TYPE_BUFFER
  , pattern VK_OBJECT_TYPE_IMAGE
  , pattern VK_OBJECT_TYPE_EVENT
  , pattern VK_OBJECT_TYPE_QUERY_POOL
  , pattern VK_OBJECT_TYPE_BUFFER_VIEW
  , pattern VK_OBJECT_TYPE_IMAGE_VIEW
  , pattern VK_OBJECT_TYPE_SHADER_MODULE
  , pattern VK_OBJECT_TYPE_PIPELINE_CACHE
  , pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT
  , pattern VK_OBJECT_TYPE_RENDER_PASS
  , pattern VK_OBJECT_TYPE_PIPELINE
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
  , pattern VK_OBJECT_TYPE_SAMPLER
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_SET
  , pattern VK_OBJECT_TYPE_FRAMEBUFFER
  , pattern VK_OBJECT_TYPE_COMMAND_POOL
  , VkFlags
  ) where

import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.Storable
  ( Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
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





-- ** VkBool32

-- | VkBool32 - Vulkan boolean type
--
-- = Description
--
-- @VK_TRUE@ represents a boolean __True__ (integer 1) value, and
-- @VK_FALSE@ a boolean __False__ (integer 0) value.
--
-- All values returned from a Vulkan implementation in a @VkBool32@ will be
-- either @VK_TRUE@ or @VK_FALSE@.
--
-- Applications /must/ not pass any other values than @VK_TRUE@ or
-- @VK_FALSE@ into a Vulkan implementation where a @VkBool32@ is expected.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.VkDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsFeaturesNVX',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.VkDisplayPresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayPropertiesKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage.VkPhysicalDevice16BitStorageFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization.VkPhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceIDProperties',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceLimits',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures',
-- 'Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryProperties',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkPhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters.VkPhysicalDeviceShaderDrawParameterFeatures',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDeviceSparseProperties',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointerFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPipelineColorBlendAdvancedStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineColorBlendAttachmentState',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color.VkPipelineCoverageToColorStateCreateInfoNV',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkProtectedSubmitInfo',
-- 'Graphics.Vulkan.Core10.Sampler.VkSamplerCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.VkTextureLODGatherFormatPropertiesAMD',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.vkGetPhysicalDeviceSurfaceSupportKHR',
-- 'Graphics.Vulkan.Core10.Fence.vkWaitForFences'
newtype VkBool32 = VkBool32 Int32
  deriving (Eq, Ord, Storable)

instance Show VkBool32 where
  showsPrec _ VK_FALSE = showString "VK_FALSE"
  showsPrec _ VK_TRUE = showString "VK_TRUE"
  showsPrec p (VkBool32 x) = showParen (p >= 11) (showString "VkBool32 " . showsPrec 11 x)

instance Read VkBool32 where
  readPrec = parens ( choose [ ("VK_FALSE", pure VK_FALSE)
                             , ("VK_TRUE",  pure VK_TRUE)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkBool32")
                        v <- step readPrec
                        pure (VkBool32 v)
                        )
                    )

-- No documentation found for Nested "VkBool32" "VK_FALSE"
pattern VK_FALSE :: VkBool32
pattern VK_FALSE = VkBool32 0

-- No documentation found for Nested "VkBool32" "VK_TRUE"
pattern VK_TRUE :: VkBool32
pattern VK_TRUE = VkBool32 1
-- ** VkFormat

-- | VkFormat - Available image formats
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceFormatKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.Pipeline.VkVertexInputAttributeDescription',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_capabilities.vkGetPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2KHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
newtype VkFormat = VkFormat Int32
  deriving (Eq, Ord, Storable)

instance Show VkFormat where
  showsPrec _ VK_FORMAT_UNDEFINED = showString "VK_FORMAT_UNDEFINED"
  showsPrec _ VK_FORMAT_R4G4_UNORM_PACK8 = showString "VK_FORMAT_R4G4_UNORM_PACK8"
  showsPrec _ VK_FORMAT_R4G4B4A4_UNORM_PACK16 = showString "VK_FORMAT_R4G4B4A4_UNORM_PACK16"
  showsPrec _ VK_FORMAT_B4G4R4A4_UNORM_PACK16 = showString "VK_FORMAT_B4G4R4A4_UNORM_PACK16"
  showsPrec _ VK_FORMAT_R5G6B5_UNORM_PACK16 = showString "VK_FORMAT_R5G6B5_UNORM_PACK16"
  showsPrec _ VK_FORMAT_B5G6R5_UNORM_PACK16 = showString "VK_FORMAT_B5G6R5_UNORM_PACK16"
  showsPrec _ VK_FORMAT_R5G5B5A1_UNORM_PACK16 = showString "VK_FORMAT_R5G5B5A1_UNORM_PACK16"
  showsPrec _ VK_FORMAT_B5G5R5A1_UNORM_PACK16 = showString "VK_FORMAT_B5G5R5A1_UNORM_PACK16"
  showsPrec _ VK_FORMAT_A1R5G5B5_UNORM_PACK16 = showString "VK_FORMAT_A1R5G5B5_UNORM_PACK16"
  showsPrec _ VK_FORMAT_R8_UNORM = showString "VK_FORMAT_R8_UNORM"
  showsPrec _ VK_FORMAT_R8_SNORM = showString "VK_FORMAT_R8_SNORM"
  showsPrec _ VK_FORMAT_R8_USCALED = showString "VK_FORMAT_R8_USCALED"
  showsPrec _ VK_FORMAT_R8_SSCALED = showString "VK_FORMAT_R8_SSCALED"
  showsPrec _ VK_FORMAT_R8_UINT = showString "VK_FORMAT_R8_UINT"
  showsPrec _ VK_FORMAT_R8_SINT = showString "VK_FORMAT_R8_SINT"
  showsPrec _ VK_FORMAT_R8_SRGB = showString "VK_FORMAT_R8_SRGB"
  showsPrec _ VK_FORMAT_R8G8_UNORM = showString "VK_FORMAT_R8G8_UNORM"
  showsPrec _ VK_FORMAT_R8G8_SNORM = showString "VK_FORMAT_R8G8_SNORM"
  showsPrec _ VK_FORMAT_R8G8_USCALED = showString "VK_FORMAT_R8G8_USCALED"
  showsPrec _ VK_FORMAT_R8G8_SSCALED = showString "VK_FORMAT_R8G8_SSCALED"
  showsPrec _ VK_FORMAT_R8G8_UINT = showString "VK_FORMAT_R8G8_UINT"
  showsPrec _ VK_FORMAT_R8G8_SINT = showString "VK_FORMAT_R8G8_SINT"
  showsPrec _ VK_FORMAT_R8G8_SRGB = showString "VK_FORMAT_R8G8_SRGB"
  showsPrec _ VK_FORMAT_R8G8B8_UNORM = showString "VK_FORMAT_R8G8B8_UNORM"
  showsPrec _ VK_FORMAT_R8G8B8_SNORM = showString "VK_FORMAT_R8G8B8_SNORM"
  showsPrec _ VK_FORMAT_R8G8B8_USCALED = showString "VK_FORMAT_R8G8B8_USCALED"
  showsPrec _ VK_FORMAT_R8G8B8_SSCALED = showString "VK_FORMAT_R8G8B8_SSCALED"
  showsPrec _ VK_FORMAT_R8G8B8_UINT = showString "VK_FORMAT_R8G8B8_UINT"
  showsPrec _ VK_FORMAT_R8G8B8_SINT = showString "VK_FORMAT_R8G8B8_SINT"
  showsPrec _ VK_FORMAT_R8G8B8_SRGB = showString "VK_FORMAT_R8G8B8_SRGB"
  showsPrec _ VK_FORMAT_B8G8R8_UNORM = showString "VK_FORMAT_B8G8R8_UNORM"
  showsPrec _ VK_FORMAT_B8G8R8_SNORM = showString "VK_FORMAT_B8G8R8_SNORM"
  showsPrec _ VK_FORMAT_B8G8R8_USCALED = showString "VK_FORMAT_B8G8R8_USCALED"
  showsPrec _ VK_FORMAT_B8G8R8_SSCALED = showString "VK_FORMAT_B8G8R8_SSCALED"
  showsPrec _ VK_FORMAT_B8G8R8_UINT = showString "VK_FORMAT_B8G8R8_UINT"
  showsPrec _ VK_FORMAT_B8G8R8_SINT = showString "VK_FORMAT_B8G8R8_SINT"
  showsPrec _ VK_FORMAT_B8G8R8_SRGB = showString "VK_FORMAT_B8G8R8_SRGB"
  showsPrec _ VK_FORMAT_R8G8B8A8_UNORM = showString "VK_FORMAT_R8G8B8A8_UNORM"
  showsPrec _ VK_FORMAT_R8G8B8A8_SNORM = showString "VK_FORMAT_R8G8B8A8_SNORM"
  showsPrec _ VK_FORMAT_R8G8B8A8_USCALED = showString "VK_FORMAT_R8G8B8A8_USCALED"
  showsPrec _ VK_FORMAT_R8G8B8A8_SSCALED = showString "VK_FORMAT_R8G8B8A8_SSCALED"
  showsPrec _ VK_FORMAT_R8G8B8A8_UINT = showString "VK_FORMAT_R8G8B8A8_UINT"
  showsPrec _ VK_FORMAT_R8G8B8A8_SINT = showString "VK_FORMAT_R8G8B8A8_SINT"
  showsPrec _ VK_FORMAT_R8G8B8A8_SRGB = showString "VK_FORMAT_R8G8B8A8_SRGB"
  showsPrec _ VK_FORMAT_B8G8R8A8_UNORM = showString "VK_FORMAT_B8G8R8A8_UNORM"
  showsPrec _ VK_FORMAT_B8G8R8A8_SNORM = showString "VK_FORMAT_B8G8R8A8_SNORM"
  showsPrec _ VK_FORMAT_B8G8R8A8_USCALED = showString "VK_FORMAT_B8G8R8A8_USCALED"
  showsPrec _ VK_FORMAT_B8G8R8A8_SSCALED = showString "VK_FORMAT_B8G8R8A8_SSCALED"
  showsPrec _ VK_FORMAT_B8G8R8A8_UINT = showString "VK_FORMAT_B8G8R8A8_UINT"
  showsPrec _ VK_FORMAT_B8G8R8A8_SINT = showString "VK_FORMAT_B8G8R8A8_SINT"
  showsPrec _ VK_FORMAT_B8G8R8A8_SRGB = showString "VK_FORMAT_B8G8R8A8_SRGB"
  showsPrec _ VK_FORMAT_A8B8G8R8_UNORM_PACK32 = showString "VK_FORMAT_A8B8G8R8_UNORM_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SNORM_PACK32 = showString "VK_FORMAT_A8B8G8R8_SNORM_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_USCALED_PACK32 = showString "VK_FORMAT_A8B8G8R8_USCALED_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = showString "VK_FORMAT_A8B8G8R8_SSCALED_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_UINT_PACK32 = showString "VK_FORMAT_A8B8G8R8_UINT_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SINT_PACK32 = showString "VK_FORMAT_A8B8G8R8_SINT_PACK32"
  showsPrec _ VK_FORMAT_A8B8G8R8_SRGB_PACK32 = showString "VK_FORMAT_A8B8G8R8_SRGB_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_UNORM_PACK32 = showString "VK_FORMAT_A2R10G10B10_UNORM_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_SNORM_PACK32 = showString "VK_FORMAT_A2R10G10B10_SNORM_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_USCALED_PACK32 = showString "VK_FORMAT_A2R10G10B10_USCALED_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = showString "VK_FORMAT_A2R10G10B10_SSCALED_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_UINT_PACK32 = showString "VK_FORMAT_A2R10G10B10_UINT_PACK32"
  showsPrec _ VK_FORMAT_A2R10G10B10_SINT_PACK32 = showString "VK_FORMAT_A2R10G10B10_SINT_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_UNORM_PACK32 = showString "VK_FORMAT_A2B10G10R10_UNORM_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_SNORM_PACK32 = showString "VK_FORMAT_A2B10G10R10_SNORM_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_USCALED_PACK32 = showString "VK_FORMAT_A2B10G10R10_USCALED_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = showString "VK_FORMAT_A2B10G10R10_SSCALED_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_UINT_PACK32 = showString "VK_FORMAT_A2B10G10R10_UINT_PACK32"
  showsPrec _ VK_FORMAT_A2B10G10R10_SINT_PACK32 = showString "VK_FORMAT_A2B10G10R10_SINT_PACK32"
  showsPrec _ VK_FORMAT_R16_UNORM = showString "VK_FORMAT_R16_UNORM"
  showsPrec _ VK_FORMAT_R16_SNORM = showString "VK_FORMAT_R16_SNORM"
  showsPrec _ VK_FORMAT_R16_USCALED = showString "VK_FORMAT_R16_USCALED"
  showsPrec _ VK_FORMAT_R16_SSCALED = showString "VK_FORMAT_R16_SSCALED"
  showsPrec _ VK_FORMAT_R16_UINT = showString "VK_FORMAT_R16_UINT"
  showsPrec _ VK_FORMAT_R16_SINT = showString "VK_FORMAT_R16_SINT"
  showsPrec _ VK_FORMAT_R16_SFLOAT = showString "VK_FORMAT_R16_SFLOAT"
  showsPrec _ VK_FORMAT_R16G16_UNORM = showString "VK_FORMAT_R16G16_UNORM"
  showsPrec _ VK_FORMAT_R16G16_SNORM = showString "VK_FORMAT_R16G16_SNORM"
  showsPrec _ VK_FORMAT_R16G16_USCALED = showString "VK_FORMAT_R16G16_USCALED"
  showsPrec _ VK_FORMAT_R16G16_SSCALED = showString "VK_FORMAT_R16G16_SSCALED"
  showsPrec _ VK_FORMAT_R16G16_UINT = showString "VK_FORMAT_R16G16_UINT"
  showsPrec _ VK_FORMAT_R16G16_SINT = showString "VK_FORMAT_R16G16_SINT"
  showsPrec _ VK_FORMAT_R16G16_SFLOAT = showString "VK_FORMAT_R16G16_SFLOAT"
  showsPrec _ VK_FORMAT_R16G16B16_UNORM = showString "VK_FORMAT_R16G16B16_UNORM"
  showsPrec _ VK_FORMAT_R16G16B16_SNORM = showString "VK_FORMAT_R16G16B16_SNORM"
  showsPrec _ VK_FORMAT_R16G16B16_USCALED = showString "VK_FORMAT_R16G16B16_USCALED"
  showsPrec _ VK_FORMAT_R16G16B16_SSCALED = showString "VK_FORMAT_R16G16B16_SSCALED"
  showsPrec _ VK_FORMAT_R16G16B16_UINT = showString "VK_FORMAT_R16G16B16_UINT"
  showsPrec _ VK_FORMAT_R16G16B16_SINT = showString "VK_FORMAT_R16G16B16_SINT"
  showsPrec _ VK_FORMAT_R16G16B16_SFLOAT = showString "VK_FORMAT_R16G16B16_SFLOAT"
  showsPrec _ VK_FORMAT_R16G16B16A16_UNORM = showString "VK_FORMAT_R16G16B16A16_UNORM"
  showsPrec _ VK_FORMAT_R16G16B16A16_SNORM = showString "VK_FORMAT_R16G16B16A16_SNORM"
  showsPrec _ VK_FORMAT_R16G16B16A16_USCALED = showString "VK_FORMAT_R16G16B16A16_USCALED"
  showsPrec _ VK_FORMAT_R16G16B16A16_SSCALED = showString "VK_FORMAT_R16G16B16A16_SSCALED"
  showsPrec _ VK_FORMAT_R16G16B16A16_UINT = showString "VK_FORMAT_R16G16B16A16_UINT"
  showsPrec _ VK_FORMAT_R16G16B16A16_SINT = showString "VK_FORMAT_R16G16B16A16_SINT"
  showsPrec _ VK_FORMAT_R16G16B16A16_SFLOAT = showString "VK_FORMAT_R16G16B16A16_SFLOAT"
  showsPrec _ VK_FORMAT_R32_UINT = showString "VK_FORMAT_R32_UINT"
  showsPrec _ VK_FORMAT_R32_SINT = showString "VK_FORMAT_R32_SINT"
  showsPrec _ VK_FORMAT_R32_SFLOAT = showString "VK_FORMAT_R32_SFLOAT"
  showsPrec _ VK_FORMAT_R32G32_UINT = showString "VK_FORMAT_R32G32_UINT"
  showsPrec _ VK_FORMAT_R32G32_SINT = showString "VK_FORMAT_R32G32_SINT"
  showsPrec _ VK_FORMAT_R32G32_SFLOAT = showString "VK_FORMAT_R32G32_SFLOAT"
  showsPrec _ VK_FORMAT_R32G32B32_UINT = showString "VK_FORMAT_R32G32B32_UINT"
  showsPrec _ VK_FORMAT_R32G32B32_SINT = showString "VK_FORMAT_R32G32B32_SINT"
  showsPrec _ VK_FORMAT_R32G32B32_SFLOAT = showString "VK_FORMAT_R32G32B32_SFLOAT"
  showsPrec _ VK_FORMAT_R32G32B32A32_UINT = showString "VK_FORMAT_R32G32B32A32_UINT"
  showsPrec _ VK_FORMAT_R32G32B32A32_SINT = showString "VK_FORMAT_R32G32B32A32_SINT"
  showsPrec _ VK_FORMAT_R32G32B32A32_SFLOAT = showString "VK_FORMAT_R32G32B32A32_SFLOAT"
  showsPrec _ VK_FORMAT_R64_UINT = showString "VK_FORMAT_R64_UINT"
  showsPrec _ VK_FORMAT_R64_SINT = showString "VK_FORMAT_R64_SINT"
  showsPrec _ VK_FORMAT_R64_SFLOAT = showString "VK_FORMAT_R64_SFLOAT"
  showsPrec _ VK_FORMAT_R64G64_UINT = showString "VK_FORMAT_R64G64_UINT"
  showsPrec _ VK_FORMAT_R64G64_SINT = showString "VK_FORMAT_R64G64_SINT"
  showsPrec _ VK_FORMAT_R64G64_SFLOAT = showString "VK_FORMAT_R64G64_SFLOAT"
  showsPrec _ VK_FORMAT_R64G64B64_UINT = showString "VK_FORMAT_R64G64B64_UINT"
  showsPrec _ VK_FORMAT_R64G64B64_SINT = showString "VK_FORMAT_R64G64B64_SINT"
  showsPrec _ VK_FORMAT_R64G64B64_SFLOAT = showString "VK_FORMAT_R64G64B64_SFLOAT"
  showsPrec _ VK_FORMAT_R64G64B64A64_UINT = showString "VK_FORMAT_R64G64B64A64_UINT"
  showsPrec _ VK_FORMAT_R64G64B64A64_SINT = showString "VK_FORMAT_R64G64B64A64_SINT"
  showsPrec _ VK_FORMAT_R64G64B64A64_SFLOAT = showString "VK_FORMAT_R64G64B64A64_SFLOAT"
  showsPrec _ VK_FORMAT_B10G11R11_UFLOAT_PACK32 = showString "VK_FORMAT_B10G11R11_UFLOAT_PACK32"
  showsPrec _ VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = showString "VK_FORMAT_E5B9G9R9_UFLOAT_PACK32"
  showsPrec _ VK_FORMAT_D16_UNORM = showString "VK_FORMAT_D16_UNORM"
  showsPrec _ VK_FORMAT_X8_D24_UNORM_PACK32 = showString "VK_FORMAT_X8_D24_UNORM_PACK32"
  showsPrec _ VK_FORMAT_D32_SFLOAT = showString "VK_FORMAT_D32_SFLOAT"
  showsPrec _ VK_FORMAT_S8_UINT = showString "VK_FORMAT_S8_UINT"
  showsPrec _ VK_FORMAT_D16_UNORM_S8_UINT = showString "VK_FORMAT_D16_UNORM_S8_UINT"
  showsPrec _ VK_FORMAT_D24_UNORM_S8_UINT = showString "VK_FORMAT_D24_UNORM_S8_UINT"
  showsPrec _ VK_FORMAT_D32_SFLOAT_S8_UINT = showString "VK_FORMAT_D32_SFLOAT_S8_UINT"
  showsPrec _ VK_FORMAT_BC1_RGB_UNORM_BLOCK = showString "VK_FORMAT_BC1_RGB_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC1_RGB_SRGB_BLOCK = showString "VK_FORMAT_BC1_RGB_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC1_RGBA_UNORM_BLOCK = showString "VK_FORMAT_BC1_RGBA_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC1_RGBA_SRGB_BLOCK = showString "VK_FORMAT_BC1_RGBA_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC2_UNORM_BLOCK = showString "VK_FORMAT_BC2_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC2_SRGB_BLOCK = showString "VK_FORMAT_BC2_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC3_UNORM_BLOCK = showString "VK_FORMAT_BC3_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC3_SRGB_BLOCK = showString "VK_FORMAT_BC3_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_BC4_UNORM_BLOCK = showString "VK_FORMAT_BC4_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC4_SNORM_BLOCK = showString "VK_FORMAT_BC4_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC5_UNORM_BLOCK = showString "VK_FORMAT_BC5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC5_SNORM_BLOCK = showString "VK_FORMAT_BC5_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC6H_UFLOAT_BLOCK = showString "VK_FORMAT_BC6H_UFLOAT_BLOCK"
  showsPrec _ VK_FORMAT_BC6H_SFLOAT_BLOCK = showString "VK_FORMAT_BC6H_SFLOAT_BLOCK"
  showsPrec _ VK_FORMAT_BC7_UNORM_BLOCK = showString "VK_FORMAT_BC7_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_BC7_SRGB_BLOCK = showString "VK_FORMAT_BC7_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = showString "VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11_UNORM_BLOCK = showString "VK_FORMAT_EAC_R11_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11_SNORM_BLOCK = showString "VK_FORMAT_EAC_R11_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11G11_UNORM_BLOCK = showString "VK_FORMAT_EAC_R11G11_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_EAC_R11G11_SNORM_BLOCK = showString "VK_FORMAT_EAC_R11G11_SNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_4x4_UNORM_BLOCK = showString "VK_FORMAT_ASTC_4x4_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_4x4_SRGB_BLOCK = showString "VK_FORMAT_ASTC_4x4_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x4_UNORM_BLOCK = showString "VK_FORMAT_ASTC_5x4_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x4_SRGB_BLOCK = showString "VK_FORMAT_ASTC_5x4_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_5x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_5x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_5x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_6x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_6x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x6_UNORM_BLOCK = showString "VK_FORMAT_ASTC_6x6_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_6x6_SRGB_BLOCK = showString "VK_FORMAT_ASTC_6x6_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_8x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_8x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x6_UNORM_BLOCK = showString "VK_FORMAT_ASTC_8x6_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x6_SRGB_BLOCK = showString "VK_FORMAT_ASTC_8x6_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x8_UNORM_BLOCK = showString "VK_FORMAT_ASTC_8x8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_8x8_SRGB_BLOCK = showString "VK_FORMAT_ASTC_8x8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x5_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x5_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x5_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x5_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x6_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x6_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x6_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x6_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x8_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x8_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x8_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x8_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x10_UNORM_BLOCK = showString "VK_FORMAT_ASTC_10x10_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_10x10_SRGB_BLOCK = showString "VK_FORMAT_ASTC_10x10_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x10_UNORM_BLOCK = showString "VK_FORMAT_ASTC_12x10_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x10_SRGB_BLOCK = showString "VK_FORMAT_ASTC_12x10_SRGB_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x12_UNORM_BLOCK = showString "VK_FORMAT_ASTC_12x12_UNORM_BLOCK"
  showsPrec _ VK_FORMAT_ASTC_12x12_SRGB_BLOCK = showString "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkFormat 1000156000) = showString "VK_FORMAT_G8B8G8R8_422_UNORM"
  showsPrec _ (VkFormat 1000156001) = showString "VK_FORMAT_B8G8R8G8_422_UNORM"
  showsPrec _ (VkFormat 1000156002) = showString "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156003) = showString "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156004) = showString "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156005) = showString "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156006) = showString "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM"
  showsPrec _ (VkFormat 1000156007) = showString "VK_FORMAT_R10X6_UNORM_PACK16"
  showsPrec _ (VkFormat 1000156008) = showString "VK_FORMAT_R10X6G10X6_UNORM_2PACK16"
  showsPrec _ (VkFormat 1000156009) = showString "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156010) = showString "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156011) = showString "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156012) = showString "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156013) = showString "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156014) = showString "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156015) = showString "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156016) = showString "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156017) = showString "VK_FORMAT_R12X4_UNORM_PACK16"
  showsPrec _ (VkFormat 1000156018) = showString "VK_FORMAT_R12X4G12X4_UNORM_2PACK16"
  showsPrec _ (VkFormat 1000156019) = showString "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156020) = showString "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156021) = showString "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16"
  showsPrec _ (VkFormat 1000156022) = showString "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156023) = showString "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156024) = showString "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156025) = showString "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156026) = showString "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16"
  showsPrec _ (VkFormat 1000156027) = showString "VK_FORMAT_G16B16G16R16_422_UNORM"
  showsPrec _ (VkFormat 1000156028) = showString "VK_FORMAT_B16G16R16G16_422_UNORM"
  showsPrec _ (VkFormat 1000156029) = showString "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156030) = showString "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM"
  showsPrec _ (VkFormat 1000156031) = showString "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156032) = showString "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM"
  showsPrec _ (VkFormat 1000156033) = showString "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM"
  showsPrec _ (VkFormat 1000054000) = showString "VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054001) = showString "VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054002) = showString "VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054003) = showString "VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054004) = showString "VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054005) = showString "VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054006) = showString "VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG"
  showsPrec _ (VkFormat 1000054007) = showString "VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG"
  showsPrec p (VkFormat x) = showParen (p >= 11) (showString "VkFormat " . showsPrec 11 x)

instance Read VkFormat where
  readPrec = parens ( choose [ ("VK_FORMAT_UNDEFINED",                  pure VK_FORMAT_UNDEFINED)
                             , ("VK_FORMAT_R4G4_UNORM_PACK8",           pure VK_FORMAT_R4G4_UNORM_PACK8)
                             , ("VK_FORMAT_R4G4B4A4_UNORM_PACK16",      pure VK_FORMAT_R4G4B4A4_UNORM_PACK16)
                             , ("VK_FORMAT_B4G4R4A4_UNORM_PACK16",      pure VK_FORMAT_B4G4R4A4_UNORM_PACK16)
                             , ("VK_FORMAT_R5G6B5_UNORM_PACK16",        pure VK_FORMAT_R5G6B5_UNORM_PACK16)
                             , ("VK_FORMAT_B5G6R5_UNORM_PACK16",        pure VK_FORMAT_B5G6R5_UNORM_PACK16)
                             , ("VK_FORMAT_R5G5B5A1_UNORM_PACK16",      pure VK_FORMAT_R5G5B5A1_UNORM_PACK16)
                             , ("VK_FORMAT_B5G5R5A1_UNORM_PACK16",      pure VK_FORMAT_B5G5R5A1_UNORM_PACK16)
                             , ("VK_FORMAT_A1R5G5B5_UNORM_PACK16",      pure VK_FORMAT_A1R5G5B5_UNORM_PACK16)
                             , ("VK_FORMAT_R8_UNORM",                   pure VK_FORMAT_R8_UNORM)
                             , ("VK_FORMAT_R8_SNORM",                   pure VK_FORMAT_R8_SNORM)
                             , ("VK_FORMAT_R8_USCALED",                 pure VK_FORMAT_R8_USCALED)
                             , ("VK_FORMAT_R8_SSCALED",                 pure VK_FORMAT_R8_SSCALED)
                             , ("VK_FORMAT_R8_UINT",                    pure VK_FORMAT_R8_UINT)
                             , ("VK_FORMAT_R8_SINT",                    pure VK_FORMAT_R8_SINT)
                             , ("VK_FORMAT_R8_SRGB",                    pure VK_FORMAT_R8_SRGB)
                             , ("VK_FORMAT_R8G8_UNORM",                 pure VK_FORMAT_R8G8_UNORM)
                             , ("VK_FORMAT_R8G8_SNORM",                 pure VK_FORMAT_R8G8_SNORM)
                             , ("VK_FORMAT_R8G8_USCALED",               pure VK_FORMAT_R8G8_USCALED)
                             , ("VK_FORMAT_R8G8_SSCALED",               pure VK_FORMAT_R8G8_SSCALED)
                             , ("VK_FORMAT_R8G8_UINT",                  pure VK_FORMAT_R8G8_UINT)
                             , ("VK_FORMAT_R8G8_SINT",                  pure VK_FORMAT_R8G8_SINT)
                             , ("VK_FORMAT_R8G8_SRGB",                  pure VK_FORMAT_R8G8_SRGB)
                             , ("VK_FORMAT_R8G8B8_UNORM",               pure VK_FORMAT_R8G8B8_UNORM)
                             , ("VK_FORMAT_R8G8B8_SNORM",               pure VK_FORMAT_R8G8B8_SNORM)
                             , ("VK_FORMAT_R8G8B8_USCALED",             pure VK_FORMAT_R8G8B8_USCALED)
                             , ("VK_FORMAT_R8G8B8_SSCALED",             pure VK_FORMAT_R8G8B8_SSCALED)
                             , ("VK_FORMAT_R8G8B8_UINT",                pure VK_FORMAT_R8G8B8_UINT)
                             , ("VK_FORMAT_R8G8B8_SINT",                pure VK_FORMAT_R8G8B8_SINT)
                             , ("VK_FORMAT_R8G8B8_SRGB",                pure VK_FORMAT_R8G8B8_SRGB)
                             , ("VK_FORMAT_B8G8R8_UNORM",               pure VK_FORMAT_B8G8R8_UNORM)
                             , ("VK_FORMAT_B8G8R8_SNORM",               pure VK_FORMAT_B8G8R8_SNORM)
                             , ("VK_FORMAT_B8G8R8_USCALED",             pure VK_FORMAT_B8G8R8_USCALED)
                             , ("VK_FORMAT_B8G8R8_SSCALED",             pure VK_FORMAT_B8G8R8_SSCALED)
                             , ("VK_FORMAT_B8G8R8_UINT",                pure VK_FORMAT_B8G8R8_UINT)
                             , ("VK_FORMAT_B8G8R8_SINT",                pure VK_FORMAT_B8G8R8_SINT)
                             , ("VK_FORMAT_B8G8R8_SRGB",                pure VK_FORMAT_B8G8R8_SRGB)
                             , ("VK_FORMAT_R8G8B8A8_UNORM",             pure VK_FORMAT_R8G8B8A8_UNORM)
                             , ("VK_FORMAT_R8G8B8A8_SNORM",             pure VK_FORMAT_R8G8B8A8_SNORM)
                             , ("VK_FORMAT_R8G8B8A8_USCALED",           pure VK_FORMAT_R8G8B8A8_USCALED)
                             , ("VK_FORMAT_R8G8B8A8_SSCALED",           pure VK_FORMAT_R8G8B8A8_SSCALED)
                             , ("VK_FORMAT_R8G8B8A8_UINT",              pure VK_FORMAT_R8G8B8A8_UINT)
                             , ("VK_FORMAT_R8G8B8A8_SINT",              pure VK_FORMAT_R8G8B8A8_SINT)
                             , ("VK_FORMAT_R8G8B8A8_SRGB",              pure VK_FORMAT_R8G8B8A8_SRGB)
                             , ("VK_FORMAT_B8G8R8A8_UNORM",             pure VK_FORMAT_B8G8R8A8_UNORM)
                             , ("VK_FORMAT_B8G8R8A8_SNORM",             pure VK_FORMAT_B8G8R8A8_SNORM)
                             , ("VK_FORMAT_B8G8R8A8_USCALED",           pure VK_FORMAT_B8G8R8A8_USCALED)
                             , ("VK_FORMAT_B8G8R8A8_SSCALED",           pure VK_FORMAT_B8G8R8A8_SSCALED)
                             , ("VK_FORMAT_B8G8R8A8_UINT",              pure VK_FORMAT_B8G8R8A8_UINT)
                             , ("VK_FORMAT_B8G8R8A8_SINT",              pure VK_FORMAT_B8G8R8A8_SINT)
                             , ("VK_FORMAT_B8G8R8A8_SRGB",              pure VK_FORMAT_B8G8R8A8_SRGB)
                             , ("VK_FORMAT_A8B8G8R8_UNORM_PACK32",      pure VK_FORMAT_A8B8G8R8_UNORM_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SNORM_PACK32",      pure VK_FORMAT_A8B8G8R8_SNORM_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_USCALED_PACK32",    pure VK_FORMAT_A8B8G8R8_USCALED_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SSCALED_PACK32",    pure VK_FORMAT_A8B8G8R8_SSCALED_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_UINT_PACK32",       pure VK_FORMAT_A8B8G8R8_UINT_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SINT_PACK32",       pure VK_FORMAT_A8B8G8R8_SINT_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SRGB_PACK32",       pure VK_FORMAT_A8B8G8R8_SRGB_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_UNORM_PACK32",   pure VK_FORMAT_A2R10G10B10_UNORM_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SNORM_PACK32",   pure VK_FORMAT_A2R10G10B10_SNORM_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_USCALED_PACK32", pure VK_FORMAT_A2R10G10B10_USCALED_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SSCALED_PACK32", pure VK_FORMAT_A2R10G10B10_SSCALED_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_UINT_PACK32",    pure VK_FORMAT_A2R10G10B10_UINT_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SINT_PACK32",    pure VK_FORMAT_A2R10G10B10_SINT_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_UNORM_PACK32",   pure VK_FORMAT_A2B10G10R10_UNORM_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SNORM_PACK32",   pure VK_FORMAT_A2B10G10R10_SNORM_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_USCALED_PACK32", pure VK_FORMAT_A2B10G10R10_USCALED_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SSCALED_PACK32", pure VK_FORMAT_A2B10G10R10_SSCALED_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_UINT_PACK32",    pure VK_FORMAT_A2B10G10R10_UINT_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SINT_PACK32",    pure VK_FORMAT_A2B10G10R10_SINT_PACK32)
                             , ("VK_FORMAT_R16_UNORM",                  pure VK_FORMAT_R16_UNORM)
                             , ("VK_FORMAT_R16_SNORM",                  pure VK_FORMAT_R16_SNORM)
                             , ("VK_FORMAT_R16_USCALED",                pure VK_FORMAT_R16_USCALED)
                             , ("VK_FORMAT_R16_SSCALED",                pure VK_FORMAT_R16_SSCALED)
                             , ("VK_FORMAT_R16_UINT",                   pure VK_FORMAT_R16_UINT)
                             , ("VK_FORMAT_R16_SINT",                   pure VK_FORMAT_R16_SINT)
                             , ("VK_FORMAT_R16_SFLOAT",                 pure VK_FORMAT_R16_SFLOAT)
                             , ("VK_FORMAT_R16G16_UNORM",               pure VK_FORMAT_R16G16_UNORM)
                             , ("VK_FORMAT_R16G16_SNORM",               pure VK_FORMAT_R16G16_SNORM)
                             , ("VK_FORMAT_R16G16_USCALED",             pure VK_FORMAT_R16G16_USCALED)
                             , ("VK_FORMAT_R16G16_SSCALED",             pure VK_FORMAT_R16G16_SSCALED)
                             , ("VK_FORMAT_R16G16_UINT",                pure VK_FORMAT_R16G16_UINT)
                             , ("VK_FORMAT_R16G16_SINT",                pure VK_FORMAT_R16G16_SINT)
                             , ("VK_FORMAT_R16G16_SFLOAT",              pure VK_FORMAT_R16G16_SFLOAT)
                             , ("VK_FORMAT_R16G16B16_UNORM",            pure VK_FORMAT_R16G16B16_UNORM)
                             , ("VK_FORMAT_R16G16B16_SNORM",            pure VK_FORMAT_R16G16B16_SNORM)
                             , ("VK_FORMAT_R16G16B16_USCALED",          pure VK_FORMAT_R16G16B16_USCALED)
                             , ("VK_FORMAT_R16G16B16_SSCALED",          pure VK_FORMAT_R16G16B16_SSCALED)
                             , ("VK_FORMAT_R16G16B16_UINT",             pure VK_FORMAT_R16G16B16_UINT)
                             , ("VK_FORMAT_R16G16B16_SINT",             pure VK_FORMAT_R16G16B16_SINT)
                             , ("VK_FORMAT_R16G16B16_SFLOAT",           pure VK_FORMAT_R16G16B16_SFLOAT)
                             , ("VK_FORMAT_R16G16B16A16_UNORM",         pure VK_FORMAT_R16G16B16A16_UNORM)
                             , ("VK_FORMAT_R16G16B16A16_SNORM",         pure VK_FORMAT_R16G16B16A16_SNORM)
                             , ("VK_FORMAT_R16G16B16A16_USCALED",       pure VK_FORMAT_R16G16B16A16_USCALED)
                             , ("VK_FORMAT_R16G16B16A16_SSCALED",       pure VK_FORMAT_R16G16B16A16_SSCALED)
                             , ("VK_FORMAT_R16G16B16A16_UINT",          pure VK_FORMAT_R16G16B16A16_UINT)
                             , ("VK_FORMAT_R16G16B16A16_SINT",          pure VK_FORMAT_R16G16B16A16_SINT)
                             , ("VK_FORMAT_R16G16B16A16_SFLOAT",        pure VK_FORMAT_R16G16B16A16_SFLOAT)
                             , ("VK_FORMAT_R32_UINT",                   pure VK_FORMAT_R32_UINT)
                             , ("VK_FORMAT_R32_SINT",                   pure VK_FORMAT_R32_SINT)
                             , ("VK_FORMAT_R32_SFLOAT",                 pure VK_FORMAT_R32_SFLOAT)
                             , ("VK_FORMAT_R32G32_UINT",                pure VK_FORMAT_R32G32_UINT)
                             , ("VK_FORMAT_R32G32_SINT",                pure VK_FORMAT_R32G32_SINT)
                             , ("VK_FORMAT_R32G32_SFLOAT",              pure VK_FORMAT_R32G32_SFLOAT)
                             , ("VK_FORMAT_R32G32B32_UINT",             pure VK_FORMAT_R32G32B32_UINT)
                             , ("VK_FORMAT_R32G32B32_SINT",             pure VK_FORMAT_R32G32B32_SINT)
                             , ("VK_FORMAT_R32G32B32_SFLOAT",           pure VK_FORMAT_R32G32B32_SFLOAT)
                             , ("VK_FORMAT_R32G32B32A32_UINT",          pure VK_FORMAT_R32G32B32A32_UINT)
                             , ("VK_FORMAT_R32G32B32A32_SINT",          pure VK_FORMAT_R32G32B32A32_SINT)
                             , ("VK_FORMAT_R32G32B32A32_SFLOAT",        pure VK_FORMAT_R32G32B32A32_SFLOAT)
                             , ("VK_FORMAT_R64_UINT",                   pure VK_FORMAT_R64_UINT)
                             , ("VK_FORMAT_R64_SINT",                   pure VK_FORMAT_R64_SINT)
                             , ("VK_FORMAT_R64_SFLOAT",                 pure VK_FORMAT_R64_SFLOAT)
                             , ("VK_FORMAT_R64G64_UINT",                pure VK_FORMAT_R64G64_UINT)
                             , ("VK_FORMAT_R64G64_SINT",                pure VK_FORMAT_R64G64_SINT)
                             , ("VK_FORMAT_R64G64_SFLOAT",              pure VK_FORMAT_R64G64_SFLOAT)
                             , ("VK_FORMAT_R64G64B64_UINT",             pure VK_FORMAT_R64G64B64_UINT)
                             , ("VK_FORMAT_R64G64B64_SINT",             pure VK_FORMAT_R64G64B64_SINT)
                             , ("VK_FORMAT_R64G64B64_SFLOAT",           pure VK_FORMAT_R64G64B64_SFLOAT)
                             , ("VK_FORMAT_R64G64B64A64_UINT",          pure VK_FORMAT_R64G64B64A64_UINT)
                             , ("VK_FORMAT_R64G64B64A64_SINT",          pure VK_FORMAT_R64G64B64A64_SINT)
                             , ("VK_FORMAT_R64G64B64A64_SFLOAT",        pure VK_FORMAT_R64G64B64A64_SFLOAT)
                             , ("VK_FORMAT_B10G11R11_UFLOAT_PACK32",    pure VK_FORMAT_B10G11R11_UFLOAT_PACK32)
                             , ("VK_FORMAT_E5B9G9R9_UFLOAT_PACK32",     pure VK_FORMAT_E5B9G9R9_UFLOAT_PACK32)
                             , ("VK_FORMAT_D16_UNORM",                  pure VK_FORMAT_D16_UNORM)
                             , ("VK_FORMAT_X8_D24_UNORM_PACK32",        pure VK_FORMAT_X8_D24_UNORM_PACK32)
                             , ("VK_FORMAT_D32_SFLOAT",                 pure VK_FORMAT_D32_SFLOAT)
                             , ("VK_FORMAT_S8_UINT",                    pure VK_FORMAT_S8_UINT)
                             , ("VK_FORMAT_D16_UNORM_S8_UINT",          pure VK_FORMAT_D16_UNORM_S8_UINT)
                             , ("VK_FORMAT_D24_UNORM_S8_UINT",          pure VK_FORMAT_D24_UNORM_S8_UINT)
                             , ("VK_FORMAT_D32_SFLOAT_S8_UINT",         pure VK_FORMAT_D32_SFLOAT_S8_UINT)
                             , ("VK_FORMAT_BC1_RGB_UNORM_BLOCK",        pure VK_FORMAT_BC1_RGB_UNORM_BLOCK)
                             , ("VK_FORMAT_BC1_RGB_SRGB_BLOCK",         pure VK_FORMAT_BC1_RGB_SRGB_BLOCK)
                             , ("VK_FORMAT_BC1_RGBA_UNORM_BLOCK",       pure VK_FORMAT_BC1_RGBA_UNORM_BLOCK)
                             , ("VK_FORMAT_BC1_RGBA_SRGB_BLOCK",        pure VK_FORMAT_BC1_RGBA_SRGB_BLOCK)
                             , ("VK_FORMAT_BC2_UNORM_BLOCK",            pure VK_FORMAT_BC2_UNORM_BLOCK)
                             , ("VK_FORMAT_BC2_SRGB_BLOCK",             pure VK_FORMAT_BC2_SRGB_BLOCK)
                             , ("VK_FORMAT_BC3_UNORM_BLOCK",            pure VK_FORMAT_BC3_UNORM_BLOCK)
                             , ("VK_FORMAT_BC3_SRGB_BLOCK",             pure VK_FORMAT_BC3_SRGB_BLOCK)
                             , ("VK_FORMAT_BC4_UNORM_BLOCK",            pure VK_FORMAT_BC4_UNORM_BLOCK)
                             , ("VK_FORMAT_BC4_SNORM_BLOCK",            pure VK_FORMAT_BC4_SNORM_BLOCK)
                             , ("VK_FORMAT_BC5_UNORM_BLOCK",            pure VK_FORMAT_BC5_UNORM_BLOCK)
                             , ("VK_FORMAT_BC5_SNORM_BLOCK",            pure VK_FORMAT_BC5_SNORM_BLOCK)
                             , ("VK_FORMAT_BC6H_UFLOAT_BLOCK",          pure VK_FORMAT_BC6H_UFLOAT_BLOCK)
                             , ("VK_FORMAT_BC6H_SFLOAT_BLOCK",          pure VK_FORMAT_BC6H_SFLOAT_BLOCK)
                             , ("VK_FORMAT_BC7_UNORM_BLOCK",            pure VK_FORMAT_BC7_UNORM_BLOCK)
                             , ("VK_FORMAT_BC7_SRGB_BLOCK",             pure VK_FORMAT_BC7_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK",    pure VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK",     pure VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK",  pure VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK",   pure VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK",  pure VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK",   pure VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK)
                             , ("VK_FORMAT_EAC_R11_UNORM_BLOCK",        pure VK_FORMAT_EAC_R11_UNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11_SNORM_BLOCK",        pure VK_FORMAT_EAC_R11_SNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11G11_UNORM_BLOCK",     pure VK_FORMAT_EAC_R11G11_UNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11G11_SNORM_BLOCK",     pure VK_FORMAT_EAC_R11G11_SNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_4x4_UNORM_BLOCK",       pure VK_FORMAT_ASTC_4x4_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_4x4_SRGB_BLOCK",        pure VK_FORMAT_ASTC_4x4_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_5x4_UNORM_BLOCK",       pure VK_FORMAT_ASTC_5x4_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_5x4_SRGB_BLOCK",        pure VK_FORMAT_ASTC_5x4_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_5x5_UNORM_BLOCK",       pure VK_FORMAT_ASTC_5x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_5x5_SRGB_BLOCK",        pure VK_FORMAT_ASTC_5x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_6x5_UNORM_BLOCK",       pure VK_FORMAT_ASTC_6x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_6x5_SRGB_BLOCK",        pure VK_FORMAT_ASTC_6x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_6x6_UNORM_BLOCK",       pure VK_FORMAT_ASTC_6x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_6x6_SRGB_BLOCK",        pure VK_FORMAT_ASTC_6x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x5_UNORM_BLOCK",       pure VK_FORMAT_ASTC_8x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x5_SRGB_BLOCK",        pure VK_FORMAT_ASTC_8x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x6_UNORM_BLOCK",       pure VK_FORMAT_ASTC_8x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x6_SRGB_BLOCK",        pure VK_FORMAT_ASTC_8x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x8_UNORM_BLOCK",       pure VK_FORMAT_ASTC_8x8_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x8_SRGB_BLOCK",        pure VK_FORMAT_ASTC_8x8_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x5_UNORM_BLOCK",      pure VK_FORMAT_ASTC_10x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x5_SRGB_BLOCK",       pure VK_FORMAT_ASTC_10x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x6_UNORM_BLOCK",      pure VK_FORMAT_ASTC_10x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x6_SRGB_BLOCK",       pure VK_FORMAT_ASTC_10x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x8_UNORM_BLOCK",      pure VK_FORMAT_ASTC_10x8_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x8_SRGB_BLOCK",       pure VK_FORMAT_ASTC_10x8_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x10_UNORM_BLOCK",     pure VK_FORMAT_ASTC_10x10_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x10_SRGB_BLOCK",      pure VK_FORMAT_ASTC_10x10_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_12x10_UNORM_BLOCK",     pure VK_FORMAT_ASTC_12x10_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_12x10_SRGB_BLOCK",      pure VK_FORMAT_ASTC_12x10_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_12x12_UNORM_BLOCK",     pure VK_FORMAT_ASTC_12x12_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_12x12_SRGB_BLOCK",      pure VK_FORMAT_ASTC_12x12_SRGB_BLOCK)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_FORMAT_G8B8G8R8_422_UNORM",                         pure (VkFormat 1000156000))
                             , ("VK_FORMAT_B8G8R8G8_422_UNORM",                         pure (VkFormat 1000156001))
                             , ("VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM",                  pure (VkFormat 1000156002))
                             , ("VK_FORMAT_G8_B8R8_2PLANE_420_UNORM",                   pure (VkFormat 1000156003))
                             , ("VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM",                  pure (VkFormat 1000156004))
                             , ("VK_FORMAT_G8_B8R8_2PLANE_422_UNORM",                   pure (VkFormat 1000156005))
                             , ("VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM",                  pure (VkFormat 1000156006))
                             , ("VK_FORMAT_R10X6_UNORM_PACK16",                         pure (VkFormat 1000156007))
                             , ("VK_FORMAT_R10X6G10X6_UNORM_2PACK16",                   pure (VkFormat 1000156008))
                             , ("VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16",         pure (VkFormat 1000156009))
                             , ("VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16",     pure (VkFormat 1000156010))
                             , ("VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16",     pure (VkFormat 1000156011))
                             , ("VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16", pure (VkFormat 1000156012))
                             , ("VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16",  pure (VkFormat 1000156013))
                             , ("VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16", pure (VkFormat 1000156014))
                             , ("VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16",  pure (VkFormat 1000156015))
                             , ("VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16", pure (VkFormat 1000156016))
                             , ("VK_FORMAT_R12X4_UNORM_PACK16",                         pure (VkFormat 1000156017))
                             , ("VK_FORMAT_R12X4G12X4_UNORM_2PACK16",                   pure (VkFormat 1000156018))
                             , ("VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16",         pure (VkFormat 1000156019))
                             , ("VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16",     pure (VkFormat 1000156020))
                             , ("VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16",     pure (VkFormat 1000156021))
                             , ("VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16", pure (VkFormat 1000156022))
                             , ("VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16",  pure (VkFormat 1000156023))
                             , ("VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16", pure (VkFormat 1000156024))
                             , ("VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16",  pure (VkFormat 1000156025))
                             , ("VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16", pure (VkFormat 1000156026))
                             , ("VK_FORMAT_G16B16G16R16_422_UNORM",                     pure (VkFormat 1000156027))
                             , ("VK_FORMAT_B16G16R16G16_422_UNORM",                     pure (VkFormat 1000156028))
                             , ("VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM",               pure (VkFormat 1000156029))
                             , ("VK_FORMAT_G16_B16R16_2PLANE_420_UNORM",                pure (VkFormat 1000156030))
                             , ("VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM",               pure (VkFormat 1000156031))
                             , ("VK_FORMAT_G16_B16R16_2PLANE_422_UNORM",                pure (VkFormat 1000156032))
                             , ("VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM",               pure (VkFormat 1000156033))
                             , ("VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054000))
                             , ("VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054001))
                             , ("VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054002))
                             , ("VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG",                pure (VkFormat 1000054003))
                             , ("VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054004))
                             , ("VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054005))
                             , ("VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054006))
                             , ("VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG",                 pure (VkFormat 1000054007))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFormat")
                        v <- step readPrec
                        pure (VkFormat v)
                        )
                    )

-- | @VK_FORMAT_UNDEFINED@ specifies that the format is not specified.
pattern VK_FORMAT_UNDEFINED :: VkFormat
pattern VK_FORMAT_UNDEFINED = VkFormat 0

-- | @VK_FORMAT_R4G4_UNORM_PACK8@ specifies a two-component, 8-bit packed
-- unsigned normalized format that has a 4-bit R component in bits 4..7,
-- and a 4-bit G component in bits 0..3.
pattern VK_FORMAT_R4G4_UNORM_PACK8 :: VkFormat
pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1

-- | @VK_FORMAT_R4G4B4A4_UNORM_PACK16@ specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 4-bit R component in bits
-- 12..15, a 4-bit G component in bits 8..11, a 4-bit B component in bits
-- 4..7, and a 4-bit A component in bits 0..3.
pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2

-- | @VK_FORMAT_B4G4R4A4_UNORM_PACK16@ specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 4-bit B component in bits
-- 12..15, a 4-bit G component in bits 8..11, a 4-bit R component in bits
-- 4..7, and a 4-bit A component in bits 0..3.
pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3

-- | @VK_FORMAT_R5G6B5_UNORM_PACK16@ specifies a three-component, 16-bit
-- packed unsigned normalized format that has a 5-bit R component in bits
-- 11..15, a 6-bit G component in bits 5..10, and a 5-bit B component in
-- bits 0..4.
pattern VK_FORMAT_R5G6B5_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4

-- | @VK_FORMAT_B5G6R5_UNORM_PACK16@ specifies a three-component, 16-bit
-- packed unsigned normalized format that has a 5-bit B component in bits
-- 11..15, a 6-bit G component in bits 5..10, and a 5-bit R component in
-- bits 0..4.
pattern VK_FORMAT_B5G6R5_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5

-- | @VK_FORMAT_R5G5B5A1_UNORM_PACK16@ specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 5-bit R component in bits
-- 11..15, a 5-bit G component in bits 6..10, a 5-bit B component in bits
-- 1..5, and a 1-bit A component in bit 0.
pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6

-- | @VK_FORMAT_B5G5R5A1_UNORM_PACK16@ specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 5-bit B component in bits
-- 11..15, a 5-bit G component in bits 6..10, a 5-bit R component in bits
-- 1..5, and a 1-bit A component in bit 0.
pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7

-- | @VK_FORMAT_A1R5G5B5_UNORM_PACK16@ specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 1-bit A component in bit
-- 15, a 5-bit R component in bits 10..14, a 5-bit G component in bits
-- 5..9, and a 5-bit B component in bits 0..4.
pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 :: VkFormat
pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8

-- | @VK_FORMAT_R8_UNORM@ specifies a one-component, 8-bit unsigned
-- normalized format that has a single 8-bit R component.
pattern VK_FORMAT_R8_UNORM :: VkFormat
pattern VK_FORMAT_R8_UNORM = VkFormat 9

-- | @VK_FORMAT_R8_SNORM@ specifies a one-component, 8-bit signed normalized
-- format that has a single 8-bit R component.
pattern VK_FORMAT_R8_SNORM :: VkFormat
pattern VK_FORMAT_R8_SNORM = VkFormat 10

-- | @VK_FORMAT_R8_USCALED@ specifies a one-component, 8-bit unsigned scaled
-- integer format that has a single 8-bit R component.
pattern VK_FORMAT_R8_USCALED :: VkFormat
pattern VK_FORMAT_R8_USCALED = VkFormat 11

-- | @VK_FORMAT_R8_SSCALED@ specifies a one-component, 8-bit signed scaled
-- integer format that has a single 8-bit R component.
pattern VK_FORMAT_R8_SSCALED :: VkFormat
pattern VK_FORMAT_R8_SSCALED = VkFormat 12

-- | @VK_FORMAT_R8_UINT@ specifies a one-component, 8-bit unsigned integer
-- format that has a single 8-bit R component.
pattern VK_FORMAT_R8_UINT :: VkFormat
pattern VK_FORMAT_R8_UINT = VkFormat 13

-- | @VK_FORMAT_R8_SINT@ specifies a one-component, 8-bit signed integer
-- format that has a single 8-bit R component.
pattern VK_FORMAT_R8_SINT :: VkFormat
pattern VK_FORMAT_R8_SINT = VkFormat 14

-- | @VK_FORMAT_R8_SRGB@ specifies a one-component, 8-bit unsigned normalized
-- format that has a single 8-bit R component stored with sRGB nonlinear
-- encoding.
pattern VK_FORMAT_R8_SRGB :: VkFormat
pattern VK_FORMAT_R8_SRGB = VkFormat 15

-- | @VK_FORMAT_R8G8_UNORM@ specifies a two-component, 16-bit unsigned
-- normalized format that has an 8-bit R component in byte 0, and an 8-bit
-- G component in byte 1.
pattern VK_FORMAT_R8G8_UNORM :: VkFormat
pattern VK_FORMAT_R8G8_UNORM = VkFormat 16

-- | @VK_FORMAT_R8G8_SNORM@ specifies a two-component, 16-bit signed
-- normalized format that has an 8-bit R component in byte 0, and an 8-bit
-- G component in byte 1.
pattern VK_FORMAT_R8G8_SNORM :: VkFormat
pattern VK_FORMAT_R8G8_SNORM = VkFormat 17

-- | @VK_FORMAT_R8G8_USCALED@ specifies a two-component, 16-bit unsigned
-- scaled integer format that has an 8-bit R component in byte 0, and an
-- 8-bit G component in byte 1.
pattern VK_FORMAT_R8G8_USCALED :: VkFormat
pattern VK_FORMAT_R8G8_USCALED = VkFormat 18

-- | @VK_FORMAT_R8G8_SSCALED@ specifies a two-component, 16-bit signed scaled
-- integer format that has an 8-bit R component in byte 0, and an 8-bit G
-- component in byte 1.
pattern VK_FORMAT_R8G8_SSCALED :: VkFormat
pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19

-- | @VK_FORMAT_R8G8_UINT@ specifies a two-component, 16-bit unsigned integer
-- format that has an 8-bit R component in byte 0, and an 8-bit G component
-- in byte 1.
pattern VK_FORMAT_R8G8_UINT :: VkFormat
pattern VK_FORMAT_R8G8_UINT = VkFormat 20

-- | @VK_FORMAT_R8G8_SINT@ specifies a two-component, 16-bit signed integer
-- format that has an 8-bit R component in byte 0, and an 8-bit G component
-- in byte 1.
pattern VK_FORMAT_R8G8_SINT :: VkFormat
pattern VK_FORMAT_R8G8_SINT = VkFormat 21

-- | @VK_FORMAT_R8G8_SRGB@ specifies a two-component, 16-bit unsigned
-- normalized format that has an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 0, and an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1.
pattern VK_FORMAT_R8G8_SRGB :: VkFormat
pattern VK_FORMAT_R8G8_SRGB = VkFormat 22

-- | @VK_FORMAT_R8G8B8_UNORM@ specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern VK_FORMAT_R8G8B8_UNORM :: VkFormat
pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23

-- | @VK_FORMAT_R8G8B8_SNORM@ specifies a three-component, 24-bit signed
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern VK_FORMAT_R8G8B8_SNORM :: VkFormat
pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24

-- | @VK_FORMAT_R8G8B8_USCALED@ specifies a three-component, 24-bit unsigned
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern VK_FORMAT_R8G8B8_USCALED :: VkFormat
pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25

-- | @VK_FORMAT_R8G8B8_SSCALED@ specifies a three-component, 24-bit signed
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern VK_FORMAT_R8G8B8_SSCALED :: VkFormat
pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26

-- | @VK_FORMAT_R8G8B8_UINT@ specifies a three-component, 24-bit unsigned
-- integer format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern VK_FORMAT_R8G8B8_UINT :: VkFormat
pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27

-- | @VK_FORMAT_R8G8B8_SINT@ specifies a three-component, 24-bit signed
-- integer format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern VK_FORMAT_R8G8B8_SINT :: VkFormat
pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28

-- | @VK_FORMAT_R8G8B8_SRGB@ specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, and an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 2.
pattern VK_FORMAT_R8G8B8_SRGB :: VkFormat
pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29

-- | @VK_FORMAT_B8G8R8_UNORM@ specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern VK_FORMAT_B8G8R8_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30

-- | @VK_FORMAT_B8G8R8_SNORM@ specifies a three-component, 24-bit signed
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern VK_FORMAT_B8G8R8_SNORM :: VkFormat
pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31

-- | @VK_FORMAT_B8G8R8_USCALED@ specifies a three-component, 24-bit unsigned
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern VK_FORMAT_B8G8R8_USCALED :: VkFormat
pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32

-- | @VK_FORMAT_B8G8R8_SSCALED@ specifies a three-component, 24-bit signed
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern VK_FORMAT_B8G8R8_SSCALED :: VkFormat
pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33

-- | @VK_FORMAT_B8G8R8_UINT@ specifies a three-component, 24-bit unsigned
-- integer format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern VK_FORMAT_B8G8R8_UINT :: VkFormat
pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34

-- | @VK_FORMAT_B8G8R8_SINT@ specifies a three-component, 24-bit signed
-- integer format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern VK_FORMAT_B8G8R8_SINT :: VkFormat
pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35

-- | @VK_FORMAT_B8G8R8_SRGB@ specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, and an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 2.
pattern VK_FORMAT_B8G8R8_SRGB :: VkFormat
pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36

-- | @VK_FORMAT_R8G8B8A8_UNORM@ specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_R8G8B8A8_UNORM :: VkFormat
pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37

-- | @VK_FORMAT_R8G8B8A8_SNORM@ specifies a four-component, 32-bit signed
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_R8G8B8A8_SNORM :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38

-- | @VK_FORMAT_R8G8B8A8_USCALED@ specifies a four-component, 32-bit unsigned
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_R8G8B8A8_USCALED :: VkFormat
pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39

-- | @VK_FORMAT_R8G8B8A8_SSCALED@ specifies a four-component, 32-bit signed
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_R8G8B8A8_SSCALED :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40

-- | @VK_FORMAT_R8G8B8A8_UINT@ specifies a four-component, 32-bit unsigned
-- integer format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_R8G8B8A8_UINT :: VkFormat
pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41

-- | @VK_FORMAT_R8G8B8A8_SINT@ specifies a four-component, 32-bit signed
-- integer format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_R8G8B8A8_SINT :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42

-- | @VK_FORMAT_R8G8B8A8_SRGB@ specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 2, and an 8-bit A component in byte 3.
pattern VK_FORMAT_R8G8B8A8_SRGB :: VkFormat
pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43

-- | @VK_FORMAT_B8G8R8A8_UNORM@ specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_B8G8R8A8_UNORM :: VkFormat
pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44

-- | @VK_FORMAT_B8G8R8A8_SNORM@ specifies a four-component, 32-bit signed
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_B8G8R8A8_SNORM :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45

-- | @VK_FORMAT_B8G8R8A8_USCALED@ specifies a four-component, 32-bit unsigned
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_B8G8R8A8_USCALED :: VkFormat
pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46

-- | @VK_FORMAT_B8G8R8A8_SSCALED@ specifies a four-component, 32-bit signed
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_B8G8R8A8_SSCALED :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47

-- | @VK_FORMAT_B8G8R8A8_UINT@ specifies a four-component, 32-bit unsigned
-- integer format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_B8G8R8A8_UINT :: VkFormat
pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48

-- | @VK_FORMAT_B8G8R8A8_SINT@ specifies a four-component, 32-bit signed
-- integer format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern VK_FORMAT_B8G8R8A8_SINT :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49

-- | @VK_FORMAT_B8G8R8A8_SRGB@ specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 2, and an 8-bit A component in byte 3.
pattern VK_FORMAT_B8G8R8A8_SRGB :: VkFormat
pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50

-- | @VK_FORMAT_A8B8G8R8_UNORM_PACK32@ specifies a four-component, 32-bit
-- packed unsigned normalized format that has an 8-bit A component in bits
-- 24..31, an 8-bit B component in bits 16..23, an 8-bit G component in
-- bits 8..15, and an 8-bit R component in bits 0..7.
pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51

-- | @VK_FORMAT_A8B8G8R8_SNORM_PACK32@ specifies a four-component, 32-bit
-- packed signed normalized format that has an 8-bit A component in bits
-- 24..31, an 8-bit B component in bits 16..23, an 8-bit G component in
-- bits 8..15, and an 8-bit R component in bits 0..7.
pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52

-- | @VK_FORMAT_A8B8G8R8_USCALED_PACK32@ specifies a four-component, 32-bit
-- packed unsigned scaled integer format that has an 8-bit A component in
-- bits 24..31, an 8-bit B component in bits 16..23, an 8-bit G component
-- in bits 8..15, and an 8-bit R component in bits 0..7.
pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53

-- | @VK_FORMAT_A8B8G8R8_SSCALED_PACK32@ specifies a four-component, 32-bit
-- packed signed scaled integer format that has an 8-bit A component in
-- bits 24..31, an 8-bit B component in bits 16..23, an 8-bit G component
-- in bits 8..15, and an 8-bit R component in bits 0..7.
pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54

-- | @VK_FORMAT_A8B8G8R8_UINT_PACK32@ specifies a four-component, 32-bit
-- packed unsigned integer format that has an 8-bit A component in bits
-- 24..31, an 8-bit B component in bits 16..23, an 8-bit G component in
-- bits 8..15, and an 8-bit R component in bits 0..7.
pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55

-- | @VK_FORMAT_A8B8G8R8_SINT_PACK32@ specifies a four-component, 32-bit
-- packed signed integer format that has an 8-bit A component in bits
-- 24..31, an 8-bit B component in bits 16..23, an 8-bit G component in
-- bits 8..15, and an 8-bit R component in bits 0..7.
pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56

-- | @VK_FORMAT_A8B8G8R8_SRGB_PACK32@ specifies a four-component, 32-bit
-- packed unsigned normalized format that has an 8-bit A component in bits
-- 24..31, an 8-bit B component stored with sRGB nonlinear encoding in bits
-- 16..23, an 8-bit G component stored with sRGB nonlinear encoding in bits
-- 8..15, and an 8-bit R component stored with sRGB nonlinear encoding in
-- bits 0..7.
pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 :: VkFormat
pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57

-- | @VK_FORMAT_A2R10G10B10_UNORM_PACK32@ specifies a four-component, 32-bit
-- packed unsigned normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58

-- | @VK_FORMAT_A2R10G10B10_SNORM_PACK32@ specifies a four-component, 32-bit
-- packed signed normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59

-- | @VK_FORMAT_A2R10G10B10_USCALED_PACK32@ specifies a four-component,
-- 32-bit packed unsigned scaled integer format that has a 2-bit A
-- component in bits 30..31, a 10-bit R component in bits 20..29, a 10-bit
-- G component in bits 10..19, and a 10-bit B component in bits 0..9.
pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60

-- | @VK_FORMAT_A2R10G10B10_SSCALED_PACK32@ specifies a four-component,
-- 32-bit packed signed scaled integer format that has a 2-bit A component
-- in bits 30..31, a 10-bit R component in bits 20..29, a 10-bit G
-- component in bits 10..19, and a 10-bit B component in bits 0..9.
pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61

-- | @VK_FORMAT_A2R10G10B10_UINT_PACK32@ specifies a four-component, 32-bit
-- packed unsigned integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62

-- | @VK_FORMAT_A2R10G10B10_SINT_PACK32@ specifies a four-component, 32-bit
-- packed signed integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63

-- | @VK_FORMAT_A2B10G10R10_UNORM_PACK32@ specifies a four-component, 32-bit
-- packed unsigned normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64

-- | @VK_FORMAT_A2B10G10R10_SNORM_PACK32@ specifies a four-component, 32-bit
-- packed signed normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65

-- | @VK_FORMAT_A2B10G10R10_USCALED_PACK32@ specifies a four-component,
-- 32-bit packed unsigned scaled integer format that has a 2-bit A
-- component in bits 30..31, a 10-bit B component in bits 20..29, a 10-bit
-- G component in bits 10..19, and a 10-bit R component in bits 0..9.
pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66

-- | @VK_FORMAT_A2B10G10R10_SSCALED_PACK32@ specifies a four-component,
-- 32-bit packed signed scaled integer format that has a 2-bit A component
-- in bits 30..31, a 10-bit B component in bits 20..29, a 10-bit G
-- component in bits 10..19, and a 10-bit R component in bits 0..9.
pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67

-- | @VK_FORMAT_A2B10G10R10_UINT_PACK32@ specifies a four-component, 32-bit
-- packed unsigned integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68

-- | @VK_FORMAT_A2B10G10R10_SINT_PACK32@ specifies a four-component, 32-bit
-- packed signed integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 :: VkFormat
pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69

-- | @VK_FORMAT_R16_UNORM@ specifies a one-component, 16-bit unsigned
-- normalized format that has a single 16-bit R component.
pattern VK_FORMAT_R16_UNORM :: VkFormat
pattern VK_FORMAT_R16_UNORM = VkFormat 70

-- | @VK_FORMAT_R16_SNORM@ specifies a one-component, 16-bit signed
-- normalized format that has a single 16-bit R component.
pattern VK_FORMAT_R16_SNORM :: VkFormat
pattern VK_FORMAT_R16_SNORM = VkFormat 71

-- | @VK_FORMAT_R16_USCALED@ specifies a one-component, 16-bit unsigned
-- scaled integer format that has a single 16-bit R component.
pattern VK_FORMAT_R16_USCALED :: VkFormat
pattern VK_FORMAT_R16_USCALED = VkFormat 72

-- | @VK_FORMAT_R16_SSCALED@ specifies a one-component, 16-bit signed scaled
-- integer format that has a single 16-bit R component.
pattern VK_FORMAT_R16_SSCALED :: VkFormat
pattern VK_FORMAT_R16_SSCALED = VkFormat 73

-- | @VK_FORMAT_R16_UINT@ specifies a one-component, 16-bit unsigned integer
-- format that has a single 16-bit R component.
pattern VK_FORMAT_R16_UINT :: VkFormat
pattern VK_FORMAT_R16_UINT = VkFormat 74

-- | @VK_FORMAT_R16_SINT@ specifies a one-component, 16-bit signed integer
-- format that has a single 16-bit R component.
pattern VK_FORMAT_R16_SINT :: VkFormat
pattern VK_FORMAT_R16_SINT = VkFormat 75

-- | @VK_FORMAT_R16_SFLOAT@ specifies a one-component, 16-bit signed
-- floating-point format that has a single 16-bit R component.
pattern VK_FORMAT_R16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16_SFLOAT = VkFormat 76

-- | @VK_FORMAT_R16G16_UNORM@ specifies a two-component, 32-bit unsigned
-- normalized format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern VK_FORMAT_R16G16_UNORM :: VkFormat
pattern VK_FORMAT_R16G16_UNORM = VkFormat 77

-- | @VK_FORMAT_R16G16_SNORM@ specifies a two-component, 32-bit signed
-- normalized format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern VK_FORMAT_R16G16_SNORM :: VkFormat
pattern VK_FORMAT_R16G16_SNORM = VkFormat 78

-- | @VK_FORMAT_R16G16_USCALED@ specifies a two-component, 32-bit unsigned
-- scaled integer format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern VK_FORMAT_R16G16_USCALED :: VkFormat
pattern VK_FORMAT_R16G16_USCALED = VkFormat 79

-- | @VK_FORMAT_R16G16_SSCALED@ specifies a two-component, 32-bit signed
-- scaled integer format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern VK_FORMAT_R16G16_SSCALED :: VkFormat
pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80

-- | @VK_FORMAT_R16G16_UINT@ specifies a two-component, 32-bit unsigned
-- integer format that has a 16-bit R component in bytes 0..1, and a 16-bit
-- G component in bytes 2..3.
pattern VK_FORMAT_R16G16_UINT :: VkFormat
pattern VK_FORMAT_R16G16_UINT = VkFormat 81

-- | @VK_FORMAT_R16G16_SINT@ specifies a two-component, 32-bit signed integer
-- format that has a 16-bit R component in bytes 0..1, and a 16-bit G
-- component in bytes 2..3.
pattern VK_FORMAT_R16G16_SINT :: VkFormat
pattern VK_FORMAT_R16G16_SINT = VkFormat 82

-- | @VK_FORMAT_R16G16_SFLOAT@ specifies a two-component, 32-bit signed
-- floating-point format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern VK_FORMAT_R16G16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83

-- | @VK_FORMAT_R16G16B16_UNORM@ specifies a three-component, 48-bit unsigned
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern VK_FORMAT_R16G16B16_UNORM :: VkFormat
pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84

-- | @VK_FORMAT_R16G16B16_SNORM@ specifies a three-component, 48-bit signed
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern VK_FORMAT_R16G16B16_SNORM :: VkFormat
pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85

-- | @VK_FORMAT_R16G16B16_USCALED@ specifies a three-component, 48-bit
-- unsigned scaled integer format that has a 16-bit R component in bytes
-- 0..1, a 16-bit G component in bytes 2..3, and a 16-bit B component in
-- bytes 4..5.
pattern VK_FORMAT_R16G16B16_USCALED :: VkFormat
pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86

-- | @VK_FORMAT_R16G16B16_SSCALED@ specifies a three-component, 48-bit signed
-- scaled integer format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, and a 16-bit B component in bytes
-- 4..5.
pattern VK_FORMAT_R16G16B16_SSCALED :: VkFormat
pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87

-- | @VK_FORMAT_R16G16B16_UINT@ specifies a three-component, 48-bit unsigned
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern VK_FORMAT_R16G16B16_UINT :: VkFormat
pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88

-- | @VK_FORMAT_R16G16B16_SINT@ specifies a three-component, 48-bit signed
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern VK_FORMAT_R16G16B16_SINT :: VkFormat
pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89

-- | @VK_FORMAT_R16G16B16_SFLOAT@ specifies a three-component, 48-bit signed
-- floating-point format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, and a 16-bit B component in bytes
-- 4..5.
pattern VK_FORMAT_R16G16B16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90

-- | @VK_FORMAT_R16G16B16A16_UNORM@ specifies a four-component, 64-bit
-- unsigned normalized format that has a 16-bit R component in bytes 0..1,
-- a 16-bit G component in bytes 2..3, a 16-bit B component in bytes 4..5,
-- and a 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_UNORM :: VkFormat
pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91

-- | @VK_FORMAT_R16G16B16A16_SNORM@ specifies a four-component, 64-bit signed
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, a 16-bit B component in bytes 4..5, and a
-- 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_SNORM :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92

-- | @VK_FORMAT_R16G16B16A16_USCALED@ specifies a four-component, 64-bit
-- unsigned scaled integer format that has a 16-bit R component in bytes
-- 0..1, a 16-bit G component in bytes 2..3, a 16-bit B component in bytes
-- 4..5, and a 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_USCALED :: VkFormat
pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93

-- | @VK_FORMAT_R16G16B16A16_SSCALED@ specifies a four-component, 64-bit
-- signed scaled integer format that has a 16-bit R component in bytes
-- 0..1, a 16-bit G component in bytes 2..3, a 16-bit B component in bytes
-- 4..5, and a 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_SSCALED :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94

-- | @VK_FORMAT_R16G16B16A16_UINT@ specifies a four-component, 64-bit
-- unsigned integer format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, a 16-bit B component in bytes 4..5,
-- and a 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_UINT :: VkFormat
pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95

-- | @VK_FORMAT_R16G16B16A16_SINT@ specifies a four-component, 64-bit signed
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, a 16-bit B component in bytes 4..5, and a
-- 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_SINT :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96

-- | @VK_FORMAT_R16G16B16A16_SFLOAT@ specifies a four-component, 64-bit
-- signed floating-point format that has a 16-bit R component in bytes
-- 0..1, a 16-bit G component in bytes 2..3, a 16-bit B component in bytes
-- 4..5, and a 16-bit A component in bytes 6..7.
pattern VK_FORMAT_R16G16B16A16_SFLOAT :: VkFormat
pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97

-- | @VK_FORMAT_R32_UINT@ specifies a one-component, 32-bit unsigned integer
-- format that has a single 32-bit R component.
pattern VK_FORMAT_R32_UINT :: VkFormat
pattern VK_FORMAT_R32_UINT = VkFormat 98

-- | @VK_FORMAT_R32_SINT@ specifies a one-component, 32-bit signed integer
-- format that has a single 32-bit R component.
pattern VK_FORMAT_R32_SINT :: VkFormat
pattern VK_FORMAT_R32_SINT = VkFormat 99

-- | @VK_FORMAT_R32_SFLOAT@ specifies a one-component, 32-bit signed
-- floating-point format that has a single 32-bit R component.
pattern VK_FORMAT_R32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32_SFLOAT = VkFormat 100

-- | @VK_FORMAT_R32G32_UINT@ specifies a two-component, 64-bit unsigned
-- integer format that has a 32-bit R component in bytes 0..3, and a 32-bit
-- G component in bytes 4..7.
pattern VK_FORMAT_R32G32_UINT :: VkFormat
pattern VK_FORMAT_R32G32_UINT = VkFormat 101

-- | @VK_FORMAT_R32G32_SINT@ specifies a two-component, 64-bit signed integer
-- format that has a 32-bit R component in bytes 0..3, and a 32-bit G
-- component in bytes 4..7.
pattern VK_FORMAT_R32G32_SINT :: VkFormat
pattern VK_FORMAT_R32G32_SINT = VkFormat 102

-- | @VK_FORMAT_R32G32_SFLOAT@ specifies a two-component, 64-bit signed
-- floating-point format that has a 32-bit R component in bytes 0..3, and a
-- 32-bit G component in bytes 4..7.
pattern VK_FORMAT_R32G32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103

-- | @VK_FORMAT_R32G32B32_UINT@ specifies a three-component, 96-bit unsigned
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, and a 32-bit B component in bytes 8..11.
pattern VK_FORMAT_R32G32B32_UINT :: VkFormat
pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104

-- | @VK_FORMAT_R32G32B32_SINT@ specifies a three-component, 96-bit signed
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, and a 32-bit B component in bytes 8..11.
pattern VK_FORMAT_R32G32B32_SINT :: VkFormat
pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105

-- | @VK_FORMAT_R32G32B32_SFLOAT@ specifies a three-component, 96-bit signed
-- floating-point format that has a 32-bit R component in bytes 0..3, a
-- 32-bit G component in bytes 4..7, and a 32-bit B component in bytes
-- 8..11.
pattern VK_FORMAT_R32G32B32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106

-- | @VK_FORMAT_R32G32B32A32_UINT@ specifies a four-component, 128-bit
-- unsigned integer format that has a 32-bit R component in bytes 0..3, a
-- 32-bit G component in bytes 4..7, a 32-bit B component in bytes 8..11,
-- and a 32-bit A component in bytes 12..15.
pattern VK_FORMAT_R32G32B32A32_UINT :: VkFormat
pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107

-- | @VK_FORMAT_R32G32B32A32_SINT@ specifies a four-component, 128-bit signed
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, a 32-bit B component in bytes 8..11, and a
-- 32-bit A component in bytes 12..15.
pattern VK_FORMAT_R32G32B32A32_SINT :: VkFormat
pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108

-- | @VK_FORMAT_R32G32B32A32_SFLOAT@ specifies a four-component, 128-bit
-- signed floating-point format that has a 32-bit R component in bytes
-- 0..3, a 32-bit G component in bytes 4..7, a 32-bit B component in bytes
-- 8..11, and a 32-bit A component in bytes 12..15.
pattern VK_FORMAT_R32G32B32A32_SFLOAT :: VkFormat
pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109

-- | @VK_FORMAT_R64_UINT@ specifies a one-component, 64-bit unsigned integer
-- format that has a single 64-bit R component.
pattern VK_FORMAT_R64_UINT :: VkFormat
pattern VK_FORMAT_R64_UINT = VkFormat 110

-- | @VK_FORMAT_R64_SINT@ specifies a one-component, 64-bit signed integer
-- format that has a single 64-bit R component.
pattern VK_FORMAT_R64_SINT :: VkFormat
pattern VK_FORMAT_R64_SINT = VkFormat 111

-- | @VK_FORMAT_R64_SFLOAT@ specifies a one-component, 64-bit signed
-- floating-point format that has a single 64-bit R component.
pattern VK_FORMAT_R64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64_SFLOAT = VkFormat 112

-- | @VK_FORMAT_R64G64_UINT@ specifies a two-component, 128-bit unsigned
-- integer format that has a 64-bit R component in bytes 0..7, and a 64-bit
-- G component in bytes 8..15.
pattern VK_FORMAT_R64G64_UINT :: VkFormat
pattern VK_FORMAT_R64G64_UINT = VkFormat 113

-- | @VK_FORMAT_R64G64_SINT@ specifies a two-component, 128-bit signed
-- integer format that has a 64-bit R component in bytes 0..7, and a 64-bit
-- G component in bytes 8..15.
pattern VK_FORMAT_R64G64_SINT :: VkFormat
pattern VK_FORMAT_R64G64_SINT = VkFormat 114

-- | @VK_FORMAT_R64G64_SFLOAT@ specifies a two-component, 128-bit signed
-- floating-point format that has a 64-bit R component in bytes 0..7, and a
-- 64-bit G component in bytes 8..15.
pattern VK_FORMAT_R64G64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115

-- | @VK_FORMAT_R64G64B64_UINT@ specifies a three-component, 192-bit unsigned
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, and a 64-bit B component in bytes 16..23.
pattern VK_FORMAT_R64G64B64_UINT :: VkFormat
pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116

-- | @VK_FORMAT_R64G64B64_SINT@ specifies a three-component, 192-bit signed
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, and a 64-bit B component in bytes 16..23.
pattern VK_FORMAT_R64G64B64_SINT :: VkFormat
pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117

-- | @VK_FORMAT_R64G64B64_SFLOAT@ specifies a three-component, 192-bit signed
-- floating-point format that has a 64-bit R component in bytes 0..7, a
-- 64-bit G component in bytes 8..15, and a 64-bit B component in bytes
-- 16..23.
pattern VK_FORMAT_R64G64B64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118

-- | @VK_FORMAT_R64G64B64A64_UINT@ specifies a four-component, 256-bit
-- unsigned integer format that has a 64-bit R component in bytes 0..7, a
-- 64-bit G component in bytes 8..15, a 64-bit B component in bytes 16..23,
-- and a 64-bit A component in bytes 24..31.
pattern VK_FORMAT_R64G64B64A64_UINT :: VkFormat
pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119

-- | @VK_FORMAT_R64G64B64A64_SINT@ specifies a four-component, 256-bit signed
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, a 64-bit B component in bytes 16..23, and a
-- 64-bit A component in bytes 24..31.
pattern VK_FORMAT_R64G64B64A64_SINT :: VkFormat
pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120

-- | @VK_FORMAT_R64G64B64A64_SFLOAT@ specifies a four-component, 256-bit
-- signed floating-point format that has a 64-bit R component in bytes
-- 0..7, a 64-bit G component in bytes 8..15, a 64-bit B component in bytes
-- 16..23, and a 64-bit A component in bytes 24..31.
pattern VK_FORMAT_R64G64B64A64_SFLOAT :: VkFormat
pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121

-- | @VK_FORMAT_B10G11R11_UFLOAT_PACK32@ specifies a three-component, 32-bit
-- packed unsigned floating-point format that has a 10-bit B component in
-- bits 22..31, an 11-bit G component in bits 11..21, an 11-bit R component
-- in bits 0..10. See
-- [{html_spec_relative}#fundamentals-fp10](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-fp10)
-- and
-- [{html_spec_relative}#fundamentals-fp11](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-fp11).
pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 :: VkFormat
pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122

-- | @VK_FORMAT_E5B9G9R9_UFLOAT_PACK32@ specifies a three-component, 32-bit
-- packed unsigned floating-point format that has a 5-bit shared exponent
-- in bits 27..31, a 9-bit B component mantissa in bits 18..26, a 9-bit G
-- component mantissa in bits 9..17, and a 9-bit R component mantissa in
-- bits 0..8.
pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 :: VkFormat
pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123

-- | @VK_FORMAT_D16_UNORM@ specifies a one-component, 16-bit unsigned
-- normalized format that has a single 16-bit depth component.
pattern VK_FORMAT_D16_UNORM :: VkFormat
pattern VK_FORMAT_D16_UNORM = VkFormat 124

-- | @VK_FORMAT_X8_D24_UNORM_PACK32@ specifies a two-component, 32-bit format
-- that has 24 unsigned normalized bits in the depth component and,
-- optionally:, 8 bits that are unused.
pattern VK_FORMAT_X8_D24_UNORM_PACK32 :: VkFormat
pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125

-- | @VK_FORMAT_D32_SFLOAT@ specifies a one-component, 32-bit signed
-- floating-point format that has 32-bits in the depth component.
pattern VK_FORMAT_D32_SFLOAT :: VkFormat
pattern VK_FORMAT_D32_SFLOAT = VkFormat 126

-- | @VK_FORMAT_S8_UINT@ specifies a one-component, 8-bit unsigned integer
-- format that has 8-bits in the stencil component.
pattern VK_FORMAT_S8_UINT :: VkFormat
pattern VK_FORMAT_S8_UINT = VkFormat 127

-- | @VK_FORMAT_D16_UNORM_S8_UINT@ specifies a two-component, 24-bit format
-- that has 16 unsigned normalized bits in the depth component and 8
-- unsigned integer bits in the stencil component.
pattern VK_FORMAT_D16_UNORM_S8_UINT :: VkFormat
pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128

-- | @VK_FORMAT_D24_UNORM_S8_UINT@ specifies a two-component, 32-bit packed
-- format that has 8 unsigned integer bits in the stencil component, and 24
-- unsigned normalized bits in the depth component.
pattern VK_FORMAT_D24_UNORM_S8_UINT :: VkFormat
pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129

-- | @VK_FORMAT_D32_SFLOAT_S8_UINT@ specifies a two-component format that has
-- 32 signed float bits in the depth component and 8 unsigned integer bits
-- in the stencil component. There are optionally: 24-bits that are unused.
pattern VK_FORMAT_D32_SFLOAT_S8_UINT :: VkFormat
pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130

-- | @VK_FORMAT_BC1_RGB_UNORM_BLOCK@ specifies a three-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data. This format has
-- no alpha and is considered opaque.
pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131

-- | @VK_FORMAT_BC1_RGB_SRGB_BLOCK@ specifies a three-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data with sRGB
-- nonlinear encoding. This format has no alpha and is considered opaque.
pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132

-- | @VK_FORMAT_BC1_RGBA_UNORM_BLOCK@ specifies a four-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data, and provides 1
-- bit of alpha.
pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133

-- | @VK_FORMAT_BC1_RGBA_SRGB_BLOCK@ specifies a four-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data with sRGB
-- nonlinear encoding, and provides 1 bit of alpha.
pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134

-- | @VK_FORMAT_BC2_UNORM_BLOCK@ specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values.
pattern VK_FORMAT_BC2_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135

-- | @VK_FORMAT_BC2_SRGB_BLOCK@ specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values with sRGB nonlinear
-- encoding.
pattern VK_FORMAT_BC2_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136

-- | @VK_FORMAT_BC3_UNORM_BLOCK@ specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values.
pattern VK_FORMAT_BC3_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137

-- | @VK_FORMAT_BC3_SRGB_BLOCK@ specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values with sRGB nonlinear
-- encoding.
pattern VK_FORMAT_BC3_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138

-- | @VK_FORMAT_BC4_UNORM_BLOCK@ specifies a one-component, block-compressed
-- format where each 64-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized red texel data.
pattern VK_FORMAT_BC4_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139

-- | @VK_FORMAT_BC4_SNORM_BLOCK@ specifies a one-component, block-compressed
-- format where each 64-bit compressed texel block encodes a 4×4 rectangle
-- of signed normalized red texel data.
pattern VK_FORMAT_BC4_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140

-- | @VK_FORMAT_BC5_UNORM_BLOCK@ specifies a two-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RG texel data with the first 64 bits encoding red
-- values followed by 64 bits encoding green values.
pattern VK_FORMAT_BC5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141

-- | @VK_FORMAT_BC5_SNORM_BLOCK@ specifies a two-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of signed normalized RG texel data with the first 64 bits encoding red
-- values followed by 64 bits encoding green values.
pattern VK_FORMAT_BC5_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142

-- | @VK_FORMAT_BC6H_UFLOAT_BLOCK@ specifies a three-component,
-- block-compressed format where each 128-bit compressed texel block
-- encodes a 4×4 rectangle of unsigned floating-point RGB texel data.
pattern VK_FORMAT_BC6H_UFLOAT_BLOCK :: VkFormat
pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143

-- | @VK_FORMAT_BC6H_SFLOAT_BLOCK@ specifies a three-component,
-- block-compressed format where each 128-bit compressed texel block
-- encodes a 4×4 rectangle of signed floating-point RGB texel data.
pattern VK_FORMAT_BC6H_SFLOAT_BLOCK :: VkFormat
pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144

-- | @VK_FORMAT_BC7_UNORM_BLOCK@ specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data.
pattern VK_FORMAT_BC7_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145

-- | @VK_FORMAT_BC7_SRGB_BLOCK@ specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern VK_FORMAT_BC7_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146

-- | @VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK@ specifies a three-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data. This format has no
-- alpha and is considered opaque.
pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147

-- | @VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK@ specifies a three-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data with sRGB nonlinear
-- encoding. This format has no alpha and is considered opaque.
pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148

-- | @VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK@ specifies a four-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data, and provides 1 bit of
-- alpha.
pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149

-- | @VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK@ specifies a four-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data with sRGB nonlinear
-- encoding, and provides 1 bit of alpha.
pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150

-- | @VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK@ specifies a four-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data with the first 64
-- bits encoding alpha values followed by 64 bits encoding RGB values.
pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151

-- | @VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK@ specifies a four-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data with the first 64
-- bits encoding alpha values followed by 64 bits encoding RGB values with
-- sRGB nonlinear encoding applied.
pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152

-- | @VK_FORMAT_EAC_R11_UNORM_BLOCK@ specifies a one-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized red texel data.
pattern VK_FORMAT_EAC_R11_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153

-- | @VK_FORMAT_EAC_R11_SNORM_BLOCK@ specifies a one-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of signed normalized red texel data.
pattern VK_FORMAT_EAC_R11_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154

-- | @VK_FORMAT_EAC_R11G11_UNORM_BLOCK@ specifies a two-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RG texel data with the first 64
-- bits encoding red values followed by 64 bits encoding green values.
pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155

-- | @VK_FORMAT_EAC_R11G11_SNORM_BLOCK@ specifies a two-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of signed normalized RG texel data with the first 64 bits
-- encoding red values followed by 64 bits encoding green values.
pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK :: VkFormat
pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156

-- | @VK_FORMAT_ASTC_4x4_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157

-- | @VK_FORMAT_ASTC_4x4_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158

-- | @VK_FORMAT_ASTC_5x4_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×4 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159

-- | @VK_FORMAT_ASTC_5x4_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×4 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160

-- | @VK_FORMAT_ASTC_5x5_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×5 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161

-- | @VK_FORMAT_ASTC_5x5_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×5 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162

-- | @VK_FORMAT_ASTC_6x5_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×5 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163

-- | @VK_FORMAT_ASTC_6x5_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×5 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164

-- | @VK_FORMAT_ASTC_6x6_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×6 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165

-- | @VK_FORMAT_ASTC_6x6_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×6 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166

-- | @VK_FORMAT_ASTC_8x5_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×5 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167

-- | @VK_FORMAT_ASTC_8x5_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×5 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168

-- | @VK_FORMAT_ASTC_8x6_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×6 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169

-- | @VK_FORMAT_ASTC_8x6_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×6 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170

-- | @VK_FORMAT_ASTC_8x8_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×8 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171

-- | @VK_FORMAT_ASTC_8x8_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×8 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172

-- | @VK_FORMAT_ASTC_10x5_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×5 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173

-- | @VK_FORMAT_ASTC_10x5_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×5 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174

-- | @VK_FORMAT_ASTC_10x6_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×6 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175

-- | @VK_FORMAT_ASTC_10x6_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×6 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176

-- | @VK_FORMAT_ASTC_10x8_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×8 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177

-- | @VK_FORMAT_ASTC_10x8_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×8 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178

-- | @VK_FORMAT_ASTC_10x10_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×10 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179

-- | @VK_FORMAT_ASTC_10x10_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×10 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180

-- | @VK_FORMAT_ASTC_12x10_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×10 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181

-- | @VK_FORMAT_ASTC_12x10_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×10 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182

-- | @VK_FORMAT_ASTC_12x12_UNORM_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×12 rectangle of unsigned normalized RGBA texel data.
pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183

-- | @VK_FORMAT_ASTC_12x12_SRGB_BLOCK@ specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×12 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK :: VkFormat
pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184
-- ** VkResult

-- | VkResult - Vulkan command return codes
--
-- = Description
--
-- -   @VK_SUCCESS@ Command successfully completed
--
-- -   @VK_NOT_READY@ A fence or query has not yet completed
--
-- -   @VK_TIMEOUT@ A wait operation has not completed in the specified
--     time
--
-- -   @VK_EVENT_SET@ An event is signaled
--
-- -   @VK_EVENT_RESET@ An event is unsignaled
--
-- -   @VK_INCOMPLETE@ A return array was too small for the result
--
-- -   @VK_SUBOPTIMAL_KHR@ A swapchain no longer matches the surface
--     properties exactly, but /can/ still be used to present to the
--     surface successfully.
--
-- -   @VK_ERROR_OUT_OF_HOST_MEMORY@ A host memory allocation has failed.
--
-- -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@ A device memory allocation has
--     failed.
--
-- -   @VK_ERROR_INITIALIZATION_FAILED@ Initialization of an object could
--     not be completed for implementation-specific reasons.
--
-- -   @VK_ERROR_DEVICE_LOST@ The logical or physical device has been lost.
--     See [Lost
--     Device](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#devsandqueues-lost-device)
--
-- -   @VK_ERROR_MEMORY_MAP_FAILED@ Mapping of a memory object has failed.
--
-- -   @VK_ERROR_LAYER_NOT_PRESENT@ A requested layer is not present or
--     could not be loaded.
--
-- -   @VK_ERROR_EXTENSION_NOT_PRESENT@ A requested extension is not
--     supported.
--
-- -   @VK_ERROR_FEATURE_NOT_PRESENT@ A requested feature is not supported.
--
-- -   @VK_ERROR_INCOMPATIBLE_DRIVER@ The requested version of Vulkan is
--     not supported by the driver or is otherwise incompatible for
--     implementation-specific reasons.
--
-- -   @VK_ERROR_TOO_MANY_OBJECTS@ Too many objects of the type have
--     already been created.
--
-- -   @VK_ERROR_FORMAT_NOT_SUPPORTED@ A requested format is not supported
--     on this device.
--
-- -   @VK_ERROR_FRAGMENTED_POOL@ A pool allocation has failed due to
--     fragmentation of the pool’s memory. This /must/ only be returned if
--     no attempt to allocate host or device memory was made to accomodate
--     the new allocation. This /should/ be returned in preference to
--     @VK_ERROR_OUT_OF_POOL_MEMORY@, but only if the implementation is
--     certain that the pool allocation failure was due to fragmentation.
--
-- -   @VK_ERROR_SURFACE_LOST_KHR@ A surface is no longer available.
--
-- -   @VK_ERROR_NATIVE_WINDOW_IN_USE_KHR@ The requested window is already
--     in use by Vulkan or another API in a manner which prevents it from
--     being used again.
--
-- -   @VK_ERROR_OUT_OF_DATE_KHR@ A surface has changed in such a way that
--     it is no longer compatible with the swapchain, and further
--     presentation requests using the swapchain will fail. Applications
--     /must/ query the new surface properties and recreate their swapchain
--     if they wish to continue presenting to the surface.
--
-- -   @VK_ERROR_INCOMPATIBLE_DISPLAY_KHR@ The display used by a swapchain
--     does not use the same presentable image layout, or is incompatible
--     in a way that prevents sharing an image.
--
-- -   @VK_ERROR_INVALID_SHADER_NV@ One or more shaders failed to compile
--     or link. More details are reported back to the application via
--     @{html_spec_relative}#VK_EXT_debug_report@ if enabled.
--
-- -   @VK_ERROR_OUT_OF_POOL_MEMORY@ A pool memory allocation has failed.
--     This /must/ only be returned if no attempt to allocate host or
--     device memory was made to accomodate the new allocation. If the
--     failure was definitely due to fragmentation of the pool,
--     @VK_ERROR_FRAGMENTED_POOL@ /should/ be returned instead.
--
-- -   @VK_ERROR_INVALID_EXTERNAL_HANDLE@ An external handle is not a valid
--     handle of the specified type.
--
-- -   @VK_ERROR_FRAGMENTATION_EXT@ A descriptor pool creation has failed
--     due to fragmentation.
--
-- If a command returns a run time error, unless otherwise specified any
-- output parameters will have undefined contents, except that if the
-- output parameter is a structure with @sType@ and @pNext@ fields, those
-- fields will be unmodified. Any structures chained from @pNext@ will also
-- have undefined contents, except that @sType@ and @pNext@ will be
-- unmodified.
--
-- Out of memory errors do not damage any currently existing Vulkan
-- objects. Objects that have already been successfully created /can/ still
-- be used by the application.
--
-- Performance-critical commands generally do not have return codes. If a
-- run time error occurs in such commands, the implementation will defer
-- reporting the error until a specified point. For commands that record
-- into command buffers (@vkCmd*@) run time errors are reported by
-- @vkEndCommandBuffer@.
--
-- = See Also
--
-- No cross-references are available,
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkPresentInfoKHR'
newtype VkResult = VkResult Int32
  deriving (Eq, Ord, Storable)

instance Show VkResult where
  showsPrec _ VK_SUCCESS = showString "VK_SUCCESS"
  showsPrec _ VK_NOT_READY = showString "VK_NOT_READY"
  showsPrec _ VK_TIMEOUT = showString "VK_TIMEOUT"
  showsPrec _ VK_EVENT_SET = showString "VK_EVENT_SET"
  showsPrec _ VK_EVENT_RESET = showString "VK_EVENT_RESET"
  showsPrec _ VK_INCOMPLETE = showString "VK_INCOMPLETE"
  showsPrec _ VK_ERROR_OUT_OF_HOST_MEMORY = showString "VK_ERROR_OUT_OF_HOST_MEMORY"
  showsPrec _ VK_ERROR_OUT_OF_DEVICE_MEMORY = showString "VK_ERROR_OUT_OF_DEVICE_MEMORY"
  showsPrec _ VK_ERROR_INITIALIZATION_FAILED = showString "VK_ERROR_INITIALIZATION_FAILED"
  showsPrec _ VK_ERROR_DEVICE_LOST = showString "VK_ERROR_DEVICE_LOST"
  showsPrec _ VK_ERROR_MEMORY_MAP_FAILED = showString "VK_ERROR_MEMORY_MAP_FAILED"
  showsPrec _ VK_ERROR_LAYER_NOT_PRESENT = showString "VK_ERROR_LAYER_NOT_PRESENT"
  showsPrec _ VK_ERROR_EXTENSION_NOT_PRESENT = showString "VK_ERROR_EXTENSION_NOT_PRESENT"
  showsPrec _ VK_ERROR_FEATURE_NOT_PRESENT = showString "VK_ERROR_FEATURE_NOT_PRESENT"
  showsPrec _ VK_ERROR_INCOMPATIBLE_DRIVER = showString "VK_ERROR_INCOMPATIBLE_DRIVER"
  showsPrec _ VK_ERROR_TOO_MANY_OBJECTS = showString "VK_ERROR_TOO_MANY_OBJECTS"
  showsPrec _ VK_ERROR_FORMAT_NOT_SUPPORTED = showString "VK_ERROR_FORMAT_NOT_SUPPORTED"
  showsPrec _ VK_ERROR_FRAGMENTED_POOL = showString "VK_ERROR_FRAGMENTED_POOL"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkResult (-1000069000)) = showString "VK_ERROR_OUT_OF_POOL_MEMORY"
  showsPrec _ (VkResult (-1000072003)) = showString "VK_ERROR_INVALID_EXTERNAL_HANDLE"
  showsPrec _ (VkResult (-1000000000)) = showString "VK_ERROR_SURFACE_LOST_KHR"
  showsPrec _ (VkResult (-1000000001)) = showString "VK_ERROR_NATIVE_WINDOW_IN_USE_KHR"
  showsPrec _ (VkResult 1000001003) = showString "VK_SUBOPTIMAL_KHR"
  showsPrec _ (VkResult (-1000001004)) = showString "VK_ERROR_OUT_OF_DATE_KHR"
  showsPrec _ (VkResult (-1000003001)) = showString "VK_ERROR_INCOMPATIBLE_DISPLAY_KHR"
  showsPrec _ (VkResult (-1000011001)) = showString "VK_ERROR_VALIDATION_FAILED_EXT"
  showsPrec _ (VkResult (-1000012000)) = showString "VK_ERROR_INVALID_SHADER_NV"
  showsPrec _ (VkResult (-1000161000)) = showString "VK_ERROR_FRAGMENTATION_EXT"
  showsPrec _ (VkResult (-1000174001)) = showString "VK_ERROR_NOT_PERMITTED_EXT"
  showsPrec p (VkResult x) = showParen (p >= 11) (showString "VkResult " . showsPrec 11 x)

instance Read VkResult where
  readPrec = parens ( choose [ ("VK_SUCCESS",                     pure VK_SUCCESS)
                             , ("VK_NOT_READY",                   pure VK_NOT_READY)
                             , ("VK_TIMEOUT",                     pure VK_TIMEOUT)
                             , ("VK_EVENT_SET",                   pure VK_EVENT_SET)
                             , ("VK_EVENT_RESET",                 pure VK_EVENT_RESET)
                             , ("VK_INCOMPLETE",                  pure VK_INCOMPLETE)
                             , ("VK_ERROR_OUT_OF_HOST_MEMORY",    pure VK_ERROR_OUT_OF_HOST_MEMORY)
                             , ("VK_ERROR_OUT_OF_DEVICE_MEMORY",  pure VK_ERROR_OUT_OF_DEVICE_MEMORY)
                             , ("VK_ERROR_INITIALIZATION_FAILED", pure VK_ERROR_INITIALIZATION_FAILED)
                             , ("VK_ERROR_DEVICE_LOST",           pure VK_ERROR_DEVICE_LOST)
                             , ("VK_ERROR_MEMORY_MAP_FAILED",     pure VK_ERROR_MEMORY_MAP_FAILED)
                             , ("VK_ERROR_LAYER_NOT_PRESENT",     pure VK_ERROR_LAYER_NOT_PRESENT)
                             , ("VK_ERROR_EXTENSION_NOT_PRESENT", pure VK_ERROR_EXTENSION_NOT_PRESENT)
                             , ("VK_ERROR_FEATURE_NOT_PRESENT",   pure VK_ERROR_FEATURE_NOT_PRESENT)
                             , ("VK_ERROR_INCOMPATIBLE_DRIVER",   pure VK_ERROR_INCOMPATIBLE_DRIVER)
                             , ("VK_ERROR_TOO_MANY_OBJECTS",      pure VK_ERROR_TOO_MANY_OBJECTS)
                             , ("VK_ERROR_FORMAT_NOT_SUPPORTED",  pure VK_ERROR_FORMAT_NOT_SUPPORTED)
                             , ("VK_ERROR_FRAGMENTED_POOL",       pure VK_ERROR_FRAGMENTED_POOL)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_ERROR_OUT_OF_POOL_MEMORY",       pure (VkResult (-1000069000)))
                             , ("VK_ERROR_INVALID_EXTERNAL_HANDLE",  pure (VkResult (-1000072003)))
                             , ("VK_ERROR_SURFACE_LOST_KHR",         pure (VkResult (-1000000000)))
                             , ("VK_ERROR_NATIVE_WINDOW_IN_USE_KHR", pure (VkResult (-1000000001)))
                             , ("VK_SUBOPTIMAL_KHR",                 pure (VkResult 1000001003))
                             , ("VK_ERROR_OUT_OF_DATE_KHR",          pure (VkResult (-1000001004)))
                             , ("VK_ERROR_INCOMPATIBLE_DISPLAY_KHR", pure (VkResult (-1000003001)))
                             , ("VK_ERROR_VALIDATION_FAILED_EXT",    pure (VkResult (-1000011001)))
                             , ("VK_ERROR_INVALID_SHADER_NV",        pure (VkResult (-1000012000)))
                             , ("VK_ERROR_FRAGMENTATION_EXT",        pure (VkResult (-1000161000)))
                             , ("VK_ERROR_NOT_PERMITTED_EXT",        pure (VkResult (-1000174001)))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkResult")
                        v <- step readPrec
                        pure (VkResult v)
                        )
                    )

-- No documentation found for Nested "VkResult" "VK_SUCCESS"
pattern VK_SUCCESS :: VkResult
pattern VK_SUCCESS = VkResult 0

-- No documentation found for Nested "VkResult" "VK_NOT_READY"
pattern VK_NOT_READY :: VkResult
pattern VK_NOT_READY = VkResult 1

-- No documentation found for Nested "VkResult" "VK_TIMEOUT"
pattern VK_TIMEOUT :: VkResult
pattern VK_TIMEOUT = VkResult 2

-- No documentation found for Nested "VkResult" "VK_EVENT_SET"
pattern VK_EVENT_SET :: VkResult
pattern VK_EVENT_SET = VkResult 3

-- No documentation found for Nested "VkResult" "VK_EVENT_RESET"
pattern VK_EVENT_RESET :: VkResult
pattern VK_EVENT_RESET = VkResult 4

-- No documentation found for Nested "VkResult" "VK_INCOMPLETE"
pattern VK_INCOMPLETE :: VkResult
pattern VK_INCOMPLETE = VkResult 5

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_HOST_MEMORY"
pattern VK_ERROR_OUT_OF_HOST_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)

-- No documentation found for Nested "VkResult" "VK_ERROR_OUT_OF_DEVICE_MEMORY"
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY :: VkResult
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)

-- No documentation found for Nested "VkResult" "VK_ERROR_INITIALIZATION_FAILED"
pattern VK_ERROR_INITIALIZATION_FAILED :: VkResult
pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)

-- No documentation found for Nested "VkResult" "VK_ERROR_DEVICE_LOST"
pattern VK_ERROR_DEVICE_LOST :: VkResult
pattern VK_ERROR_DEVICE_LOST = VkResult (-4)

-- No documentation found for Nested "VkResult" "VK_ERROR_MEMORY_MAP_FAILED"
pattern VK_ERROR_MEMORY_MAP_FAILED :: VkResult
pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)

-- No documentation found for Nested "VkResult" "VK_ERROR_LAYER_NOT_PRESENT"
pattern VK_ERROR_LAYER_NOT_PRESENT :: VkResult
pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)

-- No documentation found for Nested "VkResult" "VK_ERROR_EXTENSION_NOT_PRESENT"
pattern VK_ERROR_EXTENSION_NOT_PRESENT :: VkResult
pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)

-- No documentation found for Nested "VkResult" "VK_ERROR_FEATURE_NOT_PRESENT"
pattern VK_ERROR_FEATURE_NOT_PRESENT :: VkResult
pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)

-- No documentation found for Nested "VkResult" "VK_ERROR_INCOMPATIBLE_DRIVER"
pattern VK_ERROR_INCOMPATIBLE_DRIVER :: VkResult
pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)

-- No documentation found for Nested "VkResult" "VK_ERROR_TOO_MANY_OBJECTS"
pattern VK_ERROR_TOO_MANY_OBJECTS :: VkResult
pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)

-- No documentation found for Nested "VkResult" "VK_ERROR_FORMAT_NOT_SUPPORTED"
pattern VK_ERROR_FORMAT_NOT_SUPPORTED :: VkResult
pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)

-- No documentation found for Nested "VkResult" "VK_ERROR_FRAGMENTED_POOL"
pattern VK_ERROR_FRAGMENTED_POOL :: VkResult
pattern VK_ERROR_FRAGMENTED_POOL = VkResult (-12)
-- ** VkStructureType

-- | VkStructureType - Vulkan structure types (@stype@)
--
-- = Description
--
-- Each value corresponds to a particular structure with a @sType@ member
-- with a matching name. As a general rule, the name of each
-- @VkStructureType@ value is obtained by taking the name of the structure,
-- stripping the leading @Vk@, prefixing each capital letter with @_@,
-- converting the entire resulting string to upper case, and prefixing it
-- with @VK_STRUCTURE_TYPE_@. For example, structures of type
-- @VkImageCreateInfo@ correspond to a @VkStructureType@ of
-- @VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO@, and thus its @sType@ member
-- /must/ equal that when it is passed to the API.
--
-- The values @VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO@ and
-- @VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO@ are reserved for internal
-- use by the loader, and do not have corresponding Vulkan structures in
-- this Specification.
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkAcquireNextImageInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferFormatPropertiesANDROID',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferPropertiesANDROID',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkAndroidHardwareBufferUsageANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_android_surface.VkAndroidSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkApplicationInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkBindImageMemorySwapchainInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo',
-- 'Graphics.Vulkan.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkCmdProcessCommandsInfoNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkCmdReserveSpaceForCommandsInfoNVX',
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferBeginInfo',
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.Core10.CommandPool.VkCommandPoolCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkCopyDescriptorSet',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkD3D12FenceSubmitInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.VkDebugMarkerMarkerInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectNameInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_marker.VkDebugMarkerObjectTagInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportCallbackCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsLabelEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCallbackDataEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsMessengerCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsObjectNameInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsObjectTagInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationBufferCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationImageCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation.VkDedicatedAllocationMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorPoolCreateInfo',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetLayoutBindingFlagsCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.VkDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetVariableDescriptorCountAllocateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkDescriptorSetVariableDescriptorCountLayoutSupportEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.Core10.Device.VkDeviceCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.VkDeviceEventInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsFeaturesNVX',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkDeviceGeneratedCommandsLimitsNVX',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupBindSparseInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupCommandBufferBeginInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentCapabilitiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkDeviceGroupPresentInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupSubmitInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkDeviceGroupSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.Device.VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_global_priority.VkDeviceQueueGlobalPriorityCreateInfoEXT',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.VkDisplayEventInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayModeCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.VkDisplayPowerInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display_swapchain.VkDisplayPresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplaySurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.Event.VkEventCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkExportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExportMemoryAllocateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkExportMemoryWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.VkExportMemoryWin32HandleInfoNV',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkExportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkExternalFormatANDROID',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory.VkExternalMemoryImageCreateInfoNV',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties',
-- 'Graphics.Vulkan.Core10.Fence.VkFenceCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.VkFenceGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkFenceGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2',
-- 'Graphics.Vulkan.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_hdr_metadata.VkHdrMetadataEXT',
-- 'Graphics.Vulkan.Extensions.VK_MVK_ios_surface.VkIOSSurfaceCreateInfoMVK',
-- 'Graphics.Vulkan.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_image_format_list.VkImageFormatListCreateInfoKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkImageSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkImportAndroidHardwareBufferInfoANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_fd.VkImportFenceFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_fence_win32.VkImportFenceWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkImportMemoryFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkImportMemoryHostPointerInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkImportMemoryWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_external_memory_win32.VkImportMemoryWin32HandleInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.VkImportSemaphoreFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkImportSemaphoreWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutCreateInfoNVX',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstanceCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_MVK_macos_surface.VkMacOSSurfaceCreateInfoMVK',
-- 'Graphics.Vulkan.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo',
-- 'Graphics.Vulkan.Core10.Memory.VkMemoryAllocateInfo',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkMemoryFdPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.VkMemoryGetAndroidHardwareBufferInfoANDROID',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_fd.VkMemoryGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkMemoryGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkMemoryHostPointerPropertiesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_memory_win32.VkMemoryWin32HandlePropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_mir_surface.VkMirSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkMultisamplePropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableCreateInfoNVX',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_16bit_storage.VkPhysicalDevice16BitStorageFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPhysicalDeviceBlendOperationAdvancedPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization.VkPhysicalDeviceConservativeRasterizationPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingFeaturesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_descriptor_indexing.VkPhysicalDeviceDescriptorIndexingPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.VkPhysicalDeviceDiscardRectanglePropertiesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkPhysicalDeviceExternalFenceInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_external_memory_host.VkPhysicalDeviceExternalMemoryHostPropertiesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkPhysicalDeviceExternalSemaphoreInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceIDProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures',
-- 'Graphics.Vulkan.Extensions.VK_NVX_multiview_per_view_attributes.VkPhysicalDeviceMultiviewPerViewAttributesPropertiesNVX',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_push_descriptor.VkPhysicalDevicePushDescriptorPropertiesKHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPhysicalDeviceSampleLocationsPropertiesEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkPhysicalDeviceSamplerFilterMinmaxPropertiesEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkPhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Graphics.Vulkan.Extensions.VK_AMD_shader_core_properties.VkPhysicalDeviceShaderCorePropertiesAMD',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_shader_draw_parameters.VkPhysicalDeviceShaderDrawParameterFeatures',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.VkPhysicalDeviceSurfaceInfo2KHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointerFeatures',
-- 'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VkPhysicalDeviceVertexAttributeDivisorPropertiesEXT',
-- 'Graphics.Vulkan.Core10.PipelineCache.VkPipelineCacheCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_blend_operation_advanced.VkPipelineColorBlendAdvancedStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_fragment_coverage_to_color.VkPipelineCoverageToColorStateCreateInfoNV',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_discard_rectangles.VkPipelineDiscardRectangleStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineDynamicStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo',
-- 'Graphics.Vulkan.Core10.PipelineLayout.VkPipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_AMD_rasterization_order.VkPipelineRasterizationStateRasterizationOrderAMD',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkPipelineSampleLocationsStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkPipelineTessellationDomainOriginStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineTessellationStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_vertex_attribute_divisor.VkPipelineVertexInputDivisorStateCreateInfoEXT',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo',
-- 'Graphics.Vulkan.Core10.Pipeline.VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_viewport_swizzle.VkPipelineViewportSwizzleStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_NV_clip_space_w_scaling.VkPipelineViewportWScalingStateCreateInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkPresentInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_incremental_present.VkPresentRegionsKHR',
-- 'Graphics.Vulkan.Extensions.VK_GOOGLE_display_timing.VkPresentTimesInfoGOOGLE',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_protected_memory.VkProtectedSubmitInfo',
-- 'Graphics.Vulkan.Core10.Query.VkQueryPoolCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.Core10.Pass.VkRenderPassCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkRenderPassSampleLocationsBeginInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sample_locations.VkSampleLocationsInfoEXT',
-- 'Graphics.Vulkan.Core10.Sampler.VkSamplerCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeCreateInfoEXT',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionImageFormatProperties',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo',
-- 'Graphics.Vulkan.Core10.QueueSemaphore.VkSemaphoreCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_fd.VkSemaphoreGetFdInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_external_semaphore_win32.VkSemaphoreGetWin32HandleInfoKHR',
-- 'Graphics.Vulkan.Core10.Shader.VkShaderModuleCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.VkShaderModuleValidationCacheCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image.VkSharedPresentSurfaceCapabilitiesKHR',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2',
-- 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2',
-- 'Graphics.Vulkan.Core10.Queue.VkSubmitInfo',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_surface_counter.VkSurfaceCapabilities2EXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceCapabilities2KHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_get_surface_capabilities2.VkSurfaceFormat2KHR',
-- 'Graphics.Vulkan.Extensions.VK_EXT_display_control.VkSwapchainCounterCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_AMD_texture_gather_bias_lod.VkTextureLODGatherFormatPropertiesAMD',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.VkValidationCacheCreateInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_validation_flags.VkValidationFlagsEXT',
-- 'Graphics.Vulkan.Extensions.VK_NN_vi_surface.VkViSurfaceCreateInfoNN',
-- 'Graphics.Vulkan.Extensions.VK_KHR_wayland_surface.VkWaylandSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_NV_win32_keyed_mutex.VkWin32KeyedMutexAcquireReleaseInfoNV',
-- 'Graphics.Vulkan.Extensions.VK_KHR_win32_surface.VkWin32SurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Core10.DescriptorSet.VkWriteDescriptorSet',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xcb_surface.VkXcbSurfaceCreateInfoKHR',
-- 'Graphics.Vulkan.Extensions.VK_KHR_xlib_surface.VkXlibSurfaceCreateInfoKHR'
newtype VkStructureType = VkStructureType Int32
  deriving (Eq, Ord, Storable)

instance Show VkStructureType where
  showsPrec _ VK_STRUCTURE_TYPE_APPLICATION_INFO = showString "VK_STRUCTURE_TYPE_APPLICATION_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SUBMIT_INFO = showString "VK_STRUCTURE_TYPE_SUBMIT_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = showString "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = showString "VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
  showsPrec _ VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = showString "VK_STRUCTURE_TYPE_BIND_SPARSE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_FENCE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = showString "VK_STRUCTURE_TYPE_EVENT_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = showString "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = showString "VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = showString "VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = showString "VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = showString "VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = showString "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
  showsPrec _ VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = showString "VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
  showsPrec _ VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = showString "VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = showString "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = showString "VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = showString "VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = showString "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
  showsPrec _ VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = showString "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
  showsPrec _ VK_STRUCTURE_TYPE_MEMORY_BARRIER = showString "VK_STRUCTURE_TYPE_MEMORY_BARRIER"
  showsPrec _ VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
  showsPrec _ VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = showString "VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkStructureType 1000094000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES"
  showsPrec _ (VkStructureType 1000157000) = showString "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
  showsPrec _ (VkStructureType 1000157001) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
  showsPrec _ (VkStructureType 1000083000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES"
  showsPrec _ (VkStructureType 1000127000) = showString "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
  showsPrec _ (VkStructureType 1000127001) = showString "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
  showsPrec _ (VkStructureType 1000060000) = showString "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO"
  showsPrec _ (VkStructureType 1000060003) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO"
  showsPrec _ (VkStructureType 1000060004) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO"
  showsPrec _ (VkStructureType 1000060005) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO"
  showsPrec _ (VkStructureType 1000060006) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO"
  showsPrec _ (VkStructureType 1000060013) = showString "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO"
  showsPrec _ (VkStructureType 1000060014) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO"
  showsPrec _ (VkStructureType 1000070000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES"
  showsPrec _ (VkStructureType 1000070001) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000146000) = showString "VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2"
  showsPrec _ (VkStructureType 1000146001) = showString "VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2"
  showsPrec _ (VkStructureType 1000146002) = showString "VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2"
  showsPrec _ (VkStructureType 1000146003) = showString "VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2"
  showsPrec _ (VkStructureType 1000146004) = showString "VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2"
  showsPrec _ (VkStructureType 1000059000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2"
  showsPrec _ (VkStructureType 1000059001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059002) = showString "VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059003) = showString "VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059004) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2"
  showsPrec _ (VkStructureType 1000059005) = showString "VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059006) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059007) = showString "VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2"
  showsPrec _ (VkStructureType 1000059008) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2"
  showsPrec _ (VkStructureType 1000117000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES"
  showsPrec _ (VkStructureType 1000117001) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO"
  showsPrec _ (VkStructureType 1000117002) = showString "VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000117003) = showString "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000053000) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO"
  showsPrec _ (VkStructureType 1000053001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES"
  showsPrec _ (VkStructureType 1000053002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES"
  showsPrec _ (VkStructureType 1000120000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES"
  showsPrec _ (VkStructureType 1000145000) = showString "VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO"
  showsPrec _ (VkStructureType 1000145001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES"
  showsPrec _ (VkStructureType 1000145002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES"
  showsPrec _ (VkStructureType 1000145003) = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2"
  showsPrec _ (VkStructureType 1000156000) = showString "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO"
  showsPrec _ (VkStructureType 1000156001) = showString "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO"
  showsPrec _ (VkStructureType 1000156002) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO"
  showsPrec _ (VkStructureType 1000156003) = showString "VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO"
  showsPrec _ (VkStructureType 1000156004) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES"
  showsPrec _ (VkStructureType 1000156005) = showString "VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES"
  showsPrec _ (VkStructureType 1000085000) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000071000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO"
  showsPrec _ (VkStructureType 1000071001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES"
  showsPrec _ (VkStructureType 1000071002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO"
  showsPrec _ (VkStructureType 1000071003) = showString "VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES"
  showsPrec _ (VkStructureType 1000071004) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES"
  showsPrec _ (VkStructureType 1000072000) = showString "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO"
  showsPrec _ (VkStructureType 1000072001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000072002) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO"
  showsPrec _ (VkStructureType 1000112000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO"
  showsPrec _ (VkStructureType 1000112001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES"
  showsPrec _ (VkStructureType 1000113000) = showString "VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000077000) = showString "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO"
  showsPrec _ (VkStructureType 1000076000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO"
  showsPrec _ (VkStructureType 1000076001) = showString "VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES"
  showsPrec _ (VkStructureType 1000168000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES"
  showsPrec _ (VkStructureType 1000168001) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT"
  showsPrec _ (VkStructureType 1000063000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES"
  showsPrec _ (VkStructureType 1000001000) = showString "VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000001001) = showString "VK_STRUCTURE_TYPE_PRESENT_INFO_KHR"
  showsPrec _ (VkStructureType 1000060007) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR"
  showsPrec _ (VkStructureType 1000060008) = showString "VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000060009) = showString "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR"
  showsPrec _ (VkStructureType 1000060010) = showString "VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR"
  showsPrec _ (VkStructureType 1000060011) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR"
  showsPrec _ (VkStructureType 1000060012) = showString "VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000002000) = showString "VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000002001) = showString "VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000003000) = showString "VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR"
  showsPrec _ (VkStructureType 1000004000) = showString "VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000005000) = showString "VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000006000) = showString "VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000007000) = showString "VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000008000) = showString "VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000009000) = showString "VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000010000) = showString "VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID"
  showsPrec _ (VkStructureType 1000011000) = showString "VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000018000) = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD"
  showsPrec _ (VkStructureType 1000022000) = showString "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT"
  showsPrec _ (VkStructureType 1000022001) = showString "VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT"
  showsPrec _ (VkStructureType 1000022002) = showString "VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT"
  showsPrec _ (VkStructureType 1000026000) = showString "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000026001) = showString "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000026002) = showString "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
  showsPrec _ (VkStructureType 1000041000) = showString "VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD"
  showsPrec _ (VkStructureType 1000056000) = showString "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000056001) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV"
  showsPrec _ (VkStructureType 1000057000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV"
  showsPrec _ (VkStructureType 1000057001) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV"
  showsPrec _ (VkStructureType 1000058000) = showString "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV"
  showsPrec _ (VkStructureType 1000061000) = showString "VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT"
  showsPrec _ (VkStructureType 1000062000) = showString "VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN"
  showsPrec _ (VkStructureType 1000073000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000073001) = showString "VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000073002) = showString "VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000073003) = showString "VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000074000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000074001) = showString "VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000074002) = showString "VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000075000) = showString "VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR"
  showsPrec _ (VkStructureType 1000078000) = showString "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000078001) = showString "VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000078002) = showString "VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR"
  showsPrec _ (VkStructureType 1000078003) = showString "VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000079000) = showString "VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000079001) = showString "VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000080000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR"
  showsPrec _ (VkStructureType 1000084000) = showString "VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR"
  showsPrec _ (VkStructureType 1000086000) = showString "VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX"
  showsPrec _ (VkStructureType 1000086001) = showString "VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX"
  showsPrec _ (VkStructureType 1000086002) = showString "VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX"
  showsPrec _ (VkStructureType 1000086003) = showString "VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX"
  showsPrec _ (VkStructureType 1000086004) = showString "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX"
  showsPrec _ (VkStructureType 1000086005) = showString "VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX"
  showsPrec _ (VkStructureType 1000087000) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000090000) = showString "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT"
  showsPrec _ (VkStructureType 1000091000) = showString "VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT"
  showsPrec _ (VkStructureType 1000091001) = showString "VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT"
  showsPrec _ (VkStructureType 1000091002) = showString "VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT"
  showsPrec _ (VkStructureType 1000091003) = showString "VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000092000) = showString "VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE"
  showsPrec _ (VkStructureType 1000097000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX"
  showsPrec _ (VkStructureType 1000098000) = showString "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000099000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000099001) = showString "VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000101000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000101001) = showString "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000105000) = showString "VK_STRUCTURE_TYPE_HDR_METADATA_EXT"
  showsPrec _ (VkStructureType 1000111000) = showString "VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR"
  showsPrec _ (VkStructureType 1000114000) = showString "VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000114001) = showString "VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000114002) = showString "VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR"
  showsPrec _ (VkStructureType 1000115000) = showString "VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000115001) = showString "VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR"
  showsPrec _ (VkStructureType 1000119000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR"
  showsPrec _ (VkStructureType 1000119001) = showString "VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR"
  showsPrec _ (VkStructureType 1000119002) = showString "VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR"
  showsPrec _ (VkStructureType 1000122000) = showString "VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK"
  showsPrec _ (VkStructureType 1000123000) = showString "VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK"
  showsPrec _ (VkStructureType 1000128000) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT"
  showsPrec _ (VkStructureType 1000128001) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT"
  showsPrec _ (VkStructureType 1000128002) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT"
  showsPrec _ (VkStructureType 1000128003) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT"
  showsPrec _ (VkStructureType 1000128004) = showString "VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000129000) = showString "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID"
  showsPrec _ (VkStructureType 1000129001) = showString "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID"
  showsPrec _ (VkStructureType 1000129002) = showString "VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID"
  showsPrec _ (VkStructureType 1000129003) = showString "VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
  showsPrec _ (VkStructureType 1000129004) = showString "VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID"
  showsPrec _ (VkStructureType 1000129005) = showString "VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID"
  showsPrec _ (VkStructureType 1000130000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000130001) = showString "VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000143000) = showString "VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT"
  showsPrec _ (VkStructureType 1000143001) = showString "VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT"
  showsPrec _ (VkStructureType 1000143002) = showString "VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000143003) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000143004) = showString "VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000147000) = showString "VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR"
  showsPrec _ (VkStructureType 1000148000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000148001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000148002) = showString "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000149000) = showString "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000152000) = showString "VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV"
  showsPrec _ (VkStructureType 1000160000) = showString "VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000160001) = showString "VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000161000) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000161001) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT"
  showsPrec _ (VkStructureType 1000161002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000161003) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000161004) = showString "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT"
  showsPrec _ (VkStructureType 1000174000) = showString "VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT"
  showsPrec _ (VkStructureType 1000178000) = showString "VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT"
  showsPrec _ (VkStructureType 1000178001) = showString "VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000178002) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000185000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD"
  showsPrec _ (VkStructureType 1000190000) = showString "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT"
  showsPrec _ (VkStructureType 1000190001) = showString "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT"
  showsPrec p (VkStructureType x) = showParen (p >= 11) (showString "VkStructureType " . showsPrec 11 x)

instance Read VkStructureType where
  readPrec = parens ( choose [ ("VK_STRUCTURE_TYPE_APPLICATION_INFO",                          pure VK_STRUCTURE_TYPE_APPLICATION_INFO)
                             , ("VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO",                      pure VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO",                  pure VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO",                        pure VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SUBMIT_INFO",                               pure VK_STRUCTURE_TYPE_SUBMIT_INFO)
                             , ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO",                      pure VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE",                       pure VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE)
                             , ("VK_STRUCTURE_TYPE_BIND_SPARSE_INFO",                          pure VK_STRUCTURE_TYPE_BIND_SPARSE_INFO)
                             , ("VK_STRUCTURE_TYPE_FENCE_CREATE_INFO",                         pure VK_STRUCTURE_TYPE_FENCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO",                     pure VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_EVENT_CREATE_INFO",                         pure VK_STRUCTURE_TYPE_EVENT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO",                    pure VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO",                        pure VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO",                   pure VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO",                         pure VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO",                    pure VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO",                 pure VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO",                pure VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO",         pure VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO",   pure VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO",   pure VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO",       pure VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO",  pure VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO",    pure VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO",  pure VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO",    pure VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO",        pure VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO",             pure VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO",              pure VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO",               pure VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO",                       pure VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO",         pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO",               pure VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO",              pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET",                      pure VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
                             , ("VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET",                       pure VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
                             , ("VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO",                   pure VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO",                   pure VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO",                  pure VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO",              pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO",           pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO",                 pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO",                    pure VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER",                     pure VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER",                      pure VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_MEMORY_BARRIER",                            pure VK_STRUCTURE_TYPE_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO",               pure VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO",                 pure VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SUBGROUP_PROPERTIES",                          pure (VkStructureType 1000094000))
                             , ("VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO",                                      pure (VkStructureType 1000157000))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO",                                       pure (VkStructureType 1000157001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_16BIT_STORAGE_FEATURES",                       pure (VkStructureType 1000083000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS",                                pure (VkStructureType 1000127000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO",                               pure (VkStructureType 1000127001))
                             , ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_FLAGS_INFO",                                   pure (VkStructureType 1000060000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_RENDER_PASS_BEGIN_INFO",                          pure (VkStructureType 1000060003))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_COMMAND_BUFFER_BEGIN_INFO",                       pure (VkStructureType 1000060004))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_SUBMIT_INFO",                                     pure (VkStructureType 1000060005))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_BIND_SPARSE_INFO",                                pure (VkStructureType 1000060006))
                             , ("VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_DEVICE_GROUP_INFO",                         pure (VkStructureType 1000060013))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_DEVICE_GROUP_INFO",                          pure (VkStructureType 1000060014))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES",                             pure (VkStructureType 1000070000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO",                              pure (VkStructureType 1000070001))
                             , ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_REQUIREMENTS_INFO_2",                            pure (VkStructureType 1000146000))
                             , ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_REQUIREMENTS_INFO_2",                             pure (VkStructureType 1000146001))
                             , ("VK_STRUCTURE_TYPE_IMAGE_SPARSE_MEMORY_REQUIREMENTS_INFO_2",                      pure (VkStructureType 1000146002))
                             , ("VK_STRUCTURE_TYPE_MEMORY_REQUIREMENTS_2",                                        pure (VkStructureType 1000146003))
                             , ("VK_STRUCTURE_TYPE_SPARSE_IMAGE_MEMORY_REQUIREMENTS_2",                           pure (VkStructureType 1000146004))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_FEATURES_2",                                   pure (VkStructureType 1000059000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROPERTIES_2",                                 pure (VkStructureType 1000059001))
                             , ("VK_STRUCTURE_TYPE_FORMAT_PROPERTIES_2",                                          pure (VkStructureType 1000059002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_FORMAT_PROPERTIES_2",                                    pure (VkStructureType 1000059003))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_IMAGE_FORMAT_INFO_2",                          pure (VkStructureType 1000059004))
                             , ("VK_STRUCTURE_TYPE_QUEUE_FAMILY_PROPERTIES_2",                                    pure (VkStructureType 1000059005))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2",                          pure (VkStructureType 1000059006))
                             , ("VK_STRUCTURE_TYPE_SPARSE_IMAGE_FORMAT_PROPERTIES_2",                             pure (VkStructureType 1000059007))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SPARSE_IMAGE_FORMAT_INFO_2",                   pure (VkStructureType 1000059008))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_POINT_CLIPPING_PROPERTIES",                    pure (VkStructureType 1000117000))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_INPUT_ATTACHMENT_ASPECT_CREATE_INFO",              pure (VkStructureType 1000117001))
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_USAGE_CREATE_INFO",                                 pure (VkStructureType 1000117002))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_DOMAIN_ORIGIN_STATE_CREATE_INFO",        pure (VkStructureType 1000117003))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_MULTIVIEW_CREATE_INFO",                            pure (VkStructureType 1000053000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_FEATURES",                           pure (VkStructureType 1000053001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PROPERTIES",                         pure (VkStructureType 1000053002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VARIABLE_POINTER_FEATURES",                    pure (VkStructureType 1000120000))
                             , ("VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO",                                        pure (VkStructureType 1000145000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES",                    pure (VkStructureType 1000145001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES",                  pure (VkStructureType 1000145002))
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2",                                          pure (VkStructureType 1000145003))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_CREATE_INFO",                         pure (VkStructureType 1000156000))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_INFO",                                pure (VkStructureType 1000156001))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_PLANE_MEMORY_INFO",                                 pure (VkStructureType 1000156002))
                             , ("VK_STRUCTURE_TYPE_IMAGE_PLANE_MEMORY_REQUIREMENTS_INFO",                         pure (VkStructureType 1000156003))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_YCBCR_CONVERSION_FEATURES",            pure (VkStructureType 1000156004))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_YCBCR_CONVERSION_IMAGE_FORMAT_PROPERTIES",             pure (VkStructureType 1000156005))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_UPDATE_TEMPLATE_CREATE_INFO",                       pure (VkStructureType 1000085000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_IMAGE_FORMAT_INFO",                   pure (VkStructureType 1000071000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_IMAGE_FORMAT_PROPERTIES",                             pure (VkStructureType 1000071001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_BUFFER_INFO",                         pure (VkStructureType 1000071002))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_BUFFER_PROPERTIES",                                   pure (VkStructureType 1000071003))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_ID_PROPERTIES",                                pure (VkStructureType 1000071004))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO",                           pure (VkStructureType 1000072000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO",                            pure (VkStructureType 1000072001))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO",                                  pure (VkStructureType 1000072002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_FENCE_INFO",                          pure (VkStructureType 1000112000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_FENCE_PROPERTIES",                                    pure (VkStructureType 1000112001))
                             , ("VK_STRUCTURE_TYPE_EXPORT_FENCE_CREATE_INFO",                                     pure (VkStructureType 1000113000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_CREATE_INFO",                                 pure (VkStructureType 1000077000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_SEMAPHORE_INFO",                      pure (VkStructureType 1000076000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_SEMAPHORE_PROPERTIES",                                pure (VkStructureType 1000076001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MAINTENANCE_3_PROPERTIES",                     pure (VkStructureType 1000168000))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_SUPPORT",                                pure (VkStructureType 1000168001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_DRAW_PARAMETER_FEATURES",               pure (VkStructureType 1000063000))
                             , ("VK_STRUCTURE_TYPE_SWAPCHAIN_CREATE_INFO_KHR",                                    pure (VkStructureType 1000001000))
                             , ("VK_STRUCTURE_TYPE_PRESENT_INFO_KHR",                                             pure (VkStructureType 1000001001))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR",                        pure (VkStructureType 1000060007))
                             , ("VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR",                              pure (VkStructureType 1000060008))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR",                         pure (VkStructureType 1000060009))
                             , ("VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR",                                  pure (VkStructureType 1000060010))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR",                                pure (VkStructureType 1000060011))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR",                       pure (VkStructureType 1000060012))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_MODE_CREATE_INFO_KHR",                                 pure (VkStructureType 1000002000))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_SURFACE_CREATE_INFO_KHR",                              pure (VkStructureType 1000002001))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_PRESENT_INFO_KHR",                                     pure (VkStructureType 1000003000))
                             , ("VK_STRUCTURE_TYPE_XLIB_SURFACE_CREATE_INFO_KHR",                                 pure (VkStructureType 1000004000))
                             , ("VK_STRUCTURE_TYPE_XCB_SURFACE_CREATE_INFO_KHR",                                  pure (VkStructureType 1000005000))
                             , ("VK_STRUCTURE_TYPE_WAYLAND_SURFACE_CREATE_INFO_KHR",                              pure (VkStructureType 1000006000))
                             , ("VK_STRUCTURE_TYPE_MIR_SURFACE_CREATE_INFO_KHR",                                  pure (VkStructureType 1000007000))
                             , ("VK_STRUCTURE_TYPE_ANDROID_SURFACE_CREATE_INFO_KHR",                              pure (VkStructureType 1000008000))
                             , ("VK_STRUCTURE_TYPE_WIN32_SURFACE_CREATE_INFO_KHR",                                pure (VkStructureType 1000009000))
                             , ("VK_STRUCTURE_TYPE_NATIVE_BUFFER_ANDROID",                                        pure (VkStructureType 1000010000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_REPORT_CALLBACK_CREATE_INFO_EXT",                        pure (VkStructureType 1000011000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_RASTERIZATION_ORDER_AMD",         pure (VkStructureType 1000018000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_NAME_INFO_EXT",                            pure (VkStructureType 1000022000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_MARKER_OBJECT_TAG_INFO_EXT",                             pure (VkStructureType 1000022001))
                             , ("VK_STRUCTURE_TYPE_DEBUG_MARKER_MARKER_INFO_EXT",                                 pure (VkStructureType 1000022002))
                             , ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV",                    pure (VkStructureType 1000026000))
                             , ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV",                   pure (VkStructureType 1000026001))
                             , ("VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV",                 pure (VkStructureType 1000026002))
                             , ("VK_STRUCTURE_TYPE_TEXTURE_LOD_GATHER_FORMAT_PROPERTIES_AMD",                     pure (VkStructureType 1000041000))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO_NV",                         pure (VkStructureType 1000056000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO_NV",                               pure (VkStructureType 1000056001))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_NV",                           pure (VkStructureType 1000057000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_NV",                           pure (VkStructureType 1000057001))
                             , ("VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_NV",                    pure (VkStructureType 1000058000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_CAPABILITIES_KHR",                        pure (VkStructureType 1000060007))
                             , ("VK_STRUCTURE_TYPE_IMAGE_SWAPCHAIN_CREATE_INFO_KHR",                              pure (VkStructureType 1000060008))
                             , ("VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_SWAPCHAIN_INFO_KHR",                         pure (VkStructureType 1000060009))
                             , ("VK_STRUCTURE_TYPE_ACQUIRE_NEXT_IMAGE_INFO_KHR",                                  pure (VkStructureType 1000060010))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_PRESENT_INFO_KHR",                                pure (VkStructureType 1000060011))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GROUP_SWAPCHAIN_CREATE_INFO_KHR",                       pure (VkStructureType 1000060012))
                             , ("VK_STRUCTURE_TYPE_VALIDATION_FLAGS_EXT",                                         pure (VkStructureType 1000061000))
                             , ("VK_STRUCTURE_TYPE_VI_SURFACE_CREATE_INFO_NN",                                    pure (VkStructureType 1000062000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_WIN32_HANDLE_INFO_KHR",                          pure (VkStructureType 1000073000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_MEMORY_WIN32_HANDLE_INFO_KHR",                          pure (VkStructureType 1000073001))
                             , ("VK_STRUCTURE_TYPE_MEMORY_WIN32_HANDLE_PROPERTIES_KHR",                           pure (VkStructureType 1000073002))
                             , ("VK_STRUCTURE_TYPE_MEMORY_GET_WIN32_HANDLE_INFO_KHR",                             pure (VkStructureType 1000073003))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_FD_INFO_KHR",                                    pure (VkStructureType 1000074000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_FD_PROPERTIES_KHR",                                     pure (VkStructureType 1000074001))
                             , ("VK_STRUCTURE_TYPE_MEMORY_GET_FD_INFO_KHR",                                       pure (VkStructureType 1000074002))
                             , ("VK_STRUCTURE_TYPE_WIN32_KEYED_MUTEX_ACQUIRE_RELEASE_INFO_KHR",                   pure (VkStructureType 1000075000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR",                       pure (VkStructureType 1000078000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_SEMAPHORE_WIN32_HANDLE_INFO_KHR",                       pure (VkStructureType 1000078001))
                             , ("VK_STRUCTURE_TYPE_D3D12_FENCE_SUBMIT_INFO_KHR",                                  pure (VkStructureType 1000078002))
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_GET_WIN32_HANDLE_INFO_KHR",                          pure (VkStructureType 1000078003))
                             , ("VK_STRUCTURE_TYPE_IMPORT_SEMAPHORE_FD_INFO_KHR",                                 pure (VkStructureType 1000079000))
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_GET_FD_INFO_KHR",                                    pure (VkStructureType 1000079001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PUSH_DESCRIPTOR_PROPERTIES_KHR",               pure (VkStructureType 1000080000))
                             , ("VK_STRUCTURE_TYPE_PRESENT_REGIONS_KHR",                                          pure (VkStructureType 1000084000))
                             , ("VK_STRUCTURE_TYPE_OBJECT_TABLE_CREATE_INFO_NVX",                                 pure (VkStructureType 1000086000))
                             , ("VK_STRUCTURE_TYPE_INDIRECT_COMMANDS_LAYOUT_CREATE_INFO_NVX",                     pure (VkStructureType 1000086001))
                             , ("VK_STRUCTURE_TYPE_CMD_PROCESS_COMMANDS_INFO_NVX",                                pure (VkStructureType 1000086002))
                             , ("VK_STRUCTURE_TYPE_CMD_RESERVE_SPACE_FOR_COMMANDS_INFO_NVX",                      pure (VkStructureType 1000086003))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_LIMITS_NVX",                         pure (VkStructureType 1000086004))
                             , ("VK_STRUCTURE_TYPE_DEVICE_GENERATED_COMMANDS_FEATURES_NVX",                       pure (VkStructureType 1000086005))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_W_SCALING_STATE_CREATE_INFO_NV",             pure (VkStructureType 1000087000))
                             , ("VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_EXT",                                   pure (VkStructureType 1000090000))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_POWER_INFO_EXT",                                       pure (VkStructureType 1000091000))
                             , ("VK_STRUCTURE_TYPE_DEVICE_EVENT_INFO_EXT",                                        pure (VkStructureType 1000091001))
                             , ("VK_STRUCTURE_TYPE_DISPLAY_EVENT_INFO_EXT",                                       pure (VkStructureType 1000091002))
                             , ("VK_STRUCTURE_TYPE_SWAPCHAIN_COUNTER_CREATE_INFO_EXT",                            pure (VkStructureType 1000091003))
                             , ("VK_STRUCTURE_TYPE_PRESENT_TIMES_INFO_GOOGLE",                                    pure (VkStructureType 1000092000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MULTIVIEW_PER_VIEW_ATTRIBUTES_PROPERTIES_NVX", pure (VkStructureType 1000097000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_SWIZZLE_STATE_CREATE_INFO_NV",               pure (VkStructureType 1000098000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DISCARD_RECTANGLE_PROPERTIES_EXT",             pure (VkStructureType 1000099000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DISCARD_RECTANGLE_STATE_CREATE_INFO_EXT",             pure (VkStructureType 1000099001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_CONSERVATIVE_RASTERIZATION_PROPERTIES_EXT",    pure (VkStructureType 1000101000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_CONSERVATIVE_STATE_CREATE_INFO_EXT",    pure (VkStructureType 1000101001))
                             , ("VK_STRUCTURE_TYPE_HDR_METADATA_EXT",                                             pure (VkStructureType 1000105000))
                             , ("VK_STRUCTURE_TYPE_SHARED_PRESENT_SURFACE_CAPABILITIES_KHR",                      pure (VkStructureType 1000111000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_FENCE_WIN32_HANDLE_INFO_KHR",                           pure (VkStructureType 1000114000))
                             , ("VK_STRUCTURE_TYPE_EXPORT_FENCE_WIN32_HANDLE_INFO_KHR",                           pure (VkStructureType 1000114001))
                             , ("VK_STRUCTURE_TYPE_FENCE_GET_WIN32_HANDLE_INFO_KHR",                              pure (VkStructureType 1000114002))
                             , ("VK_STRUCTURE_TYPE_IMPORT_FENCE_FD_INFO_KHR",                                     pure (VkStructureType 1000115000))
                             , ("VK_STRUCTURE_TYPE_FENCE_GET_FD_INFO_KHR",                                        pure (VkStructureType 1000115001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SURFACE_INFO_2_KHR",                           pure (VkStructureType 1000119000))
                             , ("VK_STRUCTURE_TYPE_SURFACE_CAPABILITIES_2_KHR",                                   pure (VkStructureType 1000119001))
                             , ("VK_STRUCTURE_TYPE_SURFACE_FORMAT_2_KHR",                                         pure (VkStructureType 1000119002))
                             , ("VK_STRUCTURE_TYPE_IOS_SURFACE_CREATE_INFO_MVK",                                  pure (VkStructureType 1000122000))
                             , ("VK_STRUCTURE_TYPE_MACOS_SURFACE_CREATE_INFO_MVK",                                pure (VkStructureType 1000123000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_NAME_INFO_EXT",                             pure (VkStructureType 1000128000))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_OBJECT_TAG_INFO_EXT",                              pure (VkStructureType 1000128001))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_LABEL_EXT",                                        pure (VkStructureType 1000128002))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CALLBACK_DATA_EXT",                      pure (VkStructureType 1000128003))
                             , ("VK_STRUCTURE_TYPE_DEBUG_UTILS_MESSENGER_CREATE_INFO_EXT",                        pure (VkStructureType 1000128004))
                             , ("VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_USAGE_ANDROID",                        pure (VkStructureType 1000129000))
                             , ("VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_PROPERTIES_ANDROID",                   pure (VkStructureType 1000129001))
                             , ("VK_STRUCTURE_TYPE_ANDROID_HARDWARE_BUFFER_FORMAT_PROPERTIES_ANDROID",            pure (VkStructureType 1000129002))
                             , ("VK_STRUCTURE_TYPE_IMPORT_ANDROID_HARDWARE_BUFFER_INFO_ANDROID",                  pure (VkStructureType 1000129003))
                             , ("VK_STRUCTURE_TYPE_MEMORY_GET_ANDROID_HARDWARE_BUFFER_INFO_ANDROID",              pure (VkStructureType 1000129004))
                             , ("VK_STRUCTURE_TYPE_EXTERNAL_FORMAT_ANDROID",                                      pure (VkStructureType 1000129005))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLER_FILTER_MINMAX_PROPERTIES_EXT",         pure (VkStructureType 1000130000))
                             , ("VK_STRUCTURE_TYPE_SAMPLER_REDUCTION_MODE_CREATE_INFO_EXT",                       pure (VkStructureType 1000130001))
                             , ("VK_STRUCTURE_TYPE_SAMPLE_LOCATIONS_INFO_EXT",                                    pure (VkStructureType 1000143000))
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_SAMPLE_LOCATIONS_BEGIN_INFO_EXT",                  pure (VkStructureType 1000143001))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_SAMPLE_LOCATIONS_STATE_CREATE_INFO_EXT",              pure (VkStructureType 1000143002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SAMPLE_LOCATIONS_PROPERTIES_EXT",              pure (VkStructureType 1000143003))
                             , ("VK_STRUCTURE_TYPE_MULTISAMPLE_PROPERTIES_EXT",                                   pure (VkStructureType 1000143004))
                             , ("VK_STRUCTURE_TYPE_IMAGE_FORMAT_LIST_CREATE_INFO_KHR",                            pure (VkStructureType 1000147000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_FEATURES_EXT",        pure (VkStructureType 1000148000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_BLEND_OPERATION_ADVANCED_PROPERTIES_EXT",      pure (VkStructureType 1000148001))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_ADVANCED_STATE_CREATE_INFO_EXT",          pure (VkStructureType 1000148002))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_TO_COLOR_STATE_CREATE_INFO_NV",              pure (VkStructureType 1000149000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COVERAGE_MODULATION_STATE_CREATE_INFO_NV",            pure (VkStructureType 1000152000))
                             , ("VK_STRUCTURE_TYPE_VALIDATION_CACHE_CREATE_INFO_EXT",                             pure (VkStructureType 1000160000))
                             , ("VK_STRUCTURE_TYPE_SHADER_MODULE_VALIDATION_CACHE_CREATE_INFO_EXT",               pure (VkStructureType 1000160001))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_BINDING_FLAGS_CREATE_INFO_EXT",          pure (VkStructureType 1000161000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_FEATURES_EXT",             pure (VkStructureType 1000161001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_DESCRIPTOR_INDEXING_PROPERTIES_EXT",           pure (VkStructureType 1000161002))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_ALLOCATE_INFO_EXT",   pure (VkStructureType 1000161003))
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_VARIABLE_DESCRIPTOR_COUNT_LAYOUT_SUPPORT_EXT",  pure (VkStructureType 1000161004))
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_GLOBAL_PRIORITY_CREATE_INFO_EXT",                 pure (VkStructureType 1000174000))
                             , ("VK_STRUCTURE_TYPE_IMPORT_MEMORY_HOST_POINTER_INFO_EXT",                          pure (VkStructureType 1000178000))
                             , ("VK_STRUCTURE_TYPE_MEMORY_HOST_POINTER_PROPERTIES_EXT",                           pure (VkStructureType 1000178001))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_EXTERNAL_MEMORY_HOST_PROPERTIES_EXT",          pure (VkStructureType 1000178002))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_SHADER_CORE_PROPERTIES_AMD",                   pure (VkStructureType 1000185000))
                             , ("VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VERTEX_ATTRIBUTE_DIVISOR_PROPERTIES_EXT",      pure (VkStructureType 1000190000))
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_DIVISOR_STATE_CREATE_INFO_EXT",          pure (VkStructureType 1000190001))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStructureType")
                        v <- step readPrec
                        pure (VkStructureType v)
                        )
                    )

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_APPLICATION_INFO"
pattern VK_STRUCTURE_TYPE_APPLICATION_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = VkStructureType 0

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VkStructureType 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = VkStructureType 2

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = VkStructureType 3

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SUBMIT_INFO"
pattern VK_STRUCTURE_TYPE_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = VkStructureType 4

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VkStructureType 5

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE :: VkStructureType
pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VkStructureType 6

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_SPARSE_INFO"
pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = VkStructureType 7

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FENCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = VkStructureType 8

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VkStructureType 9

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EVENT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = VkStructureType 10

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = VkStructureType 11

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = VkStructureType 12

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = VkStructureType 13

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = VkStructureType 14

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = VkStructureType 15

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = VkStructureType 16

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = VkStructureType 17

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = VkStructureType 18

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = VkStructureType 19

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = VkStructureType 20

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = VkStructureType 21

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = VkStructureType 22

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = VkStructureType 23

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = VkStructureType 24

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = VkStructureType 25

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = VkStructureType 26

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = VkStructureType 27

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = VkStructureType 28

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = VkStructureType 29

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = VkStructureType 30

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VkStructureType 31

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = VkStructureType 32

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = VkStructureType 33

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = VkStructureType 34

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET :: VkStructureType
pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VkStructureType 35

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET :: VkStructureType
pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VkStructureType 36

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = VkStructureType 37

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = VkStructureType 38

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = VkStructureType 39

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = VkStructureType 40

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = VkStructureType 41

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 42

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = VkStructureType 43

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER :: VkStructureType
pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = VkStructureType 44

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER :: VkStructureType
pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VkStructureType 45

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_BARRIER"
pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = VkStructureType 46

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = VkStructureType 47

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = VkStructureType 48
-- ** VkObjectType

-- | VkObjectType - Specify an enumeration to track object handle types
--
-- = Description
--
-- \'
--
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | 'VkObjectType'                                | Vulkan Handle Type                                                                                  |
-- +===============================================+=====================================================================================================+
-- | @VK_OBJECT_TYPE_UNKNOWN@                      | Unknown\/Undefined Handle                                                                           |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_INSTANCE@                     | 'Graphics.Vulkan.Core10.DeviceInitialization.VkInstance'                                            |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_PHYSICAL_DEVICE@              | 'Graphics.Vulkan.Core10.DeviceInitialization.VkPhysicalDevice'                                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DEVICE@                       | 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice'                                              |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_QUEUE@                        | 'Graphics.Vulkan.Core10.Queue.VkQueue'                                                              |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_SEMAPHORE@                    | 'Graphics.Vulkan.Core10.Queue.VkSemaphore'                                                          |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_COMMAND_BUFFER@               | 'Graphics.Vulkan.Core10.Queue.VkCommandBuffer'                                                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_FENCE@                        | 'Graphics.Vulkan.Core10.Queue.VkFence'                                                              |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DEVICE_MEMORY@                | 'Graphics.Vulkan.Core10.Memory.VkDeviceMemory'                                                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_BUFFER@                       | 'Graphics.Vulkan.Core10.MemoryManagement.VkBuffer'                                                  |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_IMAGE@                        | 'Graphics.Vulkan.Core10.MemoryManagement.VkImage'                                                   |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_EVENT@                        | 'Graphics.Vulkan.Core10.Event.VkEvent'                                                              |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_QUERY_POOL@                   | 'Graphics.Vulkan.Core10.Query.VkQueryPool'                                                          |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_BUFFER_VIEW@                  | 'Graphics.Vulkan.Core10.BufferView.VkBufferView'                                                    |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_IMAGE_VIEW@                   | 'Graphics.Vulkan.Core10.ImageView.VkImageView'                                                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_SHADER_MODULE@                | 'Graphics.Vulkan.Core10.Shader.VkShaderModule'                                                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_PIPELINE_CACHE@               | 'Graphics.Vulkan.Core10.PipelineCache.VkPipelineCache'                                              |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_PIPELINE_LAYOUT@              | 'Graphics.Vulkan.Core10.Pipeline.VkPipelineLayout'                                                  |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_RENDER_PASS@                  | 'Graphics.Vulkan.Core10.Pipeline.VkRenderPass'                                                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_PIPELINE@                     | 'Graphics.Vulkan.Core10.Pipeline.VkPipeline'                                                        |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT@        | 'Graphics.Vulkan.Core10.PipelineLayout.VkDescriptorSetLayout'                                       |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_SAMPLER@                      | 'Graphics.Vulkan.Core10.Sampler.VkSampler'                                                          |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DESCRIPTOR_POOL@              | 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorPool'                                             |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DESCRIPTOR_SET@               | 'Graphics.Vulkan.Core10.DescriptorSet.VkDescriptorSet'                                              |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_FRAMEBUFFER@                  | 'Graphics.Vulkan.Core10.Pass.VkFramebuffer'                                                         |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_COMMAND_POOL@                 | 'Graphics.Vulkan.Core10.CommandPool.VkCommandPool'                                                  |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_SURFACE_KHR@                  | 'Graphics.Vulkan.Extensions.VK_KHR_surface.VkSurfaceKHR'                                            |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_SWAPCHAIN_KHR@                | 'Graphics.Vulkan.Extensions.VK_KHR_swapchain.VkSwapchainKHR'                                        |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DISPLAY_KHR@                  | 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayKHR'                                            |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DISPLAY_MODE_KHR@             | 'Graphics.Vulkan.Extensions.VK_KHR_display.VkDisplayModeKHR'                                        |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT@    | 'Graphics.Vulkan.Extensions.VK_EXT_debug_report.VkDebugReportCallbackEXT'                           |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE@   | 'Graphics.Vulkan.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplate' |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_OBJECT_TABLE_NVX@             | 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkObjectTableNVX'                      |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX@ | 'Graphics.Vulkan.Extensions.VK_NVX_device_generated_commands.VkIndirectCommandsLayoutNVX'           |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
-- | @VK_OBJECT_TYPE_VALIDATION_CACHE_EXT@         | 'Graphics.Vulkan.Extensions.VK_EXT_validation_cache.VkValidationCacheEXT'                           |
-- +-----------------------------------------------+-----------------------------------------------------------------------------------------------------+
--
-- VkObjectType and Vulkan Handle Relationship
--
-- = See Also
--
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsObjectNameInfoEXT',
-- 'Graphics.Vulkan.Extensions.VK_EXT_debug_utils.VkDebugUtilsObjectTagInfoEXT'
newtype VkObjectType = VkObjectType Int32
  deriving (Eq, Ord, Storable)

instance Show VkObjectType where
  showsPrec _ VK_OBJECT_TYPE_UNKNOWN = showString "VK_OBJECT_TYPE_UNKNOWN"
  showsPrec _ VK_OBJECT_TYPE_INSTANCE = showString "VK_OBJECT_TYPE_INSTANCE"
  showsPrec _ VK_OBJECT_TYPE_PHYSICAL_DEVICE = showString "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
  showsPrec _ VK_OBJECT_TYPE_DEVICE = showString "VK_OBJECT_TYPE_DEVICE"
  showsPrec _ VK_OBJECT_TYPE_QUEUE = showString "VK_OBJECT_TYPE_QUEUE"
  showsPrec _ VK_OBJECT_TYPE_SEMAPHORE = showString "VK_OBJECT_TYPE_SEMAPHORE"
  showsPrec _ VK_OBJECT_TYPE_COMMAND_BUFFER = showString "VK_OBJECT_TYPE_COMMAND_BUFFER"
  showsPrec _ VK_OBJECT_TYPE_FENCE = showString "VK_OBJECT_TYPE_FENCE"
  showsPrec _ VK_OBJECT_TYPE_DEVICE_MEMORY = showString "VK_OBJECT_TYPE_DEVICE_MEMORY"
  showsPrec _ VK_OBJECT_TYPE_BUFFER = showString "VK_OBJECT_TYPE_BUFFER"
  showsPrec _ VK_OBJECT_TYPE_IMAGE = showString "VK_OBJECT_TYPE_IMAGE"
  showsPrec _ VK_OBJECT_TYPE_EVENT = showString "VK_OBJECT_TYPE_EVENT"
  showsPrec _ VK_OBJECT_TYPE_QUERY_POOL = showString "VK_OBJECT_TYPE_QUERY_POOL"
  showsPrec _ VK_OBJECT_TYPE_BUFFER_VIEW = showString "VK_OBJECT_TYPE_BUFFER_VIEW"
  showsPrec _ VK_OBJECT_TYPE_IMAGE_VIEW = showString "VK_OBJECT_TYPE_IMAGE_VIEW"
  showsPrec _ VK_OBJECT_TYPE_SHADER_MODULE = showString "VK_OBJECT_TYPE_SHADER_MODULE"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE_CACHE = showString "VK_OBJECT_TYPE_PIPELINE_CACHE"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE_LAYOUT = showString "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
  showsPrec _ VK_OBJECT_TYPE_RENDER_PASS = showString "VK_OBJECT_TYPE_RENDER_PASS"
  showsPrec _ VK_OBJECT_TYPE_PIPELINE = showString "VK_OBJECT_TYPE_PIPELINE"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
  showsPrec _ VK_OBJECT_TYPE_SAMPLER = showString "VK_OBJECT_TYPE_SAMPLER"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_POOL = showString "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
  showsPrec _ VK_OBJECT_TYPE_DESCRIPTOR_SET = showString "VK_OBJECT_TYPE_DESCRIPTOR_SET"
  showsPrec _ VK_OBJECT_TYPE_FRAMEBUFFER = showString "VK_OBJECT_TYPE_FRAMEBUFFER"
  showsPrec _ VK_OBJECT_TYPE_COMMAND_POOL = showString "VK_OBJECT_TYPE_COMMAND_POOL"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkObjectType 1000156000) = showString "VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION"
  showsPrec _ (VkObjectType 1000085000) = showString "VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE"
  showsPrec _ (VkObjectType 1000000000) = showString "VK_OBJECT_TYPE_SURFACE_KHR"
  showsPrec _ (VkObjectType 1000001000) = showString "VK_OBJECT_TYPE_SWAPCHAIN_KHR"
  showsPrec _ (VkObjectType 1000002000) = showString "VK_OBJECT_TYPE_DISPLAY_KHR"
  showsPrec _ (VkObjectType 1000002001) = showString "VK_OBJECT_TYPE_DISPLAY_MODE_KHR"
  showsPrec _ (VkObjectType 1000011000) = showString "VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT"
  showsPrec _ (VkObjectType 1000086000) = showString "VK_OBJECT_TYPE_OBJECT_TABLE_NVX"
  showsPrec _ (VkObjectType 1000086001) = showString "VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX"
  showsPrec _ (VkObjectType 1000128000) = showString "VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT"
  showsPrec _ (VkObjectType 1000160000) = showString "VK_OBJECT_TYPE_VALIDATION_CACHE_EXT"
  showsPrec p (VkObjectType x) = showParen (p >= 11) (showString "VkObjectType " . showsPrec 11 x)

instance Read VkObjectType where
  readPrec = parens ( choose [ ("VK_OBJECT_TYPE_UNKNOWN",               pure VK_OBJECT_TYPE_UNKNOWN)
                             , ("VK_OBJECT_TYPE_INSTANCE",              pure VK_OBJECT_TYPE_INSTANCE)
                             , ("VK_OBJECT_TYPE_PHYSICAL_DEVICE",       pure VK_OBJECT_TYPE_PHYSICAL_DEVICE)
                             , ("VK_OBJECT_TYPE_DEVICE",                pure VK_OBJECT_TYPE_DEVICE)
                             , ("VK_OBJECT_TYPE_QUEUE",                 pure VK_OBJECT_TYPE_QUEUE)
                             , ("VK_OBJECT_TYPE_SEMAPHORE",             pure VK_OBJECT_TYPE_SEMAPHORE)
                             , ("VK_OBJECT_TYPE_COMMAND_BUFFER",        pure VK_OBJECT_TYPE_COMMAND_BUFFER)
                             , ("VK_OBJECT_TYPE_FENCE",                 pure VK_OBJECT_TYPE_FENCE)
                             , ("VK_OBJECT_TYPE_DEVICE_MEMORY",         pure VK_OBJECT_TYPE_DEVICE_MEMORY)
                             , ("VK_OBJECT_TYPE_BUFFER",                pure VK_OBJECT_TYPE_BUFFER)
                             , ("VK_OBJECT_TYPE_IMAGE",                 pure VK_OBJECT_TYPE_IMAGE)
                             , ("VK_OBJECT_TYPE_EVENT",                 pure VK_OBJECT_TYPE_EVENT)
                             , ("VK_OBJECT_TYPE_QUERY_POOL",            pure VK_OBJECT_TYPE_QUERY_POOL)
                             , ("VK_OBJECT_TYPE_BUFFER_VIEW",           pure VK_OBJECT_TYPE_BUFFER_VIEW)
                             , ("VK_OBJECT_TYPE_IMAGE_VIEW",            pure VK_OBJECT_TYPE_IMAGE_VIEW)
                             , ("VK_OBJECT_TYPE_SHADER_MODULE",         pure VK_OBJECT_TYPE_SHADER_MODULE)
                             , ("VK_OBJECT_TYPE_PIPELINE_CACHE",        pure VK_OBJECT_TYPE_PIPELINE_CACHE)
                             , ("VK_OBJECT_TYPE_PIPELINE_LAYOUT",       pure VK_OBJECT_TYPE_PIPELINE_LAYOUT)
                             , ("VK_OBJECT_TYPE_RENDER_PASS",           pure VK_OBJECT_TYPE_RENDER_PASS)
                             , ("VK_OBJECT_TYPE_PIPELINE",              pure VK_OBJECT_TYPE_PIPELINE)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT", pure VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT)
                             , ("VK_OBJECT_TYPE_SAMPLER",               pure VK_OBJECT_TYPE_SAMPLER)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_POOL",       pure VK_OBJECT_TYPE_DESCRIPTOR_POOL)
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_SET",        pure VK_OBJECT_TYPE_DESCRIPTOR_SET)
                             , ("VK_OBJECT_TYPE_FRAMEBUFFER",           pure VK_OBJECT_TYPE_FRAMEBUFFER)
                             , ("VK_OBJECT_TYPE_COMMAND_POOL",          pure VK_OBJECT_TYPE_COMMAND_POOL)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_OBJECT_TYPE_SAMPLER_YCBCR_CONVERSION",     pure (VkObjectType 1000156000))
                             , ("VK_OBJECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE",   pure (VkObjectType 1000085000))
                             , ("VK_OBJECT_TYPE_SURFACE_KHR",                  pure (VkObjectType 1000000000))
                             , ("VK_OBJECT_TYPE_SWAPCHAIN_KHR",                pure (VkObjectType 1000001000))
                             , ("VK_OBJECT_TYPE_DISPLAY_KHR",                  pure (VkObjectType 1000002000))
                             , ("VK_OBJECT_TYPE_DISPLAY_MODE_KHR",             pure (VkObjectType 1000002001))
                             , ("VK_OBJECT_TYPE_DEBUG_REPORT_CALLBACK_EXT",    pure (VkObjectType 1000011000))
                             , ("VK_OBJECT_TYPE_OBJECT_TABLE_NVX",             pure (VkObjectType 1000086000))
                             , ("VK_OBJECT_TYPE_INDIRECT_COMMANDS_LAYOUT_NVX", pure (VkObjectType 1000086001))
                             , ("VK_OBJECT_TYPE_DEBUG_UTILS_MESSENGER_EXT",    pure (VkObjectType 1000128000))
                             , ("VK_OBJECT_TYPE_VALIDATION_CACHE_EXT",         pure (VkObjectType 1000160000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkObjectType")
                        v <- step readPrec
                        pure (VkObjectType v)
                        )
                    )

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_UNKNOWN"
pattern VK_OBJECT_TYPE_UNKNOWN :: VkObjectType
pattern VK_OBJECT_TYPE_UNKNOWN = VkObjectType 0

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_INSTANCE"
pattern VK_OBJECT_TYPE_INSTANCE :: VkObjectType
pattern VK_OBJECT_TYPE_INSTANCE = VkObjectType 1

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PHYSICAL_DEVICE"
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE :: VkObjectType
pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE = VkObjectType 2

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE"
pattern VK_OBJECT_TYPE_DEVICE :: VkObjectType
pattern VK_OBJECT_TYPE_DEVICE = VkObjectType 3

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUEUE"
pattern VK_OBJECT_TYPE_QUEUE :: VkObjectType
pattern VK_OBJECT_TYPE_QUEUE = VkObjectType 4

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SEMAPHORE"
pattern VK_OBJECT_TYPE_SEMAPHORE :: VkObjectType
pattern VK_OBJECT_TYPE_SEMAPHORE = VkObjectType 5

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_BUFFER"
pattern VK_OBJECT_TYPE_COMMAND_BUFFER :: VkObjectType
pattern VK_OBJECT_TYPE_COMMAND_BUFFER = VkObjectType 6

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FENCE"
pattern VK_OBJECT_TYPE_FENCE :: VkObjectType
pattern VK_OBJECT_TYPE_FENCE = VkObjectType 7

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DEVICE_MEMORY"
pattern VK_OBJECT_TYPE_DEVICE_MEMORY :: VkObjectType
pattern VK_OBJECT_TYPE_DEVICE_MEMORY = VkObjectType 8

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER"
pattern VK_OBJECT_TYPE_BUFFER :: VkObjectType
pattern VK_OBJECT_TYPE_BUFFER = VkObjectType 9

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE"
pattern VK_OBJECT_TYPE_IMAGE :: VkObjectType
pattern VK_OBJECT_TYPE_IMAGE = VkObjectType 10

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_EVENT"
pattern VK_OBJECT_TYPE_EVENT :: VkObjectType
pattern VK_OBJECT_TYPE_EVENT = VkObjectType 11

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_QUERY_POOL"
pattern VK_OBJECT_TYPE_QUERY_POOL :: VkObjectType
pattern VK_OBJECT_TYPE_QUERY_POOL = VkObjectType 12

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_BUFFER_VIEW"
pattern VK_OBJECT_TYPE_BUFFER_VIEW :: VkObjectType
pattern VK_OBJECT_TYPE_BUFFER_VIEW = VkObjectType 13

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_IMAGE_VIEW"
pattern VK_OBJECT_TYPE_IMAGE_VIEW :: VkObjectType
pattern VK_OBJECT_TYPE_IMAGE_VIEW = VkObjectType 14

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SHADER_MODULE"
pattern VK_OBJECT_TYPE_SHADER_MODULE :: VkObjectType
pattern VK_OBJECT_TYPE_SHADER_MODULE = VkObjectType 15

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_CACHE"
pattern VK_OBJECT_TYPE_PIPELINE_CACHE :: VkObjectType
pattern VK_OBJECT_TYPE_PIPELINE_CACHE = VkObjectType 16

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE_LAYOUT"
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT :: VkObjectType
pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT = VkObjectType 17

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_RENDER_PASS"
pattern VK_OBJECT_TYPE_RENDER_PASS :: VkObjectType
pattern VK_OBJECT_TYPE_RENDER_PASS = VkObjectType 18

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_PIPELINE"
pattern VK_OBJECT_TYPE_PIPELINE :: VkObjectType
pattern VK_OBJECT_TYPE_PIPELINE = VkObjectType 19

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = VkObjectType 20

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_SAMPLER"
pattern VK_OBJECT_TYPE_SAMPLER :: VkObjectType
pattern VK_OBJECT_TYPE_SAMPLER = VkObjectType 21

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_POOL"
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL = VkObjectType 22

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_DESCRIPTOR_SET"
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET :: VkObjectType
pattern VK_OBJECT_TYPE_DESCRIPTOR_SET = VkObjectType 23

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_FRAMEBUFFER"
pattern VK_OBJECT_TYPE_FRAMEBUFFER :: VkObjectType
pattern VK_OBJECT_TYPE_FRAMEBUFFER = VkObjectType 24

-- No documentation found for Nested "VkObjectType" "VK_OBJECT_TYPE_COMMAND_POOL"
pattern VK_OBJECT_TYPE_COMMAND_POOL :: VkObjectType
pattern VK_OBJECT_TYPE_COMMAND_POOL = VkObjectType 25
-- | VkFlags - Vulkan bitmasks
--
-- = Description
--
-- Bitmasks are passed to many commands and structures to compactly
-- represent options, but @VkFlags@ is not used directly in the API.
-- Instead, a @Vk*Flags@ type which is an alias of @VkFlags@, and whose
-- name matches the corresponding @Vk*FlagBits@ that are valid for that
-- type, is used.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Pipeline.VkColorComponentFlags'
type VkFlags = Word32
