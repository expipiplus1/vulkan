{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language LambdaCase #-}

module Graphics.Vulkan.Core10.Core
  ( Format
  , pattern FORMAT_UNDEFINED
  , pattern FORMAT_R4G4_UNORM_PACK8
  , pattern FORMAT_R4G4B4A4_UNORM_PACK16
  , pattern FORMAT_B4G4R4A4_UNORM_PACK16
  , pattern FORMAT_R5G6B5_UNORM_PACK16
  , pattern FORMAT_B5G6R5_UNORM_PACK16
  , pattern FORMAT_R5G5B5A1_UNORM_PACK16
  , pattern FORMAT_B5G5R5A1_UNORM_PACK16
  , pattern FORMAT_A1R5G5B5_UNORM_PACK16
  , pattern FORMAT_R8_UNORM
  , pattern FORMAT_R8_SNORM
  , pattern FORMAT_R8_USCALED
  , pattern FORMAT_R8_SSCALED
  , pattern FORMAT_R8_UINT
  , pattern FORMAT_R8_SINT
  , pattern FORMAT_R8_SRGB
  , pattern FORMAT_R8G8_UNORM
  , pattern FORMAT_R8G8_SNORM
  , pattern FORMAT_R8G8_USCALED
  , pattern FORMAT_R8G8_SSCALED
  , pattern FORMAT_R8G8_UINT
  , pattern FORMAT_R8G8_SINT
  , pattern FORMAT_R8G8_SRGB
  , pattern FORMAT_R8G8B8_UNORM
  , pattern FORMAT_R8G8B8_SNORM
  , pattern FORMAT_R8G8B8_USCALED
  , pattern FORMAT_R8G8B8_SSCALED
  , pattern FORMAT_R8G8B8_UINT
  , pattern FORMAT_R8G8B8_SINT
  , pattern FORMAT_R8G8B8_SRGB
  , pattern FORMAT_B8G8R8_UNORM
  , pattern FORMAT_B8G8R8_SNORM
  , pattern FORMAT_B8G8R8_USCALED
  , pattern FORMAT_B8G8R8_SSCALED
  , pattern FORMAT_B8G8R8_UINT
  , pattern FORMAT_B8G8R8_SINT
  , pattern FORMAT_B8G8R8_SRGB
  , pattern FORMAT_R8G8B8A8_UNORM
  , pattern FORMAT_R8G8B8A8_SNORM
  , pattern FORMAT_R8G8B8A8_USCALED
  , pattern FORMAT_R8G8B8A8_SSCALED
  , pattern FORMAT_R8G8B8A8_UINT
  , pattern FORMAT_R8G8B8A8_SINT
  , pattern FORMAT_R8G8B8A8_SRGB
  , pattern FORMAT_B8G8R8A8_UNORM
  , pattern FORMAT_B8G8R8A8_SNORM
  , pattern FORMAT_B8G8R8A8_USCALED
  , pattern FORMAT_B8G8R8A8_SSCALED
  , pattern FORMAT_B8G8R8A8_UINT
  , pattern FORMAT_B8G8R8A8_SINT
  , pattern FORMAT_B8G8R8A8_SRGB
  , pattern FORMAT_A8B8G8R8_UNORM_PACK32
  , pattern FORMAT_A8B8G8R8_SNORM_PACK32
  , pattern FORMAT_A8B8G8R8_USCALED_PACK32
  , pattern FORMAT_A8B8G8R8_SSCALED_PACK32
  , pattern FORMAT_A8B8G8R8_UINT_PACK32
  , pattern FORMAT_A8B8G8R8_SINT_PACK32
  , pattern FORMAT_A8B8G8R8_SRGB_PACK32
  , pattern FORMAT_A2R10G10B10_UNORM_PACK32
  , pattern FORMAT_A2R10G10B10_SNORM_PACK32
  , pattern FORMAT_A2R10G10B10_USCALED_PACK32
  , pattern FORMAT_A2R10G10B10_SSCALED_PACK32
  , pattern FORMAT_A2R10G10B10_UINT_PACK32
  , pattern FORMAT_A2R10G10B10_SINT_PACK32
  , pattern FORMAT_A2B10G10R10_UNORM_PACK32
  , pattern FORMAT_A2B10G10R10_SNORM_PACK32
  , pattern FORMAT_A2B10G10R10_USCALED_PACK32
  , pattern FORMAT_A2B10G10R10_SSCALED_PACK32
  , pattern FORMAT_A2B10G10R10_UINT_PACK32
  , pattern FORMAT_A2B10G10R10_SINT_PACK32
  , pattern FORMAT_R16_UNORM
  , pattern FORMAT_R16_SNORM
  , pattern FORMAT_R16_USCALED
  , pattern FORMAT_R16_SSCALED
  , pattern FORMAT_R16_UINT
  , pattern FORMAT_R16_SINT
  , pattern FORMAT_R16_SFLOAT
  , pattern FORMAT_R16G16_UNORM
  , pattern FORMAT_R16G16_SNORM
  , pattern FORMAT_R16G16_USCALED
  , pattern FORMAT_R16G16_SSCALED
  , pattern FORMAT_R16G16_UINT
  , pattern FORMAT_R16G16_SINT
  , pattern FORMAT_R16G16_SFLOAT
  , pattern FORMAT_R16G16B16_UNORM
  , pattern FORMAT_R16G16B16_SNORM
  , pattern FORMAT_R16G16B16_USCALED
  , pattern FORMAT_R16G16B16_SSCALED
  , pattern FORMAT_R16G16B16_UINT
  , pattern FORMAT_R16G16B16_SINT
  , pattern FORMAT_R16G16B16_SFLOAT
  , pattern FORMAT_R16G16B16A16_UNORM
  , pattern FORMAT_R16G16B16A16_SNORM
  , pattern FORMAT_R16G16B16A16_USCALED
  , pattern FORMAT_R16G16B16A16_SSCALED
  , pattern FORMAT_R16G16B16A16_UINT
  , pattern FORMAT_R16G16B16A16_SINT
  , pattern FORMAT_R16G16B16A16_SFLOAT
  , pattern FORMAT_R32_UINT
  , pattern FORMAT_R32_SINT
  , pattern FORMAT_R32_SFLOAT
  , pattern FORMAT_R32G32_UINT
  , pattern FORMAT_R32G32_SINT
  , pattern FORMAT_R32G32_SFLOAT
  , pattern FORMAT_R32G32B32_UINT
  , pattern FORMAT_R32G32B32_SINT
  , pattern FORMAT_R32G32B32_SFLOAT
  , pattern FORMAT_R32G32B32A32_UINT
  , pattern FORMAT_R32G32B32A32_SINT
  , pattern FORMAT_R32G32B32A32_SFLOAT
  , pattern FORMAT_R64_UINT
  , pattern FORMAT_R64_SINT
  , pattern FORMAT_R64_SFLOAT
  , pattern FORMAT_R64G64_UINT
  , pattern FORMAT_R64G64_SINT
  , pattern FORMAT_R64G64_SFLOAT
  , pattern FORMAT_R64G64B64_UINT
  , pattern FORMAT_R64G64B64_SINT
  , pattern FORMAT_R64G64B64_SFLOAT
  , pattern FORMAT_R64G64B64A64_UINT
  , pattern FORMAT_R64G64B64A64_SINT
  , pattern FORMAT_R64G64B64A64_SFLOAT
  , pattern FORMAT_B10G11R11_UFLOAT_PACK32
  , pattern FORMAT_E5B9G9R9_UFLOAT_PACK32
  , pattern FORMAT_D16_UNORM
  , pattern FORMAT_X8_D24_UNORM_PACK32
  , pattern FORMAT_D32_SFLOAT
  , pattern FORMAT_S8_UINT
  , pattern FORMAT_D16_UNORM_S8_UINT
  , pattern FORMAT_D24_UNORM_S8_UINT
  , pattern FORMAT_D32_SFLOAT_S8_UINT
  , pattern FORMAT_BC1_RGB_UNORM_BLOCK
  , pattern FORMAT_BC1_RGB_SRGB_BLOCK
  , pattern FORMAT_BC1_RGBA_UNORM_BLOCK
  , pattern FORMAT_BC1_RGBA_SRGB_BLOCK
  , pattern FORMAT_BC2_UNORM_BLOCK
  , pattern FORMAT_BC2_SRGB_BLOCK
  , pattern FORMAT_BC3_UNORM_BLOCK
  , pattern FORMAT_BC3_SRGB_BLOCK
  , pattern FORMAT_BC4_UNORM_BLOCK
  , pattern FORMAT_BC4_SNORM_BLOCK
  , pattern FORMAT_BC5_UNORM_BLOCK
  , pattern FORMAT_BC5_SNORM_BLOCK
  , pattern FORMAT_BC6H_UFLOAT_BLOCK
  , pattern FORMAT_BC6H_SFLOAT_BLOCK
  , pattern FORMAT_BC7_UNORM_BLOCK
  , pattern FORMAT_BC7_SRGB_BLOCK
  , pattern FORMAT_ETC2_R8G8B8_UNORM_BLOCK
  , pattern FORMAT_ETC2_R8G8B8_SRGB_BLOCK
  , pattern FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
  , pattern FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
  , pattern FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
  , pattern FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
  , pattern FORMAT_EAC_R11_UNORM_BLOCK
  , pattern FORMAT_EAC_R11_SNORM_BLOCK
  , pattern FORMAT_EAC_R11G11_UNORM_BLOCK
  , pattern FORMAT_EAC_R11G11_SNORM_BLOCK
  , pattern FORMAT_ASTC_4x4_UNORM_BLOCK
  , pattern FORMAT_ASTC_4x4_SRGB_BLOCK
  , pattern FORMAT_ASTC_5x4_UNORM_BLOCK
  , pattern FORMAT_ASTC_5x4_SRGB_BLOCK
  , pattern FORMAT_ASTC_5x5_UNORM_BLOCK
  , pattern FORMAT_ASTC_5x5_SRGB_BLOCK
  , pattern FORMAT_ASTC_6x5_UNORM_BLOCK
  , pattern FORMAT_ASTC_6x5_SRGB_BLOCK
  , pattern FORMAT_ASTC_6x6_UNORM_BLOCK
  , pattern FORMAT_ASTC_6x6_SRGB_BLOCK
  , pattern FORMAT_ASTC_8x5_UNORM_BLOCK
  , pattern FORMAT_ASTC_8x5_SRGB_BLOCK
  , pattern FORMAT_ASTC_8x6_UNORM_BLOCK
  , pattern FORMAT_ASTC_8x6_SRGB_BLOCK
  , pattern FORMAT_ASTC_8x8_UNORM_BLOCK
  , pattern FORMAT_ASTC_8x8_SRGB_BLOCK
  , pattern FORMAT_ASTC_10x5_UNORM_BLOCK
  , pattern FORMAT_ASTC_10x5_SRGB_BLOCK
  , pattern FORMAT_ASTC_10x6_UNORM_BLOCK
  , pattern FORMAT_ASTC_10x6_SRGB_BLOCK
  , pattern FORMAT_ASTC_10x8_UNORM_BLOCK
  , pattern FORMAT_ASTC_10x8_SRGB_BLOCK
  , pattern FORMAT_ASTC_10x10_UNORM_BLOCK
  , pattern FORMAT_ASTC_10x10_SRGB_BLOCK
  , pattern FORMAT_ASTC_12x10_UNORM_BLOCK
  , pattern FORMAT_ASTC_12x10_SRGB_BLOCK
  , pattern FORMAT_ASTC_12x12_UNORM_BLOCK
  , pattern FORMAT_ASTC_12x12_SRGB_BLOCK
  , ObjectType
  , pattern OBJECT_TYPE_UNKNOWN
  , pattern OBJECT_TYPE_INSTANCE
  , pattern OBJECT_TYPE_PHYSICAL_DEVICE
  , pattern OBJECT_TYPE_DEVICE
  , pattern OBJECT_TYPE_QUEUE
  , pattern OBJECT_TYPE_SEMAPHORE
  , pattern OBJECT_TYPE_COMMAND_BUFFER
  , pattern OBJECT_TYPE_FENCE
  , pattern OBJECT_TYPE_DEVICE_MEMORY
  , pattern OBJECT_TYPE_BUFFER
  , pattern OBJECT_TYPE_IMAGE
  , pattern OBJECT_TYPE_EVENT
  , pattern OBJECT_TYPE_QUERY_POOL
  , pattern OBJECT_TYPE_BUFFER_VIEW
  , pattern OBJECT_TYPE_IMAGE_VIEW
  , pattern OBJECT_TYPE_SHADER_MODULE
  , pattern OBJECT_TYPE_PIPELINE_CACHE
  , pattern OBJECT_TYPE_PIPELINE_LAYOUT
  , pattern OBJECT_TYPE_RENDER_PASS
  , pattern OBJECT_TYPE_PIPELINE
  , pattern OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
  , pattern OBJECT_TYPE_SAMPLER
  , pattern OBJECT_TYPE_DESCRIPTOR_POOL
  , pattern OBJECT_TYPE_DESCRIPTOR_SET
  , pattern OBJECT_TYPE_FRAMEBUFFER
  , pattern OBJECT_TYPE_COMMAND_POOL
  , Result
  , pattern SUCCESS
  , pattern NOT_READY
  , pattern TIMEOUT
  , pattern EVENT_SET
  , pattern EVENT_RESET
  , pattern INCOMPLETE
  , pattern ERROR_OUT_OF_HOST_MEMORY
  , pattern ERROR_OUT_OF_DEVICE_MEMORY
  , pattern ERROR_INITIALIZATION_FAILED
  , pattern ERROR_DEVICE_LOST
  , pattern ERROR_MEMORY_MAP_FAILED
  , pattern ERROR_LAYER_NOT_PRESENT
  , pattern ERROR_EXTENSION_NOT_PRESENT
  , pattern ERROR_FEATURE_NOT_PRESENT
  , pattern ERROR_INCOMPATIBLE_DRIVER
  , pattern ERROR_TOO_MANY_OBJECTS
  , pattern ERROR_FORMAT_NOT_SUPPORTED
  , pattern ERROR_FRAGMENTED_POOL
  , StructureType
  , pattern STRUCTURE_TYPE_APPLICATION_INFO
  , pattern STRUCTURE_TYPE_INSTANCE_CREATE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO
  , pattern STRUCTURE_TYPE_DEVICE_CREATE_INFO
  , pattern STRUCTURE_TYPE_SUBMIT_INFO
  , pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_MAPPED_MEMORY_RANGE
  , pattern STRUCTURE_TYPE_BIND_SPARSE_INFO
  , pattern STRUCTURE_TYPE_FENCE_CREATE_INFO
  , pattern STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO
  , pattern STRUCTURE_TYPE_EVENT_CREATE_INFO
  , pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern STRUCTURE_TYPE_BUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_IMAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO
  , pattern STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO
  , pattern STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO
  , pattern STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO
  , pattern STRUCTURE_TYPE_SAMPLER_CREATE_INFO
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO
  , pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO
  , pattern STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET
  , pattern STRUCTURE_TYPE_COPY_DESCRIPTOR_SET
  , pattern STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO
  , pattern STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO
  , pattern STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO
  , pattern STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO
  , pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER
  , pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER
  , pattern STRUCTURE_TYPE_MEMORY_BARRIER
  , pattern STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
  , pattern STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
  , VendorId
  , pattern VENDOR_ID_VIV
  , pattern VENDOR_ID_VSI
  , pattern VENDOR_ID_KAZAN
  , bool32ToBool
  , boolToBool32
  , Zero(..)
  ) where




import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkFormat(..)
  , VkObjectType(..)
  , VkResult(..)
  , VkStructureType(..)
  , VkVendorId(..)
  , pattern VK_ERROR_DEVICE_LOST
  , pattern VK_ERROR_EXTENSION_NOT_PRESENT
  , pattern VK_ERROR_FEATURE_NOT_PRESENT
  , pattern VK_ERROR_FORMAT_NOT_SUPPORTED
  , pattern VK_ERROR_FRAGMENTED_POOL
  , pattern VK_ERROR_INCOMPATIBLE_DRIVER
  , pattern VK_ERROR_INITIALIZATION_FAILED
  , pattern VK_ERROR_LAYER_NOT_PRESENT
  , pattern VK_ERROR_MEMORY_MAP_FAILED
  , pattern VK_ERROR_OUT_OF_DEVICE_MEMORY
  , pattern VK_ERROR_OUT_OF_HOST_MEMORY
  , pattern VK_ERROR_TOO_MANY_OBJECTS
  , pattern VK_EVENT_RESET
  , pattern VK_EVENT_SET
  , pattern VK_FALSE
  , pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16
  , pattern VK_FORMAT_A2B10G10R10_SINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UINT_PACK32
  , pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32
  , pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SINT_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UINT_PACK32
  , pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32
  , pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32
  , pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32
  , pattern VK_FORMAT_A8B8G8R8_UINT_PACK32
  , pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32
  , pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32
  , pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK
  , pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK
  , pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32
  , pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16
  , pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16
  , pattern VK_FORMAT_B5G6R5_UNORM_PACK16
  , pattern VK_FORMAT_B8G8R8A8_SINT
  , pattern VK_FORMAT_B8G8R8A8_SNORM
  , pattern VK_FORMAT_B8G8R8A8_SRGB
  , pattern VK_FORMAT_B8G8R8A8_SSCALED
  , pattern VK_FORMAT_B8G8R8A8_UINT
  , pattern VK_FORMAT_B8G8R8A8_UNORM
  , pattern VK_FORMAT_B8G8R8A8_USCALED
  , pattern VK_FORMAT_B8G8R8_SINT
  , pattern VK_FORMAT_B8G8R8_SNORM
  , pattern VK_FORMAT_B8G8R8_SRGB
  , pattern VK_FORMAT_B8G8R8_SSCALED
  , pattern VK_FORMAT_B8G8R8_UINT
  , pattern VK_FORMAT_B8G8R8_UNORM
  , pattern VK_FORMAT_B8G8R8_USCALED
  , pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK
  , pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK
  , pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK
  , pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK
  , pattern VK_FORMAT_BC2_SRGB_BLOCK
  , pattern VK_FORMAT_BC2_UNORM_BLOCK
  , pattern VK_FORMAT_BC3_SRGB_BLOCK
  , pattern VK_FORMAT_BC3_UNORM_BLOCK
  , pattern VK_FORMAT_BC4_SNORM_BLOCK
  , pattern VK_FORMAT_BC4_UNORM_BLOCK
  , pattern VK_FORMAT_BC5_SNORM_BLOCK
  , pattern VK_FORMAT_BC5_UNORM_BLOCK
  , pattern VK_FORMAT_BC6H_SFLOAT_BLOCK
  , pattern VK_FORMAT_BC6H_UFLOAT_BLOCK
  , pattern VK_FORMAT_BC7_SRGB_BLOCK
  , pattern VK_FORMAT_BC7_UNORM_BLOCK
  , pattern VK_FORMAT_D16_UNORM
  , pattern VK_FORMAT_D16_UNORM_S8_UINT
  , pattern VK_FORMAT_D24_UNORM_S8_UINT
  , pattern VK_FORMAT_D32_SFLOAT
  , pattern VK_FORMAT_D32_SFLOAT_S8_UINT
  , pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32
  , pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11_SNORM_BLOCK
  , pattern VK_FORMAT_EAC_R11_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK
  , pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK
  , pattern VK_FORMAT_R16G16B16A16_SFLOAT
  , pattern VK_FORMAT_R16G16B16A16_SINT
  , pattern VK_FORMAT_R16G16B16A16_SNORM
  , pattern VK_FORMAT_R16G16B16A16_SSCALED
  , pattern VK_FORMAT_R16G16B16A16_UINT
  , pattern VK_FORMAT_R16G16B16A16_UNORM
  , pattern VK_FORMAT_R16G16B16A16_USCALED
  , pattern VK_FORMAT_R16G16B16_SFLOAT
  , pattern VK_FORMAT_R16G16B16_SINT
  , pattern VK_FORMAT_R16G16B16_SNORM
  , pattern VK_FORMAT_R16G16B16_SSCALED
  , pattern VK_FORMAT_R16G16B16_UINT
  , pattern VK_FORMAT_R16G16B16_UNORM
  , pattern VK_FORMAT_R16G16B16_USCALED
  , pattern VK_FORMAT_R16G16_SFLOAT
  , pattern VK_FORMAT_R16G16_SINT
  , pattern VK_FORMAT_R16G16_SNORM
  , pattern VK_FORMAT_R16G16_SSCALED
  , pattern VK_FORMAT_R16G16_UINT
  , pattern VK_FORMAT_R16G16_UNORM
  , pattern VK_FORMAT_R16G16_USCALED
  , pattern VK_FORMAT_R16_SFLOAT
  , pattern VK_FORMAT_R16_SINT
  , pattern VK_FORMAT_R16_SNORM
  , pattern VK_FORMAT_R16_SSCALED
  , pattern VK_FORMAT_R16_UINT
  , pattern VK_FORMAT_R16_UNORM
  , pattern VK_FORMAT_R16_USCALED
  , pattern VK_FORMAT_R32G32B32A32_SFLOAT
  , pattern VK_FORMAT_R32G32B32A32_SINT
  , pattern VK_FORMAT_R32G32B32A32_UINT
  , pattern VK_FORMAT_R32G32B32_SFLOAT
  , pattern VK_FORMAT_R32G32B32_SINT
  , pattern VK_FORMAT_R32G32B32_UINT
  , pattern VK_FORMAT_R32G32_SFLOAT
  , pattern VK_FORMAT_R32G32_SINT
  , pattern VK_FORMAT_R32G32_UINT
  , pattern VK_FORMAT_R32_SFLOAT
  , pattern VK_FORMAT_R32_SINT
  , pattern VK_FORMAT_R32_UINT
  , pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16
  , pattern VK_FORMAT_R4G4_UNORM_PACK8
  , pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16
  , pattern VK_FORMAT_R5G6B5_UNORM_PACK16
  , pattern VK_FORMAT_R64G64B64A64_SFLOAT
  , pattern VK_FORMAT_R64G64B64A64_SINT
  , pattern VK_FORMAT_R64G64B64A64_UINT
  , pattern VK_FORMAT_R64G64B64_SFLOAT
  , pattern VK_FORMAT_R64G64B64_SINT
  , pattern VK_FORMAT_R64G64B64_UINT
  , pattern VK_FORMAT_R64G64_SFLOAT
  , pattern VK_FORMAT_R64G64_SINT
  , pattern VK_FORMAT_R64G64_UINT
  , pattern VK_FORMAT_R64_SFLOAT
  , pattern VK_FORMAT_R64_SINT
  , pattern VK_FORMAT_R64_UINT
  , pattern VK_FORMAT_R8G8B8A8_SINT
  , pattern VK_FORMAT_R8G8B8A8_SNORM
  , pattern VK_FORMAT_R8G8B8A8_SRGB
  , pattern VK_FORMAT_R8G8B8A8_SSCALED
  , pattern VK_FORMAT_R8G8B8A8_UINT
  , pattern VK_FORMAT_R8G8B8A8_UNORM
  , pattern VK_FORMAT_R8G8B8A8_USCALED
  , pattern VK_FORMAT_R8G8B8_SINT
  , pattern VK_FORMAT_R8G8B8_SNORM
  , pattern VK_FORMAT_R8G8B8_SRGB
  , pattern VK_FORMAT_R8G8B8_SSCALED
  , pattern VK_FORMAT_R8G8B8_UINT
  , pattern VK_FORMAT_R8G8B8_UNORM
  , pattern VK_FORMAT_R8G8B8_USCALED
  , pattern VK_FORMAT_R8G8_SINT
  , pattern VK_FORMAT_R8G8_SNORM
  , pattern VK_FORMAT_R8G8_SRGB
  , pattern VK_FORMAT_R8G8_SSCALED
  , pattern VK_FORMAT_R8G8_UINT
  , pattern VK_FORMAT_R8G8_UNORM
  , pattern VK_FORMAT_R8G8_USCALED
  , pattern VK_FORMAT_R8_SINT
  , pattern VK_FORMAT_R8_SNORM
  , pattern VK_FORMAT_R8_SRGB
  , pattern VK_FORMAT_R8_SSCALED
  , pattern VK_FORMAT_R8_UINT
  , pattern VK_FORMAT_R8_UNORM
  , pattern VK_FORMAT_R8_USCALED
  , pattern VK_FORMAT_S8_UINT
  , pattern VK_FORMAT_UNDEFINED
  , pattern VK_FORMAT_X8_D24_UNORM_PACK32
  , pattern VK_INCOMPLETE
  , pattern VK_NOT_READY
  , pattern VK_OBJECT_TYPE_BUFFER
  , pattern VK_OBJECT_TYPE_BUFFER_VIEW
  , pattern VK_OBJECT_TYPE_COMMAND_BUFFER
  , pattern VK_OBJECT_TYPE_COMMAND_POOL
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_POOL
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_SET
  , pattern VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT
  , pattern VK_OBJECT_TYPE_DEVICE
  , pattern VK_OBJECT_TYPE_DEVICE_MEMORY
  , pattern VK_OBJECT_TYPE_EVENT
  , pattern VK_OBJECT_TYPE_FENCE
  , pattern VK_OBJECT_TYPE_FRAMEBUFFER
  , pattern VK_OBJECT_TYPE_IMAGE
  , pattern VK_OBJECT_TYPE_IMAGE_VIEW
  , pattern VK_OBJECT_TYPE_INSTANCE
  , pattern VK_OBJECT_TYPE_PHYSICAL_DEVICE
  , pattern VK_OBJECT_TYPE_PIPELINE
  , pattern VK_OBJECT_TYPE_PIPELINE_CACHE
  , pattern VK_OBJECT_TYPE_PIPELINE_LAYOUT
  , pattern VK_OBJECT_TYPE_QUERY_POOL
  , pattern VK_OBJECT_TYPE_QUEUE
  , pattern VK_OBJECT_TYPE_RENDER_PASS
  , pattern VK_OBJECT_TYPE_SAMPLER
  , pattern VK_OBJECT_TYPE_SEMAPHORE
  , pattern VK_OBJECT_TYPE_SHADER_MODULE
  , pattern VK_OBJECT_TYPE_UNKNOWN
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
  , pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO
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
  , pattern VK_SUCCESS
  , pattern VK_TIMEOUT
  , pattern VK_TRUE
  , pattern VK_VENDOR_ID_KAZAN
  , pattern VK_VENDOR_ID_VIV
  , pattern VK_VENDOR_ID_VSI
  )
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )


-- | VkFormat - Available image formats
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pass.VkAttachmentDescription',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkVertexInputAttributeDescription',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.vkGetPhysicalDeviceFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.vkGetPhysicalDeviceImageFormatProperties',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.vkGetPhysicalDeviceSparseImageFormatProperties'
type Format = VkFormat


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_UNDEFINED' specifies that the
-- format is not specified.
pattern FORMAT_UNDEFINED :: (a ~ Format) => a
pattern FORMAT_UNDEFINED = VK_FORMAT_UNDEFINED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R4G4_UNORM_PACK8' specifies a
-- two-component, 8-bit packed unsigned normalized format that has a 4-bit
-- R component in bits 4..7, and a 4-bit G component in bits 0..3.
pattern FORMAT_R4G4_UNORM_PACK8 :: (a ~ Format) => a
pattern FORMAT_R4G4_UNORM_PACK8 = VK_FORMAT_R4G4_UNORM_PACK8


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R4G4B4A4_UNORM_PACK16'
-- specifies a four-component, 16-bit packed unsigned normalized format
-- that has a 4-bit R component in bits 12..15, a 4-bit G component in bits
-- 8..11, a 4-bit B component in bits 4..7, and a 4-bit A component in bits
-- 0..3.
pattern FORMAT_R4G4B4A4_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_R4G4B4A4_UNORM_PACK16 = VK_FORMAT_R4G4B4A4_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B4G4R4A4_UNORM_PACK16'
-- specifies a four-component, 16-bit packed unsigned normalized format
-- that has a 4-bit B component in bits 12..15, a 4-bit G component in bits
-- 8..11, a 4-bit R component in bits 4..7, and a 4-bit A component in bits
-- 0..3.
pattern FORMAT_B4G4R4A4_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_B4G4R4A4_UNORM_PACK16 = VK_FORMAT_B4G4R4A4_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R5G6B5_UNORM_PACK16' specifies
-- a three-component, 16-bit packed unsigned normalized format that has a
-- 5-bit R component in bits 11..15, a 6-bit G component in bits 5..10, and
-- a 5-bit B component in bits 0..4.
pattern FORMAT_R5G6B5_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_R5G6B5_UNORM_PACK16 = VK_FORMAT_R5G6B5_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B5G6R5_UNORM_PACK16' specifies
-- a three-component, 16-bit packed unsigned normalized format that has a
-- 5-bit B component in bits 11..15, a 6-bit G component in bits 5..10, and
-- a 5-bit R component in bits 0..4.
pattern FORMAT_B5G6R5_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_B5G6R5_UNORM_PACK16 = VK_FORMAT_B5G6R5_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R5G5B5A1_UNORM_PACK16'
-- specifies a four-component, 16-bit packed unsigned normalized format
-- that has a 5-bit R component in bits 11..15, a 5-bit G component in bits
-- 6..10, a 5-bit B component in bits 1..5, and a 1-bit A component in bit
-- 0.
pattern FORMAT_R5G5B5A1_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_R5G5B5A1_UNORM_PACK16 = VK_FORMAT_R5G5B5A1_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B5G5R5A1_UNORM_PACK16'
-- specifies a four-component, 16-bit packed unsigned normalized format
-- that has a 5-bit B component in bits 11..15, a 5-bit G component in bits
-- 6..10, a 5-bit R component in bits 1..5, and a 1-bit A component in bit
-- 0.
pattern FORMAT_B5G5R5A1_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_B5G5R5A1_UNORM_PACK16 = VK_FORMAT_B5G5R5A1_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A1R5G5B5_UNORM_PACK16'
-- specifies a four-component, 16-bit packed unsigned normalized format
-- that has a 1-bit A component in bit 15, a 5-bit R component in bits
-- 10..14, a 5-bit G component in bits 5..9, and a 5-bit B component in
-- bits 0..4.
pattern FORMAT_A1R5G5B5_UNORM_PACK16 :: (a ~ Format) => a
pattern FORMAT_A1R5G5B5_UNORM_PACK16 = VK_FORMAT_A1R5G5B5_UNORM_PACK16


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_UNORM' specifies a
-- one-component, 8-bit unsigned normalized format that has a single 8-bit
-- R component.
pattern FORMAT_R8_UNORM :: (a ~ Format) => a
pattern FORMAT_R8_UNORM = VK_FORMAT_R8_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_SNORM' specifies a
-- one-component, 8-bit signed normalized format that has a single 8-bit R
-- component.
pattern FORMAT_R8_SNORM :: (a ~ Format) => a
pattern FORMAT_R8_SNORM = VK_FORMAT_R8_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_USCALED' specifies a
-- one-component, 8-bit unsigned scaled integer format that has a single
-- 8-bit R component.
pattern FORMAT_R8_USCALED :: (a ~ Format) => a
pattern FORMAT_R8_USCALED = VK_FORMAT_R8_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_SSCALED' specifies a
-- one-component, 8-bit signed scaled integer format that has a single
-- 8-bit R component.
pattern FORMAT_R8_SSCALED :: (a ~ Format) => a
pattern FORMAT_R8_SSCALED = VK_FORMAT_R8_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_UINT' specifies a
-- one-component, 8-bit unsigned integer format that has a single 8-bit R
-- component.
pattern FORMAT_R8_UINT :: (a ~ Format) => a
pattern FORMAT_R8_UINT = VK_FORMAT_R8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_SINT' specifies a
-- one-component, 8-bit signed integer format that has a single 8-bit R
-- component.
pattern FORMAT_R8_SINT :: (a ~ Format) => a
pattern FORMAT_R8_SINT = VK_FORMAT_R8_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8_SRGB' specifies a
-- one-component, 8-bit unsigned normalized format that has a single 8-bit
-- R component stored with sRGB nonlinear encoding.
pattern FORMAT_R8_SRGB :: (a ~ Format) => a
pattern FORMAT_R8_SRGB = VK_FORMAT_R8_SRGB


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_UNORM' specifies a
-- two-component, 16-bit unsigned normalized format that has an 8-bit R
-- component in byte 0, and an 8-bit G component in byte 1.
pattern FORMAT_R8G8_UNORM :: (a ~ Format) => a
pattern FORMAT_R8G8_UNORM = VK_FORMAT_R8G8_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_SNORM' specifies a
-- two-component, 16-bit signed normalized format that has an 8-bit R
-- component in byte 0, and an 8-bit G component in byte 1.
pattern FORMAT_R8G8_SNORM :: (a ~ Format) => a
pattern FORMAT_R8G8_SNORM = VK_FORMAT_R8G8_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_USCALED' specifies a
-- two-component, 16-bit unsigned scaled integer format that has an 8-bit R
-- component in byte 0, and an 8-bit G component in byte 1.
pattern FORMAT_R8G8_USCALED :: (a ~ Format) => a
pattern FORMAT_R8G8_USCALED = VK_FORMAT_R8G8_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_SSCALED' specifies a
-- two-component, 16-bit signed scaled integer format that has an 8-bit R
-- component in byte 0, and an 8-bit G component in byte 1.
pattern FORMAT_R8G8_SSCALED :: (a ~ Format) => a
pattern FORMAT_R8G8_SSCALED = VK_FORMAT_R8G8_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_UINT' specifies a
-- two-component, 16-bit unsigned integer format that has an 8-bit R
-- component in byte 0, and an 8-bit G component in byte 1.
pattern FORMAT_R8G8_UINT :: (a ~ Format) => a
pattern FORMAT_R8G8_UINT = VK_FORMAT_R8G8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_SINT' specifies a
-- two-component, 16-bit signed integer format that has an 8-bit R
-- component in byte 0, and an 8-bit G component in byte 1.
pattern FORMAT_R8G8_SINT :: (a ~ Format) => a
pattern FORMAT_R8G8_SINT = VK_FORMAT_R8G8_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8_SRGB' specifies a
-- two-component, 16-bit unsigned normalized format that has an 8-bit R
-- component stored with sRGB nonlinear encoding in byte 0, and an 8-bit G
-- component stored with sRGB nonlinear encoding in byte 1.
pattern FORMAT_R8G8_SRGB :: (a ~ Format) => a
pattern FORMAT_R8G8_SRGB = VK_FORMAT_R8G8_SRGB


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_UNORM' specifies a
-- three-component, 24-bit unsigned normalized format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit B
-- component in byte 2.
pattern FORMAT_R8G8B8_UNORM :: (a ~ Format) => a
pattern FORMAT_R8G8B8_UNORM = VK_FORMAT_R8G8B8_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_SNORM' specifies a
-- three-component, 24-bit signed normalized format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit B
-- component in byte 2.
pattern FORMAT_R8G8B8_SNORM :: (a ~ Format) => a
pattern FORMAT_R8G8B8_SNORM = VK_FORMAT_R8G8B8_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_USCALED' specifies a
-- three-component, 24-bit unsigned scaled format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit B
-- component in byte 2.
pattern FORMAT_R8G8B8_USCALED :: (a ~ Format) => a
pattern FORMAT_R8G8B8_USCALED = VK_FORMAT_R8G8B8_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_SSCALED' specifies a
-- three-component, 24-bit signed scaled format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit B
-- component in byte 2.
pattern FORMAT_R8G8B8_SSCALED :: (a ~ Format) => a
pattern FORMAT_R8G8B8_SSCALED = VK_FORMAT_R8G8B8_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_UINT' specifies a
-- three-component, 24-bit unsigned integer format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit B
-- component in byte 2.
pattern FORMAT_R8G8B8_UINT :: (a ~ Format) => a
pattern FORMAT_R8G8B8_UINT = VK_FORMAT_R8G8B8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_SINT' specifies a
-- three-component, 24-bit signed integer format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit B
-- component in byte 2.
pattern FORMAT_R8G8B8_SINT :: (a ~ Format) => a
pattern FORMAT_R8G8B8_SINT = VK_FORMAT_R8G8B8_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8_SRGB' specifies a
-- three-component, 24-bit unsigned normalized format that has an 8-bit R
-- component stored with sRGB nonlinear encoding in byte 0, an 8-bit G
-- component stored with sRGB nonlinear encoding in byte 1, and an 8-bit B
-- component stored with sRGB nonlinear encoding in byte 2.
pattern FORMAT_R8G8B8_SRGB :: (a ~ Format) => a
pattern FORMAT_R8G8B8_SRGB = VK_FORMAT_R8G8B8_SRGB


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_UNORM' specifies a
-- three-component, 24-bit unsigned normalized format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit R
-- component in byte 2.
pattern FORMAT_B8G8R8_UNORM :: (a ~ Format) => a
pattern FORMAT_B8G8R8_UNORM = VK_FORMAT_B8G8R8_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_SNORM' specifies a
-- three-component, 24-bit signed normalized format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit R
-- component in byte 2.
pattern FORMAT_B8G8R8_SNORM :: (a ~ Format) => a
pattern FORMAT_B8G8R8_SNORM = VK_FORMAT_B8G8R8_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_USCALED' specifies a
-- three-component, 24-bit unsigned scaled format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit R
-- component in byte 2.
pattern FORMAT_B8G8R8_USCALED :: (a ~ Format) => a
pattern FORMAT_B8G8R8_USCALED = VK_FORMAT_B8G8R8_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_SSCALED' specifies a
-- three-component, 24-bit signed scaled format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit R
-- component in byte 2.
pattern FORMAT_B8G8R8_SSCALED :: (a ~ Format) => a
pattern FORMAT_B8G8R8_SSCALED = VK_FORMAT_B8G8R8_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_UINT' specifies a
-- three-component, 24-bit unsigned integer format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit R
-- component in byte 2.
pattern FORMAT_B8G8R8_UINT :: (a ~ Format) => a
pattern FORMAT_B8G8R8_UINT = VK_FORMAT_B8G8R8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_SINT' specifies a
-- three-component, 24-bit signed integer format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, and an 8-bit R
-- component in byte 2.
pattern FORMAT_B8G8R8_SINT :: (a ~ Format) => a
pattern FORMAT_B8G8R8_SINT = VK_FORMAT_B8G8R8_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8_SRGB' specifies a
-- three-component, 24-bit unsigned normalized format that has an 8-bit B
-- component stored with sRGB nonlinear encoding in byte 0, an 8-bit G
-- component stored with sRGB nonlinear encoding in byte 1, and an 8-bit R
-- component stored with sRGB nonlinear encoding in byte 2.
pattern FORMAT_B8G8R8_SRGB :: (a ~ Format) => a
pattern FORMAT_B8G8R8_SRGB = VK_FORMAT_B8G8R8_SRGB


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UNORM' specifies a
-- four-component, 32-bit unsigned normalized format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit B
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_UNORM :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_UNORM = VK_FORMAT_R8G8B8A8_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_SNORM' specifies a
-- four-component, 32-bit signed normalized format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit B
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_SNORM :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_SNORM = VK_FORMAT_R8G8B8A8_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_USCALED' specifies a
-- four-component, 32-bit unsigned scaled format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit B
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_USCALED :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_USCALED = VK_FORMAT_R8G8B8A8_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_SSCALED' specifies a
-- four-component, 32-bit signed scaled format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit B
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_SSCALED :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_SSCALED = VK_FORMAT_R8G8B8A8_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_UINT' specifies a
-- four-component, 32-bit unsigned integer format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit B
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_UINT :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_UINT = VK_FORMAT_R8G8B8A8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_SINT' specifies a
-- four-component, 32-bit signed integer format that has an 8-bit R
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit B
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_SINT :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_SINT = VK_FORMAT_R8G8B8A8_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R8G8B8A8_SRGB' specifies a
-- four-component, 32-bit unsigned normalized format that has an 8-bit R
-- component stored with sRGB nonlinear encoding in byte 0, an 8-bit G
-- component stored with sRGB nonlinear encoding in byte 1, an 8-bit B
-- component stored with sRGB nonlinear encoding in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_R8G8B8A8_SRGB :: (a ~ Format) => a
pattern FORMAT_R8G8B8A8_SRGB = VK_FORMAT_R8G8B8A8_SRGB


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_UNORM' specifies a
-- four-component, 32-bit unsigned normalized format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit R
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_UNORM :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_UNORM = VK_FORMAT_B8G8R8A8_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_SNORM' specifies a
-- four-component, 32-bit signed normalized format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit R
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_SNORM :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_SNORM = VK_FORMAT_B8G8R8A8_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_USCALED' specifies a
-- four-component, 32-bit unsigned scaled format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit R
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_USCALED :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_USCALED = VK_FORMAT_B8G8R8A8_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_SSCALED' specifies a
-- four-component, 32-bit signed scaled format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit R
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_SSCALED :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_SSCALED = VK_FORMAT_B8G8R8A8_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_UINT' specifies a
-- four-component, 32-bit unsigned integer format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit R
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_UINT :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_UINT = VK_FORMAT_B8G8R8A8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_SINT' specifies a
-- four-component, 32-bit signed integer format that has an 8-bit B
-- component in byte 0, an 8-bit G component in byte 1, an 8-bit R
-- component in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_SINT :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_SINT = VK_FORMAT_B8G8R8A8_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B8G8R8A8_SRGB' specifies a
-- four-component, 32-bit unsigned normalized format that has an 8-bit B
-- component stored with sRGB nonlinear encoding in byte 0, an 8-bit G
-- component stored with sRGB nonlinear encoding in byte 1, an 8-bit R
-- component stored with sRGB nonlinear encoding in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_B8G8R8A8_SRGB :: (a ~ Format) => a
pattern FORMAT_B8G8R8A8_SRGB = VK_FORMAT_B8G8R8A8_SRGB


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_UNORM_PACK32'
-- specifies a four-component, 32-bit packed unsigned normalized format
-- that has an 8-bit A component in bits 24..31, an 8-bit B component in
-- bits 16..23, an 8-bit G component in bits 8..15, and an 8-bit R
-- component in bits 0..7.
pattern FORMAT_A8B8G8R8_UNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_UNORM_PACK32 = VK_FORMAT_A8B8G8R8_UNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_SNORM_PACK32'
-- specifies a four-component, 32-bit packed signed normalized format that
-- has an 8-bit A component in bits 24..31, an 8-bit B component in bits
-- 16..23, an 8-bit G component in bits 8..15, and an 8-bit R component in
-- bits 0..7.
pattern FORMAT_A8B8G8R8_SNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_SNORM_PACK32 = VK_FORMAT_A8B8G8R8_SNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_USCALED_PACK32'
-- specifies a four-component, 32-bit packed unsigned scaled integer format
-- that has an 8-bit A component in bits 24..31, an 8-bit B component in
-- bits 16..23, an 8-bit G component in bits 8..15, and an 8-bit R
-- component in bits 0..7.
pattern FORMAT_A8B8G8R8_USCALED_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_USCALED_PACK32 = VK_FORMAT_A8B8G8R8_USCALED_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_SSCALED_PACK32'
-- specifies a four-component, 32-bit packed signed scaled integer format
-- that has an 8-bit A component in bits 24..31, an 8-bit B component in
-- bits 16..23, an 8-bit G component in bits 8..15, and an 8-bit R
-- component in bits 0..7.
pattern FORMAT_A8B8G8R8_SSCALED_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_SSCALED_PACK32 = VK_FORMAT_A8B8G8R8_SSCALED_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_UINT_PACK32' specifies
-- a four-component, 32-bit packed unsigned integer format that has an
-- 8-bit A component in bits 24..31, an 8-bit B component in bits 16..23,
-- an 8-bit G component in bits 8..15, and an 8-bit R component in bits
-- 0..7.
pattern FORMAT_A8B8G8R8_UINT_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_UINT_PACK32 = VK_FORMAT_A8B8G8R8_UINT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_SINT_PACK32' specifies
-- a four-component, 32-bit packed signed integer format that has an 8-bit
-- A component in bits 24..31, an 8-bit B component in bits 16..23, an
-- 8-bit G component in bits 8..15, and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_SINT_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_SINT_PACK32 = VK_FORMAT_A8B8G8R8_SINT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A8B8G8R8_SRGB_PACK32' specifies
-- a four-component, 32-bit packed unsigned normalized format that has an
-- 8-bit A component in bits 24..31, an 8-bit B component stored with sRGB
-- nonlinear encoding in bits 16..23, an 8-bit G component stored with sRGB
-- nonlinear encoding in bits 8..15, and an 8-bit R component stored with
-- sRGB nonlinear encoding in bits 0..7.
pattern FORMAT_A8B8G8R8_SRGB_PACK32 :: (a ~ Format) => a
pattern FORMAT_A8B8G8R8_SRGB_PACK32 = VK_FORMAT_A8B8G8R8_SRGB_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2R10G10B10_UNORM_PACK32'
-- specifies a four-component, 32-bit packed unsigned normalized format
-- that has a 2-bit A component in bits 30..31, a 10-bit R component in
-- bits 20..29, a 10-bit G component in bits 10..19, and a 10-bit B
-- component in bits 0..9.
pattern FORMAT_A2R10G10B10_UNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2R10G10B10_UNORM_PACK32 = VK_FORMAT_A2R10G10B10_UNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2R10G10B10_SNORM_PACK32'
-- specifies a four-component, 32-bit packed signed normalized format that
-- has a 2-bit A component in bits 30..31, a 10-bit R component in bits
-- 20..29, a 10-bit G component in bits 10..19, and a 10-bit B component in
-- bits 0..9.
pattern FORMAT_A2R10G10B10_SNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2R10G10B10_SNORM_PACK32 = VK_FORMAT_A2R10G10B10_SNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2R10G10B10_USCALED_PACK32'
-- specifies a four-component, 32-bit packed unsigned scaled integer format
-- that has a 2-bit A component in bits 30..31, a 10-bit R component in
-- bits 20..29, a 10-bit G component in bits 10..19, and a 10-bit B
-- component in bits 0..9.
pattern FORMAT_A2R10G10B10_USCALED_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2R10G10B10_USCALED_PACK32 = VK_FORMAT_A2R10G10B10_USCALED_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2R10G10B10_SSCALED_PACK32'
-- specifies a four-component, 32-bit packed signed scaled integer format
-- that has a 2-bit A component in bits 30..31, a 10-bit R component in
-- bits 20..29, a 10-bit G component in bits 10..19, and a 10-bit B
-- component in bits 0..9.
pattern FORMAT_A2R10G10B10_SSCALED_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2R10G10B10_SSCALED_PACK32 = VK_FORMAT_A2R10G10B10_SSCALED_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2R10G10B10_UINT_PACK32'
-- specifies a four-component, 32-bit packed unsigned integer format that
-- has a 2-bit A component in bits 30..31, a 10-bit R component in bits
-- 20..29, a 10-bit G component in bits 10..19, and a 10-bit B component in
-- bits 0..9.
pattern FORMAT_A2R10G10B10_UINT_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2R10G10B10_UINT_PACK32 = VK_FORMAT_A2R10G10B10_UINT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2R10G10B10_SINT_PACK32'
-- specifies a four-component, 32-bit packed signed integer format that has
-- a 2-bit A component in bits 30..31, a 10-bit R component in bits 20..29,
-- a 10-bit G component in bits 10..19, and a 10-bit B component in bits
-- 0..9.
pattern FORMAT_A2R10G10B10_SINT_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2R10G10B10_SINT_PACK32 = VK_FORMAT_A2R10G10B10_SINT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2B10G10R10_UNORM_PACK32'
-- specifies a four-component, 32-bit packed unsigned normalized format
-- that has a 2-bit A component in bits 30..31, a 10-bit B component in
-- bits 20..29, a 10-bit G component in bits 10..19, and a 10-bit R
-- component in bits 0..9.
pattern FORMAT_A2B10G10R10_UNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2B10G10R10_UNORM_PACK32 = VK_FORMAT_A2B10G10R10_UNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2B10G10R10_SNORM_PACK32'
-- specifies a four-component, 32-bit packed signed normalized format that
-- has a 2-bit A component in bits 30..31, a 10-bit B component in bits
-- 20..29, a 10-bit G component in bits 10..19, and a 10-bit R component in
-- bits 0..9.
pattern FORMAT_A2B10G10R10_SNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2B10G10R10_SNORM_PACK32 = VK_FORMAT_A2B10G10R10_SNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2B10G10R10_USCALED_PACK32'
-- specifies a four-component, 32-bit packed unsigned scaled integer format
-- that has a 2-bit A component in bits 30..31, a 10-bit B component in
-- bits 20..29, a 10-bit G component in bits 10..19, and a 10-bit R
-- component in bits 0..9.
pattern FORMAT_A2B10G10R10_USCALED_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2B10G10R10_USCALED_PACK32 = VK_FORMAT_A2B10G10R10_USCALED_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2B10G10R10_SSCALED_PACK32'
-- specifies a four-component, 32-bit packed signed scaled integer format
-- that has a 2-bit A component in bits 30..31, a 10-bit B component in
-- bits 20..29, a 10-bit G component in bits 10..19, and a 10-bit R
-- component in bits 0..9.
pattern FORMAT_A2B10G10R10_SSCALED_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2B10G10R10_SSCALED_PACK32 = VK_FORMAT_A2B10G10R10_SSCALED_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2B10G10R10_UINT_PACK32'
-- specifies a four-component, 32-bit packed unsigned integer format that
-- has a 2-bit A component in bits 30..31, a 10-bit B component in bits
-- 20..29, a 10-bit G component in bits 10..19, and a 10-bit R component in
-- bits 0..9.
pattern FORMAT_A2B10G10R10_UINT_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2B10G10R10_UINT_PACK32 = VK_FORMAT_A2B10G10R10_UINT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_A2B10G10R10_SINT_PACK32'
-- specifies a four-component, 32-bit packed signed integer format that has
-- a 2-bit A component in bits 30..31, a 10-bit B component in bits 20..29,
-- a 10-bit G component in bits 10..19, and a 10-bit R component in bits
-- 0..9.
pattern FORMAT_A2B10G10R10_SINT_PACK32 :: (a ~ Format) => a
pattern FORMAT_A2B10G10R10_SINT_PACK32 = VK_FORMAT_A2B10G10R10_SINT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_UNORM' specifies a
-- one-component, 16-bit unsigned normalized format that has a single
-- 16-bit R component.
pattern FORMAT_R16_UNORM :: (a ~ Format) => a
pattern FORMAT_R16_UNORM = VK_FORMAT_R16_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_SNORM' specifies a
-- one-component, 16-bit signed normalized format that has a single 16-bit
-- R component.
pattern FORMAT_R16_SNORM :: (a ~ Format) => a
pattern FORMAT_R16_SNORM = VK_FORMAT_R16_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_USCALED' specifies a
-- one-component, 16-bit unsigned scaled integer format that has a single
-- 16-bit R component.
pattern FORMAT_R16_USCALED :: (a ~ Format) => a
pattern FORMAT_R16_USCALED = VK_FORMAT_R16_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_SSCALED' specifies a
-- one-component, 16-bit signed scaled integer format that has a single
-- 16-bit R component.
pattern FORMAT_R16_SSCALED :: (a ~ Format) => a
pattern FORMAT_R16_SSCALED = VK_FORMAT_R16_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_UINT' specifies a
-- one-component, 16-bit unsigned integer format that has a single 16-bit R
-- component.
pattern FORMAT_R16_UINT :: (a ~ Format) => a
pattern FORMAT_R16_UINT = VK_FORMAT_R16_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_SINT' specifies a
-- one-component, 16-bit signed integer format that has a single 16-bit R
-- component.
pattern FORMAT_R16_SINT :: (a ~ Format) => a
pattern FORMAT_R16_SINT = VK_FORMAT_R16_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16_SFLOAT' specifies a
-- one-component, 16-bit signed floating-point format that has a single
-- 16-bit R component.
pattern FORMAT_R16_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R16_SFLOAT = VK_FORMAT_R16_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_UNORM' specifies a
-- two-component, 32-bit unsigned normalized format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_UNORM :: (a ~ Format) => a
pattern FORMAT_R16G16_UNORM = VK_FORMAT_R16G16_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SNORM' specifies a
-- two-component, 32-bit signed normalized format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_SNORM :: (a ~ Format) => a
pattern FORMAT_R16G16_SNORM = VK_FORMAT_R16G16_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_USCALED' specifies a
-- two-component, 32-bit unsigned scaled integer format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_USCALED :: (a ~ Format) => a
pattern FORMAT_R16G16_USCALED = VK_FORMAT_R16G16_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SSCALED' specifies a
-- two-component, 32-bit signed scaled integer format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_SSCALED :: (a ~ Format) => a
pattern FORMAT_R16G16_SSCALED = VK_FORMAT_R16G16_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_UINT' specifies a
-- two-component, 32-bit unsigned integer format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_UINT :: (a ~ Format) => a
pattern FORMAT_R16G16_UINT = VK_FORMAT_R16G16_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SINT' specifies a
-- two-component, 32-bit signed integer format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_SINT :: (a ~ Format) => a
pattern FORMAT_R16G16_SINT = VK_FORMAT_R16G16_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16_SFLOAT' specifies a
-- two-component, 32-bit signed floating-point format that has a 16-bit R
-- component in bytes 0..1, and a 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R16G16_SFLOAT = VK_FORMAT_R16G16_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_UNORM' specifies a
-- three-component, 48-bit unsigned normalized format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_UNORM :: (a ~ Format) => a
pattern FORMAT_R16G16B16_UNORM = VK_FORMAT_R16G16B16_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SNORM' specifies a
-- three-component, 48-bit signed normalized format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_SNORM :: (a ~ Format) => a
pattern FORMAT_R16G16B16_SNORM = VK_FORMAT_R16G16B16_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_USCALED' specifies a
-- three-component, 48-bit unsigned scaled integer format that has a 16-bit
-- R component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_USCALED :: (a ~ Format) => a
pattern FORMAT_R16G16B16_USCALED = VK_FORMAT_R16G16B16_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SSCALED' specifies a
-- three-component, 48-bit signed scaled integer format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_SSCALED :: (a ~ Format) => a
pattern FORMAT_R16G16B16_SSCALED = VK_FORMAT_R16G16B16_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_UINT' specifies a
-- three-component, 48-bit unsigned integer format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_UINT :: (a ~ Format) => a
pattern FORMAT_R16G16B16_UINT = VK_FORMAT_R16G16B16_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SINT' specifies a
-- three-component, 48-bit signed integer format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_SINT :: (a ~ Format) => a
pattern FORMAT_R16G16B16_SINT = VK_FORMAT_R16G16B16_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16_SFLOAT' specifies a
-- three-component, 48-bit signed floating-point format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, and a
-- 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R16G16B16_SFLOAT = VK_FORMAT_R16G16B16_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_UNORM' specifies a
-- four-component, 64-bit unsigned normalized format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, a 16-bit B
-- component in bytes 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_UNORM :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_UNORM = VK_FORMAT_R16G16B16A16_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_SNORM' specifies a
-- four-component, 64-bit signed normalized format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, a 16-bit B
-- component in bytes 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SNORM :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_SNORM = VK_FORMAT_R16G16B16A16_SNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_USCALED' specifies
-- a four-component, 64-bit unsigned scaled integer format that has a
-- 16-bit R component in bytes 0..1, a 16-bit G component in bytes 2..3, a
-- 16-bit B component in bytes 4..5, and a 16-bit A component in bytes
-- 6..7.
pattern FORMAT_R16G16B16A16_USCALED :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_USCALED = VK_FORMAT_R16G16B16A16_USCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_SSCALED' specifies
-- a four-component, 64-bit signed scaled integer format that has a 16-bit
-- R component in bytes 0..1, a 16-bit G component in bytes 2..3, a 16-bit
-- B component in bytes 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SSCALED :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_SSCALED = VK_FORMAT_R16G16B16A16_SSCALED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_UINT' specifies a
-- four-component, 64-bit unsigned integer format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, a 16-bit B
-- component in bytes 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_UINT :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_UINT = VK_FORMAT_R16G16B16A16_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_SINT' specifies a
-- four-component, 64-bit signed integer format that has a 16-bit R
-- component in bytes 0..1, a 16-bit G component in bytes 2..3, a 16-bit B
-- component in bytes 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SINT :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_SINT = VK_FORMAT_R16G16B16A16_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R16G16B16A16_SFLOAT' specifies
-- a four-component, 64-bit signed floating-point format that has a 16-bit
-- R component in bytes 0..1, a 16-bit G component in bytes 2..3, a 16-bit
-- B component in bytes 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R16G16B16A16_SFLOAT = VK_FORMAT_R16G16B16A16_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32_UINT' specifies a
-- one-component, 32-bit unsigned integer format that has a single 32-bit R
-- component.
pattern FORMAT_R32_UINT :: (a ~ Format) => a
pattern FORMAT_R32_UINT = VK_FORMAT_R32_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32_SINT' specifies a
-- one-component, 32-bit signed integer format that has a single 32-bit R
-- component.
pattern FORMAT_R32_SINT :: (a ~ Format) => a
pattern FORMAT_R32_SINT = VK_FORMAT_R32_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32_SFLOAT' specifies a
-- one-component, 32-bit signed floating-point format that has a single
-- 32-bit R component.
pattern FORMAT_R32_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R32_SFLOAT = VK_FORMAT_R32_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32_UINT' specifies a
-- two-component, 64-bit unsigned integer format that has a 32-bit R
-- component in bytes 0..3, and a 32-bit G component in bytes 4..7.
pattern FORMAT_R32G32_UINT :: (a ~ Format) => a
pattern FORMAT_R32G32_UINT = VK_FORMAT_R32G32_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32_SINT' specifies a
-- two-component, 64-bit signed integer format that has a 32-bit R
-- component in bytes 0..3, and a 32-bit G component in bytes 4..7.
pattern FORMAT_R32G32_SINT :: (a ~ Format) => a
pattern FORMAT_R32G32_SINT = VK_FORMAT_R32G32_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32_SFLOAT' specifies a
-- two-component, 64-bit signed floating-point format that has a 32-bit R
-- component in bytes 0..3, and a 32-bit G component in bytes 4..7.
pattern FORMAT_R32G32_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R32G32_SFLOAT = VK_FORMAT_R32G32_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32_UINT' specifies a
-- three-component, 96-bit unsigned integer format that has a 32-bit R
-- component in bytes 0..3, a 32-bit G component in bytes 4..7, and a
-- 32-bit B component in bytes 8..11.
pattern FORMAT_R32G32B32_UINT :: (a ~ Format) => a
pattern FORMAT_R32G32B32_UINT = VK_FORMAT_R32G32B32_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32_SINT' specifies a
-- three-component, 96-bit signed integer format that has a 32-bit R
-- component in bytes 0..3, a 32-bit G component in bytes 4..7, and a
-- 32-bit B component in bytes 8..11.
pattern FORMAT_R32G32B32_SINT :: (a ~ Format) => a
pattern FORMAT_R32G32B32_SINT = VK_FORMAT_R32G32B32_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32_SFLOAT' specifies a
-- three-component, 96-bit signed floating-point format that has a 32-bit R
-- component in bytes 0..3, a 32-bit G component in bytes 4..7, and a
-- 32-bit B component in bytes 8..11.
pattern FORMAT_R32G32B32_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R32G32B32_SFLOAT = VK_FORMAT_R32G32B32_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32A32_UINT' specifies a
-- four-component, 128-bit unsigned integer format that has a 32-bit R
-- component in bytes 0..3, a 32-bit G component in bytes 4..7, a 32-bit B
-- component in bytes 8..11, and a 32-bit A component in bytes 12..15.
pattern FORMAT_R32G32B32A32_UINT :: (a ~ Format) => a
pattern FORMAT_R32G32B32A32_UINT = VK_FORMAT_R32G32B32A32_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32A32_SINT' specifies a
-- four-component, 128-bit signed integer format that has a 32-bit R
-- component in bytes 0..3, a 32-bit G component in bytes 4..7, a 32-bit B
-- component in bytes 8..11, and a 32-bit A component in bytes 12..15.
pattern FORMAT_R32G32B32A32_SINT :: (a ~ Format) => a
pattern FORMAT_R32G32B32A32_SINT = VK_FORMAT_R32G32B32A32_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R32G32B32A32_SFLOAT' specifies
-- a four-component, 128-bit signed floating-point format that has a 32-bit
-- R component in bytes 0..3, a 32-bit G component in bytes 4..7, a 32-bit
-- B component in bytes 8..11, and a 32-bit A component in bytes 12..15.
pattern FORMAT_R32G32B32A32_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R32G32B32A32_SFLOAT = VK_FORMAT_R32G32B32A32_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64_UINT' specifies a
-- one-component, 64-bit unsigned integer format that has a single 64-bit R
-- component.
pattern FORMAT_R64_UINT :: (a ~ Format) => a
pattern FORMAT_R64_UINT = VK_FORMAT_R64_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64_SINT' specifies a
-- one-component, 64-bit signed integer format that has a single 64-bit R
-- component.
pattern FORMAT_R64_SINT :: (a ~ Format) => a
pattern FORMAT_R64_SINT = VK_FORMAT_R64_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64_SFLOAT' specifies a
-- one-component, 64-bit signed floating-point format that has a single
-- 64-bit R component.
pattern FORMAT_R64_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R64_SFLOAT = VK_FORMAT_R64_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64_UINT' specifies a
-- two-component, 128-bit unsigned integer format that has a 64-bit R
-- component in bytes 0..7, and a 64-bit G component in bytes 8..15.
pattern FORMAT_R64G64_UINT :: (a ~ Format) => a
pattern FORMAT_R64G64_UINT = VK_FORMAT_R64G64_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64_SINT' specifies a
-- two-component, 128-bit signed integer format that has a 64-bit R
-- component in bytes 0..7, and a 64-bit G component in bytes 8..15.
pattern FORMAT_R64G64_SINT :: (a ~ Format) => a
pattern FORMAT_R64G64_SINT = VK_FORMAT_R64G64_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64_SFLOAT' specifies a
-- two-component, 128-bit signed floating-point format that has a 64-bit R
-- component in bytes 0..7, and a 64-bit G component in bytes 8..15.
pattern FORMAT_R64G64_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R64G64_SFLOAT = VK_FORMAT_R64G64_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64B64_UINT' specifies a
-- three-component, 192-bit unsigned integer format that has a 64-bit R
-- component in bytes 0..7, a 64-bit G component in bytes 8..15, and a
-- 64-bit B component in bytes 16..23.
pattern FORMAT_R64G64B64_UINT :: (a ~ Format) => a
pattern FORMAT_R64G64B64_UINT = VK_FORMAT_R64G64B64_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64B64_SINT' specifies a
-- three-component, 192-bit signed integer format that has a 64-bit R
-- component in bytes 0..7, a 64-bit G component in bytes 8..15, and a
-- 64-bit B component in bytes 16..23.
pattern FORMAT_R64G64B64_SINT :: (a ~ Format) => a
pattern FORMAT_R64G64B64_SINT = VK_FORMAT_R64G64B64_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64B64_SFLOAT' specifies a
-- three-component, 192-bit signed floating-point format that has a 64-bit
-- R component in bytes 0..7, a 64-bit G component in bytes 8..15, and a
-- 64-bit B component in bytes 16..23.
pattern FORMAT_R64G64B64_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R64G64B64_SFLOAT = VK_FORMAT_R64G64B64_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64B64A64_UINT' specifies a
-- four-component, 256-bit unsigned integer format that has a 64-bit R
-- component in bytes 0..7, a 64-bit G component in bytes 8..15, a 64-bit B
-- component in bytes 16..23, and a 64-bit A component in bytes 24..31.
pattern FORMAT_R64G64B64A64_UINT :: (a ~ Format) => a
pattern FORMAT_R64G64B64A64_UINT = VK_FORMAT_R64G64B64A64_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64B64A64_SINT' specifies a
-- four-component, 256-bit signed integer format that has a 64-bit R
-- component in bytes 0..7, a 64-bit G component in bytes 8..15, a 64-bit B
-- component in bytes 16..23, and a 64-bit A component in bytes 24..31.
pattern FORMAT_R64G64B64A64_SINT :: (a ~ Format) => a
pattern FORMAT_R64G64B64A64_SINT = VK_FORMAT_R64G64B64A64_SINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_R64G64B64A64_SFLOAT' specifies
-- a four-component, 256-bit signed floating-point format that has a 64-bit
-- R component in bytes 0..7, a 64-bit G component in bytes 8..15, a 64-bit
-- B component in bytes 16..23, and a 64-bit A component in bytes 24..31.
pattern FORMAT_R64G64B64A64_SFLOAT :: (a ~ Format) => a
pattern FORMAT_R64G64B64A64_SFLOAT = VK_FORMAT_R64G64B64A64_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_B10G11R11_UFLOAT_PACK32'
-- specifies a three-component, 32-bit packed unsigned floating-point
-- format that has a 10-bit B component in bits 22..31, an 11-bit G
-- component in bits 11..21, an 11-bit R component in bits 0..10. See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fundamentals-fp10>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#fundamentals-fp11>.
pattern FORMAT_B10G11R11_UFLOAT_PACK32 :: (a ~ Format) => a
pattern FORMAT_B10G11R11_UFLOAT_PACK32 = VK_FORMAT_B10G11R11_UFLOAT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_E5B9G9R9_UFLOAT_PACK32'
-- specifies a three-component, 32-bit packed unsigned floating-point
-- format that has a 5-bit shared exponent in bits 27..31, a 9-bit B
-- component mantissa in bits 18..26, a 9-bit G component mantissa in bits
-- 9..17, and a 9-bit R component mantissa in bits 0..8.
pattern FORMAT_E5B9G9R9_UFLOAT_PACK32 :: (a ~ Format) => a
pattern FORMAT_E5B9G9R9_UFLOAT_PACK32 = VK_FORMAT_E5B9G9R9_UFLOAT_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D16_UNORM' specifies a
-- one-component, 16-bit unsigned normalized format that has a single
-- 16-bit depth component.
pattern FORMAT_D16_UNORM :: (a ~ Format) => a
pattern FORMAT_D16_UNORM = VK_FORMAT_D16_UNORM


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_X8_D24_UNORM_PACK32' specifies
-- a two-component, 32-bit format that has 24 unsigned normalized bits in
-- the depth component and, optionally:, 8 bits that are unused.
pattern FORMAT_X8_D24_UNORM_PACK32 :: (a ~ Format) => a
pattern FORMAT_X8_D24_UNORM_PACK32 = VK_FORMAT_X8_D24_UNORM_PACK32


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D32_SFLOAT' specifies a
-- one-component, 32-bit signed floating-point format that has 32-bits in
-- the depth component.
pattern FORMAT_D32_SFLOAT :: (a ~ Format) => a
pattern FORMAT_D32_SFLOAT = VK_FORMAT_D32_SFLOAT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_S8_UINT' specifies a
-- one-component, 8-bit unsigned integer format that has 8-bits in the
-- stencil component.
pattern FORMAT_S8_UINT :: (a ~ Format) => a
pattern FORMAT_S8_UINT = VK_FORMAT_S8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D16_UNORM_S8_UINT' specifies a
-- two-component, 24-bit format that has 16 unsigned normalized bits in the
-- depth component and 8 unsigned integer bits in the stencil component.
pattern FORMAT_D16_UNORM_S8_UINT :: (a ~ Format) => a
pattern FORMAT_D16_UNORM_S8_UINT = VK_FORMAT_D16_UNORM_S8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D24_UNORM_S8_UINT' specifies a
-- two-component, 32-bit packed format that has 8 unsigned integer bits in
-- the stencil component, and 24 unsigned normalized bits in the depth
-- component.
pattern FORMAT_D24_UNORM_S8_UINT :: (a ~ Format) => a
pattern FORMAT_D24_UNORM_S8_UINT = VK_FORMAT_D24_UNORM_S8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_D32_SFLOAT_S8_UINT' specifies a
-- two-component format that has 32 signed float bits in the depth
-- component and 8 unsigned integer bits in the stencil component. There
-- are optionally: 24-bits that are unused.
pattern FORMAT_D32_SFLOAT_S8_UINT :: (a ~ Format) => a
pattern FORMAT_D32_SFLOAT_S8_UINT = VK_FORMAT_D32_SFLOAT_S8_UINT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGB_UNORM_BLOCK' specifies
-- a three-component, block-compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGB texel
-- data. This format has no alpha and is considered opaque.
pattern FORMAT_BC1_RGB_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC1_RGB_UNORM_BLOCK = VK_FORMAT_BC1_RGB_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGB_SRGB_BLOCK' specifies a
-- three-component, block-compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGB texel
-- data with sRGB nonlinear encoding. This format has no alpha and is
-- considered opaque.
pattern FORMAT_BC1_RGB_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC1_RGB_SRGB_BLOCK = VK_FORMAT_BC1_RGB_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGBA_UNORM_BLOCK' specifies
-- a four-component, block-compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGB texel
-- data, and provides 1 bit of alpha.
pattern FORMAT_BC1_RGBA_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC1_RGBA_UNORM_BLOCK = VK_FORMAT_BC1_RGBA_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC1_RGBA_SRGB_BLOCK' specifies
-- a four-component, block-compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGB texel
-- data with sRGB nonlinear encoding, and provides 1 bit of alpha.
pattern FORMAT_BC1_RGBA_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC1_RGBA_SRGB_BLOCK = VK_FORMAT_BC1_RGBA_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC2_UNORM_BLOCK' specifies a
-- four-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data with the first 64 bits encoding alpha values followed by 64 bits
-- encoding RGB values.
pattern FORMAT_BC2_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC2_UNORM_BLOCK = VK_FORMAT_BC2_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC2_SRGB_BLOCK' specifies a
-- four-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data with the first 64 bits encoding alpha values followed by 64 bits
-- encoding RGB values with sRGB nonlinear encoding.
pattern FORMAT_BC2_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC2_SRGB_BLOCK = VK_FORMAT_BC2_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC3_UNORM_BLOCK' specifies a
-- four-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data with the first 64 bits encoding alpha values followed by 64 bits
-- encoding RGB values.
pattern FORMAT_BC3_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC3_UNORM_BLOCK = VK_FORMAT_BC3_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC3_SRGB_BLOCK' specifies a
-- four-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data with the first 64 bits encoding alpha values followed by 64 bits
-- encoding RGB values with sRGB nonlinear encoding.
pattern FORMAT_BC3_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC3_SRGB_BLOCK = VK_FORMAT_BC3_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC4_UNORM_BLOCK' specifies a
-- one-component, block-compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized red texel
-- data.
pattern FORMAT_BC4_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC4_UNORM_BLOCK = VK_FORMAT_BC4_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC4_SNORM_BLOCK' specifies a
-- one-component, block-compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of signed normalized red texel data.
pattern FORMAT_BC4_SNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC4_SNORM_BLOCK = VK_FORMAT_BC4_SNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC5_UNORM_BLOCK' specifies a
-- two-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RG texel data
-- with the first 64 bits encoding red values followed by 64 bits encoding
-- green values.
pattern FORMAT_BC5_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC5_UNORM_BLOCK = VK_FORMAT_BC5_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC5_SNORM_BLOCK' specifies a
-- two-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of signed normalized RG texel data
-- with the first 64 bits encoding red values followed by 64 bits encoding
-- green values.
pattern FORMAT_BC5_SNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC5_SNORM_BLOCK = VK_FORMAT_BC5_SNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC6H_UFLOAT_BLOCK' specifies a
-- three-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned floating-point RGB texel
-- data.
pattern FORMAT_BC6H_UFLOAT_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC6H_UFLOAT_BLOCK = VK_FORMAT_BC6H_UFLOAT_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC6H_SFLOAT_BLOCK' specifies a
-- three-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of signed floating-point RGB texel
-- data.
pattern FORMAT_BC6H_SFLOAT_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC6H_SFLOAT_BLOCK = VK_FORMAT_BC6H_SFLOAT_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC7_UNORM_BLOCK' specifies a
-- four-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_BC7_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC7_UNORM_BLOCK = VK_FORMAT_BC7_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_BC7_SRGB_BLOCK' specifies a
-- four-component, block-compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_BC7_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_BC7_SRGB_BLOCK = VK_FORMAT_BC7_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK'
-- specifies a three-component, ETC2 compressed format where each 64-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized
-- RGB texel data. This format has no alpha and is considered opaque.
pattern FORMAT_ETC2_R8G8B8_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK'
-- specifies a three-component, ETC2 compressed format where each 64-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized
-- RGB texel data with sRGB nonlinear encoding. This format has no alpha
-- and is considered opaque.
pattern FORMAT_ETC2_R8G8B8_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK'
-- specifies a four-component, ETC2 compressed format where each 64-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized
-- RGB texel data, and provides 1 bit of alpha.
pattern FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK'
-- specifies a four-component, ETC2 compressed format where each 64-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized
-- RGB texel data with sRGB nonlinear encoding, and provides 1 bit of
-- alpha.
pattern FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK'
-- specifies a four-component, ETC2 compressed format where each 128-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized
-- RGBA texel data with the first 64 bits encoding alpha values followed by
-- 64 bits encoding RGB values.
pattern FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK'
-- specifies a four-component, ETC2 compressed format where each 128-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized
-- RGBA texel data with the first 64 bits encoding alpha values followed by
-- 64 bits encoding RGB values with sRGB nonlinear encoding applied.
pattern FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11_UNORM_BLOCK' specifies
-- a one-component, ETC2 compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized red texel
-- data.
pattern FORMAT_EAC_R11_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_EAC_R11_UNORM_BLOCK = VK_FORMAT_EAC_R11_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11_SNORM_BLOCK' specifies
-- a one-component, ETC2 compressed format where each 64-bit compressed
-- texel block encodes a 4×4 rectangle of signed normalized red texel data.
pattern FORMAT_EAC_R11_SNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_EAC_R11_SNORM_BLOCK = VK_FORMAT_EAC_R11_SNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11G11_UNORM_BLOCK'
-- specifies a two-component, ETC2 compressed format where each 128-bit
-- compressed texel block encodes a 4×4 rectangle of unsigned normalized RG
-- texel data with the first 64 bits encoding red values followed by 64
-- bits encoding green values.
pattern FORMAT_EAC_R11G11_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_EAC_R11G11_UNORM_BLOCK = VK_FORMAT_EAC_R11G11_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_EAC_R11G11_SNORM_BLOCK'
-- specifies a two-component, ETC2 compressed format where each 128-bit
-- compressed texel block encodes a 4×4 rectangle of signed normalized RG
-- texel data with the first 64 bits encoding red values followed by 64
-- bits encoding green values.
pattern FORMAT_EAC_R11G11_SNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_EAC_R11G11_SNORM_BLOCK = VK_FORMAT_EAC_R11G11_SNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_4x4_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_4x4_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_4x4_UNORM_BLOCK = VK_FORMAT_ASTC_4x4_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_4x4_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 4×4 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_4x4_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_4x4_SRGB_BLOCK = VK_FORMAT_ASTC_4x4_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x4_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 5×4 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_5x4_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_5x4_UNORM_BLOCK = VK_FORMAT_ASTC_5x4_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x4_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 5×4 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_5x4_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_5x4_SRGB_BLOCK = VK_FORMAT_ASTC_5x4_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x5_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 5×5 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_5x5_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_5x5_UNORM_BLOCK = VK_FORMAT_ASTC_5x5_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_5x5_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 5×5 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_5x5_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_5x5_SRGB_BLOCK = VK_FORMAT_ASTC_5x5_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x5_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 6×5 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_6x5_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_6x5_UNORM_BLOCK = VK_FORMAT_ASTC_6x5_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x5_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 6×5 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_6x5_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_6x5_SRGB_BLOCK = VK_FORMAT_ASTC_6x5_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x6_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 6×6 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_6x6_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_6x6_UNORM_BLOCK = VK_FORMAT_ASTC_6x6_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_6x6_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 6×6 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_6x6_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_6x6_SRGB_BLOCK = VK_FORMAT_ASTC_6x6_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x5_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes an 8×5 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_8x5_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_8x5_UNORM_BLOCK = VK_FORMAT_ASTC_8x5_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x5_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes an 8×5 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_8x5_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_8x5_SRGB_BLOCK = VK_FORMAT_ASTC_8x5_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x6_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes an 8×6 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_8x6_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_8x6_UNORM_BLOCK = VK_FORMAT_ASTC_8x6_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x6_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes an 8×6 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_8x6_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_8x6_SRGB_BLOCK = VK_FORMAT_ASTC_8x6_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x8_UNORM_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes an 8×8 rectangle of unsigned normalized RGBA texel
-- data.
pattern FORMAT_ASTC_8x8_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_8x8_UNORM_BLOCK = VK_FORMAT_ASTC_8x8_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_8x8_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes an 8×8 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_8x8_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_8x8_SRGB_BLOCK = VK_FORMAT_ASTC_8x8_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x5_UNORM_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 10×5 rectangle of unsigned normalized
-- RGBA texel data.
pattern FORMAT_ASTC_10x5_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x5_UNORM_BLOCK = VK_FORMAT_ASTC_10x5_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x5_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 10×5 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x5_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x5_SRGB_BLOCK = VK_FORMAT_ASTC_10x5_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x6_UNORM_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 10×6 rectangle of unsigned normalized
-- RGBA texel data.
pattern FORMAT_ASTC_10x6_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x6_UNORM_BLOCK = VK_FORMAT_ASTC_10x6_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x6_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 10×6 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x6_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x6_SRGB_BLOCK = VK_FORMAT_ASTC_10x6_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x8_UNORM_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 10×8 rectangle of unsigned normalized
-- RGBA texel data.
pattern FORMAT_ASTC_10x8_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x8_UNORM_BLOCK = VK_FORMAT_ASTC_10x8_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x8_SRGB_BLOCK' specifies
-- a four-component, ASTC compressed format where each 128-bit compressed
-- texel block encodes a 10×8 rectangle of unsigned normalized RGBA texel
-- data with sRGB nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x8_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x8_SRGB_BLOCK = VK_FORMAT_ASTC_10x8_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x10_UNORM_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 10×10 rectangle of unsigned normalized
-- RGBA texel data.
pattern FORMAT_ASTC_10x10_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x10_UNORM_BLOCK = VK_FORMAT_ASTC_10x10_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_10x10_SRGB_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 10×10 rectangle of unsigned normalized
-- RGBA texel data with sRGB nonlinear encoding applied to the RGB
-- components.
pattern FORMAT_ASTC_10x10_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_10x10_SRGB_BLOCK = VK_FORMAT_ASTC_10x10_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x10_UNORM_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 12×10 rectangle of unsigned normalized
-- RGBA texel data.
pattern FORMAT_ASTC_12x10_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_12x10_UNORM_BLOCK = VK_FORMAT_ASTC_12x10_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x10_SRGB_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 12×10 rectangle of unsigned normalized
-- RGBA texel data with sRGB nonlinear encoding applied to the RGB
-- components.
pattern FORMAT_ASTC_12x10_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_12x10_SRGB_BLOCK = VK_FORMAT_ASTC_12x10_SRGB_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x12_UNORM_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 12×12 rectangle of unsigned normalized
-- RGBA texel data.
pattern FORMAT_ASTC_12x12_UNORM_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_12x12_UNORM_BLOCK = VK_FORMAT_ASTC_12x12_UNORM_BLOCK


-- | 'Graphics.Vulkan.C.Core10.Core.VK_FORMAT_ASTC_12x12_SRGB_BLOCK'
-- specifies a four-component, ASTC compressed format where each 128-bit
-- compressed texel block encodes a 12×12 rectangle of unsigned normalized
-- RGBA texel data with sRGB nonlinear encoding applied to the RGB
-- components.
pattern FORMAT_ASTC_12x12_SRGB_BLOCK :: (a ~ Format) => a
pattern FORMAT_ASTC_12x12_SRGB_BLOCK = VK_FORMAT_ASTC_12x12_SRGB_BLOCK

-- | VkObjectType - Specify an enumeration to track object handle types
--
-- = Description
--
-- \'
--
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VkObjectT | Vulkan Handle Type        |
-- > | ype'                                     |                           |
-- > +==========================================+===========================+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | Unknown\/Undefined Handle |
-- > | _TYPE_UNKNOWN'                           |                           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_INSTANCE'                          | .DeviceInitialization.VkI |
-- > |                                          | nstance'                  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_PHYSICAL_DEVICE'                   | .DeviceInitialization.VkP |
-- > |                                          | hysicalDevice'            |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_DEVICE'                            | .DeviceInitialization.VkD |
-- > |                                          | evice'                    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_QUEUE'                             | .Queue.VkQueue'           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_SEMAPHORE'                         | .Queue.VkSemaphore'       |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_COMMAND_BUFFER'                    | .Queue.VkCommandBuffer'   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_FENCE'                             | .Queue.VkFence'           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_DEVICE_MEMORY'                     | .Memory.VkDeviceMemory'   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_BUFFER'                            | .MemoryManagement.VkBuffe |
-- > |                                          | r'                        |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_IMAGE'                             | .MemoryManagement.VkImage |
-- > |                                          | '                         |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_EVENT'                             | .Event.VkEvent'           |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_QUERY_POOL'                        | .Query.VkQueryPool'       |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_BUFFER_VIEW'                       | .BufferView.VkBufferView' |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_IMAGE_VIEW'                        | .ImageView.VkImageView'   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_SHADER_MODULE'                     | .Shader.VkShaderModule'   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_PIPELINE_CACHE'                    | .PipelineCache.VkPipeline |
-- > |                                          | Cache'                    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_PIPELINE_LAYOUT'                   | .Pipeline.VkPipelineLayou |
-- > |                                          | t'                        |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_RENDER_PASS'                       | .Pipeline.VkRenderPass'   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_PIPELINE'                          | .Pipeline.VkPipeline'     |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_DESCRIPTOR_SET_LAYOUT'             | .PipelineLayout.VkDescrip |
-- > |                                          | torSetLayout'             |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_SAMPLER'                           | .Sampler.VkSampler'       |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_DESCRIPTOR_POOL'                   | .DescriptorSet.VkDescript |
-- > |                                          | orPool'                   |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_DESCRIPTOR_SET'                    | .DescriptorSet.VkDescript |
-- > |                                          | orSet'                    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_FRAMEBUFFER'                       | .Pass.VkFramebuffer'      |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core10.Core.VK_OBJECT | 'Graphics.Vulkan.C.Core10 |
-- > | _TYPE_COMMAND_POOL'                      | .CommandPool.VkCommandPoo |
-- > |                                          | l'                        |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core11.Promoted_from_ | 'Graphics.Vulkan.C.Core11 |
-- > | VK_KHR_sampler_ycbcr_conversion.VK_OBJEC | .Promoted_from_VK_KHR_sam |
-- > | T_TYPE_SAMPLER_YCBCR_CONVERSION'         | pler_ycbcr_conversion.VkS |
-- > |                                          | amplerYcbcrConversion'    |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Core11.Promoted_from_ | 'Graphics.Vulkan.C.Core11 |
-- > | VK_KHR_descriptor_update_template.VK_OBJ | .Promoted_from_VK_KHR_des |
-- > | ECT_TYPE_DESCRIPTOR_UPDATE_TEMPLATE'     | criptor_update_template.V |
-- > |                                          | kDescriptorUpdateTemplate |
-- > |                                          | '                         |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_KHR_sur | 'Graphics.Vulkan.C.Extens |
-- > | face.VK_OBJECT_TYPE_SURFACE_KHR'         | ions.VK_KHR_surface.VkSur |
-- > |                                          | faceKHR'                  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_KHR_swa | 'Graphics.Vulkan.C.Extens |
-- > | pchain.VK_OBJECT_TYPE_SWAPCHAIN_KHR'     | ions.VK_KHR_swapchain.VkS |
-- > |                                          | wapchainKHR'              |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_KHR_dis | 'Graphics.Vulkan.C.Extens |
-- > | play.VK_OBJECT_TYPE_DISPLAY_KHR'         | ions.VK_KHR_display.VkDis |
-- > |                                          | playKHR'                  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_KHR_dis | 'Graphics.Vulkan.C.Extens |
-- > | play.VK_OBJECT_TYPE_DISPLAY_MODE_KHR'    | ions.VK_KHR_display.VkDis |
-- > |                                          | playModeKHR'              |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_report.VK_OBJECT_TYPE_DEBUG_REPORT_CA | ions.VK_EXT_debug_report. |
-- > | LLBACK_EXT'                              | VkDebugReportCallbackEXT' |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_dev | 'Graphics.Vulkan.C.Extens |
-- > | ice_generated_commands.VK_OBJECT_TYPE_OB | ions.VK_NVX_device_genera |
-- > | JECT_TABLE_NVX'                          | ted_commands.VkObjectTabl |
-- > |                                          | eNVX'                     |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NVX_dev | 'Graphics.Vulkan.C.Extens |
-- > | ice_generated_commands.VK_OBJECT_TYPE_IN | ions.VK_NVX_device_genera |
-- > | DIRECT_COMMANDS_LAYOUT_NVX'              | ted_commands.VkIndirectCo |
-- > |                                          | mmandsLayoutNVX'          |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_deb | 'Graphics.Vulkan.C.Extens |
-- > | ug_utils.VK_OBJECT_TYPE_DEBUG_UTILS_MESS | ions.VK_EXT_debug_utils.V |
-- > | ENGER_EXT'                               | kDebugUtilsMessengerEXT'  |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_EXT_val | 'Graphics.Vulkan.C.Extens |
-- > | idation_cache.VK_OBJECT_TYPE_VALIDATION_ | ions.VK_EXT_validation_ca |
-- > | CACHE_EXT'                               | che.VkValidationCacheEXT' |
-- > +------------------------------------------+---------------------------+
-- > | 'Graphics.Vulkan.C.Extensions.VK_NV_ray_ | 'Graphics.Vulkan.C.Extens |
-- > | tracing.VK_OBJECT_TYPE_ACCELERATION_STRU | ions.VK_NV_ray_tracing.Vk |
-- > | CTURE_NV'                                | AccelerationStructureNV'  |
-- > +------------------------------------------+---------------------------+
-- >
-- > VkObjectType and Vulkan Handle Relationship
--
-- = See Also
--
-- No cross-references are available
type ObjectType = VkObjectType


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_UNKNOWN"
pattern OBJECT_TYPE_UNKNOWN :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_UNKNOWN = VK_OBJECT_TYPE_UNKNOWN


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_INSTANCE"
pattern OBJECT_TYPE_INSTANCE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_INSTANCE = VK_OBJECT_TYPE_INSTANCE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_PHYSICAL_DEVICE"
pattern OBJECT_TYPE_PHYSICAL_DEVICE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_PHYSICAL_DEVICE = VK_OBJECT_TYPE_PHYSICAL_DEVICE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_DEVICE"
pattern OBJECT_TYPE_DEVICE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_DEVICE = VK_OBJECT_TYPE_DEVICE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_QUEUE"
pattern OBJECT_TYPE_QUEUE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_QUEUE = VK_OBJECT_TYPE_QUEUE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_SEMAPHORE"
pattern OBJECT_TYPE_SEMAPHORE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_SEMAPHORE = VK_OBJECT_TYPE_SEMAPHORE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_COMMAND_BUFFER"
pattern OBJECT_TYPE_COMMAND_BUFFER :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_COMMAND_BUFFER = VK_OBJECT_TYPE_COMMAND_BUFFER


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_FENCE"
pattern OBJECT_TYPE_FENCE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_FENCE = VK_OBJECT_TYPE_FENCE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_DEVICE_MEMORY"
pattern OBJECT_TYPE_DEVICE_MEMORY :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_DEVICE_MEMORY = VK_OBJECT_TYPE_DEVICE_MEMORY


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_BUFFER"
pattern OBJECT_TYPE_BUFFER :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_BUFFER = VK_OBJECT_TYPE_BUFFER


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_IMAGE"
pattern OBJECT_TYPE_IMAGE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_IMAGE = VK_OBJECT_TYPE_IMAGE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_EVENT"
pattern OBJECT_TYPE_EVENT :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_EVENT = VK_OBJECT_TYPE_EVENT


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_QUERY_POOL"
pattern OBJECT_TYPE_QUERY_POOL :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_QUERY_POOL = VK_OBJECT_TYPE_QUERY_POOL


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_BUFFER_VIEW"
pattern OBJECT_TYPE_BUFFER_VIEW :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_BUFFER_VIEW = VK_OBJECT_TYPE_BUFFER_VIEW


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_IMAGE_VIEW"
pattern OBJECT_TYPE_IMAGE_VIEW :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_IMAGE_VIEW = VK_OBJECT_TYPE_IMAGE_VIEW


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_SHADER_MODULE"
pattern OBJECT_TYPE_SHADER_MODULE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_SHADER_MODULE = VK_OBJECT_TYPE_SHADER_MODULE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_PIPELINE_CACHE"
pattern OBJECT_TYPE_PIPELINE_CACHE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_PIPELINE_CACHE = VK_OBJECT_TYPE_PIPELINE_CACHE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_PIPELINE_LAYOUT"
pattern OBJECT_TYPE_PIPELINE_LAYOUT :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_PIPELINE_LAYOUT = VK_OBJECT_TYPE_PIPELINE_LAYOUT


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_RENDER_PASS"
pattern OBJECT_TYPE_RENDER_PASS :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_RENDER_PASS = VK_OBJECT_TYPE_RENDER_PASS


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_PIPELINE"
pattern OBJECT_TYPE_PIPELINE :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_PIPELINE = VK_OBJECT_TYPE_PIPELINE


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT"
pattern OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT = VK_OBJECT_TYPE_DESCRIPTOR_SET_LAYOUT


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_SAMPLER"
pattern OBJECT_TYPE_SAMPLER :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_SAMPLER = VK_OBJECT_TYPE_SAMPLER


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_DESCRIPTOR_POOL"
pattern OBJECT_TYPE_DESCRIPTOR_POOL :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_DESCRIPTOR_POOL = VK_OBJECT_TYPE_DESCRIPTOR_POOL


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_DESCRIPTOR_SET"
pattern OBJECT_TYPE_DESCRIPTOR_SET :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_DESCRIPTOR_SET = VK_OBJECT_TYPE_DESCRIPTOR_SET


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_FRAMEBUFFER"
pattern OBJECT_TYPE_FRAMEBUFFER :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_FRAMEBUFFER = VK_OBJECT_TYPE_FRAMEBUFFER


-- No documentation found for Nested "ObjectType" "OBJECT_TYPE_COMMAND_POOL"
pattern OBJECT_TYPE_COMMAND_POOL :: (a ~ ObjectType) => a
pattern OBJECT_TYPE_COMMAND_POOL = VK_OBJECT_TYPE_COMMAND_POOL

-- | VkResult - Vulkan command return codes
--
-- = Description
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
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.vkEndCommandBuffer'.
--
-- = See Also
--
-- No cross-references are available
type Result = VkResult


-- | 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' Command successfully
-- completed
pattern SUCCESS :: (a ~ Result) => a
pattern SUCCESS = VK_SUCCESS


-- | 'Graphics.Vulkan.C.Core10.Core.VK_NOT_READY' A fence or query has not
-- yet completed
pattern NOT_READY :: (a ~ Result) => a
pattern NOT_READY = VK_NOT_READY


-- | 'Graphics.Vulkan.C.Core10.Core.VK_TIMEOUT' A wait operation has not
-- completed in the specified time
pattern TIMEOUT :: (a ~ Result) => a
pattern TIMEOUT = VK_TIMEOUT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_EVENT_SET' An event is signaled
pattern EVENT_SET :: (a ~ Result) => a
pattern EVENT_SET = VK_EVENT_SET


-- | 'Graphics.Vulkan.C.Core10.Core.VK_EVENT_RESET' An event is unsignaled
pattern EVENT_RESET :: (a ~ Result) => a
pattern EVENT_RESET = VK_EVENT_RESET


-- | 'Graphics.Vulkan.C.Core10.Core.VK_INCOMPLETE' A return array was too
-- small for the result
pattern INCOMPLETE :: (a ~ Result) => a
pattern INCOMPLETE = VK_INCOMPLETE


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_HOST_MEMORY' A host
-- memory allocation has failed.
pattern ERROR_OUT_OF_HOST_MEMORY :: (a ~ Result) => a
pattern ERROR_OUT_OF_HOST_MEMORY = VK_ERROR_OUT_OF_HOST_MEMORY


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_OUT_OF_DEVICE_MEMORY' A device
-- memory allocation has failed.
pattern ERROR_OUT_OF_DEVICE_MEMORY :: (a ~ Result) => a
pattern ERROR_OUT_OF_DEVICE_MEMORY = VK_ERROR_OUT_OF_DEVICE_MEMORY


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INITIALIZATION_FAILED'
-- Initialization of an object could not be completed for
-- implementation-specific reasons.
pattern ERROR_INITIALIZATION_FAILED :: (a ~ Result) => a
pattern ERROR_INITIALIZATION_FAILED = VK_ERROR_INITIALIZATION_FAILED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_DEVICE_LOST' The logical or
-- physical device has been lost. See
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#devsandqueues-lost-device Lost Device>
pattern ERROR_DEVICE_LOST :: (a ~ Result) => a
pattern ERROR_DEVICE_LOST = VK_ERROR_DEVICE_LOST


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_MEMORY_MAP_FAILED' Mapping of a
-- memory object has failed.
pattern ERROR_MEMORY_MAP_FAILED :: (a ~ Result) => a
pattern ERROR_MEMORY_MAP_FAILED = VK_ERROR_MEMORY_MAP_FAILED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_LAYER_NOT_PRESENT' A requested
-- layer is not present or could not be loaded.
pattern ERROR_LAYER_NOT_PRESENT :: (a ~ Result) => a
pattern ERROR_LAYER_NOT_PRESENT = VK_ERROR_LAYER_NOT_PRESENT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_EXTENSION_NOT_PRESENT' A
-- requested extension is not supported.
pattern ERROR_EXTENSION_NOT_PRESENT :: (a ~ Result) => a
pattern ERROR_EXTENSION_NOT_PRESENT = VK_ERROR_EXTENSION_NOT_PRESENT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FEATURE_NOT_PRESENT' A requested
-- feature is not supported.
pattern ERROR_FEATURE_NOT_PRESENT :: (a ~ Result) => a
pattern ERROR_FEATURE_NOT_PRESENT = VK_ERROR_FEATURE_NOT_PRESENT


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_INCOMPATIBLE_DRIVER' The
-- requested version of Vulkan is not supported by the driver or is
-- otherwise incompatible for implementation-specific reasons.
pattern ERROR_INCOMPATIBLE_DRIVER :: (a ~ Result) => a
pattern ERROR_INCOMPATIBLE_DRIVER = VK_ERROR_INCOMPATIBLE_DRIVER


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_TOO_MANY_OBJECTS' Too many
-- objects of the type have already been created.
pattern ERROR_TOO_MANY_OBJECTS :: (a ~ Result) => a
pattern ERROR_TOO_MANY_OBJECTS = VK_ERROR_TOO_MANY_OBJECTS


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FORMAT_NOT_SUPPORTED' A
-- requested format is not supported on this device.
pattern ERROR_FORMAT_NOT_SUPPORTED :: (a ~ Result) => a
pattern ERROR_FORMAT_NOT_SUPPORTED = VK_ERROR_FORMAT_NOT_SUPPORTED


-- | 'Graphics.Vulkan.C.Core10.Core.VK_ERROR_FRAGMENTED_POOL' A pool
-- allocation has failed due to fragmentation of the pool’s memory. This
-- /must/ only be returned if no attempt to allocate host or device memory
-- was made to accommodate the new allocation. This /should/ be returned in
-- preference to
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance1.VK_ERROR_OUT_OF_POOL_MEMORY',
-- but only if the implementation is certain that the pool allocation
-- failure was due to fragmentation.
pattern ERROR_FRAGMENTED_POOL :: (a ~ Result) => a
pattern ERROR_FRAGMENTED_POOL = VK_ERROR_FRAGMENTED_POOL

-- | VkStructureType - Vulkan structure types (@stype@)
--
-- = Description
--
-- Each value corresponds to a particular structure with a @sType@ member
-- with a matching name. As a general rule, the name of each
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType' value is obtained by
-- taking the name of the structure, stripping the leading @Vk@, prefixing
-- each capital letter with @_@, converting the entire resulting string to
-- upper case, and prefixing it with @VK_STRUCTURE_TYPE_@. For example,
-- structures of type 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo'
-- correspond to a 'Graphics.Vulkan.C.Core10.Core.VkStructureType' of
-- 'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO', and
-- thus its @sType@ member /must/ equal that when it is passed to the API.
--
-- The values
-- 'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO'
-- and
-- 'Graphics.Vulkan.C.Core10.Core.VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO'
-- are reserved for internal use by the loader, and do not have
-- corresponding Vulkan structures in this Specification.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkApplicationInfo',
-- 'Graphics.Vulkan.C.Core10.Core.VkBaseInStructure',
-- 'Graphics.Vulkan.C.Core10.Core.VkBaseOutStructure',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindBufferMemoryDeviceGroupInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindBufferMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_and_VK_KHR_bind_memory2.VkBindImageMemoryDeviceGroupInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2.VkBindImageMemoryInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkBindImagePlaneMemoryInfo',
-- 'Graphics.Vulkan.C.Core10.SparseResourceMemoryManagement.VkBindSparseInfo',
-- 'Graphics.Vulkan.C.Core10.Buffer.VkBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkBufferMemoryBarrier',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkBufferMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.BufferView.VkBufferViewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferBeginInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.CommandPool.VkCommandPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkComputePipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkCopyDescriptorSet',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkDescriptorSetLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkDescriptorSetLayoutSupport',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_descriptor_update_template.VkDescriptorUpdateTemplateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupBindSparseInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupCommandBufferBeginInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkDeviceGroupDeviceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkDeviceGroupSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.Device.VkDeviceQueueCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkDeviceQueueInfo2',
-- 'Graphics.Vulkan.C.Core10.Event.VkEventCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence.VkExportFenceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExportMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore.VkExportSemaphoreCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalBufferProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkExternalFenceProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkExternalImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryBufferCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory.VkExternalMemoryImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkExternalSemaphoreProperties',
-- 'Graphics.Vulkan.C.Core10.Fence.VkFenceCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.Pass.VkFramebufferCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkGraphicsPipelineCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Image.VkImageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkImageFormatProperties2',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkImageMemoryBarrier',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkImagePlaneMemoryRequirementsInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkImageSparseMemoryRequirementsInfo2',
-- 'Graphics.Vulkan.C.Core10.ImageView.VkImageViewCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkImageViewUsageCreateInfo',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkInstanceCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMappedMemoryRange',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group.VkMemoryAllocateFlagsInfo',
-- 'Graphics.Vulkan.C.Core10.Memory.VkMemoryAllocateInfo',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkMemoryBarrier',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedAllocateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation.VkMemoryDedicatedRequirements',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_16bit_storage.VkPhysicalDevice16BitStorageFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalBufferInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_fence_capabilities.VkPhysicalDeviceExternalFenceInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceExternalImageFormatInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_semaphore_capabilities.VkPhysicalDeviceExternalSemaphoreInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceFeatures2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation.VkPhysicalDeviceGroupProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities.VkPhysicalDeviceIDProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance3.VkPhysicalDeviceMaintenance3Properties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceMemoryProperties2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkPhysicalDeviceMultiviewProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPhysicalDevicePointClippingProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceProperties2',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkPhysicalDeviceProtectedMemoryProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkPhysicalDeviceSamplerYcbcrConversionFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_shader_draw_parameters.VkPhysicalDeviceShaderDrawParametersFeatures',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkPhysicalDeviceSparseImageFormatInfo2',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_subgroup.VkPhysicalDeviceSubgroupProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_variable_pointers.VkPhysicalDeviceVariablePointersFeatures',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineColorBlendStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDepthStencilStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineDynamicStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineInputAssemblyStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.PipelineLayout.VkPipelineLayoutCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineMultisampleStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineRasterizationStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineShaderStageCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkPipelineTessellationDomainOriginStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineTessellationStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineVertexInputStateCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Pipeline.VkPipelineViewportStateCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory.VkProtectedSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkQueueFamilyProperties2',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.VkRenderPassBeginInfo',
-- 'Graphics.Vulkan.C.Core10.Pass.VkRenderPassCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_maintenance2.VkRenderPassInputAttachmentAspectCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_multiview.VkRenderPassMultiviewCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Sampler.VkSamplerCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionImageFormatProperties',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_sampler_ycbcr_conversion.VkSamplerYcbcrConversionInfo',
-- 'Graphics.Vulkan.C.Core10.QueueSemaphore.VkSemaphoreCreateInfo',
-- 'Graphics.Vulkan.C.Core10.Shader.VkShaderModuleCreateInfo',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_physical_device_properties2.VkSparseImageFormatProperties2',
-- 'Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_get_memory_requirements2.VkSparseImageMemoryRequirements2',
-- 'Graphics.Vulkan.C.Core10.Queue.VkSubmitInfo',
-- 'Graphics.Vulkan.C.Core10.DescriptorSet.VkWriteDescriptorSet'
type StructureType = VkStructureType


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_APPLICATION_INFO"
pattern STRUCTURE_TYPE_APPLICATION_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_APPLICATION_INFO = VK_STRUCTURE_TYPE_APPLICATION_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_INSTANCE_CREATE_INFO"
pattern STRUCTURE_TYPE_INSTANCE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO"
pattern STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_DEVICE_CREATE_INFO"
pattern STRUCTURE_TYPE_DEVICE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_DEVICE_CREATE_INFO = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_SUBMIT_INFO"
pattern STRUCTURE_TYPE_SUBMIT_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_SUBMIT_INFO = VK_STRUCTURE_TYPE_SUBMIT_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_MAPPED_MEMORY_RANGE"
pattern STRUCTURE_TYPE_MAPPED_MEMORY_RANGE :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_BIND_SPARSE_INFO"
pattern STRUCTURE_TYPE_BIND_SPARSE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_BIND_SPARSE_INFO = VK_STRUCTURE_TYPE_BIND_SPARSE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_FENCE_CREATE_INFO"
pattern STRUCTURE_TYPE_FENCE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_FENCE_CREATE_INFO = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO"
pattern STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_EVENT_CREATE_INFO"
pattern STRUCTURE_TYPE_EVENT_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_EVENT_CREATE_INFO = VK_STRUCTURE_TYPE_EVENT_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO"
pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_BUFFER_CREATE_INFO"
pattern STRUCTURE_TYPE_BUFFER_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_BUFFER_CREATE_INFO = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO"
pattern STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_IMAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_IMAGE_CREATE_INFO = VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO"
pattern STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO"
pattern STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO"
pattern STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO"
pattern STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO"
pattern STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_SAMPLER_CREATE_INFO"
pattern STRUCTURE_TYPE_SAMPLER_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET"
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_COPY_DESCRIPTOR_SET"
pattern STRUCTURE_TYPE_COPY_DESCRIPTOR_SET :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO"
pattern STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO"
pattern STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO"
pattern STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO"
pattern STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER"
pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER"
pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_MEMORY_BARRIER"
pattern STRUCTURE_TYPE_MEMORY_BARRIER :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_MEMORY_BARRIER = VK_STRUCTURE_TYPE_MEMORY_BARRIER


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO"
pattern STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO


-- No documentation found for Nested "StructureType" "STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO"
pattern STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO :: (a ~ StructureType) => a
pattern STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO

-- | VkVendorId - Khronos vendor IDs
--
-- = Description
--
-- __Note__
--
-- Khronos vendor IDs may be allocated by vendors at any time. Only the
-- latest canonical versions of this Specification, of the corresponding
-- @vk.xml@ API Registry, and of the corresponding @vulkan_core.h@ header
-- file /must/ contain all reserved Khronos vendor IDs.
--
-- Only Khronos vendor IDs are given symbolic names at present. PCI vendor
-- IDs returned by the implementation can be looked up in the PCI-SIG
-- database.
--
-- = See Also
--
-- No cross-references are available
type VendorId = VkVendorId


-- No documentation found for Nested "VendorId" "VENDOR_ID_VIV"
pattern VENDOR_ID_VIV :: (a ~ VendorId) => a
pattern VENDOR_ID_VIV = VK_VENDOR_ID_VIV


-- No documentation found for Nested "VendorId" "VENDOR_ID_VSI"
pattern VENDOR_ID_VSI :: (a ~ VendorId) => a
pattern VENDOR_ID_VSI = VK_VENDOR_ID_VSI


-- No documentation found for Nested "VendorId" "VENDOR_ID_KAZAN"
pattern VENDOR_ID_KAZAN :: (a ~ VendorId) => a
pattern VENDOR_ID_KAZAN = VK_VENDOR_ID_KAZAN

bool32ToBool :: VkBool32 -> Bool
bool32ToBool = \case
  VK_FALSE -> False
  VK_TRUE  -> True
  -- TODO: add pattern totality
  _        -> error "unhandled VkBool32 Value"

boolToBool32 :: Bool -> VkBool32
boolToBool32 = \case
  False -> VK_FALSE
  True  -> VK_TRUE
