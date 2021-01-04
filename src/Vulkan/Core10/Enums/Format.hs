{-# language CPP #-}
-- No documentation found for Chapter "Format"
module Vulkan.Core10.Enums.Format  (Format( FORMAT_UNDEFINED
                                          , FORMAT_R4G4_UNORM_PACK8
                                          , FORMAT_R4G4B4A4_UNORM_PACK16
                                          , FORMAT_B4G4R4A4_UNORM_PACK16
                                          , FORMAT_R5G6B5_UNORM_PACK16
                                          , FORMAT_B5G6R5_UNORM_PACK16
                                          , FORMAT_R5G5B5A1_UNORM_PACK16
                                          , FORMAT_B5G5R5A1_UNORM_PACK16
                                          , FORMAT_A1R5G5B5_UNORM_PACK16
                                          , FORMAT_R8_UNORM
                                          , FORMAT_R8_SNORM
                                          , FORMAT_R8_USCALED
                                          , FORMAT_R8_SSCALED
                                          , FORMAT_R8_UINT
                                          , FORMAT_R8_SINT
                                          , FORMAT_R8_SRGB
                                          , FORMAT_R8G8_UNORM
                                          , FORMAT_R8G8_SNORM
                                          , FORMAT_R8G8_USCALED
                                          , FORMAT_R8G8_SSCALED
                                          , FORMAT_R8G8_UINT
                                          , FORMAT_R8G8_SINT
                                          , FORMAT_R8G8_SRGB
                                          , FORMAT_R8G8B8_UNORM
                                          , FORMAT_R8G8B8_SNORM
                                          , FORMAT_R8G8B8_USCALED
                                          , FORMAT_R8G8B8_SSCALED
                                          , FORMAT_R8G8B8_UINT
                                          , FORMAT_R8G8B8_SINT
                                          , FORMAT_R8G8B8_SRGB
                                          , FORMAT_B8G8R8_UNORM
                                          , FORMAT_B8G8R8_SNORM
                                          , FORMAT_B8G8R8_USCALED
                                          , FORMAT_B8G8R8_SSCALED
                                          , FORMAT_B8G8R8_UINT
                                          , FORMAT_B8G8R8_SINT
                                          , FORMAT_B8G8R8_SRGB
                                          , FORMAT_R8G8B8A8_UNORM
                                          , FORMAT_R8G8B8A8_SNORM
                                          , FORMAT_R8G8B8A8_USCALED
                                          , FORMAT_R8G8B8A8_SSCALED
                                          , FORMAT_R8G8B8A8_UINT
                                          , FORMAT_R8G8B8A8_SINT
                                          , FORMAT_R8G8B8A8_SRGB
                                          , FORMAT_B8G8R8A8_UNORM
                                          , FORMAT_B8G8R8A8_SNORM
                                          , FORMAT_B8G8R8A8_USCALED
                                          , FORMAT_B8G8R8A8_SSCALED
                                          , FORMAT_B8G8R8A8_UINT
                                          , FORMAT_B8G8R8A8_SINT
                                          , FORMAT_B8G8R8A8_SRGB
                                          , FORMAT_A8B8G8R8_UNORM_PACK32
                                          , FORMAT_A8B8G8R8_SNORM_PACK32
                                          , FORMAT_A8B8G8R8_USCALED_PACK32
                                          , FORMAT_A8B8G8R8_SSCALED_PACK32
                                          , FORMAT_A8B8G8R8_UINT_PACK32
                                          , FORMAT_A8B8G8R8_SINT_PACK32
                                          , FORMAT_A8B8G8R8_SRGB_PACK32
                                          , FORMAT_A2R10G10B10_UNORM_PACK32
                                          , FORMAT_A2R10G10B10_SNORM_PACK32
                                          , FORMAT_A2R10G10B10_USCALED_PACK32
                                          , FORMAT_A2R10G10B10_SSCALED_PACK32
                                          , FORMAT_A2R10G10B10_UINT_PACK32
                                          , FORMAT_A2R10G10B10_SINT_PACK32
                                          , FORMAT_A2B10G10R10_UNORM_PACK32
                                          , FORMAT_A2B10G10R10_SNORM_PACK32
                                          , FORMAT_A2B10G10R10_USCALED_PACK32
                                          , FORMAT_A2B10G10R10_SSCALED_PACK32
                                          , FORMAT_A2B10G10R10_UINT_PACK32
                                          , FORMAT_A2B10G10R10_SINT_PACK32
                                          , FORMAT_R16_UNORM
                                          , FORMAT_R16_SNORM
                                          , FORMAT_R16_USCALED
                                          , FORMAT_R16_SSCALED
                                          , FORMAT_R16_UINT
                                          , FORMAT_R16_SINT
                                          , FORMAT_R16_SFLOAT
                                          , FORMAT_R16G16_UNORM
                                          , FORMAT_R16G16_SNORM
                                          , FORMAT_R16G16_USCALED
                                          , FORMAT_R16G16_SSCALED
                                          , FORMAT_R16G16_UINT
                                          , FORMAT_R16G16_SINT
                                          , FORMAT_R16G16_SFLOAT
                                          , FORMAT_R16G16B16_UNORM
                                          , FORMAT_R16G16B16_SNORM
                                          , FORMAT_R16G16B16_USCALED
                                          , FORMAT_R16G16B16_SSCALED
                                          , FORMAT_R16G16B16_UINT
                                          , FORMAT_R16G16B16_SINT
                                          , FORMAT_R16G16B16_SFLOAT
                                          , FORMAT_R16G16B16A16_UNORM
                                          , FORMAT_R16G16B16A16_SNORM
                                          , FORMAT_R16G16B16A16_USCALED
                                          , FORMAT_R16G16B16A16_SSCALED
                                          , FORMAT_R16G16B16A16_UINT
                                          , FORMAT_R16G16B16A16_SINT
                                          , FORMAT_R16G16B16A16_SFLOAT
                                          , FORMAT_R32_UINT
                                          , FORMAT_R32_SINT
                                          , FORMAT_R32_SFLOAT
                                          , FORMAT_R32G32_UINT
                                          , FORMAT_R32G32_SINT
                                          , FORMAT_R32G32_SFLOAT
                                          , FORMAT_R32G32B32_UINT
                                          , FORMAT_R32G32B32_SINT
                                          , FORMAT_R32G32B32_SFLOAT
                                          , FORMAT_R32G32B32A32_UINT
                                          , FORMAT_R32G32B32A32_SINT
                                          , FORMAT_R32G32B32A32_SFLOAT
                                          , FORMAT_R64_UINT
                                          , FORMAT_R64_SINT
                                          , FORMAT_R64_SFLOAT
                                          , FORMAT_R64G64_UINT
                                          , FORMAT_R64G64_SINT
                                          , FORMAT_R64G64_SFLOAT
                                          , FORMAT_R64G64B64_UINT
                                          , FORMAT_R64G64B64_SINT
                                          , FORMAT_R64G64B64_SFLOAT
                                          , FORMAT_R64G64B64A64_UINT
                                          , FORMAT_R64G64B64A64_SINT
                                          , FORMAT_R64G64B64A64_SFLOAT
                                          , FORMAT_B10G11R11_UFLOAT_PACK32
                                          , FORMAT_E5B9G9R9_UFLOAT_PACK32
                                          , FORMAT_D16_UNORM
                                          , FORMAT_X8_D24_UNORM_PACK32
                                          , FORMAT_D32_SFLOAT
                                          , FORMAT_S8_UINT
                                          , FORMAT_D16_UNORM_S8_UINT
                                          , FORMAT_D24_UNORM_S8_UINT
                                          , FORMAT_D32_SFLOAT_S8_UINT
                                          , FORMAT_BC1_RGB_UNORM_BLOCK
                                          , FORMAT_BC1_RGB_SRGB_BLOCK
                                          , FORMAT_BC1_RGBA_UNORM_BLOCK
                                          , FORMAT_BC1_RGBA_SRGB_BLOCK
                                          , FORMAT_BC2_UNORM_BLOCK
                                          , FORMAT_BC2_SRGB_BLOCK
                                          , FORMAT_BC3_UNORM_BLOCK
                                          , FORMAT_BC3_SRGB_BLOCK
                                          , FORMAT_BC4_UNORM_BLOCK
                                          , FORMAT_BC4_SNORM_BLOCK
                                          , FORMAT_BC5_UNORM_BLOCK
                                          , FORMAT_BC5_SNORM_BLOCK
                                          , FORMAT_BC6H_UFLOAT_BLOCK
                                          , FORMAT_BC6H_SFLOAT_BLOCK
                                          , FORMAT_BC7_UNORM_BLOCK
                                          , FORMAT_BC7_SRGB_BLOCK
                                          , FORMAT_ETC2_R8G8B8_UNORM_BLOCK
                                          , FORMAT_ETC2_R8G8B8_SRGB_BLOCK
                                          , FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK
                                          , FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK
                                          , FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK
                                          , FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK
                                          , FORMAT_EAC_R11_UNORM_BLOCK
                                          , FORMAT_EAC_R11_SNORM_BLOCK
                                          , FORMAT_EAC_R11G11_UNORM_BLOCK
                                          , FORMAT_EAC_R11G11_SNORM_BLOCK
                                          , FORMAT_ASTC_4x4_UNORM_BLOCK
                                          , FORMAT_ASTC_4x4_SRGB_BLOCK
                                          , FORMAT_ASTC_5x4_UNORM_BLOCK
                                          , FORMAT_ASTC_5x4_SRGB_BLOCK
                                          , FORMAT_ASTC_5x5_UNORM_BLOCK
                                          , FORMAT_ASTC_5x5_SRGB_BLOCK
                                          , FORMAT_ASTC_6x5_UNORM_BLOCK
                                          , FORMAT_ASTC_6x5_SRGB_BLOCK
                                          , FORMAT_ASTC_6x6_UNORM_BLOCK
                                          , FORMAT_ASTC_6x6_SRGB_BLOCK
                                          , FORMAT_ASTC_8x5_UNORM_BLOCK
                                          , FORMAT_ASTC_8x5_SRGB_BLOCK
                                          , FORMAT_ASTC_8x6_UNORM_BLOCK
                                          , FORMAT_ASTC_8x6_SRGB_BLOCK
                                          , FORMAT_ASTC_8x8_UNORM_BLOCK
                                          , FORMAT_ASTC_8x8_SRGB_BLOCK
                                          , FORMAT_ASTC_10x5_UNORM_BLOCK
                                          , FORMAT_ASTC_10x5_SRGB_BLOCK
                                          , FORMAT_ASTC_10x6_UNORM_BLOCK
                                          , FORMAT_ASTC_10x6_SRGB_BLOCK
                                          , FORMAT_ASTC_10x8_UNORM_BLOCK
                                          , FORMAT_ASTC_10x8_SRGB_BLOCK
                                          , FORMAT_ASTC_10x10_UNORM_BLOCK
                                          , FORMAT_ASTC_10x10_SRGB_BLOCK
                                          , FORMAT_ASTC_12x10_UNORM_BLOCK
                                          , FORMAT_ASTC_12x10_SRGB_BLOCK
                                          , FORMAT_ASTC_12x12_UNORM_BLOCK
                                          , FORMAT_ASTC_12x12_SRGB_BLOCK
                                          , FORMAT_A4B4G4R4_UNORM_PACK16_EXT
                                          , FORMAT_A4R4G4B4_UNORM_PACK16_EXT
                                          , FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT
                                          , FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT
                                          , FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG
                                          , FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG
                                          , FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG
                                          , FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG
                                          , FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG
                                          , FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG
                                          , FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG
                                          , FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG
                                          , FORMAT_G16_B16_R16_3PLANE_444_UNORM
                                          , FORMAT_G16_B16R16_2PLANE_422_UNORM
                                          , FORMAT_G16_B16_R16_3PLANE_422_UNORM
                                          , FORMAT_G16_B16R16_2PLANE_420_UNORM
                                          , FORMAT_G16_B16_R16_3PLANE_420_UNORM
                                          , FORMAT_B16G16R16G16_422_UNORM
                                          , FORMAT_G16B16G16R16_422_UNORM
                                          , FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16
                                          , FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16
                                          , FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16
                                          , FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16
                                          , FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16
                                          , FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16
                                          , FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16
                                          , FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16
                                          , FORMAT_R12X4G12X4_UNORM_2PACK16
                                          , FORMAT_R12X4_UNORM_PACK16
                                          , FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16
                                          , FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16
                                          , FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16
                                          , FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16
                                          , FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16
                                          , FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16
                                          , FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16
                                          , FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16
                                          , FORMAT_R10X6G10X6_UNORM_2PACK16
                                          , FORMAT_R10X6_UNORM_PACK16
                                          , FORMAT_G8_B8_R8_3PLANE_444_UNORM
                                          , FORMAT_G8_B8R8_2PLANE_422_UNORM
                                          , FORMAT_G8_B8_R8_3PLANE_422_UNORM
                                          , FORMAT_G8_B8R8_2PLANE_420_UNORM
                                          , FORMAT_G8_B8_R8_3PLANE_420_UNORM
                                          , FORMAT_B8G8R8G8_422_UNORM
                                          , FORMAT_G8B8G8R8_422_UNORM
                                          , ..
                                          )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkFormat - Available image formats
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Vulkan.Core10.Pass.AttachmentDescription',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_create_renderpass2.AttachmentDescription2',
-- 'Vulkan.Core10.BufferView.BufferViewCreateInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_imageless_framebuffer.FramebufferAttachmentImageInfo',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.GeometryTrianglesNV',
-- 'Vulkan.Core10.Image.ImageCreateInfo',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_image_format_list.ImageFormatListCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_astc_decode_mode.ImageViewASTCDecodeModeEXT',
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceImageFormatInfo2',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.PhysicalDeviceSparseImageFormatInfo2',
-- 'Vulkan.Extensions.VK_EXT_custom_border_color.SamplerCustomBorderColorCreateInfoEXT',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_surface.SurfaceFormatKHR',
-- 'Vulkan.Extensions.VK_KHR_swapchain.SwapchainCreateInfoKHR',
-- 'Vulkan.Core10.Pipeline.VertexInputAttributeDescription',
-- 'Vulkan.Extensions.VK_NV_external_memory_capabilities.getPhysicalDeviceExternalImageFormatPropertiesNV',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceFormatProperties',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2',
-- 'Vulkan.Extensions.VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2KHR',
-- 'Vulkan.Core10.DeviceInitialization.getPhysicalDeviceImageFormatProperties',
-- 'Vulkan.Core10.SparseResourceMemoryManagement.getPhysicalDeviceSparseImageFormatProperties'
newtype Format = Format Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FORMAT_UNDEFINED' specifies that the format is not specified.
pattern FORMAT_UNDEFINED                          = Format 0
-- | 'FORMAT_R4G4_UNORM_PACK8' specifies a two-component, 8-bit packed
-- unsigned normalized format that has a 4-bit R component in bits 4..7,
-- and a 4-bit G component in bits 0..3.
pattern FORMAT_R4G4_UNORM_PACK8                   = Format 1
-- | 'FORMAT_R4G4B4A4_UNORM_PACK16' specifies a four-component, 16-bit packed
-- unsigned normalized format that has a 4-bit R component in bits 12..15,
-- a 4-bit G component in bits 8..11, a 4-bit B component in bits 4..7, and
-- a 4-bit A component in bits 0..3.
pattern FORMAT_R4G4B4A4_UNORM_PACK16              = Format 2
-- | 'FORMAT_B4G4R4A4_UNORM_PACK16' specifies a four-component, 16-bit packed
-- unsigned normalized format that has a 4-bit B component in bits 12..15,
-- a 4-bit G component in bits 8..11, a 4-bit R component in bits 4..7, and
-- a 4-bit A component in bits 0..3.
pattern FORMAT_B4G4R4A4_UNORM_PACK16              = Format 3
-- | 'FORMAT_R5G6B5_UNORM_PACK16' specifies a three-component, 16-bit packed
-- unsigned normalized format that has a 5-bit R component in bits 11..15,
-- a 6-bit G component in bits 5..10, and a 5-bit B component in bits 0..4.
pattern FORMAT_R5G6B5_UNORM_PACK16                = Format 4
-- | 'FORMAT_B5G6R5_UNORM_PACK16' specifies a three-component, 16-bit packed
-- unsigned normalized format that has a 5-bit B component in bits 11..15,
-- a 6-bit G component in bits 5..10, and a 5-bit R component in bits 0..4.
pattern FORMAT_B5G6R5_UNORM_PACK16                = Format 5
-- | 'FORMAT_R5G5B5A1_UNORM_PACK16' specifies a four-component, 16-bit packed
-- unsigned normalized format that has a 5-bit R component in bits 11..15,
-- a 5-bit G component in bits 6..10, a 5-bit B component in bits 1..5, and
-- a 1-bit A component in bit 0.
pattern FORMAT_R5G5B5A1_UNORM_PACK16              = Format 6
-- | 'FORMAT_B5G5R5A1_UNORM_PACK16' specifies a four-component, 16-bit packed
-- unsigned normalized format that has a 5-bit B component in bits 11..15,
-- a 5-bit G component in bits 6..10, a 5-bit R component in bits 1..5, and
-- a 1-bit A component in bit 0.
pattern FORMAT_B5G5R5A1_UNORM_PACK16              = Format 7
-- | 'FORMAT_A1R5G5B5_UNORM_PACK16' specifies a four-component, 16-bit packed
-- unsigned normalized format that has a 1-bit A component in bit 15, a
-- 5-bit R component in bits 10..14, a 5-bit G component in bits 5..9, and
-- a 5-bit B component in bits 0..4.
pattern FORMAT_A1R5G5B5_UNORM_PACK16              = Format 8
-- | 'FORMAT_R8_UNORM' specifies a one-component, 8-bit unsigned normalized
-- format that has a single 8-bit R component.
pattern FORMAT_R8_UNORM                           = Format 9
-- | 'FORMAT_R8_SNORM' specifies a one-component, 8-bit signed normalized
-- format that has a single 8-bit R component.
pattern FORMAT_R8_SNORM                           = Format 10
-- | 'FORMAT_R8_USCALED' specifies a one-component, 8-bit unsigned scaled
-- integer format that has a single 8-bit R component.
pattern FORMAT_R8_USCALED                         = Format 11
-- | 'FORMAT_R8_SSCALED' specifies a one-component, 8-bit signed scaled
-- integer format that has a single 8-bit R component.
pattern FORMAT_R8_SSCALED                         = Format 12
-- | 'FORMAT_R8_UINT' specifies a one-component, 8-bit unsigned integer
-- format that has a single 8-bit R component.
pattern FORMAT_R8_UINT                            = Format 13
-- | 'FORMAT_R8_SINT' specifies a one-component, 8-bit signed integer format
-- that has a single 8-bit R component.
pattern FORMAT_R8_SINT                            = Format 14
-- | 'FORMAT_R8_SRGB' specifies a one-component, 8-bit unsigned normalized
-- format that has a single 8-bit R component stored with sRGB nonlinear
-- encoding.
pattern FORMAT_R8_SRGB                            = Format 15
-- | 'FORMAT_R8G8_UNORM' specifies a two-component, 16-bit unsigned
-- normalized format that has an 8-bit R component in byte 0, and an 8-bit
-- G component in byte 1.
pattern FORMAT_R8G8_UNORM                         = Format 16
-- | 'FORMAT_R8G8_SNORM' specifies a two-component, 16-bit signed normalized
-- format that has an 8-bit R component in byte 0, and an 8-bit G component
-- in byte 1.
pattern FORMAT_R8G8_SNORM                         = Format 17
-- | 'FORMAT_R8G8_USCALED' specifies a two-component, 16-bit unsigned scaled
-- integer format that has an 8-bit R component in byte 0, and an 8-bit G
-- component in byte 1.
pattern FORMAT_R8G8_USCALED                       = Format 18
-- | 'FORMAT_R8G8_SSCALED' specifies a two-component, 16-bit signed scaled
-- integer format that has an 8-bit R component in byte 0, and an 8-bit G
-- component in byte 1.
pattern FORMAT_R8G8_SSCALED                       = Format 19
-- | 'FORMAT_R8G8_UINT' specifies a two-component, 16-bit unsigned integer
-- format that has an 8-bit R component in byte 0, and an 8-bit G component
-- in byte 1.
pattern FORMAT_R8G8_UINT                          = Format 20
-- | 'FORMAT_R8G8_SINT' specifies a two-component, 16-bit signed integer
-- format that has an 8-bit R component in byte 0, and an 8-bit G component
-- in byte 1.
pattern FORMAT_R8G8_SINT                          = Format 21
-- | 'FORMAT_R8G8_SRGB' specifies a two-component, 16-bit unsigned normalized
-- format that has an 8-bit R component stored with sRGB nonlinear encoding
-- in byte 0, and an 8-bit G component stored with sRGB nonlinear encoding
-- in byte 1.
pattern FORMAT_R8G8_SRGB                          = Format 22
-- | 'FORMAT_R8G8B8_UNORM' specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern FORMAT_R8G8B8_UNORM                       = Format 23
-- | 'FORMAT_R8G8B8_SNORM' specifies a three-component, 24-bit signed
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern FORMAT_R8G8B8_SNORM                       = Format 24
-- | 'FORMAT_R8G8B8_USCALED' specifies a three-component, 24-bit unsigned
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern FORMAT_R8G8B8_USCALED                     = Format 25
-- | 'FORMAT_R8G8B8_SSCALED' specifies a three-component, 24-bit signed
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern FORMAT_R8G8B8_SSCALED                     = Format 26
-- | 'FORMAT_R8G8B8_UINT' specifies a three-component, 24-bit unsigned
-- integer format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit B component in byte 2.
pattern FORMAT_R8G8B8_UINT                        = Format 27
-- | 'FORMAT_R8G8B8_SINT' specifies a three-component, 24-bit signed integer
-- format that has an 8-bit R component in byte 0, an 8-bit G component in
-- byte 1, and an 8-bit B component in byte 2.
pattern FORMAT_R8G8B8_SINT                        = Format 28
-- | 'FORMAT_R8G8B8_SRGB' specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, and an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 2.
pattern FORMAT_R8G8B8_SRGB                        = Format 29
-- | 'FORMAT_B8G8R8_UNORM' specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern FORMAT_B8G8R8_UNORM                       = Format 30
-- | 'FORMAT_B8G8R8_SNORM' specifies a three-component, 24-bit signed
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern FORMAT_B8G8R8_SNORM                       = Format 31
-- | 'FORMAT_B8G8R8_USCALED' specifies a three-component, 24-bit unsigned
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern FORMAT_B8G8R8_USCALED                     = Format 32
-- | 'FORMAT_B8G8R8_SSCALED' specifies a three-component, 24-bit signed
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern FORMAT_B8G8R8_SSCALED                     = Format 33
-- | 'FORMAT_B8G8R8_UINT' specifies a three-component, 24-bit unsigned
-- integer format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, and an 8-bit R component in byte 2.
pattern FORMAT_B8G8R8_UINT                        = Format 34
-- | 'FORMAT_B8G8R8_SINT' specifies a three-component, 24-bit signed integer
-- format that has an 8-bit B component in byte 0, an 8-bit G component in
-- byte 1, and an 8-bit R component in byte 2.
pattern FORMAT_B8G8R8_SINT                        = Format 35
-- | 'FORMAT_B8G8R8_SRGB' specifies a three-component, 24-bit unsigned
-- normalized format that has an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, and an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 2.
pattern FORMAT_B8G8R8_SRGB                        = Format 36
-- | 'FORMAT_R8G8B8A8_UNORM' specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_R8G8B8A8_UNORM                     = Format 37
-- | 'FORMAT_R8G8B8A8_SNORM' specifies a four-component, 32-bit signed
-- normalized format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_R8G8B8A8_SNORM                     = Format 38
-- | 'FORMAT_R8G8B8A8_USCALED' specifies a four-component, 32-bit unsigned
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_R8G8B8A8_USCALED                   = Format 39
-- | 'FORMAT_R8G8B8A8_SSCALED' specifies a four-component, 32-bit signed
-- scaled format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_R8G8B8A8_SSCALED                   = Format 40
-- | 'FORMAT_R8G8B8A8_UINT' specifies a four-component, 32-bit unsigned
-- integer format that has an 8-bit R component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit B component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_R8G8B8A8_UINT                      = Format 41
-- | 'FORMAT_R8G8B8A8_SINT' specifies a four-component, 32-bit signed integer
-- format that has an 8-bit R component in byte 0, an 8-bit G component in
-- byte 1, an 8-bit B component in byte 2, and an 8-bit A component in byte
-- 3.
pattern FORMAT_R8G8B8A8_SINT                      = Format 42
-- | 'FORMAT_R8G8B8A8_SRGB' specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_R8G8B8A8_SRGB                      = Format 43
-- | 'FORMAT_B8G8R8A8_UNORM' specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_B8G8R8A8_UNORM                     = Format 44
-- | 'FORMAT_B8G8R8A8_SNORM' specifies a four-component, 32-bit signed
-- normalized format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_B8G8R8A8_SNORM                     = Format 45
-- | 'FORMAT_B8G8R8A8_USCALED' specifies a four-component, 32-bit unsigned
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_B8G8R8A8_USCALED                   = Format 46
-- | 'FORMAT_B8G8R8A8_SSCALED' specifies a four-component, 32-bit signed
-- scaled format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_B8G8R8A8_SSCALED                   = Format 47
-- | 'FORMAT_B8G8R8A8_UINT' specifies a four-component, 32-bit unsigned
-- integer format that has an 8-bit B component in byte 0, an 8-bit G
-- component in byte 1, an 8-bit R component in byte 2, and an 8-bit A
-- component in byte 3.
pattern FORMAT_B8G8R8A8_UINT                      = Format 48
-- | 'FORMAT_B8G8R8A8_SINT' specifies a four-component, 32-bit signed integer
-- format that has an 8-bit B component in byte 0, an 8-bit G component in
-- byte 1, an 8-bit R component in byte 2, and an 8-bit A component in byte
-- 3.
pattern FORMAT_B8G8R8A8_SINT                      = Format 49
-- | 'FORMAT_B8G8R8A8_SRGB' specifies a four-component, 32-bit unsigned
-- normalized format that has an 8-bit B component stored with sRGB
-- nonlinear encoding in byte 0, an 8-bit G component stored with sRGB
-- nonlinear encoding in byte 1, an 8-bit R component stored with sRGB
-- nonlinear encoding in byte 2, and an 8-bit A component in byte 3.
pattern FORMAT_B8G8R8A8_SRGB                      = Format 50
-- | 'FORMAT_A8B8G8R8_UNORM_PACK32' specifies a four-component, 32-bit packed
-- unsigned normalized format that has an 8-bit A component in bits 24..31,
-- an 8-bit B component in bits 16..23, an 8-bit G component in bits 8..15,
-- and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_UNORM_PACK32              = Format 51
-- | 'FORMAT_A8B8G8R8_SNORM_PACK32' specifies a four-component, 32-bit packed
-- signed normalized format that has an 8-bit A component in bits 24..31,
-- an 8-bit B component in bits 16..23, an 8-bit G component in bits 8..15,
-- and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_SNORM_PACK32              = Format 52
-- | 'FORMAT_A8B8G8R8_USCALED_PACK32' specifies a four-component, 32-bit
-- packed unsigned scaled integer format that has an 8-bit A component in
-- bits 24..31, an 8-bit B component in bits 16..23, an 8-bit G component
-- in bits 8..15, and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_USCALED_PACK32            = Format 53
-- | 'FORMAT_A8B8G8R8_SSCALED_PACK32' specifies a four-component, 32-bit
-- packed signed scaled integer format that has an 8-bit A component in
-- bits 24..31, an 8-bit B component in bits 16..23, an 8-bit G component
-- in bits 8..15, and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_SSCALED_PACK32            = Format 54
-- | 'FORMAT_A8B8G8R8_UINT_PACK32' specifies a four-component, 32-bit packed
-- unsigned integer format that has an 8-bit A component in bits 24..31, an
-- 8-bit B component in bits 16..23, an 8-bit G component in bits 8..15,
-- and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_UINT_PACK32               = Format 55
-- | 'FORMAT_A8B8G8R8_SINT_PACK32' specifies a four-component, 32-bit packed
-- signed integer format that has an 8-bit A component in bits 24..31, an
-- 8-bit B component in bits 16..23, an 8-bit G component in bits 8..15,
-- and an 8-bit R component in bits 0..7.
pattern FORMAT_A8B8G8R8_SINT_PACK32               = Format 56
-- | 'FORMAT_A8B8G8R8_SRGB_PACK32' specifies a four-component, 32-bit packed
-- unsigned normalized format that has an 8-bit A component in bits 24..31,
-- an 8-bit B component stored with sRGB nonlinear encoding in bits 16..23,
-- an 8-bit G component stored with sRGB nonlinear encoding in bits 8..15,
-- and an 8-bit R component stored with sRGB nonlinear encoding in bits
-- 0..7.
pattern FORMAT_A8B8G8R8_SRGB_PACK32               = Format 57
-- | 'FORMAT_A2R10G10B10_UNORM_PACK32' specifies a four-component, 32-bit
-- packed unsigned normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern FORMAT_A2R10G10B10_UNORM_PACK32           = Format 58
-- | 'FORMAT_A2R10G10B10_SNORM_PACK32' specifies a four-component, 32-bit
-- packed signed normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern FORMAT_A2R10G10B10_SNORM_PACK32           = Format 59
-- | 'FORMAT_A2R10G10B10_USCALED_PACK32' specifies a four-component, 32-bit
-- packed unsigned scaled integer format that has a 2-bit A component in
-- bits 30..31, a 10-bit R component in bits 20..29, a 10-bit G component
-- in bits 10..19, and a 10-bit B component in bits 0..9.
pattern FORMAT_A2R10G10B10_USCALED_PACK32         = Format 60
-- | 'FORMAT_A2R10G10B10_SSCALED_PACK32' specifies a four-component, 32-bit
-- packed signed scaled integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern FORMAT_A2R10G10B10_SSCALED_PACK32         = Format 61
-- | 'FORMAT_A2R10G10B10_UINT_PACK32' specifies a four-component, 32-bit
-- packed unsigned integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern FORMAT_A2R10G10B10_UINT_PACK32            = Format 62
-- | 'FORMAT_A2R10G10B10_SINT_PACK32' specifies a four-component, 32-bit
-- packed signed integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit R component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit B component in bits 0..9.
pattern FORMAT_A2R10G10B10_SINT_PACK32            = Format 63
-- | 'FORMAT_A2B10G10R10_UNORM_PACK32' specifies a four-component, 32-bit
-- packed unsigned normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern FORMAT_A2B10G10R10_UNORM_PACK32           = Format 64
-- | 'FORMAT_A2B10G10R10_SNORM_PACK32' specifies a four-component, 32-bit
-- packed signed normalized format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern FORMAT_A2B10G10R10_SNORM_PACK32           = Format 65
-- | 'FORMAT_A2B10G10R10_USCALED_PACK32' specifies a four-component, 32-bit
-- packed unsigned scaled integer format that has a 2-bit A component in
-- bits 30..31, a 10-bit B component in bits 20..29, a 10-bit G component
-- in bits 10..19, and a 10-bit R component in bits 0..9.
pattern FORMAT_A2B10G10R10_USCALED_PACK32         = Format 66
-- | 'FORMAT_A2B10G10R10_SSCALED_PACK32' specifies a four-component, 32-bit
-- packed signed scaled integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern FORMAT_A2B10G10R10_SSCALED_PACK32         = Format 67
-- | 'FORMAT_A2B10G10R10_UINT_PACK32' specifies a four-component, 32-bit
-- packed unsigned integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern FORMAT_A2B10G10R10_UINT_PACK32            = Format 68
-- | 'FORMAT_A2B10G10R10_SINT_PACK32' specifies a four-component, 32-bit
-- packed signed integer format that has a 2-bit A component in bits
-- 30..31, a 10-bit B component in bits 20..29, a 10-bit G component in
-- bits 10..19, and a 10-bit R component in bits 0..9.
pattern FORMAT_A2B10G10R10_SINT_PACK32            = Format 69
-- | 'FORMAT_R16_UNORM' specifies a one-component, 16-bit unsigned normalized
-- format that has a single 16-bit R component.
pattern FORMAT_R16_UNORM                          = Format 70
-- | 'FORMAT_R16_SNORM' specifies a one-component, 16-bit signed normalized
-- format that has a single 16-bit R component.
pattern FORMAT_R16_SNORM                          = Format 71
-- | 'FORMAT_R16_USCALED' specifies a one-component, 16-bit unsigned scaled
-- integer format that has a single 16-bit R component.
pattern FORMAT_R16_USCALED                        = Format 72
-- | 'FORMAT_R16_SSCALED' specifies a one-component, 16-bit signed scaled
-- integer format that has a single 16-bit R component.
pattern FORMAT_R16_SSCALED                        = Format 73
-- | 'FORMAT_R16_UINT' specifies a one-component, 16-bit unsigned integer
-- format that has a single 16-bit R component.
pattern FORMAT_R16_UINT                           = Format 74
-- | 'FORMAT_R16_SINT' specifies a one-component, 16-bit signed integer
-- format that has a single 16-bit R component.
pattern FORMAT_R16_SINT                           = Format 75
-- | 'FORMAT_R16_SFLOAT' specifies a one-component, 16-bit signed
-- floating-point format that has a single 16-bit R component.
pattern FORMAT_R16_SFLOAT                         = Format 76
-- | 'FORMAT_R16G16_UNORM' specifies a two-component, 32-bit unsigned
-- normalized format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_UNORM                       = Format 77
-- | 'FORMAT_R16G16_SNORM' specifies a two-component, 32-bit signed
-- normalized format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_SNORM                       = Format 78
-- | 'FORMAT_R16G16_USCALED' specifies a two-component, 32-bit unsigned
-- scaled integer format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_USCALED                     = Format 79
-- | 'FORMAT_R16G16_SSCALED' specifies a two-component, 32-bit signed scaled
-- integer format that has a 16-bit R component in bytes 0..1, and a 16-bit
-- G component in bytes 2..3.
pattern FORMAT_R16G16_SSCALED                     = Format 80
-- | 'FORMAT_R16G16_UINT' specifies a two-component, 32-bit unsigned integer
-- format that has a 16-bit R component in bytes 0..1, and a 16-bit G
-- component in bytes 2..3.
pattern FORMAT_R16G16_UINT                        = Format 81
-- | 'FORMAT_R16G16_SINT' specifies a two-component, 32-bit signed integer
-- format that has a 16-bit R component in bytes 0..1, and a 16-bit G
-- component in bytes 2..3.
pattern FORMAT_R16G16_SINT                        = Format 82
-- | 'FORMAT_R16G16_SFLOAT' specifies a two-component, 32-bit signed
-- floating-point format that has a 16-bit R component in bytes 0..1, and a
-- 16-bit G component in bytes 2..3.
pattern FORMAT_R16G16_SFLOAT                      = Format 83
-- | 'FORMAT_R16G16B16_UNORM' specifies a three-component, 48-bit unsigned
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_UNORM                    = Format 84
-- | 'FORMAT_R16G16B16_SNORM' specifies a three-component, 48-bit signed
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_SNORM                    = Format 85
-- | 'FORMAT_R16G16B16_USCALED' specifies a three-component, 48-bit unsigned
-- scaled integer format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, and a 16-bit B component in bytes
-- 4..5.
pattern FORMAT_R16G16B16_USCALED                  = Format 86
-- | 'FORMAT_R16G16B16_SSCALED' specifies a three-component, 48-bit signed
-- scaled integer format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, and a 16-bit B component in bytes
-- 4..5.
pattern FORMAT_R16G16B16_SSCALED                  = Format 87
-- | 'FORMAT_R16G16B16_UINT' specifies a three-component, 48-bit unsigned
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_UINT                     = Format 88
-- | 'FORMAT_R16G16B16_SINT' specifies a three-component, 48-bit signed
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, and a 16-bit B component in bytes 4..5.
pattern FORMAT_R16G16B16_SINT                     = Format 89
-- | 'FORMAT_R16G16B16_SFLOAT' specifies a three-component, 48-bit signed
-- floating-point format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, and a 16-bit B component in bytes
-- 4..5.
pattern FORMAT_R16G16B16_SFLOAT                   = Format 90
-- | 'FORMAT_R16G16B16A16_UNORM' specifies a four-component, 64-bit unsigned
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, a 16-bit B component in bytes 4..5, and a
-- 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_UNORM                 = Format 91
-- | 'FORMAT_R16G16B16A16_SNORM' specifies a four-component, 64-bit signed
-- normalized format that has a 16-bit R component in bytes 0..1, a 16-bit
-- G component in bytes 2..3, a 16-bit B component in bytes 4..5, and a
-- 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SNORM                 = Format 92
-- | 'FORMAT_R16G16B16A16_USCALED' specifies a four-component, 64-bit
-- unsigned scaled integer format that has a 16-bit R component in bytes
-- 0..1, a 16-bit G component in bytes 2..3, a 16-bit B component in bytes
-- 4..5, and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_USCALED               = Format 93
-- | 'FORMAT_R16G16B16A16_SSCALED' specifies a four-component, 64-bit signed
-- scaled integer format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, a 16-bit B component in bytes 4..5,
-- and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SSCALED               = Format 94
-- | 'FORMAT_R16G16B16A16_UINT' specifies a four-component, 64-bit unsigned
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, a 16-bit B component in bytes 4..5, and a
-- 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_UINT                  = Format 95
-- | 'FORMAT_R16G16B16A16_SINT' specifies a four-component, 64-bit signed
-- integer format that has a 16-bit R component in bytes 0..1, a 16-bit G
-- component in bytes 2..3, a 16-bit B component in bytes 4..5, and a
-- 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SINT                  = Format 96
-- | 'FORMAT_R16G16B16A16_SFLOAT' specifies a four-component, 64-bit signed
-- floating-point format that has a 16-bit R component in bytes 0..1, a
-- 16-bit G component in bytes 2..3, a 16-bit B component in bytes 4..5,
-- and a 16-bit A component in bytes 6..7.
pattern FORMAT_R16G16B16A16_SFLOAT                = Format 97
-- | 'FORMAT_R32_UINT' specifies a one-component, 32-bit unsigned integer
-- format that has a single 32-bit R component.
pattern FORMAT_R32_UINT                           = Format 98
-- | 'FORMAT_R32_SINT' specifies a one-component, 32-bit signed integer
-- format that has a single 32-bit R component.
pattern FORMAT_R32_SINT                           = Format 99
-- | 'FORMAT_R32_SFLOAT' specifies a one-component, 32-bit signed
-- floating-point format that has a single 32-bit R component.
pattern FORMAT_R32_SFLOAT                         = Format 100
-- | 'FORMAT_R32G32_UINT' specifies a two-component, 64-bit unsigned integer
-- format that has a 32-bit R component in bytes 0..3, and a 32-bit G
-- component in bytes 4..7.
pattern FORMAT_R32G32_UINT                        = Format 101
-- | 'FORMAT_R32G32_SINT' specifies a two-component, 64-bit signed integer
-- format that has a 32-bit R component in bytes 0..3, and a 32-bit G
-- component in bytes 4..7.
pattern FORMAT_R32G32_SINT                        = Format 102
-- | 'FORMAT_R32G32_SFLOAT' specifies a two-component, 64-bit signed
-- floating-point format that has a 32-bit R component in bytes 0..3, and a
-- 32-bit G component in bytes 4..7.
pattern FORMAT_R32G32_SFLOAT                      = Format 103
-- | 'FORMAT_R32G32B32_UINT' specifies a three-component, 96-bit unsigned
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, and a 32-bit B component in bytes 8..11.
pattern FORMAT_R32G32B32_UINT                     = Format 104
-- | 'FORMAT_R32G32B32_SINT' specifies a three-component, 96-bit signed
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, and a 32-bit B component in bytes 8..11.
pattern FORMAT_R32G32B32_SINT                     = Format 105
-- | 'FORMAT_R32G32B32_SFLOAT' specifies a three-component, 96-bit signed
-- floating-point format that has a 32-bit R component in bytes 0..3, a
-- 32-bit G component in bytes 4..7, and a 32-bit B component in bytes
-- 8..11.
pattern FORMAT_R32G32B32_SFLOAT                   = Format 106
-- | 'FORMAT_R32G32B32A32_UINT' specifies a four-component, 128-bit unsigned
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, a 32-bit B component in bytes 8..11, and a
-- 32-bit A component in bytes 12..15.
pattern FORMAT_R32G32B32A32_UINT                  = Format 107
-- | 'FORMAT_R32G32B32A32_SINT' specifies a four-component, 128-bit signed
-- integer format that has a 32-bit R component in bytes 0..3, a 32-bit G
-- component in bytes 4..7, a 32-bit B component in bytes 8..11, and a
-- 32-bit A component in bytes 12..15.
pattern FORMAT_R32G32B32A32_SINT                  = Format 108
-- | 'FORMAT_R32G32B32A32_SFLOAT' specifies a four-component, 128-bit signed
-- floating-point format that has a 32-bit R component in bytes 0..3, a
-- 32-bit G component in bytes 4..7, a 32-bit B component in bytes 8..11,
-- and a 32-bit A component in bytes 12..15.
pattern FORMAT_R32G32B32A32_SFLOAT                = Format 109
-- | 'FORMAT_R64_UINT' specifies a one-component, 64-bit unsigned integer
-- format that has a single 64-bit R component.
pattern FORMAT_R64_UINT                           = Format 110
-- | 'FORMAT_R64_SINT' specifies a one-component, 64-bit signed integer
-- format that has a single 64-bit R component.
pattern FORMAT_R64_SINT                           = Format 111
-- | 'FORMAT_R64_SFLOAT' specifies a one-component, 64-bit signed
-- floating-point format that has a single 64-bit R component.
pattern FORMAT_R64_SFLOAT                         = Format 112
-- | 'FORMAT_R64G64_UINT' specifies a two-component, 128-bit unsigned integer
-- format that has a 64-bit R component in bytes 0..7, and a 64-bit G
-- component in bytes 8..15.
pattern FORMAT_R64G64_UINT                        = Format 113
-- | 'FORMAT_R64G64_SINT' specifies a two-component, 128-bit signed integer
-- format that has a 64-bit R component in bytes 0..7, and a 64-bit G
-- component in bytes 8..15.
pattern FORMAT_R64G64_SINT                        = Format 114
-- | 'FORMAT_R64G64_SFLOAT' specifies a two-component, 128-bit signed
-- floating-point format that has a 64-bit R component in bytes 0..7, and a
-- 64-bit G component in bytes 8..15.
pattern FORMAT_R64G64_SFLOAT                      = Format 115
-- | 'FORMAT_R64G64B64_UINT' specifies a three-component, 192-bit unsigned
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, and a 64-bit B component in bytes 16..23.
pattern FORMAT_R64G64B64_UINT                     = Format 116
-- | 'FORMAT_R64G64B64_SINT' specifies a three-component, 192-bit signed
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, and a 64-bit B component in bytes 16..23.
pattern FORMAT_R64G64B64_SINT                     = Format 117
-- | 'FORMAT_R64G64B64_SFLOAT' specifies a three-component, 192-bit signed
-- floating-point format that has a 64-bit R component in bytes 0..7, a
-- 64-bit G component in bytes 8..15, and a 64-bit B component in bytes
-- 16..23.
pattern FORMAT_R64G64B64_SFLOAT                   = Format 118
-- | 'FORMAT_R64G64B64A64_UINT' specifies a four-component, 256-bit unsigned
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, a 64-bit B component in bytes 16..23, and a
-- 64-bit A component in bytes 24..31.
pattern FORMAT_R64G64B64A64_UINT                  = Format 119
-- | 'FORMAT_R64G64B64A64_SINT' specifies a four-component, 256-bit signed
-- integer format that has a 64-bit R component in bytes 0..7, a 64-bit G
-- component in bytes 8..15, a 64-bit B component in bytes 16..23, and a
-- 64-bit A component in bytes 24..31.
pattern FORMAT_R64G64B64A64_SINT                  = Format 120
-- | 'FORMAT_R64G64B64A64_SFLOAT' specifies a four-component, 256-bit signed
-- floating-point format that has a 64-bit R component in bytes 0..7, a
-- 64-bit G component in bytes 8..15, a 64-bit B component in bytes 16..23,
-- and a 64-bit A component in bytes 24..31.
pattern FORMAT_R64G64B64A64_SFLOAT                = Format 121
-- | 'FORMAT_B10G11R11_UFLOAT_PACK32' specifies a three-component, 32-bit
-- packed unsigned floating-point format that has a 10-bit B component in
-- bits 22..31, an 11-bit G component in bits 11..21, an 11-bit R component
-- in bits 0..10. See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-fp10>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fundamentals-fp11>.
pattern FORMAT_B10G11R11_UFLOAT_PACK32            = Format 122
-- | 'FORMAT_E5B9G9R9_UFLOAT_PACK32' specifies a three-component, 32-bit
-- packed unsigned floating-point format that has a 5-bit shared exponent
-- in bits 27..31, a 9-bit B component mantissa in bits 18..26, a 9-bit G
-- component mantissa in bits 9..17, and a 9-bit R component mantissa in
-- bits 0..8.
pattern FORMAT_E5B9G9R9_UFLOAT_PACK32             = Format 123
-- | 'FORMAT_D16_UNORM' specifies a one-component, 16-bit unsigned normalized
-- format that has a single 16-bit depth component.
pattern FORMAT_D16_UNORM                          = Format 124
-- | 'FORMAT_X8_D24_UNORM_PACK32' specifies a two-component, 32-bit format
-- that has 24 unsigned normalized bits in the depth component and,
-- optionally:, 8 bits that are unused.
pattern FORMAT_X8_D24_UNORM_PACK32                = Format 125
-- | 'FORMAT_D32_SFLOAT' specifies a one-component, 32-bit signed
-- floating-point format that has 32-bits in the depth component.
pattern FORMAT_D32_SFLOAT                         = Format 126
-- | 'FORMAT_S8_UINT' specifies a one-component, 8-bit unsigned integer
-- format that has 8-bits in the stencil component.
pattern FORMAT_S8_UINT                            = Format 127
-- | 'FORMAT_D16_UNORM_S8_UINT' specifies a two-component, 24-bit format that
-- has 16 unsigned normalized bits in the depth component and 8 unsigned
-- integer bits in the stencil component.
pattern FORMAT_D16_UNORM_S8_UINT                  = Format 128
-- | 'FORMAT_D24_UNORM_S8_UINT' specifies a two-component, 32-bit packed
-- format that has 8 unsigned integer bits in the stencil component, and 24
-- unsigned normalized bits in the depth component.
pattern FORMAT_D24_UNORM_S8_UINT                  = Format 129
-- | 'FORMAT_D32_SFLOAT_S8_UINT' specifies a two-component format that has 32
-- signed float bits in the depth component and 8 unsigned integer bits in
-- the stencil component. There are optionally: 24-bits that are unused.
pattern FORMAT_D32_SFLOAT_S8_UINT                 = Format 130
-- | 'FORMAT_BC1_RGB_UNORM_BLOCK' specifies a three-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data. This format has
-- no alpha and is considered opaque.
pattern FORMAT_BC1_RGB_UNORM_BLOCK                = Format 131
-- | 'FORMAT_BC1_RGB_SRGB_BLOCK' specifies a three-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data with sRGB
-- nonlinear encoding. This format has no alpha and is considered opaque.
pattern FORMAT_BC1_RGB_SRGB_BLOCK                 = Format 132
-- | 'FORMAT_BC1_RGBA_UNORM_BLOCK' specifies a four-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data, and provides 1
-- bit of alpha.
pattern FORMAT_BC1_RGBA_UNORM_BLOCK               = Format 133
-- | 'FORMAT_BC1_RGBA_SRGB_BLOCK' specifies a four-component,
-- block-compressed format where each 64-bit compressed texel block encodes
-- a 4×4 rectangle of unsigned normalized RGB texel data with sRGB
-- nonlinear encoding, and provides 1 bit of alpha.
pattern FORMAT_BC1_RGBA_SRGB_BLOCK                = Format 134
-- | 'FORMAT_BC2_UNORM_BLOCK' specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values.
pattern FORMAT_BC2_UNORM_BLOCK                    = Format 135
-- | 'FORMAT_BC2_SRGB_BLOCK' specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values with sRGB nonlinear
-- encoding.
pattern FORMAT_BC2_SRGB_BLOCK                     = Format 136
-- | 'FORMAT_BC3_UNORM_BLOCK' specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values.
pattern FORMAT_BC3_UNORM_BLOCK                    = Format 137
-- | 'FORMAT_BC3_SRGB_BLOCK' specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with the first 64 bits encoding
-- alpha values followed by 64 bits encoding RGB values with sRGB nonlinear
-- encoding.
pattern FORMAT_BC3_SRGB_BLOCK                     = Format 138
-- | 'FORMAT_BC4_UNORM_BLOCK' specifies a one-component, block-compressed
-- format where each 64-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized red texel data.
pattern FORMAT_BC4_UNORM_BLOCK                    = Format 139
-- | 'FORMAT_BC4_SNORM_BLOCK' specifies a one-component, block-compressed
-- format where each 64-bit compressed texel block encodes a 4×4 rectangle
-- of signed normalized red texel data.
pattern FORMAT_BC4_SNORM_BLOCK                    = Format 140
-- | 'FORMAT_BC5_UNORM_BLOCK' specifies a two-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RG texel data with the first 64 bits encoding red
-- values followed by 64 bits encoding green values.
pattern FORMAT_BC5_UNORM_BLOCK                    = Format 141
-- | 'FORMAT_BC5_SNORM_BLOCK' specifies a two-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of signed normalized RG texel data with the first 64 bits encoding red
-- values followed by 64 bits encoding green values.
pattern FORMAT_BC5_SNORM_BLOCK                    = Format 142
-- | 'FORMAT_BC6H_UFLOAT_BLOCK' specifies a three-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned floating-point RGB texel data.
pattern FORMAT_BC6H_UFLOAT_BLOCK                  = Format 143
-- | 'FORMAT_BC6H_SFLOAT_BLOCK' specifies a three-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of signed floating-point RGB texel data.
pattern FORMAT_BC6H_SFLOAT_BLOCK                  = Format 144
-- | 'FORMAT_BC7_UNORM_BLOCK' specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data.
pattern FORMAT_BC7_UNORM_BLOCK                    = Format 145
-- | 'FORMAT_BC7_SRGB_BLOCK' specifies a four-component, block-compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern FORMAT_BC7_SRGB_BLOCK                     = Format 146
-- | 'FORMAT_ETC2_R8G8B8_UNORM_BLOCK' specifies a three-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data. This format has no
-- alpha and is considered opaque.
pattern FORMAT_ETC2_R8G8B8_UNORM_BLOCK            = Format 147
-- | 'FORMAT_ETC2_R8G8B8_SRGB_BLOCK' specifies a three-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data with sRGB nonlinear
-- encoding. This format has no alpha and is considered opaque.
pattern FORMAT_ETC2_R8G8B8_SRGB_BLOCK             = Format 148
-- | 'FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK' specifies a four-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data, and provides 1 bit of
-- alpha.
pattern FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK          = Format 149
-- | 'FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK' specifies a four-component, ETC2
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGB texel data with sRGB nonlinear
-- encoding, and provides 1 bit of alpha.
pattern FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK           = Format 150
-- | 'FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK' specifies a four-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data with the first 64
-- bits encoding alpha values followed by 64 bits encoding RGB values.
pattern FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK          = Format 151
-- | 'FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK' specifies a four-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data with the first 64
-- bits encoding alpha values followed by 64 bits encoding RGB values with
-- sRGB nonlinear encoding applied.
pattern FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK           = Format 152
-- | 'FORMAT_EAC_R11_UNORM_BLOCK' specifies a one-component, ETC2 compressed
-- format where each 64-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized red texel data.
pattern FORMAT_EAC_R11_UNORM_BLOCK                = Format 153
-- | 'FORMAT_EAC_R11_SNORM_BLOCK' specifies a one-component, ETC2 compressed
-- format where each 64-bit compressed texel block encodes a 4×4 rectangle
-- of signed normalized red texel data.
pattern FORMAT_EAC_R11_SNORM_BLOCK                = Format 154
-- | 'FORMAT_EAC_R11G11_UNORM_BLOCK' specifies a two-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RG texel data with the first 64
-- bits encoding red values followed by 64 bits encoding green values.
pattern FORMAT_EAC_R11G11_UNORM_BLOCK             = Format 155
-- | 'FORMAT_EAC_R11G11_SNORM_BLOCK' specifies a two-component, ETC2
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of signed normalized RG texel data with the first 64 bits
-- encoding red values followed by 64 bits encoding green values.
pattern FORMAT_EAC_R11G11_SNORM_BLOCK             = Format 156
-- | 'FORMAT_ASTC_4x4_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_4x4_UNORM_BLOCK               = Format 157
-- | 'FORMAT_ASTC_4x4_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes a 4×4 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern FORMAT_ASTC_4x4_SRGB_BLOCK                = Format 158
-- | 'FORMAT_ASTC_5x4_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×4 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_5x4_UNORM_BLOCK               = Format 159
-- | 'FORMAT_ASTC_5x4_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes a 5×4 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern FORMAT_ASTC_5x4_SRGB_BLOCK                = Format 160
-- | 'FORMAT_ASTC_5x5_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×5 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_5x5_UNORM_BLOCK               = Format 161
-- | 'FORMAT_ASTC_5x5_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes a 5×5 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern FORMAT_ASTC_5x5_SRGB_BLOCK                = Format 162
-- | 'FORMAT_ASTC_6x5_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×5 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_6x5_UNORM_BLOCK               = Format 163
-- | 'FORMAT_ASTC_6x5_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes a 6×5 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern FORMAT_ASTC_6x5_SRGB_BLOCK                = Format 164
-- | 'FORMAT_ASTC_6x6_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×6 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_6x6_UNORM_BLOCK               = Format 165
-- | 'FORMAT_ASTC_6x6_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes a 6×6 rectangle
-- of unsigned normalized RGBA texel data with sRGB nonlinear encoding
-- applied to the RGB components.
pattern FORMAT_ASTC_6x6_SRGB_BLOCK                = Format 166
-- | 'FORMAT_ASTC_8x5_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×5 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_8x5_UNORM_BLOCK               = Format 167
-- | 'FORMAT_ASTC_8x5_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes an 8×5
-- rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_ASTC_8x5_SRGB_BLOCK                = Format 168
-- | 'FORMAT_ASTC_8x6_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×6 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_8x6_UNORM_BLOCK               = Format 169
-- | 'FORMAT_ASTC_8x6_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes an 8×6
-- rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_ASTC_8x6_SRGB_BLOCK                = Format 170
-- | 'FORMAT_ASTC_8x8_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes an
-- 8×8 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_8x8_UNORM_BLOCK               = Format 171
-- | 'FORMAT_ASTC_8x8_SRGB_BLOCK' specifies a four-component, ASTC compressed
-- format where each 128-bit compressed texel block encodes an 8×8
-- rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_ASTC_8x8_SRGB_BLOCK                = Format 172
-- | 'FORMAT_ASTC_10x5_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×5 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_10x5_UNORM_BLOCK              = Format 173
-- | 'FORMAT_ASTC_10x5_SRGB_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×5 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x5_SRGB_BLOCK               = Format 174
-- | 'FORMAT_ASTC_10x6_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×6 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_10x6_UNORM_BLOCK              = Format 175
-- | 'FORMAT_ASTC_10x6_SRGB_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×6 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x6_SRGB_BLOCK               = Format 176
-- | 'FORMAT_ASTC_10x8_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×8 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_10x8_UNORM_BLOCK              = Format 177
-- | 'FORMAT_ASTC_10x8_SRGB_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×8 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x8_SRGB_BLOCK               = Format 178
-- | 'FORMAT_ASTC_10x10_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×10 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_10x10_UNORM_BLOCK             = Format 179
-- | 'FORMAT_ASTC_10x10_SRGB_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×10 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_10x10_SRGB_BLOCK              = Format 180
-- | 'FORMAT_ASTC_12x10_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×10 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_12x10_UNORM_BLOCK             = Format 181
-- | 'FORMAT_ASTC_12x10_SRGB_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×10 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_12x10_SRGB_BLOCK              = Format 182
-- | 'FORMAT_ASTC_12x12_UNORM_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×12 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_ASTC_12x12_UNORM_BLOCK             = Format 183
-- | 'FORMAT_ASTC_12x12_SRGB_BLOCK' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×12 rectangle of unsigned normalized RGBA texel data with sRGB
-- nonlinear encoding applied to the RGB components.
pattern FORMAT_ASTC_12x12_SRGB_BLOCK              = Format 184
-- | 'FORMAT_A4B4G4R4_UNORM_PACK16_EXT' specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 4-bit A component in bits
-- 12..15, a 4-bit B component in bits 8..11, a 4-bit G component in bits
-- 4..7, and a 4-bit R component in bits 0..3.
pattern FORMAT_A4B4G4R4_UNORM_PACK16_EXT          = Format 1000340001
-- | 'FORMAT_A4R4G4B4_UNORM_PACK16_EXT' specifies a four-component, 16-bit
-- packed unsigned normalized format that has a 4-bit A component in bits
-- 12..15, a 4-bit R component in bits 8..11, a 4-bit G component in bits
-- 4..7, and a 4-bit B component in bits 0..3.
pattern FORMAT_A4R4G4B4_UNORM_PACK16_EXT          = Format 1000340000
-- | 'FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×12 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT        = Format 1000066013
-- | 'FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 12×10 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT        = Format 1000066012
-- | 'FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×10 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT        = Format 1000066011
-- | 'FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×8 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT         = Format 1000066010
-- | 'FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×6 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT         = Format 1000066009
-- | 'FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 10×5 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT         = Format 1000066008
-- | 'FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 8×8 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT          = Format 1000066007
-- | 'FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 8×6 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT          = Format 1000066006
-- | 'FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 8×5 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT          = Format 1000066005
-- | 'FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×6 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT          = Format 1000066004
-- | 'FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 6×5 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT          = Format 1000066003
-- | 'FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×5 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT          = Format 1000066002
-- | 'FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 5×4 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT          = Format 1000066001
-- | 'FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT' specifies a four-component, ASTC
-- compressed format where each 128-bit compressed texel block encodes a
-- 4×4 rectangle of signed floating-point RGBA texel data.
pattern FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT          = Format 1000066000
-- | 'FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG         = Format 1000054007
-- | 'FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes an
-- 8×4 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG         = Format 1000054006
-- | 'FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG         = Format 1000054005
-- | 'FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes an
-- 8×4 rectangle of unsigned normalized RGBA texel data with sRGB nonlinear
-- encoding applied to the RGB components.
pattern FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG         = Format 1000054004
-- | 'FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG        = Format 1000054003
-- | 'FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes an
-- 8×4 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG        = Format 1000054002
-- | 'FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes a 4×4
-- rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG        = Format 1000054001
-- | 'FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG' specifies a four-component, PVRTC
-- compressed format where each 64-bit compressed texel block encodes an
-- 8×4 rectangle of unsigned normalized RGBA texel data.
pattern FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG        = Format 1000054000
-- | 'FORMAT_G16_B16_R16_3PLANE_444_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, a 16-bit B component in each 16-bit word of plane 1, and a
-- 16-bit R component in each 16-bit word of plane 2. Each plane has the
-- same dimensions and each R, G and B component contributes to a single
-- texel. The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane.
pattern FORMAT_G16_B16_R16_3PLANE_444_UNORM       = Format 1000156033
-- | 'FORMAT_G16_B16R16_2PLANE_422_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, and a two-component, 32-bit BR plane 1 consisting of a
-- 16-bit B component in the word in bytes 0..1, and a 16-bit R component
-- in the word in bytes 2..3. The horizontal dimensions of the BR plane is
-- halved relative to the image dimensions, and each R and B value is
-- shared with the G components for which
-- \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G16_B16R16_2PLANE_422_UNORM        = Format 1000156032
-- | 'FORMAT_G16_B16_R16_3PLANE_422_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, a 16-bit B component in each 16-bit word of plane 1, and a
-- 16-bit R component in each 16-bit word of plane 2. The horizontal
-- dimension of the R and B plane is halved relative to the image
-- dimensions, and each R and B value is shared with the G components for
-- which \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G16_B16_R16_3PLANE_422_UNORM       = Format 1000156031
-- | 'FORMAT_G16_B16R16_2PLANE_420_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, and a two-component, 32-bit BR plane 1 consisting of a
-- 16-bit B component in the word in bytes 0..1, and a 16-bit R component
-- in the word in bytes 2..3. The horizontal and vertical dimensions of the
-- BR plane is halved relative to the image dimensions, and each R and B
-- value is shared with the G components for which
-- \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G16_B16R16_2PLANE_420_UNORM        = Format 1000156030
-- | 'FORMAT_G16_B16_R16_3PLANE_420_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has a 16-bit G component in each 16-bit word
-- of plane 0, a 16-bit B component in each 16-bit word of plane 1, and a
-- 16-bit R component in each 16-bit word of plane 2. The horizontal and
-- vertical dimensions of the R and B planes are halved relative to the
-- image dimensions, and each R and B component is shared with the G
-- components for which \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G16_B16_R16_3PLANE_420_UNORM       = Format 1000156029
-- | 'FORMAT_B16G16R16G16_422_UNORM' specifies a four-component, 64-bit
-- format containing a pair of G components, an R component, and a B
-- component, collectively encoding a 2×1 rectangle of unsigned normalized
-- RGB texel data. One G value is present at each /i/ coordinate, with the
-- B and R values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has a 16-bit B component
-- in the word in bytes 0..1, a 16-bit G component for the even /i/
-- coordinate in the word in bytes 2..3, a 16-bit R component in the word
-- in bytes 4..5, and a 16-bit G component for the odd /i/ coordinate in
-- the word in bytes 6..7. This format only supports images with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern FORMAT_B16G16R16G16_422_UNORM             = Format 1000156028
-- | 'FORMAT_G16B16G16R16_422_UNORM' specifies a four-component, 64-bit
-- format containing a pair of G components, an R component, and a B
-- component, collectively encoding a 2×1 rectangle of unsigned normalized
-- RGB texel data. One G value is present at each /i/ coordinate, with the
-- B and R values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has a 16-bit G component
-- for the even /i/ coordinate in the word in bytes 0..1, a 16-bit B
-- component in the word in bytes 2..3, a 16-bit G component for the odd
-- /i/ coordinate in the word in bytes 4..5, and a 16-bit R component in
-- the word in bytes 6..7. This format only supports images with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern FORMAT_G16B16G16R16_422_UNORM             = Format 1000156027
-- | 'FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16' specifies an
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, a 12-bit B component
-- in the top 12 bits of each 16-bit word of plane 1, and a 12-bit R
-- component in the top 12 bits of each 16-bit word of plane 2, with the
-- bottom 4 bits of each word unused. Each plane has the same dimensions
-- and each R, G and B component contributes to a single texel. The
-- location of each plane when this image is in linear layout can be
-- determined via 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane.
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = Format 1000156026
-- | 'FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16' specifies an unsigned
-- normalized /multi-planar format/ that has a 12-bit G component in the
-- top 12 bits of each 16-bit word of plane 0, and a two-component, 32-bit
-- BR plane 1 consisting of a 12-bit B component in the top 12 bits of the
-- word in bytes 0..1, and a 12-bit R component in the top 12 bits of the
-- word in bytes 2..3, the bottom 4 bits of each word unused. The
-- horizontal dimensions of the BR plane is halved relative to the image
-- dimensions, and each R and B value is shared with the G components for
-- which \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = Format 1000156025
-- | 'FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16' specifies an
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, a 12-bit B component
-- in the top 12 bits of each 16-bit word of plane 1, and a 12-bit R
-- component in the top 12 bits of each 16-bit word of plane 2, with the
-- bottom 4 bits of each word unused. The horizontal dimension of the R and
-- B plane is halved relative to the image dimensions, and each R and B
-- value is shared with the G components for which
-- \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = Format 1000156024
-- | 'FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16' specifies an unsigned
-- normalized /multi-planar format/ that has a 12-bit G component in the
-- top 12 bits of each 16-bit word of plane 0, and a two-component, 32-bit
-- BR plane 1 consisting of a 12-bit B component in the top 12 bits of the
-- word in bytes 0..1, and a 12-bit R component in the top 12 bits of the
-- word in bytes 2..3, the bottom 4 bits of each word unused. The
-- horizontal and vertical dimensions of the BR plane is halved relative to
-- the image dimensions, and each R and B value is shared with the G
-- components for which \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = Format 1000156023
-- | 'FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16' specifies an
-- unsigned normalized /multi-planar format/ that has a 12-bit G component
-- in the top 12 bits of each 16-bit word of plane 0, a 12-bit B component
-- in the top 12 bits of each 16-bit word of plane 1, and a 12-bit R
-- component in the top 12 bits of each 16-bit word of plane 2, with the
-- bottom 4 bits of each word unused. The horizontal and vertical
-- dimensions of the R and B planes are halved relative to the image
-- dimensions, and each R and B component is shared with the G components
-- for which \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = Format 1000156022
-- | 'FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16' specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 12-bit B component in the top 12 bits of the word in bytes 0..1, a
-- 12-bit G component for the even /i/ coordinate in the top 12 bits of the
-- word in bytes 2..3, a 12-bit R component in the top 12 bits of the word
-- in bytes 4..5, and a 12-bit G component for the odd /i/ coordinate in
-- the top 12 bits of the word in bytes 6..7, with the bottom 4 bits of
-- each word unused. This format only supports images with a width that is
-- a multiple of two. For the purposes of the constraints on copy extents,
-- this format is treated as a compressed format with a 2×1 compressed
-- texel block.
pattern FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = Format 1000156021
-- | 'FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16' specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 12-bit G component for the even /i/ coordinate in the top 12 bits of
-- the word in bytes 0..1, a 12-bit B component in the top 12 bits of the
-- word in bytes 2..3, a 12-bit G component for the odd /i/ coordinate in
-- the top 12 bits of the word in bytes 4..5, and a 12-bit R component in
-- the top 12 bits of the word in bytes 6..7, with the bottom 4 bits of
-- each word unused. This format only supports images with a width that is
-- a multiple of two. For the purposes of the constraints on copy extents,
-- this format is treated as a compressed format with a 2×1 compressed
-- texel block.
pattern FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = Format 1000156020
-- | 'FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16' specifies a four-component,
-- 64-bit unsigned normalized format that has a 12-bit R component in the
-- top 12 bits of the word in bytes 0..1, a 12-bit G component in the top
-- 12 bits of the word in bytes 2..3, a 12-bit B component in the top 12
-- bits of the word in bytes 4..5, and a 12-bit A component in the top 12
-- bits of the word in bytes 6..7, with the bottom 4 bits of each word
-- unused.
pattern FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = Format 1000156019
-- | 'FORMAT_R12X4G12X4_UNORM_2PACK16' specifies a two-component, 32-bit
-- unsigned normalized format that has a 12-bit R component in the top 12
-- bits of the word in bytes 0..1, and a 12-bit G component in the top 12
-- bits of the word in bytes 2..3, with the bottom 4 bits of each word
-- unused.
pattern FORMAT_R12X4G12X4_UNORM_2PACK16           = Format 1000156018
-- | 'FORMAT_R12X4_UNORM_PACK16' specifies a one-component, 16-bit unsigned
-- normalized format that has a single 12-bit R component in the top 12
-- bits of a 16-bit word, with the bottom 4 bits unused.
pattern FORMAT_R12X4_UNORM_PACK16                 = Format 1000156017
-- | 'FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16' specifies an
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, a 10-bit B component
-- in the top 10 bits of each 16-bit word of plane 1, and a 10-bit R
-- component in the top 10 bits of each 16-bit word of plane 2, with the
-- bottom 6 bits of each word unused. Each plane has the same dimensions
-- and each R, G and B component contributes to a single texel. The
-- location of each plane when this image is in linear layout can be
-- determined via 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane.
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = Format 1000156016
-- | 'FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16' specifies an unsigned
-- normalized /multi-planar format/ that has a 10-bit G component in the
-- top 10 bits of each 16-bit word of plane 0, and a two-component, 32-bit
-- BR plane 1 consisting of a 10-bit B component in the top 10 bits of the
-- word in bytes 0..1, and a 10-bit R component in the top 10 bits of the
-- word in bytes 2..3, the bottom 6 bits of each word unused. The
-- horizontal dimensions of the BR plane is halved relative to the image
-- dimensions, and each R and B value is shared with the G components for
-- which \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = Format 1000156015
-- | 'FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16' specifies an
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, a 10-bit B component
-- in the top 10 bits of each 16-bit word of plane 1, and a 10-bit R
-- component in the top 10 bits of each 16-bit word of plane 2, with the
-- bottom 6 bits of each word unused. The horizontal dimension of the R and
-- B plane is halved relative to the image dimensions, and each R and B
-- value is shared with the G components for which
-- \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = Format 1000156014
-- | 'FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16' specifies an unsigned
-- normalized /multi-planar format/ that has a 10-bit G component in the
-- top 10 bits of each 16-bit word of plane 0, and a two-component, 32-bit
-- BR plane 1 consisting of a 10-bit B component in the top 10 bits of the
-- word in bytes 0..1, and a 10-bit R component in the top 10 bits of the
-- word in bytes 2..3, the bottom 6 bits of each word unused. The
-- horizontal and vertical dimensions of the BR plane is halved relative to
-- the image dimensions, and each R and B value is shared with the G
-- components for which \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = Format 1000156013
-- | 'FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16' specifies an
-- unsigned normalized /multi-planar format/ that has a 10-bit G component
-- in the top 10 bits of each 16-bit word of plane 0, a 10-bit B component
-- in the top 10 bits of each 16-bit word of plane 1, and a 10-bit R
-- component in the top 10 bits of each 16-bit word of plane 2, with the
-- bottom 6 bits of each word unused. The horizontal and vertical
-- dimensions of the R and B planes are halved relative to the image
-- dimensions, and each R and B component is shared with the G components
-- for which \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = Format 1000156012
-- | 'FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16' specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 10-bit B component in the top 10 bits of the word in bytes 0..1, a
-- 10-bit G component for the even /i/ coordinate in the top 10 bits of the
-- word in bytes 2..3, a 10-bit R component in the top 10 bits of the word
-- in bytes 4..5, and a 10-bit G component for the odd /i/ coordinate in
-- the top 10 bits of the word in bytes 6..7, with the bottom 6 bits of
-- each word unused. This format only supports images with a width that is
-- a multiple of two. For the purposes of the constraints on copy extents,
-- this format is treated as a compressed format with a 2×1 compressed
-- texel block.
pattern FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = Format 1000156011
-- | 'FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16' specifies a
-- four-component, 64-bit format containing a pair of G components, an R
-- component, and a B component, collectively encoding a 2×1 rectangle of
-- unsigned normalized RGB texel data. One G value is present at each /i/
-- coordinate, with the B and R values shared across both G values and thus
-- recorded at half the horizontal resolution of the image. This format has
-- a 10-bit G component for the even /i/ coordinate in the top 10 bits of
-- the word in bytes 0..1, a 10-bit B component in the top 10 bits of the
-- word in bytes 2..3, a 10-bit G component for the odd /i/ coordinate in
-- the top 10 bits of the word in bytes 4..5, and a 10-bit R component in
-- the top 10 bits of the word in bytes 6..7, with the bottom 6 bits of
-- each word unused. This format only supports images with a width that is
-- a multiple of two. For the purposes of the constraints on copy extents,
-- this format is treated as a compressed format with a 2×1 compressed
-- texel block.
pattern FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = Format 1000156010
-- | 'FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16' specifies a four-component,
-- 64-bit unsigned normalized format that has a 10-bit R component in the
-- top 10 bits of the word in bytes 0..1, a 10-bit G component in the top
-- 10 bits of the word in bytes 2..3, a 10-bit B component in the top 10
-- bits of the word in bytes 4..5, and a 10-bit A component in the top 10
-- bits of the word in bytes 6..7, with the bottom 6 bits of each word
-- unused.
pattern FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = Format 1000156009
-- | 'FORMAT_R10X6G10X6_UNORM_2PACK16' specifies a two-component, 32-bit
-- unsigned normalized format that has a 10-bit R component in the top 10
-- bits of the word in bytes 0..1, and a 10-bit G component in the top 10
-- bits of the word in bytes 2..3, with the bottom 6 bits of each word
-- unused.
pattern FORMAT_R10X6G10X6_UNORM_2PACK16           = Format 1000156008
-- | 'FORMAT_R10X6_UNORM_PACK16' specifies a one-component, 16-bit unsigned
-- normalized format that has a single 10-bit R component in the top 10
-- bits of a 16-bit word, with the bottom 6 bits unused.
pattern FORMAT_R10X6_UNORM_PACK16                 = Format 1000156007
-- | 'FORMAT_G8_B8_R8_3PLANE_444_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, an 8-bit
-- B component in plane 1, and an 8-bit R component in plane 2. Each plane
-- has the same dimensions and each R, G and B component contributes to a
-- single texel. The location of each plane when this image is in linear
-- layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane.
pattern FORMAT_G8_B8_R8_3PLANE_444_UNORM          = Format 1000156006
-- | 'FORMAT_G8_B8R8_2PLANE_422_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, and a
-- two-component, 16-bit BR plane 1 consisting of an 8-bit B component in
-- byte 0 and an 8-bit R component in byte 1. The horizontal dimensions of
-- the BR plane is halved relative to the image dimensions, and each R and
-- B value is shared with the G components for which
-- \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G8_B8R8_2PLANE_422_UNORM           = Format 1000156005
-- | 'FORMAT_G8_B8_R8_3PLANE_422_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, an 8-bit
-- B component in plane 1, and an 8-bit R component in plane 2. The
-- horizontal dimension of the R and B plane is halved relative to the
-- image dimensions, and each R and B value is shared with the G components
-- for which \(\left\lfloor i_G \times 0.5 \right\rfloor = i_B =
-- i_R\). The location of each plane when this image is in linear layout
-- can be determined via 'Vulkan.Core10.Image.getImageSubresourceLayout',
-- using 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT'
-- for the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width that is a
-- multiple of two.
pattern FORMAT_G8_B8_R8_3PLANE_422_UNORM          = Format 1000156004
-- | 'FORMAT_G8_B8R8_2PLANE_420_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, and a
-- two-component, 16-bit BR plane 1 consisting of an 8-bit B component in
-- byte 0 and an 8-bit R component in byte 1. The horizontal and vertical
-- dimensions of the BR plane is halved relative to the image dimensions,
-- and each R and B value is shared with the G components for which
-- \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the BR plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G8_B8R8_2PLANE_420_UNORM           = Format 1000156003
-- | 'FORMAT_G8_B8_R8_3PLANE_420_UNORM' specifies an unsigned normalized
-- /multi-planar format/ that has an 8-bit G component in plane 0, an 8-bit
-- B component in plane 1, and an 8-bit R component in plane 2. The
-- horizontal and vertical dimensions of the R and B planes are halved
-- relative to the image dimensions, and each R and B component is shared
-- with the G components for which \(\left\lfloor i_G \times 0.5
-- \right\rfloor = i_B = i_R\) and \(\left\lfloor j_G \times 0.5
-- \right\rfloor = j_B = j_R\). The location of each plane when this image
-- is in linear layout can be determined via
-- 'Vulkan.Core10.Image.getImageSubresourceLayout', using
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_0_BIT' for
-- the G plane,
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_1_BIT' for
-- the B plane, and
-- 'Vulkan.Core10.Enums.ImageAspectFlagBits.IMAGE_ASPECT_PLANE_2_BIT' for
-- the R plane. This format only supports images with a width and height
-- that is a multiple of two.
pattern FORMAT_G8_B8_R8_3PLANE_420_UNORM          = Format 1000156002
-- | 'FORMAT_B8G8R8G8_422_UNORM' specifies a four-component, 32-bit format
-- containing a pair of G components, an R component, and a B component,
-- collectively encoding a 2×1 rectangle of unsigned normalized RGB texel
-- data. One G value is present at each /i/ coordinate, with the B and R
-- values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has an 8-bit B component
-- in byte 0, an 8-bit G component for the even /i/ coordinate in byte 1,
-- an 8-bit R component in byte 2, and an 8-bit G component for the odd /i/
-- coordinate in byte 3. This format only supports images with a width that
-- is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern FORMAT_B8G8R8G8_422_UNORM                 = Format 1000156001
-- | 'FORMAT_G8B8G8R8_422_UNORM' specifies a four-component, 32-bit format
-- containing a pair of G components, an R component, and a B component,
-- collectively encoding a 2×1 rectangle of unsigned normalized RGB texel
-- data. One G value is present at each /i/ coordinate, with the B and R
-- values shared across both G values and thus recorded at half the
-- horizontal resolution of the image. This format has an 8-bit G component
-- for the even /i/ coordinate in byte 0, an 8-bit B component in byte 1,
-- an 8-bit G component for the odd /i/ coordinate in byte 2, and an 8-bit
-- R component in byte 3. This format only supports images with a width
-- that is a multiple of two. For the purposes of the constraints on copy
-- extents, this format is treated as a compressed format with a 2×1
-- compressed texel block.
pattern FORMAT_G8B8G8R8_422_UNORM                 = Format 1000156000
{-# complete FORMAT_UNDEFINED,
             FORMAT_R4G4_UNORM_PACK8,
             FORMAT_R4G4B4A4_UNORM_PACK16,
             FORMAT_B4G4R4A4_UNORM_PACK16,
             FORMAT_R5G6B5_UNORM_PACK16,
             FORMAT_B5G6R5_UNORM_PACK16,
             FORMAT_R5G5B5A1_UNORM_PACK16,
             FORMAT_B5G5R5A1_UNORM_PACK16,
             FORMAT_A1R5G5B5_UNORM_PACK16,
             FORMAT_R8_UNORM,
             FORMAT_R8_SNORM,
             FORMAT_R8_USCALED,
             FORMAT_R8_SSCALED,
             FORMAT_R8_UINT,
             FORMAT_R8_SINT,
             FORMAT_R8_SRGB,
             FORMAT_R8G8_UNORM,
             FORMAT_R8G8_SNORM,
             FORMAT_R8G8_USCALED,
             FORMAT_R8G8_SSCALED,
             FORMAT_R8G8_UINT,
             FORMAT_R8G8_SINT,
             FORMAT_R8G8_SRGB,
             FORMAT_R8G8B8_UNORM,
             FORMAT_R8G8B8_SNORM,
             FORMAT_R8G8B8_USCALED,
             FORMAT_R8G8B8_SSCALED,
             FORMAT_R8G8B8_UINT,
             FORMAT_R8G8B8_SINT,
             FORMAT_R8G8B8_SRGB,
             FORMAT_B8G8R8_UNORM,
             FORMAT_B8G8R8_SNORM,
             FORMAT_B8G8R8_USCALED,
             FORMAT_B8G8R8_SSCALED,
             FORMAT_B8G8R8_UINT,
             FORMAT_B8G8R8_SINT,
             FORMAT_B8G8R8_SRGB,
             FORMAT_R8G8B8A8_UNORM,
             FORMAT_R8G8B8A8_SNORM,
             FORMAT_R8G8B8A8_USCALED,
             FORMAT_R8G8B8A8_SSCALED,
             FORMAT_R8G8B8A8_UINT,
             FORMAT_R8G8B8A8_SINT,
             FORMAT_R8G8B8A8_SRGB,
             FORMAT_B8G8R8A8_UNORM,
             FORMAT_B8G8R8A8_SNORM,
             FORMAT_B8G8R8A8_USCALED,
             FORMAT_B8G8R8A8_SSCALED,
             FORMAT_B8G8R8A8_UINT,
             FORMAT_B8G8R8A8_SINT,
             FORMAT_B8G8R8A8_SRGB,
             FORMAT_A8B8G8R8_UNORM_PACK32,
             FORMAT_A8B8G8R8_SNORM_PACK32,
             FORMAT_A8B8G8R8_USCALED_PACK32,
             FORMAT_A8B8G8R8_SSCALED_PACK32,
             FORMAT_A8B8G8R8_UINT_PACK32,
             FORMAT_A8B8G8R8_SINT_PACK32,
             FORMAT_A8B8G8R8_SRGB_PACK32,
             FORMAT_A2R10G10B10_UNORM_PACK32,
             FORMAT_A2R10G10B10_SNORM_PACK32,
             FORMAT_A2R10G10B10_USCALED_PACK32,
             FORMAT_A2R10G10B10_SSCALED_PACK32,
             FORMAT_A2R10G10B10_UINT_PACK32,
             FORMAT_A2R10G10B10_SINT_PACK32,
             FORMAT_A2B10G10R10_UNORM_PACK32,
             FORMAT_A2B10G10R10_SNORM_PACK32,
             FORMAT_A2B10G10R10_USCALED_PACK32,
             FORMAT_A2B10G10R10_SSCALED_PACK32,
             FORMAT_A2B10G10R10_UINT_PACK32,
             FORMAT_A2B10G10R10_SINT_PACK32,
             FORMAT_R16_UNORM,
             FORMAT_R16_SNORM,
             FORMAT_R16_USCALED,
             FORMAT_R16_SSCALED,
             FORMAT_R16_UINT,
             FORMAT_R16_SINT,
             FORMAT_R16_SFLOAT,
             FORMAT_R16G16_UNORM,
             FORMAT_R16G16_SNORM,
             FORMAT_R16G16_USCALED,
             FORMAT_R16G16_SSCALED,
             FORMAT_R16G16_UINT,
             FORMAT_R16G16_SINT,
             FORMAT_R16G16_SFLOAT,
             FORMAT_R16G16B16_UNORM,
             FORMAT_R16G16B16_SNORM,
             FORMAT_R16G16B16_USCALED,
             FORMAT_R16G16B16_SSCALED,
             FORMAT_R16G16B16_UINT,
             FORMAT_R16G16B16_SINT,
             FORMAT_R16G16B16_SFLOAT,
             FORMAT_R16G16B16A16_UNORM,
             FORMAT_R16G16B16A16_SNORM,
             FORMAT_R16G16B16A16_USCALED,
             FORMAT_R16G16B16A16_SSCALED,
             FORMAT_R16G16B16A16_UINT,
             FORMAT_R16G16B16A16_SINT,
             FORMAT_R16G16B16A16_SFLOAT,
             FORMAT_R32_UINT,
             FORMAT_R32_SINT,
             FORMAT_R32_SFLOAT,
             FORMAT_R32G32_UINT,
             FORMAT_R32G32_SINT,
             FORMAT_R32G32_SFLOAT,
             FORMAT_R32G32B32_UINT,
             FORMAT_R32G32B32_SINT,
             FORMAT_R32G32B32_SFLOAT,
             FORMAT_R32G32B32A32_UINT,
             FORMAT_R32G32B32A32_SINT,
             FORMAT_R32G32B32A32_SFLOAT,
             FORMAT_R64_UINT,
             FORMAT_R64_SINT,
             FORMAT_R64_SFLOAT,
             FORMAT_R64G64_UINT,
             FORMAT_R64G64_SINT,
             FORMAT_R64G64_SFLOAT,
             FORMAT_R64G64B64_UINT,
             FORMAT_R64G64B64_SINT,
             FORMAT_R64G64B64_SFLOAT,
             FORMAT_R64G64B64A64_UINT,
             FORMAT_R64G64B64A64_SINT,
             FORMAT_R64G64B64A64_SFLOAT,
             FORMAT_B10G11R11_UFLOAT_PACK32,
             FORMAT_E5B9G9R9_UFLOAT_PACK32,
             FORMAT_D16_UNORM,
             FORMAT_X8_D24_UNORM_PACK32,
             FORMAT_D32_SFLOAT,
             FORMAT_S8_UINT,
             FORMAT_D16_UNORM_S8_UINT,
             FORMAT_D24_UNORM_S8_UINT,
             FORMAT_D32_SFLOAT_S8_UINT,
             FORMAT_BC1_RGB_UNORM_BLOCK,
             FORMAT_BC1_RGB_SRGB_BLOCK,
             FORMAT_BC1_RGBA_UNORM_BLOCK,
             FORMAT_BC1_RGBA_SRGB_BLOCK,
             FORMAT_BC2_UNORM_BLOCK,
             FORMAT_BC2_SRGB_BLOCK,
             FORMAT_BC3_UNORM_BLOCK,
             FORMAT_BC3_SRGB_BLOCK,
             FORMAT_BC4_UNORM_BLOCK,
             FORMAT_BC4_SNORM_BLOCK,
             FORMAT_BC5_UNORM_BLOCK,
             FORMAT_BC5_SNORM_BLOCK,
             FORMAT_BC6H_UFLOAT_BLOCK,
             FORMAT_BC6H_SFLOAT_BLOCK,
             FORMAT_BC7_UNORM_BLOCK,
             FORMAT_BC7_SRGB_BLOCK,
             FORMAT_ETC2_R8G8B8_UNORM_BLOCK,
             FORMAT_ETC2_R8G8B8_SRGB_BLOCK,
             FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK,
             FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK,
             FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK,
             FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK,
             FORMAT_EAC_R11_UNORM_BLOCK,
             FORMAT_EAC_R11_SNORM_BLOCK,
             FORMAT_EAC_R11G11_UNORM_BLOCK,
             FORMAT_EAC_R11G11_SNORM_BLOCK,
             FORMAT_ASTC_4x4_UNORM_BLOCK,
             FORMAT_ASTC_4x4_SRGB_BLOCK,
             FORMAT_ASTC_5x4_UNORM_BLOCK,
             FORMAT_ASTC_5x4_SRGB_BLOCK,
             FORMAT_ASTC_5x5_UNORM_BLOCK,
             FORMAT_ASTC_5x5_SRGB_BLOCK,
             FORMAT_ASTC_6x5_UNORM_BLOCK,
             FORMAT_ASTC_6x5_SRGB_BLOCK,
             FORMAT_ASTC_6x6_UNORM_BLOCK,
             FORMAT_ASTC_6x6_SRGB_BLOCK,
             FORMAT_ASTC_8x5_UNORM_BLOCK,
             FORMAT_ASTC_8x5_SRGB_BLOCK,
             FORMAT_ASTC_8x6_UNORM_BLOCK,
             FORMAT_ASTC_8x6_SRGB_BLOCK,
             FORMAT_ASTC_8x8_UNORM_BLOCK,
             FORMAT_ASTC_8x8_SRGB_BLOCK,
             FORMAT_ASTC_10x5_UNORM_BLOCK,
             FORMAT_ASTC_10x5_SRGB_BLOCK,
             FORMAT_ASTC_10x6_UNORM_BLOCK,
             FORMAT_ASTC_10x6_SRGB_BLOCK,
             FORMAT_ASTC_10x8_UNORM_BLOCK,
             FORMAT_ASTC_10x8_SRGB_BLOCK,
             FORMAT_ASTC_10x10_UNORM_BLOCK,
             FORMAT_ASTC_10x10_SRGB_BLOCK,
             FORMAT_ASTC_12x10_UNORM_BLOCK,
             FORMAT_ASTC_12x10_SRGB_BLOCK,
             FORMAT_ASTC_12x12_UNORM_BLOCK,
             FORMAT_ASTC_12x12_SRGB_BLOCK,
             FORMAT_A4B4G4R4_UNORM_PACK16_EXT,
             FORMAT_A4R4G4B4_UNORM_PACK16_EXT,
             FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT,
             FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT,
             FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG,
             FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG,
             FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG,
             FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG,
             FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG,
             FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG,
             FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG,
             FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG,
             FORMAT_G16_B16_R16_3PLANE_444_UNORM,
             FORMAT_G16_B16R16_2PLANE_422_UNORM,
             FORMAT_G16_B16_R16_3PLANE_422_UNORM,
             FORMAT_G16_B16R16_2PLANE_420_UNORM,
             FORMAT_G16_B16_R16_3PLANE_420_UNORM,
             FORMAT_B16G16R16G16_422_UNORM,
             FORMAT_G16B16G16R16_422_UNORM,
             FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16,
             FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16,
             FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16,
             FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16,
             FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16,
             FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16,
             FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16,
             FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16,
             FORMAT_R12X4G12X4_UNORM_2PACK16,
             FORMAT_R12X4_UNORM_PACK16,
             FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16,
             FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16,
             FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16,
             FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16,
             FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16,
             FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16,
             FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16,
             FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16,
             FORMAT_R10X6G10X6_UNORM_2PACK16,
             FORMAT_R10X6_UNORM_PACK16,
             FORMAT_G8_B8_R8_3PLANE_444_UNORM,
             FORMAT_G8_B8R8_2PLANE_422_UNORM,
             FORMAT_G8_B8_R8_3PLANE_422_UNORM,
             FORMAT_G8_B8R8_2PLANE_420_UNORM,
             FORMAT_G8_B8_R8_3PLANE_420_UNORM,
             FORMAT_B8G8R8G8_422_UNORM,
             FORMAT_G8B8G8R8_422_UNORM :: Format #-}

conNameFormat :: String
conNameFormat = "Format"

enumPrefixFormat :: String
enumPrefixFormat = "FORMAT_"

showTableFormat :: [(Format, String)]
showTableFormat =
  [ (FORMAT_UNDEFINED                         , "UNDEFINED")
  , (FORMAT_R4G4_UNORM_PACK8                  , "R4G4_UNORM_PACK8")
  , (FORMAT_R4G4B4A4_UNORM_PACK16             , "R4G4B4A4_UNORM_PACK16")
  , (FORMAT_B4G4R4A4_UNORM_PACK16             , "B4G4R4A4_UNORM_PACK16")
  , (FORMAT_R5G6B5_UNORM_PACK16               , "R5G6B5_UNORM_PACK16")
  , (FORMAT_B5G6R5_UNORM_PACK16               , "B5G6R5_UNORM_PACK16")
  , (FORMAT_R5G5B5A1_UNORM_PACK16             , "R5G5B5A1_UNORM_PACK16")
  , (FORMAT_B5G5R5A1_UNORM_PACK16             , "B5G5R5A1_UNORM_PACK16")
  , (FORMAT_A1R5G5B5_UNORM_PACK16             , "A1R5G5B5_UNORM_PACK16")
  , (FORMAT_R8_UNORM                          , "R8_UNORM")
  , (FORMAT_R8_SNORM                          , "R8_SNORM")
  , (FORMAT_R8_USCALED                        , "R8_USCALED")
  , (FORMAT_R8_SSCALED                        , "R8_SSCALED")
  , (FORMAT_R8_UINT                           , "R8_UINT")
  , (FORMAT_R8_SINT                           , "R8_SINT")
  , (FORMAT_R8_SRGB                           , "R8_SRGB")
  , (FORMAT_R8G8_UNORM                        , "R8G8_UNORM")
  , (FORMAT_R8G8_SNORM                        , "R8G8_SNORM")
  , (FORMAT_R8G8_USCALED                      , "R8G8_USCALED")
  , (FORMAT_R8G8_SSCALED                      , "R8G8_SSCALED")
  , (FORMAT_R8G8_UINT                         , "R8G8_UINT")
  , (FORMAT_R8G8_SINT                         , "R8G8_SINT")
  , (FORMAT_R8G8_SRGB                         , "R8G8_SRGB")
  , (FORMAT_R8G8B8_UNORM                      , "R8G8B8_UNORM")
  , (FORMAT_R8G8B8_SNORM                      , "R8G8B8_SNORM")
  , (FORMAT_R8G8B8_USCALED                    , "R8G8B8_USCALED")
  , (FORMAT_R8G8B8_SSCALED                    , "R8G8B8_SSCALED")
  , (FORMAT_R8G8B8_UINT                       , "R8G8B8_UINT")
  , (FORMAT_R8G8B8_SINT                       , "R8G8B8_SINT")
  , (FORMAT_R8G8B8_SRGB                       , "R8G8B8_SRGB")
  , (FORMAT_B8G8R8_UNORM                      , "B8G8R8_UNORM")
  , (FORMAT_B8G8R8_SNORM                      , "B8G8R8_SNORM")
  , (FORMAT_B8G8R8_USCALED                    , "B8G8R8_USCALED")
  , (FORMAT_B8G8R8_SSCALED                    , "B8G8R8_SSCALED")
  , (FORMAT_B8G8R8_UINT                       , "B8G8R8_UINT")
  , (FORMAT_B8G8R8_SINT                       , "B8G8R8_SINT")
  , (FORMAT_B8G8R8_SRGB                       , "B8G8R8_SRGB")
  , (FORMAT_R8G8B8A8_UNORM                    , "R8G8B8A8_UNORM")
  , (FORMAT_R8G8B8A8_SNORM                    , "R8G8B8A8_SNORM")
  , (FORMAT_R8G8B8A8_USCALED                  , "R8G8B8A8_USCALED")
  , (FORMAT_R8G8B8A8_SSCALED                  , "R8G8B8A8_SSCALED")
  , (FORMAT_R8G8B8A8_UINT                     , "R8G8B8A8_UINT")
  , (FORMAT_R8G8B8A8_SINT                     , "R8G8B8A8_SINT")
  , (FORMAT_R8G8B8A8_SRGB                     , "R8G8B8A8_SRGB")
  , (FORMAT_B8G8R8A8_UNORM                    , "B8G8R8A8_UNORM")
  , (FORMAT_B8G8R8A8_SNORM                    , "B8G8R8A8_SNORM")
  , (FORMAT_B8G8R8A8_USCALED                  , "B8G8R8A8_USCALED")
  , (FORMAT_B8G8R8A8_SSCALED                  , "B8G8R8A8_SSCALED")
  , (FORMAT_B8G8R8A8_UINT                     , "B8G8R8A8_UINT")
  , (FORMAT_B8G8R8A8_SINT                     , "B8G8R8A8_SINT")
  , (FORMAT_B8G8R8A8_SRGB                     , "B8G8R8A8_SRGB")
  , (FORMAT_A8B8G8R8_UNORM_PACK32             , "A8B8G8R8_UNORM_PACK32")
  , (FORMAT_A8B8G8R8_SNORM_PACK32             , "A8B8G8R8_SNORM_PACK32")
  , (FORMAT_A8B8G8R8_USCALED_PACK32           , "A8B8G8R8_USCALED_PACK32")
  , (FORMAT_A8B8G8R8_SSCALED_PACK32           , "A8B8G8R8_SSCALED_PACK32")
  , (FORMAT_A8B8G8R8_UINT_PACK32              , "A8B8G8R8_UINT_PACK32")
  , (FORMAT_A8B8G8R8_SINT_PACK32              , "A8B8G8R8_SINT_PACK32")
  , (FORMAT_A8B8G8R8_SRGB_PACK32              , "A8B8G8R8_SRGB_PACK32")
  , (FORMAT_A2R10G10B10_UNORM_PACK32          , "A2R10G10B10_UNORM_PACK32")
  , (FORMAT_A2R10G10B10_SNORM_PACK32          , "A2R10G10B10_SNORM_PACK32")
  , (FORMAT_A2R10G10B10_USCALED_PACK32        , "A2R10G10B10_USCALED_PACK32")
  , (FORMAT_A2R10G10B10_SSCALED_PACK32        , "A2R10G10B10_SSCALED_PACK32")
  , (FORMAT_A2R10G10B10_UINT_PACK32           , "A2R10G10B10_UINT_PACK32")
  , (FORMAT_A2R10G10B10_SINT_PACK32           , "A2R10G10B10_SINT_PACK32")
  , (FORMAT_A2B10G10R10_UNORM_PACK32          , "A2B10G10R10_UNORM_PACK32")
  , (FORMAT_A2B10G10R10_SNORM_PACK32          , "A2B10G10R10_SNORM_PACK32")
  , (FORMAT_A2B10G10R10_USCALED_PACK32        , "A2B10G10R10_USCALED_PACK32")
  , (FORMAT_A2B10G10R10_SSCALED_PACK32        , "A2B10G10R10_SSCALED_PACK32")
  , (FORMAT_A2B10G10R10_UINT_PACK32           , "A2B10G10R10_UINT_PACK32")
  , (FORMAT_A2B10G10R10_SINT_PACK32           , "A2B10G10R10_SINT_PACK32")
  , (FORMAT_R16_UNORM                         , "R16_UNORM")
  , (FORMAT_R16_SNORM                         , "R16_SNORM")
  , (FORMAT_R16_USCALED                       , "R16_USCALED")
  , (FORMAT_R16_SSCALED                       , "R16_SSCALED")
  , (FORMAT_R16_UINT                          , "R16_UINT")
  , (FORMAT_R16_SINT                          , "R16_SINT")
  , (FORMAT_R16_SFLOAT                        , "R16_SFLOAT")
  , (FORMAT_R16G16_UNORM                      , "R16G16_UNORM")
  , (FORMAT_R16G16_SNORM                      , "R16G16_SNORM")
  , (FORMAT_R16G16_USCALED                    , "R16G16_USCALED")
  , (FORMAT_R16G16_SSCALED                    , "R16G16_SSCALED")
  , (FORMAT_R16G16_UINT                       , "R16G16_UINT")
  , (FORMAT_R16G16_SINT                       , "R16G16_SINT")
  , (FORMAT_R16G16_SFLOAT                     , "R16G16_SFLOAT")
  , (FORMAT_R16G16B16_UNORM                   , "R16G16B16_UNORM")
  , (FORMAT_R16G16B16_SNORM                   , "R16G16B16_SNORM")
  , (FORMAT_R16G16B16_USCALED                 , "R16G16B16_USCALED")
  , (FORMAT_R16G16B16_SSCALED                 , "R16G16B16_SSCALED")
  , (FORMAT_R16G16B16_UINT                    , "R16G16B16_UINT")
  , (FORMAT_R16G16B16_SINT                    , "R16G16B16_SINT")
  , (FORMAT_R16G16B16_SFLOAT                  , "R16G16B16_SFLOAT")
  , (FORMAT_R16G16B16A16_UNORM                , "R16G16B16A16_UNORM")
  , (FORMAT_R16G16B16A16_SNORM                , "R16G16B16A16_SNORM")
  , (FORMAT_R16G16B16A16_USCALED              , "R16G16B16A16_USCALED")
  , (FORMAT_R16G16B16A16_SSCALED              , "R16G16B16A16_SSCALED")
  , (FORMAT_R16G16B16A16_UINT                 , "R16G16B16A16_UINT")
  , (FORMAT_R16G16B16A16_SINT                 , "R16G16B16A16_SINT")
  , (FORMAT_R16G16B16A16_SFLOAT               , "R16G16B16A16_SFLOAT")
  , (FORMAT_R32_UINT                          , "R32_UINT")
  , (FORMAT_R32_SINT                          , "R32_SINT")
  , (FORMAT_R32_SFLOAT                        , "R32_SFLOAT")
  , (FORMAT_R32G32_UINT                       , "R32G32_UINT")
  , (FORMAT_R32G32_SINT                       , "R32G32_SINT")
  , (FORMAT_R32G32_SFLOAT                     , "R32G32_SFLOAT")
  , (FORMAT_R32G32B32_UINT                    , "R32G32B32_UINT")
  , (FORMAT_R32G32B32_SINT                    , "R32G32B32_SINT")
  , (FORMAT_R32G32B32_SFLOAT                  , "R32G32B32_SFLOAT")
  , (FORMAT_R32G32B32A32_UINT                 , "R32G32B32A32_UINT")
  , (FORMAT_R32G32B32A32_SINT                 , "R32G32B32A32_SINT")
  , (FORMAT_R32G32B32A32_SFLOAT               , "R32G32B32A32_SFLOAT")
  , (FORMAT_R64_UINT                          , "R64_UINT")
  , (FORMAT_R64_SINT                          , "R64_SINT")
  , (FORMAT_R64_SFLOAT                        , "R64_SFLOAT")
  , (FORMAT_R64G64_UINT                       , "R64G64_UINT")
  , (FORMAT_R64G64_SINT                       , "R64G64_SINT")
  , (FORMAT_R64G64_SFLOAT                     , "R64G64_SFLOAT")
  , (FORMAT_R64G64B64_UINT                    , "R64G64B64_UINT")
  , (FORMAT_R64G64B64_SINT                    , "R64G64B64_SINT")
  , (FORMAT_R64G64B64_SFLOAT                  , "R64G64B64_SFLOAT")
  , (FORMAT_R64G64B64A64_UINT                 , "R64G64B64A64_UINT")
  , (FORMAT_R64G64B64A64_SINT                 , "R64G64B64A64_SINT")
  , (FORMAT_R64G64B64A64_SFLOAT               , "R64G64B64A64_SFLOAT")
  , (FORMAT_B10G11R11_UFLOAT_PACK32           , "B10G11R11_UFLOAT_PACK32")
  , (FORMAT_E5B9G9R9_UFLOAT_PACK32            , "E5B9G9R9_UFLOAT_PACK32")
  , (FORMAT_D16_UNORM                         , "D16_UNORM")
  , (FORMAT_X8_D24_UNORM_PACK32               , "X8_D24_UNORM_PACK32")
  , (FORMAT_D32_SFLOAT                        , "D32_SFLOAT")
  , (FORMAT_S8_UINT                           , "S8_UINT")
  , (FORMAT_D16_UNORM_S8_UINT                 , "D16_UNORM_S8_UINT")
  , (FORMAT_D24_UNORM_S8_UINT                 , "D24_UNORM_S8_UINT")
  , (FORMAT_D32_SFLOAT_S8_UINT                , "D32_SFLOAT_S8_UINT")
  , (FORMAT_BC1_RGB_UNORM_BLOCK               , "BC1_RGB_UNORM_BLOCK")
  , (FORMAT_BC1_RGB_SRGB_BLOCK                , "BC1_RGB_SRGB_BLOCK")
  , (FORMAT_BC1_RGBA_UNORM_BLOCK              , "BC1_RGBA_UNORM_BLOCK")
  , (FORMAT_BC1_RGBA_SRGB_BLOCK               , "BC1_RGBA_SRGB_BLOCK")
  , (FORMAT_BC2_UNORM_BLOCK                   , "BC2_UNORM_BLOCK")
  , (FORMAT_BC2_SRGB_BLOCK                    , "BC2_SRGB_BLOCK")
  , (FORMAT_BC3_UNORM_BLOCK                   , "BC3_UNORM_BLOCK")
  , (FORMAT_BC3_SRGB_BLOCK                    , "BC3_SRGB_BLOCK")
  , (FORMAT_BC4_UNORM_BLOCK                   , "BC4_UNORM_BLOCK")
  , (FORMAT_BC4_SNORM_BLOCK                   , "BC4_SNORM_BLOCK")
  , (FORMAT_BC5_UNORM_BLOCK                   , "BC5_UNORM_BLOCK")
  , (FORMAT_BC5_SNORM_BLOCK                   , "BC5_SNORM_BLOCK")
  , (FORMAT_BC6H_UFLOAT_BLOCK                 , "BC6H_UFLOAT_BLOCK")
  , (FORMAT_BC6H_SFLOAT_BLOCK                 , "BC6H_SFLOAT_BLOCK")
  , (FORMAT_BC7_UNORM_BLOCK                   , "BC7_UNORM_BLOCK")
  , (FORMAT_BC7_SRGB_BLOCK                    , "BC7_SRGB_BLOCK")
  , (FORMAT_ETC2_R8G8B8_UNORM_BLOCK           , "ETC2_R8G8B8_UNORM_BLOCK")
  , (FORMAT_ETC2_R8G8B8_SRGB_BLOCK            , "ETC2_R8G8B8_SRGB_BLOCK")
  , (FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK         , "ETC2_R8G8B8A1_UNORM_BLOCK")
  , (FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK          , "ETC2_R8G8B8A1_SRGB_BLOCK")
  , (FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK         , "ETC2_R8G8B8A8_UNORM_BLOCK")
  , (FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK          , "ETC2_R8G8B8A8_SRGB_BLOCK")
  , (FORMAT_EAC_R11_UNORM_BLOCK               , "EAC_R11_UNORM_BLOCK")
  , (FORMAT_EAC_R11_SNORM_BLOCK               , "EAC_R11_SNORM_BLOCK")
  , (FORMAT_EAC_R11G11_UNORM_BLOCK            , "EAC_R11G11_UNORM_BLOCK")
  , (FORMAT_EAC_R11G11_SNORM_BLOCK            , "EAC_R11G11_SNORM_BLOCK")
  , (FORMAT_ASTC_4x4_UNORM_BLOCK              , "ASTC_4x4_UNORM_BLOCK")
  , (FORMAT_ASTC_4x4_SRGB_BLOCK               , "ASTC_4x4_SRGB_BLOCK")
  , (FORMAT_ASTC_5x4_UNORM_BLOCK              , "ASTC_5x4_UNORM_BLOCK")
  , (FORMAT_ASTC_5x4_SRGB_BLOCK               , "ASTC_5x4_SRGB_BLOCK")
  , (FORMAT_ASTC_5x5_UNORM_BLOCK              , "ASTC_5x5_UNORM_BLOCK")
  , (FORMAT_ASTC_5x5_SRGB_BLOCK               , "ASTC_5x5_SRGB_BLOCK")
  , (FORMAT_ASTC_6x5_UNORM_BLOCK              , "ASTC_6x5_UNORM_BLOCK")
  , (FORMAT_ASTC_6x5_SRGB_BLOCK               , "ASTC_6x5_SRGB_BLOCK")
  , (FORMAT_ASTC_6x6_UNORM_BLOCK              , "ASTC_6x6_UNORM_BLOCK")
  , (FORMAT_ASTC_6x6_SRGB_BLOCK               , "ASTC_6x6_SRGB_BLOCK")
  , (FORMAT_ASTC_8x5_UNORM_BLOCK              , "ASTC_8x5_UNORM_BLOCK")
  , (FORMAT_ASTC_8x5_SRGB_BLOCK               , "ASTC_8x5_SRGB_BLOCK")
  , (FORMAT_ASTC_8x6_UNORM_BLOCK              , "ASTC_8x6_UNORM_BLOCK")
  , (FORMAT_ASTC_8x6_SRGB_BLOCK               , "ASTC_8x6_SRGB_BLOCK")
  , (FORMAT_ASTC_8x8_UNORM_BLOCK              , "ASTC_8x8_UNORM_BLOCK")
  , (FORMAT_ASTC_8x8_SRGB_BLOCK               , "ASTC_8x8_SRGB_BLOCK")
  , (FORMAT_ASTC_10x5_UNORM_BLOCK             , "ASTC_10x5_UNORM_BLOCK")
  , (FORMAT_ASTC_10x5_SRGB_BLOCK              , "ASTC_10x5_SRGB_BLOCK")
  , (FORMAT_ASTC_10x6_UNORM_BLOCK             , "ASTC_10x6_UNORM_BLOCK")
  , (FORMAT_ASTC_10x6_SRGB_BLOCK              , "ASTC_10x6_SRGB_BLOCK")
  , (FORMAT_ASTC_10x8_UNORM_BLOCK             , "ASTC_10x8_UNORM_BLOCK")
  , (FORMAT_ASTC_10x8_SRGB_BLOCK              , "ASTC_10x8_SRGB_BLOCK")
  , (FORMAT_ASTC_10x10_UNORM_BLOCK            , "ASTC_10x10_UNORM_BLOCK")
  , (FORMAT_ASTC_10x10_SRGB_BLOCK             , "ASTC_10x10_SRGB_BLOCK")
  , (FORMAT_ASTC_12x10_UNORM_BLOCK            , "ASTC_12x10_UNORM_BLOCK")
  , (FORMAT_ASTC_12x10_SRGB_BLOCK             , "ASTC_12x10_SRGB_BLOCK")
  , (FORMAT_ASTC_12x12_UNORM_BLOCK            , "ASTC_12x12_UNORM_BLOCK")
  , (FORMAT_ASTC_12x12_SRGB_BLOCK             , "ASTC_12x12_SRGB_BLOCK")
  , (FORMAT_A4B4G4R4_UNORM_PACK16_EXT         , "A4B4G4R4_UNORM_PACK16_EXT")
  , (FORMAT_A4R4G4B4_UNORM_PACK16_EXT         , "A4R4G4B4_UNORM_PACK16_EXT")
  , (FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT       , "ASTC_12x12_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT       , "ASTC_12x10_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT       , "ASTC_10x10_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT        , "ASTC_10x8_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT        , "ASTC_10x6_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT        , "ASTC_10x5_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT         , "ASTC_8x8_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT         , "ASTC_8x6_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT         , "ASTC_8x5_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT         , "ASTC_6x6_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT         , "ASTC_6x5_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT         , "ASTC_5x5_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT         , "ASTC_5x4_SFLOAT_BLOCK_EXT")
  , (FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT         , "ASTC_4x4_SFLOAT_BLOCK_EXT")
  , (FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG        , "PVRTC2_4BPP_SRGB_BLOCK_IMG")
  , (FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG        , "PVRTC2_2BPP_SRGB_BLOCK_IMG")
  , (FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG        , "PVRTC1_4BPP_SRGB_BLOCK_IMG")
  , (FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG        , "PVRTC1_2BPP_SRGB_BLOCK_IMG")
  , (FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG       , "PVRTC2_4BPP_UNORM_BLOCK_IMG")
  , (FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG       , "PVRTC2_2BPP_UNORM_BLOCK_IMG")
  , (FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG       , "PVRTC1_4BPP_UNORM_BLOCK_IMG")
  , (FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG       , "PVRTC1_2BPP_UNORM_BLOCK_IMG")
  , (FORMAT_G16_B16_R16_3PLANE_444_UNORM      , "G16_B16_R16_3PLANE_444_UNORM")
  , (FORMAT_G16_B16R16_2PLANE_422_UNORM       , "G16_B16R16_2PLANE_422_UNORM")
  , (FORMAT_G16_B16_R16_3PLANE_422_UNORM      , "G16_B16_R16_3PLANE_422_UNORM")
  , (FORMAT_G16_B16R16_2PLANE_420_UNORM       , "G16_B16R16_2PLANE_420_UNORM")
  , (FORMAT_G16_B16_R16_3PLANE_420_UNORM      , "G16_B16_R16_3PLANE_420_UNORM")
  , (FORMAT_B16G16R16G16_422_UNORM            , "B16G16R16G16_422_UNORM")
  , (FORMAT_G16B16G16R16_422_UNORM            , "G16B16G16R16_422_UNORM")
  , (FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16, "G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16")
  , (FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16, "G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16")
  , (FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16, "G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16")
  , (FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16, "G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16")
  , (FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16, "G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16")
  , (FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16, "B12X4G12X4R12X4G12X4_422_UNORM_4PACK16")
  , (FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16, "G12X4B12X4G12X4R12X4_422_UNORM_4PACK16")
  , (FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16, "R12X4G12X4B12X4A12X4_UNORM_4PACK16")
  , (FORMAT_R12X4G12X4_UNORM_2PACK16          , "R12X4G12X4_UNORM_2PACK16")
  , (FORMAT_R12X4_UNORM_PACK16                , "R12X4_UNORM_PACK16")
  , (FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16, "G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16")
  , (FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16, "G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16")
  , (FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16, "G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16")
  , (FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16, "G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16")
  , (FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16, "G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16")
  , (FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16, "B10X6G10X6R10X6G10X6_422_UNORM_4PACK16")
  , (FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16, "G10X6B10X6G10X6R10X6_422_UNORM_4PACK16")
  , (FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16, "R10X6G10X6B10X6A10X6_UNORM_4PACK16")
  , (FORMAT_R10X6G10X6_UNORM_2PACK16          , "R10X6G10X6_UNORM_2PACK16")
  , (FORMAT_R10X6_UNORM_PACK16                , "R10X6_UNORM_PACK16")
  , (FORMAT_G8_B8_R8_3PLANE_444_UNORM         , "G8_B8_R8_3PLANE_444_UNORM")
  , (FORMAT_G8_B8R8_2PLANE_422_UNORM          , "G8_B8R8_2PLANE_422_UNORM")
  , (FORMAT_G8_B8_R8_3PLANE_422_UNORM         , "G8_B8_R8_3PLANE_422_UNORM")
  , (FORMAT_G8_B8R8_2PLANE_420_UNORM          , "G8_B8R8_2PLANE_420_UNORM")
  , (FORMAT_G8_B8_R8_3PLANE_420_UNORM         , "G8_B8_R8_3PLANE_420_UNORM")
  , (FORMAT_B8G8R8G8_422_UNORM                , "B8G8R8G8_422_UNORM")
  , (FORMAT_G8B8G8R8_422_UNORM                , "G8B8G8R8_422_UNORM")
  ]

instance Show Format where
  showsPrec = enumShowsPrec enumPrefixFormat showTableFormat conNameFormat (\(Format x) -> x) (showsPrec 11)

instance Read Format where
  readPrec = enumReadPrec enumPrefixFormat showTableFormat conNameFormat Format

