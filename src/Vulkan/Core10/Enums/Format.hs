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
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkFormat"
newtype Format = Format Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkFormat" "VK_FORMAT_UNDEFINED"
pattern FORMAT_UNDEFINED                          = Format 0
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R4G4_UNORM_PACK8"
pattern FORMAT_R4G4_UNORM_PACK8                   = Format 1
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R4G4B4A4_UNORM_PACK16"
pattern FORMAT_R4G4B4A4_UNORM_PACK16              = Format 2
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B4G4R4A4_UNORM_PACK16"
pattern FORMAT_B4G4R4A4_UNORM_PACK16              = Format 3
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R5G6B5_UNORM_PACK16"
pattern FORMAT_R5G6B5_UNORM_PACK16                = Format 4
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B5G6R5_UNORM_PACK16"
pattern FORMAT_B5G6R5_UNORM_PACK16                = Format 5
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R5G5B5A1_UNORM_PACK16"
pattern FORMAT_R5G5B5A1_UNORM_PACK16              = Format 6
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B5G5R5A1_UNORM_PACK16"
pattern FORMAT_B5G5R5A1_UNORM_PACK16              = Format 7
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A1R5G5B5_UNORM_PACK16"
pattern FORMAT_A1R5G5B5_UNORM_PACK16              = Format 8
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_UNORM"
pattern FORMAT_R8_UNORM                           = Format 9
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SNORM"
pattern FORMAT_R8_SNORM                           = Format 10
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_USCALED"
pattern FORMAT_R8_USCALED                         = Format 11
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SSCALED"
pattern FORMAT_R8_SSCALED                         = Format 12
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_UINT"
pattern FORMAT_R8_UINT                            = Format 13
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SINT"
pattern FORMAT_R8_SINT                            = Format 14
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8_SRGB"
pattern FORMAT_R8_SRGB                            = Format 15
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_UNORM"
pattern FORMAT_R8G8_UNORM                         = Format 16
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SNORM"
pattern FORMAT_R8G8_SNORM                         = Format 17
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_USCALED"
pattern FORMAT_R8G8_USCALED                       = Format 18
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SSCALED"
pattern FORMAT_R8G8_SSCALED                       = Format 19
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_UINT"
pattern FORMAT_R8G8_UINT                          = Format 20
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SINT"
pattern FORMAT_R8G8_SINT                          = Format 21
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8_SRGB"
pattern FORMAT_R8G8_SRGB                          = Format 22
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_UNORM"
pattern FORMAT_R8G8B8_UNORM                       = Format 23
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SNORM"
pattern FORMAT_R8G8B8_SNORM                       = Format 24
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_USCALED"
pattern FORMAT_R8G8B8_USCALED                     = Format 25
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SSCALED"
pattern FORMAT_R8G8B8_SSCALED                     = Format 26
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_UINT"
pattern FORMAT_R8G8B8_UINT                        = Format 27
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SINT"
pattern FORMAT_R8G8B8_SINT                        = Format 28
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8_SRGB"
pattern FORMAT_R8G8B8_SRGB                        = Format 29
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_UNORM"
pattern FORMAT_B8G8R8_UNORM                       = Format 30
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SNORM"
pattern FORMAT_B8G8R8_SNORM                       = Format 31
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_USCALED"
pattern FORMAT_B8G8R8_USCALED                     = Format 32
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SSCALED"
pattern FORMAT_B8G8R8_SSCALED                     = Format 33
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_UINT"
pattern FORMAT_B8G8R8_UINT                        = Format 34
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SINT"
pattern FORMAT_B8G8R8_SINT                        = Format 35
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8_SRGB"
pattern FORMAT_B8G8R8_SRGB                        = Format 36
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_UNORM"
pattern FORMAT_R8G8B8A8_UNORM                     = Format 37
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SNORM"
pattern FORMAT_R8G8B8A8_SNORM                     = Format 38
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_USCALED"
pattern FORMAT_R8G8B8A8_USCALED                   = Format 39
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SSCALED"
pattern FORMAT_R8G8B8A8_SSCALED                   = Format 40
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_UINT"
pattern FORMAT_R8G8B8A8_UINT                      = Format 41
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SINT"
pattern FORMAT_R8G8B8A8_SINT                      = Format 42
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R8G8B8A8_SRGB"
pattern FORMAT_R8G8B8A8_SRGB                      = Format 43
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_UNORM"
pattern FORMAT_B8G8R8A8_UNORM                     = Format 44
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SNORM"
pattern FORMAT_B8G8R8A8_SNORM                     = Format 45
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_USCALED"
pattern FORMAT_B8G8R8A8_USCALED                   = Format 46
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SSCALED"
pattern FORMAT_B8G8R8A8_SSCALED                   = Format 47
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_UINT"
pattern FORMAT_B8G8R8A8_UINT                      = Format 48
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SINT"
pattern FORMAT_B8G8R8A8_SINT                      = Format 49
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8A8_SRGB"
pattern FORMAT_B8G8R8A8_SRGB                      = Format 50
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_UNORM_PACK32"
pattern FORMAT_A8B8G8R8_UNORM_PACK32              = Format 51
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SNORM_PACK32"
pattern FORMAT_A8B8G8R8_SNORM_PACK32              = Format 52
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_USCALED_PACK32"
pattern FORMAT_A8B8G8R8_USCALED_PACK32            = Format 53
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SSCALED_PACK32"
pattern FORMAT_A8B8G8R8_SSCALED_PACK32            = Format 54
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_UINT_PACK32"
pattern FORMAT_A8B8G8R8_UINT_PACK32               = Format 55
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SINT_PACK32"
pattern FORMAT_A8B8G8R8_SINT_PACK32               = Format 56
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A8B8G8R8_SRGB_PACK32"
pattern FORMAT_A8B8G8R8_SRGB_PACK32               = Format 57
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_UNORM_PACK32"
pattern FORMAT_A2R10G10B10_UNORM_PACK32           = Format 58
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_SNORM_PACK32"
pattern FORMAT_A2R10G10B10_SNORM_PACK32           = Format 59
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_USCALED_PACK32"
pattern FORMAT_A2R10G10B10_USCALED_PACK32         = Format 60
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_SSCALED_PACK32"
pattern FORMAT_A2R10G10B10_SSCALED_PACK32         = Format 61
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_UINT_PACK32"
pattern FORMAT_A2R10G10B10_UINT_PACK32            = Format 62
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2R10G10B10_SINT_PACK32"
pattern FORMAT_A2R10G10B10_SINT_PACK32            = Format 63
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_UNORM_PACK32"
pattern FORMAT_A2B10G10R10_UNORM_PACK32           = Format 64
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_SNORM_PACK32"
pattern FORMAT_A2B10G10R10_SNORM_PACK32           = Format 65
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_USCALED_PACK32"
pattern FORMAT_A2B10G10R10_USCALED_PACK32         = Format 66
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_SSCALED_PACK32"
pattern FORMAT_A2B10G10R10_SSCALED_PACK32         = Format 67
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_UINT_PACK32"
pattern FORMAT_A2B10G10R10_UINT_PACK32            = Format 68
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A2B10G10R10_SINT_PACK32"
pattern FORMAT_A2B10G10R10_SINT_PACK32            = Format 69
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_UNORM"
pattern FORMAT_R16_UNORM                          = Format 70
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SNORM"
pattern FORMAT_R16_SNORM                          = Format 71
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_USCALED"
pattern FORMAT_R16_USCALED                        = Format 72
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SSCALED"
pattern FORMAT_R16_SSCALED                        = Format 73
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_UINT"
pattern FORMAT_R16_UINT                           = Format 74
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SINT"
pattern FORMAT_R16_SINT                           = Format 75
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16_SFLOAT"
pattern FORMAT_R16_SFLOAT                         = Format 76
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_UNORM"
pattern FORMAT_R16G16_UNORM                       = Format 77
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SNORM"
pattern FORMAT_R16G16_SNORM                       = Format 78
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_USCALED"
pattern FORMAT_R16G16_USCALED                     = Format 79
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SSCALED"
pattern FORMAT_R16G16_SSCALED                     = Format 80
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_UINT"
pattern FORMAT_R16G16_UINT                        = Format 81
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SINT"
pattern FORMAT_R16G16_SINT                        = Format 82
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16_SFLOAT"
pattern FORMAT_R16G16_SFLOAT                      = Format 83
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_UNORM"
pattern FORMAT_R16G16B16_UNORM                    = Format 84
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SNORM"
pattern FORMAT_R16G16B16_SNORM                    = Format 85
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_USCALED"
pattern FORMAT_R16G16B16_USCALED                  = Format 86
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SSCALED"
pattern FORMAT_R16G16B16_SSCALED                  = Format 87
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_UINT"
pattern FORMAT_R16G16B16_UINT                     = Format 88
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SINT"
pattern FORMAT_R16G16B16_SINT                     = Format 89
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16_SFLOAT"
pattern FORMAT_R16G16B16_SFLOAT                   = Format 90
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_UNORM"
pattern FORMAT_R16G16B16A16_UNORM                 = Format 91
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SNORM"
pattern FORMAT_R16G16B16A16_SNORM                 = Format 92
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_USCALED"
pattern FORMAT_R16G16B16A16_USCALED               = Format 93
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SSCALED"
pattern FORMAT_R16G16B16A16_SSCALED               = Format 94
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_UINT"
pattern FORMAT_R16G16B16A16_UINT                  = Format 95
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SINT"
pattern FORMAT_R16G16B16A16_SINT                  = Format 96
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R16G16B16A16_SFLOAT"
pattern FORMAT_R16G16B16A16_SFLOAT                = Format 97
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32_UINT"
pattern FORMAT_R32_UINT                           = Format 98
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32_SINT"
pattern FORMAT_R32_SINT                           = Format 99
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32_SFLOAT"
pattern FORMAT_R32_SFLOAT                         = Format 100
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32_UINT"
pattern FORMAT_R32G32_UINT                        = Format 101
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32_SINT"
pattern FORMAT_R32G32_SINT                        = Format 102
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32_SFLOAT"
pattern FORMAT_R32G32_SFLOAT                      = Format 103
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32_UINT"
pattern FORMAT_R32G32B32_UINT                     = Format 104
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32_SINT"
pattern FORMAT_R32G32B32_SINT                     = Format 105
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32_SFLOAT"
pattern FORMAT_R32G32B32_SFLOAT                   = Format 106
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32A32_UINT"
pattern FORMAT_R32G32B32A32_UINT                  = Format 107
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32A32_SINT"
pattern FORMAT_R32G32B32A32_SINT                  = Format 108
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R32G32B32A32_SFLOAT"
pattern FORMAT_R32G32B32A32_SFLOAT                = Format 109
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64_UINT"
pattern FORMAT_R64_UINT                           = Format 110
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64_SINT"
pattern FORMAT_R64_SINT                           = Format 111
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64_SFLOAT"
pattern FORMAT_R64_SFLOAT                         = Format 112
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64_UINT"
pattern FORMAT_R64G64_UINT                        = Format 113
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64_SINT"
pattern FORMAT_R64G64_SINT                        = Format 114
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64_SFLOAT"
pattern FORMAT_R64G64_SFLOAT                      = Format 115
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64_UINT"
pattern FORMAT_R64G64B64_UINT                     = Format 116
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64_SINT"
pattern FORMAT_R64G64B64_SINT                     = Format 117
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64_SFLOAT"
pattern FORMAT_R64G64B64_SFLOAT                   = Format 118
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64A64_UINT"
pattern FORMAT_R64G64B64A64_UINT                  = Format 119
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64A64_SINT"
pattern FORMAT_R64G64B64A64_SINT                  = Format 120
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R64G64B64A64_SFLOAT"
pattern FORMAT_R64G64B64A64_SFLOAT                = Format 121
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B10G11R11_UFLOAT_PACK32"
pattern FORMAT_B10G11R11_UFLOAT_PACK32            = Format 122
-- No documentation found for Nested "VkFormat" "VK_FORMAT_E5B9G9R9_UFLOAT_PACK32"
pattern FORMAT_E5B9G9R9_UFLOAT_PACK32             = Format 123
-- No documentation found for Nested "VkFormat" "VK_FORMAT_D16_UNORM"
pattern FORMAT_D16_UNORM                          = Format 124
-- No documentation found for Nested "VkFormat" "VK_FORMAT_X8_D24_UNORM_PACK32"
pattern FORMAT_X8_D24_UNORM_PACK32                = Format 125
-- No documentation found for Nested "VkFormat" "VK_FORMAT_D32_SFLOAT"
pattern FORMAT_D32_SFLOAT                         = Format 126
-- No documentation found for Nested "VkFormat" "VK_FORMAT_S8_UINT"
pattern FORMAT_S8_UINT                            = Format 127
-- No documentation found for Nested "VkFormat" "VK_FORMAT_D16_UNORM_S8_UINT"
pattern FORMAT_D16_UNORM_S8_UINT                  = Format 128
-- No documentation found for Nested "VkFormat" "VK_FORMAT_D24_UNORM_S8_UINT"
pattern FORMAT_D24_UNORM_S8_UINT                  = Format 129
-- No documentation found for Nested "VkFormat" "VK_FORMAT_D32_SFLOAT_S8_UINT"
pattern FORMAT_D32_SFLOAT_S8_UINT                 = Format 130
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGB_UNORM_BLOCK"
pattern FORMAT_BC1_RGB_UNORM_BLOCK                = Format 131
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGB_SRGB_BLOCK"
pattern FORMAT_BC1_RGB_SRGB_BLOCK                 = Format 132
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGBA_UNORM_BLOCK"
pattern FORMAT_BC1_RGBA_UNORM_BLOCK               = Format 133
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC1_RGBA_SRGB_BLOCK"
pattern FORMAT_BC1_RGBA_SRGB_BLOCK                = Format 134
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC2_UNORM_BLOCK"
pattern FORMAT_BC2_UNORM_BLOCK                    = Format 135
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC2_SRGB_BLOCK"
pattern FORMAT_BC2_SRGB_BLOCK                     = Format 136
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC3_UNORM_BLOCK"
pattern FORMAT_BC3_UNORM_BLOCK                    = Format 137
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC3_SRGB_BLOCK"
pattern FORMAT_BC3_SRGB_BLOCK                     = Format 138
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC4_UNORM_BLOCK"
pattern FORMAT_BC4_UNORM_BLOCK                    = Format 139
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC4_SNORM_BLOCK"
pattern FORMAT_BC4_SNORM_BLOCK                    = Format 140
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC5_UNORM_BLOCK"
pattern FORMAT_BC5_UNORM_BLOCK                    = Format 141
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC5_SNORM_BLOCK"
pattern FORMAT_BC5_SNORM_BLOCK                    = Format 142
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC6H_UFLOAT_BLOCK"
pattern FORMAT_BC6H_UFLOAT_BLOCK                  = Format 143
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC6H_SFLOAT_BLOCK"
pattern FORMAT_BC6H_SFLOAT_BLOCK                  = Format 144
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC7_UNORM_BLOCK"
pattern FORMAT_BC7_UNORM_BLOCK                    = Format 145
-- No documentation found for Nested "VkFormat" "VK_FORMAT_BC7_SRGB_BLOCK"
pattern FORMAT_BC7_SRGB_BLOCK                     = Format 146
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK"
pattern FORMAT_ETC2_R8G8B8_UNORM_BLOCK            = Format 147
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK"
pattern FORMAT_ETC2_R8G8B8_SRGB_BLOCK             = Format 148
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK"
pattern FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK          = Format 149
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK"
pattern FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK           = Format 150
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK"
pattern FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK          = Format 151
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK"
pattern FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK           = Format 152
-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11_UNORM_BLOCK"
pattern FORMAT_EAC_R11_UNORM_BLOCK                = Format 153
-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11_SNORM_BLOCK"
pattern FORMAT_EAC_R11_SNORM_BLOCK                = Format 154
-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11G11_UNORM_BLOCK"
pattern FORMAT_EAC_R11G11_UNORM_BLOCK             = Format 155
-- No documentation found for Nested "VkFormat" "VK_FORMAT_EAC_R11G11_SNORM_BLOCK"
pattern FORMAT_EAC_R11G11_SNORM_BLOCK             = Format 156
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_4x4_UNORM_BLOCK"
pattern FORMAT_ASTC_4x4_UNORM_BLOCK               = Format 157
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_4x4_SRGB_BLOCK"
pattern FORMAT_ASTC_4x4_SRGB_BLOCK                = Format 158
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x4_UNORM_BLOCK"
pattern FORMAT_ASTC_5x4_UNORM_BLOCK               = Format 159
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x4_SRGB_BLOCK"
pattern FORMAT_ASTC_5x4_SRGB_BLOCK                = Format 160
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x5_UNORM_BLOCK"
pattern FORMAT_ASTC_5x5_UNORM_BLOCK               = Format 161
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x5_SRGB_BLOCK"
pattern FORMAT_ASTC_5x5_SRGB_BLOCK                = Format 162
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x5_UNORM_BLOCK"
pattern FORMAT_ASTC_6x5_UNORM_BLOCK               = Format 163
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x5_SRGB_BLOCK"
pattern FORMAT_ASTC_6x5_SRGB_BLOCK                = Format 164
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x6_UNORM_BLOCK"
pattern FORMAT_ASTC_6x6_UNORM_BLOCK               = Format 165
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x6_SRGB_BLOCK"
pattern FORMAT_ASTC_6x6_SRGB_BLOCK                = Format 166
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x5_UNORM_BLOCK"
pattern FORMAT_ASTC_8x5_UNORM_BLOCK               = Format 167
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x5_SRGB_BLOCK"
pattern FORMAT_ASTC_8x5_SRGB_BLOCK                = Format 168
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x6_UNORM_BLOCK"
pattern FORMAT_ASTC_8x6_UNORM_BLOCK               = Format 169
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x6_SRGB_BLOCK"
pattern FORMAT_ASTC_8x6_SRGB_BLOCK                = Format 170
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x8_UNORM_BLOCK"
pattern FORMAT_ASTC_8x8_UNORM_BLOCK               = Format 171
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x8_SRGB_BLOCK"
pattern FORMAT_ASTC_8x8_SRGB_BLOCK                = Format 172
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x5_UNORM_BLOCK"
pattern FORMAT_ASTC_10x5_UNORM_BLOCK              = Format 173
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x5_SRGB_BLOCK"
pattern FORMAT_ASTC_10x5_SRGB_BLOCK               = Format 174
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x6_UNORM_BLOCK"
pattern FORMAT_ASTC_10x6_UNORM_BLOCK              = Format 175
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x6_SRGB_BLOCK"
pattern FORMAT_ASTC_10x6_SRGB_BLOCK               = Format 176
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x8_UNORM_BLOCK"
pattern FORMAT_ASTC_10x8_UNORM_BLOCK              = Format 177
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x8_SRGB_BLOCK"
pattern FORMAT_ASTC_10x8_SRGB_BLOCK               = Format 178
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x10_UNORM_BLOCK"
pattern FORMAT_ASTC_10x10_UNORM_BLOCK             = Format 179
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x10_SRGB_BLOCK"
pattern FORMAT_ASTC_10x10_SRGB_BLOCK              = Format 180
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x10_UNORM_BLOCK"
pattern FORMAT_ASTC_12x10_UNORM_BLOCK             = Format 181
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x10_SRGB_BLOCK"
pattern FORMAT_ASTC_12x10_SRGB_BLOCK              = Format 182
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x12_UNORM_BLOCK"
pattern FORMAT_ASTC_12x12_UNORM_BLOCK             = Format 183
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x12_SRGB_BLOCK"
pattern FORMAT_ASTC_12x12_SRGB_BLOCK              = Format 184
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A4B4G4R4_UNORM_PACK16_EXT"
pattern FORMAT_A4B4G4R4_UNORM_PACK16_EXT          = Format 1000340001
-- No documentation found for Nested "VkFormat" "VK_FORMAT_A4R4G4B4_UNORM_PACK16_EXT"
pattern FORMAT_A4R4G4B4_UNORM_PACK16_EXT          = Format 1000340000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_12x12_SFLOAT_BLOCK_EXT        = Format 1000066013
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_12x10_SFLOAT_BLOCK_EXT        = Format 1000066012
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x10_SFLOAT_BLOCK_EXT        = Format 1000066011
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x8_SFLOAT_BLOCK_EXT         = Format 1000066010
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x6_SFLOAT_BLOCK_EXT         = Format 1000066009
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_10x5_SFLOAT_BLOCK_EXT         = Format 1000066008
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_8x8_SFLOAT_BLOCK_EXT          = Format 1000066007
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_8x6_SFLOAT_BLOCK_EXT          = Format 1000066006
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_8x5_SFLOAT_BLOCK_EXT          = Format 1000066005
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_6x6_SFLOAT_BLOCK_EXT          = Format 1000066004
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_6x5_SFLOAT_BLOCK_EXT          = Format 1000066003
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_5x5_SFLOAT_BLOCK_EXT          = Format 1000066002
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_5x4_SFLOAT_BLOCK_EXT          = Format 1000066001
-- No documentation found for Nested "VkFormat" "VK_FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT"
pattern FORMAT_ASTC_4x4_SFLOAT_BLOCK_EXT          = Format 1000066000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG"
pattern FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG         = Format 1000054007
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG"
pattern FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG         = Format 1000054006
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG"
pattern FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG         = Format 1000054005
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG"
pattern FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG         = Format 1000054004
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG"
pattern FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG        = Format 1000054003
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG"
pattern FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG        = Format 1000054002
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG"
pattern FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG        = Format 1000054001
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG"
pattern FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG        = Format 1000054000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_444_UNORM"
pattern FORMAT_G16_B16_R16_3PLANE_444_UNORM       = Format 1000156033
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_422_UNORM"
pattern FORMAT_G16_B16R16_2PLANE_422_UNORM        = Format 1000156032
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_422_UNORM"
pattern FORMAT_G16_B16_R16_3PLANE_422_UNORM       = Format 1000156031
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16R16_2PLANE_420_UNORM"
pattern FORMAT_G16_B16R16_2PLANE_420_UNORM        = Format 1000156030
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16_B16_R16_3PLANE_420_UNORM"
pattern FORMAT_G16_B16_R16_3PLANE_420_UNORM       = Format 1000156029
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B16G16R16G16_422_UNORM"
pattern FORMAT_B16G16R16G16_422_UNORM             = Format 1000156028
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G16B16G16R16_422_UNORM"
pattern FORMAT_G16B16G16R16_422_UNORM             = Format 1000156027
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16"
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_444_UNORM_3PACK16 = Format 1000156026
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16"
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_422_UNORM_3PACK16 = Format 1000156025
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16"
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_422_UNORM_3PACK16 = Format 1000156024
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16"
pattern FORMAT_G12X4_B12X4R12X4_2PLANE_420_UNORM_3PACK16 = Format 1000156023
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16"
pattern FORMAT_G12X4_B12X4_R12X4_3PLANE_420_UNORM_3PACK16 = Format 1000156022
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16"
pattern FORMAT_B12X4G12X4R12X4G12X4_422_UNORM_4PACK16 = Format 1000156021
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16"
pattern FORMAT_G12X4B12X4G12X4R12X4_422_UNORM_4PACK16 = Format 1000156020
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16"
pattern FORMAT_R12X4G12X4B12X4A12X4_UNORM_4PACK16 = Format 1000156019
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4G12X4_UNORM_2PACK16"
pattern FORMAT_R12X4G12X4_UNORM_2PACK16           = Format 1000156018
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R12X4_UNORM_PACK16"
pattern FORMAT_R12X4_UNORM_PACK16                 = Format 1000156017
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16"
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_444_UNORM_3PACK16 = Format 1000156016
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16"
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_422_UNORM_3PACK16 = Format 1000156015
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16"
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_422_UNORM_3PACK16 = Format 1000156014
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16"
pattern FORMAT_G10X6_B10X6R10X6_2PLANE_420_UNORM_3PACK16 = Format 1000156013
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16"
pattern FORMAT_G10X6_B10X6_R10X6_3PLANE_420_UNORM_3PACK16 = Format 1000156012
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16"
pattern FORMAT_B10X6G10X6R10X6G10X6_422_UNORM_4PACK16 = Format 1000156011
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16"
pattern FORMAT_G10X6B10X6G10X6R10X6_422_UNORM_4PACK16 = Format 1000156010
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16"
pattern FORMAT_R10X6G10X6B10X6A10X6_UNORM_4PACK16 = Format 1000156009
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6G10X6_UNORM_2PACK16"
pattern FORMAT_R10X6G10X6_UNORM_2PACK16           = Format 1000156008
-- No documentation found for Nested "VkFormat" "VK_FORMAT_R10X6_UNORM_PACK16"
pattern FORMAT_R10X6_UNORM_PACK16                 = Format 1000156007
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_444_UNORM"
pattern FORMAT_G8_B8_R8_3PLANE_444_UNORM          = Format 1000156006
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_422_UNORM"
pattern FORMAT_G8_B8R8_2PLANE_422_UNORM           = Format 1000156005
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_422_UNORM"
pattern FORMAT_G8_B8_R8_3PLANE_422_UNORM          = Format 1000156004
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8R8_2PLANE_420_UNORM"
pattern FORMAT_G8_B8R8_2PLANE_420_UNORM           = Format 1000156003
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8_B8_R8_3PLANE_420_UNORM"
pattern FORMAT_G8_B8_R8_3PLANE_420_UNORM          = Format 1000156002
-- No documentation found for Nested "VkFormat" "VK_FORMAT_B8G8R8G8_422_UNORM"
pattern FORMAT_B8G8R8G8_422_UNORM                 = Format 1000156001
-- No documentation found for Nested "VkFormat" "VK_FORMAT_G8B8G8R8_422_UNORM"
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

