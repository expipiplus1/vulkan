{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Graphics.Vulkan.Core where

import Text.Read.Lex( Lexeme(Ident)
                    )
import GHC.Read( expectP
               , choose
               )
import Data.Word( Word64
                , Word32
                )
import Foreign.Ptr( plusPtr
                  )
import Data.Int( Int32
               )
import Foreign.Storable( Storable(..)
                       )
import Text.Read( Read(..)
                , parens
                )
import Text.ParserCombinators.ReadPrec( prec
                                      , (+++)
                                      , step
                                      )
import Foreign.C.Types( CFloat
                      )

newtype VkDeviceSize = VkDeviceSize Word64
  deriving (Eq, Storable)

-- ** VkFormat
-- | Vulkan format definitions
newtype VkFormat = VkFormat Int32
  deriving (Eq, Storable)

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
  showsPrec p (VkFormat x) = showParen (p >= 11) (showString "VkFormat " . showsPrec 11 x)

instance Read VkFormat where
  readPrec = parens ( choose [ ("VK_FORMAT_UNDEFINED", pure VK_FORMAT_UNDEFINED)
                             , ("VK_FORMAT_R4G4_UNORM_PACK8", pure VK_FORMAT_R4G4_UNORM_PACK8)
                             , ("VK_FORMAT_R4G4B4A4_UNORM_PACK16", pure VK_FORMAT_R4G4B4A4_UNORM_PACK16)
                             , ("VK_FORMAT_B4G4R4A4_UNORM_PACK16", pure VK_FORMAT_B4G4R4A4_UNORM_PACK16)
                             , ("VK_FORMAT_R5G6B5_UNORM_PACK16", pure VK_FORMAT_R5G6B5_UNORM_PACK16)
                             , ("VK_FORMAT_B5G6R5_UNORM_PACK16", pure VK_FORMAT_B5G6R5_UNORM_PACK16)
                             , ("VK_FORMAT_R5G5B5A1_UNORM_PACK16", pure VK_FORMAT_R5G5B5A1_UNORM_PACK16)
                             , ("VK_FORMAT_B5G5R5A1_UNORM_PACK16", pure VK_FORMAT_B5G5R5A1_UNORM_PACK16)
                             , ("VK_FORMAT_A1R5G5B5_UNORM_PACK16", pure VK_FORMAT_A1R5G5B5_UNORM_PACK16)
                             , ("VK_FORMAT_R8_UNORM", pure VK_FORMAT_R8_UNORM)
                             , ("VK_FORMAT_R8_SNORM", pure VK_FORMAT_R8_SNORM)
                             , ("VK_FORMAT_R8_USCALED", pure VK_FORMAT_R8_USCALED)
                             , ("VK_FORMAT_R8_SSCALED", pure VK_FORMAT_R8_SSCALED)
                             , ("VK_FORMAT_R8_UINT", pure VK_FORMAT_R8_UINT)
                             , ("VK_FORMAT_R8_SINT", pure VK_FORMAT_R8_SINT)
                             , ("VK_FORMAT_R8_SRGB", pure VK_FORMAT_R8_SRGB)
                             , ("VK_FORMAT_R8G8_UNORM", pure VK_FORMAT_R8G8_UNORM)
                             , ("VK_FORMAT_R8G8_SNORM", pure VK_FORMAT_R8G8_SNORM)
                             , ("VK_FORMAT_R8G8_USCALED", pure VK_FORMAT_R8G8_USCALED)
                             , ("VK_FORMAT_R8G8_SSCALED", pure VK_FORMAT_R8G8_SSCALED)
                             , ("VK_FORMAT_R8G8_UINT", pure VK_FORMAT_R8G8_UINT)
                             , ("VK_FORMAT_R8G8_SINT", pure VK_FORMAT_R8G8_SINT)
                             , ("VK_FORMAT_R8G8_SRGB", pure VK_FORMAT_R8G8_SRGB)
                             , ("VK_FORMAT_R8G8B8_UNORM", pure VK_FORMAT_R8G8B8_UNORM)
                             , ("VK_FORMAT_R8G8B8_SNORM", pure VK_FORMAT_R8G8B8_SNORM)
                             , ("VK_FORMAT_R8G8B8_USCALED", pure VK_FORMAT_R8G8B8_USCALED)
                             , ("VK_FORMAT_R8G8B8_SSCALED", pure VK_FORMAT_R8G8B8_SSCALED)
                             , ("VK_FORMAT_R8G8B8_UINT", pure VK_FORMAT_R8G8B8_UINT)
                             , ("VK_FORMAT_R8G8B8_SINT", pure VK_FORMAT_R8G8B8_SINT)
                             , ("VK_FORMAT_R8G8B8_SRGB", pure VK_FORMAT_R8G8B8_SRGB)
                             , ("VK_FORMAT_B8G8R8_UNORM", pure VK_FORMAT_B8G8R8_UNORM)
                             , ("VK_FORMAT_B8G8R8_SNORM", pure VK_FORMAT_B8G8R8_SNORM)
                             , ("VK_FORMAT_B8G8R8_USCALED", pure VK_FORMAT_B8G8R8_USCALED)
                             , ("VK_FORMAT_B8G8R8_SSCALED", pure VK_FORMAT_B8G8R8_SSCALED)
                             , ("VK_FORMAT_B8G8R8_UINT", pure VK_FORMAT_B8G8R8_UINT)
                             , ("VK_FORMAT_B8G8R8_SINT", pure VK_FORMAT_B8G8R8_SINT)
                             , ("VK_FORMAT_B8G8R8_SRGB", pure VK_FORMAT_B8G8R8_SRGB)
                             , ("VK_FORMAT_R8G8B8A8_UNORM", pure VK_FORMAT_R8G8B8A8_UNORM)
                             , ("VK_FORMAT_R8G8B8A8_SNORM", pure VK_FORMAT_R8G8B8A8_SNORM)
                             , ("VK_FORMAT_R8G8B8A8_USCALED", pure VK_FORMAT_R8G8B8A8_USCALED)
                             , ("VK_FORMAT_R8G8B8A8_SSCALED", pure VK_FORMAT_R8G8B8A8_SSCALED)
                             , ("VK_FORMAT_R8G8B8A8_UINT", pure VK_FORMAT_R8G8B8A8_UINT)
                             , ("VK_FORMAT_R8G8B8A8_SINT", pure VK_FORMAT_R8G8B8A8_SINT)
                             , ("VK_FORMAT_R8G8B8A8_SRGB", pure VK_FORMAT_R8G8B8A8_SRGB)
                             , ("VK_FORMAT_B8G8R8A8_UNORM", pure VK_FORMAT_B8G8R8A8_UNORM)
                             , ("VK_FORMAT_B8G8R8A8_SNORM", pure VK_FORMAT_B8G8R8A8_SNORM)
                             , ("VK_FORMAT_B8G8R8A8_USCALED", pure VK_FORMAT_B8G8R8A8_USCALED)
                             , ("VK_FORMAT_B8G8R8A8_SSCALED", pure VK_FORMAT_B8G8R8A8_SSCALED)
                             , ("VK_FORMAT_B8G8R8A8_UINT", pure VK_FORMAT_B8G8R8A8_UINT)
                             , ("VK_FORMAT_B8G8R8A8_SINT", pure VK_FORMAT_B8G8R8A8_SINT)
                             , ("VK_FORMAT_B8G8R8A8_SRGB", pure VK_FORMAT_B8G8R8A8_SRGB)
                             , ("VK_FORMAT_A8B8G8R8_UNORM_PACK32", pure VK_FORMAT_A8B8G8R8_UNORM_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SNORM_PACK32", pure VK_FORMAT_A8B8G8R8_SNORM_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_USCALED_PACK32", pure VK_FORMAT_A8B8G8R8_USCALED_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SSCALED_PACK32", pure VK_FORMAT_A8B8G8R8_SSCALED_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_UINT_PACK32", pure VK_FORMAT_A8B8G8R8_UINT_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SINT_PACK32", pure VK_FORMAT_A8B8G8R8_SINT_PACK32)
                             , ("VK_FORMAT_A8B8G8R8_SRGB_PACK32", pure VK_FORMAT_A8B8G8R8_SRGB_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_UNORM_PACK32", pure VK_FORMAT_A2R10G10B10_UNORM_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SNORM_PACK32", pure VK_FORMAT_A2R10G10B10_SNORM_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_USCALED_PACK32", pure VK_FORMAT_A2R10G10B10_USCALED_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SSCALED_PACK32", pure VK_FORMAT_A2R10G10B10_SSCALED_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_UINT_PACK32", pure VK_FORMAT_A2R10G10B10_UINT_PACK32)
                             , ("VK_FORMAT_A2R10G10B10_SINT_PACK32", pure VK_FORMAT_A2R10G10B10_SINT_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_UNORM_PACK32", pure VK_FORMAT_A2B10G10R10_UNORM_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SNORM_PACK32", pure VK_FORMAT_A2B10G10R10_SNORM_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_USCALED_PACK32", pure VK_FORMAT_A2B10G10R10_USCALED_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SSCALED_PACK32", pure VK_FORMAT_A2B10G10R10_SSCALED_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_UINT_PACK32", pure VK_FORMAT_A2B10G10R10_UINT_PACK32)
                             , ("VK_FORMAT_A2B10G10R10_SINT_PACK32", pure VK_FORMAT_A2B10G10R10_SINT_PACK32)
                             , ("VK_FORMAT_R16_UNORM", pure VK_FORMAT_R16_UNORM)
                             , ("VK_FORMAT_R16_SNORM", pure VK_FORMAT_R16_SNORM)
                             , ("VK_FORMAT_R16_USCALED", pure VK_FORMAT_R16_USCALED)
                             , ("VK_FORMAT_R16_SSCALED", pure VK_FORMAT_R16_SSCALED)
                             , ("VK_FORMAT_R16_UINT", pure VK_FORMAT_R16_UINT)
                             , ("VK_FORMAT_R16_SINT", pure VK_FORMAT_R16_SINT)
                             , ("VK_FORMAT_R16_SFLOAT", pure VK_FORMAT_R16_SFLOAT)
                             , ("VK_FORMAT_R16G16_UNORM", pure VK_FORMAT_R16G16_UNORM)
                             , ("VK_FORMAT_R16G16_SNORM", pure VK_FORMAT_R16G16_SNORM)
                             , ("VK_FORMAT_R16G16_USCALED", pure VK_FORMAT_R16G16_USCALED)
                             , ("VK_FORMAT_R16G16_SSCALED", pure VK_FORMAT_R16G16_SSCALED)
                             , ("VK_FORMAT_R16G16_UINT", pure VK_FORMAT_R16G16_UINT)
                             , ("VK_FORMAT_R16G16_SINT", pure VK_FORMAT_R16G16_SINT)
                             , ("VK_FORMAT_R16G16_SFLOAT", pure VK_FORMAT_R16G16_SFLOAT)
                             , ("VK_FORMAT_R16G16B16_UNORM", pure VK_FORMAT_R16G16B16_UNORM)
                             , ("VK_FORMAT_R16G16B16_SNORM", pure VK_FORMAT_R16G16B16_SNORM)
                             , ("VK_FORMAT_R16G16B16_USCALED", pure VK_FORMAT_R16G16B16_USCALED)
                             , ("VK_FORMAT_R16G16B16_SSCALED", pure VK_FORMAT_R16G16B16_SSCALED)
                             , ("VK_FORMAT_R16G16B16_UINT", pure VK_FORMAT_R16G16B16_UINT)
                             , ("VK_FORMAT_R16G16B16_SINT", pure VK_FORMAT_R16G16B16_SINT)
                             , ("VK_FORMAT_R16G16B16_SFLOAT", pure VK_FORMAT_R16G16B16_SFLOAT)
                             , ("VK_FORMAT_R16G16B16A16_UNORM", pure VK_FORMAT_R16G16B16A16_UNORM)
                             , ("VK_FORMAT_R16G16B16A16_SNORM", pure VK_FORMAT_R16G16B16A16_SNORM)
                             , ("VK_FORMAT_R16G16B16A16_USCALED", pure VK_FORMAT_R16G16B16A16_USCALED)
                             , ("VK_FORMAT_R16G16B16A16_SSCALED", pure VK_FORMAT_R16G16B16A16_SSCALED)
                             , ("VK_FORMAT_R16G16B16A16_UINT", pure VK_FORMAT_R16G16B16A16_UINT)
                             , ("VK_FORMAT_R16G16B16A16_SINT", pure VK_FORMAT_R16G16B16A16_SINT)
                             , ("VK_FORMAT_R16G16B16A16_SFLOAT", pure VK_FORMAT_R16G16B16A16_SFLOAT)
                             , ("VK_FORMAT_R32_UINT", pure VK_FORMAT_R32_UINT)
                             , ("VK_FORMAT_R32_SINT", pure VK_FORMAT_R32_SINT)
                             , ("VK_FORMAT_R32_SFLOAT", pure VK_FORMAT_R32_SFLOAT)
                             , ("VK_FORMAT_R32G32_UINT", pure VK_FORMAT_R32G32_UINT)
                             , ("VK_FORMAT_R32G32_SINT", pure VK_FORMAT_R32G32_SINT)
                             , ("VK_FORMAT_R32G32_SFLOAT", pure VK_FORMAT_R32G32_SFLOAT)
                             , ("VK_FORMAT_R32G32B32_UINT", pure VK_FORMAT_R32G32B32_UINT)
                             , ("VK_FORMAT_R32G32B32_SINT", pure VK_FORMAT_R32G32B32_SINT)
                             , ("VK_FORMAT_R32G32B32_SFLOAT", pure VK_FORMAT_R32G32B32_SFLOAT)
                             , ("VK_FORMAT_R32G32B32A32_UINT", pure VK_FORMAT_R32G32B32A32_UINT)
                             , ("VK_FORMAT_R32G32B32A32_SINT", pure VK_FORMAT_R32G32B32A32_SINT)
                             , ("VK_FORMAT_R32G32B32A32_SFLOAT", pure VK_FORMAT_R32G32B32A32_SFLOAT)
                             , ("VK_FORMAT_R64_UINT", pure VK_FORMAT_R64_UINT)
                             , ("VK_FORMAT_R64_SINT", pure VK_FORMAT_R64_SINT)
                             , ("VK_FORMAT_R64_SFLOAT", pure VK_FORMAT_R64_SFLOAT)
                             , ("VK_FORMAT_R64G64_UINT", pure VK_FORMAT_R64G64_UINT)
                             , ("VK_FORMAT_R64G64_SINT", pure VK_FORMAT_R64G64_SINT)
                             , ("VK_FORMAT_R64G64_SFLOAT", pure VK_FORMAT_R64G64_SFLOAT)
                             , ("VK_FORMAT_R64G64B64_UINT", pure VK_FORMAT_R64G64B64_UINT)
                             , ("VK_FORMAT_R64G64B64_SINT", pure VK_FORMAT_R64G64B64_SINT)
                             , ("VK_FORMAT_R64G64B64_SFLOAT", pure VK_FORMAT_R64G64B64_SFLOAT)
                             , ("VK_FORMAT_R64G64B64A64_UINT", pure VK_FORMAT_R64G64B64A64_UINT)
                             , ("VK_FORMAT_R64G64B64A64_SINT", pure VK_FORMAT_R64G64B64A64_SINT)
                             , ("VK_FORMAT_R64G64B64A64_SFLOAT", pure VK_FORMAT_R64G64B64A64_SFLOAT)
                             , ("VK_FORMAT_B10G11R11_UFLOAT_PACK32", pure VK_FORMAT_B10G11R11_UFLOAT_PACK32)
                             , ("VK_FORMAT_E5B9G9R9_UFLOAT_PACK32", pure VK_FORMAT_E5B9G9R9_UFLOAT_PACK32)
                             , ("VK_FORMAT_D16_UNORM", pure VK_FORMAT_D16_UNORM)
                             , ("VK_FORMAT_X8_D24_UNORM_PACK32", pure VK_FORMAT_X8_D24_UNORM_PACK32)
                             , ("VK_FORMAT_D32_SFLOAT", pure VK_FORMAT_D32_SFLOAT)
                             , ("VK_FORMAT_S8_UINT", pure VK_FORMAT_S8_UINT)
                             , ("VK_FORMAT_D16_UNORM_S8_UINT", pure VK_FORMAT_D16_UNORM_S8_UINT)
                             , ("VK_FORMAT_D24_UNORM_S8_UINT", pure VK_FORMAT_D24_UNORM_S8_UINT)
                             , ("VK_FORMAT_D32_SFLOAT_S8_UINT", pure VK_FORMAT_D32_SFLOAT_S8_UINT)
                             , ("VK_FORMAT_BC1_RGB_UNORM_BLOCK", pure VK_FORMAT_BC1_RGB_UNORM_BLOCK)
                             , ("VK_FORMAT_BC1_RGB_SRGB_BLOCK", pure VK_FORMAT_BC1_RGB_SRGB_BLOCK)
                             , ("VK_FORMAT_BC1_RGBA_UNORM_BLOCK", pure VK_FORMAT_BC1_RGBA_UNORM_BLOCK)
                             , ("VK_FORMAT_BC1_RGBA_SRGB_BLOCK", pure VK_FORMAT_BC1_RGBA_SRGB_BLOCK)
                             , ("VK_FORMAT_BC2_UNORM_BLOCK", pure VK_FORMAT_BC2_UNORM_BLOCK)
                             , ("VK_FORMAT_BC2_SRGB_BLOCK", pure VK_FORMAT_BC2_SRGB_BLOCK)
                             , ("VK_FORMAT_BC3_UNORM_BLOCK", pure VK_FORMAT_BC3_UNORM_BLOCK)
                             , ("VK_FORMAT_BC3_SRGB_BLOCK", pure VK_FORMAT_BC3_SRGB_BLOCK)
                             , ("VK_FORMAT_BC4_UNORM_BLOCK", pure VK_FORMAT_BC4_UNORM_BLOCK)
                             , ("VK_FORMAT_BC4_SNORM_BLOCK", pure VK_FORMAT_BC4_SNORM_BLOCK)
                             , ("VK_FORMAT_BC5_UNORM_BLOCK", pure VK_FORMAT_BC5_UNORM_BLOCK)
                             , ("VK_FORMAT_BC5_SNORM_BLOCK", pure VK_FORMAT_BC5_SNORM_BLOCK)
                             , ("VK_FORMAT_BC6H_UFLOAT_BLOCK", pure VK_FORMAT_BC6H_UFLOAT_BLOCK)
                             , ("VK_FORMAT_BC6H_SFLOAT_BLOCK", pure VK_FORMAT_BC6H_SFLOAT_BLOCK)
                             , ("VK_FORMAT_BC7_UNORM_BLOCK", pure VK_FORMAT_BC7_UNORM_BLOCK)
                             , ("VK_FORMAT_BC7_SRGB_BLOCK", pure VK_FORMAT_BC7_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK", pure VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK", pure VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK", pure VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK", pure VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK", pure VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK)
                             , ("VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK", pure VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK)
                             , ("VK_FORMAT_EAC_R11_UNORM_BLOCK", pure VK_FORMAT_EAC_R11_UNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11_SNORM_BLOCK", pure VK_FORMAT_EAC_R11_SNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11G11_UNORM_BLOCK", pure VK_FORMAT_EAC_R11G11_UNORM_BLOCK)
                             , ("VK_FORMAT_EAC_R11G11_SNORM_BLOCK", pure VK_FORMAT_EAC_R11G11_SNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_4x4_UNORM_BLOCK", pure VK_FORMAT_ASTC_4x4_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_4x4_SRGB_BLOCK", pure VK_FORMAT_ASTC_4x4_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_5x4_UNORM_BLOCK", pure VK_FORMAT_ASTC_5x4_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_5x4_SRGB_BLOCK", pure VK_FORMAT_ASTC_5x4_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_5x5_UNORM_BLOCK", pure VK_FORMAT_ASTC_5x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_5x5_SRGB_BLOCK", pure VK_FORMAT_ASTC_5x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_6x5_UNORM_BLOCK", pure VK_FORMAT_ASTC_6x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_6x5_SRGB_BLOCK", pure VK_FORMAT_ASTC_6x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_6x6_UNORM_BLOCK", pure VK_FORMAT_ASTC_6x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_6x6_SRGB_BLOCK", pure VK_FORMAT_ASTC_6x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x5_UNORM_BLOCK", pure VK_FORMAT_ASTC_8x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x5_SRGB_BLOCK", pure VK_FORMAT_ASTC_8x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x6_UNORM_BLOCK", pure VK_FORMAT_ASTC_8x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x6_SRGB_BLOCK", pure VK_FORMAT_ASTC_8x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_8x8_UNORM_BLOCK", pure VK_FORMAT_ASTC_8x8_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_8x8_SRGB_BLOCK", pure VK_FORMAT_ASTC_8x8_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x5_UNORM_BLOCK", pure VK_FORMAT_ASTC_10x5_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x5_SRGB_BLOCK", pure VK_FORMAT_ASTC_10x5_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x6_UNORM_BLOCK", pure VK_FORMAT_ASTC_10x6_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x6_SRGB_BLOCK", pure VK_FORMAT_ASTC_10x6_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x8_UNORM_BLOCK", pure VK_FORMAT_ASTC_10x8_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x8_SRGB_BLOCK", pure VK_FORMAT_ASTC_10x8_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_10x10_UNORM_BLOCK", pure VK_FORMAT_ASTC_10x10_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_10x10_SRGB_BLOCK", pure VK_FORMAT_ASTC_10x10_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_12x10_UNORM_BLOCK", pure VK_FORMAT_ASTC_12x10_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_12x10_SRGB_BLOCK", pure VK_FORMAT_ASTC_12x10_SRGB_BLOCK)
                             , ("VK_FORMAT_ASTC_12x12_UNORM_BLOCK", pure VK_FORMAT_ASTC_12x12_UNORM_BLOCK)
                             , ("VK_FORMAT_ASTC_12x12_SRGB_BLOCK", pure VK_FORMAT_ASTC_12x12_SRGB_BLOCK)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkFormat")
                        v <- step readPrec
                        pure (VkFormat v)
                        )
                    )


pattern VK_FORMAT_UNDEFINED = VkFormat 0

pattern VK_FORMAT_R4G4_UNORM_PACK8 = VkFormat 1

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = VkFormat 2

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = VkFormat 3

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = VkFormat 4

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = VkFormat 5

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = VkFormat 6

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = VkFormat 7

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = VkFormat 8

pattern VK_FORMAT_R8_UNORM = VkFormat 9

pattern VK_FORMAT_R8_SNORM = VkFormat 10

pattern VK_FORMAT_R8_USCALED = VkFormat 11

pattern VK_FORMAT_R8_SSCALED = VkFormat 12

pattern VK_FORMAT_R8_UINT = VkFormat 13

pattern VK_FORMAT_R8_SINT = VkFormat 14

pattern VK_FORMAT_R8_SRGB = VkFormat 15

pattern VK_FORMAT_R8G8_UNORM = VkFormat 16

pattern VK_FORMAT_R8G8_SNORM = VkFormat 17

pattern VK_FORMAT_R8G8_USCALED = VkFormat 18

pattern VK_FORMAT_R8G8_SSCALED = VkFormat 19

pattern VK_FORMAT_R8G8_UINT = VkFormat 20

pattern VK_FORMAT_R8G8_SINT = VkFormat 21

pattern VK_FORMAT_R8G8_SRGB = VkFormat 22

pattern VK_FORMAT_R8G8B8_UNORM = VkFormat 23

pattern VK_FORMAT_R8G8B8_SNORM = VkFormat 24

pattern VK_FORMAT_R8G8B8_USCALED = VkFormat 25

pattern VK_FORMAT_R8G8B8_SSCALED = VkFormat 26

pattern VK_FORMAT_R8G8B8_UINT = VkFormat 27

pattern VK_FORMAT_R8G8B8_SINT = VkFormat 28

pattern VK_FORMAT_R8G8B8_SRGB = VkFormat 29

pattern VK_FORMAT_B8G8R8_UNORM = VkFormat 30

pattern VK_FORMAT_B8G8R8_SNORM = VkFormat 31

pattern VK_FORMAT_B8G8R8_USCALED = VkFormat 32

pattern VK_FORMAT_B8G8R8_SSCALED = VkFormat 33

pattern VK_FORMAT_B8G8R8_UINT = VkFormat 34

pattern VK_FORMAT_B8G8R8_SINT = VkFormat 35

pattern VK_FORMAT_B8G8R8_SRGB = VkFormat 36

pattern VK_FORMAT_R8G8B8A8_UNORM = VkFormat 37

pattern VK_FORMAT_R8G8B8A8_SNORM = VkFormat 38

pattern VK_FORMAT_R8G8B8A8_USCALED = VkFormat 39

pattern VK_FORMAT_R8G8B8A8_SSCALED = VkFormat 40

pattern VK_FORMAT_R8G8B8A8_UINT = VkFormat 41

pattern VK_FORMAT_R8G8B8A8_SINT = VkFormat 42

pattern VK_FORMAT_R8G8B8A8_SRGB = VkFormat 43

pattern VK_FORMAT_B8G8R8A8_UNORM = VkFormat 44

pattern VK_FORMAT_B8G8R8A8_SNORM = VkFormat 45

pattern VK_FORMAT_B8G8R8A8_USCALED = VkFormat 46

pattern VK_FORMAT_B8G8R8A8_SSCALED = VkFormat 47

pattern VK_FORMAT_B8G8R8A8_UINT = VkFormat 48

pattern VK_FORMAT_B8G8R8A8_SINT = VkFormat 49

pattern VK_FORMAT_B8G8R8A8_SRGB = VkFormat 50

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = VkFormat 51

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = VkFormat 52

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = VkFormat 53

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = VkFormat 54

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = VkFormat 55

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = VkFormat 56

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = VkFormat 57

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = VkFormat 58

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = VkFormat 59

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = VkFormat 60

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = VkFormat 61

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = VkFormat 62

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = VkFormat 63

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = VkFormat 64

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = VkFormat 65

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = VkFormat 66

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = VkFormat 67

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = VkFormat 68

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = VkFormat 69

pattern VK_FORMAT_R16_UNORM = VkFormat 70

pattern VK_FORMAT_R16_SNORM = VkFormat 71

pattern VK_FORMAT_R16_USCALED = VkFormat 72

pattern VK_FORMAT_R16_SSCALED = VkFormat 73

pattern VK_FORMAT_R16_UINT = VkFormat 74

pattern VK_FORMAT_R16_SINT = VkFormat 75

pattern VK_FORMAT_R16_SFLOAT = VkFormat 76

pattern VK_FORMAT_R16G16_UNORM = VkFormat 77

pattern VK_FORMAT_R16G16_SNORM = VkFormat 78

pattern VK_FORMAT_R16G16_USCALED = VkFormat 79

pattern VK_FORMAT_R16G16_SSCALED = VkFormat 80

pattern VK_FORMAT_R16G16_UINT = VkFormat 81

pattern VK_FORMAT_R16G16_SINT = VkFormat 82

pattern VK_FORMAT_R16G16_SFLOAT = VkFormat 83

pattern VK_FORMAT_R16G16B16_UNORM = VkFormat 84

pattern VK_FORMAT_R16G16B16_SNORM = VkFormat 85

pattern VK_FORMAT_R16G16B16_USCALED = VkFormat 86

pattern VK_FORMAT_R16G16B16_SSCALED = VkFormat 87

pattern VK_FORMAT_R16G16B16_UINT = VkFormat 88

pattern VK_FORMAT_R16G16B16_SINT = VkFormat 89

pattern VK_FORMAT_R16G16B16_SFLOAT = VkFormat 90

pattern VK_FORMAT_R16G16B16A16_UNORM = VkFormat 91

pattern VK_FORMAT_R16G16B16A16_SNORM = VkFormat 92

pattern VK_FORMAT_R16G16B16A16_USCALED = VkFormat 93

pattern VK_FORMAT_R16G16B16A16_SSCALED = VkFormat 94

pattern VK_FORMAT_R16G16B16A16_UINT = VkFormat 95

pattern VK_FORMAT_R16G16B16A16_SINT = VkFormat 96

pattern VK_FORMAT_R16G16B16A16_SFLOAT = VkFormat 97

pattern VK_FORMAT_R32_UINT = VkFormat 98

pattern VK_FORMAT_R32_SINT = VkFormat 99

pattern VK_FORMAT_R32_SFLOAT = VkFormat 100

pattern VK_FORMAT_R32G32_UINT = VkFormat 101

pattern VK_FORMAT_R32G32_SINT = VkFormat 102

pattern VK_FORMAT_R32G32_SFLOAT = VkFormat 103

pattern VK_FORMAT_R32G32B32_UINT = VkFormat 104

pattern VK_FORMAT_R32G32B32_SINT = VkFormat 105

pattern VK_FORMAT_R32G32B32_SFLOAT = VkFormat 106

pattern VK_FORMAT_R32G32B32A32_UINT = VkFormat 107

pattern VK_FORMAT_R32G32B32A32_SINT = VkFormat 108

pattern VK_FORMAT_R32G32B32A32_SFLOAT = VkFormat 109

pattern VK_FORMAT_R64_UINT = VkFormat 110

pattern VK_FORMAT_R64_SINT = VkFormat 111

pattern VK_FORMAT_R64_SFLOAT = VkFormat 112

pattern VK_FORMAT_R64G64_UINT = VkFormat 113

pattern VK_FORMAT_R64G64_SINT = VkFormat 114

pattern VK_FORMAT_R64G64_SFLOAT = VkFormat 115

pattern VK_FORMAT_R64G64B64_UINT = VkFormat 116

pattern VK_FORMAT_R64G64B64_SINT = VkFormat 117

pattern VK_FORMAT_R64G64B64_SFLOAT = VkFormat 118

pattern VK_FORMAT_R64G64B64A64_UINT = VkFormat 119

pattern VK_FORMAT_R64G64B64A64_SINT = VkFormat 120

pattern VK_FORMAT_R64G64B64A64_SFLOAT = VkFormat 121

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = VkFormat 122

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = VkFormat 123

pattern VK_FORMAT_D16_UNORM = VkFormat 124

pattern VK_FORMAT_X8_D24_UNORM_PACK32 = VkFormat 125

pattern VK_FORMAT_D32_SFLOAT = VkFormat 126

pattern VK_FORMAT_S8_UINT = VkFormat 127

pattern VK_FORMAT_D16_UNORM_S8_UINT = VkFormat 128

pattern VK_FORMAT_D24_UNORM_S8_UINT = VkFormat 129

pattern VK_FORMAT_D32_SFLOAT_S8_UINT = VkFormat 130

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = VkFormat 131

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = VkFormat 132

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = VkFormat 133

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = VkFormat 134

pattern VK_FORMAT_BC2_UNORM_BLOCK = VkFormat 135

pattern VK_FORMAT_BC2_SRGB_BLOCK = VkFormat 136

pattern VK_FORMAT_BC3_UNORM_BLOCK = VkFormat 137

pattern VK_FORMAT_BC3_SRGB_BLOCK = VkFormat 138

pattern VK_FORMAT_BC4_UNORM_BLOCK = VkFormat 139

pattern VK_FORMAT_BC4_SNORM_BLOCK = VkFormat 140

pattern VK_FORMAT_BC5_UNORM_BLOCK = VkFormat 141

pattern VK_FORMAT_BC5_SNORM_BLOCK = VkFormat 142

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = VkFormat 143

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = VkFormat 144

pattern VK_FORMAT_BC7_UNORM_BLOCK = VkFormat 145

pattern VK_FORMAT_BC7_SRGB_BLOCK = VkFormat 146

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = VkFormat 147

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = VkFormat 148

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = VkFormat 149

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = VkFormat 150

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = VkFormat 151

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = VkFormat 152

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = VkFormat 153

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = VkFormat 154

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = VkFormat 155

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = VkFormat 156

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = VkFormat 157

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = VkFormat 158

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = VkFormat 159

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = VkFormat 160

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = VkFormat 161

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = VkFormat 162

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = VkFormat 163

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = VkFormat 164

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = VkFormat 165

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = VkFormat 166

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = VkFormat 167

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = VkFormat 168

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = VkFormat 169

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = VkFormat 170

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = VkFormat 171

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = VkFormat 172

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = VkFormat 173

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = VkFormat 174

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = VkFormat 175

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = VkFormat 176

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = VkFormat 177

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = VkFormat 178

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = VkFormat 179

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = VkFormat 180

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = VkFormat 181

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = VkFormat 182

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = VkFormat 183

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = VkFormat 184

type VkFlags = Word32


data VkExtent2D =
  VkExtent2D{ width :: Word32 
            , height :: Word32 
            }
  deriving (Eq)

instance Storable VkExtent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkExtent2D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (width (poked :: VkExtent2D))
                *> poke (ptr `plusPtr` 4) (height (poked :: VkExtent2D))


-- ** VkSharingMode

newtype VkSharingMode = VkSharingMode Int32
  deriving (Eq, Storable)

instance Show VkSharingMode where
  showsPrec _ VK_SHARING_MODE_EXCLUSIVE = showString "VK_SHARING_MODE_EXCLUSIVE"
  showsPrec _ VK_SHARING_MODE_CONCURRENT = showString "VK_SHARING_MODE_CONCURRENT"
  showsPrec p (VkSharingMode x) = showParen (p >= 11) (showString "VkSharingMode " . showsPrec 11 x)

instance Read VkSharingMode where
  readPrec = parens ( choose [ ("VK_SHARING_MODE_EXCLUSIVE", pure VK_SHARING_MODE_EXCLUSIVE)
                             , ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkSharingMode")
                        v <- step readPrec
                        pure (VkSharingMode v)
                        )
                    )


pattern VK_SHARING_MODE_EXCLUSIVE = VkSharingMode 0

pattern VK_SHARING_MODE_CONCURRENT = VkSharingMode 1

-- ** VkStructureType
-- | Structure type enumerant
newtype VkStructureType = VkStructureType Int32
  deriving (Eq, Storable)

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
  showsPrec p (VkStructureType x) = showParen (p >= 11) (showString "VkStructureType " . showsPrec 11 x)

instance Read VkStructureType where
  readPrec = parens ( choose [ ("VK_STRUCTURE_TYPE_APPLICATION_INFO", pure VK_STRUCTURE_TYPE_APPLICATION_INFO)
                             , ("VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO", pure VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO", pure VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO", pure VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SUBMIT_INFO", pure VK_STRUCTURE_TYPE_SUBMIT_INFO)
                             , ("VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO", pure VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE", pure VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE)
                             , ("VK_STRUCTURE_TYPE_BIND_SPARSE_INFO", pure VK_STRUCTURE_TYPE_BIND_SPARSE_INFO)
                             , ("VK_STRUCTURE_TYPE_FENCE_CREATE_INFO", pure VK_STRUCTURE_TYPE_FENCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO", pure VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_EVENT_CREATE_INFO", pure VK_STRUCTURE_TYPE_EVENT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO", pure VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO", pure VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO", pure VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO", pure VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO", pure VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO", pure VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO", pure VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO", pure VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO", pure VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO", pure VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO", pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO", pure VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO", pure VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET", pure VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET)
                             , ("VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET", pure VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET)
                             , ("VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO", pure VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO", pure VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO", pure VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO", pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO", pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO)
                             , ("VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO", pure VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO)
                             , ("VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO", pure VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO)
                             , ("VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER", pure VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER", pure VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_MEMORY_BARRIER", pure VK_STRUCTURE_TYPE_MEMORY_BARRIER)
                             , ("VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO", pure VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO)
                             , ("VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO", pure VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkStructureType")
                        v <- step readPrec
                        pure (VkStructureType v)
                        )
                    )


pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = VkStructureType 0

pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = VkStructureType 1

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = VkStructureType 2

pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = VkStructureType 3

pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = VkStructureType 4

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = VkStructureType 5

pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = VkStructureType 6

pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = VkStructureType 7

pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = VkStructureType 8

pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = VkStructureType 9

pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = VkStructureType 10

pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = VkStructureType 11

pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = VkStructureType 12

pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = VkStructureType 13

pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = VkStructureType 14

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = VkStructureType 15

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = VkStructureType 16

pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = VkStructureType 17

pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = VkStructureType 18

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = VkStructureType 19

pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = VkStructureType 20

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = VkStructureType 21

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = VkStructureType 22

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = VkStructureType 23

pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = VkStructureType 24

pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = VkStructureType 25

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = VkStructureType 26

pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = VkStructureType 27

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = VkStructureType 28

pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = VkStructureType 29

pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = VkStructureType 30

pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = VkStructureType 31

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = VkStructureType 32

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = VkStructureType 33

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = VkStructureType 34

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = VkStructureType 35

pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = VkStructureType 36

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = VkStructureType 37

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = VkStructureType 38

pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = VkStructureType 39

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = VkStructureType 40

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = VkStructureType 41

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = VkStructureType 42

pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = VkStructureType 43

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = VkStructureType 44

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = VkStructureType 45

pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = VkStructureType 46

pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = VkStructureType 47

pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = VkStructureType 48

newtype VkBool32 = VkBool32 Word32
  deriving (Eq, Storable)


data VkOffset2D =
  VkOffset2D{ x :: Int32 
            , y :: Int32 
            }
  deriving (Eq)

instance Storable VkOffset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = VkOffset2D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: VkOffset2D))
                *> poke (ptr `plusPtr` 4) (y (poked :: VkOffset2D))



data VkOffset3D =
  VkOffset3D{ x :: Int32 
            , y :: Int32 
            , z :: Int32 
            }
  deriving (Eq)

instance Storable VkOffset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkOffset3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: VkOffset3D))
                *> poke (ptr `plusPtr` 4) (y (poked :: VkOffset3D))
                *> poke (ptr `plusPtr` 8) (z (poked :: VkOffset3D))



data VkExtent3D =
  VkExtent3D{ width :: Word32 
            , height :: Word32 
            , depth :: Word32 
            }
  deriving (Eq)

instance Storable VkExtent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = VkExtent3D <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (width (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 4) (height (poked :: VkExtent3D))
                *> poke (ptr `plusPtr` 8) (depth (poked :: VkExtent3D))



data VkRect3D =
  VkRect3D{ offset :: VkOffset3D 
          , extent :: VkExtent3D 
          }
  deriving (Eq)

instance Storable VkRect3D where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkRect3D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: VkRect3D))
                *> poke (ptr `plusPtr` 12) (extent (poked :: VkRect3D))


-- ** VkResult
-- | Error and return codes
newtype VkResult = VkResult Int32
  deriving (Eq, Storable)

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
  showsPrec p (VkResult x) = showParen (p >= 11) (showString "VkResult " . showsPrec 11 x)

instance Read VkResult where
  readPrec = parens ( choose [ ("VK_SUCCESS", pure VK_SUCCESS)
                             , ("VK_NOT_READY", pure VK_NOT_READY)
                             , ("VK_TIMEOUT", pure VK_TIMEOUT)
                             , ("VK_EVENT_SET", pure VK_EVENT_SET)
                             , ("VK_EVENT_RESET", pure VK_EVENT_RESET)
                             , ("VK_INCOMPLETE", pure VK_INCOMPLETE)
                             , ("VK_ERROR_OUT_OF_HOST_MEMORY", pure VK_ERROR_OUT_OF_HOST_MEMORY)
                             , ("VK_ERROR_OUT_OF_DEVICE_MEMORY", pure VK_ERROR_OUT_OF_DEVICE_MEMORY)
                             , ("VK_ERROR_INITIALIZATION_FAILED", pure VK_ERROR_INITIALIZATION_FAILED)
                             , ("VK_ERROR_DEVICE_LOST", pure VK_ERROR_DEVICE_LOST)
                             , ("VK_ERROR_MEMORY_MAP_FAILED", pure VK_ERROR_MEMORY_MAP_FAILED)
                             , ("VK_ERROR_LAYER_NOT_PRESENT", pure VK_ERROR_LAYER_NOT_PRESENT)
                             , ("VK_ERROR_EXTENSION_NOT_PRESENT", pure VK_ERROR_EXTENSION_NOT_PRESENT)
                             , ("VK_ERROR_FEATURE_NOT_PRESENT", pure VK_ERROR_FEATURE_NOT_PRESENT)
                             , ("VK_ERROR_INCOMPATIBLE_DRIVER", pure VK_ERROR_INCOMPATIBLE_DRIVER)
                             , ("VK_ERROR_TOO_MANY_OBJECTS", pure VK_ERROR_TOO_MANY_OBJECTS)
                             , ("VK_ERROR_FORMAT_NOT_SUPPORTED", pure VK_ERROR_FORMAT_NOT_SUPPORTED)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkResult")
                        v <- step readPrec
                        pure (VkResult v)
                        )
                    )

-- | Command completed successfully
pattern VK_SUCCESS = VkResult 0
-- | A fence or query has not yet completed
pattern VK_NOT_READY = VkResult 1
-- | A wait operation has not completed in the specified time
pattern VK_TIMEOUT = VkResult 2
-- | An event is signaled
pattern VK_EVENT_SET = VkResult 3
-- | An event is unsignalled
pattern VK_EVENT_RESET = VkResult 4
-- | A return array was too small for the resul
pattern VK_INCOMPLETE = VkResult 5
-- | A host memory allocation has failed
pattern VK_ERROR_OUT_OF_HOST_MEMORY = VkResult (-1)
-- | A device memory allocation has failed
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = VkResult (-2)
-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern VK_ERROR_INITIALIZATION_FAILED = VkResult (-3)
-- | Initialization of a object has failed
pattern VK_ERROR_DEVICE_LOST = VkResult (-4)
-- | Mapping of a memory object has failed
pattern VK_ERROR_MEMORY_MAP_FAILED = VkResult (-5)
-- | Layer specified does not exist
pattern VK_ERROR_LAYER_NOT_PRESENT = VkResult (-6)
-- | Extension specified does not exist
pattern VK_ERROR_EXTENSION_NOT_PRESENT = VkResult (-7)
-- | Requested feature is not available on this device
pattern VK_ERROR_FEATURE_NOT_PRESENT = VkResult (-8)
-- | Unable to find a Vulkan driver
pattern VK_ERROR_INCOMPATIBLE_DRIVER = VkResult (-9)
-- | Too many objects of the type have already been created
pattern VK_ERROR_TOO_MANY_OBJECTS = VkResult (-10)
-- | Requested format is not supported on this device
pattern VK_ERROR_FORMAT_NOT_SUPPORTED = VkResult (-11)


data VkViewport =
  VkViewport{ x :: CFloat 
            , y :: CFloat 
            , width :: CFloat 
            , height :: CFloat 
            , minDepth :: CFloat 
            , maxDepth :: CFloat 
            }
  deriving (Eq)

instance Storable VkViewport where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = VkViewport <$> peek (ptr `plusPtr` 0)
                        <*> peek (ptr `plusPtr` 4)
                        <*> peek (ptr `plusPtr` 8)
                        <*> peek (ptr `plusPtr` 12)
                        <*> peek (ptr `plusPtr` 16)
                        <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: VkViewport))
                *> poke (ptr `plusPtr` 4) (y (poked :: VkViewport))
                *> poke (ptr `plusPtr` 8) (width (poked :: VkViewport))
                *> poke (ptr `plusPtr` 12) (height (poked :: VkViewport))
                *> poke (ptr `plusPtr` 16) (minDepth (poked :: VkViewport))
                *> poke (ptr `plusPtr` 20) (maxDepth (poked :: VkViewport))



data VkRect2D =
  VkRect2D{ offset :: VkOffset2D 
          , extent :: VkExtent2D 
          }
  deriving (Eq)

instance Storable VkRect2D where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = VkRect2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: VkRect2D))
                *> poke (ptr `plusPtr` 8) (extent (poked :: VkRect2D))


