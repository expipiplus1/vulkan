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
import Data.Word( Word64(..)
                , Word32(..)
                )
import Foreign.Ptr( plusPtr
                  )
import Data.Int( Int32(..)
               , Int32
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
import Foreign.C.Types( CFloat(..)
                      )

newtype VkDeviceSize = VkDeviceSize Word64
  deriving (Eq, Storable)

-- ** Format
-- | Vulkan format definitions
newtype Format = Format Int32
  deriving (Eq, Storable)

instance Show Format where
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
  showsPrec p (Format x) = showParen (p >= 11) (showString "Format " . showsPrec 11 x)

instance Read Format where
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
                        expectP (Ident "Format")
                        v <- step readPrec
                        pure (Format v)
                        )
                    )


pattern VK_FORMAT_UNDEFINED = Format 0

pattern VK_FORMAT_R4G4_UNORM_PACK8 = Format 1

pattern VK_FORMAT_R4G4B4A4_UNORM_PACK16 = Format 2

pattern VK_FORMAT_B4G4R4A4_UNORM_PACK16 = Format 3

pattern VK_FORMAT_R5G6B5_UNORM_PACK16 = Format 4

pattern VK_FORMAT_B5G6R5_UNORM_PACK16 = Format 5

pattern VK_FORMAT_R5G5B5A1_UNORM_PACK16 = Format 6

pattern VK_FORMAT_B5G5R5A1_UNORM_PACK16 = Format 7

pattern VK_FORMAT_A1R5G5B5_UNORM_PACK16 = Format 8

pattern VK_FORMAT_R8_UNORM = Format 9

pattern VK_FORMAT_R8_SNORM = Format 10

pattern VK_FORMAT_R8_USCALED = Format 11

pattern VK_FORMAT_R8_SSCALED = Format 12

pattern VK_FORMAT_R8_UINT = Format 13

pattern VK_FORMAT_R8_SINT = Format 14

pattern VK_FORMAT_R8_SRGB = Format 15

pattern VK_FORMAT_R8G8_UNORM = Format 16

pattern VK_FORMAT_R8G8_SNORM = Format 17

pattern VK_FORMAT_R8G8_USCALED = Format 18

pattern VK_FORMAT_R8G8_SSCALED = Format 19

pattern VK_FORMAT_R8G8_UINT = Format 20

pattern VK_FORMAT_R8G8_SINT = Format 21

pattern VK_FORMAT_R8G8_SRGB = Format 22

pattern VK_FORMAT_R8G8B8_UNORM = Format 23

pattern VK_FORMAT_R8G8B8_SNORM = Format 24

pattern VK_FORMAT_R8G8B8_USCALED = Format 25

pattern VK_FORMAT_R8G8B8_SSCALED = Format 26

pattern VK_FORMAT_R8G8B8_UINT = Format 27

pattern VK_FORMAT_R8G8B8_SINT = Format 28

pattern VK_FORMAT_R8G8B8_SRGB = Format 29

pattern VK_FORMAT_B8G8R8_UNORM = Format 30

pattern VK_FORMAT_B8G8R8_SNORM = Format 31

pattern VK_FORMAT_B8G8R8_USCALED = Format 32

pattern VK_FORMAT_B8G8R8_SSCALED = Format 33

pattern VK_FORMAT_B8G8R8_UINT = Format 34

pattern VK_FORMAT_B8G8R8_SINT = Format 35

pattern VK_FORMAT_B8G8R8_SRGB = Format 36

pattern VK_FORMAT_R8G8B8A8_UNORM = Format 37

pattern VK_FORMAT_R8G8B8A8_SNORM = Format 38

pattern VK_FORMAT_R8G8B8A8_USCALED = Format 39

pattern VK_FORMAT_R8G8B8A8_SSCALED = Format 40

pattern VK_FORMAT_R8G8B8A8_UINT = Format 41

pattern VK_FORMAT_R8G8B8A8_SINT = Format 42

pattern VK_FORMAT_R8G8B8A8_SRGB = Format 43

pattern VK_FORMAT_B8G8R8A8_UNORM = Format 44

pattern VK_FORMAT_B8G8R8A8_SNORM = Format 45

pattern VK_FORMAT_B8G8R8A8_USCALED = Format 46

pattern VK_FORMAT_B8G8R8A8_SSCALED = Format 47

pattern VK_FORMAT_B8G8R8A8_UINT = Format 48

pattern VK_FORMAT_B8G8R8A8_SINT = Format 49

pattern VK_FORMAT_B8G8R8A8_SRGB = Format 50

pattern VK_FORMAT_A8B8G8R8_UNORM_PACK32 = Format 51

pattern VK_FORMAT_A8B8G8R8_SNORM_PACK32 = Format 52

pattern VK_FORMAT_A8B8G8R8_USCALED_PACK32 = Format 53

pattern VK_FORMAT_A8B8G8R8_SSCALED_PACK32 = Format 54

pattern VK_FORMAT_A8B8G8R8_UINT_PACK32 = Format 55

pattern VK_FORMAT_A8B8G8R8_SINT_PACK32 = Format 56

pattern VK_FORMAT_A8B8G8R8_SRGB_PACK32 = Format 57

pattern VK_FORMAT_A2R10G10B10_UNORM_PACK32 = Format 58

pattern VK_FORMAT_A2R10G10B10_SNORM_PACK32 = Format 59

pattern VK_FORMAT_A2R10G10B10_USCALED_PACK32 = Format 60

pattern VK_FORMAT_A2R10G10B10_SSCALED_PACK32 = Format 61

pattern VK_FORMAT_A2R10G10B10_UINT_PACK32 = Format 62

pattern VK_FORMAT_A2R10G10B10_SINT_PACK32 = Format 63

pattern VK_FORMAT_A2B10G10R10_UNORM_PACK32 = Format 64

pattern VK_FORMAT_A2B10G10R10_SNORM_PACK32 = Format 65

pattern VK_FORMAT_A2B10G10R10_USCALED_PACK32 = Format 66

pattern VK_FORMAT_A2B10G10R10_SSCALED_PACK32 = Format 67

pattern VK_FORMAT_A2B10G10R10_UINT_PACK32 = Format 68

pattern VK_FORMAT_A2B10G10R10_SINT_PACK32 = Format 69

pattern VK_FORMAT_R16_UNORM = Format 70

pattern VK_FORMAT_R16_SNORM = Format 71

pattern VK_FORMAT_R16_USCALED = Format 72

pattern VK_FORMAT_R16_SSCALED = Format 73

pattern VK_FORMAT_R16_UINT = Format 74

pattern VK_FORMAT_R16_SINT = Format 75

pattern VK_FORMAT_R16_SFLOAT = Format 76

pattern VK_FORMAT_R16G16_UNORM = Format 77

pattern VK_FORMAT_R16G16_SNORM = Format 78

pattern VK_FORMAT_R16G16_USCALED = Format 79

pattern VK_FORMAT_R16G16_SSCALED = Format 80

pattern VK_FORMAT_R16G16_UINT = Format 81

pattern VK_FORMAT_R16G16_SINT = Format 82

pattern VK_FORMAT_R16G16_SFLOAT = Format 83

pattern VK_FORMAT_R16G16B16_UNORM = Format 84

pattern VK_FORMAT_R16G16B16_SNORM = Format 85

pattern VK_FORMAT_R16G16B16_USCALED = Format 86

pattern VK_FORMAT_R16G16B16_SSCALED = Format 87

pattern VK_FORMAT_R16G16B16_UINT = Format 88

pattern VK_FORMAT_R16G16B16_SINT = Format 89

pattern VK_FORMAT_R16G16B16_SFLOAT = Format 90

pattern VK_FORMAT_R16G16B16A16_UNORM = Format 91

pattern VK_FORMAT_R16G16B16A16_SNORM = Format 92

pattern VK_FORMAT_R16G16B16A16_USCALED = Format 93

pattern VK_FORMAT_R16G16B16A16_SSCALED = Format 94

pattern VK_FORMAT_R16G16B16A16_UINT = Format 95

pattern VK_FORMAT_R16G16B16A16_SINT = Format 96

pattern VK_FORMAT_R16G16B16A16_SFLOAT = Format 97

pattern VK_FORMAT_R32_UINT = Format 98

pattern VK_FORMAT_R32_SINT = Format 99

pattern VK_FORMAT_R32_SFLOAT = Format 100

pattern VK_FORMAT_R32G32_UINT = Format 101

pattern VK_FORMAT_R32G32_SINT = Format 102

pattern VK_FORMAT_R32G32_SFLOAT = Format 103

pattern VK_FORMAT_R32G32B32_UINT = Format 104

pattern VK_FORMAT_R32G32B32_SINT = Format 105

pattern VK_FORMAT_R32G32B32_SFLOAT = Format 106

pattern VK_FORMAT_R32G32B32A32_UINT = Format 107

pattern VK_FORMAT_R32G32B32A32_SINT = Format 108

pattern VK_FORMAT_R32G32B32A32_SFLOAT = Format 109

pattern VK_FORMAT_R64_UINT = Format 110

pattern VK_FORMAT_R64_SINT = Format 111

pattern VK_FORMAT_R64_SFLOAT = Format 112

pattern VK_FORMAT_R64G64_UINT = Format 113

pattern VK_FORMAT_R64G64_SINT = Format 114

pattern VK_FORMAT_R64G64_SFLOAT = Format 115

pattern VK_FORMAT_R64G64B64_UINT = Format 116

pattern VK_FORMAT_R64G64B64_SINT = Format 117

pattern VK_FORMAT_R64G64B64_SFLOAT = Format 118

pattern VK_FORMAT_R64G64B64A64_UINT = Format 119

pattern VK_FORMAT_R64G64B64A64_SINT = Format 120

pattern VK_FORMAT_R64G64B64A64_SFLOAT = Format 121

pattern VK_FORMAT_B10G11R11_UFLOAT_PACK32 = Format 122

pattern VK_FORMAT_E5B9G9R9_UFLOAT_PACK32 = Format 123

pattern VK_FORMAT_D16_UNORM = Format 124

pattern VK_FORMAT_X8_D24_UNORM_PACK32 = Format 125

pattern VK_FORMAT_D32_SFLOAT = Format 126

pattern VK_FORMAT_S8_UINT = Format 127

pattern VK_FORMAT_D16_UNORM_S8_UINT = Format 128

pattern VK_FORMAT_D24_UNORM_S8_UINT = Format 129

pattern VK_FORMAT_D32_SFLOAT_S8_UINT = Format 130

pattern VK_FORMAT_BC1_RGB_UNORM_BLOCK = Format 131

pattern VK_FORMAT_BC1_RGB_SRGB_BLOCK = Format 132

pattern VK_FORMAT_BC1_RGBA_UNORM_BLOCK = Format 133

pattern VK_FORMAT_BC1_RGBA_SRGB_BLOCK = Format 134

pattern VK_FORMAT_BC2_UNORM_BLOCK = Format 135

pattern VK_FORMAT_BC2_SRGB_BLOCK = Format 136

pattern VK_FORMAT_BC3_UNORM_BLOCK = Format 137

pattern VK_FORMAT_BC3_SRGB_BLOCK = Format 138

pattern VK_FORMAT_BC4_UNORM_BLOCK = Format 139

pattern VK_FORMAT_BC4_SNORM_BLOCK = Format 140

pattern VK_FORMAT_BC5_UNORM_BLOCK = Format 141

pattern VK_FORMAT_BC5_SNORM_BLOCK = Format 142

pattern VK_FORMAT_BC6H_UFLOAT_BLOCK = Format 143

pattern VK_FORMAT_BC6H_SFLOAT_BLOCK = Format 144

pattern VK_FORMAT_BC7_UNORM_BLOCK = Format 145

pattern VK_FORMAT_BC7_SRGB_BLOCK = Format 146

pattern VK_FORMAT_ETC2_R8G8B8_UNORM_BLOCK = Format 147

pattern VK_FORMAT_ETC2_R8G8B8_SRGB_BLOCK = Format 148

pattern VK_FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK = Format 149

pattern VK_FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK = Format 150

pattern VK_FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK = Format 151

pattern VK_FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK = Format 152

pattern VK_FORMAT_EAC_R11_UNORM_BLOCK = Format 153

pattern VK_FORMAT_EAC_R11_SNORM_BLOCK = Format 154

pattern VK_FORMAT_EAC_R11G11_UNORM_BLOCK = Format 155

pattern VK_FORMAT_EAC_R11G11_SNORM_BLOCK = Format 156

pattern VK_FORMAT_ASTC_4x4_UNORM_BLOCK = Format 157

pattern VK_FORMAT_ASTC_4x4_SRGB_BLOCK = Format 158

pattern VK_FORMAT_ASTC_5x4_UNORM_BLOCK = Format 159

pattern VK_FORMAT_ASTC_5x4_SRGB_BLOCK = Format 160

pattern VK_FORMAT_ASTC_5x5_UNORM_BLOCK = Format 161

pattern VK_FORMAT_ASTC_5x5_SRGB_BLOCK = Format 162

pattern VK_FORMAT_ASTC_6x5_UNORM_BLOCK = Format 163

pattern VK_FORMAT_ASTC_6x5_SRGB_BLOCK = Format 164

pattern VK_FORMAT_ASTC_6x6_UNORM_BLOCK = Format 165

pattern VK_FORMAT_ASTC_6x6_SRGB_BLOCK = Format 166

pattern VK_FORMAT_ASTC_8x5_UNORM_BLOCK = Format 167

pattern VK_FORMAT_ASTC_8x5_SRGB_BLOCK = Format 168

pattern VK_FORMAT_ASTC_8x6_UNORM_BLOCK = Format 169

pattern VK_FORMAT_ASTC_8x6_SRGB_BLOCK = Format 170

pattern VK_FORMAT_ASTC_8x8_UNORM_BLOCK = Format 171

pattern VK_FORMAT_ASTC_8x8_SRGB_BLOCK = Format 172

pattern VK_FORMAT_ASTC_10x5_UNORM_BLOCK = Format 173

pattern VK_FORMAT_ASTC_10x5_SRGB_BLOCK = Format 174

pattern VK_FORMAT_ASTC_10x6_UNORM_BLOCK = Format 175

pattern VK_FORMAT_ASTC_10x6_SRGB_BLOCK = Format 176

pattern VK_FORMAT_ASTC_10x8_UNORM_BLOCK = Format 177

pattern VK_FORMAT_ASTC_10x8_SRGB_BLOCK = Format 178

pattern VK_FORMAT_ASTC_10x10_UNORM_BLOCK = Format 179

pattern VK_FORMAT_ASTC_10x10_SRGB_BLOCK = Format 180

pattern VK_FORMAT_ASTC_12x10_UNORM_BLOCK = Format 181

pattern VK_FORMAT_ASTC_12x10_SRGB_BLOCK = Format 182

pattern VK_FORMAT_ASTC_12x12_UNORM_BLOCK = Format 183

pattern VK_FORMAT_ASTC_12x12_SRGB_BLOCK = Format 184

type VkFlags = Word32


data Extent2D =
  Extent2D{ width :: Word32 
          , height :: Word32 
          }
  deriving (Eq)

instance Storable Extent2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = Extent2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (width (poked :: Extent2D))
                *> poke (ptr `plusPtr` 4) (height (poked :: Extent2D))


-- ** SharingMode

newtype SharingMode = SharingMode Int32
  deriving (Eq, Storable)

instance Show SharingMode where
  showsPrec _ VK_SHARING_MODE_EXCLUSIVE = showString "VK_SHARING_MODE_EXCLUSIVE"
  showsPrec _ VK_SHARING_MODE_CONCURRENT = showString "VK_SHARING_MODE_CONCURRENT"
  showsPrec p (SharingMode x) = showParen (p >= 11) (showString "SharingMode " . showsPrec 11 x)

instance Read SharingMode where
  readPrec = parens ( choose [ ("VK_SHARING_MODE_EXCLUSIVE", pure VK_SHARING_MODE_EXCLUSIVE)
                             , ("VK_SHARING_MODE_CONCURRENT", pure VK_SHARING_MODE_CONCURRENT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "SharingMode")
                        v <- step readPrec
                        pure (SharingMode v)
                        )
                    )


pattern VK_SHARING_MODE_EXCLUSIVE = SharingMode 0

pattern VK_SHARING_MODE_CONCURRENT = SharingMode 1

-- ** StructureType
-- | Structure type enumerant
newtype StructureType = StructureType Int32
  deriving (Eq, Storable)

instance Show StructureType where
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
  showsPrec p (StructureType x) = showParen (p >= 11) (showString "StructureType " . showsPrec 11 x)

instance Read StructureType where
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
                        expectP (Ident "StructureType")
                        v <- step readPrec
                        pure (StructureType v)
                        )
                    )


pattern VK_STRUCTURE_TYPE_APPLICATION_INFO = StructureType 0

pattern VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO = StructureType 1

pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO = StructureType 2

pattern VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO = StructureType 3

pattern VK_STRUCTURE_TYPE_SUBMIT_INFO = StructureType 4

pattern VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO = StructureType 5

pattern VK_STRUCTURE_TYPE_MAPPED_MEMORY_RANGE = StructureType 6

pattern VK_STRUCTURE_TYPE_BIND_SPARSE_INFO = StructureType 7

pattern VK_STRUCTURE_TYPE_FENCE_CREATE_INFO = StructureType 8

pattern VK_STRUCTURE_TYPE_SEMAPHORE_CREATE_INFO = StructureType 9

pattern VK_STRUCTURE_TYPE_EVENT_CREATE_INFO = StructureType 10

pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO = StructureType 11

pattern VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO = StructureType 12

pattern VK_STRUCTURE_TYPE_BUFFER_VIEW_CREATE_INFO = StructureType 13

pattern VK_STRUCTURE_TYPE_IMAGE_CREATE_INFO = StructureType 14

pattern VK_STRUCTURE_TYPE_IMAGE_VIEW_CREATE_INFO = StructureType 15

pattern VK_STRUCTURE_TYPE_SHADER_MODULE_CREATE_INFO = StructureType 16

pattern VK_STRUCTURE_TYPE_PIPELINE_CACHE_CREATE_INFO = StructureType 17

pattern VK_STRUCTURE_TYPE_PIPELINE_SHADER_STAGE_CREATE_INFO = StructureType 18

pattern VK_STRUCTURE_TYPE_PIPELINE_VERTEX_INPUT_STATE_CREATE_INFO = StructureType 19

pattern VK_STRUCTURE_TYPE_PIPELINE_INPUT_ASSEMBLY_STATE_CREATE_INFO = StructureType 20

pattern VK_STRUCTURE_TYPE_PIPELINE_TESSELLATION_STATE_CREATE_INFO = StructureType 21

pattern VK_STRUCTURE_TYPE_PIPELINE_VIEWPORT_STATE_CREATE_INFO = StructureType 22

pattern VK_STRUCTURE_TYPE_PIPELINE_RASTERIZATION_STATE_CREATE_INFO = StructureType 23

pattern VK_STRUCTURE_TYPE_PIPELINE_MULTISAMPLE_STATE_CREATE_INFO = StructureType 24

pattern VK_STRUCTURE_TYPE_PIPELINE_DEPTH_STENCIL_STATE_CREATE_INFO = StructureType 25

pattern VK_STRUCTURE_TYPE_PIPELINE_COLOR_BLEND_STATE_CREATE_INFO = StructureType 26

pattern VK_STRUCTURE_TYPE_PIPELINE_DYNAMIC_STATE_CREATE_INFO = StructureType 27

pattern VK_STRUCTURE_TYPE_GRAPHICS_PIPELINE_CREATE_INFO = StructureType 28

pattern VK_STRUCTURE_TYPE_COMPUTE_PIPELINE_CREATE_INFO = StructureType 29

pattern VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO = StructureType 30

pattern VK_STRUCTURE_TYPE_SAMPLER_CREATE_INFO = StructureType 31

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_LAYOUT_CREATE_INFO = StructureType 32

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO = StructureType 33

pattern VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO = StructureType 34

pattern VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET = StructureType 35

pattern VK_STRUCTURE_TYPE_COPY_DESCRIPTOR_SET = StructureType 36

pattern VK_STRUCTURE_TYPE_FRAMEBUFFER_CREATE_INFO = StructureType 37

pattern VK_STRUCTURE_TYPE_RENDER_PASS_CREATE_INFO = StructureType 38

pattern VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO = StructureType 39

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO = StructureType 40

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_INHERITANCE_INFO = StructureType 41

pattern VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO = StructureType 42

pattern VK_STRUCTURE_TYPE_RENDER_PASS_BEGIN_INFO = StructureType 43

pattern VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER = StructureType 44

pattern VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER = StructureType 45

pattern VK_STRUCTURE_TYPE_MEMORY_BARRIER = StructureType 46

pattern VK_STRUCTURE_TYPE_LOADER_INSTANCE_CREATE_INFO = StructureType 47

pattern VK_STRUCTURE_TYPE_LOADER_DEVICE_CREATE_INFO = StructureType 48

newtype VkBool32 = VkBool32 Word32
  deriving (Eq, Storable)


data Offset2D =
  Offset2D{ x :: Int32 
          , y :: Int32 
          }
  deriving (Eq)

instance Storable Offset2D where
  sizeOf ~_ = 8
  alignment ~_ = 4
  peek ptr = Offset2D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: Offset2D))
                *> poke (ptr `plusPtr` 4) (y (poked :: Offset2D))



data Offset3D =
  Offset3D{ x :: Int32 
          , y :: Int32 
          , z :: Int32 
          }
  deriving (Eq)

instance Storable Offset3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = Offset3D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: Offset3D))
                *> poke (ptr `plusPtr` 4) (y (poked :: Offset3D))
                *> poke (ptr `plusPtr` 8) (z (poked :: Offset3D))



data Extent3D =
  Extent3D{ width :: Word32 
          , height :: Word32 
          , depth :: Word32 
          }
  deriving (Eq)

instance Storable Extent3D where
  sizeOf ~_ = 12
  alignment ~_ = 4
  peek ptr = Extent3D <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
                      <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (width (poked :: Extent3D))
                *> poke (ptr `plusPtr` 4) (height (poked :: Extent3D))
                *> poke (ptr `plusPtr` 8) (depth (poked :: Extent3D))



data Rect3D =
  Rect3D{ offset :: Offset3D 
        , extent :: Extent3D 
        }
  deriving (Eq)

instance Storable Rect3D where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = Rect3D <$> peek (ptr `plusPtr` 0)
                    <*> peek (ptr `plusPtr` 12)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: Rect3D))
                *> poke (ptr `plusPtr` 12) (extent (poked :: Rect3D))


-- ** Result
-- | Error and return codes
newtype Result = Result Int32
  deriving (Eq, Storable)

instance Show Result where
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
  showsPrec p (Result x) = showParen (p >= 11) (showString "Result " . showsPrec 11 x)

instance Read Result where
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
                        expectP (Ident "Result")
                        v <- step readPrec
                        pure (Result v)
                        )
                    )

-- | Command completed successfully
pattern VK_SUCCESS = Result 0
-- | A fence or query has not yet completed
pattern VK_NOT_READY = Result 1
-- | A wait operation has not completed in the specified time
pattern VK_TIMEOUT = Result 2
-- | An event is signaled
pattern VK_EVENT_SET = Result 3
-- | An event is unsignalled
pattern VK_EVENT_RESET = Result 4
-- | A return array was too small for the resul
pattern VK_INCOMPLETE = Result 5
-- | A host memory allocation has failed
pattern VK_ERROR_OUT_OF_HOST_MEMORY = Result (-1)
-- | A device memory allocation has failed
pattern VK_ERROR_OUT_OF_DEVICE_MEMORY = Result (-2)
-- | The logical device has been lost. See <<devsandqueues-lost-device>>
pattern VK_ERROR_INITIALIZATION_FAILED = Result (-3)
-- | Initialization of a object has failed
pattern VK_ERROR_DEVICE_LOST = Result (-4)
-- | Mapping of a memory object has failed
pattern VK_ERROR_MEMORY_MAP_FAILED = Result (-5)
-- | Layer specified does not exist
pattern VK_ERROR_LAYER_NOT_PRESENT = Result (-6)
-- | Extension specified does not exist
pattern VK_ERROR_EXTENSION_NOT_PRESENT = Result (-7)
-- | Requested feature is not available on this device
pattern VK_ERROR_FEATURE_NOT_PRESENT = Result (-8)
-- | Unable to find a Vulkan driver
pattern VK_ERROR_INCOMPATIBLE_DRIVER = Result (-9)
-- | Too many objects of the type have already been created
pattern VK_ERROR_TOO_MANY_OBJECTS = Result (-10)
-- | Requested format is not supported on this device
pattern VK_ERROR_FORMAT_NOT_SUPPORTED = Result (-11)


data Viewport =
  Viewport{ x :: CFloat 
          , y :: CFloat 
          , width :: CFloat 
          , height :: CFloat 
          , minDepth :: CFloat 
          , maxDepth :: CFloat 
          }
  deriving (Eq)

instance Storable Viewport where
  sizeOf ~_ = 24
  alignment ~_ = 4
  peek ptr = Viewport <$> peek (ptr `plusPtr` 0)
                      <*> peek (ptr `plusPtr` 4)
                      <*> peek (ptr `plusPtr` 8)
                      <*> peek (ptr `plusPtr` 12)
                      <*> peek (ptr `plusPtr` 16)
                      <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (x (poked :: Viewport))
                *> poke (ptr `plusPtr` 4) (y (poked :: Viewport))
                *> poke (ptr `plusPtr` 8) (width (poked :: Viewport))
                *> poke (ptr `plusPtr` 12) (height (poked :: Viewport))
                *> poke (ptr `plusPtr` 16) (minDepth (poked :: Viewport))
                *> poke (ptr `plusPtr` 20) (maxDepth (poked :: Viewport))



data Rect2D =
  Rect2D{ offset :: Offset2D 
        , extent :: Extent2D 
        }
  deriving (Eq)

instance Storable Rect2D where
  sizeOf ~_ = 16
  alignment ~_ = 4
  peek ptr = Rect2D <$> peek (ptr `plusPtr` 0)
                    <*> peek (ptr `plusPtr` 8)
  poke ptr poked = poke (ptr `plusPtr` 0) (offset (poked :: Rect2D))
                *> poke (ptr `plusPtr` 8) (extent (poked :: Rect2D))


