{-# LANGUAGE CPP #-}

{-
  GL-to-vulkan conversion from
  https://github.com/KhronosGroup/KTX-Software/blob/bf849b7f/lib/vk_format.h
-}

{-
Copyright (c) 2016 Oculus VR, LLC.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
     http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

module Vulkan.Utils.FromGL
  ( internalFormat
  ) where

import qualified Vulkan.Core10.Enums.Format as Vk

#include "gl_enums.h"

-- | Convert an OpenGL format enum into a 'Vk.Format'
--
-- >>> internalFormat GL_RGB8
-- Just FORMAT_R8G8B8_UNORM
internalFormat :: (Eq a, Num a) => a -> Maybe Vk.Format
internalFormat = \case
  GL_R8    -> Just Vk.FORMAT_R8_UNORM       -- 1-component, 8-bit unsigned normalized
  GL_RG8   -> Just Vk.FORMAT_R8G8_UNORM     -- 2-component, 8-bit unsigned normalized
  GL_RGB8  -> Just Vk.FORMAT_R8G8B8_UNORM   -- 3-component, 8-bit unsigned normalized
  GL_RGBA8 -> Just Vk.FORMAT_R8G8B8A8_UNORM -- 4-component, 8-bit unsigned normalized

  GL_R8_SNORM    -> Just Vk.FORMAT_R8_SNORM       -- 1-component, 8-bit signed normalized
  GL_RG8_SNORM   -> Just Vk.FORMAT_R8G8_SNORM     -- 2-component, 8-bit signed normalized
  GL_RGB8_SNORM  -> Just Vk.FORMAT_R8G8B8_SNORM   -- 3-component, 8-bit signed normalized
  GL_RGBA8_SNORM -> Just Vk.FORMAT_R8G8B8A8_SNORM -- 4-component, 8-bit signed normalized

  GL_R8UI    -> Just Vk.FORMAT_R8_UINT       -- 1-component, 8-bit unsigned integer
  GL_RG8UI   -> Just Vk.FORMAT_R8G8_UINT     -- 2-component, 8-bit unsigned integer
  GL_RGB8UI  -> Just Vk.FORMAT_R8G8B8_UINT   -- 3-component, 8-bit unsigned integer
  GL_RGBA8UI -> Just Vk.FORMAT_R8G8B8A8_UINT -- 4-component, 8-bit unsigned integer

  GL_R8I    -> Just Vk.FORMAT_R8_SINT       -- 1-component, 8-bit signed integer
  GL_RG8I   -> Just Vk.FORMAT_R8G8_SINT     -- 2-component, 8-bit signed integer
  GL_RGB8I  -> Just Vk.FORMAT_R8G8B8_SINT   -- 3-component, 8-bit signed integer
  GL_RGBA8I -> Just Vk.FORMAT_R8G8B8A8_SINT -- 4-component, 8-bit signed integer

  GL_SR8          -> Just Vk.FORMAT_R8_SRGB       -- 1-component, 8-bit sRGB
  GL_SRG8         -> Just Vk.FORMAT_R8G8_SRGB     -- 2-component, 8-bit sRGB
  GL_SRGB8        -> Just Vk.FORMAT_R8G8B8_SRGB   -- 3-component, 8-bit sRGB
  GL_SRGB8_ALPHA8 -> Just Vk.FORMAT_R8G8B8A8_SRGB -- 4-component, 8-bit sRGB

  --
  -- 16 bits per component
  --
  GL_R16    -> Just Vk.FORMAT_R16_UNORM          -- 1-component, 16-bit unsigned normalized
  GL_RG16   -> Just Vk.FORMAT_R16G16_UNORM       -- 2-component, 16-bit unsigned normalized
  GL_RGB16  -> Just Vk.FORMAT_R16G16B16_UNORM    -- 3-component, 16-bit unsigned normalized
  GL_RGBA16 -> Just Vk.FORMAT_R16G16B16A16_UNORM -- 4-component, 16-bit unsigned normalized

  GL_R16_SNORM    -> Just Vk.FORMAT_R16_SNORM          -- 1-component, 16-bit signed normalized
  GL_RG16_SNORM   -> Just Vk.FORMAT_R16G16_SNORM       -- 2-component, 16-bit signed normalized
  GL_RGB16_SNORM  -> Just Vk.FORMAT_R16G16B16_SNORM    -- 3-component, 16-bit signed normalized
  GL_RGBA16_SNORM -> Just Vk.FORMAT_R16G16B16A16_SNORM -- 4-component, 16-bit signed normalized

  GL_R16UI    -> Just Vk.FORMAT_R16_UINT          -- 1-component, 16-bit unsigned integer
  GL_RG16UI   -> Just Vk.FORMAT_R16G16_UINT       -- 2-component, 16-bit unsigned integer
  GL_RGB16UI  -> Just Vk.FORMAT_R16G16B16_UINT    -- 3-component, 16-bit unsigned integer
  GL_RGBA16UI -> Just Vk.FORMAT_R16G16B16A16_UINT -- 4-component, 16-bit unsigned integer

  GL_R16I    -> Just Vk.FORMAT_R16_SINT          -- 1-component, 16-bit signed integer
  GL_RG16I   -> Just Vk.FORMAT_R16G16_SINT       -- 2-component, 16-bit signed integer
  GL_RGB16I  -> Just Vk.FORMAT_R16G16B16_SINT    -- 3-component, 16-bit signed integer
  GL_RGBA16I -> Just Vk.FORMAT_R16G16B16A16_SINT -- 4-component, 16-bit signed integer

  GL_R16F    -> Just Vk.FORMAT_R16_SFLOAT          -- 1-component, 16-bit floating-point
  GL_RG16F   -> Just Vk.FORMAT_R16G16_SFLOAT       -- 2-component, 16-bit floating-point
  GL_RGB16F  -> Just Vk.FORMAT_R16G16B16_SFLOAT    -- 3-component, 16-bit floating-point
  GL_RGBA16F -> Just Vk.FORMAT_R16G16B16A16_SFLOAT -- 4-component, 16-bit floating-point

  --
  -- 32 bits per component
  --
  GL_R32UI    -> Just Vk.FORMAT_R32_UINT          -- 1-component, 32-bit unsigned integer
  GL_RG32UI   -> Just Vk.FORMAT_R32G32_UINT       -- 2-component, 32-bit unsigned integer
  GL_RGB32UI  -> Just Vk.FORMAT_R32G32B32_UINT    -- 3-component, 32-bit unsigned integer
  GL_RGBA32UI -> Just Vk.FORMAT_R32G32B32A32_UINT -- 4-component, 32-bit unsigned integer

  GL_R32I    -> Just Vk.FORMAT_R32_SINT          -- 1-component, 32-bit signed integer
  GL_RG32I   -> Just Vk.FORMAT_R32G32_SINT       -- 2-component, 32-bit signed integer
  GL_RGB32I  -> Just Vk.FORMAT_R32G32B32_SINT    -- 3-component, 32-bit signed integer
  GL_RGBA32I -> Just Vk.FORMAT_R32G32B32A32_SINT -- 4-component, 32-bit signed integer

  GL_R32F    -> Just Vk.FORMAT_R32_SFLOAT          -- 1-component, 32-bit floating-point
  GL_RG32F   -> Just Vk.FORMAT_R32G32_SFLOAT       -- 2-component, 32-bit floating-point
  GL_RGB32F  -> Just Vk.FORMAT_R32G32B32_SFLOAT    -- 3-component, 32-bit floating-point
  GL_RGBA32F -> Just Vk.FORMAT_R32G32B32A32_SFLOAT -- 4-component, 32-bit floating-point

  --
  -- Packed
  --
  GL_R3_G3_B2       -> Nothing                                 -- 3-component 3:3:2,       unsigned normalized
  GL_RGB4           -> Nothing                                 -- 3-component 4:4:4,       unsigned normalized
  GL_RGB5           -> Just Vk.FORMAT_R5G5B5A1_UNORM_PACK16    -- 3-component 5:5:5,       unsigned normalized
  GL_RGB565         -> Just Vk.FORMAT_R5G6B5_UNORM_PACK16      -- 3-component 5:6:5,       unsigned normalized
  GL_RGB10          -> Just Vk.FORMAT_A2R10G10B10_UNORM_PACK32 -- 3-component 10:10:10,    unsigned normalized
  GL_RGB12          -> Nothing                                 -- 3-component 12:12:12,    unsigned normalized
  GL_RGBA2          -> Nothing                                 -- 4-component 2:2:2:2,     unsigned normalized
  GL_RGBA4          -> Just Vk.FORMAT_R4G4B4A4_UNORM_PACK16    -- 4-component 4:4:4:4,     unsigned normalized
  GL_RGBA12         -> Nothing                                 -- 4-component 12:12:12:12, unsigned normalized
  GL_RGB5_A1        -> Just Vk.FORMAT_A1R5G5B5_UNORM_PACK16    -- 4-component 5:5:5:1,     unsigned normalized
  GL_RGB10_A2       -> Just Vk.FORMAT_A2R10G10B10_UNORM_PACK32 -- 4-component 10:10:10:2,  unsigned normalized
  GL_RGB10_A2UI     -> Just Vk.FORMAT_A2R10G10B10_UINT_PACK32  -- 4-component 10:10:10:2,  unsigned integer
  GL_R11F_G11F_B10F -> Just Vk.FORMAT_B10G11R11_UFLOAT_PACK32  -- 3-component 11:11:10,    floating-point
  GL_RGB9_E5        -> Just Vk.FORMAT_E5B9G9R9_UFLOAT_PACK32   -- 3-component/exp 9:9:9/5, floating-point

  --
  -- S3TC/DXT/BC
  --

  GL_COMPRESSED_RGB_S3TC_DXT1_EXT  -> Just Vk.FORMAT_BC1_RGB_UNORM_BLOCK  -- line through 3D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT -> Just Vk.FORMAT_BC1_RGBA_UNORM_BLOCK -- line through 3D space plus 1-bit alpha, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT -> Just Vk.FORMAT_BC2_UNORM_BLOCK      -- line through 3D space plus line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT -> Just Vk.FORMAT_BC3_UNORM_BLOCK      -- line through 3D space plus 4-bit alpha, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB_S3TC_DXT1_EXT       -> Just Vk.FORMAT_BC1_RGB_SRGB_BLOCK  -- line through 3D space, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT -> Just Vk.FORMAT_BC1_RGBA_SRGB_BLOCK -- line through 3D space plus 1-bit alpha, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT -> Just Vk.FORMAT_BC2_SRGB_BLOCK      -- line through 3D space plus line through 1D space, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT -> Just Vk.FORMAT_BC3_SRGB_BLOCK      -- line through 3D space plus 4-bit alpha, 4x4 blocks, sRGB

  GL_COMPRESSED_LUMINANCE_LATC1_EXT              -> Just Vk.FORMAT_BC4_UNORM_BLOCK -- line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_LUMINANCE_ALPHA_LATC2_EXT        -> Just Vk.FORMAT_BC5_UNORM_BLOCK -- two lines through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_LUMINANCE_LATC1_EXT       -> Just Vk.FORMAT_BC4_SNORM_BLOCK -- line through 1D space, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_LUMINANCE_ALPHA_LATC2_EXT -> Just Vk.FORMAT_BC5_SNORM_BLOCK -- two lines through 1D space, 4x4 blocks, signed normalized

  GL_COMPRESSED_RED_RGTC1        -> Just Vk.FORMAT_BC4_UNORM_BLOCK -- line through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RG_RGTC2         -> Just Vk.FORMAT_BC5_UNORM_BLOCK -- two lines through 1D space, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_RED_RGTC1 -> Just Vk.FORMAT_BC4_SNORM_BLOCK -- line through 1D space, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_RG_RGTC2  -> Just Vk.FORMAT_BC5_SNORM_BLOCK -- two lines through 1D space, 4x4 blocks, signed normalized

  GL_COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT -> Just Vk.FORMAT_BC6H_UFLOAT_BLOCK -- 3-component, 4x4 blocks, unsigned floating-point
  GL_COMPRESSED_RGB_BPTC_SIGNED_FLOAT   -> Just Vk.FORMAT_BC6H_SFLOAT_BLOCK -- 3-component, 4x4 blocks, signed floating-point
  GL_COMPRESSED_RGBA_BPTC_UNORM         -> Just Vk.FORMAT_BC7_UNORM_BLOCK   -- 4-component, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SRGB_ALPHA_BPTC_UNORM   -> Just Vk.FORMAT_BC7_SRGB_BLOCK    -- 4-component, 4x4 blocks, sRGB

  --
  -- ETC
  --
  GL_ETC1_RGB8_OES -> Just Vk.FORMAT_ETC2_R8G8B8_UNORM_BLOCK -- 3-component ETC1, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_RGB8_ETC2                     -> Just Vk.FORMAT_ETC2_R8G8B8_UNORM_BLOCK   -- 3-component ETC2, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 -> Just Vk.FORMAT_ETC2_R8G8B8A1_UNORM_BLOCK -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA8_ETC2_EAC                -> Just Vk.FORMAT_ETC2_R8G8B8A8_UNORM_BLOCK -- 4-component ETC2, 4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ETC2                     -> Just Vk.FORMAT_ETC2_R8G8B8_SRGB_BLOCK   -- 3-component ETC2, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 -> Just Vk.FORMAT_ETC2_R8G8B8A1_SRGB_BLOCK -- 4-component ETC2 with 1-bit alpha, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ETC2_EAC          -> Just Vk.FORMAT_ETC2_R8G8B8A8_SRGB_BLOCK -- 4-component ETC2, 4x4 blocks, sRGB

  GL_COMPRESSED_R11_EAC         -> Just Vk.FORMAT_EAC_R11_UNORM_BLOCK    -- 1-component ETC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RG11_EAC        -> Just Vk.FORMAT_EAC_R11G11_UNORM_BLOCK -- 2-component ETC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_SIGNED_R11_EAC  -> Just Vk.FORMAT_EAC_R11_SNORM_BLOCK    -- 1-component ETC, 4x4 blocks, signed normalized
  GL_COMPRESSED_SIGNED_RG11_EAC -> Just Vk.FORMAT_EAC_R11G11_SNORM_BLOCK -- 2-component ETC, 4x4 blocks, signed normalized

  --
  -- PVRTC
  --
  GL_COMPRESSED_RGB_PVRTC_2BPPV1_IMG  -> Just Vk.FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG -- 3-component PVRTC, 16x8 blocks, unsigned normalized
  GL_COMPRESSED_RGB_PVRTC_4BPPV1_IMG  -> Just Vk.FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG -- 3-component PVRTC,  8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_2BPPV1_IMG -> Just Vk.FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG -- 4-component PVRTC, 16x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_4BPPV1_IMG -> Just Vk.FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG -- 4-component PVRTC,  8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_2BPPV2_IMG -> Just Vk.FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG -- 4-component PVRTC,  8x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_PVRTC_4BPPV2_IMG -> Just Vk.FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG -- 4-component PVRTC,  4x4 blocks, unsigned normalized

  GL_COMPRESSED_SRGB_PVRTC_2BPPV1_EXT       -> Just Vk.FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG -- 3-component PVRTC, 16x8 blocks, sRGB
  GL_COMPRESSED_SRGB_PVRTC_4BPPV1_EXT       -> Just Vk.FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG -- 3-component PVRTC,  8x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV1_EXT -> Just Vk.FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG -- 4-component PVRTC, 16x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV1_EXT -> Just Vk.FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG -- 4-component PVRTC,  8x8 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_2BPPV2_IMG -> Just Vk.FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG -- 4-component PVRTC,  8x4 blocks, sRGB
  GL_COMPRESSED_SRGB_ALPHA_PVRTC_4BPPV2_IMG -> Just Vk.FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG -- 4-component PVRTC,  4x4 blocks, sRGB

  --
  -- ASTC
  --
  GL_COMPRESSED_RGBA_ASTC_4x4_KHR   -> Just Vk.FORMAT_ASTC_4x4_UNORM_BLOCK   -- 4-component ASTC, 4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x4_KHR   -> Just Vk.FORMAT_ASTC_5x4_UNORM_BLOCK   -- 4-component ASTC, 5x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5_KHR   -> Just Vk.FORMAT_ASTC_5x5_UNORM_BLOCK   -- 4-component ASTC, 5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x5_KHR   -> Just Vk.FORMAT_ASTC_6x5_UNORM_BLOCK   -- 4-component ASTC, 6x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6_KHR   -> Just Vk.FORMAT_ASTC_6x6_UNORM_BLOCK   -- 4-component ASTC, 6x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x5_KHR   -> Just Vk.FORMAT_ASTC_8x5_UNORM_BLOCK   -- 4-component ASTC, 8x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x6_KHR   -> Just Vk.FORMAT_ASTC_8x6_UNORM_BLOCK   -- 4-component ASTC, 8x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_8x8_KHR   -> Just Vk.FORMAT_ASTC_8x8_UNORM_BLOCK   -- 4-component ASTC, 8x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x5_KHR  -> Just Vk.FORMAT_ASTC_10x5_UNORM_BLOCK  -- 4-component ASTC, 10x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x6_KHR  -> Just Vk.FORMAT_ASTC_10x6_UNORM_BLOCK  -- 4-component ASTC, 10x6 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x8_KHR  -> Just Vk.FORMAT_ASTC_10x8_UNORM_BLOCK  -- 4-component ASTC, 10x8 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_10x10_KHR -> Just Vk.FORMAT_ASTC_10x10_UNORM_BLOCK -- 4-component ASTC, 10x10 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_12x10_KHR -> Just Vk.FORMAT_ASTC_12x10_UNORM_BLOCK -- 4-component ASTC, 12x10 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_12x12_KHR -> Just Vk.FORMAT_ASTC_12x12_UNORM_BLOCK -- 4-component ASTC, 12x12 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR   -> Just Vk.FORMAT_ASTC_4x4_SRGB_BLOCK   -- 4-component ASTC, 4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR   -> Just Vk.FORMAT_ASTC_5x4_SRGB_BLOCK   -- 4-component ASTC, 5x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR   -> Just Vk.FORMAT_ASTC_5x5_SRGB_BLOCK   -- 4-component ASTC, 5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR   -> Just Vk.FORMAT_ASTC_6x5_SRGB_BLOCK   -- 4-component ASTC, 6x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR   -> Just Vk.FORMAT_ASTC_6x6_SRGB_BLOCK   -- 4-component ASTC, 6x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR   -> Just Vk.FORMAT_ASTC_8x5_SRGB_BLOCK   -- 4-component ASTC, 8x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR   -> Just Vk.FORMAT_ASTC_8x6_SRGB_BLOCK   -- 4-component ASTC, 8x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR   -> Just Vk.FORMAT_ASTC_8x8_SRGB_BLOCK   -- 4-component ASTC, 8x8 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR  -> Just Vk.FORMAT_ASTC_10x5_SRGB_BLOCK  -- 4-component ASTC, 10x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR  -> Just Vk.FORMAT_ASTC_10x6_SRGB_BLOCK  -- 4-component ASTC, 10x6 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR  -> Just Vk.FORMAT_ASTC_10x8_SRGB_BLOCK  -- 4-component ASTC, 10x8 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR -> Just Vk.FORMAT_ASTC_10x10_SRGB_BLOCK -- 4-component ASTC, 10x10 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR -> Just Vk.FORMAT_ASTC_12x10_SRGB_BLOCK -- 4-component ASTC, 12x10 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR -> Just Vk.FORMAT_ASTC_12x12_SRGB_BLOCK -- 4-component ASTC, 12x12 blocks, sRGB

  GL_COMPRESSED_RGBA_ASTC_3x3x3_OES -> Nothing -- 4-component ASTC, 3x3x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x3x3_OES -> Nothing -- 4-component ASTC, 4x3x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x4x3_OES -> Nothing -- 4-component ASTC, 4x4x3 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_4x4x4_OES -> Nothing -- 4-component ASTC, 4x4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x4x4_OES -> Nothing -- 4-component ASTC, 5x4x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5x4_OES -> Nothing -- 4-component ASTC, 5x5x4 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_5x5x5_OES -> Nothing -- 4-component ASTC, 5x5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x5x5_OES -> Nothing -- 4-component ASTC, 6x5x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6x5_OES -> Nothing -- 4-component ASTC, 6x6x5 blocks, unsigned normalized
  GL_COMPRESSED_RGBA_ASTC_6x6x6_OES -> Nothing -- 4-component ASTC, 6x6x6 blocks, unsigned normalized

  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_3x3x3_OES -> Nothing -- 4-component ASTC, 3x3x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x3x3_OES -> Nothing -- 4-component ASTC, 4x3x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x3_OES -> Nothing -- 4-component ASTC, 4x4x3 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_4x4x4_OES -> Nothing -- 4-component ASTC, 4x4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x4x4_OES -> Nothing -- 4-component ASTC, 5x4x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x4_OES -> Nothing -- 4-component ASTC, 5x5x4 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_5x5x5_OES -> Nothing -- 4-component ASTC, 5x5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x5x5_OES -> Nothing -- 4-component ASTC, 6x5x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x5_OES -> Nothing -- 4-component ASTC, 6x6x5 blocks, sRGB
  GL_COMPRESSED_SRGB8_ALPHA8_ASTC_6x6x6_OES -> Nothing -- 4-component ASTC, 6x6x6 blocks, sRGB

  --
  -- ATC
  --
  GL_ATC_RGB_AMD                     -> Nothing -- 3-component, 4x4 blocks, unsigned normalized
  GL_ATC_RGBA_EXPLICIT_ALPHA_AMD     -> Nothing -- 4-component, 4x4 blocks, unsigned normalized
  GL_ATC_RGBA_INTERPOLATED_ALPHA_AMD -> Nothing -- 4-component, 4x4 blocks, unsigned normalized

  --
  -- Palletized
  --
  GL_PALETTE4_RGB8_OES     -> Nothing -- 3-component 8:8:8,   4-bit palette, unsigned normalized
  GL_PALETTE4_RGBA8_OES    -> Nothing -- 4-component 8:8:8:8, 4-bit palette, unsigned normalized
  GL_PALETTE4_R5_G6_B5_OES -> Nothing -- 3-component 5:6:5,   4-bit palette, unsigned normalized
  GL_PALETTE4_RGBA4_OES    -> Nothing -- 4-component 4:4:4:4, 4-bit palette, unsigned normalized
  GL_PALETTE4_RGB5_A1_OES  -> Nothing -- 4-component 5:5:5:1, 4-bit palette, unsigned normalized
  GL_PALETTE8_RGB8_OES     -> Nothing -- 3-component 8:8:8,   8-bit palette, unsigned normalized
  GL_PALETTE8_RGBA8_OES    -> Nothing -- 4-component 8:8:8:8, 8-bit palette, unsigned normalized
  GL_PALETTE8_R5_G6_B5_OES -> Nothing -- 3-component 5:6:5,   8-bit palette, unsigned normalized
  GL_PALETTE8_RGBA4_OES    -> Nothing -- 4-component 4:4:4:4, 8-bit palette, unsigned normalized
  GL_PALETTE8_RGB5_A1_OES  -> Nothing -- 4-component 5:5:5:1, 8-bit palette, unsigned normalized

  --
  -- Depth/stencil
  --
  GL_DEPTH_COMPONENT16     -> Just Vk.FORMAT_D16_UNORM
  GL_DEPTH_COMPONENT24     -> Just Vk.FORMAT_X8_D24_UNORM_PACK32
  GL_DEPTH_COMPONENT32     -> Nothing
  GL_DEPTH_COMPONENT32F    -> Just Vk.FORMAT_D32_SFLOAT
  GL_DEPTH_COMPONENT32F_NV -> Just Vk.FORMAT_D32_SFLOAT
  GL_STENCIL_INDEX1        -> Nothing
  GL_STENCIL_INDEX4        -> Nothing
  GL_STENCIL_INDEX8        -> Just Vk.FORMAT_S8_UINT
  GL_STENCIL_INDEX16       -> Nothing
  GL_DEPTH24_STENCIL8      -> Just Vk.FORMAT_D24_UNORM_S8_UINT
  GL_DEPTH32F_STENCIL8     -> Just Vk.FORMAT_D32_SFLOAT_S8_UINT
  GL_DEPTH32F_STENCIL8_NV  -> Just Vk.FORMAT_D32_SFLOAT_S8_UINT

  _ -> Nothing
