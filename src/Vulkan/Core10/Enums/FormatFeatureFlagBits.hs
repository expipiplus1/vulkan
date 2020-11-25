{-# language CPP #-}
-- No documentation found for Chapter "FormatFeatureFlagBits"
module Vulkan.Core10.Enums.FormatFeatureFlagBits  ( FormatFeatureFlags
                                                  , FormatFeatureFlagBits( FORMAT_FEATURE_SAMPLED_IMAGE_BIT
                                                                         , FORMAT_FEATURE_STORAGE_IMAGE_BIT
                                                                         , FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT
                                                                         , FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT
                                                                         , FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT
                                                                         , FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
                                                                         , FORMAT_FEATURE_VERTEX_BUFFER_BIT
                                                                         , FORMAT_FEATURE_COLOR_ATTACHMENT_BIT
                                                                         , FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT
                                                                         , FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                         , FORMAT_FEATURE_BLIT_SRC_BIT
                                                                         , FORMAT_FEATURE_BLIT_DST_BIT
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT
                                                                         , FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                         , FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT
                                                                         , FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT
                                                                         , FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT
                                                                         , FORMAT_FEATURE_DISJOINT_BIT
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
                                                                         , FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
                                                                         , FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT
                                                                         , FORMAT_FEATURE_TRANSFER_DST_BIT
                                                                         , FORMAT_FEATURE_TRANSFER_SRC_BIT
                                                                         , ..
                                                                         )
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type FormatFeatureFlags = FormatFeatureFlagBits

-- No documentation found for TopLevel "VkFormatFeatureFlagBits"
newtype FormatFeatureFlagBits = FormatFeatureFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_BIT                        = FormatFeatureFlagBits 0x00000001
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_IMAGE_BIT"
pattern FORMAT_FEATURE_STORAGE_IMAGE_BIT                        = FormatFeatureFlagBits 0x00000002
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT"
pattern FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT                 = FormatFeatureFlagBits 0x00000004
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT"
pattern FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT                 = FormatFeatureFlagBits 0x00000008
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT"
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT                 = FormatFeatureFlagBits 0x00000010
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
pattern FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT          = FormatFeatureFlagBits 0x00000020
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_VERTEX_BUFFER_BIT"
pattern FORMAT_FEATURE_VERTEX_BUFFER_BIT                        = FormatFeatureFlagBits 0x00000040
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BIT"
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BIT                     = FormatFeatureFlagBits 0x00000080
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT"
pattern FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT               = FormatFeatureFlagBits 0x00000100
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT             = FormatFeatureFlagBits 0x00000200
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_BLIT_SRC_BIT"
pattern FORMAT_FEATURE_BLIT_SRC_BIT                             = FormatFeatureFlagBits 0x00000400
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_BLIT_DST_BIT"
pattern FORMAT_FEATURE_BLIT_DST_BIT                             = FormatFeatureFlagBits 0x00000800
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT          = FormatFeatureFlagBits 0x00001000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = FormatFeatureFlagBits 0x40000000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT             = FormatFeatureFlagBits 0x01000000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR"
pattern FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR = FormatFeatureFlagBits 0x20000000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG       = FormatFeatureFlagBits 0x00002000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT          = FormatFeatureFlagBits 0x00010000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT"
pattern FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT               = FormatFeatureFlagBits 0x00800000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_DISJOINT_BIT"
pattern FORMAT_FEATURE_DISJOINT_BIT                             = FormatFeatureFlagBits 0x00400000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT =
  FormatFeatureFlagBits 0x00200000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT =
  FormatFeatureFlagBits 0x00100000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT =
  FormatFeatureFlagBits 0x00080000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
pattern FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = FormatFeatureFlagBits 0x00040000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT"
pattern FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT = FormatFeatureFlagBits 0x00020000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_TRANSFER_DST_BIT"
pattern FORMAT_FEATURE_TRANSFER_DST_BIT            = FormatFeatureFlagBits 0x00008000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_TRANSFER_SRC_BIT"
pattern FORMAT_FEATURE_TRANSFER_SRC_BIT            = FormatFeatureFlagBits 0x00004000

conNameFormatFeatureFlagBits :: String
conNameFormatFeatureFlagBits = "FormatFeatureFlagBits"

enumPrefixFormatFeatureFlagBits :: String
enumPrefixFormatFeatureFlagBits = "FORMAT_FEATURE_"

showTableFormatFeatureFlagBits :: [(FormatFeatureFlagBits, String)]
showTableFormatFeatureFlagBits =
  [ (FORMAT_FEATURE_SAMPLED_IMAGE_BIT                       , "SAMPLED_IMAGE_BIT")
  , (FORMAT_FEATURE_STORAGE_IMAGE_BIT                       , "STORAGE_IMAGE_BIT")
  , (FORMAT_FEATURE_STORAGE_IMAGE_ATOMIC_BIT                , "STORAGE_IMAGE_ATOMIC_BIT")
  , (FORMAT_FEATURE_UNIFORM_TEXEL_BUFFER_BIT                , "UNIFORM_TEXEL_BUFFER_BIT")
  , (FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_BIT                , "STORAGE_TEXEL_BUFFER_BIT")
  , (FORMAT_FEATURE_STORAGE_TEXEL_BUFFER_ATOMIC_BIT         , "STORAGE_TEXEL_BUFFER_ATOMIC_BIT")
  , (FORMAT_FEATURE_VERTEX_BUFFER_BIT                       , "VERTEX_BUFFER_BIT")
  , (FORMAT_FEATURE_COLOR_ATTACHMENT_BIT                    , "COLOR_ATTACHMENT_BIT")
  , (FORMAT_FEATURE_COLOR_ATTACHMENT_BLEND_BIT              , "COLOR_ATTACHMENT_BLEND_BIT")
  , (FORMAT_FEATURE_DEPTH_STENCIL_ATTACHMENT_BIT            , "DEPTH_STENCIL_ATTACHMENT_BIT")
  , (FORMAT_FEATURE_BLIT_SRC_BIT                            , "BLIT_SRC_BIT")
  , (FORMAT_FEATURE_BLIT_DST_BIT                            , "BLIT_DST_BIT")
  , (FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_LINEAR_BIT         , "SAMPLED_IMAGE_FILTER_LINEAR_BIT")
  , (FORMAT_FEATURE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR, "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR")
  , (FORMAT_FEATURE_FRAGMENT_DENSITY_MAP_BIT_EXT            , "FRAGMENT_DENSITY_MAP_BIT_EXT")
  , (FORMAT_FEATURE_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR, "ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR")
  , (FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG      , "SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG")
  , (FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_MINMAX_BIT         , "SAMPLED_IMAGE_FILTER_MINMAX_BIT")
  , (FORMAT_FEATURE_COSITED_CHROMA_SAMPLES_BIT              , "COSITED_CHROMA_SAMPLES_BIT")
  , (FORMAT_FEATURE_DISJOINT_BIT                            , "DISJOINT_BIT")
  , ( FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
    )
  , ( FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
    )
  , ( FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
    )
  , ( FORMAT_FEATURE_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
    )
  , (FORMAT_FEATURE_MIDPOINT_CHROMA_SAMPLES_BIT, "MIDPOINT_CHROMA_SAMPLES_BIT")
  , (FORMAT_FEATURE_TRANSFER_DST_BIT           , "TRANSFER_DST_BIT")
  , (FORMAT_FEATURE_TRANSFER_SRC_BIT           , "TRANSFER_SRC_BIT")
  ]


instance Show FormatFeatureFlagBits where
showsPrec = enumShowsPrec enumPrefixFormatFeatureFlagBits
                          showTableFormatFeatureFlagBits
                          conNameFormatFeatureFlagBits
                          (\(FormatFeatureFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read FormatFeatureFlagBits where
  readPrec = enumReadPrec enumPrefixFormatFeatureFlagBits
                          showTableFormatFeatureFlagBits
                          conNameFormatFeatureFlagBits
                          FormatFeatureFlagBits

