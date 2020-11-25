{-# language CPP #-}
-- No documentation found for Chapter "ImageCreateFlagBits"
module Vulkan.Core10.Enums.ImageCreateFlagBits  ( ImageCreateFlags
                                                , ImageCreateFlagBits( IMAGE_CREATE_SPARSE_BINDING_BIT
                                                                     , IMAGE_CREATE_SPARSE_RESIDENCY_BIT
                                                                     , IMAGE_CREATE_SPARSE_ALIASED_BIT
                                                                     , IMAGE_CREATE_MUTABLE_FORMAT_BIT
                                                                     , IMAGE_CREATE_CUBE_COMPATIBLE_BIT
                                                                     , IMAGE_CREATE_SUBSAMPLED_BIT_EXT
                                                                     , IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
                                                                     , IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
                                                                     , IMAGE_CREATE_DISJOINT_BIT
                                                                     , IMAGE_CREATE_PROTECTED_BIT
                                                                     , IMAGE_CREATE_EXTENDED_USAGE_BIT
                                                                     , IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
                                                                     , IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
                                                                     , IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
                                                                     , IMAGE_CREATE_ALIAS_BIT
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
type ImageCreateFlags = ImageCreateFlagBits

-- No documentation found for TopLevel "VkImageCreateFlagBits"
newtype ImageCreateFlagBits = ImageCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
pattern IMAGE_CREATE_SPARSE_BINDING_BIT              = ImageCreateFlagBits 0x00000001
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT            = ImageCreateFlagBits 0x00000002
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT              = ImageCreateFlagBits 0x00000004
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT              = ImageCreateFlagBits 0x00000008
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT             = ImageCreateFlagBits 0x00000010
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT              = ImageCreateFlagBits 0x00004000
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = ImageCreateFlagBits 0x00001000
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV           = ImageCreateFlagBits 0x00002000
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_DISJOINT_BIT"
pattern IMAGE_CREATE_DISJOINT_BIT                    = ImageCreateFlagBits 0x00000200
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_PROTECTED_BIT"
pattern IMAGE_CREATE_PROTECTED_BIT                   = ImageCreateFlagBits 0x00000800
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT              = ImageCreateFlagBits 0x00000100
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000080
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT         = ImageCreateFlagBits 0x00000020
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = ImageCreateFlagBits 0x00000040
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_ALIAS_BIT"
pattern IMAGE_CREATE_ALIAS_BIT                       = ImageCreateFlagBits 0x00000400

conNameImageCreateFlagBits :: String
conNameImageCreateFlagBits = "ImageCreateFlagBits"

enumPrefixImageCreateFlagBits :: String
enumPrefixImageCreateFlagBits = "IMAGE_CREATE_"

showTableImageCreateFlagBits :: [(ImageCreateFlagBits, String)]
showTableImageCreateFlagBits =
  [ (IMAGE_CREATE_SPARSE_BINDING_BIT             , "SPARSE_BINDING_BIT")
  , (IMAGE_CREATE_SPARSE_RESIDENCY_BIT           , "SPARSE_RESIDENCY_BIT")
  , (IMAGE_CREATE_SPARSE_ALIASED_BIT             , "SPARSE_ALIASED_BIT")
  , (IMAGE_CREATE_MUTABLE_FORMAT_BIT             , "MUTABLE_FORMAT_BIT")
  , (IMAGE_CREATE_CUBE_COMPATIBLE_BIT            , "CUBE_COMPATIBLE_BIT")
  , (IMAGE_CREATE_SUBSAMPLED_BIT_EXT             , "SUBSAMPLED_BIT_EXT")
  , (IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT, "SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT")
  , (IMAGE_CREATE_CORNER_SAMPLED_BIT_NV          , "CORNER_SAMPLED_BIT_NV")
  , (IMAGE_CREATE_DISJOINT_BIT                   , "DISJOINT_BIT")
  , (IMAGE_CREATE_PROTECTED_BIT                  , "PROTECTED_BIT")
  , (IMAGE_CREATE_EXTENDED_USAGE_BIT             , "EXTENDED_USAGE_BIT")
  , (IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT, "BLOCK_TEXEL_VIEW_COMPATIBLE_BIT")
  , (IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT        , "2D_ARRAY_COMPATIBLE_BIT")
  , (IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT, "SPLIT_INSTANCE_BIND_REGIONS_BIT")
  , (IMAGE_CREATE_ALIAS_BIT                      , "ALIAS_BIT")
  ]


instance Show ImageCreateFlagBits where
showsPrec = enumShowsPrec enumPrefixImageCreateFlagBits
                          showTableImageCreateFlagBits
                          conNameImageCreateFlagBits
                          (\(ImageCreateFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ImageCreateFlagBits where
  readPrec = enumReadPrec enumPrefixImageCreateFlagBits
                          showTableImageCreateFlagBits
                          conNameImageCreateFlagBits
                          ImageCreateFlagBits

