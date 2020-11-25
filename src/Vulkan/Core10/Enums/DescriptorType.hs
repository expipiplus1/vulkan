{-# language CPP #-}
-- No documentation found for Chapter "DescriptorType"
module Vulkan.Core10.Enums.DescriptorType  (DescriptorType( DESCRIPTOR_TYPE_SAMPLER
                                                          , DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER
                                                          , DESCRIPTOR_TYPE_SAMPLED_IMAGE
                                                          , DESCRIPTOR_TYPE_STORAGE_IMAGE
                                                          , DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER
                                                          , DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER
                                                          , DESCRIPTOR_TYPE_UNIFORM_BUFFER
                                                          , DESCRIPTOR_TYPE_STORAGE_BUFFER
                                                          , DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC
                                                          , DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC
                                                          , DESCRIPTOR_TYPE_INPUT_ATTACHMENT
                                                          , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
                                                          , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
                                                          , DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
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
-- No documentation found for TopLevel "VkDescriptorType"
newtype DescriptorType = DescriptorType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLER"
pattern DESCRIPTOR_TYPE_SAMPLER                    = DescriptorType 0
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER     = DescriptorType 1
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE              = DescriptorType 2
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE              = DescriptorType 3
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER       = DescriptorType 4
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER       = DescriptorType 5
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER             = DescriptorType 6
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER             = DescriptorType 7
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC     = DescriptorType 8
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC     = DescriptorType 9
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT           = DescriptorType 10
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV  = DescriptorType 1000165000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR = DescriptorType 1000150000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT   = DescriptorType 1000138000
{-# complete DESCRIPTOR_TYPE_SAMPLER,
             DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER,
             DESCRIPTOR_TYPE_SAMPLED_IMAGE,
             DESCRIPTOR_TYPE_STORAGE_IMAGE,
             DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER,
             DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER,
             DESCRIPTOR_TYPE_UNIFORM_BUFFER,
             DESCRIPTOR_TYPE_STORAGE_BUFFER,
             DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC,
             DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC,
             DESCRIPTOR_TYPE_INPUT_ATTACHMENT,
             DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV,
             DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR,
             DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT :: DescriptorType #-}

conNameDescriptorType :: String
conNameDescriptorType = "DescriptorType"

enumPrefixDescriptorType :: String
enumPrefixDescriptorType = "DESCRIPTOR_TYPE_"

showTableDescriptorType :: [(DescriptorType, String)]
showTableDescriptorType =
  [ (DESCRIPTOR_TYPE_SAMPLER                   , "SAMPLER")
  , (DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER    , "COMBINED_IMAGE_SAMPLER")
  , (DESCRIPTOR_TYPE_SAMPLED_IMAGE             , "SAMPLED_IMAGE")
  , (DESCRIPTOR_TYPE_STORAGE_IMAGE             , "STORAGE_IMAGE")
  , (DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER      , "UNIFORM_TEXEL_BUFFER")
  , (DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER      , "STORAGE_TEXEL_BUFFER")
  , (DESCRIPTOR_TYPE_UNIFORM_BUFFER            , "UNIFORM_BUFFER")
  , (DESCRIPTOR_TYPE_STORAGE_BUFFER            , "STORAGE_BUFFER")
  , (DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC    , "UNIFORM_BUFFER_DYNAMIC")
  , (DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC    , "STORAGE_BUFFER_DYNAMIC")
  , (DESCRIPTOR_TYPE_INPUT_ATTACHMENT          , "INPUT_ATTACHMENT")
  , (DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV , "ACCELERATION_STRUCTURE_NV")
  , (DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR, "ACCELERATION_STRUCTURE_KHR")
  , (DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT  , "INLINE_UNIFORM_BLOCK_EXT")
  ]


instance Show DescriptorType where
showsPrec = enumShowsPrec enumPrefixDescriptorType
                          showTableDescriptorType
                          conNameDescriptorType
                          (\(DescriptorType x) -> x)
                          (showsPrec 11)


instance Read DescriptorType where
  readPrec = enumReadPrec enumPrefixDescriptorType showTableDescriptorType conNameDescriptorType DescriptorType

