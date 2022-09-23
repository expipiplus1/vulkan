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
                                                          , DESCRIPTOR_TYPE_MUTABLE_EXT
                                                          , DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM
                                                          , DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM
                                                          , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV
                                                          , DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR
                                                          , DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK
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

-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   'DESCRIPTOR_TYPE_SAMPLER' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-sampler sampler descriptor>.
--
-- -   'DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-combinedimagesampler combined image sampler descriptor>.
--
-- -   'DESCRIPTOR_TYPE_SAMPLED_IMAGE' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_IMAGE' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_BUFFER' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic dynamic uniform buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic dynamic storage buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_INPUT_ATTACHMENT' specifies an
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-inputattachment input attachment descriptor>.
--
-- -   'DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK' specifies an
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>.
--
-- -   'DESCRIPTOR_TYPE_MUTABLE_EXT' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-mutable descriptor of mutable type>.
--
-- -   'DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-weightimage sampled weight image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM' specifies a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-blockmatch block matching image descriptor>.
--
-- When a descriptor set is updated via elements of
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet', members of
-- @pImageInfo@, @pBufferInfo@ and @pTexelBufferView@ are only accessed by
-- the implementation when they correspond to descriptor type being defined
-- - otherwise they are ignored. The members accessed are as follows for
-- each descriptor type:
--
-- -   For 'DESCRIPTOR_TYPE_SAMPLER', only the @sampler@ member of each
--     element of
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pImageInfo@ is
--     accessed.
--
-- -   For 'DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'DESCRIPTOR_TYPE_STORAGE_IMAGE', or
--     'DESCRIPTOR_TYPE_INPUT_ATTACHMENT', only the @imageView@ and
--     @imageLayout@ members of each element of
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pImageInfo@ are
--     accessed.
--
-- -   For 'DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', all members of each
--     element of
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pImageInfo@ are
--     accessed.
--
-- -   For 'DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', or
--     'DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', all members of each
--     element of
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pBufferInfo@ are
--     accessed.
--
-- -   For 'DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' or
--     'DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER', each element of
--     'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pTexelBufferView@
--     is accessed.
--
-- When updating descriptors with a @descriptorType@ of
-- 'DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK', none of the @pImageInfo@,
-- @pBufferInfo@, or @pTexelBufferView@ members are accessed, instead the
-- source data of the descriptor update operation is taken from the
-- 'Vulkan.Core13.Promoted_From_VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlock'
-- structure in the @pNext@ chain of
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'. When updating
-- descriptors with a @descriptorType@ of
-- 'DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR', none of the @pImageInfo@,
-- @pBufferInfo@, or @pTexelBufferView@ members are accessed, instead the
-- source data of the descriptor update operation is taken from the
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.WriteDescriptorSetAccelerationStructureKHR'
-- structure in the @pNext@ chain of
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'. When updating
-- descriptors with a @descriptorType@ of
-- 'DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV', none of the @pImageInfo@,
-- @pBufferInfo@, or @pTexelBufferView@ members are accessed, instead the
-- source data of the descriptor update operation is taken from the
-- 'Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV'
-- structure in the @pNext@ chain of
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.DescriptorSet.DescriptorPoolSize',
-- 'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry',
-- 'Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Vulkan.Extensions.VK_EXT_mutable_descriptor_type.MutableDescriptorTypeListEXT',
-- 'Vulkan.Core10.DescriptorSet.WriteDescriptorSet'
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
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_MUTABLE_EXT"
pattern DESCRIPTOR_TYPE_MUTABLE_EXT                = DescriptorType 1000351000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM"
pattern DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM     = DescriptorType 1000440001
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM"
pattern DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM   = DescriptorType 1000440000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV  = DescriptorType 1000165000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR = DescriptorType 1000150000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK       = DescriptorType 1000138000
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
             DESCRIPTOR_TYPE_MUTABLE_EXT,
             DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM,
             DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM,
             DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV,
             DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR,
             DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK :: DescriptorType #-}

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
  , (DESCRIPTOR_TYPE_MUTABLE_EXT               , "MUTABLE_EXT")
  , (DESCRIPTOR_TYPE_BLOCK_MATCH_IMAGE_QCOM    , "BLOCK_MATCH_IMAGE_QCOM")
  , (DESCRIPTOR_TYPE_SAMPLE_WEIGHT_IMAGE_QCOM  , "SAMPLE_WEIGHT_IMAGE_QCOM")
  , (DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV , "ACCELERATION_STRUCTURE_NV")
  , (DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_KHR, "ACCELERATION_STRUCTURE_KHR")
  , (DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK      , "INLINE_UNIFORM_BLOCK")
  ]

instance Show DescriptorType where
  showsPrec = enumShowsPrec enumPrefixDescriptorType
                            showTableDescriptorType
                            conNameDescriptorType
                            (\(DescriptorType x) -> x)
                            (showsPrec 11)

instance Read DescriptorType where
  readPrec = enumReadPrec enumPrefixDescriptorType showTableDescriptorType conNameDescriptorType DescriptorType

