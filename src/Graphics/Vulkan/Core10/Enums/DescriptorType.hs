{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.DescriptorType  (DescriptorType( DESCRIPTOR_TYPE_SAMPLER
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
                                                                   , DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT
                                                                   , ..
                                                                   )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
-- | VkDescriptorType - Specifies the type of a descriptor in a descriptor
-- set
--
-- = Description
--
-- -   'DESCRIPTOR_TYPE_SAMPLER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampler sampler descriptor>.
--
-- -   'DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-combinedimagesampler combined image sampler descriptor>.
--
-- -   'DESCRIPTOR_TYPE_SAMPLED_IMAGE' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-sampledimage sampled image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_IMAGE' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storageimage storage image descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformtexelbuffer uniform texel buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbuffer uniform buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_BUFFER' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebuffer storage buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-uniformbufferdynamic dynamic uniform buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC' specifies a
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-storagebufferdynamic dynamic storage buffer descriptor>.
--
-- -   'DESCRIPTOR_TYPE_INPUT_ATTACHMENT' specifies an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inputattachment input attachment descriptor>.
--
-- -   'DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT' specifies an
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#descriptorsets-inlineuniformblock inline uniform block>.
--
-- When a descriptor set is updated via elements of
-- 'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet', members of
-- @pImageInfo@, @pBufferInfo@ and @pTexelBufferView@ are only accessed by
-- the implementation when they correspond to descriptor type being defined
-- - otherwise they are ignored. The members accessed are as follows for
-- each descriptor type:
--
-- -   For 'DESCRIPTOR_TYPE_SAMPLER', only the
--     'Graphics.Vulkan.Core10.Handles.Sampler' member of each element of
--     'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pImageInfo@
--     is accessed.
--
-- -   For 'DESCRIPTOR_TYPE_SAMPLED_IMAGE',
--     'DESCRIPTOR_TYPE_STORAGE_IMAGE', or
--     'DESCRIPTOR_TYPE_INPUT_ATTACHMENT', only the
--     'Graphics.Vulkan.Core10.Handles.ImageView' and
--     'Graphics.Vulkan.Core10.Enums.ImageLayout.ImageLayout' members of
--     each element of
--     'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pImageInfo@
--     are accessed.
--
-- -   For 'DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER', all members of each
--     element of
--     'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pImageInfo@
--     are accessed.
--
-- -   For 'DESCRIPTOR_TYPE_UNIFORM_BUFFER',
--     'DESCRIPTOR_TYPE_STORAGE_BUFFER',
--     'DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC', or
--     'DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC', all members of each
--     element of
--     'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pBufferInfo@
--     are accessed.
--
-- -   For 'DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER' or
--     'DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER', each element of
--     'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'::@pTexelBufferView@
--     is accessed.
--
-- When updating descriptors with a 'DescriptorType' of
-- 'DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT', none of the @pImageInfo@,
-- @pBufferInfo@, or @pTexelBufferView@ members are accessed, instead the
-- source data of the descriptor update operation is taken from the
-- 'Graphics.Vulkan.Extensions.VK_EXT_inline_uniform_block.WriteDescriptorSetInlineUniformBlockEXT'
-- structure in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'. When updating
-- descriptors with a 'DescriptorType' of
-- 'DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV', none of the @pImageInfo@,
-- @pBufferInfo@, or @pTexelBufferView@ members are accessed, instead the
-- source data of the descriptor update operation is taken from the
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.WriteDescriptorSetAccelerationStructureNV'
-- structure in the @pNext@ chain of
-- 'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorPoolSize',
-- 'Graphics.Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding',
-- 'Graphics.Vulkan.Core11.Promoted_From_VK_KHR_descriptor_update_template.DescriptorUpdateTemplateEntry',
-- 'Graphics.Vulkan.Extensions.VK_NVX_image_view_handle.ImageViewHandleInfoNVX',
-- 'Graphics.Vulkan.Core10.DescriptorSet.WriteDescriptorSet'
newtype DescriptorType = DescriptorType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLER"
pattern DESCRIPTOR_TYPE_SAMPLER = DescriptorType 0
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
pattern DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER = DescriptorType 1
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_SAMPLED_IMAGE"
pattern DESCRIPTOR_TYPE_SAMPLED_IMAGE = DescriptorType 2
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_IMAGE"
pattern DESCRIPTOR_TYPE_STORAGE_IMAGE = DescriptorType 3
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER = DescriptorType 4
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER = DescriptorType 5
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER = DescriptorType 6
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER = DescriptorType 7
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC = DescriptorType 8
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
pattern DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC = DescriptorType 9
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
pattern DESCRIPTOR_TYPE_INPUT_ATTACHMENT = DescriptorType 10
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
pattern DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV = DescriptorType 1000165000
-- No documentation found for Nested "VkDescriptorType" "VK_DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
pattern DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT = DescriptorType 1000138000
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
             DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT :: DescriptorType #-}

instance Show DescriptorType where
  showsPrec p = \case
    DESCRIPTOR_TYPE_SAMPLER -> showString "DESCRIPTOR_TYPE_SAMPLER"
    DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER -> showString "DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER"
    DESCRIPTOR_TYPE_SAMPLED_IMAGE -> showString "DESCRIPTOR_TYPE_SAMPLED_IMAGE"
    DESCRIPTOR_TYPE_STORAGE_IMAGE -> showString "DESCRIPTOR_TYPE_STORAGE_IMAGE"
    DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER -> showString "DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER"
    DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER -> showString "DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER"
    DESCRIPTOR_TYPE_UNIFORM_BUFFER -> showString "DESCRIPTOR_TYPE_UNIFORM_BUFFER"
    DESCRIPTOR_TYPE_STORAGE_BUFFER -> showString "DESCRIPTOR_TYPE_STORAGE_BUFFER"
    DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC -> showString "DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC"
    DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC -> showString "DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC"
    DESCRIPTOR_TYPE_INPUT_ATTACHMENT -> showString "DESCRIPTOR_TYPE_INPUT_ATTACHMENT"
    DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV -> showString "DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV"
    DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT -> showString "DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT"
    DescriptorType x -> showParen (p >= 11) (showString "DescriptorType " . showsPrec 11 x)

instance Read DescriptorType where
  readPrec = parens (choose [("DESCRIPTOR_TYPE_SAMPLER", pure DESCRIPTOR_TYPE_SAMPLER)
                            , ("DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER", pure DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER)
                            , ("DESCRIPTOR_TYPE_SAMPLED_IMAGE", pure DESCRIPTOR_TYPE_SAMPLED_IMAGE)
                            , ("DESCRIPTOR_TYPE_STORAGE_IMAGE", pure DESCRIPTOR_TYPE_STORAGE_IMAGE)
                            , ("DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER", pure DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER)
                            , ("DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER", pure DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER)
                            , ("DESCRIPTOR_TYPE_UNIFORM_BUFFER", pure DESCRIPTOR_TYPE_UNIFORM_BUFFER)
                            , ("DESCRIPTOR_TYPE_STORAGE_BUFFER", pure DESCRIPTOR_TYPE_STORAGE_BUFFER)
                            , ("DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC", pure DESCRIPTOR_TYPE_UNIFORM_BUFFER_DYNAMIC)
                            , ("DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC", pure DESCRIPTOR_TYPE_STORAGE_BUFFER_DYNAMIC)
                            , ("DESCRIPTOR_TYPE_INPUT_ATTACHMENT", pure DESCRIPTOR_TYPE_INPUT_ATTACHMENT)
                            , ("DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV", pure DESCRIPTOR_TYPE_ACCELERATION_STRUCTURE_NV)
                            , ("DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT", pure DESCRIPTOR_TYPE_INLINE_UNIFORM_BLOCK_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "DescriptorType")
                       v <- step readPrec
                       pure (DescriptorType v)))

