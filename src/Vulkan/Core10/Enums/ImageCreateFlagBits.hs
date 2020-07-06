{-# language CPP #-}
module Vulkan.Core10.Enums.ImageCreateFlagBits  ( ImageCreateFlagBits( IMAGE_CREATE_SPARSE_BINDING_BIT
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
                                                , ImageCreateFlags
                                                ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkImageCreateFlagBits - Bitmask specifying additional parameters of an
-- image
--
-- = Description
--
-- See
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory-physicalfeatures Sparse Physical Device Features>
-- for more details.
--
-- = See Also
--
-- 'ImageCreateFlags'
newtype ImageCreateFlagBits = ImageCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'IMAGE_CREATE_SPARSE_BINDING_BIT' specifies that the image will be
-- backed using sparse memory binding.
pattern IMAGE_CREATE_SPARSE_BINDING_BIT = ImageCreateFlagBits 0x00000001
-- | 'IMAGE_CREATE_SPARSE_RESIDENCY_BIT' specifies that the image /can/ be
-- partially backed using sparse memory binding. Images created with this
-- flag /must/ also be created with the 'IMAGE_CREATE_SPARSE_BINDING_BIT'
-- flag.
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT = ImageCreateFlagBits 0x00000002
-- | 'IMAGE_CREATE_SPARSE_ALIASED_BIT' specifies that the image will be
-- backed using sparse memory binding with memory ranges that might also
-- simultaneously be backing another image (or another portion of the same
-- image). Images created with this flag /must/ also be created with the
-- 'IMAGE_CREATE_SPARSE_BINDING_BIT' flag
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT = ImageCreateFlagBits 0x00000004
-- | 'IMAGE_CREATE_MUTABLE_FORMAT_BIT' specifies that the image /can/ be used
-- to create a 'Vulkan.Core10.Handles.ImageView' with a different format
-- from the image. For
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar>
-- formats, 'IMAGE_CREATE_MUTABLE_FORMAT_BIT' specifies that a
-- 'Vulkan.Core10.Handles.ImageView' can be created of a /plane/ of the
-- image.
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT = ImageCreateFlagBits 0x00000008
-- | 'IMAGE_CREATE_CUBE_COMPATIBLE_BIT' specifies that the image /can/ be
-- used to create a 'Vulkan.Core10.Handles.ImageView' of type
-- 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE' or
-- 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'.
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000010
-- | 'IMAGE_CREATE_SUBSAMPLED_BIT_EXT' specifies that an image /can/ be in a
-- subsampled format which /may/ be more optimal when written as an
-- attachment by a render pass that has a fragment density map attachment.
-- Accessing a subsampled image has additional considerations:
--
-- -   Image data read as an image sampler is undefined if the sampler was
--     not created with @flags@ containing
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
--     or was not sampled through the use of a combined image sampler with
--     an immutable sampler in
--     'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding'.
--
-- -   Image data read with an input attachment is undefined if the
--     contents were not written as an attachment in an earlier subpass of
--     the same render pass.
--
-- -   Image data read as an image sampler in the fragment shader will be
--     additionally be read by the device during
--     'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_SHADER_BIT'
--     if
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subsampledcoarsereconstructionearlyaccess ::subsampledCoarseReconstructionEarlyAccess>
--     is 'Vulkan.Core10.FundamentalTypes.TRUE' and the sampler was created
--     with @flags@ containing
--     'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT'.
--
-- -   Image data read with load operations are resampled to the fragment
--     density of the render pass if
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#limits-subsampledloads ::subsampledLoads>
--     is 'Vulkan.Core10.FundamentalTypes.TRUE', otherwise they are
--     undefined.
--
-- -   Image contents outside of the render area become undefined if the
--     image is stored as a render pass attachment.
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT = ImageCreateFlagBits 0x00004000
-- | 'IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT' specifies that
-- an image with a depth or depth\/stencil format /can/ be used with custom
-- sample locations when used as a depth\/stencil attachment.
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = ImageCreateFlagBits 0x00001000
-- | 'IMAGE_CREATE_CORNER_SAMPLED_BIT_NV' specifies that the image is a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-images-corner-sampled corner-sampled image>.
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV = ImageCreateFlagBits 0x00002000
-- | 'IMAGE_CREATE_DISJOINT_BIT' specifies that an image with a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-requiring-sampler-ycbcr-conversion multi-planar format>
-- /must/ have each plane separately bound to memory, rather than having a
-- single memory binding for the whole image; the presence of this bit
-- distinguishes a /disjoint image/ from an image without this bit set.
pattern IMAGE_CREATE_DISJOINT_BIT = ImageCreateFlagBits 0x00000200
-- | 'IMAGE_CREATE_PROTECTED_BIT' specifies that the image is a protected
-- image.
pattern IMAGE_CREATE_PROTECTED_BIT = ImageCreateFlagBits 0x00000800
-- | 'IMAGE_CREATE_EXTENDED_USAGE_BIT' specifies that the image /can/ be
-- created with usage flags that are not supported for the format the image
-- is created with but are supported for at least one format a
-- 'Vulkan.Core10.Handles.ImageView' created from the image /can/ have.
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT = ImageCreateFlagBits 0x00000100
-- | 'IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT' specifies that the image
-- having a compressed format /can/ be used to create a
-- 'Vulkan.Core10.Handles.ImageView' with an uncompressed format where each
-- texel in the image view corresponds to a compressed texel block of the
-- image.
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000080
-- | 'IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT' specifies that the image /can/ be
-- used to create a 'Vulkan.Core10.Handles.ImageView' of type
-- 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
-- 'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'.
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000020
-- | 'IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT' specifies that the image
-- /can/ be used with a non-zero value of the
-- @splitInstanceBindRegionCount@ member of a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'
-- structure passed into
-- 'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2'. This
-- flag also has the effect of making the image use the standard sparse
-- image block dimensions.
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = ImageCreateFlagBits 0x00000040
-- | 'IMAGE_CREATE_ALIAS_BIT' specifies that two images created with the same
-- creation parameters and aliased to the same memory /can/ interpret the
-- contents of the memory consistently with each other, subject to the
-- rules described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
-- section. This flag further specifies that each plane of a /disjoint/
-- image /can/ share an in-memory non-linear representation with
-- single-plane images, and that a single-plane image /can/ share an
-- in-memory non-linear representation with a plane of a multi-planar
-- disjoint image, according to the rules in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#formats-compatible-planes>.
-- If the @pNext@ chain includes a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
-- or
-- 'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
-- structure whose @handleTypes@ member is not @0@, it is as if
-- 'IMAGE_CREATE_ALIAS_BIT' is set.
pattern IMAGE_CREATE_ALIAS_BIT = ImageCreateFlagBits 0x00000400

type ImageCreateFlags = ImageCreateFlagBits

instance Show ImageCreateFlagBits where
  showsPrec p = \case
    IMAGE_CREATE_SPARSE_BINDING_BIT -> showString "IMAGE_CREATE_SPARSE_BINDING_BIT"
    IMAGE_CREATE_SPARSE_RESIDENCY_BIT -> showString "IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
    IMAGE_CREATE_SPARSE_ALIASED_BIT -> showString "IMAGE_CREATE_SPARSE_ALIASED_BIT"
    IMAGE_CREATE_MUTABLE_FORMAT_BIT -> showString "IMAGE_CREATE_MUTABLE_FORMAT_BIT"
    IMAGE_CREATE_CUBE_COMPATIBLE_BIT -> showString "IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
    IMAGE_CREATE_SUBSAMPLED_BIT_EXT -> showString "IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
    IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT -> showString "IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
    IMAGE_CREATE_CORNER_SAMPLED_BIT_NV -> showString "IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
    IMAGE_CREATE_DISJOINT_BIT -> showString "IMAGE_CREATE_DISJOINT_BIT"
    IMAGE_CREATE_PROTECTED_BIT -> showString "IMAGE_CREATE_PROTECTED_BIT"
    IMAGE_CREATE_EXTENDED_USAGE_BIT -> showString "IMAGE_CREATE_EXTENDED_USAGE_BIT"
    IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT -> showString "IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
    IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT -> showString "IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
    IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT -> showString "IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
    IMAGE_CREATE_ALIAS_BIT -> showString "IMAGE_CREATE_ALIAS_BIT"
    ImageCreateFlagBits x -> showParen (p >= 11) (showString "ImageCreateFlagBits 0x" . showHex x)

instance Read ImageCreateFlagBits where
  readPrec = parens (choose [("IMAGE_CREATE_SPARSE_BINDING_BIT", pure IMAGE_CREATE_SPARSE_BINDING_BIT)
                            , ("IMAGE_CREATE_SPARSE_RESIDENCY_BIT", pure IMAGE_CREATE_SPARSE_RESIDENCY_BIT)
                            , ("IMAGE_CREATE_SPARSE_ALIASED_BIT", pure IMAGE_CREATE_SPARSE_ALIASED_BIT)
                            , ("IMAGE_CREATE_MUTABLE_FORMAT_BIT", pure IMAGE_CREATE_MUTABLE_FORMAT_BIT)
                            , ("IMAGE_CREATE_CUBE_COMPATIBLE_BIT", pure IMAGE_CREATE_CUBE_COMPATIBLE_BIT)
                            , ("IMAGE_CREATE_SUBSAMPLED_BIT_EXT", pure IMAGE_CREATE_SUBSAMPLED_BIT_EXT)
                            , ("IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT", pure IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT)
                            , ("IMAGE_CREATE_CORNER_SAMPLED_BIT_NV", pure IMAGE_CREATE_CORNER_SAMPLED_BIT_NV)
                            , ("IMAGE_CREATE_DISJOINT_BIT", pure IMAGE_CREATE_DISJOINT_BIT)
                            , ("IMAGE_CREATE_PROTECTED_BIT", pure IMAGE_CREATE_PROTECTED_BIT)
                            , ("IMAGE_CREATE_EXTENDED_USAGE_BIT", pure IMAGE_CREATE_EXTENDED_USAGE_BIT)
                            , ("IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT", pure IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT)
                            , ("IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT", pure IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT)
                            , ("IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT", pure IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT)
                            , ("IMAGE_CREATE_ALIAS_BIT", pure IMAGE_CREATE_ALIAS_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageCreateFlagBits")
                       v <- step readPrec
                       pure (ImageCreateFlagBits v)))

