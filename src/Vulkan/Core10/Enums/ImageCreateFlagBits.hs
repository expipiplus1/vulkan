{-# language CPP #-}
-- No documentation found for Chapter "ImageCreateFlagBits"
module Vulkan.Core10.Enums.ImageCreateFlagBits  ( pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR
                                                , ImageCreateFlags
                                                , ImageCreateFlagBits( IMAGE_CREATE_SPARSE_BINDING_BIT
                                                                     , IMAGE_CREATE_SPARSE_RESIDENCY_BIT
                                                                     , IMAGE_CREATE_SPARSE_ALIASED_BIT
                                                                     , IMAGE_CREATE_MUTABLE_FORMAT_BIT
                                                                     , IMAGE_CREATE_CUBE_COMPATIBLE_BIT
                                                                     , IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT
                                                                     , IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT
                                                                     , IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT
                                                                     , IMAGE_CREATE_SUBSAMPLED_BIT_EXT
                                                                     , IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
                                                                     , IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT
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

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
-- No documentation found for TopLevel "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT_KHR = IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT


type ImageCreateFlags = ImageCreateFlagBits

-- | VkImageCreateFlagBits - Bitmask specifying additional parameters of an
-- image
--
-- = Description
--
-- -   'IMAGE_CREATE_SPARSE_BINDING_BIT' specifies that the image will be
--     backed using sparse memory binding.
--
-- -   'IMAGE_CREATE_SPARSE_RESIDENCY_BIT' specifies that the image /can/
--     be partially backed using sparse memory binding. Images created with
--     this flag /must/ also be created with the
--     'IMAGE_CREATE_SPARSE_BINDING_BIT' flag.
--
-- -   'IMAGE_CREATE_SPARSE_ALIASED_BIT' specifies that the image will be
--     backed using sparse memory binding with memory ranges that might
--     also simultaneously be backing another image (or another portion of
--     the same image). Images created with this flag /must/ also be
--     created with the 'IMAGE_CREATE_SPARSE_BINDING_BIT' flag.
--
-- -   'IMAGE_CREATE_MUTABLE_FORMAT_BIT' specifies that the image /can/ be
--     used to create a 'Vulkan.Core10.Handles.ImageView' with a different
--     format from the image. For
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-multiplanar multi-planar formats>,
--     'IMAGE_CREATE_MUTABLE_FORMAT_BIT' specifies that a
--     'Vulkan.Core10.Handles.ImageView' can be created of a /plane/ of the
--     image.
--
-- -   'IMAGE_CREATE_CUBE_COMPATIBLE_BIT' specifies that the image /can/ be
--     used to create a 'Vulkan.Core10.Handles.ImageView' of type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_CUBE_ARRAY'.
--
-- -   'IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT' specifies that the image
--     /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' of type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D' or
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D_ARRAY'.
--
-- -   'IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT' specifies that the image
--     /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' of type
--     'Vulkan.Core10.Enums.ImageViewType.IMAGE_VIEW_TYPE_2D'.
--
-- -   'IMAGE_CREATE_PROTECTED_BIT' specifies that the image is a protected
--     image.
--
-- -   'IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT' specifies that the
--     image /can/ be used with a non-zero value of the
--     @splitInstanceBindRegionCount@ member of a
--     'Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2.BindImageMemoryDeviceGroupInfo'
--     structure passed into
--     'Vulkan.Core11.Promoted_From_VK_KHR_bind_memory2.bindImageMemory2'.
--     This flag also has the effect of making the image use the standard
--     sparse image block dimensions.
--
-- -   'IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT' specifies that the
--     image having a compressed format /can/ be used to create a
--     'Vulkan.Core10.Handles.ImageView' with an uncompressed format where
--     each texel in the image view corresponds to a compressed texel block
--     of the image.
--
-- -   'IMAGE_CREATE_EXTENDED_USAGE_BIT' specifies that the image /can/ be
--     created with usage flags that are not supported for the format the
--     image is created with but are supported for at least one format a
--     'Vulkan.Core10.Handles.ImageView' created from the image /can/ have.
--
-- -   'IMAGE_CREATE_DISJOINT_BIT' specifies that an image with a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-multiplanar multi-planar format>
--     /must/ have each plane separately bound to memory, rather than
--     having a single memory binding for the whole image; the presence of
--     this bit distinguishes a /disjoint image/ from an image without this
--     bit set.
--
-- -   'IMAGE_CREATE_ALIAS_BIT' specifies that two images created with the
--     same creation parameters and aliased to the same memory /can/
--     interpret the contents of the memory consistently with each other,
--     subject to the rules described in the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-memory-aliasing Memory Aliasing>
--     section. This flag further specifies that each plane of a /disjoint/
--     image /can/ share an in-memory non-linear representation with
--     single-plane images, and that a single-plane image /can/ share an
--     in-memory non-linear representation with a plane of a multi-planar
--     disjoint image, according to the rules in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-compatible-planes>.
--     If the @pNext@ chain includes a
--     'Vulkan.Core11.Promoted_From_VK_KHR_external_memory.ExternalMemoryImageCreateInfo'
--     or
--     'Vulkan.Extensions.VK_NV_external_memory.ExternalMemoryImageCreateInfoNV'
--     structure whose @handleTypes@ member is not @0@, it is as if
--     'IMAGE_CREATE_ALIAS_BIT' is set.
--
-- -   'IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT' specifies
--     that an image with a depth or depth\/stencil format /can/ be used
--     with custom sample locations when used as a depth\/stencil
--     attachment.
--
-- -   'IMAGE_CREATE_CORNER_SAMPLED_BIT_NV' specifies that the image is a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-images-corner-sampled corner-sampled image>.
--
-- -   'IMAGE_CREATE_SUBSAMPLED_BIT_EXT' specifies that an image /can/ be
--     in a subsampled format which /may/ be more optimal when written as
--     an attachment by a render pass that has a fragment density map
--     attachment. Accessing a subsampled image has additional
--     considerations:
--
--     -   Image data read as an image sampler will have undefined values
--         if the sampler was not created with @flags@ containing
--         'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
--         or was not sampled through a combined
--         <VkDescriptorSetAndBindingMappingEXT.html embedded sampler and image mapping>
--         if using descriptor heaps, or the use of a combined image
--         sampler with an immutable sampler in
--         'Vulkan.Core10.DescriptorSet.DescriptorSetLayoutBinding'.
--
--     -   Image data read with an input attachment will have undefined
--         values if the contents were not written as an attachment in an
--         earlier subpass of the same render pass.
--
--     -   Image data read as an image sampler in the fragment shader will
--         be additionally be read by the device during
--         'Vulkan.Core10.Enums.PipelineStageFlagBits.PIPELINE_STAGE_VERTEX_SHADER_BIT'
--         if
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-subsampledCoarseReconstructionEarlyAccess ::subsampledCoarseReconstructionEarlyAccess>
--         is 'Vulkan.Core10.FundamentalTypes.TRUE' and the sampler was
--         created with @flags@ containing
--         'Vulkan.Core10.Enums.SamplerCreateFlagBits.SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT'.
--
--     -   Image data read with load operations are resampled to the
--         fragment density of the render pass if
--         <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-subsampledLoads ::subsampledLoads>
--         is 'Vulkan.Core10.FundamentalTypes.TRUE'. Otherwise, values of
--         image data are undefined.
--
--     -   Image contents outside of the render area take on undefined
--         values if the image is stored as a render pass attachment.
--
-- -   'IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT' specifies that an
--     image /can/ be used in a render pass with non-zero
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-fragmentdensitymapoffsets fragment density map offsets>.
--     In a render pass with non-zero offsets, fragment density map
--     attachments, input attachments, color attachments, depth\/stencil
--     attachment, resolve attachments, and preserve attachments /must/ be
--     created with 'IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT'.
--
-- -   'Vulkan.Extensions.VK_EXT_descriptor_buffer.IMAGE_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT'
--     specifies that the image /can/ be used with descriptor buffers when
--     capturing and replaying (e.g. for trace capture and replay), see
--     'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
--     for more detail.
--
-- -   'IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT'
--     specifies that an image /can/ be used with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#multisampled-render-to-single-sampled multisampled rendering as a single-sampled framebuffer attachment>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageCreateFlagBits VK_IMAGE_CREATE_VIDEO_PROFILE_INDEPENDENT_BIT_KHR>
--     specifies that the image /can/ be used in
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-coding video coding operations>
--     without having to specify at image creation time the set of video
--     profiles the image will be used with, except for images used only as
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#dpb DPB>
--     pictures, as long as the image is otherwise
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-profile-compatibility compatible>
--     with the video profile in question.
--
--     This enables exchanging video picture data without additional copies
--     or conversions when used as:
--
--     -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#decode-output-picture Decode output pictures>,
--         regardless of the video profile used to produce them.
--
--     -   <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#encode-input-picture Encode input pictures>,
--         regardless of the video profile used to consume them.
--
--     This includes images created with both
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR>
--     and
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR>,
--     which is necessary to use the same video picture as the
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#reconstructed-picture reconstructed picture>
--     and
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#decode-output-picture decode output picture>
--     in a video decode operation on implementations supporting
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkVideoDecodeCapabilityFlagBitsKHR VK_VIDEO_DECODE_CAPABILITY_DPB_AND_OUTPUT_COINCIDE_BIT_KHR>.
--
--     However, images with only DPB usage remain tied to the video
--     profiles the image was created with, as the data layout of such
--     DPB-only images /may/ be implementation- and codec-dependent.
--
--     If an application would like to share or reuse the device memory
--     backing such images (e.g. for the purposes of temporal aliasing),
--     then it /should/ create separate image objects for each video
--     profile and bind them to the same underlying device memory range,
--     similar to how memory resources /can/ be shared across separate
--     video sessions or any other memory-backed resource.
--
-- See
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory-sparseresourcefeatures Sparse Resource Features>
-- and
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#sparsememory-physicalfeatures Sparse Physical Device Features>
-- for more details.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ImageCreateFlags'
newtype ImageCreateFlagBits = ImageCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_BINDING_BIT"
pattern IMAGE_CREATE_SPARSE_BINDING_BIT = ImageCreateFlagBits 0x00000001

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_RESIDENCY_BIT"
pattern IMAGE_CREATE_SPARSE_RESIDENCY_BIT = ImageCreateFlagBits 0x00000002

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPARSE_ALIASED_BIT"
pattern IMAGE_CREATE_SPARSE_ALIASED_BIT = ImageCreateFlagBits 0x00000004

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_MUTABLE_FORMAT_BIT"
pattern IMAGE_CREATE_MUTABLE_FORMAT_BIT = ImageCreateFlagBits 0x00000008

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_CUBE_COMPATIBLE_BIT"
pattern IMAGE_CREATE_CUBE_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000010

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT"
pattern IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT = ImageCreateFlagBits 0x00008000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT"
pattern IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT = ImageCreateFlagBits 0x00020000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT"
pattern IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT = ImageCreateFlagBits 0x00040000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SUBSAMPLED_BIT_EXT"
pattern IMAGE_CREATE_SUBSAMPLED_BIT_EXT = ImageCreateFlagBits 0x00004000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
pattern IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT = ImageCreateFlagBits 0x00001000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT"
pattern IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT = ImageCreateFlagBits 0x00010000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_CORNER_SAMPLED_BIT_NV"
pattern IMAGE_CREATE_CORNER_SAMPLED_BIT_NV = ImageCreateFlagBits 0x00002000

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_DISJOINT_BIT"
pattern IMAGE_CREATE_DISJOINT_BIT = ImageCreateFlagBits 0x00000200

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_PROTECTED_BIT"
pattern IMAGE_CREATE_PROTECTED_BIT = ImageCreateFlagBits 0x00000800

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_EXTENDED_USAGE_BIT"
pattern IMAGE_CREATE_EXTENDED_USAGE_BIT = ImageCreateFlagBits 0x00000100

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
pattern IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000080

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT"
pattern IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT = ImageCreateFlagBits 0x00000020

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT"
pattern IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT = ImageCreateFlagBits 0x00000040

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_ALIAS_BIT"
pattern IMAGE_CREATE_ALIAS_BIT = ImageCreateFlagBits 0x00000400

conNameImageCreateFlagBits :: String
conNameImageCreateFlagBits = "ImageCreateFlagBits"

enumPrefixImageCreateFlagBits :: String
enumPrefixImageCreateFlagBits = "IMAGE_CREATE_"

showTableImageCreateFlagBits :: [(ImageCreateFlagBits, String)]
showTableImageCreateFlagBits =
  [
    ( IMAGE_CREATE_SPARSE_BINDING_BIT
    , "SPARSE_BINDING_BIT"
    )
  ,
    ( IMAGE_CREATE_SPARSE_RESIDENCY_BIT
    , "SPARSE_RESIDENCY_BIT"
    )
  ,
    ( IMAGE_CREATE_SPARSE_ALIASED_BIT
    , "SPARSE_ALIASED_BIT"
    )
  ,
    ( IMAGE_CREATE_MUTABLE_FORMAT_BIT
    , "MUTABLE_FORMAT_BIT"
    )
  ,
    ( IMAGE_CREATE_CUBE_COMPATIBLE_BIT
    , "CUBE_COMPATIBLE_BIT"
    )
  ,
    ( IMAGE_CREATE_FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT
    , "FRAGMENT_DENSITY_MAP_OFFSET_BIT_EXT"
    )
  ,
    ( IMAGE_CREATE_2D_VIEW_COMPATIBLE_BIT_EXT
    , "2D_VIEW_COMPATIBLE_BIT_EXT"
    )
  ,
    ( IMAGE_CREATE_MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT
    , "MULTISAMPLED_RENDER_TO_SINGLE_SAMPLED_BIT_EXT"
    )
  ,
    ( IMAGE_CREATE_SUBSAMPLED_BIT_EXT
    , "SUBSAMPLED_BIT_EXT"
    )
  ,
    ( IMAGE_CREATE_SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT
    , "SAMPLE_LOCATIONS_COMPATIBLE_DEPTH_BIT_EXT"
    )
  ,
    ( IMAGE_CREATE_DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT
    , "DESCRIPTOR_HEAP_CAPTURE_REPLAY_BIT_EXT"
    )
  ,
    ( IMAGE_CREATE_CORNER_SAMPLED_BIT_NV
    , "CORNER_SAMPLED_BIT_NV"
    )
  , (IMAGE_CREATE_DISJOINT_BIT, "DISJOINT_BIT")
  , (IMAGE_CREATE_PROTECTED_BIT, "PROTECTED_BIT")
  ,
    ( IMAGE_CREATE_EXTENDED_USAGE_BIT
    , "EXTENDED_USAGE_BIT"
    )
  ,
    ( IMAGE_CREATE_BLOCK_TEXEL_VIEW_COMPATIBLE_BIT
    , "BLOCK_TEXEL_VIEW_COMPATIBLE_BIT"
    )
  ,
    ( IMAGE_CREATE_2D_ARRAY_COMPATIBLE_BIT
    , "2D_ARRAY_COMPATIBLE_BIT"
    )
  ,
    ( IMAGE_CREATE_SPLIT_INSTANCE_BIND_REGIONS_BIT
    , "SPLIT_INSTANCE_BIND_REGIONS_BIT"
    )
  , (IMAGE_CREATE_ALIAS_BIT, "ALIAS_BIT")
  ]

instance Show ImageCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixImageCreateFlagBits
      showTableImageCreateFlagBits
      conNameImageCreateFlagBits
      (\(ImageCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixImageCreateFlagBits
      showTableImageCreateFlagBits
      conNameImageCreateFlagBits
      ImageCreateFlagBits
