{-# language CPP #-}
-- No documentation found for Chapter "FormatFeatureFlags2"
module Vulkan.Core13.Enums.FormatFeatureFlags2  ( pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_STORAGE_IMAGE_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_VERTEX_BUFFER_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_BLIT_SRC_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_BLIT_DST_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT
                                                , pattern FORMAT_FEATURE_2_TRANSFER_SRC_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_TRANSFER_DST_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_DISJOINT_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT_KHR
                                                , pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT_KHR
                                                , FormatFeatureFlags2
                                                , FormatFeatureFlagBits2( FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT
                                                                        , FORMAT_FEATURE_2_STORAGE_IMAGE_BIT
                                                                        , FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT
                                                                        , FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT
                                                                        , FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT
                                                                        , FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
                                                                        , FORMAT_FEATURE_2_VERTEX_BUFFER_BIT
                                                                        , FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT
                                                                        , FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT
                                                                        , FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                        , FORMAT_FEATURE_2_BLIT_SRC_BIT
                                                                        , FORMAT_FEATURE_2_BLIT_DST_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT
                                                                        , FORMAT_FEATURE_2_TRANSFER_SRC_BIT
                                                                        , FORMAT_FEATURE_2_TRANSFER_DST_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT
                                                                        , FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
                                                                        , FORMAT_FEATURE_2_DISJOINT_BIT
                                                                        , FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT
                                                                        , FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT
                                                                        , FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT
                                                                        , FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT
                                                                        , FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV
                                                                        , FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV
                                                                        , FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV
                                                                        , FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM
                                                                        , FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM
                                                                        , FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM
                                                                        , FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM
                                                                        , FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV
                                                                        , FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                        , FORMAT_FEATURE_2_FRAGMENT_DENSITY_MAP_BIT_EXT
                                                                        , FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR
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
import Vulkan.Core10.FundamentalTypes (Flags64)
-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_STORAGE_IMAGE_BIT_KHR"
pattern FORMAT_FEATURE_2_STORAGE_IMAGE_BIT_KHR = FORMAT_FEATURE_2_STORAGE_IMAGE_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT_KHR"
pattern FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT_KHR = FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR"
pattern FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT_KHR = FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT_KHR"
pattern FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT_KHR = FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT_KHR"
pattern FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT_KHR = FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_VERTEX_BUFFER_BIT_KHR"
pattern FORMAT_FEATURE_2_VERTEX_BUFFER_BIT_KHR = FORMAT_FEATURE_2_VERTEX_BUFFER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT_KHR"
pattern FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT_KHR = FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT_KHR"
pattern FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT_KHR = FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT_KHR"
pattern FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT_KHR = FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_BLIT_SRC_BIT_KHR"
pattern FORMAT_FEATURE_2_BLIT_SRC_BIT_KHR = FORMAT_FEATURE_2_BLIT_SRC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_BLIT_DST_BIT_KHR"
pattern FORMAT_FEATURE_2_BLIT_DST_BIT_KHR = FORMAT_FEATURE_2_BLIT_DST_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT_EXT = FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_TRANSFER_SRC_BIT_KHR"
pattern FORMAT_FEATURE_2_TRANSFER_SRC_BIT_KHR = FORMAT_FEATURE_2_TRANSFER_SRC_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_TRANSFER_DST_BIT_KHR"
pattern FORMAT_FEATURE_2_TRANSFER_DST_BIT_KHR = FORMAT_FEATURE_2_TRANSFER_DST_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT_KHR"
pattern FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT_KHR = FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_DISJOINT_BIT_KHR"
pattern FORMAT_FEATURE_2_DISJOINT_BIT_KHR = FORMAT_FEATURE_2_DISJOINT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT_KHR"
pattern FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT_KHR = FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT_KHR"
pattern FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT_KHR = FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT_KHR"
pattern FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT_KHR = FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT


-- No documentation found for TopLevel "VK_FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT_KHR"
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT_KHR = FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT


type FormatFeatureFlags2 = FormatFeatureFlagBits2

-- | VkFormatFeatureFlagBits2 - Bitmask specifying features supported by a
-- buffer
--
-- = Description
--
-- The following bits /may/ be set in @linearTilingFeatures@ and
-- @optimalTilingFeatures@, specifying that the features are supported by
-- <VkImage.html images> or <VkImageView.html image views> or
-- <VkSamplerYcbcrConversion.html sampler Y′CBCR conversion objects>
-- created with the queried
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'::@format@:
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT' specifies that an image view
--     /can/ be
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
--
-- -   'FORMAT_FEATURE_2_STORAGE_IMAGE_BIT' specifies that an image view
--     /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
--
-- -   'FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT' specifies that an image
--     view /can/ be used as storage image that supports atomic operations.
--
-- -   'FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT' specifies that an image view
--     /can/ be used as a framebuffer color attachment and as an input
--     attachment.
--
-- -   'FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT' specifies that an
--     image view /can/ be used as a framebuffer color attachment that
--     supports blending and as an input attachment.
--
-- -   'FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that an
--     image view /can/ be used as a framebuffer depth\/stencil attachment
--     and as an input attachment.
--
-- -   'FORMAT_FEATURE_2_BLIT_SRC_BIT' specifies that an image /can/ be
--     used as the @srcImage@ for
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2'
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
--
-- -   'FORMAT_FEATURE_2_BLIT_DST_BIT' specifies that an image /can/ be
--     used as the @dstImage@ for
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2'
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT' specifies that if
--     'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT' is also set, an image view
--     /can/ be used with a sampler that has either of @magFilter@ or
--     @minFilter@ set to 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR', or
--     @mipmapMode@ set to
--     'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'.
--     If 'FORMAT_FEATURE_2_BLIT_SRC_BIT' is also set, an image can be used
--     as the @srcImage@ for
--     'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2'
--     and 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage' with a
--     @filter@ of 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR'. This bit
--     /must/ only be exposed for formats that also support the
--     'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT' or
--     'FORMAT_FEATURE_2_BLIT_SRC_BIT'.
--
--     If the format being queried is a depth\/stencil format, this bit
--     only specifies that the depth aspect (not the stencil aspect) of an
--     image of this format supports linear filtering. Where depth
--     comparison is supported it /may/ be linear filtered whether this bit
--     is present or not, but where this bit is not present the filtered
--     value /may/ be computed in an implementation-dependent manner which
--     differs from the normal rules of linear filtering. The resulting
--     value /must/ be in the range [0,1] and /should/ be proportional to,
--     or a weighted average of, the number of comparison passes or
--     failures.
--
-- -   'FORMAT_FEATURE_2_TRANSFER_SRC_BIT' specifies that an image /can/ be
--     used as a source image for
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy commands>.
--
-- -   'FORMAT_FEATURE_2_TRANSFER_DST_BIT' specifies that an image /can/ be
--     used as a destination image for
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy commands>
--     and
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears clear commands>.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT' specifies
--     'Vulkan.Core10.Handles.Image' /can/ be used as a sampled image with
--     a min or max
--     'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode'.
--     This bit /must/ only be exposed for formats that also support the
--     'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT'.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT' specifies that
--     'Vulkan.Core10.Handles.Image' /can/ be used with a sampler that has
--     either of @magFilter@ or @minFilter@ set to
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT', or be the source
--     image for a blit with @filter@ set to
--     'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'. This bit /must/ only
--     be exposed for formats that also support the
--     'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT'. If the format being queried is
--     a depth\/stencil format, this only specifies that the depth aspect
--     is cubic filterable.
--
-- -   'FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT' specifies that an
--     application /can/ define a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     using this format as a source, and that an image of this format
--     /can/ be used with a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
--     @xChromaOffset@ and\/or @yChromaOffset@ of
--     'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_MIDPOINT'.
--     Otherwise both @xChromaOffset@ and @yChromaOffset@ /must/ be
--     'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_COSITED_EVEN'.
--     If a format does not incorporate chroma downsampling (it is not a
--     “422” or “420” format) but the implementation supports sampler
--     Y′CBCR conversion for this format, the implementation /must/ set
--     'FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT'.
--
-- -   'FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT' specifies that an
--     application /can/ define a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     using this format as a source, and that an image of this format
--     /can/ be used with a
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
--     @xChromaOffset@ and\/or @yChromaOffset@ of
--     'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_COSITED_EVEN'.
--     Otherwise both @xChromaOffset@ and @yChromaOffset@ /must/ be
--     'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_MIDPOINT'. If
--     neither 'FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT' nor
--     'FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT' is set, the
--     application /must/ not define a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     using this format as a source.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT'
--     specifies that an application /can/ define a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
--     using this format as a source with @chromaFilter@ set to
--     'Vulkan.Core10.Enums.Filter.FILTER_LINEAR'.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT'
--     specifies that the format can have different chroma, min, and mag
--     filters.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
--     specifies that reconstruction is explicit, as described in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-chroma-reconstruction>.
--     If this bit is not present, reconstruction is implicit by default.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'
--     specifies that reconstruction /can/ be forcibly made explicit by
--     setting
--     'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'::@forceExplicitReconstruction@
--     to 'Vulkan.Core10.FundamentalTypes.TRUE'. If the format being
--     queried supports
--     'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
--     it /must/ also support
--     'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'.
--
-- -   'FORMAT_FEATURE_2_DISJOINT_BIT' specifies that a multi-planar image
--     /can/ have the
--     'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT'
--     set during image creation. An implementation /must/ not set
--     'FORMAT_FEATURE_2_DISJOINT_BIT' for /single-plane formats/.
--
-- -   'FORMAT_FEATURE_2_FRAGMENT_DENSITY_MAP_BIT_EXT' specifies that an
--     image view /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>.
--
-- -   'FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR'
--     specifies that an image view /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>.
--     An implementation /must/ not set this feature for formats with
--     numeric type other than @*UINT@, or set it as a buffer feature.
--
-- -   @VK_FORMAT_FEATURE_2_VIDEO_DECODE_OUTPUT_BIT_KHR@ specifies that an
--     image view with this format /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#decode-output-picture decode output picture>
--     in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-decode-operations video decode operations>.
--
-- -   @VK_FORMAT_FEATURE_2_VIDEO_DECODE_DPB_BIT_KHR@ specifies that an
--     image view with this format /can/ be used as an output
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#reconstructed-picture reconstructed picture>
--     or an input
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#reference-picture reference picture>
--     in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-decode-operations video decode operations>.
--
-- -   @VK_FORMAT_FEATURE_2_VIDEO_ENCODE_INPUT_BIT_KHR@ specifies that an
--     image view with this format /can/ be used as an
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#encode-input-picture encode input picture>
--     in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-encode-operations video encode operations>.
--
-- -   @VK_FORMAT_FEATURE_2_VIDEO_ENCODE_DPB_BIT_KHR@ specifies that an
--     image view with this format /can/ be used as an output
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#reconstructed-picture reconstructed picture>
--     or an input
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#reference-picture reference picture>
--     in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-encode-operations video encode operations>.
--
--     Note
--
--     Specific
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#video-profiles video profiles>
--     /may/ have additional restrictions on the format and other image
--     creation parameters corresponding to image views used by video
--     coding operations that /can/ be enumerated using the
--     <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vkGetPhysicalDeviceVideoFormatPropertiesKHR vkGetPhysicalDeviceVideoFormatPropertiesKHR>
--     command.
--
-- -   'FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT' specifies that
--     image views or buffer views created with this format /can/ be used
--     as
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage images>
--     or
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
--     respectively for read operations without specifying a format.
--
-- -   'FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT' specifies that
--     image views or buffer views created with this format /can/ be used
--     as
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage images>
--     or
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
--     respectively for write operations without specifying a format.
--
-- -   'FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT' specifies that
--     image views created with this format /can/ be used for depth
--     comparison performed by @OpImage*Dref*@ instructions.
--
-- -   'FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV' specifies that the
--     format is supported as a renderable
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-color-attachment Linear Color Attachment>.
--     This bit will be set for renderable color formats in the
--     @linearTilingFeatures@. This /must/ not be set in the
--     @optimalTilingFeatures@ or @bufferFeatures@ members.
--
-- -   'FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM' specifies that image views
--     created with this format /can/ be used as the
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-weightimage weight image>
--     input to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage weight image sampling>
--     operations.
--
-- -   'FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM' specifies that
--     image views created with this format /can/ be sampled in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage weight image sampling>
--     operations.
--
-- -   'FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM' specifies that image
--     views created with this format /can/ be used in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-blockmatch block matching>
--     operations.
--
-- -   'FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM' specifies that image
--     views created with this format /can/ be sampled in
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-boxfilter box filter sampling>
--     operations.
--
-- The following bits /may/ be set in @bufferFeatures@, specifying that the
-- features are supported by <VkBuffer.html buffers> or
-- <VkBufferView.html buffer views> created with the queried
-- 'Vulkan.Core11.Promoted_From_VK_KHR_get_physical_device_properties2.getPhysicalDeviceFormatProperties2'::@format@:
--
-- -   'FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT' specifies that the
--     format /can/ be used to create a buffer view that /can/ be bound to
--     a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
--     descriptor.
--
-- -   'FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT' specifies that the
--     format /can/ be used to create a buffer view that /can/ be bound to
--     a
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     descriptor.
--
-- -   'FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT' specifies that
--     atomic operations are supported on
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
--     with this format.
--
-- -   'FORMAT_FEATURE_2_VERTEX_BUFFER_BIT' specifies that the format /can/
--     be used as a vertex attribute format
--     ('Vulkan.Core10.Pipeline.VertexInputAttributeDescription'::@format@).
--
-- -   'FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
--     specifies that the format /can/ be used as the vertex format when
--     creating an
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure acceleration structure>
--     ('Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'::@vertexFormat@).
--     This format /can/ also be used as the vertex format in host memory
--     when doing
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#host-acceleration-structure host acceleration structure>
--     builds.
--
-- -   'FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT' specifies that
--     buffer views created with this format /can/ be used as
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
--     for read operations without specifying a format.
--
-- -   'FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT' specifies that
--     buffer views created with this format /can/ be used as
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
--     for write operations without specifying a format.
--
-- -   'FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV' specifies that an image
--     view with this format /can/ be used as an input or reference to
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-operations optical flow operations>
--
-- -   'FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV' specifies that an
--     image view with this format /can/ be used as a flow vector map
--     (either as hint, output or global flow) for
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-operations optical flow operations>
--
-- -   'FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV' specifies that an image
--     view with this format /can/ be used as an output cost map for
--     <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-operations optical flow operations>
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_format_feature_flags2 VK_KHR_format_feature_flags2>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_3 VK_VERSION_1_3>
newtype FormatFeatureFlagBits2 = FormatFeatureFlagBits2 Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT' specifies that an image view /can/
-- be
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-sampledimage sampled from>.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT = FormatFeatureFlagBits2 0x0000000000000001

-- | 'FORMAT_FEATURE_2_STORAGE_IMAGE_BIT' specifies that an image view /can/
-- be used as a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage image>.
pattern FORMAT_FEATURE_2_STORAGE_IMAGE_BIT = FormatFeatureFlagBits2 0x0000000000000002

-- | 'FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT' specifies that an image view
-- /can/ be used as storage image that supports atomic operations.
pattern FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT = FormatFeatureFlagBits2 0x0000000000000004

-- | 'FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT' specifies that the format
-- /can/ be used to create a buffer view that /can/ be bound to a
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_UNIFORM_TEXEL_BUFFER'
-- descriptor.
pattern FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT = FormatFeatureFlagBits2 0x0000000000000008

-- | 'FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT' specifies that the format
-- /can/ be used to create a buffer view that /can/ be bound to a
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
-- descriptor.
pattern FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT = FormatFeatureFlagBits2 0x0000000000000010

-- | 'FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT' specifies that atomic
-- operations are supported on
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_TEXEL_BUFFER'
-- with this format.
pattern FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT = FormatFeatureFlagBits2 0x0000000000000020

-- | 'FORMAT_FEATURE_2_VERTEX_BUFFER_BIT' specifies that the format /can/ be
-- used as a vertex attribute format
-- ('Vulkan.Core10.Pipeline.VertexInputAttributeDescription'::@format@).
pattern FORMAT_FEATURE_2_VERTEX_BUFFER_BIT = FormatFeatureFlagBits2 0x0000000000000040

-- | 'FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT' specifies that an image view
-- /can/ be used as a framebuffer color attachment and as an input
-- attachment.
pattern FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT = FormatFeatureFlagBits2 0x0000000000000080

-- | 'FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT' specifies that an image
-- view /can/ be used as a framebuffer color attachment that supports
-- blending and as an input attachment.
pattern FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT = FormatFeatureFlagBits2 0x0000000000000100

-- | 'FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that an image
-- view /can/ be used as a framebuffer depth\/stencil attachment and as an
-- input attachment.
pattern FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT = FormatFeatureFlagBits2 0x0000000000000200

-- | 'FORMAT_FEATURE_2_BLIT_SRC_BIT' specifies that an image /can/ be used as
-- the @srcImage@ for
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2' and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
pattern FORMAT_FEATURE_2_BLIT_SRC_BIT = FormatFeatureFlagBits2 0x0000000000000400

-- | 'FORMAT_FEATURE_2_BLIT_DST_BIT' specifies that an image /can/ be used as
-- the @dstImage@ for
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2' and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'.
pattern FORMAT_FEATURE_2_BLIT_DST_BIT = FormatFeatureFlagBits2 0x0000000000000800

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT' specifies that if
-- 'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT' is also set, an image view /can/ be
-- used with a sampler that has either of @magFilter@ or @minFilter@ set to
-- 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR', or @mipmapMode@ set to
-- 'Vulkan.Core10.Enums.SamplerMipmapMode.SAMPLER_MIPMAP_MODE_LINEAR'. If
-- 'FORMAT_FEATURE_2_BLIT_SRC_BIT' is also set, an image can be used as the
-- @srcImage@ for
-- 'Vulkan.Core13.Promoted_From_VK_KHR_copy_commands2.cmdBlitImage2' and
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage' with a @filter@ of
-- 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR'. This bit /must/ only be
-- exposed for formats that also support the
-- 'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT' or 'FORMAT_FEATURE_2_BLIT_SRC_BIT'.
--
-- If the format being queried is a depth\/stencil format, this bit only
-- specifies that the depth aspect (not the stencil aspect) of an image of
-- this format supports linear filtering. Where depth comparison is
-- supported it /may/ be linear filtered whether this bit is present or
-- not, but where this bit is not present the filtered value /may/ be
-- computed in an implementation-dependent manner which differs from the
-- normal rules of linear filtering. The resulting value /must/ be in the
-- range [0,1] and /should/ be proportional to, or a weighted average of,
-- the number of comparison passes or failures.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT = FormatFeatureFlagBits2 0x0000000000001000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT' specifies that
-- 'Vulkan.Core10.Handles.Image' /can/ be used with a sampler that has
-- either of @magFilter@ or @minFilter@ set to
-- 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT', or be the source image
-- for a blit with @filter@ set to
-- 'Vulkan.Core10.Enums.Filter.FILTER_CUBIC_EXT'. This bit /must/ only be
-- exposed for formats that also support the
-- 'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT'. If the format being queried is a
-- depth\/stencil format, this only specifies that the depth aspect is
-- cubic filterable.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT = FormatFeatureFlagBits2 0x0000000000002000

-- | 'FORMAT_FEATURE_2_TRANSFER_SRC_BIT' specifies that an image /can/ be
-- used as a source image for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy commands>.
pattern FORMAT_FEATURE_2_TRANSFER_SRC_BIT = FormatFeatureFlagBits2 0x0000000000004000

-- | 'FORMAT_FEATURE_2_TRANSFER_DST_BIT' specifies that an image /can/ be
-- used as a destination image for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#copies copy commands>
-- and
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#clears clear commands>.
pattern FORMAT_FEATURE_2_TRANSFER_DST_BIT = FormatFeatureFlagBits2 0x0000000000008000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT' specifies
-- 'Vulkan.Core10.Handles.Image' /can/ be used as a sampled image with a
-- min or max
-- 'Vulkan.Core12.Enums.SamplerReductionMode.SamplerReductionMode'. This
-- bit /must/ only be exposed for formats that also support the
-- 'FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT'.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT = FormatFeatureFlagBits2 0x0000000000010000

-- | 'FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT' specifies that an
-- application /can/ define a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- using this format as a source, and that an image of this format /can/ be
-- used with a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
-- @xChromaOffset@ and\/or @yChromaOffset@ of
-- 'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_MIDPOINT'. Otherwise
-- both @xChromaOffset@ and @yChromaOffset@ /must/ be
-- 'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_COSITED_EVEN'. If a
-- format does not incorporate chroma downsampling (it is not a “422” or
-- “420” format) but the implementation supports sampler Y′CBCR conversion
-- for this format, the implementation /must/ set
-- 'FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT'.
pattern FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT = FormatFeatureFlagBits2 0x0000000000020000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT'
-- specifies that an application /can/ define a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- using this format as a source with @chromaFilter@ set to
-- 'Vulkan.Core10.Enums.Filter.FILTER_LINEAR'.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT = FormatFeatureFlagBits2 0x0000000000040000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT'
-- specifies that the format can have different chroma, min, and mag
-- filters.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT = FormatFeatureFlagBits2 0x0000000000080000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
-- specifies that reconstruction is explicit, as described in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-chroma-reconstruction>.
-- If this bit is not present, reconstruction is implicit by default.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT = FormatFeatureFlagBits2 0x0000000000100000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'
-- specifies that reconstruction /can/ be forcibly made explicit by setting
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'::@forceExplicitReconstruction@
-- to 'Vulkan.Core10.FundamentalTypes.TRUE'. If the format being queried
-- supports
-- 'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT'
-- it /must/ also support
-- 'FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT'.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT = FormatFeatureFlagBits2 0x0000000000200000

-- | 'FORMAT_FEATURE_2_DISJOINT_BIT' specifies that a multi-planar image
-- /can/ have the
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_DISJOINT_BIT' set
-- during image creation. An implementation /must/ not set
-- 'FORMAT_FEATURE_2_DISJOINT_BIT' for /single-plane formats/.
pattern FORMAT_FEATURE_2_DISJOINT_BIT = FormatFeatureFlagBits2 0x0000000000400000

-- | 'FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT' specifies that an
-- application /can/ define a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- using this format as a source, and that an image of this format /can/ be
-- used with a
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
-- @xChromaOffset@ and\/or @yChromaOffset@ of
-- 'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_COSITED_EVEN'.
-- Otherwise both @xChromaOffset@ and @yChromaOffset@ /must/ be
-- 'Vulkan.Core11.Enums.ChromaLocation.CHROMA_LOCATION_MIDPOINT'. If
-- neither 'FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT' nor
-- 'FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT' is set, the application
-- /must/ not define a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#samplers-YCbCr-conversion sampler Y′CBCR conversion>
-- using this format as a source.
pattern FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT = FormatFeatureFlagBits2 0x0000000000800000

-- | 'FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT' specifies that image
-- views or buffer views created with this format /can/ be used as
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage images>
-- or
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
-- respectively for read operations without specifying a format.
--
-- 'FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT' specifies that buffer
-- views created with this format /can/ be used as
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
-- for read operations without specifying a format.
pattern FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT = FormatFeatureFlagBits2 0x0000000080000000

-- | 'FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT' specifies that image
-- views or buffer views created with this format /can/ be used as
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storageimage storage images>
-- or
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
-- respectively for write operations without specifying a format.
--
-- 'FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT' specifies that
-- buffer views created with this format /can/ be used as
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-storagetexelbuffer storage texel buffers>
-- for write operations without specifying a format.
pattern FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT = FormatFeatureFlagBits2 0x0000000100000000

-- | 'FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT' specifies that
-- image views created with this format /can/ be used for depth comparison
-- performed by @OpImage*Dref*@ instructions.
pattern FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT = FormatFeatureFlagBits2 0x0000000200000000

-- | 'FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV' specifies that an image view
-- with this format /can/ be used as an output cost map for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-operations optical flow operations>
pattern FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV = FormatFeatureFlagBits2 0x0000040000000000

-- | 'FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV' specifies that an image
-- view with this format /can/ be used as a flow vector map (either as
-- hint, output or global flow) for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-operations optical flow operations>
pattern FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV = FormatFeatureFlagBits2 0x0000020000000000

-- | 'FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV' specifies that an image
-- view with this format /can/ be used as an input or reference to
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#opticalflow-operations optical flow operations>
pattern FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV = FormatFeatureFlagBits2 0x0000010000000000

-- | 'FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM' specifies that image
-- views created with this format /can/ be sampled in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-boxfilter box filter sampling>
-- operations.
pattern FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM = FormatFeatureFlagBits2 0x0000002000000000

-- | 'FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM' specifies that image views
-- created with this format /can/ be used in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-blockmatch block matching>
-- operations.
pattern FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM = FormatFeatureFlagBits2 0x0000001000000000

-- | 'FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM' specifies that image
-- views created with this format /can/ be sampled in
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage weight image sampling>
-- operations.
pattern FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM = FormatFeatureFlagBits2 0x0000000800000000

-- | 'FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM' specifies that image views
-- created with this format /can/ be used as the
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#descriptorsets-weightimage weight image>
-- input to
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-weightimage weight image sampling>
-- operations.
pattern FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM = FormatFeatureFlagBits2 0x0000000400000000

-- | 'FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV' specifies that the
-- format is supported as a renderable
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#glossary-linear-color-attachment Linear Color Attachment>.
-- This bit will be set for renderable color formats in the
-- @linearTilingFeatures@. This /must/ not be set in the
-- @optimalTilingFeatures@ or @bufferFeatures@ members.
pattern FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV = FormatFeatureFlagBits2 0x0000004000000000

-- | 'FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' specifies
-- that an image view /can/ be used as a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>.
-- An implementation /must/ not set this feature for formats with numeric
-- type other than @*UINT@, or set it as a buffer feature.
pattern FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = FormatFeatureFlagBits2 0x0000000040000000

-- | 'FORMAT_FEATURE_2_FRAGMENT_DENSITY_MAP_BIT_EXT' specifies that an image
-- view /can/ be used as a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#renderpass-fragmentdensitymapattachment fragment density map attachment>.
pattern FORMAT_FEATURE_2_FRAGMENT_DENSITY_MAP_BIT_EXT = FormatFeatureFlagBits2 0x0000000001000000

-- | 'FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR'
-- specifies that the format /can/ be used as the vertex format when
-- creating an
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#acceleration-structure acceleration structure>
-- ('Vulkan.Extensions.VK_KHR_acceleration_structure.AccelerationStructureGeometryTrianglesDataKHR'::@vertexFormat@).
-- This format /can/ also be used as the vertex format in host memory when
-- doing
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#host-acceleration-structure host acceleration structure>
-- builds.
pattern FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR = FormatFeatureFlagBits2 0x0000000020000000

conNameFormatFeatureFlagBits2 :: String
conNameFormatFeatureFlagBits2 = "FormatFeatureFlagBits2"

enumPrefixFormatFeatureFlagBits2 :: String
enumPrefixFormatFeatureFlagBits2 = "FORMAT_FEATURE_2_"

showTableFormatFeatureFlagBits2 :: [(FormatFeatureFlagBits2, String)]
showTableFormatFeatureFlagBits2 =
  [
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_BIT
    , "SAMPLED_IMAGE_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_STORAGE_IMAGE_BIT
    , "STORAGE_IMAGE_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_STORAGE_IMAGE_ATOMIC_BIT
    , "STORAGE_IMAGE_ATOMIC_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_UNIFORM_TEXEL_BUFFER_BIT
    , "UNIFORM_TEXEL_BUFFER_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_BIT
    , "STORAGE_TEXEL_BUFFER_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_STORAGE_TEXEL_BUFFER_ATOMIC_BIT
    , "STORAGE_TEXEL_BUFFER_ATOMIC_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_VERTEX_BUFFER_BIT
    , "VERTEX_BUFFER_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_COLOR_ATTACHMENT_BIT
    , "COLOR_ATTACHMENT_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_COLOR_ATTACHMENT_BLEND_BIT
    , "COLOR_ATTACHMENT_BLEND_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_DEPTH_STENCIL_ATTACHMENT_BIT
    , "DEPTH_STENCIL_ATTACHMENT_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_BLIT_SRC_BIT
    , "BLIT_SRC_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_BLIT_DST_BIT
    , "BLIT_DST_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_LINEAR_BIT
    , "SAMPLED_IMAGE_FILTER_LINEAR_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_CUBIC_BIT
    , "SAMPLED_IMAGE_FILTER_CUBIC_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_TRANSFER_SRC_BIT
    , "TRANSFER_SRC_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_TRANSFER_DST_BIT
    , "TRANSFER_DST_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_FILTER_MINMAX_BIT
    , "SAMPLED_IMAGE_FILTER_MINMAX_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_MIDPOINT_CHROMA_SAMPLES_BIT
    , "MIDPOINT_CHROMA_SAMPLES_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_LINEAR_FILTER_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_SEPARATE_RECONSTRUCTION_FILTER_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT
    , "SAMPLED_IMAGE_YCBCR_CONVERSION_CHROMA_RECONSTRUCTION_EXPLICIT_FORCEABLE_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_DISJOINT_BIT
    , "DISJOINT_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_COSITED_CHROMA_SAMPLES_BIT
    , "COSITED_CHROMA_SAMPLES_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_STORAGE_READ_WITHOUT_FORMAT_BIT
    , "STORAGE_READ_WITHOUT_FORMAT_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_STORAGE_WRITE_WITHOUT_FORMAT_BIT
    , "STORAGE_WRITE_WITHOUT_FORMAT_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_SAMPLED_IMAGE_DEPTH_COMPARISON_BIT
    , "SAMPLED_IMAGE_DEPTH_COMPARISON_BIT"
    )
  ,
    ( FORMAT_FEATURE_2_OPTICAL_FLOW_COST_BIT_NV
    , "OPTICAL_FLOW_COST_BIT_NV"
    )
  ,
    ( FORMAT_FEATURE_2_OPTICAL_FLOW_VECTOR_BIT_NV
    , "OPTICAL_FLOW_VECTOR_BIT_NV"
    )
  ,
    ( FORMAT_FEATURE_2_OPTICAL_FLOW_IMAGE_BIT_NV
    , "OPTICAL_FLOW_IMAGE_BIT_NV"
    )
  ,
    ( FORMAT_FEATURE_2_BOX_FILTER_SAMPLED_BIT_QCOM
    , "BOX_FILTER_SAMPLED_BIT_QCOM"
    )
  ,
    ( FORMAT_FEATURE_2_BLOCK_MATCHING_BIT_QCOM
    , "BLOCK_MATCHING_BIT_QCOM"
    )
  ,
    ( FORMAT_FEATURE_2_WEIGHT_SAMPLED_IMAGE_BIT_QCOM
    , "WEIGHT_SAMPLED_IMAGE_BIT_QCOM"
    )
  ,
    ( FORMAT_FEATURE_2_WEIGHT_IMAGE_BIT_QCOM
    , "WEIGHT_IMAGE_BIT_QCOM"
    )
  ,
    ( FORMAT_FEATURE_2_LINEAR_COLOR_ATTACHMENT_BIT_NV
    , "LINEAR_COLOR_ATTACHMENT_BIT_NV"
    )
  ,
    ( FORMAT_FEATURE_2_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( FORMAT_FEATURE_2_FRAGMENT_DENSITY_MAP_BIT_EXT
    , "FRAGMENT_DENSITY_MAP_BIT_EXT"
    )
  ,
    ( FORMAT_FEATURE_2_ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR
    , "ACCELERATION_STRUCTURE_VERTEX_BUFFER_BIT_KHR"
    )
  ]

instance Show FormatFeatureFlagBits2 where
  showsPrec =
    enumShowsPrec
      enumPrefixFormatFeatureFlagBits2
      showTableFormatFeatureFlagBits2
      conNameFormatFeatureFlagBits2
      (\(FormatFeatureFlagBits2 x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read FormatFeatureFlagBits2 where
  readPrec =
    enumReadPrec
      enumPrefixFormatFeatureFlagBits2
      showTableFormatFeatureFlagBits2
      conNameFormatFeatureFlagBits2
      FormatFeatureFlagBits2
