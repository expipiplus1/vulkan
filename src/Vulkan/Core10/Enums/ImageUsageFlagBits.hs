{-# language CPP #-}
-- No documentation found for Chapter "ImageUsageFlagBits"
module Vulkan.Core10.Enums.ImageUsageFlagBits  ( ImageUsageFlags
                                               , ImageUsageFlagBits( IMAGE_USAGE_TRANSFER_SRC_BIT
                                                                   , IMAGE_USAGE_TRANSFER_DST_BIT
                                                                   , IMAGE_USAGE_SAMPLED_BIT
                                                                   , IMAGE_USAGE_STORAGE_BIT
                                                                   , IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_INPUT_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_TILE_MEMORY_BIT_QCOM
                                                                   , IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM
                                                                   , IMAGE_USAGE_SAMPLE_BLOCK_MATCH_BIT_QCOM
                                                                   , IMAGE_USAGE_SAMPLE_WEIGHT_BIT_QCOM
                                                                   , IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI
                                                                   , IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
                                                                   , IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                   , IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
                                                                   , IMAGE_USAGE_HOST_TRANSFER_BIT
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
type ImageUsageFlags = ImageUsageFlagBits

-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = Description
--
-- -   'IMAGE_USAGE_TRANSFER_SRC_BIT' specifies that the image /can/ be
--     used as the source of a transfer command.
--
-- -   'IMAGE_USAGE_TRANSFER_DST_BIT' specifies that the image /can/ be
--     used as the destination of a transfer command.
--
-- -   'IMAGE_USAGE_SAMPLED_BIT' specifies that the image /can/ be used to
--     create a 'Vulkan.Core10.Handles.ImageView' suitable for occupying a
--     'Vulkan.Core10.Handles.DescriptorSet' slot either of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE'
--     or
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
--     and be sampled by a shader.
--
-- -   'IMAGE_USAGE_STORAGE_BIT' specifies that the image /can/ be used to
--     create a 'Vulkan.Core10.Handles.ImageView' suitable for occupying a
--     'Vulkan.Core10.Handles.DescriptorSet' slot of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'.
--
-- -   'IMAGE_USAGE_COLOR_ATTACHMENT_BIT' specifies that the image /can/ be
--     used to create a 'Vulkan.Core10.Handles.ImageView' suitable for use
--     as a color or resolve attachment in a
--     'Vulkan.Core10.Handles.Framebuffer'.
--
-- -   'IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that the image
--     /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' suitable
--     for use as a depth\/stencil or depth\/stencil resolve attachment in
--     a 'Vulkan.Core10.Handles.Framebuffer'.
--
-- -   'IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT' specifies that
--     implementations /may/ support using
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#memory memory allocations>
--     with the
--     'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
--     to back an image with this usage. This bit /can/ be set for any
--     image that /can/ be used to create a
--     'Vulkan.Core10.Handles.ImageView' suitable for use as a color,
--     resolve, depth\/stencil, or input attachment.
--
-- -   'IMAGE_USAGE_INPUT_ATTACHMENT_BIT' specifies that the image /can/ be
--     used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
--     occupying 'Vulkan.Core10.Handles.DescriptorSet' slot of type
--     'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT';
--     be read from a shader as an input attachment; and be used as an
--     input attachment in a framebuffer.
--
-- -   'IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT' specifies that the image
--     /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' suitable
--     for use as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#fragmentdensitymapops fragment density map image>.
--
-- -   'IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' specifies
--     that the image /can/ be used to create a
--     'Vulkan.Core10.Handles.ImageView' suitable for use as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
--     or
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#primsrast-shading-rate-image shading rate image>
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_DST_BIT_KHR>
--     specifies that the image /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#decode-output-picture decode output picture>
--     in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-decode-operations video decode operation>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_SRC_BIT_KHR>
--     is reserved for future use.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_DECODE_DPB_BIT_KHR>
--     specifies that the image /can/ be used as an output
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#reconstructed-picture reconstructed picture>
--     or an input
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#reference-picture reference picture>
--     in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-decode-operations video decode operation>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_DST_BIT_KHR>
--     is reserved for future use.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_SRC_BIT_KHR>
--     specifies that the image /can/ be used as an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#encode-input-picture encode input picture>
--     in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operation>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_DPB_BIT_KHR>
--     specifies that the image /can/ be used as an output
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#reconstructed-picture reconstructed picture>
--     or an input
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#reference-picture reference picture>
--     in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operation>.
--
-- -   'IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT' specifies that the
--     image /can/ be used as a color or depth\/stencil attachment with
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#renderpass-feedbackloop feedback loop enabled>.
--
-- -   'IMAGE_USAGE_TILE_MEMORY_BIT_QCOM' specifies that the image /can/ be
--     bound to 'Vulkan.Core10.Handles.DeviceMemory' allocated from a
--     'Vulkan.Core10.DeviceInitialization.MemoryHeap' with the
--     'Vulkan.Core10.Enums.MemoryHeapFlagBits.MEMORY_HEAP_TILE_MEMORY_BIT_QCOM'
--     property.
--
-- -   'IMAGE_USAGE_HOST_TRANSFER_BIT' specifies that the image /can/ be
--     used with host copy commands and host layout transitions.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_QUANTIZATION_DELTA_MAP_BIT_KHR>
--     specifies that the image /can/ be used as a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#encode-quantization-delta-map quantization delta map>
--     in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operation>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkImageUsageFlagBits VK_IMAGE_USAGE_VIDEO_ENCODE_EMPHASIS_MAP_BIT_KHR>
--     specifies that the image /can/ be used as an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#encode-emphasis-map emphasis map>
--     in a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#video-encode-operations video encode operation>.
--
-- -   'Vulkan.Extensions.VK_EXT_host_image_copy.IMAGE_USAGE_HOST_TRANSFER_BIT_EXT'
--     specifies that the image /can/ be used with host copy commands and
--     host layout transitions.
--
-- -   'IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM' specifies that the image /can/
--     be transitioned to the
--     'Vulkan.Core10.Enums.ImageLayout.IMAGE_LAYOUT_TENSOR_ALIASING_ARM'
--     layout. See
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#resources-memory-aliasing>
--     for a complete set of rules for tensor\/image aliasing.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ImageUsageFlags'
newtype ImageUsageFlagBits = ImageUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSFER_SRC_BIT"
pattern IMAGE_USAGE_TRANSFER_SRC_BIT = ImageUsageFlagBits 0x00000001

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSFER_DST_BIT"
pattern IMAGE_USAGE_TRANSFER_DST_BIT = ImageUsageFlagBits 0x00000002

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SAMPLED_BIT"
pattern IMAGE_USAGE_SAMPLED_BIT = ImageUsageFlagBits 0x00000004

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_STORAGE_BIT"
pattern IMAGE_USAGE_STORAGE_BIT = ImageUsageFlagBits 0x00000008

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000010

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000020

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000040

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000080

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TILE_MEMORY_BIT_QCOM"
pattern IMAGE_USAGE_TILE_MEMORY_BIT_QCOM = ImageUsageFlagBits 0x08000000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM"
pattern IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM = ImageUsageFlagBits 0x00800000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SAMPLE_BLOCK_MATCH_BIT_QCOM"
pattern IMAGE_USAGE_SAMPLE_BLOCK_MATCH_BIT_QCOM = ImageUsageFlagBits 0x00200000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SAMPLE_WEIGHT_BIT_QCOM"
pattern IMAGE_USAGE_SAMPLE_WEIGHT_BIT_QCOM = ImageUsageFlagBits 0x00100000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI"
pattern IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI = ImageUsageFlagBits 0x00040000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
pattern IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT = ImageUsageFlagBits 0x00080000

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
pattern IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = ImageUsageFlagBits 0x00000100

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = ImageUsageFlagBits 0x00000200

-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_HOST_TRANSFER_BIT"
pattern IMAGE_USAGE_HOST_TRANSFER_BIT = ImageUsageFlagBits 0x00400000

conNameImageUsageFlagBits :: String
conNameImageUsageFlagBits = "ImageUsageFlagBits"

enumPrefixImageUsageFlagBits :: String
enumPrefixImageUsageFlagBits = "IMAGE_USAGE_"

showTableImageUsageFlagBits :: [(ImageUsageFlagBits, String)]
showTableImageUsageFlagBits =
  [
    ( IMAGE_USAGE_TRANSFER_SRC_BIT
    , "TRANSFER_SRC_BIT"
    )
  ,
    ( IMAGE_USAGE_TRANSFER_DST_BIT
    , "TRANSFER_DST_BIT"
    )
  , (IMAGE_USAGE_SAMPLED_BIT, "SAMPLED_BIT")
  , (IMAGE_USAGE_STORAGE_BIT, "STORAGE_BIT")
  ,
    ( IMAGE_USAGE_COLOR_ATTACHMENT_BIT
    , "COLOR_ATTACHMENT_BIT"
    )
  ,
    ( IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
    , "DEPTH_STENCIL_ATTACHMENT_BIT"
    )
  ,
    ( IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
    , "TRANSIENT_ATTACHMENT_BIT"
    )
  ,
    ( IMAGE_USAGE_INPUT_ATTACHMENT_BIT
    , "INPUT_ATTACHMENT_BIT"
    )
  ,
    ( IMAGE_USAGE_TILE_MEMORY_BIT_QCOM
    , "TILE_MEMORY_BIT_QCOM"
    )
  ,
    ( IMAGE_USAGE_TENSOR_ALIASING_BIT_ARM
    , "TENSOR_ALIASING_BIT_ARM"
    )
  ,
    ( IMAGE_USAGE_SAMPLE_BLOCK_MATCH_BIT_QCOM
    , "SAMPLE_BLOCK_MATCH_BIT_QCOM"
    )
  ,
    ( IMAGE_USAGE_SAMPLE_WEIGHT_BIT_QCOM
    , "SAMPLE_WEIGHT_BIT_QCOM"
    )
  ,
    ( IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI
    , "INVOCATION_MASK_BIT_HUAWEI"
    )
  ,
    ( IMAGE_USAGE_ATTACHMENT_FEEDBACK_LOOP_BIT_EXT
    , "ATTACHMENT_FEEDBACK_LOOP_BIT_EXT"
    )
  ,
    ( IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
    , "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR"
    )
  ,
    ( IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
    , "FRAGMENT_DENSITY_MAP_BIT_EXT"
    )
  ,
    ( IMAGE_USAGE_HOST_TRANSFER_BIT
    , "HOST_TRANSFER_BIT"
    )
  ]

instance Show ImageUsageFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixImageUsageFlagBits
      showTableImageUsageFlagBits
      conNameImageUsageFlagBits
      (\(ImageUsageFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageUsageFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixImageUsageFlagBits
      showTableImageUsageFlagBits
      conNameImageUsageFlagBits
      ImageUsageFlagBits
