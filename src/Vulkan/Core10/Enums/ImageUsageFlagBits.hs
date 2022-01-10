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
                                                                   , IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI
                                                                   , IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR
                                                                   , IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
                                                                   , ..
                                                                   )
                                               ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ImageUsageFlags = ImageUsageFlagBits

-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'ImageUsageFlags'
newtype ImageUsageFlagBits = ImageUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_USAGE_TRANSFER_SRC_BIT' specifies that the image /can/ be used as
-- the source of a transfer command.
pattern IMAGE_USAGE_TRANSFER_SRC_BIT                         = ImageUsageFlagBits 0x00000001
-- | 'IMAGE_USAGE_TRANSFER_DST_BIT' specifies that the image /can/ be used as
-- the destination of a transfer command.
pattern IMAGE_USAGE_TRANSFER_DST_BIT                         = ImageUsageFlagBits 0x00000002
-- | 'IMAGE_USAGE_SAMPLED_BIT' specifies that the image /can/ be used to
-- create a 'Vulkan.Core10.Handles.ImageView' suitable for occupying a
-- 'Vulkan.Core10.Handles.DescriptorSet' slot either of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE' or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- and be sampled by a shader.
pattern IMAGE_USAGE_SAMPLED_BIT                              = ImageUsageFlagBits 0x00000004
-- | 'IMAGE_USAGE_STORAGE_BIT' specifies that the image /can/ be used to
-- create a 'Vulkan.Core10.Handles.ImageView' suitable for occupying a
-- 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'.
pattern IMAGE_USAGE_STORAGE_BIT                              = ImageUsageFlagBits 0x00000008
-- | 'IMAGE_USAGE_COLOR_ATTACHMENT_BIT' specifies that the image /can/ be
-- used to create a 'Vulkan.Core10.Handles.ImageView' suitable for use as a
-- color or resolve attachment in a 'Vulkan.Core10.Handles.Framebuffer'.
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT                     = ImageUsageFlagBits 0x00000010
-- | 'IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that the image
-- /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
-- use as a depth\/stencil or depth\/stencil resolve attachment in a
-- 'Vulkan.Core10.Handles.Framebuffer'.
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT             = ImageUsageFlagBits 0x00000020
-- | 'IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT' specifies that implementations
-- /may/ support using
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory memory allocations>
-- with the
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
-- to back an image with this usage. This bit /can/ be set for any image
-- that /can/ be used to create a 'Vulkan.Core10.Handles.ImageView'
-- suitable for use as a color, resolve, depth\/stencil, or input
-- attachment.
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT                 = ImageUsageFlagBits 0x00000040
-- | 'IMAGE_USAGE_INPUT_ATTACHMENT_BIT' specifies that the image /can/ be
-- used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
-- occupying 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT';
-- be read from a shader as an input attachment; and be used as an input
-- attachment in a framebuffer.
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT                     = ImageUsageFlagBits 0x00000080
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI"
pattern IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI               = ImageUsageFlagBits 0x00040000
-- | 'IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR' specifies that
-- the image /can/ be used to create a 'Vulkan.Core10.Handles.ImageView'
-- suitable for use as a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-fragment-shading-rate-attachment fragment shading rate attachment>
-- or
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#primsrast-shading-rate-image shading rate image>
pattern IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR = ImageUsageFlagBits 0x00000100
-- | 'IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT' specifies that the image
-- /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
-- use as a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops fragment density map image>.
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT             = ImageUsageFlagBits 0x00000200

conNameImageUsageFlagBits :: String
conNameImageUsageFlagBits = "ImageUsageFlagBits"

enumPrefixImageUsageFlagBits :: String
enumPrefixImageUsageFlagBits = "IMAGE_USAGE_"

showTableImageUsageFlagBits :: [(ImageUsageFlagBits, String)]
showTableImageUsageFlagBits =
  [ (IMAGE_USAGE_TRANSFER_SRC_BIT                        , "TRANSFER_SRC_BIT")
  , (IMAGE_USAGE_TRANSFER_DST_BIT                        , "TRANSFER_DST_BIT")
  , (IMAGE_USAGE_SAMPLED_BIT                             , "SAMPLED_BIT")
  , (IMAGE_USAGE_STORAGE_BIT                             , "STORAGE_BIT")
  , (IMAGE_USAGE_COLOR_ATTACHMENT_BIT                    , "COLOR_ATTACHMENT_BIT")
  , (IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT            , "DEPTH_STENCIL_ATTACHMENT_BIT")
  , (IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT                , "TRANSIENT_ATTACHMENT_BIT")
  , (IMAGE_USAGE_INPUT_ATTACHMENT_BIT                    , "INPUT_ATTACHMENT_BIT")
  , (IMAGE_USAGE_INVOCATION_MASK_BIT_HUAWEI              , "INVOCATION_MASK_BIT_HUAWEI")
  , (IMAGE_USAGE_FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR, "FRAGMENT_SHADING_RATE_ATTACHMENT_BIT_KHR")
  , (IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT            , "FRAGMENT_DENSITY_MAP_BIT_EXT")
  ]

instance Show ImageUsageFlagBits where
  showsPrec = enumShowsPrec enumPrefixImageUsageFlagBits
                            showTableImageUsageFlagBits
                            conNameImageUsageFlagBits
                            (\(ImageUsageFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read ImageUsageFlagBits where
  readPrec =
    enumReadPrec enumPrefixImageUsageFlagBits showTableImageUsageFlagBits conNameImageUsageFlagBits ImageUsageFlagBits

