{-# language CPP #-}
module Vulkan.Core10.Enums.ImageUsageFlagBits  ( ImageUsageFlagBits( IMAGE_USAGE_TRANSFER_SRC_BIT
                                                                   , IMAGE_USAGE_TRANSFER_DST_BIT
                                                                   , IMAGE_USAGE_SAMPLED_BIT
                                                                   , IMAGE_USAGE_STORAGE_BIT
                                                                   , IMAGE_USAGE_COLOR_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_INPUT_ATTACHMENT_BIT
                                                                   , IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT
                                                                   , IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV
                                                                   , ..
                                                                   )
                                               , ImageUsageFlags
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
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
-- | VkImageUsageFlagBits - Bitmask specifying intended usage of an image
--
-- = See Also
--
-- 'ImageUsageFlags'
newtype ImageUsageFlagBits = ImageUsageFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_USAGE_TRANSFER_SRC_BIT' specifies that the image /can/ be used as
-- the source of a transfer command.
pattern IMAGE_USAGE_TRANSFER_SRC_BIT = ImageUsageFlagBits 0x00000001
-- | 'IMAGE_USAGE_TRANSFER_DST_BIT' specifies that the image /can/ be used as
-- the destination of a transfer command.
pattern IMAGE_USAGE_TRANSFER_DST_BIT = ImageUsageFlagBits 0x00000002
-- | 'IMAGE_USAGE_SAMPLED_BIT' specifies that the image /can/ be used to
-- create a 'Vulkan.Core10.Handles.ImageView' suitable for occupying a
-- 'Vulkan.Core10.Handles.DescriptorSet' slot either of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_SAMPLED_IMAGE' or
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER',
-- and be sampled by a shader.
pattern IMAGE_USAGE_SAMPLED_BIT = ImageUsageFlagBits 0x00000004
-- | 'IMAGE_USAGE_STORAGE_BIT' specifies that the image /can/ be used to
-- create a 'Vulkan.Core10.Handles.ImageView' suitable for occupying a
-- 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_STORAGE_IMAGE'.
pattern IMAGE_USAGE_STORAGE_BIT = ImageUsageFlagBits 0x00000008
-- | 'IMAGE_USAGE_COLOR_ATTACHMENT_BIT' specifies that the image /can/ be
-- used to create a 'Vulkan.Core10.Handles.ImageView' suitable for use as a
-- color or resolve attachment in a 'Vulkan.Core10.Handles.Framebuffer'.
pattern IMAGE_USAGE_COLOR_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000010
-- | 'IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT' specifies that the image
-- /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
-- use as a depth\/stencil or depth\/stencil resolve attachment in a
-- 'Vulkan.Core10.Handles.Framebuffer'.
pattern IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000020
-- | 'IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT' specifies that the memory bound
-- to this image will have been allocated with the
-- 'Vulkan.Core10.Enums.MemoryPropertyFlagBits.MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT'
-- (see
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#memory>
-- for more detail). This bit /can/ be set for any image that /can/ be used
-- to create a 'Vulkan.Core10.Handles.ImageView' suitable for use as a
-- color, resolve, depth\/stencil, or input attachment.
pattern IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000040
-- | 'IMAGE_USAGE_INPUT_ATTACHMENT_BIT' specifies that the image /can/ be
-- used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
-- occupying 'Vulkan.Core10.Handles.DescriptorSet' slot of type
-- 'Vulkan.Core10.Enums.DescriptorType.DESCRIPTOR_TYPE_INPUT_ATTACHMENT';
-- be read from a shader as an input attachment; and be used as an input
-- attachment in a framebuffer.
pattern IMAGE_USAGE_INPUT_ATTACHMENT_BIT = ImageUsageFlagBits 0x00000080
-- | 'IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT' specifies that the image
-- /can/ be used to create a 'Vulkan.Core10.Handles.ImageView' suitable for
-- use as a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragmentdensitymapops fragment density map image>.
pattern IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT = ImageUsageFlagBits 0x00000200
-- No documentation found for Nested "VkImageUsageFlagBits" "VK_IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
pattern IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV = ImageUsageFlagBits 0x00000100

type ImageUsageFlags = ImageUsageFlagBits

instance Show ImageUsageFlagBits where
  showsPrec p = \case
    IMAGE_USAGE_TRANSFER_SRC_BIT -> showString "IMAGE_USAGE_TRANSFER_SRC_BIT"
    IMAGE_USAGE_TRANSFER_DST_BIT -> showString "IMAGE_USAGE_TRANSFER_DST_BIT"
    IMAGE_USAGE_SAMPLED_BIT -> showString "IMAGE_USAGE_SAMPLED_BIT"
    IMAGE_USAGE_STORAGE_BIT -> showString "IMAGE_USAGE_STORAGE_BIT"
    IMAGE_USAGE_COLOR_ATTACHMENT_BIT -> showString "IMAGE_USAGE_COLOR_ATTACHMENT_BIT"
    IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT -> showString "IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT"
    IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT -> showString "IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT"
    IMAGE_USAGE_INPUT_ATTACHMENT_BIT -> showString "IMAGE_USAGE_INPUT_ATTACHMENT_BIT"
    IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT -> showString "IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT"
    IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV -> showString "IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV"
    ImageUsageFlagBits x -> showParen (p >= 11) (showString "ImageUsageFlagBits 0x" . showHex x)

instance Read ImageUsageFlagBits where
  readPrec = parens (choose [("IMAGE_USAGE_TRANSFER_SRC_BIT", pure IMAGE_USAGE_TRANSFER_SRC_BIT)
                            , ("IMAGE_USAGE_TRANSFER_DST_BIT", pure IMAGE_USAGE_TRANSFER_DST_BIT)
                            , ("IMAGE_USAGE_SAMPLED_BIT", pure IMAGE_USAGE_SAMPLED_BIT)
                            , ("IMAGE_USAGE_STORAGE_BIT", pure IMAGE_USAGE_STORAGE_BIT)
                            , ("IMAGE_USAGE_COLOR_ATTACHMENT_BIT", pure IMAGE_USAGE_COLOR_ATTACHMENT_BIT)
                            , ("IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT", pure IMAGE_USAGE_DEPTH_STENCIL_ATTACHMENT_BIT)
                            , ("IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT", pure IMAGE_USAGE_TRANSIENT_ATTACHMENT_BIT)
                            , ("IMAGE_USAGE_INPUT_ATTACHMENT_BIT", pure IMAGE_USAGE_INPUT_ATTACHMENT_BIT)
                            , ("IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT", pure IMAGE_USAGE_FRAGMENT_DENSITY_MAP_BIT_EXT)
                            , ("IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV", pure IMAGE_USAGE_SHADING_RATE_IMAGE_BIT_NV)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageUsageFlagBits")
                       v <- step readPrec
                       pure (ImageUsageFlagBits v)))

