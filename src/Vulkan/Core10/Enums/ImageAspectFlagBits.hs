{-# language CPP #-}
module Vulkan.Core10.Enums.ImageAspectFlagBits  ( ImageAspectFlagBits( IMAGE_ASPECT_COLOR_BIT
                                                                     , IMAGE_ASPECT_DEPTH_BIT
                                                                     , IMAGE_ASPECT_STENCIL_BIT
                                                                     , IMAGE_ASPECT_METADATA_BIT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
                                                                     , IMAGE_ASPECT_PLANE_2_BIT
                                                                     , IMAGE_ASPECT_PLANE_1_BIT
                                                                     , IMAGE_ASPECT_PLANE_0_BIT
                                                                     , ..
                                                                     )
                                                , ImageAspectFlags
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
-- | VkImageAspectFlagBits - Bitmask specifying which aspects of an image are
-- included in a view
--
-- = See Also
--
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo',
-- 'ImageAspectFlags',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo'
newtype ImageAspectFlagBits = ImageAspectFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'IMAGE_ASPECT_COLOR_BIT' specifies the color aspect.
pattern IMAGE_ASPECT_COLOR_BIT = ImageAspectFlagBits 0x00000001
-- | 'IMAGE_ASPECT_DEPTH_BIT' specifies the depth aspect.
pattern IMAGE_ASPECT_DEPTH_BIT = ImageAspectFlagBits 0x00000002
-- | 'IMAGE_ASPECT_STENCIL_BIT' specifies the stencil aspect.
pattern IMAGE_ASPECT_STENCIL_BIT = ImageAspectFlagBits 0x00000004
-- | 'IMAGE_ASPECT_METADATA_BIT' specifies the metadata aspect, used for
-- sparse
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#sparsememory sparse resource>
-- operations.
pattern IMAGE_ASPECT_METADATA_BIT = ImageAspectFlagBits 0x00000008
-- | 'IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT' specifies /memory plane/ 3.
pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT = ImageAspectFlagBits 0x00000400
-- | 'IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT' specifies /memory plane/ 2.
pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT = ImageAspectFlagBits 0x00000200
-- | 'IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT' specifies /memory plane/ 1.
pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT = ImageAspectFlagBits 0x00000100
-- | 'IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT' specifies /memory plane/ 0.
pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT = ImageAspectFlagBits 0x00000080
-- | 'IMAGE_ASPECT_PLANE_2_BIT' specifies plane 2 of a /multi-planar/ image
-- format.
pattern IMAGE_ASPECT_PLANE_2_BIT = ImageAspectFlagBits 0x00000040
-- | 'IMAGE_ASPECT_PLANE_1_BIT' specifies plane 1 of a /multi-planar/ image
-- format.
pattern IMAGE_ASPECT_PLANE_1_BIT = ImageAspectFlagBits 0x00000020
-- | 'IMAGE_ASPECT_PLANE_0_BIT' specifies plane 0 of a /multi-planar/ image
-- format.
pattern IMAGE_ASPECT_PLANE_0_BIT = ImageAspectFlagBits 0x00000010

type ImageAspectFlags = ImageAspectFlagBits

instance Show ImageAspectFlagBits where
  showsPrec p = \case
    IMAGE_ASPECT_COLOR_BIT -> showString "IMAGE_ASPECT_COLOR_BIT"
    IMAGE_ASPECT_DEPTH_BIT -> showString "IMAGE_ASPECT_DEPTH_BIT"
    IMAGE_ASPECT_STENCIL_BIT -> showString "IMAGE_ASPECT_STENCIL_BIT"
    IMAGE_ASPECT_METADATA_BIT -> showString "IMAGE_ASPECT_METADATA_BIT"
    IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT -> showString "IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT"
    IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT -> showString "IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT"
    IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT -> showString "IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT"
    IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT -> showString "IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT"
    IMAGE_ASPECT_PLANE_2_BIT -> showString "IMAGE_ASPECT_PLANE_2_BIT"
    IMAGE_ASPECT_PLANE_1_BIT -> showString "IMAGE_ASPECT_PLANE_1_BIT"
    IMAGE_ASPECT_PLANE_0_BIT -> showString "IMAGE_ASPECT_PLANE_0_BIT"
    ImageAspectFlagBits x -> showParen (p >= 11) (showString "ImageAspectFlagBits 0x" . showHex x)

instance Read ImageAspectFlagBits where
  readPrec = parens (choose [("IMAGE_ASPECT_COLOR_BIT", pure IMAGE_ASPECT_COLOR_BIT)
                            , ("IMAGE_ASPECT_DEPTH_BIT", pure IMAGE_ASPECT_DEPTH_BIT)
                            , ("IMAGE_ASPECT_STENCIL_BIT", pure IMAGE_ASPECT_STENCIL_BIT)
                            , ("IMAGE_ASPECT_METADATA_BIT", pure IMAGE_ASPECT_METADATA_BIT)
                            , ("IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT", pure IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT)
                            , ("IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT", pure IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT)
                            , ("IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT", pure IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT)
                            , ("IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT", pure IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT)
                            , ("IMAGE_ASPECT_PLANE_2_BIT", pure IMAGE_ASPECT_PLANE_2_BIT)
                            , ("IMAGE_ASPECT_PLANE_1_BIT", pure IMAGE_ASPECT_PLANE_1_BIT)
                            , ("IMAGE_ASPECT_PLANE_0_BIT", pure IMAGE_ASPECT_PLANE_0_BIT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageAspectFlagBits")
                       v <- step readPrec
                       pure (ImageAspectFlagBits v)))

