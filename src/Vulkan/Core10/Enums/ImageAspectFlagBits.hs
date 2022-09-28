{-# language CPP #-}
-- No documentation found for Chapter "ImageAspectFlagBits"
module Vulkan.Core10.Enums.ImageAspectFlagBits  ( ImageAspectFlags
                                                , ImageAspectFlagBits( IMAGE_ASPECT_COLOR_BIT
                                                                     , IMAGE_ASPECT_DEPTH_BIT
                                                                     , IMAGE_ASPECT_STENCIL_BIT
                                                                     , IMAGE_ASPECT_METADATA_BIT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
                                                                     , IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
                                                                     , IMAGE_ASPECT_NONE
                                                                     , IMAGE_ASPECT_PLANE_2_BIT
                                                                     , IMAGE_ASPECT_PLANE_1_BIT
                                                                     , IMAGE_ASPECT_PLANE_0_BIT
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
type ImageAspectFlags = ImageAspectFlagBits

-- | VkImageAspectFlagBits - Bitmask specifying which aspects of an image are
-- included in a view
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.BindImagePlaneMemoryInfo',
-- 'Vulkan.Core13.Promoted_From_VK_KHR_maintenance4.DeviceImageMemoryRequirements',
-- 'Vulkan.Extensions.VK_EXT_metal_objects.ExportMetalTextureInfoEXT',
-- 'ImageAspectFlags',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.ImagePlaneMemoryRequirementsInfo',
-- 'Vulkan.Extensions.VK_EXT_metal_objects.ImportMetalTextureInfoEXT'
newtype ImageAspectFlagBits = ImageAspectFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'IMAGE_ASPECT_COLOR_BIT' specifies the color aspect.
pattern IMAGE_ASPECT_COLOR_BIT = ImageAspectFlagBits 0x00000001

-- | 'IMAGE_ASPECT_DEPTH_BIT' specifies the depth aspect.
pattern IMAGE_ASPECT_DEPTH_BIT = ImageAspectFlagBits 0x00000002

-- | 'IMAGE_ASPECT_STENCIL_BIT' specifies the stencil aspect.
pattern IMAGE_ASPECT_STENCIL_BIT = ImageAspectFlagBits 0x00000004

-- | 'IMAGE_ASPECT_METADATA_BIT' specifies the metadata aspect used for
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#sparsememory sparse resource>
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

-- | 'IMAGE_ASPECT_NONE' specifies no image aspect, or the image aspect is
-- not applicable.
pattern IMAGE_ASPECT_NONE = ImageAspectFlagBits 0x00000000

-- | 'IMAGE_ASPECT_PLANE_2_BIT' specifies plane 2 of a /multi-planar/ image
-- format.
pattern IMAGE_ASPECT_PLANE_2_BIT = ImageAspectFlagBits 0x00000040

-- | 'IMAGE_ASPECT_PLANE_1_BIT' specifies plane 1 of a /multi-planar/ image
-- format.
pattern IMAGE_ASPECT_PLANE_1_BIT = ImageAspectFlagBits 0x00000020

-- | 'IMAGE_ASPECT_PLANE_0_BIT' specifies plane 0 of a /multi-planar/ image
-- format.
pattern IMAGE_ASPECT_PLANE_0_BIT = ImageAspectFlagBits 0x00000010

conNameImageAspectFlagBits :: String
conNameImageAspectFlagBits = "ImageAspectFlagBits"

enumPrefixImageAspectFlagBits :: String
enumPrefixImageAspectFlagBits = "IMAGE_ASPECT_"

showTableImageAspectFlagBits :: [(ImageAspectFlagBits, String)]
showTableImageAspectFlagBits =
  [ (IMAGE_ASPECT_COLOR_BIT, "COLOR_BIT")
  , (IMAGE_ASPECT_DEPTH_BIT, "DEPTH_BIT")
  , (IMAGE_ASPECT_STENCIL_BIT, "STENCIL_BIT")
  , (IMAGE_ASPECT_METADATA_BIT, "METADATA_BIT")
  ,
    ( IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT
    , "MEMORY_PLANE_3_BIT_EXT"
    )
  ,
    ( IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT
    , "MEMORY_PLANE_2_BIT_EXT"
    )
  ,
    ( IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT
    , "MEMORY_PLANE_1_BIT_EXT"
    )
  ,
    ( IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT
    , "MEMORY_PLANE_0_BIT_EXT"
    )
  , (IMAGE_ASPECT_NONE, "NONE")
  , (IMAGE_ASPECT_PLANE_2_BIT, "PLANE_2_BIT")
  , (IMAGE_ASPECT_PLANE_1_BIT, "PLANE_1_BIT")
  , (IMAGE_ASPECT_PLANE_0_BIT, "PLANE_0_BIT")
  ]

instance Show ImageAspectFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixImageAspectFlagBits
      showTableImageAspectFlagBits
      conNameImageAspectFlagBits
      (\(ImageAspectFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ImageAspectFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixImageAspectFlagBits
      showTableImageAspectFlagBits
      conNameImageAspectFlagBits
      ImageAspectFlagBits
