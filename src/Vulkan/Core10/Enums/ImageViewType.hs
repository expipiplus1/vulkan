{-# language CPP #-}
-- No documentation found for Chapter "ImageViewType"
module Vulkan.Core10.Enums.ImageViewType  (ImageViewType( IMAGE_VIEW_TYPE_1D
                                                        , IMAGE_VIEW_TYPE_2D
                                                        , IMAGE_VIEW_TYPE_3D
                                                        , IMAGE_VIEW_TYPE_CUBE
                                                        , IMAGE_VIEW_TYPE_1D_ARRAY
                                                        , IMAGE_VIEW_TYPE_2D_ARRAY
                                                        , IMAGE_VIEW_TYPE_CUBE_ARRAY
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

-- | VkImageViewType - Image view types
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.ImageView.ImageViewCreateInfo',
-- 'Vulkan.Extensions.VK_EXT_filter_cubic.PhysicalDeviceImageViewImageFormatInfoEXT'
newtype ImageViewType = ImageViewType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_1D"
pattern IMAGE_VIEW_TYPE_1D = ImageViewType 0

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_2D"
pattern IMAGE_VIEW_TYPE_2D = ImageViewType 1

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_3D"
pattern IMAGE_VIEW_TYPE_3D = ImageViewType 2

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_CUBE"
pattern IMAGE_VIEW_TYPE_CUBE = ImageViewType 3

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_1D_ARRAY"
pattern IMAGE_VIEW_TYPE_1D_ARRAY = ImageViewType 4

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_2D_ARRAY"
pattern IMAGE_VIEW_TYPE_2D_ARRAY = ImageViewType 5

-- No documentation found for Nested "VkImageViewType" "VK_IMAGE_VIEW_TYPE_CUBE_ARRAY"
pattern IMAGE_VIEW_TYPE_CUBE_ARRAY = ImageViewType 6

{-# COMPLETE
  IMAGE_VIEW_TYPE_1D
  , IMAGE_VIEW_TYPE_2D
  , IMAGE_VIEW_TYPE_3D
  , IMAGE_VIEW_TYPE_CUBE
  , IMAGE_VIEW_TYPE_1D_ARRAY
  , IMAGE_VIEW_TYPE_2D_ARRAY
  , IMAGE_VIEW_TYPE_CUBE_ARRAY ::
    ImageViewType
  #-}

conNameImageViewType :: String
conNameImageViewType = "ImageViewType"

enumPrefixImageViewType :: String
enumPrefixImageViewType = "IMAGE_VIEW_TYPE_"

showTableImageViewType :: [(ImageViewType, String)]
showTableImageViewType =
  [ (IMAGE_VIEW_TYPE_1D, "1D")
  , (IMAGE_VIEW_TYPE_2D, "2D")
  , (IMAGE_VIEW_TYPE_3D, "3D")
  , (IMAGE_VIEW_TYPE_CUBE, "CUBE")
  , (IMAGE_VIEW_TYPE_1D_ARRAY, "1D_ARRAY")
  , (IMAGE_VIEW_TYPE_2D_ARRAY, "2D_ARRAY")
  , (IMAGE_VIEW_TYPE_CUBE_ARRAY, "CUBE_ARRAY")
  ]

instance Show ImageViewType where
  showsPrec =
    enumShowsPrec
      enumPrefixImageViewType
      showTableImageViewType
      conNameImageViewType
      (\(ImageViewType x) -> x)
      (showsPrec 11)

instance Read ImageViewType where
  readPrec =
    enumReadPrec
      enumPrefixImageViewType
      showTableImageViewType
      conNameImageViewType
      ImageViewType
