{-# language CPP #-}
module Vulkan.Core10.Enums.ImageViewType  (ImageViewType( IMAGE_VIEW_TYPE_1D
                                                        , IMAGE_VIEW_TYPE_2D
                                                        , IMAGE_VIEW_TYPE_3D
                                                        , IMAGE_VIEW_TYPE_CUBE
                                                        , IMAGE_VIEW_TYPE_1D_ARRAY
                                                        , IMAGE_VIEW_TYPE_2D_ARRAY
                                                        , IMAGE_VIEW_TYPE_CUBE_ARRAY
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
import Vulkan.Zero (Zero)
-- | VkImageViewType - Image view types
--
-- = Description
--
-- The exact image view type is partially implicit, based on the image’s
-- type and sample count, as well as the view creation parameters as
-- described in the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#resources-image-views-compatibility image view compatibility table>
-- for 'Vulkan.Core10.ImageView.createImageView'. This table also shows
-- which SPIR-V @OpTypeImage@ @Dim@ and @Arrayed@ parameters correspond to
-- each image view type.
--
-- = See Also
--
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
{-# complete IMAGE_VIEW_TYPE_1D,
             IMAGE_VIEW_TYPE_2D,
             IMAGE_VIEW_TYPE_3D,
             IMAGE_VIEW_TYPE_CUBE,
             IMAGE_VIEW_TYPE_1D_ARRAY,
             IMAGE_VIEW_TYPE_2D_ARRAY,
             IMAGE_VIEW_TYPE_CUBE_ARRAY :: ImageViewType #-}

instance Show ImageViewType where
  showsPrec p = \case
    IMAGE_VIEW_TYPE_1D -> showString "IMAGE_VIEW_TYPE_1D"
    IMAGE_VIEW_TYPE_2D -> showString "IMAGE_VIEW_TYPE_2D"
    IMAGE_VIEW_TYPE_3D -> showString "IMAGE_VIEW_TYPE_3D"
    IMAGE_VIEW_TYPE_CUBE -> showString "IMAGE_VIEW_TYPE_CUBE"
    IMAGE_VIEW_TYPE_1D_ARRAY -> showString "IMAGE_VIEW_TYPE_1D_ARRAY"
    IMAGE_VIEW_TYPE_2D_ARRAY -> showString "IMAGE_VIEW_TYPE_2D_ARRAY"
    IMAGE_VIEW_TYPE_CUBE_ARRAY -> showString "IMAGE_VIEW_TYPE_CUBE_ARRAY"
    ImageViewType x -> showParen (p >= 11) (showString "ImageViewType " . showsPrec 11 x)

instance Read ImageViewType where
  readPrec = parens (choose [("IMAGE_VIEW_TYPE_1D", pure IMAGE_VIEW_TYPE_1D)
                            , ("IMAGE_VIEW_TYPE_2D", pure IMAGE_VIEW_TYPE_2D)
                            , ("IMAGE_VIEW_TYPE_3D", pure IMAGE_VIEW_TYPE_3D)
                            , ("IMAGE_VIEW_TYPE_CUBE", pure IMAGE_VIEW_TYPE_CUBE)
                            , ("IMAGE_VIEW_TYPE_1D_ARRAY", pure IMAGE_VIEW_TYPE_1D_ARRAY)
                            , ("IMAGE_VIEW_TYPE_2D_ARRAY", pure IMAGE_VIEW_TYPE_2D_ARRAY)
                            , ("IMAGE_VIEW_TYPE_CUBE_ARRAY", pure IMAGE_VIEW_TYPE_CUBE_ARRAY)]
                     +++
                     prec 10 (do
                       expectP (Ident "ImageViewType")
                       v <- step readPrec
                       pure (ImageViewType v)))

