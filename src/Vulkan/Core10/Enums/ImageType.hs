{-# language CPP #-}
-- No documentation found for Chapter "ImageType"
module Vulkan.Core10.Enums.ImageType  (ImageType( IMAGE_TYPE_1D
                                                , IMAGE_TYPE_2D
                                                , IMAGE_TYPE_3D
                                                , ..
                                                )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkImageType"
newtype ImageType = ImageType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkImageType" "VK_IMAGE_TYPE_1D"
pattern IMAGE_TYPE_1D = ImageType 0
-- No documentation found for Nested "VkImageType" "VK_IMAGE_TYPE_2D"
pattern IMAGE_TYPE_2D = ImageType 1
-- No documentation found for Nested "VkImageType" "VK_IMAGE_TYPE_3D"
pattern IMAGE_TYPE_3D = ImageType 2
{-# complete IMAGE_TYPE_1D,
             IMAGE_TYPE_2D,
             IMAGE_TYPE_3D :: ImageType #-}

conNameImageType :: String
conNameImageType = "ImageType"

enumPrefixImageType :: String
enumPrefixImageType = "IMAGE_TYPE_"

showTableImageType :: [(ImageType, String)]
showTableImageType = [(IMAGE_TYPE_1D, "1D"), (IMAGE_TYPE_2D, "2D"), (IMAGE_TYPE_3D, "3D")]


instance Show ImageType where
showsPrec = enumShowsPrec enumPrefixImageType showTableImageType conNameImageType (\(ImageType x) -> x) (showsPrec 11)


instance Read ImageType where
  readPrec = enumReadPrec enumPrefixImageType showTableImageType conNameImageType ImageType

