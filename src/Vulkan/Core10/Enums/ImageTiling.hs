{-# language CPP #-}
-- No documentation found for Chapter "ImageTiling"
module Vulkan.Core10.Enums.ImageTiling  (ImageTiling( IMAGE_TILING_OPTIMAL
                                                    , IMAGE_TILING_LINEAR
                                                    , IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT
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
-- No documentation found for TopLevel "VkImageTiling"
newtype ImageTiling = ImageTiling Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkImageTiling" "VK_IMAGE_TILING_OPTIMAL"
pattern IMAGE_TILING_OPTIMAL                 = ImageTiling 0
-- No documentation found for Nested "VkImageTiling" "VK_IMAGE_TILING_LINEAR"
pattern IMAGE_TILING_LINEAR                  = ImageTiling 1
-- No documentation found for Nested "VkImageTiling" "VK_IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT"
pattern IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT = ImageTiling 1000158000
{-# complete IMAGE_TILING_OPTIMAL,
             IMAGE_TILING_LINEAR,
             IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT :: ImageTiling #-}

conNameImageTiling :: String
conNameImageTiling = "ImageTiling"

enumPrefixImageTiling :: String
enumPrefixImageTiling = "IMAGE_TILING_"

showTableImageTiling :: [(ImageTiling, String)]
showTableImageTiling =
  [ (IMAGE_TILING_OPTIMAL                , "OPTIMAL")
  , (IMAGE_TILING_LINEAR                 , "LINEAR")
  , (IMAGE_TILING_DRM_FORMAT_MODIFIER_EXT, "DRM_FORMAT_MODIFIER_EXT")
  ]


instance Show ImageTiling where
showsPrec =
  enumShowsPrec enumPrefixImageTiling showTableImageTiling conNameImageTiling (\(ImageTiling x) -> x) (showsPrec 11)


instance Read ImageTiling where
  readPrec = enumReadPrec enumPrefixImageTiling showTableImageTiling conNameImageTiling ImageTiling

