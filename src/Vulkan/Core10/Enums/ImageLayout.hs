{-# language CPP #-}
-- No documentation found for Chapter "ImageLayout"
module Vulkan.Core10.Enums.ImageLayout  (ImageLayout( IMAGE_LAYOUT_UNDEFINED
                                                    , IMAGE_LAYOUT_GENERAL
                                                    , IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL
                                                    , IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL
                                                    , IMAGE_LAYOUT_PREINITIALIZED
                                                    , IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT
                                                    , IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV
                                                    , IMAGE_LAYOUT_SHARED_PRESENT_KHR
                                                    , IMAGE_LAYOUT_PRESENT_SRC_KHR
                                                    , IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL
                                                    , IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL
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
-- No documentation found for TopLevel "VkImageLayout"
newtype ImageLayout = ImageLayout Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_UNDEFINED"
pattern IMAGE_LAYOUT_UNDEFINED                        = ImageLayout 0
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_GENERAL"
pattern IMAGE_LAYOUT_GENERAL                          = ImageLayout 1
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL         = ImageLayout 2
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 3
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL  = ImageLayout 4
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL         = ImageLayout 5
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL"
pattern IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL             = ImageLayout 6
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL"
pattern IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL             = ImageLayout 7
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_PREINITIALIZED"
pattern IMAGE_LAYOUT_PREINITIALIZED                   = ImageLayout 8
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT"
pattern IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT = ImageLayout 1000218000
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV"
pattern IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV          = ImageLayout 1000164003
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_SHARED_PRESENT_KHR"
pattern IMAGE_LAYOUT_SHARED_PRESENT_KHR               = ImageLayout 1000111000
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_PRESENT_SRC_KHR"
pattern IMAGE_LAYOUT_PRESENT_SRC_KHR                  = ImageLayout 1000001002
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL        = ImageLayout 1000241003
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL       = ImageLayout 1000241002
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL          = ImageLayout 1000241001
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL         = ImageLayout 1000241000
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL = ImageLayout 1000117001
-- No documentation found for Nested "VkImageLayout" "VK_IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL"
pattern IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL = ImageLayout 1000117000
{-# complete IMAGE_LAYOUT_UNDEFINED,
             IMAGE_LAYOUT_GENERAL,
             IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
             IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL,
             IMAGE_LAYOUT_PREINITIALIZED,
             IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT,
             IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV,
             IMAGE_LAYOUT_SHARED_PRESENT_KHR,
             IMAGE_LAYOUT_PRESENT_SRC_KHR,
             IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL,
             IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL :: ImageLayout #-}

conNameImageLayout :: String
conNameImageLayout = "ImageLayout"

enumPrefixImageLayout :: String
enumPrefixImageLayout = "IMAGE_LAYOUT_"

showTableImageLayout :: [(ImageLayout, String)]
showTableImageLayout =
  [ (IMAGE_LAYOUT_UNDEFINED                       , "UNDEFINED")
  , (IMAGE_LAYOUT_GENERAL                         , "GENERAL")
  , (IMAGE_LAYOUT_COLOR_ATTACHMENT_OPTIMAL        , "COLOR_ATTACHMENT_OPTIMAL")
  , (IMAGE_LAYOUT_DEPTH_STENCIL_ATTACHMENT_OPTIMAL, "DEPTH_STENCIL_ATTACHMENT_OPTIMAL")
  , (IMAGE_LAYOUT_DEPTH_STENCIL_READ_ONLY_OPTIMAL , "DEPTH_STENCIL_READ_ONLY_OPTIMAL")
  , (IMAGE_LAYOUT_SHADER_READ_ONLY_OPTIMAL        , "SHADER_READ_ONLY_OPTIMAL")
  , (IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL            , "TRANSFER_SRC_OPTIMAL")
  , (IMAGE_LAYOUT_TRANSFER_DST_OPTIMAL            , "TRANSFER_DST_OPTIMAL")
  , (IMAGE_LAYOUT_PREINITIALIZED                  , "PREINITIALIZED")
  , (IMAGE_LAYOUT_FRAGMENT_DENSITY_MAP_OPTIMAL_EXT, "FRAGMENT_DENSITY_MAP_OPTIMAL_EXT")
  , (IMAGE_LAYOUT_SHADING_RATE_OPTIMAL_NV         , "SHADING_RATE_OPTIMAL_NV")
  , (IMAGE_LAYOUT_SHARED_PRESENT_KHR              , "SHARED_PRESENT_KHR")
  , (IMAGE_LAYOUT_PRESENT_SRC_KHR                 , "PRESENT_SRC_KHR")
  , (IMAGE_LAYOUT_STENCIL_READ_ONLY_OPTIMAL       , "STENCIL_READ_ONLY_OPTIMAL")
  , (IMAGE_LAYOUT_STENCIL_ATTACHMENT_OPTIMAL      , "STENCIL_ATTACHMENT_OPTIMAL")
  , (IMAGE_LAYOUT_DEPTH_READ_ONLY_OPTIMAL         , "DEPTH_READ_ONLY_OPTIMAL")
  , (IMAGE_LAYOUT_DEPTH_ATTACHMENT_OPTIMAL        , "DEPTH_ATTACHMENT_OPTIMAL")
  , (IMAGE_LAYOUT_DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL, "DEPTH_ATTACHMENT_STENCIL_READ_ONLY_OPTIMAL")
  , (IMAGE_LAYOUT_DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL, "DEPTH_READ_ONLY_STENCIL_ATTACHMENT_OPTIMAL")
  ]


instance Show ImageLayout where
showsPrec =
  enumShowsPrec enumPrefixImageLayout showTableImageLayout conNameImageLayout (\(ImageLayout x) -> x) (showsPrec 11)


instance Read ImageLayout where
  readPrec = enumReadPrec enumPrefixImageLayout showTableImageLayout conNameImageLayout ImageLayout

