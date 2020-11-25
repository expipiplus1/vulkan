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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type ImageAspectFlags = ImageAspectFlagBits

-- No documentation found for TopLevel "VkImageAspectFlagBits"
newtype ImageAspectFlagBits = ImageAspectFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_COLOR_BIT"
pattern IMAGE_ASPECT_COLOR_BIT              = ImageAspectFlagBits 0x00000001
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_DEPTH_BIT"
pattern IMAGE_ASPECT_DEPTH_BIT              = ImageAspectFlagBits 0x00000002
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_STENCIL_BIT"
pattern IMAGE_ASPECT_STENCIL_BIT            = ImageAspectFlagBits 0x00000004
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_METADATA_BIT"
pattern IMAGE_ASPECT_METADATA_BIT           = ImageAspectFlagBits 0x00000008
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT = ImageAspectFlagBits 0x00000400
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT = ImageAspectFlagBits 0x00000200
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT = ImageAspectFlagBits 0x00000100
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT"
pattern IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT = ImageAspectFlagBits 0x00000080
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_2_BIT"
pattern IMAGE_ASPECT_PLANE_2_BIT            = ImageAspectFlagBits 0x00000040
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_1_BIT"
pattern IMAGE_ASPECT_PLANE_1_BIT            = ImageAspectFlagBits 0x00000020
-- No documentation found for Nested "VkImageAspectFlagBits" "VK_IMAGE_ASPECT_PLANE_0_BIT"
pattern IMAGE_ASPECT_PLANE_0_BIT            = ImageAspectFlagBits 0x00000010

conNameImageAspectFlagBits :: String
conNameImageAspectFlagBits = "ImageAspectFlagBits"

enumPrefixImageAspectFlagBits :: String
enumPrefixImageAspectFlagBits = "IMAGE_ASPECT_"

showTableImageAspectFlagBits :: [(ImageAspectFlagBits, String)]
showTableImageAspectFlagBits =
  [ (IMAGE_ASPECT_COLOR_BIT             , "COLOR_BIT")
  , (IMAGE_ASPECT_DEPTH_BIT             , "DEPTH_BIT")
  , (IMAGE_ASPECT_STENCIL_BIT           , "STENCIL_BIT")
  , (IMAGE_ASPECT_METADATA_BIT          , "METADATA_BIT")
  , (IMAGE_ASPECT_MEMORY_PLANE_3_BIT_EXT, "MEMORY_PLANE_3_BIT_EXT")
  , (IMAGE_ASPECT_MEMORY_PLANE_2_BIT_EXT, "MEMORY_PLANE_2_BIT_EXT")
  , (IMAGE_ASPECT_MEMORY_PLANE_1_BIT_EXT, "MEMORY_PLANE_1_BIT_EXT")
  , (IMAGE_ASPECT_MEMORY_PLANE_0_BIT_EXT, "MEMORY_PLANE_0_BIT_EXT")
  , (IMAGE_ASPECT_PLANE_2_BIT           , "PLANE_2_BIT")
  , (IMAGE_ASPECT_PLANE_1_BIT           , "PLANE_1_BIT")
  , (IMAGE_ASPECT_PLANE_0_BIT           , "PLANE_0_BIT")
  ]


instance Show ImageAspectFlagBits where
showsPrec = enumShowsPrec enumPrefixImageAspectFlagBits
                          showTableImageAspectFlagBits
                          conNameImageAspectFlagBits
                          (\(ImageAspectFlagBits x) -> x)
                          (\x -> showString "0x" . showHex x)


instance Read ImageAspectFlagBits where
  readPrec = enumReadPrec enumPrefixImageAspectFlagBits
                          showTableImageAspectFlagBits
                          conNameImageAspectFlagBits
                          ImageAspectFlagBits

