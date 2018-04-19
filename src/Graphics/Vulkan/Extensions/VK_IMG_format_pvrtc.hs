{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_IMG_format_pvrtc
  ( pattern VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG
  , pattern VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG
  , pattern VK_IMG_FORMAT_PVRTC_SPEC_VERSION
  , pattern VK_IMG_FORMAT_PVRTC_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core10.Core
  ( VkFormat(..)
  )


-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG"
pattern VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC1_2BPP_UNORM_BLOCK_IMG = VkFormat 1000054000
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG"
pattern VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC1_4BPP_UNORM_BLOCK_IMG = VkFormat 1000054001
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG"
pattern VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC2_2BPP_UNORM_BLOCK_IMG = VkFormat 1000054002
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG"
pattern VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC2_4BPP_UNORM_BLOCK_IMG = VkFormat 1000054003
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG"
pattern VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC1_2BPP_SRGB_BLOCK_IMG = VkFormat 1000054004
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG"
pattern VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC1_4BPP_SRGB_BLOCK_IMG = VkFormat 1000054005
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG"
pattern VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC2_2BPP_SRGB_BLOCK_IMG = VkFormat 1000054006
-- No documentation found for Nested "VkFormat" "VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG"
pattern VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG :: VkFormat
pattern VK_FORMAT_PVRTC2_4BPP_SRGB_BLOCK_IMG = VkFormat 1000054007
-- No documentation found for TopLevel "VK_IMG_FORMAT_PVRTC_SPEC_VERSION"
pattern VK_IMG_FORMAT_PVRTC_SPEC_VERSION :: Integral a => a
pattern VK_IMG_FORMAT_PVRTC_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_IMG_FORMAT_PVRTC_EXTENSION_NAME"
pattern VK_IMG_FORMAT_PVRTC_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_IMG_FORMAT_PVRTC_EXTENSION_NAME = "VK_IMG_format_pvrtc"
