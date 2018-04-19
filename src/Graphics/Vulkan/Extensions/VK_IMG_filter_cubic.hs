{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_IMG_filter_cubic
  ( pattern VK_FILTER_CUBIC_IMG
  , pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  , pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION
  , pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkFormatFeatureFlagBits(..)
  )
import Graphics.Vulkan.Core10.Sampler
  ( VkFilter(..)
  )


-- No documentation found for Nested "VkFilter" "VK_FILTER_CUBIC_IMG"
pattern VK_FILTER_CUBIC_IMG :: VkFilter
pattern VK_FILTER_CUBIC_IMG = VkFilter 1000015000
-- No documentation found for Nested "VkFormatFeatureFlagBits" "VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG :: VkFormatFeatureFlagBits
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG = VkFormatFeatureFlagBits 0x00002000
-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_SPEC_VERSION"
pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION :: Integral a => a
pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_EXTENSION_NAME"
pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"
