{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.IMG.FilterCubic where

import Graphics.Vulkan.Sampler( VkFilter(..)
                              )
import Graphics.Vulkan.DeviceInitialization( VkFormatFeatureFlagBits(..)
                                           )

pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION =  0x1
pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME =  "VK_IMG_filter_cubic"
pattern VK_FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG = VkFormatFeatureFlagBits 0x2000
pattern VK_FILTER_CUBIC_IMG = VkFilter 1000015000
