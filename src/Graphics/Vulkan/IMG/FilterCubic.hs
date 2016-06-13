{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.IMG.FilterCubic where

import Graphics.Vulkan.Sampler( VkFilter(..)
                              )

pattern VK_FILTER_CUBIC_IMG = VkFilter 1000015000
