{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_IMG_filter_cubic
  ( pattern IMG_FILTER_CUBIC_EXTENSION_NAME
  , pattern IMG_FILTER_CUBIC_SPEC_VERSION
  , pattern FILTER_CUBIC_IMG
  , pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_IMG_filter_cubic
  ( pattern VK_IMG_FILTER_CUBIC_EXTENSION_NAME
  , pattern VK_IMG_FILTER_CUBIC_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( pattern FORMAT_FEATURE_SAMPLED_IMAGE_FILTER_CUBIC_BIT_IMG
  )
import Graphics.Vulkan.Core10.Sampler
  ( pattern FILTER_CUBIC_IMG
  )


-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_EXTENSION_NAME"
pattern IMG_FILTER_CUBIC_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern IMG_FILTER_CUBIC_EXTENSION_NAME = VK_IMG_FILTER_CUBIC_EXTENSION_NAME

-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_SPEC_VERSION"
pattern IMG_FILTER_CUBIC_SPEC_VERSION :: Integral a => a
pattern IMG_FILTER_CUBIC_SPEC_VERSION = VK_IMG_FILTER_CUBIC_SPEC_VERSION
