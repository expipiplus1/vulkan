{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_IMG_filter_cubic  ( IMG_FILTER_CUBIC_SPEC_VERSION
                                                       , pattern IMG_FILTER_CUBIC_SPEC_VERSION
                                                       , IMG_FILTER_CUBIC_EXTENSION_NAME
                                                       , pattern IMG_FILTER_CUBIC_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)

type IMG_FILTER_CUBIC_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_SPEC_VERSION"
pattern IMG_FILTER_CUBIC_SPEC_VERSION :: forall a . Integral a => a
pattern IMG_FILTER_CUBIC_SPEC_VERSION = 1


type IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"

-- No documentation found for TopLevel "VK_IMG_FILTER_CUBIC_EXTENSION_NAME"
pattern IMG_FILTER_CUBIC_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern IMG_FILTER_CUBIC_EXTENSION_NAME = "VK_IMG_filter_cubic"

