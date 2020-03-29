{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_viewport_array2  ( NV_VIEWPORT_ARRAY2_SPEC_VERSION
                                                         , pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION
                                                         , NV_VIEWPORT_ARRAY2_EXTENSION_NAME
                                                         , pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME
                                                         ) where

import Data.String (IsString)

type NV_VIEWPORT_ARRAY2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION"
pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION :: forall a . Integral a => a
pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION = 1


type NV_VIEWPORT_ARRAY2_EXTENSION_NAME = "VK_NV_viewport_array2"

-- No documentation found for TopLevel "VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME"
pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME = "VK_NV_viewport_array2"

