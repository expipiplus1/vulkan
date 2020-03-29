{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_fill_rectangle  ( NV_FILL_RECTANGLE_SPEC_VERSION
                                                        , pattern NV_FILL_RECTANGLE_SPEC_VERSION
                                                        , NV_FILL_RECTANGLE_EXTENSION_NAME
                                                        , pattern NV_FILL_RECTANGLE_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type NV_FILL_RECTANGLE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_SPEC_VERSION"
pattern NV_FILL_RECTANGLE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_FILL_RECTANGLE_SPEC_VERSION = 1


type NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"

-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_EXTENSION_NAME"
pattern NV_FILL_RECTANGLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"

