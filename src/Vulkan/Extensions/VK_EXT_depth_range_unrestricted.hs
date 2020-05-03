{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_depth_range_unrestricted  ( EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
                                                          , pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
                                                          , EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
                                                          , pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION"
pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = 1


type EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = "VK_EXT_depth_range_unrestricted"

-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME"
pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = "VK_EXT_depth_range_unrestricted"

