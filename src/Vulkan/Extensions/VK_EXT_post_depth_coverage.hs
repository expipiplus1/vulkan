{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_post_depth_coverage"
module Vulkan.Extensions.VK_EXT_post_depth_coverage  ( EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
                                                     , pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
                                                     , EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
                                                     , pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
                                                     ) where

import Data.String (IsString)

type EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION"
pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = 1


type EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME = "VK_EXT_post_depth_coverage"

-- No documentation found for TopLevel "VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME"
pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME = "VK_EXT_post_depth_coverage"

