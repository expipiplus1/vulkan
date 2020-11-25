{-# language CPP #-}
-- No documentation found for Chapter "VK_NV_sample_mask_override_coverage"
module Vulkan.Extensions.VK_NV_sample_mask_override_coverage  ( NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
                                                              , pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
                                                              , NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
                                                              , pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)

type NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION"
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1


type NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = "VK_NV_sample_mask_override_coverage"

-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME"
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = "VK_NV_sample_mask_override_coverage"

