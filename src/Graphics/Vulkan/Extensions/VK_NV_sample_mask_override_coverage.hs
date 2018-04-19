{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_NV_sample_mask_override_coverage
  ( pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
  , pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION"
pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION :: Integral a => a
pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME"
pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = "VK_NV_sample_mask_override_coverage"
