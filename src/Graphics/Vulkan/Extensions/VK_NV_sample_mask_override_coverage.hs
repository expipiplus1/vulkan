{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_sample_mask_override_coverage
  ( pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
  , pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_NV_sample_mask_override_coverage
  ( pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME
  , pattern VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME"
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME = VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION"
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION :: Integral a => a
pattern NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION = VK_NV_SAMPLE_MASK_OVERRIDE_COVERAGE_SPEC_VERSION
