{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_post_depth_coverage
  ( pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
  , pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_post_depth_coverage
  ( pattern VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME
  , pattern VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME"
pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME = VK_EXT_POST_DEPTH_COVERAGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION"
pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION :: Integral a => a
pattern EXT_POST_DEPTH_COVERAGE_SPEC_VERSION = VK_EXT_POST_DEPTH_COVERAGE_SPEC_VERSION
