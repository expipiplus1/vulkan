{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_EXT_depth_range_unrestricted
  ( pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
  , pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION"
pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION :: Integral a => a
pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME"
pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = "VK_EXT_depth_range_unrestricted"
