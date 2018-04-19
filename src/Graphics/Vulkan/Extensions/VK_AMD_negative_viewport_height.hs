{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_AMD_negative_viewport_height
  ( pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
  , pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION"
pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION :: Integral a => a
pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME"
pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = "VK_AMD_negative_viewport_height"
