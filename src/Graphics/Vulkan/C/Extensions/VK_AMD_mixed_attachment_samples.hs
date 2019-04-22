{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_AMD_mixed_attachment_samples
  ( pattern VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
  , pattern VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME"
pattern VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION"
pattern VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION :: Integral a => a
pattern VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1
