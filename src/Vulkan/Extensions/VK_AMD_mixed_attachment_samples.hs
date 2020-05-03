{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_mixed_attachment_samples  ( AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
                                                          , pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION
                                                          , AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
                                                          , pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION"
pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_MIXED_ATTACHMENT_SAMPLES_SPEC_VERSION = 1


type AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

-- No documentation found for TopLevel "VK_AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME"
pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_MIXED_ATTACHMENT_SAMPLES_EXTENSION_NAME = "VK_AMD_mixed_attachment_samples"

