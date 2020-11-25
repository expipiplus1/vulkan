{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_negative_viewport_height"
module Vulkan.Extensions.VK_AMD_negative_viewport_height  ( AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
                                                          , pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
                                                          , AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
                                                          , pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
                                                          ) where

import Data.String (IsString)

type AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION"
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = 1


type AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = "VK_AMD_negative_viewport_height"

-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME"
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = "VK_AMD_negative_viewport_height"

