{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_GOOGLE_decorate_string  ( GOOGLE_DECORATE_STRING_SPEC_VERSION
                                                             , pattern GOOGLE_DECORATE_STRING_SPEC_VERSION
                                                             , GOOGLE_DECORATE_STRING_EXTENSION_NAME
                                                             , pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)

type GOOGLE_DECORATE_STRING_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_SPEC_VERSION"
pattern GOOGLE_DECORATE_STRING_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_DECORATE_STRING_SPEC_VERSION = 1


type GOOGLE_DECORATE_STRING_EXTENSION_NAME = "VK_GOOGLE_decorate_string"

-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME"
pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME = "VK_GOOGLE_decorate_string"

