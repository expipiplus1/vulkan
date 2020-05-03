{-# language CPP #-}
module Vulkan.Extensions.VK_GOOGLE_user_type  ( GOOGLE_USER_TYPE_SPEC_VERSION
                                              , pattern GOOGLE_USER_TYPE_SPEC_VERSION
                                              , GOOGLE_USER_TYPE_EXTENSION_NAME
                                              , pattern GOOGLE_USER_TYPE_EXTENSION_NAME
                                              ) where

import Data.String (IsString)

type GOOGLE_USER_TYPE_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_GOOGLE_USER_TYPE_SPEC_VERSION"
pattern GOOGLE_USER_TYPE_SPEC_VERSION :: forall a . Integral a => a
pattern GOOGLE_USER_TYPE_SPEC_VERSION = 1


type GOOGLE_USER_TYPE_EXTENSION_NAME = "VK_GOOGLE_user_type"

-- No documentation found for TopLevel "VK_GOOGLE_USER_TYPE_EXTENSION_NAME"
pattern GOOGLE_USER_TYPE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern GOOGLE_USER_TYPE_EXTENSION_NAME = "VK_GOOGLE_user_type"

