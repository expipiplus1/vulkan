{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_GOOGLE_decorate_string
  ( pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME
  , pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME"
pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME = "VK_GOOGLE_decorate_string"

-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_SPEC_VERSION"
pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION :: Integral a => a
pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION = 1
