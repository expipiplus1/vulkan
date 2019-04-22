{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_GOOGLE_decorate_string
  ( pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME
  , pattern GOOGLE_DECORATE_STRING_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_GOOGLE_decorate_string
  ( pattern VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME
  , pattern VK_GOOGLE_DECORATE_STRING_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME"
pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern GOOGLE_DECORATE_STRING_EXTENSION_NAME = VK_GOOGLE_DECORATE_STRING_EXTENSION_NAME

-- No documentation found for TopLevel "VK_GOOGLE_DECORATE_STRING_SPEC_VERSION"
pattern GOOGLE_DECORATE_STRING_SPEC_VERSION :: Integral a => a
pattern GOOGLE_DECORATE_STRING_SPEC_VERSION = VK_GOOGLE_DECORATE_STRING_SPEC_VERSION
