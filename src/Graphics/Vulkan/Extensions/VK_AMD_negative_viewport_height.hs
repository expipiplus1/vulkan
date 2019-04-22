{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_negative_viewport_height
  ( pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
  , pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_negative_viewport_height
  ( pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME
  , pattern VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME"
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME = VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION"
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION :: Integral a => a
pattern AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION = VK_AMD_NEGATIVE_VIEWPORT_HEIGHT_SPEC_VERSION
