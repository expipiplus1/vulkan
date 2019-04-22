{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_viewport_array2
  ( pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME
  , pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_NV_viewport_array2
  ( pattern VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME
  , pattern VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME"
pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_VIEWPORT_ARRAY2_EXTENSION_NAME = VK_NV_VIEWPORT_ARRAY2_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION"
pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION :: Integral a => a
pattern NV_VIEWPORT_ARRAY2_SPEC_VERSION = VK_NV_VIEWPORT_ARRAY2_SPEC_VERSION
