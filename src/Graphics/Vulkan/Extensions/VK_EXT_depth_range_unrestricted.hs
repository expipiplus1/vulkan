{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_EXT_depth_range_unrestricted
  ( pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
  , pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_EXT_depth_range_unrestricted
  ( pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME
  , pattern VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME"
pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME = VK_EXT_DEPTH_RANGE_UNRESTRICTED_EXTENSION_NAME

-- No documentation found for TopLevel "VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION"
pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION :: Integral a => a
pattern EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION = VK_EXT_DEPTH_RANGE_UNRESTRICTED_SPEC_VERSION
