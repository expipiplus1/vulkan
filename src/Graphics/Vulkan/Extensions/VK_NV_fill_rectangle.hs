{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_fill_rectangle
  ( pattern NV_FILL_RECTANGLE_EXTENSION_NAME
  , pattern NV_FILL_RECTANGLE_SPEC_VERSION
  , pattern POLYGON_MODE_FILL_RECTANGLE_NV
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_NV_fill_rectangle
  ( pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME
  , pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Pipeline
  ( pattern POLYGON_MODE_FILL_RECTANGLE_NV
  )


-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_EXTENSION_NAME"
pattern NV_FILL_RECTANGLE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_FILL_RECTANGLE_EXTENSION_NAME = VK_NV_FILL_RECTANGLE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_FILL_RECTANGLE_SPEC_VERSION"
pattern NV_FILL_RECTANGLE_SPEC_VERSION :: Integral a => a
pattern NV_FILL_RECTANGLE_SPEC_VERSION = VK_NV_FILL_RECTANGLE_SPEC_VERSION
