{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_NV_fill_rectangle
  ( pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV
  , pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION
  , pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core10.Pipeline
  ( VkPolygonMode(..)
  )


-- | Nothing
pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV :: VkPolygonMode
pattern VK_POLYGON_MODE_FILL_RECTANGLE_NV = VkPolygonMode 1000153000
pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION :: Integral a => a
pattern VK_NV_FILL_RECTANGLE_SPEC_VERSION = 1
pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_FILL_RECTANGLE_EXTENSION_NAME = "VK_NV_fill_rectangle"
