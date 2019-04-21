{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_relaxed_block_layout
  ( pattern VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME
  , pattern VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME"
pattern VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_RELAXED_BLOCK_LAYOUT_EXTENSION_NAME = "VK_KHR_relaxed_block_layout"

-- No documentation found for TopLevel "VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION"
pattern VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION :: Integral a => a
pattern VK_KHR_RELAXED_BLOCK_LAYOUT_SPEC_VERSION = 1
