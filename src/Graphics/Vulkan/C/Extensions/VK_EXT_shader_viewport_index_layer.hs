{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_shader_viewport_index_layer
  ( pattern VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME
  , pattern VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME"
pattern VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME = "VK_EXT_shader_viewport_index_layer"

-- No documentation found for TopLevel "VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION"
pattern VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION :: Integral a => a
pattern VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION = 1
