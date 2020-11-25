{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_shader_viewport_index_layer"
module Vulkan.Extensions.VK_EXT_shader_viewport_index_layer  ( EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION
                                                             , pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION
                                                             , EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME
                                                             , pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)

type EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION"
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_SPEC_VERSION = 1


type EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME = "VK_EXT_shader_viewport_index_layer"

-- No documentation found for TopLevel "VK_EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME"
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_VIEWPORT_INDEX_LAYER_EXTENSION_NAME = "VK_EXT_shader_viewport_index_layer"

