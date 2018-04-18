{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_NV_geometry_shader_passthrough
  ( pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION
  , pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )





pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION :: Integral a => a
pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1
pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME = "VK_NV_geometry_shader_passthrough"
