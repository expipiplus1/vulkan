{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_NV_glsl_shader
  ( pattern VK_ERROR_INVALID_SHADER_NV
  , pattern VK_NV_GLSL_SHADER_SPEC_VERSION
  , pattern VK_NV_GLSL_SHADER_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  )


-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_SHADER_NV"
pattern VK_ERROR_INVALID_SHADER_NV :: VkResult
pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_SPEC_VERSION"
pattern VK_NV_GLSL_SHADER_SPEC_VERSION :: Integral a => a
pattern VK_NV_GLSL_SHADER_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_EXTENSION_NAME"
pattern VK_NV_GLSL_SHADER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"
