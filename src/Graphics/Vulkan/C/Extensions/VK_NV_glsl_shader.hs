{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader
  ( pattern VK_ERROR_INVALID_SHADER_NV
  , pattern VK_NV_GLSL_SHADER_EXTENSION_NAME
  , pattern VK_NV_GLSL_SHADER_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )


-- | 'VK_ERROR_INVALID_SHADER_NV' One or more shaders failed to compile or
-- link. More details are reported back to the application via
-- @https:\/\/www.khronos.org\/registry\/vulkan\/specs\/1.1-extensions\/html\/vkspec.html#VK_EXT_debug_report@
-- if enabled.
pattern VK_ERROR_INVALID_SHADER_NV :: VkResult
pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_EXTENSION_NAME"
pattern VK_NV_GLSL_SHADER_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_SPEC_VERSION"
pattern VK_NV_GLSL_SHADER_SPEC_VERSION :: Integral a => a
pattern VK_NV_GLSL_SHADER_SPEC_VERSION = 1
