{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_NV_glsl_shader
  ( pattern NV_GLSL_SHADER_EXTENSION_NAME
  , pattern NV_GLSL_SHADER_SPEC_VERSION
  , pattern ERROR_INVALID_SHADER_NV
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_NV_glsl_shader
  ( pattern VK_NV_GLSL_SHADER_EXTENSION_NAME
  , pattern VK_NV_GLSL_SHADER_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern ERROR_INVALID_SHADER_NV
  )


-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_EXTENSION_NAME"
pattern NV_GLSL_SHADER_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern NV_GLSL_SHADER_EXTENSION_NAME = VK_NV_GLSL_SHADER_EXTENSION_NAME

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_SPEC_VERSION"
pattern NV_GLSL_SHADER_SPEC_VERSION :: Integral a => a
pattern NV_GLSL_SHADER_SPEC_VERSION = VK_NV_GLSL_SHADER_SPEC_VERSION
