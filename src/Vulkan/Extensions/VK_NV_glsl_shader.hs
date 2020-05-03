{-# language CPP #-}
module Vulkan.Extensions.VK_NV_glsl_shader  ( NV_GLSL_SHADER_SPEC_VERSION
                                            , pattern NV_GLSL_SHADER_SPEC_VERSION
                                            , NV_GLSL_SHADER_EXTENSION_NAME
                                            , pattern NV_GLSL_SHADER_EXTENSION_NAME
                                            ) where

import Data.String (IsString)

type NV_GLSL_SHADER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_SPEC_VERSION"
pattern NV_GLSL_SHADER_SPEC_VERSION :: forall a . Integral a => a
pattern NV_GLSL_SHADER_SPEC_VERSION = 1


type NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

-- No documentation found for TopLevel "VK_NV_GLSL_SHADER_EXTENSION_NAME"
pattern NV_GLSL_SHADER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_GLSL_SHADER_EXTENSION_NAME = "VK_NV_glsl_shader"

