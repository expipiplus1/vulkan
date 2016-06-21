{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.NV.GlslShader where

import Graphics.Vulkan.Core( VkResult(..)
                           )

pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
pattern VK_NV_GLSL_SHADER_SPEC_VERSION =  0x1
pattern VK_NV_GLSL_SHADER_EXTENSION_NAME =  "VK_NV_glsl_shader"
