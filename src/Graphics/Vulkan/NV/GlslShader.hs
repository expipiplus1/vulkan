{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.NV.GlslShader where

import Graphics.Vulkan.Core( VkResult(..)
                           )

pattern VK_ERROR_INVALID_SHADER_NV = VkResult (-1000012000)
