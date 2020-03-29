{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_shader_non_semantic_info  ( KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION
                                                                   , pattern KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION
                                                                   , KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
                                                                   , pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME
                                                                   ) where

import Data.String (IsString)

type KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION"
pattern KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_NON_SEMANTIC_INFO_SPEC_VERSION = 1


type KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME = "VK_KHR_shader_non_semantic_info"

-- No documentation found for TopLevel "VK_KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME"
pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_NON_SEMANTIC_INFO_EXTENSION_NAME = "VK_KHR_shader_non_semantic_info"

