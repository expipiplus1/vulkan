{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_shader_trinary_minmax  ( AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
                                                       , pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
                                                       , AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
                                                       , pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)

type AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION"
pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION = 1


type AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME = "VK_AMD_shader_trinary_minmax"

-- No documentation found for TopLevel "VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME"
pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME = "VK_AMD_shader_trinary_minmax"

