{-# language CPP #-}
-- No documentation found for Chapter "VK_AMD_gpu_shader_int16"
module Vulkan.Extensions.VK_AMD_gpu_shader_int16  ( AMD_GPU_SHADER_INT16_SPEC_VERSION
                                                  , pattern AMD_GPU_SHADER_INT16_SPEC_VERSION
                                                  , AMD_GPU_SHADER_INT16_EXTENSION_NAME
                                                  , pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME
                                                  ) where

import Data.String (IsString)

type AMD_GPU_SHADER_INT16_SPEC_VERSION = 2

-- No documentation found for TopLevel "VK_AMD_GPU_SHADER_INT16_SPEC_VERSION"
pattern AMD_GPU_SHADER_INT16_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_GPU_SHADER_INT16_SPEC_VERSION = 2


type AMD_GPU_SHADER_INT16_EXTENSION_NAME = "VK_AMD_gpu_shader_int16"

-- No documentation found for TopLevel "VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME"
pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME = "VK_AMD_gpu_shader_int16"

