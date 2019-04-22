{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_gpu_shader_int16
  ( pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME
  , pattern AMD_GPU_SHADER_INT16_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_gpu_shader_int16
  ( pattern VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME
  , pattern VK_AMD_GPU_SHADER_INT16_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME"
pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_GPU_SHADER_INT16_EXTENSION_NAME = VK_AMD_GPU_SHADER_INT16_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_GPU_SHADER_INT16_SPEC_VERSION"
pattern AMD_GPU_SHADER_INT16_SPEC_VERSION :: Integral a => a
pattern AMD_GPU_SHADER_INT16_SPEC_VERSION = VK_AMD_GPU_SHADER_INT16_SPEC_VERSION
