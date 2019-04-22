{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_AMD_shader_trinary_minmax
  ( pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
  , pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_AMD_shader_trinary_minmax
  ( pattern VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME
  , pattern VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME"
pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME = VK_AMD_SHADER_TRINARY_MINMAX_EXTENSION_NAME

-- No documentation found for TopLevel "VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION"
pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION :: Integral a => a
pattern AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION = VK_AMD_SHADER_TRINARY_MINMAX_SPEC_VERSION
