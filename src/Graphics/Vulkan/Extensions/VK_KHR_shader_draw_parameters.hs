{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_draw_parameters
  ( pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
  , pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_shader_draw_parameters
  ( pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
  , pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
  )


-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME"
pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION"
pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION :: Integral a => a
pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
