{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_KHR_shader_draw_parameters
  ( pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
  , pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )





-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION"
pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME"
pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = "VK_KHR_shader_draw_parameters"
