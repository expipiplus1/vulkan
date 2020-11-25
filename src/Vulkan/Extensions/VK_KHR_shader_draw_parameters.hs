{-# language CPP #-}
-- No documentation found for Chapter "VK_KHR_shader_draw_parameters"
module Vulkan.Extensions.VK_KHR_shader_draw_parameters  ( KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
                                                        , pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION
                                                        , KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
                                                        , pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION"
pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SHADER_DRAW_PARAMETERS_SPEC_VERSION = 1


type KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = "VK_KHR_shader_draw_parameters"

-- No documentation found for TopLevel "VK_KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME"
pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SHADER_DRAW_PARAMETERS_EXTENSION_NAME = "VK_KHR_shader_draw_parameters"

