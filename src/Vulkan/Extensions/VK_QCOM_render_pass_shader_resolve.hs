{-# language CPP #-}
-- No documentation found for Chapter "VK_QCOM_render_pass_shader_resolve"
module Vulkan.Extensions.VK_QCOM_render_pass_shader_resolve  ( QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION
                                                             , pattern QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION
                                                             , QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
                                                             , pattern QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)

type QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4

-- No documentation found for TopLevel "VK_QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION"
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION :: forall a . Integral a => a
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_SPEC_VERSION = 4


type QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME = "VK_QCOM_render_pass_shader_resolve"

-- No documentation found for TopLevel "VK_QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME"
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern QCOM_RENDER_PASS_SHADER_RESOLVE_EXTENSION_NAME = "VK_QCOM_render_pass_shader_resolve"

