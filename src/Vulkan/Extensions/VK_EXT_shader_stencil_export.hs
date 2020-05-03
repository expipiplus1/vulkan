{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_shader_stencil_export  ( EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION
                                                       , pattern EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION
                                                       , EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME
                                                       , pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME
                                                       ) where

import Data.String (IsString)

type EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION"
pattern EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_STENCIL_EXPORT_SPEC_VERSION = 1


type EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME = "VK_EXT_shader_stencil_export"

-- No documentation found for TopLevel "VK_EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME"
pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_STENCIL_EXPORT_EXTENSION_NAME = "VK_EXT_shader_stencil_export"

