{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_NV_geometry_shader_passthrough  ( NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION
                                                                     , pattern NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION
                                                                     , NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
                                                                     , pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME
                                                                     ) where

import Data.String (IsString)

type NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION"
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION :: forall a . Integral a => a
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_SPEC_VERSION = 1


type NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME = "VK_NV_geometry_shader_passthrough"

-- No documentation found for TopLevel "VK_NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME"
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_GEOMETRY_SHADER_PASSTHROUGH_EXTENSION_NAME = "VK_NV_geometry_shader_passthrough"

