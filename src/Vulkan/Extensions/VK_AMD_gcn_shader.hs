{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_gcn_shader  ( AMD_GCN_SHADER_SPEC_VERSION
                                            , pattern AMD_GCN_SHADER_SPEC_VERSION
                                            , AMD_GCN_SHADER_EXTENSION_NAME
                                            , pattern AMD_GCN_SHADER_EXTENSION_NAME
                                            ) where

import Data.String (IsString)

type AMD_GCN_SHADER_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_AMD_GCN_SHADER_SPEC_VERSION"
pattern AMD_GCN_SHADER_SPEC_VERSION :: forall a . Integral a => a
pattern AMD_GCN_SHADER_SPEC_VERSION = 1


type AMD_GCN_SHADER_EXTENSION_NAME = "VK_AMD_gcn_shader"

-- No documentation found for TopLevel "VK_AMD_GCN_SHADER_EXTENSION_NAME"
pattern AMD_GCN_SHADER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern AMD_GCN_SHADER_EXTENSION_NAME = "VK_AMD_gcn_shader"

