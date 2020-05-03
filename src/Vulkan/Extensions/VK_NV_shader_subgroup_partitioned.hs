{-# language CPP #-}
module Vulkan.Extensions.VK_NV_shader_subgroup_partitioned  ( NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
                                                            , pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION
                                                            , NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                                                            , pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)

type NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION"
pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION :: forall a . Integral a => a
pattern NV_SHADER_SUBGROUP_PARTITIONED_SPEC_VERSION = 1


type NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_NV_shader_subgroup_partitioned"

-- No documentation found for TopLevel "VK_NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME"
pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern NV_SHADER_SUBGROUP_PARTITIONED_EXTENSION_NAME = "VK_NV_shader_subgroup_partitioned"

