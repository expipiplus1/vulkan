{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_shader_subgroup_ballot"
module Vulkan.Extensions.VK_EXT_shader_subgroup_ballot  ( EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION
                                                        , pattern EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION
                                                        , EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME
                                                        , pattern EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION"
pattern EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_SHADER_SUBGROUP_BALLOT_SPEC_VERSION = 1


type EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME = "VK_EXT_shader_subgroup_ballot"

-- No documentation found for TopLevel "VK_EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME"
pattern EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_SHADER_SUBGROUP_BALLOT_EXTENSION_NAME = "VK_EXT_shader_subgroup_ballot"

