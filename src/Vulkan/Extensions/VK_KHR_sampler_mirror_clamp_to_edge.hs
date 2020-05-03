{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge  ( pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR
                                                              , KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
                                                              , pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
                                                              , KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
                                                              , pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
                                                              ) where

import Data.String (IsString)
import Vulkan.Core10.Enums.SamplerAddressMode (SamplerAddressMode(SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE))
-- No documentation found for TopLevel "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR"
pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE_KHR = SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE


type KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3

-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION"
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION :: forall a . Integral a => a
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 3


type KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = "VK_KHR_sampler_mirror_clamp_to_edge"

-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME"
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = "VK_KHR_sampler_mirror_clamp_to_edge"

