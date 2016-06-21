{-# LANGUAGE PatternSynonyms #-}
module Graphics.Vulkan.KHR.SamplerMirrorClampToEdge where

import Graphics.Vulkan.Sampler( VkSamplerAddressMode(..)
                              )

pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME =  "VK_KHR_sampler_mirror_clamp_to_edge"
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION =  0x1
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = VkSamplerAddressMode 0x4
