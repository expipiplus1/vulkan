{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
  ( pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
  , pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
  , pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.Core10.Sampler
  ( VkSamplerAddressMode(..)
  )


-- | @VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE@ specifies that the mirror
-- clamp to edge wrap mode will be used. This is only valid if the
-- @{html_spec_relative}#VK_KHR_sampler_mirror_clamp_to_edge@ extension is
-- enabled.
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE :: VkSamplerAddressMode
pattern VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = VkSamplerAddressMode 4
-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION"
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION :: Integral a => a
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME"
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = "VK_KHR_sampler_mirror_clamp_to_edge"
