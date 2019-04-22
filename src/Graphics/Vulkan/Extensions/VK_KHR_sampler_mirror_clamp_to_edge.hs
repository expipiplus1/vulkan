{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
  ( pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
  , pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
  , pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
  ) where

import Data.String
  ( IsString
  )


import Graphics.Vulkan.C.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
  ( pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME
  , pattern VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Sampler
  ( pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
  )


-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME"
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME = VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION"
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION :: Integral a => a
pattern KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION = VK_KHR_SAMPLER_MIRROR_CLAMP_TO_EDGE_SPEC_VERSION
