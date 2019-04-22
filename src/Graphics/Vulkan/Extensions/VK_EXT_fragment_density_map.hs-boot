{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_fragment_density_map
  ( ImageViewCreateFlagBits
  , SamplerCreateFlagBits
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.ImageView
  ( VkImageViewCreateFlagBits
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Sampler
  ( VkSamplerCreateFlagBits
  )


-- No documentation found for TopLevel "ImageViewCreateFlagBits"
type ImageViewCreateFlagBits = VkImageViewCreateFlagBits

-- No documentation found for TopLevel "SamplerCreateFlagBits"
type SamplerCreateFlagBits = VkSamplerCreateFlagBits
