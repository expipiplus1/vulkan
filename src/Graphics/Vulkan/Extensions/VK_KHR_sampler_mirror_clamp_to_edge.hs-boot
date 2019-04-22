{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_KHR_sampler_mirror_clamp_to_edge
  ( SamplerAddressMode
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Sampler
  ( VkSamplerAddressMode
  )


-- No documentation found for TopLevel "SamplerAddressMode"
type SamplerAddressMode = VkSamplerAddressMode
