{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Sampler
  ( BorderColor
  , Filter
  , Sampler
  , SamplerAddressMode
  , SamplerCreateFlagBits
  , SamplerCreateFlags
  , SamplerMipmapMode
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Sampler
  ( VkBorderColor
  , VkFilter
  , VkSampler
  , VkSamplerAddressMode
  , VkSamplerCreateFlagBits
  , VkSamplerMipmapMode
  )


-- No documentation found for TopLevel "BorderColor"
type BorderColor = VkBorderColor

-- No documentation found for TopLevel "Filter"
type Filter = VkFilter

-- No documentation found for TopLevel "Sampler"
type Sampler = VkSampler

-- No documentation found for TopLevel "SamplerAddressMode"
type SamplerAddressMode = VkSamplerAddressMode

-- No documentation found for TopLevel "SamplerCreateFlagBits"
type SamplerCreateFlagBits = VkSamplerCreateFlagBits

-- No documentation found for TopLevel "SamplerCreateFlags"
type SamplerCreateFlags = SamplerCreateFlagBits

-- No documentation found for TopLevel "SamplerMipmapMode"
type SamplerMipmapMode = VkSamplerMipmapMode
