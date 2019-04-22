{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_sampler_filter_minmax
  ( SamplerReductionModeEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax
  ( VkSamplerReductionModeEXT
  )


-- | VkSamplerReductionModeEXT - Specify reduction mode for texture filtering
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_sampler_filter_minmax.VkSamplerReductionModeCreateInfoEXT'
type SamplerReductionModeEXT = VkSamplerReductionModeEXT
