{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_EXT_conservative_rasterization
  ( ConservativeRasterizationModeEXT
  , PipelineRasterizationConservativeStateCreateFlagsEXT
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization
  ( VkConservativeRasterizationModeEXT
  , VkPipelineRasterizationConservativeStateCreateFlagsEXT
  )


-- | VkConservativeRasterizationModeEXT - Specify the conservative
-- rasterization mode
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateInfoEXT'
type ConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT

-- | VkPipelineRasterizationConservativeStateCreateFlagsEXT - Reserved for
-- future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateFlagsEXT'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_EXT_conservative_rasterization.VkPipelineRasterizationConservativeStateCreateInfoEXT'
type PipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT
