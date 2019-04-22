{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_NV_framebuffer_mixed_samples
  ( CoverageModulationModeNV
  , PipelineCoverageModulationStateCreateFlagsNV
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples
  ( VkCoverageModulationModeNV
  , VkPipelineCoverageModulationStateCreateFlagsNV
  )


-- | VkCoverageModulationModeNV - Specify the discard rectangle mode
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateInfoNV'
type CoverageModulationModeNV = VkCoverageModulationModeNV

-- | VkPipelineCoverageModulationStateCreateFlagsNV - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateFlagsNV'
-- is a bitmask type for setting a mask, but is currently reserved for
-- future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_NV_framebuffer_mixed_samples.VkPipelineCoverageModulationStateCreateInfoNV'
type PipelineCoverageModulationStateCreateFlagsNV = VkPipelineCoverageModulationStateCreateFlagsNV
