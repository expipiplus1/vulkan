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


-- No documentation found for TopLevel "ConservativeRasterizationModeEXT"
type ConservativeRasterizationModeEXT = VkConservativeRasterizationModeEXT

-- No documentation found for TopLevel "PipelineRasterizationConservativeStateCreateFlagsEXT"
type PipelineRasterizationConservativeStateCreateFlagsEXT = VkPipelineRasterizationConservativeStateCreateFlagsEXT
