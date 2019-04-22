{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Extensions.VK_AMD_rasterization_order
  ( RasterizationOrderAMD
  ) where




import {-# source #-} Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order
  ( VkRasterizationOrderAMD
  )


-- | VkRasterizationOrderAMD - Specify rasterization order for a graphics
-- pipeline
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Extensions.VK_AMD_rasterization_order.VkPipelineRasterizationStateRasterizationOrderAMD'
type RasterizationOrderAMD = VkRasterizationOrderAMD
