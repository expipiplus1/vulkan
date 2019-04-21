{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Constants
  ( PipelineCacheHeaderVersion
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Constants
  ( VkPipelineCacheHeaderVersion
  )


-- | VkPipelineCacheHeaderVersion - Encode pipeline cache version
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData'
type PipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion
