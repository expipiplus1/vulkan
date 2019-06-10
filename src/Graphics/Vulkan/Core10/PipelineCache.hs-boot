{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  , PipelineCacheCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  , VkPipelineCacheCreateFlags
  )


-- No documentation found for TopLevel "PipelineCache"
type PipelineCache = VkPipelineCache

-- No documentation found for TopLevel "PipelineCacheCreateFlags"
type PipelineCacheCreateFlags = VkPipelineCacheCreateFlags
