{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.PipelineCache
  ( PipelineCache
  , PipelineCacheCreateFlags
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCacheCreateFlags
  , VkPipelineCache
  )


-- | VkPipelineCache - Opaque handle to a pipeline cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkDestroyPipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkMergePipelineCaches'
type PipelineCache = VkPipelineCache

-- | VkPipelineCacheCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateFlags' is a
-- bitmask type for setting a mask, but is currently reserved for future
-- use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.VkPipelineCacheCreateInfo'
type PipelineCacheCreateFlags = VkPipelineCacheCreateFlags
