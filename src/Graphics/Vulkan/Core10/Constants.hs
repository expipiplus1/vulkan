{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.Constants
  ( PipelineCacheHeaderVersion
  , pattern PIPELINE_CACHE_HEADER_VERSION_ONE
  ) where




import Graphics.Vulkan.C.Core10.Constants
  ( VkPipelineCacheHeaderVersion(..)
  , pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE
  )


-- | VkPipelineCacheHeaderVersion - Encode pipeline cache version
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData'
type PipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion


{-# complete PIPELINE_CACHE_HEADER_VERSION_ONE :: PipelineCacheHeaderVersion #-}


-- | 'Graphics.Vulkan.C.Core10.Constants.VK_PIPELINE_CACHE_HEADER_VERSION_ONE'
-- specifies version one of the pipeline cache.
pattern PIPELINE_CACHE_HEADER_VERSION_ONE :: (a ~ PipelineCacheHeaderVersion) => a
pattern PIPELINE_CACHE_HEADER_VERSION_ONE = VK_PIPELINE_CACHE_HEADER_VERSION_ONE
