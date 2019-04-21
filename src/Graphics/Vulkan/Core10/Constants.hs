{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}

module Graphics.Vulkan.Core10.Constants
  ( PipelineCacheHeaderVersion
  , pattern PIPELINE_CACHE_HEADER_VERSION_ONE
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_WHOLE_SIZE
  , pattern VK_ATTACHMENT_UNUSED
  , pattern VK_TRUE
  , pattern VK_FALSE
  , pattern VK_NULL_HANDLE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_SUBPASS_EXTERNAL
  ) where




import Graphics.Vulkan.C.Core10.Constants
  ( VkPipelineCacheHeaderVersion(..)
  , pattern VK_PIPELINE_CACHE_HEADER_VERSION_ONE
  )
import Graphics.Vulkan.C.Core10.Constants
  ( pattern VK_ATTACHMENT_UNUSED
  , pattern VK_LOD_CLAMP_NONE
  , pattern VK_NULL_HANDLE
  , pattern VK_QUEUE_FAMILY_IGNORED
  , pattern VK_REMAINING_ARRAY_LAYERS
  , pattern VK_REMAINING_MIP_LEVELS
  , pattern VK_SUBPASS_EXTERNAL
  , pattern VK_WHOLE_SIZE
  )
import Graphics.Vulkan.C.Core10.Core
  ( pattern VK_FALSE
  , pattern VK_TRUE
  )


-- | VkPipelineCacheHeaderVersion - Encode pipeline cache version
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkCreatePipelineCache',
-- 'Graphics.Vulkan.C.Core10.PipelineCache.vkGetPipelineCacheData'
type PipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion


-- | 'Graphics.Vulkan.C.Core10.Constants.VK_PIPELINE_CACHE_HEADER_VERSION_ONE'
-- specifies version one of the pipeline cache.
pattern PIPELINE_CACHE_HEADER_VERSION_ONE :: (a ~ PipelineCacheHeaderVersion) => a
pattern PIPELINE_CACHE_HEADER_VERSION_ONE = VK_PIPELINE_CACHE_HEADER_VERSION_ONE
