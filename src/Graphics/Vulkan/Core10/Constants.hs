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


-- No documentation found for TopLevel "PipelineCacheHeaderVersion"
type PipelineCacheHeaderVersion = VkPipelineCacheHeaderVersion


{-# complete PIPELINE_CACHE_HEADER_VERSION_ONE :: PipelineCacheHeaderVersion #-}


-- No documentation found for Nested "PipelineCacheHeaderVersion" "PIPELINE_CACHE_HEADER_VERSION_ONE"
pattern PIPELINE_CACHE_HEADER_VERSION_ONE :: (a ~ PipelineCacheHeaderVersion) => a
pattern PIPELINE_CACHE_HEADER_VERSION_ONE = VK_PIPELINE_CACHE_HEADER_VERSION_ONE
