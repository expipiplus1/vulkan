{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.PipelineCache
  ( VkPipelineCache
  , VkPipelineCacheCreateFlags
  , VkPipelineCacheCreateInfo
  , FN_vkCreatePipelineCache
  , PFN_vkCreatePipelineCache
  , FN_vkDestroyPipelineCache
  , PFN_vkDestroyPipelineCache
  , FN_vkGetPipelineCacheData
  , PFN_vkGetPipelineCacheData
  , FN_vkMergePipelineCaches
  , PFN_vkMergePipelineCaches
  ) where

import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.Core
  ( VkResult
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks
  , VkDevice
  )


-- | Dummy data to tag the 'Ptr' with
data VkPipelineCache_T
-- | VkPipelineCache - Opaque handle to a pipeline cache object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateComputePipelines',
-- 'Graphics.Vulkan.C.Core10.Pipeline.vkCreateGraphicsPipelines',
-- 'vkCreatePipelineCache', 'vkDestroyPipelineCache',
-- 'vkGetPipelineCacheData', 'vkMergePipelineCaches'
type VkPipelineCache = Ptr VkPipelineCache_T

data VkPipelineCacheCreateFlags

data VkPipelineCacheCreateInfo

type FN_vkCreatePipelineCache = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkPipelineCacheCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pPipelineCache" ::: Ptr VkPipelineCache) -> IO VkResult
type PFN_vkCreatePipelineCache = FunPtr FN_vkCreatePipelineCache

type FN_vkDestroyPipelineCache = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyPipelineCache = FunPtr FN_vkDestroyPipelineCache

type FN_vkGetPipelineCacheData = ("device" ::: VkDevice) -> ("pipelineCache" ::: VkPipelineCache) -> ("pDataSize" ::: Ptr CSize) -> ("pData" ::: Ptr ()) -> IO VkResult
type PFN_vkGetPipelineCacheData = FunPtr FN_vkGetPipelineCacheData

type FN_vkMergePipelineCaches = ("device" ::: VkDevice) -> ("dstCache" ::: VkPipelineCache) -> ("srcCacheCount" ::: Word32) -> ("pSrcCaches" ::: Ptr VkPipelineCache) -> IO VkResult
type PFN_vkMergePipelineCaches = FunPtr FN_vkMergePipelineCaches
