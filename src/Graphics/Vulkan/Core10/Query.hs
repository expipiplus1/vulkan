{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Query
  ( QueryPipelineStatisticFlagBits
  , QueryPipelineStatisticFlags
  , QueryPool
  , QueryPoolCreateFlags
  , withCStructQueryPoolCreateInfo
  , fromCStructQueryPoolCreateInfo
  , QueryPoolCreateInfo(..)
  , QueryResultFlagBits
  , QueryResultFlags
  , QueryType
  , createQueryPool
  , destroyQueryPool
  , getQueryPoolResults
  , withQueryPool
  ) where

import Control.Exception
  ( throwIO
  )
import Control.Monad
  ( when
  )
import Data.ByteString
  ( ByteString
  , packCStringLen
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Marshal.Alloc
  ( alloca
  )
import Foreign.Marshal.Array
  ( allocaArray
  )
import Foreign.Marshal.Utils
  ( maybePeek
  , maybeWith
  , with
  )
import Foreign.Ptr
  ( castPtr
  )
import Foreign.Storable
  ( peek
  )
import qualified Graphics.Vulkan.C.Dynamic
  ( createQueryPool
  , destroyQueryPool
  , getQueryPoolResults
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  , pattern VK_SUCCESS
  )
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlagBits(..)
  , VkQueryPoolCreateFlags(..)
  , VkQueryPoolCreateInfo(..)
  , VkQueryResultFlagBits(..)
  , VkQueryType(..)
  , VkQueryPool
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  , withCStructAllocationCallbacks
  )
import Graphics.Vulkan.Exception
  ( VulkanException(..)
  )
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  , peekVkStruct
  , withSomeVkStruct
  )


-- No documentation found for TopLevel "QueryPipelineStatisticFlagBits"
type QueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits
-- No documentation found for TopLevel "QueryPipelineStatisticFlags"
type QueryPipelineStatisticFlags = QueryPipelineStatisticFlagBits
-- No documentation found for TopLevel "QueryPool"
type QueryPool = VkQueryPool
-- No documentation found for TopLevel "QueryPoolCreateFlags"
type QueryPoolCreateFlags = VkQueryPoolCreateFlags
-- No documentation found for TopLevel "QueryPoolCreateInfo"
data QueryPoolCreateInfo = QueryPoolCreateInfo
  { -- Univalued Member elided
  -- No documentation found for Nested "QueryPoolCreateInfo" "pNext"
  vkPNext :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueryPoolCreateInfo" "flags"
  vkFlags :: QueryPoolCreateFlags
  , -- No documentation found for Nested "QueryPoolCreateInfo" "queryType"
  vkQueryType :: QueryType
  , -- No documentation found for Nested "QueryPoolCreateInfo" "queryCount"
  vkQueryCount :: Word32
  , -- No documentation found for Nested "QueryPoolCreateInfo" "pipelineStatistics"
  vkPipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Show, Eq)
withCStructQueryPoolCreateInfo :: QueryPoolCreateInfo -> (VkQueryPoolCreateInfo -> IO a) -> IO a
withCStructQueryPoolCreateInfo from cont = maybeWith withSomeVkStruct (vkPNext (from :: QueryPoolCreateInfo)) (\pPNext -> cont (VkQueryPoolCreateInfo VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO pPNext (vkFlags (from :: QueryPoolCreateInfo)) (vkQueryType (from :: QueryPoolCreateInfo)) (vkQueryCount (from :: QueryPoolCreateInfo)) (vkPipelineStatistics (from :: QueryPoolCreateInfo))))
fromCStructQueryPoolCreateInfo :: VkQueryPoolCreateInfo -> IO QueryPoolCreateInfo
fromCStructQueryPoolCreateInfo c = QueryPoolCreateInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkQueryPoolCreateInfo)))
                                                       <*> pure (vkFlags (c :: VkQueryPoolCreateInfo))
                                                       <*> pure (vkQueryType (c :: VkQueryPoolCreateInfo))
                                                       <*> pure (vkQueryCount (c :: VkQueryPoolCreateInfo))
                                                       <*> pure (vkPipelineStatistics (c :: VkQueryPoolCreateInfo))
-- No documentation found for TopLevel "QueryResultFlagBits"
type QueryResultFlagBits = VkQueryResultFlagBits
-- No documentation found for TopLevel "QueryResultFlags"
type QueryResultFlags = QueryResultFlagBits
-- No documentation found for TopLevel "QueryType"
type QueryType = VkQueryType

-- | Wrapper for 'vkCreateQueryPool'
createQueryPool :: Device ->  QueryPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (QueryPool)
createQueryPool = \(Device device commandTable) -> \createInfo -> \allocator -> alloca (\pQueryPool -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> (\a -> withCStructQueryPoolCreateInfo a . flip with) createInfo (\pCreateInfo -> Graphics.Vulkan.C.Dynamic.createQueryPool commandTable device pCreateInfo pAllocator pQueryPool >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> (peek pQueryPool)))))

-- | Wrapper for 'vkDestroyQueryPool'
destroyQueryPool :: Device ->  QueryPool ->  Maybe AllocationCallbacks ->  IO ()
destroyQueryPool = \(Device device commandTable) -> \queryPool -> \allocator -> maybeWith (\a -> withCStructAllocationCallbacks a . flip with) allocator (\pAllocator -> Graphics.Vulkan.C.Dynamic.destroyQueryPool commandTable device queryPool pAllocator *> (pure ()))

-- | Wrapper for 'vkGetQueryPoolResults'
getQueryPoolResults :: Device ->  QueryPool ->  Word32 ->  Word32 ->  CSize ->  DeviceSize ->  QueryResultFlags ->  IO ( VkResult
, ByteString )
getQueryPoolResults = \(Device device commandTable) -> \queryPool -> \firstQuery -> \queryCount -> \dataSize -> \stride -> \flags -> allocaArray (fromIntegral dataSize) (\pData -> Graphics.Vulkan.C.Dynamic.getQueryPoolResults commandTable device queryPool firstQuery queryCount dataSize (castPtr pData) stride flags >>= (\r -> when (r < VK_SUCCESS) (throwIO (VulkanException r)) *> ((,) <$> pure r<*>(packCStringLen (pData, (fromIntegral dataSize))))))
withQueryPool :: CreateInfo -> Maybe AllocationCallbacks -> (t -> IO a) -> IO a
withQueryPool createInfo allocationCallbacks =
  bracket
    (vkCreateQueryPool createInfo allocationCallbacks)
    (`vkDestroyQueryPool` allocationCallbacks)
