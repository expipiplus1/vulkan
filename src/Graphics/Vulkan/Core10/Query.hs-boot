{-# language Strict #-}
{-# language CPP #-}


module Graphics.Vulkan.Core10.Query
  ( QueryPipelineStatisticFlagBits
  , QueryPipelineStatisticFlags
  , QueryPool
  , QueryPoolCreateFlags
  , QueryResultFlagBits
  , QueryResultFlags
  , QueryType
  ) where




import {-# source #-} Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlagBits
  , VkQueryPoolCreateFlags
  , VkQueryResultFlagBits
  , VkQueryType
  , VkQueryPool
  )


-- | VkQueryPipelineStatisticFlagBits - Bitmask specifying queried pipeline
-- statistics
--
-- = Description
--
-- These values are intended to measure relative statistics on one
-- implementation. Various device architectures will count these values
-- differently. Any or all counters /may/ be affected by the issues
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-undefined Query Operation>.
--
-- __Note__
--
-- For example, tile-based rendering devices /may/ need to replay the scene
-- multiple times, affecting some of the counts.
--
-- If a pipeline has @rasterizerDiscardEnable@ enabled, implementations
-- /may/ discard primitives after the final vertex processing stage. As a
-- result, if @rasterizerDiscardEnable@ is enabled, the clipping input and
-- output primitives counters /may/ not be incremented.
--
-- When a pipeline statistics query finishes, the result for that query is
-- marked as available. The application /can/ copy the result to a buffer
-- (via
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults'),
-- or request it be put into host memory (via
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults').
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlags'
type QueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits

-- | VkQueryPipelineStatisticFlags - Bitmask of
-- VkQueryPipelineStatisticFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlags' is a
-- bitmask type for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlagBits',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo'
type QueryPipelineStatisticFlags = QueryPipelineStatisticFlagBits

-- | VkQueryPool - Opaque handle to a query pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'Graphics.Vulkan.C.Core10.Query.vkCreateQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.vkDestroyQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults'
type QueryPool = VkQueryPool

-- | VkQueryPoolCreateFlags - Reserved for future use
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateFlags' is a bitmask
-- type for setting a mask, but is currently reserved for future use.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo'
type QueryPoolCreateFlags = VkQueryPoolCreateFlags

-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlags'
type QueryResultFlagBits = VkQueryResultFlagBits

-- | VkQueryResultFlags - Bitmask of VkQueryResultFlagBits
--
-- = Description
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlags' is a bitmask type
-- for setting a mask of zero or more
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults'
type QueryResultFlags = QueryResultFlagBits

-- | VkQueryType - Specify the type of queries managed by a query pool
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo'
type QueryType = VkQueryType
