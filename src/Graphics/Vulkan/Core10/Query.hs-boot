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
  , VkQueryPool
  , VkQueryPoolCreateFlags
  , VkQueryResultFlagBits
  , VkQueryType
  )


-- No documentation found for TopLevel "QueryPipelineStatisticFlagBits"
type QueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits

-- No documentation found for TopLevel "QueryPipelineStatisticFlags"
type QueryPipelineStatisticFlags = QueryPipelineStatisticFlagBits

-- No documentation found for TopLevel "QueryPool"
type QueryPool = VkQueryPool

-- No documentation found for TopLevel "QueryPoolCreateFlags"
type QueryPoolCreateFlags = VkQueryPoolCreateFlags

-- No documentation found for TopLevel "QueryResultFlagBits"
type QueryResultFlagBits = VkQueryResultFlagBits

-- No documentation found for TopLevel "QueryResultFlags"
type QueryResultFlags = QueryResultFlagBits

-- No documentation found for TopLevel "QueryType"
type QueryType = VkQueryType
