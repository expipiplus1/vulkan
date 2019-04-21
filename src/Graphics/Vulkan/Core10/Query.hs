{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language TypeFamilies #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Query
  ( QueryPipelineStatisticFlagBits
  , pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
  , pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
  , pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
  , pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
  , pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
  , pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
  , pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
  , pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
  , pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
  , pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
  , pattern QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
  , QueryPipelineStatisticFlags
  , QueryPool
  , QueryPoolCreateFlags
  , withCStructQueryPoolCreateInfo
  , fromCStructQueryPoolCreateInfo
  , QueryPoolCreateInfo(..)
  , QueryResultFlagBits
  , pattern QUERY_RESULT_64_BIT
  , pattern QUERY_RESULT_WAIT_BIT
  , pattern QUERY_RESULT_WITH_AVAILABILITY_BIT
  , pattern QUERY_RESULT_PARTIAL_BIT
  , QueryResultFlags
  , QueryType
  , pattern QUERY_TYPE_OCCLUSION
  , pattern QUERY_TYPE_PIPELINE_STATISTICS
  , pattern QUERY_TYPE_TIMESTAMP
  , createQueryPool
  , destroyQueryPool
  , getQueryPoolResults
  , withQueryPool
  ) where

import Control.Exception
  ( bracket
  , throwIO
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , Zero(..)
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
  , vkCreateQueryPool
  , vkDestroyQueryPool
  , vkGetQueryPoolResults
  , pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_RESULT_64_BIT
  , pattern VK_QUERY_RESULT_PARTIAL_BIT
  , pattern VK_QUERY_RESULT_WAIT_BIT
  , pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
  , pattern VK_QUERY_TYPE_OCCLUSION
  , pattern VK_QUERY_TYPE_PIPELINE_STATISTICS
  , pattern VK_QUERY_TYPE_TIMESTAMP
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


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT'
-- specifies that queries managed by the pool will count the number of
-- vertices processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing input assembly>
-- stage. Vertices corresponding to incomplete primitives /may/ contribute
-- to the count.
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT'
-- specifies that queries managed by the pool will count the number of
-- primitives processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing input assembly>
-- stage. If primitive restart is enabled, restarting the primitive
-- topology has no effect on the count. Incomplete primitives /may/ be
-- counted.
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- vertex shader invocations. This counter’s value is incremented each time
-- a vertex shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-vertex-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- geometry shader invocations. This counter’s value is incremented each
-- time a geometry shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-geometry-execution invoked>.
-- In the case of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#geometry-invocations instanced geometry shaders>,
-- the geometry shader invocations count is incremented for each separate
-- instanced invocation.
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT'
-- specifies that queries managed by the pool will count the number of
-- primitives generated by geometry shader invocations. The counter’s value
-- is incremented each time the geometry shader emits a primitive.
-- Restarting primitive topology using the SPIR-V instructions
-- @OpEndPrimitive@ or @OpEndStreamPrimitive@ has no effect on the geometry
-- shader output primitives count.
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- primitives processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>
-- stage of the pipeline. The counter’s value is incremented each time a
-- primitive reaches the primitive clipping stage.
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT'
-- specifies that queries managed by the pool will count the number of
-- primitives output by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>
-- stage of the pipeline. The counter’s value is incremented each time a
-- primitive passes the primitive clipping stage. The actual number of
-- primitives output by the primitive clipping stage for a particular input
-- primitive is implementation-dependent but /must/ satisfy the following
-- conditions:
--
-- -   If at least one vertex of the input primitive lies inside the
--     clipping volume, the counter is incremented by one or more.
--
-- -   Otherwise, the counter is incremented by zero or more.
--
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- fragment shader invocations. The counter’s value is incremented each
-- time the fragment shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-fragment-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT'
-- specifies that queries managed by the pool will count the number of
-- patches processed by the tessellation control shader. The counter’s
-- value is incremented once for each patch for which a tessellation
-- control shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-tessellation-control-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- invocations of the tessellation evaluation shader. The counter’s value
-- is incremented each time the tessellation evaluation shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-tessellation-evaluation-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- compute shader invocations. The counter’s value is incremented every
-- time the compute shader is invoked. Implementations /may/ skip the
-- execution of certain compute shader invocations or execute additional
-- compute shader invocations for implementation-dependent reasons as long
-- as the results of rendering otherwise remain unchanged.
pattern QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT

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


-- | VkQueryPoolCreateInfo - Structure specifying parameters of a newly
-- created query pool
--
-- = Description
--
-- @pipelineStatistics@ is ignored if @queryType@ is not
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled, @queryType@ /must/ not be
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
--
-- -   If @queryType@ is
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS',
--     @pipelineStatistics@ /must/ be a valid combination of
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlagBits'
--     values
--
-- Unresolved directive in VkQueryPoolCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkQueryPoolCreateInfo.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPipelineStatisticFlags',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateFlags',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryType',
-- 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
-- 'Graphics.Vulkan.C.Core10.Query.vkCreateQueryPool'
data QueryPoolCreateInfo = QueryPoolCreateInfo
  { -- Univalued member elided
  -- No documentation found for Nested "QueryPoolCreateInfo" "pNext"
  next :: Maybe SomeVkStruct
  , -- No documentation found for Nested "QueryPoolCreateInfo" "flags"
  flags :: QueryPoolCreateFlags
  , -- No documentation found for Nested "QueryPoolCreateInfo" "queryType"
  queryType :: QueryType
  , -- No documentation found for Nested "QueryPoolCreateInfo" "queryCount"
  queryCount :: Word32
  , -- No documentation found for Nested "QueryPoolCreateInfo" "pipelineStatistics"
  pipelineStatistics :: QueryPipelineStatisticFlags
  }
  deriving (Show, Eq)

-- | A function to temporarily allocate memory for a 'VkQueryPoolCreateInfo' and
-- marshal a 'QueryPoolCreateInfo' into it. The 'VkQueryPoolCreateInfo' is only valid inside
-- the provided computation and must not be returned out of it.
withCStructQueryPoolCreateInfo :: QueryPoolCreateInfo -> (VkQueryPoolCreateInfo -> IO a) -> IO a
withCStructQueryPoolCreateInfo marshalled cont = maybeWith withSomeVkStruct (next (marshalled :: QueryPoolCreateInfo)) (\pPNext -> cont (VkQueryPoolCreateInfo VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO pPNext (flags (marshalled :: QueryPoolCreateInfo)) (queryType (marshalled :: QueryPoolCreateInfo)) (queryCount (marshalled :: QueryPoolCreateInfo)) (pipelineStatistics (marshalled :: QueryPoolCreateInfo))))

-- | A function to read a 'VkQueryPoolCreateInfo' and all additional
-- structures in the pointer chain into a 'QueryPoolCreateInfo'.
fromCStructQueryPoolCreateInfo :: VkQueryPoolCreateInfo -> IO QueryPoolCreateInfo
fromCStructQueryPoolCreateInfo c = QueryPoolCreateInfo <$> -- Univalued Member elided
                                                       maybePeek peekVkStruct (castPtr (vkPNext (c :: VkQueryPoolCreateInfo)))
                                                       <*> pure (vkFlags (c :: VkQueryPoolCreateInfo))
                                                       <*> pure (vkQueryType (c :: VkQueryPoolCreateInfo))
                                                       <*> pure (vkQueryCount (c :: VkQueryPoolCreateInfo))
                                                       <*> pure (vkPipelineStatistics (c :: VkQueryPoolCreateInfo))

instance Zero QueryPoolCreateInfo where
  zero = QueryPoolCreateInfo Nothing
                             zero
                             zero
                             zero
                             zero


-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlags'
type QueryResultFlagBits = VkQueryResultFlagBits


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' specifies the
-- results will be written as an array of 64-bit unsigned integer values.
-- If this bit is not set, the results will be written as an array of
-- 32-bit unsigned integer values.
pattern QUERY_RESULT_64_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_64_BIT = VK_QUERY_RESULT_64_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' specifies that
-- Vulkan will wait for each query’s status to become available before
-- retrieving its results.
pattern QUERY_RESULT_WAIT_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_WAIT_BIT = VK_QUERY_RESULT_WAIT_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- specifies that the availability status accompanies the results.
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = VK_QUERY_RESULT_WITH_AVAILABILITY_BIT


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT' specifies
-- that returning partial results is acceptable.
pattern QUERY_RESULT_PARTIAL_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_PARTIAL_BIT = VK_QUERY_RESULT_PARTIAL_BIT

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


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_OCCLUSION' specifies an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-occlusion occlusion query>.
pattern QUERY_TYPE_OCCLUSION :: (a ~ QueryType) => a
pattern QUERY_TYPE_OCCLUSION = VK_QUERY_TYPE_OCCLUSION


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_PIPELINE_STATISTICS'
-- specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-pipestats pipeline statistics query>.
pattern QUERY_TYPE_PIPELINE_STATISTICS :: (a ~ QueryType) => a
pattern QUERY_TYPE_PIPELINE_STATISTICS = VK_QUERY_TYPE_PIPELINE_STATISTICS


-- | 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_TIMESTAMP' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-timestamps timestamp query>.
pattern QUERY_TYPE_TIMESTAMP :: (a ~ QueryType) => a
pattern QUERY_TYPE_TIMESTAMP = VK_QUERY_TYPE_TIMESTAMP


-- | vkCreateQueryPool - Create a new query pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the query pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo' structure
--     containing the number and type of queries to be managed by the pool.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pQueryPool@ is a pointer to a
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPool' handle in which the
--     resulting query pool object is returned.
--
-- = Description
--
-- Unresolved directive in vkCreateQueryPool.txt -
-- include::{generated}\/validity\/protos\/vkCreateQueryPool.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo'
createQueryPool :: Device ->  QueryPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (QueryPool)
createQueryPool = \(Device device' commandTable) -> \createInfo' -> \allocator -> alloca (\pQueryPool' -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> (\marshalled -> withCStructQueryPoolCreateInfo marshalled . flip with) createInfo' (\pCreateInfo' -> vkCreateQueryPool commandTable device' pCreateInfo' pAllocator pQueryPool' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> (peek pQueryPool')))))


-- | vkDestroyQueryPool - Destroy a query pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the query pool.
--
-- -   @queryPool@ is the query pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @queryPool@ /must/ have
--     completed execution
--
-- -   If
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @queryPool@ was created, a compatible set of
--     callbacks /must/ be provided here
--
-- -   If no
--     'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks'
--     were provided when @queryPool@ was created, @pAllocator@ /must/ be
--     @NULL@
--
-- Unresolved directive in vkDestroyQueryPool.txt -
-- include::{generated}\/validity\/protos\/vkDestroyQueryPool.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool'
destroyQueryPool :: Device ->  QueryPool ->  Maybe AllocationCallbacks ->  IO ()
destroyQueryPool = \(Device device' commandTable) -> \queryPool' -> \allocator -> maybeWith (\marshalled -> withCStructAllocationCallbacks marshalled . flip with) allocator (\pAllocator -> vkDestroyQueryPool commandTable device' queryPool' pAllocator *> (pure ()))


-- | vkGetQueryPoolResults - Copy results of queries in a query pool to a
-- host memory region
--
-- = Parameters
--
-- -   @device@ is the logical device that owns the query pool.
--
-- -   @queryPool@ is the query pool managing the queries containing the
--     desired results.
--
-- -   @firstQuery@ is the initial query index.
--
-- -   @queryCount@ is the number of queries. @firstQuery@ and @queryCount@
--     together define a range of queries. For pipeline statistics queries,
--     each query index in the pool contains one integer value for each bit
--     that is enabled in
--     'Graphics.Vulkan.C.Core10.Query.VkQueryPoolCreateInfo'::@pipelineStatistics@
--     when the pool is created.
--
-- -   @dataSize@ is the size in bytes of the buffer pointed to by @pData@.
--
-- -   @pData@ is a pointer to a user-allocated buffer where the results
--     will be written
--
-- -   @stride@ is the stride in bytes between results for individual
--     queries within @pData@.
--
-- -   @flags@ is a bitmask of
--     'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlagBits' specifying
--     how and when results are returned.
--
-- = Description
--
-- If no bits are set in @flags@, and all requested queries are in the
-- available state, results are written as an array of 32-bit unsigned
-- integer values. The behavior when not all queries are available, is
-- described
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-wait-bit-not-set below>.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is not set
-- and the result overflows a 32-bit value, the value /may/ either wrap or
-- saturate. Similarly, if
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is set and the
-- result overflows a 64-bit value, the value /may/ either wrap or
-- saturate.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' is set,
-- Vulkan will wait for each query to be in the available state before
-- retrieving the numerical results for that query. In this case,
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults' is guaranteed to
-- succeed and return 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' if the
-- queries become available in a finite time (i.e. if they have been issued
-- and not reset). If queries will never finish (e.g. due to being reset
-- but not issued), then
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults' /may/ not return
-- in finite time.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' and
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT' are both
-- not set then no result values are written to @pData@ for queries that
-- are in the unavailable state at the time of the call, and
-- 'Graphics.Vulkan.C.Core10.Query.vkGetQueryPoolResults' returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_NOT_READY'. However, availability
-- state is still written to @pData@ for those queries if
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set.
--
-- __Note__
--
-- Applications /must/ take care to ensure that use of the
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' bit has the
-- desired effect.
--
-- For example, if a query has been used previously and a command buffer
-- records the commands
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery', and
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery' for that
-- query, then the query will remain in the available state until the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool'
-- command executes on a queue. Applications /can/ use fences or events to
-- ensure that a query has already been reset before checking for its
-- results or availability status. Otherwise, a stale value could be
-- returned from a previous use of the query.
--
-- The above also applies when
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' is used in
-- combination with
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'.
-- In this case, the returned availability status /may/ reflect the result
-- of a previous use of the query unless the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool'
-- command has been executed since the last use of the query.
--
-- __Note__
--
-- Applications /can/ double-buffer query pool usage, with a pool per
-- frame, and reset queries at the end of the frame in which they are read.
--
-- If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT' is set,
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WAIT_BIT' is not set,
-- and the query’s status is unavailable, an intermediate result value
-- between zero and the final result value is written to @pData@ for that
-- query.
--
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT' /must/ not
-- be used if the pool’s @queryType@ is
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_TIMESTAMP'.
--
-- If
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is set, the final integer value written for each query is non-zero if
-- the query’s status was available or zero if the status was unavailable.
-- When
-- 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'
-- is used, implementations /must/ guarantee that if they return a non-zero
-- availability value then the numerical results /must/ be valid, assuming
-- the results are not reset by a subsequent command.
--
-- __Note__
--
-- Satisfying this guarantee /may/ require careful ordering by the
-- application, e.g. to read the availability status before reading the
-- results.
--
-- == Valid Usage
--
-- -   @firstQuery@ /must/ be less than the number of queries in
--     @queryPool@
--
-- -   If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is not
--     set in @flags@ then @pData@ and @stride@ /must/ be multiples of @4@
--
-- -   If 'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_64_BIT' is set in
--     @flags@ then @pData@ and @stride@ /must/ be multiples of @8@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- -   @dataSize@ /must/ be large enough to contain the result of each
--     query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_TYPE_TIMESTAMP', @flags@
--     /must/ not contain
--     'Graphics.Vulkan.C.Core10.Query.VK_QUERY_RESULT_PARTIAL_BIT'
--
-- Unresolved directive in vkGetQueryPoolResults.txt -
-- include::{generated}\/validity\/protos\/vkGetQueryPoolResults.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryPool',
-- 'Graphics.Vulkan.C.Core10.Query.VkQueryResultFlags'
getQueryPoolResults :: Device ->  QueryPool ->  Word32 ->  Word32 ->  CSize ->  DeviceSize ->  QueryResultFlags ->  IO (VkResult, ByteString)
getQueryPoolResults = \(Device device' commandTable) -> \queryPool' -> \firstQuery' -> \queryCount' -> \dataSize' -> \stride' -> \flags' -> allocaArray (fromIntegral dataSize') (\pData' -> vkGetQueryPoolResults commandTable device' queryPool' firstQuery' queryCount' dataSize' (castPtr pData') stride' flags' >>= (\ret -> when (ret < VK_SUCCESS) (throwIO (VulkanException ret)) *> ((,) <$> pure ret<*>(packCStringLen (pData', (fromIntegral dataSize'))))))

-- | A safe wrapper for 'createQueryPool' and 'destroyQueryPool' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withQueryPool
  :: Device -> QueryPoolCreateInfo -> Maybe (AllocationCallbacks) -> (QueryPool -> IO a) -> IO a
withQueryPool device queryPoolCreateInfo allocationCallbacks = bracket
  (createQueryPool device queryPoolCreateInfo allocationCallbacks)
  (\o -> destroyQueryPool device o allocationCallbacks)
