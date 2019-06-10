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
#if defined(VK_USE_PLATFORM_GGP)
  , QueryPoolCreateInfo(..)
#endif
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
  , pattern QUERY_TYPE_RESERVED_8
  , pattern QUERY_TYPE_RESERVED_4
  , pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  , pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  , createQueryPool
  , destroyQueryPool
  , getQueryPoolResults
  , withQueryPool
  , pattern VK_QUERY_TYPE_RESERVED_4
  , pattern VK_QUERY_TYPE_RESERVED_8
  ) where

import Control.Exception
  ( bracket
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
  ( maybeWith
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
  )

#if defined(VK_USE_PLATFORM_GGP)
import Graphics.Vulkan.C.Core10.Core
  ( Zero(..)
  )
#endif
import Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlagBits(..)
  , VkQueryPoolCreateFlags(..)
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
import Graphics.Vulkan.C.Extensions.VK_EXT_transform_feedback
  ( pattern VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
  )
import Graphics.Vulkan.C.Extensions.VK_NV_ray_tracing
  ( pattern VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( AllocationCallbacks(..)
  , Device(..)
  , DeviceSize
  )

#if defined(VK_USE_PLATFORM_GGP)
import {-# source #-} Graphics.Vulkan.Marshal.SomeVkStruct
  ( SomeVkStruct
  )
#endif


-- No documentation found for TopLevel "QueryPipelineStatisticFlagBits"
type QueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits


{-# complete QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT, QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT, QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT, QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT, QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT, QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT, QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT, QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT, QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT, QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT, QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT :: QueryPipelineStatisticFlagBits #-}


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT


-- No documentation found for Nested "QueryPipelineStatisticFlagBits" "QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT :: (a ~ QueryPipelineStatisticFlagBits) => a
pattern QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT

-- No documentation found for TopLevel "QueryPipelineStatisticFlags"
type QueryPipelineStatisticFlags = QueryPipelineStatisticFlagBits

-- No documentation found for TopLevel "QueryPool"
type QueryPool = VkQueryPool

-- No documentation found for TopLevel "QueryPoolCreateFlags"
type QueryPoolCreateFlags = VkQueryPoolCreateFlags


-- No complete pragma for QueryPoolCreateFlags as it has no patterns


#if defined(VK_USE_PLATFORM_GGP)

-- No documentation found for TopLevel "VkQueryPoolCreateInfo"
data QueryPoolCreateInfo = QueryPoolCreateInfo
  { -- No documentation found for Nested "QueryPoolCreateInfo" "pNext"
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

instance Zero QueryPoolCreateInfo where
  zero = QueryPoolCreateInfo Nothing
                             zero
                             zero
                             zero
                             zero

#endif

-- No documentation found for TopLevel "QueryResultFlagBits"
type QueryResultFlagBits = VkQueryResultFlagBits


{-# complete QUERY_RESULT_64_BIT, QUERY_RESULT_WAIT_BIT, QUERY_RESULT_WITH_AVAILABILITY_BIT, QUERY_RESULT_PARTIAL_BIT :: QueryResultFlagBits #-}


-- No documentation found for Nested "QueryResultFlagBits" "QUERY_RESULT_64_BIT"
pattern QUERY_RESULT_64_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_64_BIT = VK_QUERY_RESULT_64_BIT


-- No documentation found for Nested "QueryResultFlagBits" "QUERY_RESULT_WAIT_BIT"
pattern QUERY_RESULT_WAIT_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_WAIT_BIT = VK_QUERY_RESULT_WAIT_BIT


-- No documentation found for Nested "QueryResultFlagBits" "QUERY_RESULT_WITH_AVAILABILITY_BIT"
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_WITH_AVAILABILITY_BIT = VK_QUERY_RESULT_WITH_AVAILABILITY_BIT


-- No documentation found for Nested "QueryResultFlagBits" "QUERY_RESULT_PARTIAL_BIT"
pattern QUERY_RESULT_PARTIAL_BIT :: (a ~ QueryResultFlagBits) => a
pattern QUERY_RESULT_PARTIAL_BIT = VK_QUERY_RESULT_PARTIAL_BIT

-- No documentation found for TopLevel "QueryResultFlags"
type QueryResultFlags = QueryResultFlagBits

-- No documentation found for TopLevel "QueryType"
type QueryType = VkQueryType


{-# complete QUERY_TYPE_OCCLUSION, QUERY_TYPE_PIPELINE_STATISTICS, QUERY_TYPE_TIMESTAMP, QUERY_TYPE_RESERVED_8, QUERY_TYPE_RESERVED_4, QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT, QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV :: QueryType #-}


-- No documentation found for Nested "QueryType" "QUERY_TYPE_OCCLUSION"
pattern QUERY_TYPE_OCCLUSION :: (a ~ QueryType) => a
pattern QUERY_TYPE_OCCLUSION = VK_QUERY_TYPE_OCCLUSION


-- No documentation found for Nested "QueryType" "QUERY_TYPE_PIPELINE_STATISTICS"
pattern QUERY_TYPE_PIPELINE_STATISTICS :: (a ~ QueryType) => a
pattern QUERY_TYPE_PIPELINE_STATISTICS = VK_QUERY_TYPE_PIPELINE_STATISTICS


-- No documentation found for Nested "QueryType" "QUERY_TYPE_TIMESTAMP"
pattern QUERY_TYPE_TIMESTAMP :: (a ~ QueryType) => a
pattern QUERY_TYPE_TIMESTAMP = VK_QUERY_TYPE_TIMESTAMP


-- No documentation found for Nested "QueryType" "QUERY_TYPE_RESERVED_8"
pattern QUERY_TYPE_RESERVED_8 :: (a ~ QueryType) => a
pattern QUERY_TYPE_RESERVED_8 = VK_QUERY_TYPE_RESERVED_8


-- No documentation found for Nested "QueryType" "QUERY_TYPE_RESERVED_4"
pattern QUERY_TYPE_RESERVED_4 :: (a ~ QueryType) => a
pattern QUERY_TYPE_RESERVED_4 = VK_QUERY_TYPE_RESERVED_4


-- No documentation found for Nested "QueryType" "QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT"
pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT :: (a ~ QueryType) => a
pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT = VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT


-- No documentation found for Nested "QueryType" "QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV :: (a ~ QueryType) => a
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV


-- No documentation found for TopLevel "vkCreateQueryPool"
createQueryPool :: Device ->  QueryPoolCreateInfo ->  Maybe AllocationCallbacks ->  IO (QueryPool)
createQueryPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkDestroyQueryPool"
destroyQueryPool :: Device ->  QueryPool ->  Maybe AllocationCallbacks ->  IO ()
destroyQueryPool = undefined {- {wrapped (pretty cName) :: Doc ()} -}


-- No documentation found for TopLevel "vkGetQueryPoolResults"
getQueryPoolResults :: Device ->  QueryPool ->  Word32 ->  Word32 ->  CSize ->  DeviceSize ->  QueryResultFlags ->  IO (VkResult, ByteString)
getQueryPoolResults = undefined {- {wrapped (pretty cName) :: Doc ()} -}

-- | A safe wrapper for 'createQueryPool' and 'destroyQueryPool' using 'bracket'
--
-- The allocated value must not be returned from the provided computation
withQueryPool
  :: Device -> QueryPoolCreateInfo -> Maybe AllocationCallbacks -> (QueryPool -> IO a) -> IO a
withQueryPool device queryPoolCreateInfo allocationCallbacks = bracket
  (createQueryPool device queryPoolCreateInfo allocationCallbacks)
  (\o -> destroyQueryPool device o allocationCallbacks)

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_RESERVED_4"
pattern VK_QUERY_TYPE_RESERVED_4 :: VkQueryType
pattern VK_QUERY_TYPE_RESERVED_4 = VkQueryType 1000024004

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_RESERVED_8"
pattern VK_QUERY_TYPE_RESERVED_8 :: VkQueryType
pattern VK_QUERY_TYPE_RESERVED_8 = VkQueryType 1000023008
