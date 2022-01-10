{-# language CPP #-}
-- No documentation found for Chapter "QueryPipelineStatisticFlagBits"
module Vulkan.Core10.Enums.QueryPipelineStatisticFlagBits  ( QueryPipelineStatisticFlags
                                                           , QueryPipelineStatisticFlagBits( QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
                                                                                           , QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
                                                                                           , ..
                                                                                           )
                                                           ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type QueryPipelineStatisticFlags = QueryPipelineStatisticFlagBits

-- | VkQueryPipelineStatisticFlagBits - Bitmask specifying queried pipeline
-- statistics
--
-- = Description
--
-- These values are intended to measure relative statistics on one
-- implementation. Various device architectures will count these values
-- differently. Any or all counters /may/ be affected by the issues
-- described in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-operation-undefined Query Operation>.
--
-- Note
--
-- For example, tile-based rendering devices /may/ need to replay the scene
-- multiple times, affecting some of the counts.
--
-- If a pipeline has @rasterizerDiscardEnable@ enabled, implementations
-- /may/ discard primitives after the final
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#pipeline-graphics-subsets-pre-rasterization pre-rasterization shader stage>.
-- As a result, if @rasterizerDiscardEnable@ is enabled, the clipping input
-- and output primitives counters /may/ not be incremented.
--
-- When a pipeline statistics query finishes, the result for that query is
-- marked as available. The application /can/ copy the result to a buffer
-- (via 'Vulkan.Core10.CommandBufferBuilding.cmdCopyQueryPoolResults'), or
-- request it be put into host memory (via
-- 'Vulkan.Core10.Query.getQueryPoolResults').
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'QueryPipelineStatisticFlags'
newtype QueryPipelineStatisticFlagBits = QueryPipelineStatisticFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT' specifies that
-- queries managed by the pool will count the number of vertices processed
-- by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing input assembly>
-- stage. Vertices corresponding to incomplete primitives /may/ contribute
-- to the count.
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT             = QueryPipelineStatisticFlagBits 0x00000001
-- | 'QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT' specifies that
-- queries managed by the pool will count the number of primitives
-- processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#drawing input assembly>
-- stage. If primitive restart is enabled, restarting the primitive
-- topology has no effect on the count. Incomplete primitives /may/ be
-- counted.
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT           = QueryPipelineStatisticFlagBits 0x00000002
-- | 'QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT' specifies that
-- queries managed by the pool will count the number of vertex shader
-- invocations. This counter’s value is incremented each time a vertex
-- shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-vertex-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT           = QueryPipelineStatisticFlagBits 0x00000004
-- | 'QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT' specifies
-- that queries managed by the pool will count the number of geometry
-- shader invocations. This counter’s value is incremented each time a
-- geometry shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-geometry-execution invoked>.
-- In the case of
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#geometry-invocations instanced geometry shaders>,
-- the geometry shader invocations count is incremented for each separate
-- instanced invocation.
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT         = QueryPipelineStatisticFlagBits 0x00000008
-- | 'QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT' specifies that
-- queries managed by the pool will count the number of primitives
-- generated by geometry shader invocations. The counter’s value is
-- incremented each time the geometry shader emits a primitive. Restarting
-- primitive topology using the SPIR-V instructions @OpEndPrimitive@ or
-- @OpEndStreamPrimitive@ has no effect on the geometry shader output
-- primitives count.
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT          = QueryPipelineStatisticFlagBits 0x00000010
-- | 'QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT' specifies that
-- queries managed by the pool will count the number of primitives
-- processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>
-- stage of the pipeline. The counter’s value is incremented each time a
-- primitive reaches the primitive clipping stage.
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT                = QueryPipelineStatisticFlagBits 0x00000020
-- | 'QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT' specifies that
-- queries managed by the pool will count the number of primitives output
-- by the
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>
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
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT                 = QueryPipelineStatisticFlagBits 0x00000040
-- | 'QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT' specifies
-- that queries managed by the pool will count the number of fragment
-- shader invocations. The counter’s value is incremented each time the
-- fragment shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#fragops-shader invoked>.
pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT         = QueryPipelineStatisticFlagBits 0x00000080
-- | 'QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT'
-- specifies that queries managed by the pool will count the number of
-- patches processed by the tessellation control shader. The counter’s
-- value is incremented once for each patch for which a tessellation
-- control shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-tessellation-control-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = QueryPipelineStatisticFlagBits 0x00000100
-- | 'QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- invocations of the tessellation evaluation shader. The counter’s value
-- is incremented each time the tessellation evaluation shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#shaders-tessellation-evaluation-execution invoked>.
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT =
  QueryPipelineStatisticFlagBits 0x00000200
-- | 'QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT' specifies that
-- queries managed by the pool will count the number of compute shader
-- invocations. The counter’s value is incremented every time the compute
-- shader is invoked. Implementations /may/ skip the execution of certain
-- compute shader invocations or execute additional compute shader
-- invocations for implementation-dependent reasons as long as the results
-- of rendering otherwise remain unchanged.
pattern QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = QueryPipelineStatisticFlagBits 0x00000400

conNameQueryPipelineStatisticFlagBits :: String
conNameQueryPipelineStatisticFlagBits = "QueryPipelineStatisticFlagBits"

enumPrefixQueryPipelineStatisticFlagBits :: String
enumPrefixQueryPipelineStatisticFlagBits = "QUERY_PIPELINE_STATISTIC_"

showTableQueryPipelineStatisticFlagBits :: [(QueryPipelineStatisticFlagBits, String)]
showTableQueryPipelineStatisticFlagBits =
  [ (QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT            , "INPUT_ASSEMBLY_VERTICES_BIT")
  , (QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT          , "INPUT_ASSEMBLY_PRIMITIVES_BIT")
  , (QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT          , "VERTEX_SHADER_INVOCATIONS_BIT")
  , (QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT        , "GEOMETRY_SHADER_INVOCATIONS_BIT")
  , (QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT         , "GEOMETRY_SHADER_PRIMITIVES_BIT")
  , (QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT               , "CLIPPING_INVOCATIONS_BIT")
  , (QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT                , "CLIPPING_PRIMITIVES_BIT")
  , (QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT        , "FRAGMENT_SHADER_INVOCATIONS_BIT")
  , (QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT, "TESSELLATION_CONTROL_SHADER_PATCHES_BIT")
  , ( QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
    , "TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
    )
  , (QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT, "COMPUTE_SHADER_INVOCATIONS_BIT")
  ]

instance Show QueryPipelineStatisticFlagBits where
  showsPrec = enumShowsPrec enumPrefixQueryPipelineStatisticFlagBits
                            showTableQueryPipelineStatisticFlagBits
                            conNameQueryPipelineStatisticFlagBits
                            (\(QueryPipelineStatisticFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read QueryPipelineStatisticFlagBits where
  readPrec = enumReadPrec enumPrefixQueryPipelineStatisticFlagBits
                          showTableQueryPipelineStatisticFlagBits
                          conNameQueryPipelineStatisticFlagBits
                          QueryPipelineStatisticFlagBits

