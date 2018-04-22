{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core10.Query
  ( VkQueryType(..)
  , pattern VK_QUERY_TYPE_OCCLUSION
  , pattern VK_QUERY_TYPE_PIPELINE_STATISTICS
  , pattern VK_QUERY_TYPE_TIMESTAMP
  , VkQueryPoolCreateFlags(..)
  , VkQueryResultFlagBits(..)
  , pattern VK_QUERY_RESULT_64_BIT
  , pattern VK_QUERY_RESULT_WAIT_BIT
  , pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
  , pattern VK_QUERY_RESULT_PARTIAL_BIT
  , VkQueryPipelineStatisticFlagBits(..)
  , pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT
  , pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT
  , VkQueryPool
  , vkCreateQueryPool
  , vkDestroyQueryPool
  , vkGetQueryPoolResults
  , VkQueryPoolCreateInfo(..)
  , VkQueryResultFlags
  , VkQueryPipelineStatisticFlags
  ) where

import Data.Bits
  ( Bits
  , FiniteBits
  )
import Data.Int
  ( Int32
  )
import Data.Word
  ( Word32
  )
import Foreign.C.Types
  ( CSize(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )
import GHC.Read
  ( choose
  , expectP
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )
import Text.ParserCombinators.ReadPrec
  ( (+++)
  , prec
  , step
  )
import Text.Read
  ( Read(..)
  , parens
  )
import Text.Read.Lex
  ( Lexeme(Ident)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , VkFlags
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )


-- ** VkQueryType

-- | VkQueryType - Specify the type of queries managed by a query pool
--
-- = See Also
--
-- 'VkQueryPoolCreateInfo'
newtype VkQueryType = VkQueryType Int32
  deriving (Eq, Ord, Storable)

instance Show VkQueryType where
  showsPrec _ VK_QUERY_TYPE_OCCLUSION = showString "VK_QUERY_TYPE_OCCLUSION"
  showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
  showsPrec _ VK_QUERY_TYPE_TIMESTAMP = showString "VK_QUERY_TYPE_TIMESTAMP"
  showsPrec p (VkQueryType x) = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
  readPrec = parens ( choose [ ("VK_QUERY_TYPE_OCCLUSION",           pure VK_QUERY_TYPE_OCCLUSION)
                             , ("VK_QUERY_TYPE_PIPELINE_STATISTICS", pure VK_QUERY_TYPE_PIPELINE_STATISTICS)
                             , ("VK_QUERY_TYPE_TIMESTAMP",           pure VK_QUERY_TYPE_TIMESTAMP)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryType")
                        v <- step readPrec
                        pure (VkQueryType v)
                        )
                    )

-- | @VK_QUERY_TYPE_OCCLUSION@ specifies an [occlusion
-- query](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-occlusion).
pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType
pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- | @VK_QUERY_TYPE_PIPELINE_STATISTICS@ specifies a [pipeline statistics
-- query](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-pipestats).
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

-- | @VK_QUERY_TYPE_TIMESTAMP@ specifies a [timestamp
-- query](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-timestamps).
pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType
pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2
-- ** VkQueryPoolCreateFlags

-- | VkQueryPoolCreateFlags - Reserved for future use
--
-- = Description
--
-- @VkQueryPoolCreateFlags@ is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkQueryPoolCreateInfo'
newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryPoolCreateFlags where
  
  showsPrec p (VkQueryPoolCreateFlags x) = showParen (p >= 11) (showString "VkQueryPoolCreateFlags " . showsPrec 11 x)

instance Read VkQueryPoolCreateFlags where
  readPrec = parens ( choose [ 
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryPoolCreateFlags")
                        v <- step readPrec
                        pure (VkQueryPoolCreateFlags v)
                        )
                    )


-- ** VkQueryResultFlagBits

-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = See Also
--
-- 'VkQueryResultFlags'
newtype VkQueryResultFlagBits = VkQueryResultFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryResultFlagBits where
  showsPrec _ VK_QUERY_RESULT_64_BIT = showString "VK_QUERY_RESULT_64_BIT"
  showsPrec _ VK_QUERY_RESULT_WAIT_BIT = showString "VK_QUERY_RESULT_WAIT_BIT"
  showsPrec _ VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = showString "VK_QUERY_RESULT_WITH_AVAILABILITY_BIT"
  showsPrec _ VK_QUERY_RESULT_PARTIAL_BIT = showString "VK_QUERY_RESULT_PARTIAL_BIT"
  showsPrec p (VkQueryResultFlagBits x) = showParen (p >= 11) (showString "VkQueryResultFlagBits " . showsPrec 11 x)

instance Read VkQueryResultFlagBits where
  readPrec = parens ( choose [ ("VK_QUERY_RESULT_64_BIT",                pure VK_QUERY_RESULT_64_BIT)
                             , ("VK_QUERY_RESULT_WAIT_BIT",              pure VK_QUERY_RESULT_WAIT_BIT)
                             , ("VK_QUERY_RESULT_WITH_AVAILABILITY_BIT", pure VK_QUERY_RESULT_WITH_AVAILABILITY_BIT)
                             , ("VK_QUERY_RESULT_PARTIAL_BIT",           pure VK_QUERY_RESULT_PARTIAL_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryResultFlagBits")
                        v <- step readPrec
                        pure (VkQueryResultFlagBits v)
                        )
                    )

-- | @VK_QUERY_RESULT_64_BIT@ specifies the results will be written as an
-- array of 64-bit unsigned integer values. If this bit is not set, the
-- results will be written as an array of 32-bit unsigned integer values.
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 0x00000001

-- | @VK_QUERY_RESULT_WAIT_BIT@ specifies that Vulkan will wait for each
-- query’s status to become available before retrieving its results.
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 0x00000002

-- | @VK_QUERY_RESULT_WITH_AVAILABILITY_BIT@ specifies that the availability
-- status accompanies the results.
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlagBits 0x00000004

-- | @VK_QUERY_RESULT_PARTIAL_BIT@ specifies that returning partial results
-- is acceptable.
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 0x00000008
-- ** VkQueryPipelineStatisticFlagBits

-- | VkQueryPipelineStatisticFlagBits - Bitmask specifying queried pipeline
-- statistics
--
-- = Description
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT@ specifies
--     that queries managed by the pool will count the number of vertices
--     processed by the [input
--     assembly](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing)
--     stage. Vertices corresponding to incomplete primitives /may/
--     contribute to the count.
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT@
--     specifies that queries managed by the pool will count the number of
--     primitives processed by the [input
--     assembly](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#drawing)
--     stage. If primitive restart is enabled, restarting the primitive
--     topology has no effect on the count. Incomplete primitives /may/ be
--     counted.
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT@
--     specifies that queries managed by the pool will count the number of
--     vertex shader invocations. This counter’s value is incremented each
--     time a vertex shader is
--     [invoked](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-vertex-execution).
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT@
--     specifies that queries managed by the pool will count the number of
--     geometry shader invocations. This counter’s value is incremented
--     each time a geometry shader is
--     [invoked](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-geometry-execution).
--     In the case of [instanced geometry
--     shaders](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#geometry-invocations),
--     the geometry shader invocations count is incremented for each
--     separate instanced invocation.
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT@
--     specifies that queries managed by the pool will count the number of
--     primitives generated by geometry shader invocations. The counter’s
--     value is incremented each time the geometry shader emits a
--     primitive. Restarting primitive topology using the SPIR-V
--     instructions @OpEndPrimitive@ or @OpEndStreamPrimitive@ has no
--     effect on the geometry shader output primitives count.
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT@ specifies
--     that queries managed by the pool will count the number of primitives
--     processed by the [Primitive
--     Clipping](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vertexpostproc-clipping)
--     stage of the pipeline. The counter’s value is incremented each time
--     a primitive reaches the primitive clipping stage.
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT@ specifies that
--     queries managed by the pool will count the number of primitives
--     output by the [Primitive
--     Clipping](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#vertexpostproc-clipping)
--     stage of the pipeline. The counter’s value is incremented each time
--     a primitive passes the primitive clipping stage. The actual number
--     of primitives output by the primitive clipping stage for a
--     particular input primitive is implementation-dependent but /must/
--     satisfy the following conditions:
--
--     -   If at least one vertex of the input primitive lies inside the
--         clipping volume, the counter is incremented by one or more.
--
--     -   Otherwise, the counter is incremented by zero or more.
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT@
--     specifies that queries managed by the pool will count the number of
--     fragment shader invocations. The counter’s value is incremented each
--     time the fragment shader is
--     [invoked](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-fragment-execution).
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT@
--     specifies that queries managed by the pool will count the number of
--     patches processed by the tessellation control shader. The counter’s
--     value is incremented once for each patch for which a tessellation
--     control shader is
--     [invoked](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-tessellation-control-execution).
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT@
--     specifies that queries managed by the pool will count the number of
--     invocations of the tessellation evaluation shader. The counter’s
--     value is incremented each time the tessellation evaluation shader is
--     [invoked](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#shaders-tessellation-evaluation-execution).
--
-- -   @VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT@
--     specifies that queries managed by the pool will count the number of
--     compute shader invocations. The counter’s value is incremented every
--     time the compute shader is invoked. Implementations /may/ skip the
--     execution of certain compute shader invocations or execute
--     additional compute shader invocations for implementation-dependent
--     reasons as long as the results of rendering otherwise remain
--     unchanged.
--
-- These values are intended to measure relative statistics on one
-- implementation. Various device architectures will count these values
-- differently. Any or all counters /may/ be affected by the issues
-- described in [Query
-- Operation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-undefined).
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
-- (via @vkCmdCopyQueryPoolResults@), or request it be put into host memory
-- (via @vkGetQueryPoolResults@).
--
-- = See Also
--
-- 'VkQueryPipelineStatisticFlags'
newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits)

instance Show VkQueryPipelineStatisticFlagBits where
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
  showsPrec _ VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = showString "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
  showsPrec p (VkQueryPipelineStatisticFlagBits x) = showParen (p >= 11) (showString "VkQueryPipelineStatisticFlagBits " . showsPrec 11 x)

instance Read VkQueryPipelineStatisticFlagBits where
  readPrec = parens ( choose [ ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT",                    pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT",                  pure VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT",                  pure VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT",                pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT",                 pure VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT",                       pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT",                        pure VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT",                pure VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT",        pure VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT", pure VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT)
                             , ("VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT",                 pure VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT)
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryPipelineStatisticFlagBits")
                        v <- step readPrec
                        pure (VkQueryPipelineStatisticFlagBits v)
                        )
                    )

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlagBits 0x00000001

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000002

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000004

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000008

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000010

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000020

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000040

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000080

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlagBits 0x00000100

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000200

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000400
-- | Dummy data to tag the 'Ptr' with
data VkQueryPool_T
-- | VkQueryPool - Opaque handle to a query pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'vkCreateQueryPool', 'vkDestroyQueryPool', 'vkGetQueryPoolResults'
type VkQueryPool = Ptr VkQueryPool_T
-- | vkCreateQueryPool - Create a new query pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the query pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     @VkQueryPoolCreateInfo@ structure containing the number and type of
--     queries to be managed by the pool.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- -   @pQueryPool@ is a pointer to a @VkQueryPool@ handle in which the
--     resulting query pool object is returned.
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @pCreateInfo@ /must/ be a valid pointer to a valid
--     @VkQueryPoolCreateInfo@ structure
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   @pQueryPool@ /must/ be a valid pointer to a @VkQueryPool@ handle
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkQueryPool',
-- 'VkQueryPoolCreateInfo'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateQueryPool" vkCreateQueryPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
-- | vkDestroyQueryPool - Destroy a query pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that destroys the query pool.
--
-- -   @queryPool@ is the query pool to destroy.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     [Memory
--     Allocation](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#memory-allocation)
--     chapter.
--
-- == Valid Usage
--
-- -   All submitted commands that refer to @queryPool@ /must/ have
--     completed execution
--
-- -   If @VkAllocationCallbacks@ were provided when @queryPool@ was
--     created, a compatible set of callbacks /must/ be provided here
--
-- -   If no @VkAllocationCallbacks@ were provided when @queryPool@ was
--     created, @pAllocator@ /must/ be @NULL@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   If @queryPool@ is not
--     'Graphics.Vulkan.Core10.Constants.VK_NULL_HANDLE', @queryPool@
--     /must/ be a valid @VkQueryPool@ handle
--
-- -   If @pAllocator@ is not @NULL@, @pAllocator@ /must/ be a valid
--     pointer to a valid @VkAllocationCallbacks@ structure
--
-- -   If @queryPool@ is a valid handle, it /must/ have been created,
--     allocated, or retrieved from @device@
--
-- == Host Synchronization
--
-- -   Host access to @queryPool@ /must/ be externally synchronized
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkAllocationCallbacks',
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', 'VkQueryPool'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyQueryPool" vkDestroyQueryPool :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
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
--     that is enabled in 'VkQueryPoolCreateInfo'::@pipelineStatistics@
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
-- -   @flags@ is a bitmask of 'VkQueryResultFlagBits' specifying how and
--     when results are returned.
--
-- = Description
--
-- If no bits are set in @flags@, and all requested queries are in the
-- available state, results are written as an array of 32-bit unsigned
-- integer values. The behavior when not all queries are available, is
-- described
-- [below](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-wait-bit-not-set).
--
-- If @VK_QUERY_RESULT_64_BIT@ is not set and the result overflows a 32-bit
-- value, the value /may/ either wrap or saturate. Similarly, if
-- @VK_QUERY_RESULT_64_BIT@ is set and the result overflows a 64-bit value,
-- the value /may/ either wrap or saturate.
--
-- If @VK_QUERY_RESULT_WAIT_BIT@ is set, Vulkan will wait for each query to
-- be in the available state before retrieving the numerical results for
-- that query. In this case, @vkGetQueryPoolResults@ is guaranteed to
-- succeed and return @VK_SUCCESS@ if the queries become available in a
-- finite time (i.e. if they have been issued and not reset). If queries
-- will never finish (e.g. due to being reset but not issued), then
-- @vkGetQueryPoolResults@ /may/ not return in finite time.
--
-- If @VK_QUERY_RESULT_WAIT_BIT@ and @VK_QUERY_RESULT_PARTIAL_BIT@ are both
-- not set then no result values are written to @pData@ for queries that
-- are in the unavailable state at the time of the call, and
-- @vkGetQueryPoolResults@ returns @VK_NOT_READY@. However, availability
-- state is still written to @pData@ for those queries if
-- @VK_QUERY_RESULT_WITH_AVAILABILITY_BIT@ is set.
--
-- __Note__
--
-- Applications /must/ take care to ensure that use of the
-- @VK_QUERY_RESULT_WAIT_BIT@ bit has the desired effect.
--
-- For example, if a query has been used previously and a command buffer
-- records the commands @vkCmdResetQueryPool@, @vkCmdBeginQuery@, and
-- @vkCmdEndQuery@ for that query, then the query will remain in the
-- available state until the @vkCmdResetQueryPool@ command executes on a
-- queue. Applications /can/ use fences or events to ensure that a query
-- has already been reset before checking for its results or availability
-- status. Otherwise, a stale value could be returned from a previous use
-- of the query.
--
-- The above also applies when @VK_QUERY_RESULT_WAIT_BIT@ is used in
-- combination with @VK_QUERY_RESULT_WITH_AVAILABILITY_BIT@. In this case,
-- the returned availability status /may/ reflect the result of a previous
-- use of the query unless the @vkCmdResetQueryPool@ command has been
-- executed since the last use of the query.
--
-- __Note__
--
-- Applications /can/ double-buffer query pool usage, with a pool per
-- frame, and reset queries at the end of the frame in which they are read.
--
-- If @VK_QUERY_RESULT_PARTIAL_BIT@ is set, @VK_QUERY_RESULT_WAIT_BIT@ is
-- not set, and the query’s status is unavailable, an intermediate result
-- value between zero and the final result value is written to @pData@ for
-- that query.
--
-- @VK_QUERY_RESULT_PARTIAL_BIT@ /must/ not be used if the pool’s
-- @queryType@ is @VK_QUERY_TYPE_TIMESTAMP@.
--
-- If @VK_QUERY_RESULT_WITH_AVAILABILITY_BIT@ is set, the final integer
-- value written for each query is non-zero if the query’s status was
-- available or zero if the status was unavailable. When
-- @VK_QUERY_RESULT_WITH_AVAILABILITY_BIT@ is used, implementations /must/
-- guarantee that if they return a non-zero availability value then the
-- numerical results /must/ be valid, assuming the results are not reset by
-- a subsequent command.
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
-- -   If @VK_QUERY_RESULT_64_BIT@ is not set in @flags@ then @pData@ and
--     @stride@ /must/ be multiples of @4@
--
-- -   If @VK_QUERY_RESULT_64_BIT@ is set in @flags@ then @pData@ and
--     @stride@ /must/ be multiples of @8@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- -   @dataSize@ /must/ be large enough to contain the result of each
--     query, as described
--     [here](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-operation-memorylayout)
--
-- -   If the @queryType@ used to create @queryPool@ was
--     @VK_QUERY_TYPE_TIMESTAMP@, @flags@ /must/ not contain
--     @VK_QUERY_RESULT_PARTIAL_BIT@
--
-- == Valid Usage (Implicit)
--
-- -   @device@ /must/ be a valid @VkDevice@ handle
--
-- -   @queryPool@ /must/ be a valid @VkQueryPool@ handle
--
-- -   @pData@ /must/ be a valid pointer to an array of @dataSize@ bytes
--
-- -   @flags@ /must/ be a valid combination of 'VkQueryResultFlagBits'
--     values
--
-- -   @dataSize@ /must/ be greater than @0@
--
-- -   @queryPool@ /must/ have been created, allocated, or retrieved from
--     @device@
--
-- == Return Codes
--
-- [[Success](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-successcodes)]
--     -   @VK_SUCCESS@
--
--     -   @VK_NOT_READY@
--
-- [[Failure](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#fundamentals-errorcodes)]
--     -   @VK_ERROR_OUT_OF_HOST_MEMORY@
--
--     -   @VK_ERROR_OUT_OF_DEVICE_MEMORY@
--
--     -   @VK_ERROR_DEVICE_LOST@
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.DeviceInitialization.VkDevice', @VkDeviceSize@,
-- 'VkQueryPool', 'VkQueryResultFlags'
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetQueryPoolResults" vkGetQueryPoolResults :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
-- | VkQueryPoolCreateInfo - Structure specifying parameters of a newly
-- created query pool
--
-- = Description
--
-- @pipelineStatistics@ is ignored if @queryType@ is not
-- @VK_QUERY_TYPE_PIPELINE_STATISTICS@.
--
-- == Valid Usage
--
-- -   If the [pipeline statistics
--     queries](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#features-features-pipelineStatisticsQuery)
--     feature is not enabled, @queryType@ /must/ not be
--     @VK_QUERY_TYPE_PIPELINE_STATISTICS@
--
-- -   If @queryType@ is @VK_QUERY_TYPE_PIPELINE_STATISTICS@,
--     @pipelineStatistics@ /must/ be a valid combination of
--     'VkQueryPipelineStatisticFlagBits' values
--
-- == Valid Usage (Implicit)
--
-- -   @sType@ /must/ be @VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO@
--
-- -   @pNext@ /must/ be @NULL@
--
-- -   @flags@ /must/ be @0@
--
-- -   @queryType@ /must/ be a valid 'VkQueryType' value
--
-- = See Also
--
-- 'VkQueryPipelineStatisticFlags', 'VkQueryPoolCreateFlags',
-- 'VkQueryType', 'Graphics.Vulkan.Core10.Core.VkStructureType',
-- 'vkCreateQueryPool'
data VkQueryPoolCreateInfo = VkQueryPoolCreateInfo
  { -- | @sType@ is the type of this structure.
  vkSType :: VkStructureType
  , -- | @pNext@ is @NULL@ or a pointer to an extension-specific structure.
  vkPNext :: Ptr ()
  , -- | @flags@ is reserved for future use.
  vkFlags :: VkQueryPoolCreateFlags
  , -- | @queryType@ is a 'VkQueryType' value specifying the type of queries
  -- managed by the pool.
  vkQueryType :: VkQueryType
  , -- | @queryCount@ is the number of queries managed by the pool.
  vkQueryCount :: Word32
  , -- | @pipelineStatistics@ is a bitmask of 'VkQueryPipelineStatisticFlagBits'
  -- specifying which counters will be returned in queries on the new pool,
  -- as described below in
  -- [{html_spec_relative}#queries-pipestats](https://www.khronos.org/registry/vulkan/specs/1.0-extensions/html/vkspec.html#queries-pipestats).
  vkPipelineStatistics :: VkQueryPipelineStatisticFlags
  }
  deriving (Eq, Show)

instance Storable VkQueryPoolCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkQueryPoolCreateInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 20)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 28)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 20) (vkQueryType (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkQueryCount (poked :: VkQueryPoolCreateInfo))
                *> poke (ptr `plusPtr` 28) (vkPipelineStatistics (poked :: VkQueryPoolCreateInfo))
-- | VkQueryResultFlags - Bitmask of VkQueryResultFlagBits
--
-- = Description
--
-- @VkQueryResultFlags@ is a bitmask type for setting a mask of zero or
-- more 'VkQueryResultFlagBits'.
--
-- = See Also
--
-- 'VkQueryResultFlagBits',
-- 'Graphics.Vulkan.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'vkGetQueryPoolResults'
type VkQueryResultFlags = VkQueryResultFlagBits
-- | VkQueryPipelineStatisticFlags - Bitmask of
-- VkQueryPipelineStatisticFlagBits
--
-- = Description
--
-- @VkQueryPipelineStatisticFlags@ is a bitmask type for setting a mask of
-- zero or more 'VkQueryPipelineStatisticFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'VkQueryPipelineStatisticFlagBits', 'VkQueryPoolCreateInfo'
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits
