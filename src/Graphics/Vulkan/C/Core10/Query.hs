{-# language Strict #-}
{-# language CPP #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Core10.Query
  ( VkQueryPipelineStatisticFlagBits(..)
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
  , VkQueryPipelineStatisticFlags
  , VkQueryPool
  , VkQueryPoolCreateFlags(..)
  , VkQueryPoolCreateInfo(..)
  , VkQueryResultFlagBits(..)
  , pattern VK_QUERY_RESULT_64_BIT
  , pattern VK_QUERY_RESULT_WAIT_BIT
  , pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT
  , pattern VK_QUERY_RESULT_PARTIAL_BIT
  , VkQueryResultFlags
  , VkQueryType(..)
  , pattern VK_QUERY_TYPE_OCCLUSION
  , pattern VK_QUERY_TYPE_PIPELINE_STATISTICS
  , pattern VK_QUERY_TYPE_TIMESTAMP
  , FN_vkCreateQueryPool
  , PFN_vkCreateQueryPool
  , vkCreateQueryPool
  , FN_vkDestroyQueryPool
  , PFN_vkDestroyQueryPool
  , vkDestroyQueryPool
  , FN_vkGetQueryPoolResults
  , PFN_vkGetQueryPoolResults
  , vkGetQueryPoolResults
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
  ( FunPtr
  , Ptr
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  , VkFlags
  , pattern VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkAllocationCallbacks(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- ** VkQueryPipelineStatisticFlagBits

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
-- or request it be put into host memory (via 'vkGetQueryPoolResults').
--
-- = See Also
--
-- 'VkQueryPipelineStatisticFlags'
newtype VkQueryPipelineStatisticFlagBits = VkQueryPipelineStatisticFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- | 'VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT' specifies that
-- queries managed by the pool will count the number of vertices processed
-- by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing input assembly>
-- stage. Vertices corresponding to incomplete primitives /may/ contribute
-- to the count.
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT = VkQueryPipelineStatisticFlagBits 0x00000001

-- | 'VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT' specifies
-- that queries managed by the pool will count the number of primitives
-- processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#drawing input assembly>
-- stage. If primitive restart is enabled, restarting the primitive
-- topology has no effect on the count. Incomplete primitives /may/ be
-- counted.
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000002

-- | 'VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT' specifies
-- that queries managed by the pool will count the number of vertex shader
-- invocations. This counter’s value is incremented each time a vertex
-- shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-vertex-execution invoked>.
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000004

-- | 'VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT' specifies
-- that queries managed by the pool will count the number of geometry
-- shader invocations. This counter’s value is incremented each time a
-- geometry shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-geometry-execution invoked>.
-- In the case of
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#geometry-invocations instanced geometry shaders>,
-- the geometry shader invocations count is incremented for each separate
-- instanced invocation.
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000008

-- | 'VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT' specifies
-- that queries managed by the pool will count the number of primitives
-- generated by geometry shader invocations. The counter’s value is
-- incremented each time the geometry shader emits a primitive. Restarting
-- primitive topology using the SPIR-V instructions @OpEndPrimitive@ or
-- @OpEndStreamPrimitive@ has no effect on the geometry shader output
-- primitives count.
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000010

-- | 'VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT' specifies that
-- queries managed by the pool will count the number of primitives
-- processed by the
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#vertexpostproc-clipping Primitive Clipping>
-- stage of the pipeline. The counter’s value is incremented each time a
-- primitive reaches the primitive clipping stage.
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000020

-- | 'VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT' specifies that
-- queries managed by the pool will count the number of primitives output
-- by the
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
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT = VkQueryPipelineStatisticFlagBits 0x00000040

-- | 'VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT' specifies
-- that queries managed by the pool will count the number of fragment
-- shader invocations. The counter’s value is incremented each time the
-- fragment shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-fragment-execution invoked>.
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000080

-- | 'VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT'
-- specifies that queries managed by the pool will count the number of
-- patches processed by the tessellation control shader. The counter’s
-- value is incremented once for each patch for which a tessellation
-- control shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-tessellation-control-execution invoked>.
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = VkQueryPipelineStatisticFlagBits 0x00000100

-- | 'VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT'
-- specifies that queries managed by the pool will count the number of
-- invocations of the tessellation evaluation shader. The counter’s value
-- is incremented each time the tessellation evaluation shader is
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#shaders-tessellation-evaluation-execution invoked>.
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000200

-- | 'VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT' specifies
-- that queries managed by the pool will count the number of compute shader
-- invocations. The counter’s value is incremented every time the compute
-- shader is invoked. Implementations /may/ skip the execution of certain
-- compute shader invocations or execute additional compute shader
-- invocations for implementation-dependent reasons as long as the results
-- of rendering otherwise remain unchanged.
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT :: VkQueryPipelineStatisticFlagBits
pattern VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT = VkQueryPipelineStatisticFlagBits 0x00000400

-- | VkQueryPipelineStatisticFlags - Bitmask of
-- VkQueryPipelineStatisticFlagBits
--
-- = Description
--
-- 'VkQueryPipelineStatisticFlags' is a bitmask type for setting a mask of
-- zero or more 'VkQueryPipelineStatisticFlagBits'.
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBuffer.VkCommandBufferInheritanceInfo',
-- 'VkQueryPipelineStatisticFlagBits', 'VkQueryPoolCreateInfo'
type VkQueryPipelineStatisticFlags = VkQueryPipelineStatisticFlagBits

-- | Dummy data to tag the 'Ptr' with
data VkQueryPool_T
-- | VkQueryPool - Opaque handle to a query pool object
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdBeginQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdEndQuery',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdWriteTimestamp',
-- 'vkCreateQueryPool', 'vkDestroyQueryPool', 'vkGetQueryPoolResults'
type VkQueryPool = Ptr VkQueryPool_T

-- ** VkQueryPoolCreateFlags

-- | VkQueryPoolCreateFlags - Reserved for future use
--
-- = Description
--
-- 'VkQueryPoolCreateFlags' is a bitmask type for setting a mask, but is
-- currently reserved for future use.
--
-- = See Also
--
-- 'VkQueryPoolCreateInfo'
newtype VkQueryPoolCreateFlags = VkQueryPoolCreateFlags VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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



-- | VkQueryPoolCreateInfo - Structure specifying parameters of a newly
-- created query pool
--
-- = Description
--
-- @pipelineStatistics@ is ignored if @queryType@ is not
-- 'VK_QUERY_TYPE_PIPELINE_STATISTICS'.
--
-- == Valid Usage
--
-- -   If the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#features-pipelineStatisticsQuery pipeline statistics queries>
--     feature is not enabled, @queryType@ /must/ not be
--     'VK_QUERY_TYPE_PIPELINE_STATISTICS'
--
-- -   If @queryType@ is 'VK_QUERY_TYPE_PIPELINE_STATISTICS',
--     @pipelineStatistics@ /must/ be a valid combination of
--     'VkQueryPipelineStatisticFlagBits' values
--
-- Unresolved directive in VkQueryPoolCreateInfo.txt -
-- include::{generated}\/validity\/structs\/VkQueryPoolCreateInfo.txt[]
--
-- = See Also
--
-- 'VkQueryPipelineStatisticFlags', 'VkQueryPoolCreateFlags',
-- 'VkQueryType', 'Graphics.Vulkan.C.Core10.Core.VkStructureType',
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
  -- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-pipestats>.
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

instance Zero VkQueryPoolCreateInfo where
  zero = VkQueryPoolCreateInfo VK_STRUCTURE_TYPE_QUERY_POOL_CREATE_INFO
                               zero
                               zero
                               zero
                               zero
                               zero

-- ** VkQueryResultFlagBits

-- | VkQueryResultFlagBits - Bitmask specifying how and when query results
-- are returned
--
-- = See Also
--
-- 'VkQueryResultFlags'
newtype VkQueryResultFlagBits = VkQueryResultFlagBits VkFlags
  deriving (Eq, Ord, Storable, Bits, FiniteBits, Zero)

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

-- | 'VK_QUERY_RESULT_64_BIT' specifies the results will be written as an
-- array of 64-bit unsigned integer values. If this bit is not set, the
-- results will be written as an array of 32-bit unsigned integer values.
pattern VK_QUERY_RESULT_64_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_64_BIT = VkQueryResultFlagBits 0x00000001

-- | 'VK_QUERY_RESULT_WAIT_BIT' specifies that Vulkan will wait for each
-- query’s status to become available before retrieving its results.
pattern VK_QUERY_RESULT_WAIT_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_WAIT_BIT = VkQueryResultFlagBits 0x00000002

-- | 'VK_QUERY_RESULT_WITH_AVAILABILITY_BIT' specifies that the availability
-- status accompanies the results.
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_WITH_AVAILABILITY_BIT = VkQueryResultFlagBits 0x00000004

-- | 'VK_QUERY_RESULT_PARTIAL_BIT' specifies that returning partial results
-- is acceptable.
pattern VK_QUERY_RESULT_PARTIAL_BIT :: VkQueryResultFlagBits
pattern VK_QUERY_RESULT_PARTIAL_BIT = VkQueryResultFlagBits 0x00000008

-- | VkQueryResultFlags - Bitmask of VkQueryResultFlagBits
--
-- = Description
--
-- 'VkQueryResultFlags' is a bitmask type for setting a mask of zero or
-- more 'VkQueryResultFlagBits'.
--
-- = See Also
--
-- 'VkQueryResultFlagBits',
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdCopyQueryPoolResults',
-- 'vkGetQueryPoolResults'
type VkQueryResultFlags = VkQueryResultFlagBits

-- ** VkQueryType

-- | VkQueryType - Specify the type of queries managed by a query pool
--
-- = See Also
--
-- 'VkQueryPoolCreateInfo'
newtype VkQueryType = VkQueryType Int32
  deriving (Eq, Ord, Storable, Zero)

instance Show VkQueryType where
  showsPrec _ VK_QUERY_TYPE_OCCLUSION = showString "VK_QUERY_TYPE_OCCLUSION"
  showsPrec _ VK_QUERY_TYPE_PIPELINE_STATISTICS = showString "VK_QUERY_TYPE_PIPELINE_STATISTICS"
  showsPrec _ VK_QUERY_TYPE_TIMESTAMP = showString "VK_QUERY_TYPE_TIMESTAMP"
  -- The following values are from extensions, the patterns themselves are exported from the extension modules
  showsPrec _ (VkQueryType 1000023008) = showString "VK_QUERY_TYPE_RESERVED_8"
  showsPrec _ (VkQueryType 1000024004) = showString "VK_QUERY_TYPE_RESERVED_4"
  showsPrec _ (VkQueryType 1000028004) = showString "VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT"
  showsPrec _ (VkQueryType 1000165000) = showString "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
  showsPrec p (VkQueryType x) = showParen (p >= 11) (showString "VkQueryType " . showsPrec 11 x)

instance Read VkQueryType where
  readPrec = parens ( choose [ ("VK_QUERY_TYPE_OCCLUSION",           pure VK_QUERY_TYPE_OCCLUSION)
                             , ("VK_QUERY_TYPE_PIPELINE_STATISTICS", pure VK_QUERY_TYPE_PIPELINE_STATISTICS)
                             , ("VK_QUERY_TYPE_TIMESTAMP",           pure VK_QUERY_TYPE_TIMESTAMP)
                             , -- The following values are from extensions, the patterns themselves are exported from the extension modules
                               ("VK_QUERY_TYPE_RESERVED_8",                               pure (VkQueryType 1000023008))
                             , ("VK_QUERY_TYPE_RESERVED_4",                               pure (VkQueryType 1000024004))
                             , ("VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT",            pure (VkQueryType 1000028004))
                             , ("VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV", pure (VkQueryType 1000165000))
                             ] +++
                      prec 10 (do
                        expectP (Ident "VkQueryType")
                        v <- step readPrec
                        pure (VkQueryType v)
                        )
                    )

-- | 'VK_QUERY_TYPE_OCCLUSION' specifies an
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-occlusion occlusion query>.
pattern VK_QUERY_TYPE_OCCLUSION :: VkQueryType
pattern VK_QUERY_TYPE_OCCLUSION = VkQueryType 0

-- | 'VK_QUERY_TYPE_PIPELINE_STATISTICS' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-pipestats pipeline statistics query>.
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS :: VkQueryType
pattern VK_QUERY_TYPE_PIPELINE_STATISTICS = VkQueryType 1

-- | 'VK_QUERY_TYPE_TIMESTAMP' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-timestamps timestamp query>.
pattern VK_QUERY_TYPE_TIMESTAMP :: VkQueryType
pattern VK_QUERY_TYPE_TIMESTAMP = VkQueryType 2

-- | vkCreateQueryPool - Create a new query pool object
--
-- = Parameters
--
-- -   @device@ is the logical device that creates the query pool.
--
-- -   @pCreateInfo@ is a pointer to an instance of the
--     'VkQueryPoolCreateInfo' structure containing the number and type of
--     queries to be managed by the pool.
--
-- -   @pAllocator@ controls host memory allocation as described in the
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#memory-allocation Memory Allocation>
--     chapter.
--
-- -   @pQueryPool@ is a pointer to a 'VkQueryPool' handle in which the
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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkQueryPool',
-- 'VkQueryPoolCreateInfo'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkCreateQueryPool" vkCreateQueryPool :: ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
#else
vkCreateQueryPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
vkCreateQueryPool deviceCmds = mkVkCreateQueryPool (pVkCreateQueryPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkCreateQueryPool
  :: FunPtr (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult) -> (("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult)
#endif

type FN_vkCreateQueryPool = ("device" ::: VkDevice) -> ("pCreateInfo" ::: Ptr VkQueryPoolCreateInfo) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> ("pQueryPool" ::: Ptr VkQueryPool) -> IO VkResult
type PFN_vkCreateQueryPool = FunPtr FN_vkCreateQueryPool

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
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice', 'VkQueryPool'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkDestroyQueryPool" vkDestroyQueryPool :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
#else
vkDestroyQueryPool :: DeviceCmds -> ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
vkDestroyQueryPool deviceCmds = mkVkDestroyQueryPool (pVkDestroyQueryPool deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkDestroyQueryPool
  :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()) -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ())
#endif

type FN_vkDestroyQueryPool = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("pAllocator" ::: Ptr VkAllocationCallbacks) -> IO ()
type PFN_vkDestroyQueryPool = FunPtr FN_vkDestroyQueryPool

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
-- <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-wait-bit-not-set below>.
--
-- If 'VK_QUERY_RESULT_64_BIT' is not set and the result overflows a 32-bit
-- value, the value /may/ either wrap or saturate. Similarly, if
-- 'VK_QUERY_RESULT_64_BIT' is set and the result overflows a 64-bit value,
-- the value /may/ either wrap or saturate.
--
-- If 'VK_QUERY_RESULT_WAIT_BIT' is set, Vulkan will wait for each query to
-- be in the available state before retrieving the numerical results for
-- that query. In this case, 'vkGetQueryPoolResults' is guaranteed to
-- succeed and return 'Graphics.Vulkan.C.Core10.Core.VK_SUCCESS' if the
-- queries become available in a finite time (i.e. if they have been issued
-- and not reset). If queries will never finish (e.g. due to being reset
-- but not issued), then 'vkGetQueryPoolResults' /may/ not return in finite
-- time.
--
-- If 'VK_QUERY_RESULT_WAIT_BIT' and 'VK_QUERY_RESULT_PARTIAL_BIT' are both
-- not set then no result values are written to @pData@ for queries that
-- are in the unavailable state at the time of the call, and
-- 'vkGetQueryPoolResults' returns
-- 'Graphics.Vulkan.C.Core10.Core.VK_NOT_READY'. However, availability
-- state is still written to @pData@ for those queries if
-- 'VK_QUERY_RESULT_WITH_AVAILABILITY_BIT' is set.
--
-- __Note__
--
-- Applications /must/ take care to ensure that use of the
-- 'VK_QUERY_RESULT_WAIT_BIT' bit has the desired effect.
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
-- The above also applies when 'VK_QUERY_RESULT_WAIT_BIT' is used in
-- combination with 'VK_QUERY_RESULT_WITH_AVAILABILITY_BIT'. In this case,
-- the returned availability status /may/ reflect the result of a previous
-- use of the query unless the
-- 'Graphics.Vulkan.C.Core10.CommandBufferBuilding.vkCmdResetQueryPool'
-- command has been executed since the last use of the query.
--
-- __Note__
--
-- Applications /can/ double-buffer query pool usage, with a pool per
-- frame, and reset queries at the end of the frame in which they are read.
--
-- If 'VK_QUERY_RESULT_PARTIAL_BIT' is set, 'VK_QUERY_RESULT_WAIT_BIT' is
-- not set, and the query’s status is unavailable, an intermediate result
-- value between zero and the final result value is written to @pData@ for
-- that query.
--
-- 'VK_QUERY_RESULT_PARTIAL_BIT' /must/ not be used if the pool’s
-- @queryType@ is 'VK_QUERY_TYPE_TIMESTAMP'.
--
-- If 'VK_QUERY_RESULT_WITH_AVAILABILITY_BIT' is set, the final integer
-- value written for each query is non-zero if the query’s status was
-- available or zero if the status was unavailable. When
-- 'VK_QUERY_RESULT_WITH_AVAILABILITY_BIT' is used, implementations /must/
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
-- -   If 'VK_QUERY_RESULT_64_BIT' is not set in @flags@ then @pData@ and
--     @stride@ /must/ be multiples of @4@
--
-- -   If 'VK_QUERY_RESULT_64_BIT' is set in @flags@ then @pData@ and
--     @stride@ /must/ be multiples of @8@
--
-- -   The sum of @firstQuery@ and @queryCount@ /must/ be less than or
--     equal to the number of queries in @queryPool@
--
-- -   @dataSize@ /must/ be large enough to contain the result of each
--     query, as described
--     <https://www.khronos.org/registry/vulkan/specs/1.1-extensions/html/vkspec.html#queries-operation-memorylayout here>
--
-- -   If the @queryType@ used to create @queryPool@ was
--     'VK_QUERY_TYPE_TIMESTAMP', @flags@ /must/ not contain
--     'VK_QUERY_RESULT_PARTIAL_BIT'
--
-- Unresolved directive in vkGetQueryPoolResults.txt -
-- include::{generated}\/validity\/protos\/vkGetQueryPoolResults.txt[]
--
-- = See Also
--
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDevice',
-- 'Graphics.Vulkan.C.Core10.DeviceInitialization.VkDeviceSize',
-- 'VkQueryPool', 'VkQueryResultFlags'
#if defined(EXPOSE_CORE10_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetQueryPoolResults" vkGetQueryPoolResults :: ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
#else
vkGetQueryPoolResults :: DeviceCmds -> ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
vkGetQueryPoolResults deviceCmds = mkVkGetQueryPoolResults (pVkGetQueryPoolResults deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetQueryPoolResults
  :: FunPtr (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult) -> (("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult)
#endif

type FN_vkGetQueryPoolResults = ("device" ::: VkDevice) -> ("queryPool" ::: VkQueryPool) -> ("firstQuery" ::: Word32) -> ("queryCount" ::: Word32) -> ("dataSize" ::: CSize) -> ("pData" ::: Ptr ()) -> ("stride" ::: VkDeviceSize) -> ("flags" ::: VkQueryResultFlags) -> IO VkResult
type PFN_vkGetQueryPoolResults = FunPtr FN_vkGetQueryPoolResults
