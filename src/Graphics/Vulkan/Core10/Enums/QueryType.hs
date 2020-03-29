{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.QueryType  (QueryType( QUERY_TYPE_OCCLUSION
                                                         , QUERY_TYPE_PIPELINE_STATISTICS
                                                         , QUERY_TYPE_TIMESTAMP
                                                         , QUERY_TYPE_PERFORMANCE_QUERY_INTEL
                                                         , QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
                                                         , QUERY_TYPE_PERFORMANCE_QUERY_KHR
                                                         , QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
                                                         , ..
                                                         )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Zero (Zero)
-- | VkQueryType - Specify the type of queries managed by a query pool
--
-- = See Also
--
-- 'Graphics.Vulkan.Core10.Query.QueryPoolCreateInfo',
-- 'Graphics.Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV'
newtype QueryType = QueryType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'QUERY_TYPE_OCCLUSION' specifies an
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-occlusion occlusion query>.
pattern QUERY_TYPE_OCCLUSION = QueryType 0
-- | 'QUERY_TYPE_PIPELINE_STATISTICS' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-pipestats pipeline statistics query>.
pattern QUERY_TYPE_PIPELINE_STATISTICS = QueryType 1
-- | 'QUERY_TYPE_TIMESTAMP' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-timestamps timestamp query>.
pattern QUERY_TYPE_TIMESTAMP = QueryType 2
-- | 'QUERY_TYPE_PERFORMANCE_QUERY_INTEL' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-performance-intel Intel performance query>.
pattern QUERY_TYPE_PERFORMANCE_QUERY_INTEL = QueryType 1000210000
-- | 'QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#acceleration-structure-copying ray tracing acceleration structure size query>.
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = QueryType 1000165000
-- | 'QUERY_TYPE_PERFORMANCE_QUERY_KHR' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-performance performance query>.
pattern QUERY_TYPE_PERFORMANCE_QUERY_KHR = QueryType 1000116000
-- | 'QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' specifies a
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#queries-transform-feedback transform feedback query>.
pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT = QueryType 1000028004
{-# complete QUERY_TYPE_OCCLUSION,
             QUERY_TYPE_PIPELINE_STATISTICS,
             QUERY_TYPE_TIMESTAMP,
             QUERY_TYPE_PERFORMANCE_QUERY_INTEL,
             QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV,
             QUERY_TYPE_PERFORMANCE_QUERY_KHR,
             QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT :: QueryType #-}

instance Show QueryType where
  showsPrec p = \case
    QUERY_TYPE_OCCLUSION -> showString "QUERY_TYPE_OCCLUSION"
    QUERY_TYPE_PIPELINE_STATISTICS -> showString "QUERY_TYPE_PIPELINE_STATISTICS"
    QUERY_TYPE_TIMESTAMP -> showString "QUERY_TYPE_TIMESTAMP"
    QUERY_TYPE_PERFORMANCE_QUERY_INTEL -> showString "QUERY_TYPE_PERFORMANCE_QUERY_INTEL"
    QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV -> showString "QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
    QUERY_TYPE_PERFORMANCE_QUERY_KHR -> showString "QUERY_TYPE_PERFORMANCE_QUERY_KHR"
    QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT -> showString "QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT"
    QueryType x -> showParen (p >= 11) (showString "QueryType " . showsPrec 11 x)

instance Read QueryType where
  readPrec = parens (choose [("QUERY_TYPE_OCCLUSION", pure QUERY_TYPE_OCCLUSION)
                            , ("QUERY_TYPE_PIPELINE_STATISTICS", pure QUERY_TYPE_PIPELINE_STATISTICS)
                            , ("QUERY_TYPE_TIMESTAMP", pure QUERY_TYPE_TIMESTAMP)
                            , ("QUERY_TYPE_PERFORMANCE_QUERY_INTEL", pure QUERY_TYPE_PERFORMANCE_QUERY_INTEL)
                            , ("QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV", pure QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV)
                            , ("QUERY_TYPE_PERFORMANCE_QUERY_KHR", pure QUERY_TYPE_PERFORMANCE_QUERY_KHR)
                            , ("QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT", pure QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "QueryType")
                       v <- step readPrec
                       pure (QueryType v)))

