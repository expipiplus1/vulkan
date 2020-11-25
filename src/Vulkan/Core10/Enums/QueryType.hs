{-# language CPP #-}
-- No documentation found for Chapter "QueryType"
module Vulkan.Core10.Enums.QueryType  (QueryType( QUERY_TYPE_OCCLUSION
                                                , QUERY_TYPE_PIPELINE_STATISTICS
                                                , QUERY_TYPE_TIMESTAMP
                                                , QUERY_TYPE_PERFORMANCE_QUERY_INTEL
                                                , QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
                                                , QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR
                                                , QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR
                                                , QUERY_TYPE_PERFORMANCE_QUERY_KHR
                                                , QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
                                                , ..
                                                )) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Zero (Zero)
-- No documentation found for TopLevel "VkQueryType"
newtype QueryType = QueryType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_OCCLUSION"
pattern QUERY_TYPE_OCCLUSION                     = QueryType 0
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PIPELINE_STATISTICS"
pattern QUERY_TYPE_PIPELINE_STATISTICS           = QueryType 1
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_TIMESTAMP"
pattern QUERY_TYPE_TIMESTAMP                     = QueryType 2
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL"
pattern QUERY_TYPE_PERFORMANCE_QUERY_INTEL       = QueryType 1000210000
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = QueryType 1000165000
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR = QueryType 1000150001
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR = QueryType 1000150000
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR"
pattern QUERY_TYPE_PERFORMANCE_QUERY_KHR         = QueryType 1000116000
-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT"
pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT = QueryType 1000028004
{-# complete QUERY_TYPE_OCCLUSION,
             QUERY_TYPE_PIPELINE_STATISTICS,
             QUERY_TYPE_TIMESTAMP,
             QUERY_TYPE_PERFORMANCE_QUERY_INTEL,
             QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV,
             QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR,
             QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR,
             QUERY_TYPE_PERFORMANCE_QUERY_KHR,
             QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT :: QueryType #-}

conNameQueryType :: String
conNameQueryType = "QueryType"

enumPrefixQueryType :: String
enumPrefixQueryType = "QUERY_TYPE_"

showTableQueryType :: [(QueryType, String)]
showTableQueryType =
  [ (QUERY_TYPE_OCCLUSION                    , "OCCLUSION")
  , (QUERY_TYPE_PIPELINE_STATISTICS          , "PIPELINE_STATISTICS")
  , (QUERY_TYPE_TIMESTAMP                    , "TIMESTAMP")
  , (QUERY_TYPE_PERFORMANCE_QUERY_INTEL      , "PERFORMANCE_QUERY_INTEL")
  , (QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV, "ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV")
  , (QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR, "ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR")
  , (QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR, "ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR")
  , (QUERY_TYPE_PERFORMANCE_QUERY_KHR        , "PERFORMANCE_QUERY_KHR")
  , (QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT, "TRANSFORM_FEEDBACK_STREAM_EXT")
  ]


instance Show QueryType where
showsPrec = enumShowsPrec enumPrefixQueryType showTableQueryType conNameQueryType (\(QueryType x) -> x) (showsPrec 11)


instance Read QueryType where
  readPrec = enumReadPrec enumPrefixQueryType showTableQueryType conNameQueryType QueryType

