{-# language CPP #-}
-- No documentation found for Chapter "QueryType"
module Vulkan.Core10.Enums.QueryType  (QueryType( QUERY_TYPE_OCCLUSION
                                                , QUERY_TYPE_PIPELINE_STATISTICS
                                                , QUERY_TYPE_TIMESTAMP
                                                , QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT
                                                , QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT
                                                , QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR
                                                , QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR
                                                , QUERY_TYPE_PRIMITIVES_GENERATED_EXT
                                                , QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT
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
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkQueryType - Specify the type of queries managed by a query pool
--
-- = Description
--
-- -   'QUERY_TYPE_OCCLUSION' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-occlusion occlusion query>.
--
-- -   'QUERY_TYPE_PIPELINE_STATISTICS' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-pipestats pipeline statistics query>.
--
-- -   'QUERY_TYPE_TIMESTAMP' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-timestamps timestamp query>.
--
-- -   'QUERY_TYPE_PERFORMANCE_QUERY_KHR' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-performance performance query>.
--
-- -   'QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-transform-feedback transform feedback query>.
--
-- -   'QUERY_TYPE_PRIMITIVES_GENERATED_EXT' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-primitives-generated primitives generated query>.
--
-- -   'QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-copying acceleration structure size query>
--     for use with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR'
--     or
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.writeAccelerationStructuresPropertiesKHR'.
--
-- -   'QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR' specifies
--     a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-copying serialization acceleration structure size query>.
--
-- -   'QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-copying acceleration structure size query>
--     for use with
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR'
--     or
--     'Vulkan.Extensions.VK_KHR_acceleration_structure.writeAccelerationStructuresPropertiesKHR'.
--
-- -   'QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR'
--     specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#serialized-as-header serialization acceleration structure pointer count query>.
--
-- -   'QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV' specifies an
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#acceleration-structure-copying acceleration structure size query>
--     for use with
--     'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV'.
--
-- -   'QUERY_TYPE_PERFORMANCE_QUERY_INTEL' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-performance-intel Intel performance query>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryType VK_QUERY_TYPE_RESULT_STATUS_ONLY_KHR>
--     specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-result-status-only result status query>.
--
-- -   <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VkQueryType VK_QUERY_TYPE_VIDEO_ENCODE_FEEDBACK_KHR>
--     specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-video-encode-feedback video encode feedback query>.
--
-- -   'QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT' specifies a
--     <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#queries-mesh-shader generated mesh primitives query>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Query.QueryPoolCreateInfo',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.cmdWriteAccelerationStructuresPropertiesKHR',
-- 'Vulkan.Extensions.VK_NV_ray_tracing.cmdWriteAccelerationStructuresPropertiesNV',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.cmdWriteMicromapsPropertiesEXT',
-- 'Vulkan.Extensions.VK_KHR_acceleration_structure.writeAccelerationStructuresPropertiesKHR',
-- 'Vulkan.Extensions.VK_EXT_opacity_micromap.writeMicromapsPropertiesEXT'
newtype QueryType = QueryType Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_OCCLUSION"
pattern QUERY_TYPE_OCCLUSION = QueryType 0

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PIPELINE_STATISTICS"
pattern QUERY_TYPE_PIPELINE_STATISTICS = QueryType 1

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_TIMESTAMP"
pattern QUERY_TYPE_TIMESTAMP = QueryType 2

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT"
pattern QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT = QueryType 1000396001

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT"
pattern QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT = QueryType 1000396000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR = QueryType 1000386001

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR = QueryType 1000386000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PRIMITIVES_GENERATED_EXT"
pattern QUERY_TYPE_PRIMITIVES_GENERATED_EXT = QueryType 1000382000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT"
pattern QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT = QueryType 1000328000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PERFORMANCE_QUERY_INTEL"
pattern QUERY_TYPE_PERFORMANCE_QUERY_INTEL = QueryType 1000210000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV = QueryType 1000165000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR = QueryType 1000150001

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR"
pattern QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR = QueryType 1000150000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_PERFORMANCE_QUERY_KHR"
pattern QUERY_TYPE_PERFORMANCE_QUERY_KHR = QueryType 1000116000

-- No documentation found for Nested "VkQueryType" "VK_QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT"
pattern QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT = QueryType 1000028004

{-# COMPLETE
  QUERY_TYPE_OCCLUSION
  , QUERY_TYPE_PIPELINE_STATISTICS
  , QUERY_TYPE_TIMESTAMP
  , QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT
  , QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT
  , QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR
  , QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR
  , QUERY_TYPE_PRIMITIVES_GENERATED_EXT
  , QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT
  , QUERY_TYPE_PERFORMANCE_QUERY_INTEL
  , QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
  , QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR
  , QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR
  , QUERY_TYPE_PERFORMANCE_QUERY_KHR
  , QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT ::
    QueryType
  #-}

conNameQueryType :: String
conNameQueryType = "QueryType"

enumPrefixQueryType :: String
enumPrefixQueryType = "QUERY_TYPE_"

showTableQueryType :: [(QueryType, String)]
showTableQueryType =
  [ (QUERY_TYPE_OCCLUSION, "OCCLUSION")
  , (QUERY_TYPE_PIPELINE_STATISTICS, "PIPELINE_STATISTICS")
  , (QUERY_TYPE_TIMESTAMP, "TIMESTAMP")
  ,
    ( QUERY_TYPE_MICROMAP_COMPACTED_SIZE_EXT
    , "MICROMAP_COMPACTED_SIZE_EXT"
    )
  ,
    ( QUERY_TYPE_MICROMAP_SERIALIZATION_SIZE_EXT
    , "MICROMAP_SERIALIZATION_SIZE_EXT"
    )
  ,
    ( QUERY_TYPE_ACCELERATION_STRUCTURE_SIZE_KHR
    , "ACCELERATION_STRUCTURE_SIZE_KHR"
    )
  ,
    ( QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR
    , "ACCELERATION_STRUCTURE_SERIALIZATION_BOTTOM_LEVEL_POINTERS_KHR"
    )
  ,
    ( QUERY_TYPE_PRIMITIVES_GENERATED_EXT
    , "PRIMITIVES_GENERATED_EXT"
    )
  ,
    ( QUERY_TYPE_MESH_PRIMITIVES_GENERATED_EXT
    , "MESH_PRIMITIVES_GENERATED_EXT"
    )
  ,
    ( QUERY_TYPE_PERFORMANCE_QUERY_INTEL
    , "PERFORMANCE_QUERY_INTEL"
    )
  ,
    ( QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV
    , "ACCELERATION_STRUCTURE_COMPACTED_SIZE_NV"
    )
  ,
    ( QUERY_TYPE_ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR
    , "ACCELERATION_STRUCTURE_SERIALIZATION_SIZE_KHR"
    )
  ,
    ( QUERY_TYPE_ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR
    , "ACCELERATION_STRUCTURE_COMPACTED_SIZE_KHR"
    )
  ,
    ( QUERY_TYPE_PERFORMANCE_QUERY_KHR
    , "PERFORMANCE_QUERY_KHR"
    )
  ,
    ( QUERY_TYPE_TRANSFORM_FEEDBACK_STREAM_EXT
    , "TRANSFORM_FEEDBACK_STREAM_EXT"
    )
  ]

instance Show QueryType where
  showsPrec =
    enumShowsPrec
      enumPrefixQueryType
      showTableQueryType
      conNameQueryType
      (\(QueryType x) -> x)
      (showsPrec 11)

instance Read QueryType where
  readPrec =
    enumReadPrec
      enumPrefixQueryType
      showTableQueryType
      conNameQueryType
      QueryType
