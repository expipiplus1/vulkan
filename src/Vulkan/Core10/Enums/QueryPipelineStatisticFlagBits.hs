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
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
type QueryPipelineStatisticFlags = QueryPipelineStatisticFlagBits

-- No documentation found for TopLevel "VkQueryPipelineStatisticFlagBits"
newtype QueryPipelineStatisticFlagBits = QueryPipelineStatisticFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT"
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_VERTICES_BIT             = QueryPipelineStatisticFlagBits 0x00000001
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT"
pattern QUERY_PIPELINE_STATISTIC_INPUT_ASSEMBLY_PRIMITIVES_BIT           = QueryPipelineStatisticFlagBits 0x00000002
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_VERTEX_SHADER_INVOCATIONS_BIT           = QueryPipelineStatisticFlagBits 0x00000004
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_INVOCATIONS_BIT         = QueryPipelineStatisticFlagBits 0x00000008
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT"
pattern QUERY_PIPELINE_STATISTIC_GEOMETRY_SHADER_PRIMITIVES_BIT          = QueryPipelineStatisticFlagBits 0x00000010
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_INVOCATIONS_BIT                = QueryPipelineStatisticFlagBits 0x00000020
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT"
pattern QUERY_PIPELINE_STATISTIC_CLIPPING_PRIMITIVES_BIT                 = QueryPipelineStatisticFlagBits 0x00000040
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_FRAGMENT_SHADER_INVOCATIONS_BIT         = QueryPipelineStatisticFlagBits 0x00000080
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT"
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_CONTROL_SHADER_PATCHES_BIT = QueryPipelineStatisticFlagBits 0x00000100
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT"
pattern QUERY_PIPELINE_STATISTIC_TESSELLATION_EVALUATION_SHADER_INVOCATIONS_BIT =
  QueryPipelineStatisticFlagBits 0x00000200
-- No documentation found for Nested "VkQueryPipelineStatisticFlagBits" "VK_QUERY_PIPELINE_STATISTIC_COMPUTE_SHADER_INVOCATIONS_BIT"
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

