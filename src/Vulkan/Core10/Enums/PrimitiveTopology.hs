{-# language CPP #-}
-- No documentation found for Chapter "PrimitiveTopology"
module Vulkan.Core10.Enums.PrimitiveTopology  (PrimitiveTopology( PRIMITIVE_TOPOLOGY_POINT_LIST
                                                                , PRIMITIVE_TOPOLOGY_LINE_LIST
                                                                , PRIMITIVE_TOPOLOGY_LINE_STRIP
                                                                , PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
                                                                , PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
                                                                , PRIMITIVE_TOPOLOGY_TRIANGLE_FAN
                                                                , PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY
                                                                , PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY
                                                                , PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY
                                                                , PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY
                                                                , PRIMITIVE_TOPOLOGY_PATCH_LIST
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
-- No documentation found for TopLevel "VkPrimitiveTopology"
newtype PrimitiveTopology = PrimitiveTopology Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_POINT_LIST"
pattern PRIMITIVE_TOPOLOGY_POINT_LIST                    = PrimitiveTopology 0
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_LIST"
pattern PRIMITIVE_TOPOLOGY_LINE_LIST                     = PrimitiveTopology 1
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP"
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP                    = PrimitiveTopology 2
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST                 = PrimitiveTopology 3
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP                = PrimitiveTopology 4
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_FAN                  = PrimitiveTopology 5
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY      = PrimitiveTopology 6
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY     = PrimitiveTopology 7
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY  = PrimitiveTopology 8
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY"
pattern PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY = PrimitiveTopology 9
-- No documentation found for Nested "VkPrimitiveTopology" "VK_PRIMITIVE_TOPOLOGY_PATCH_LIST"
pattern PRIMITIVE_TOPOLOGY_PATCH_LIST                    = PrimitiveTopology 10
{-# complete PRIMITIVE_TOPOLOGY_POINT_LIST,
             PRIMITIVE_TOPOLOGY_LINE_LIST,
             PRIMITIVE_TOPOLOGY_LINE_STRIP,
             PRIMITIVE_TOPOLOGY_TRIANGLE_LIST,
             PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
             PRIMITIVE_TOPOLOGY_TRIANGLE_FAN,
             PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY,
             PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY,
             PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY,
             PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY,
             PRIMITIVE_TOPOLOGY_PATCH_LIST :: PrimitiveTopology #-}

conNamePrimitiveTopology :: String
conNamePrimitiveTopology = "PrimitiveTopology"

enumPrefixPrimitiveTopology :: String
enumPrefixPrimitiveTopology = "PRIMITIVE_TOPOLOGY_"

showTablePrimitiveTopology :: [(PrimitiveTopology, String)]
showTablePrimitiveTopology =
  [ (PRIMITIVE_TOPOLOGY_POINT_LIST                   , "POINT_LIST")
  , (PRIMITIVE_TOPOLOGY_LINE_LIST                    , "LINE_LIST")
  , (PRIMITIVE_TOPOLOGY_LINE_STRIP                   , "LINE_STRIP")
  , (PRIMITIVE_TOPOLOGY_TRIANGLE_LIST                , "TRIANGLE_LIST")
  , (PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP               , "TRIANGLE_STRIP")
  , (PRIMITIVE_TOPOLOGY_TRIANGLE_FAN                 , "TRIANGLE_FAN")
  , (PRIMITIVE_TOPOLOGY_LINE_LIST_WITH_ADJACENCY     , "LINE_LIST_WITH_ADJACENCY")
  , (PRIMITIVE_TOPOLOGY_LINE_STRIP_WITH_ADJACENCY    , "LINE_STRIP_WITH_ADJACENCY")
  , (PRIMITIVE_TOPOLOGY_TRIANGLE_LIST_WITH_ADJACENCY , "TRIANGLE_LIST_WITH_ADJACENCY")
  , (PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP_WITH_ADJACENCY, "TRIANGLE_STRIP_WITH_ADJACENCY")
  , (PRIMITIVE_TOPOLOGY_PATCH_LIST                   , "PATCH_LIST")
  ]


instance Show PrimitiveTopology where
showsPrec = enumShowsPrec enumPrefixPrimitiveTopology
                          showTablePrimitiveTopology
                          conNamePrimitiveTopology
                          (\(PrimitiveTopology x) -> x)
                          (showsPrec 11)


instance Read PrimitiveTopology where
  readPrec =
    enumReadPrec enumPrefixPrimitiveTopology showTablePrimitiveTopology conNamePrimitiveTopology PrimitiveTopology

