{-# language CPP #-}
-- No documentation found for Chapter "VertexInputRate"
module Vulkan.Core10.Enums.VertexInputRate  (VertexInputRate( VERTEX_INPUT_RATE_VERTEX
                                                            , VERTEX_INPUT_RATE_INSTANCE
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
-- No documentation found for TopLevel "VkVertexInputRate"
newtype VertexInputRate = VertexInputRate Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkVertexInputRate" "VK_VERTEX_INPUT_RATE_VERTEX"
pattern VERTEX_INPUT_RATE_VERTEX   = VertexInputRate 0
-- No documentation found for Nested "VkVertexInputRate" "VK_VERTEX_INPUT_RATE_INSTANCE"
pattern VERTEX_INPUT_RATE_INSTANCE = VertexInputRate 1
{-# complete VERTEX_INPUT_RATE_VERTEX,
             VERTEX_INPUT_RATE_INSTANCE :: VertexInputRate #-}

conNameVertexInputRate :: String
conNameVertexInputRate = "VertexInputRate"

enumPrefixVertexInputRate :: String
enumPrefixVertexInputRate = "VERTEX_INPUT_RATE_"

showTableVertexInputRate :: [(VertexInputRate, String)]
showTableVertexInputRate = [(VERTEX_INPUT_RATE_VERTEX, "VERTEX"), (VERTEX_INPUT_RATE_INSTANCE, "INSTANCE")]


instance Show VertexInputRate where
showsPrec = enumShowsPrec enumPrefixVertexInputRate
                          showTableVertexInputRate
                          conNameVertexInputRate
                          (\(VertexInputRate x) -> x)
                          (showsPrec 11)


instance Read VertexInputRate where
  readPrec = enumReadPrec enumPrefixVertexInputRate showTableVertexInputRate conNameVertexInputRate VertexInputRate

