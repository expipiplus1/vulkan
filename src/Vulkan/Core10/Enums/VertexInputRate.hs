{-# language CPP #-}
-- No documentation found for Chapter "VertexInputRate"
module Vulkan.Core10.Enums.VertexInputRate  (VertexInputRate( VERTEX_INPUT_RATE_VERTEX
                                                            , VERTEX_INPUT_RATE_INSTANCE
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

-- | VkVertexInputRate - Specify rate at which vertex attributes are pulled
-- from buffers
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Pipeline.VertexInputBindingDescription',
-- 'Vulkan.Extensions.VK_EXT_vertex_input_dynamic_state.VertexInputBindingDescription2EXT'
newtype VertexInputRate = VertexInputRate Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'VERTEX_INPUT_RATE_VERTEX' specifies that vertex attribute addressing is
-- a function of the vertex index.
pattern VERTEX_INPUT_RATE_VERTEX = VertexInputRate 0

-- | 'VERTEX_INPUT_RATE_INSTANCE' specifies that vertex attribute addressing
-- is a function of the instance index.
pattern VERTEX_INPUT_RATE_INSTANCE = VertexInputRate 1

{-# COMPLETE
  VERTEX_INPUT_RATE_VERTEX
  , VERTEX_INPUT_RATE_INSTANCE ::
    VertexInputRate
  #-}

conNameVertexInputRate :: String
conNameVertexInputRate = "VertexInputRate"

enumPrefixVertexInputRate :: String
enumPrefixVertexInputRate = "VERTEX_INPUT_RATE_"

showTableVertexInputRate :: [(VertexInputRate, String)]
showTableVertexInputRate =
  [ (VERTEX_INPUT_RATE_VERTEX, "VERTEX")
  , (VERTEX_INPUT_RATE_INSTANCE, "INSTANCE")
  ]

instance Show VertexInputRate where
  showsPrec =
    enumShowsPrec
      enumPrefixVertexInputRate
      showTableVertexInputRate
      conNameVertexInputRate
      (\(VertexInputRate x) -> x)
      (showsPrec 11)

instance Read VertexInputRate where
  readPrec =
    enumReadPrec
      enumPrefixVertexInputRate
      showTableVertexInputRate
      conNameVertexInputRate
      VertexInputRate
