{-# language CPP #-}
-- No documentation found for Chapter "PipelineBindPoint"
module Vulkan.Core10.Enums.PipelineBindPoint  (PipelineBindPoint( PIPELINE_BIND_POINT_GRAPHICS
                                                                , PIPELINE_BIND_POINT_COMPUTE
                                                                , PIPELINE_BIND_POINT_RAY_TRACING_KHR
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
-- No documentation found for TopLevel "VkPipelineBindPoint"
newtype PipelineBindPoint = PipelineBindPoint Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkPipelineBindPoint" "VK_PIPELINE_BIND_POINT_GRAPHICS"
pattern PIPELINE_BIND_POINT_GRAPHICS        = PipelineBindPoint 0
-- No documentation found for Nested "VkPipelineBindPoint" "VK_PIPELINE_BIND_POINT_COMPUTE"
pattern PIPELINE_BIND_POINT_COMPUTE         = PipelineBindPoint 1
-- No documentation found for Nested "VkPipelineBindPoint" "VK_PIPELINE_BIND_POINT_RAY_TRACING_KHR"
pattern PIPELINE_BIND_POINT_RAY_TRACING_KHR = PipelineBindPoint 1000165000
{-# complete PIPELINE_BIND_POINT_GRAPHICS,
             PIPELINE_BIND_POINT_COMPUTE,
             PIPELINE_BIND_POINT_RAY_TRACING_KHR :: PipelineBindPoint #-}

conNamePipelineBindPoint :: String
conNamePipelineBindPoint = "PipelineBindPoint"

enumPrefixPipelineBindPoint :: String
enumPrefixPipelineBindPoint = "PIPELINE_BIND_POINT_"

showTablePipelineBindPoint :: [(PipelineBindPoint, String)]
showTablePipelineBindPoint =
  [ (PIPELINE_BIND_POINT_GRAPHICS       , "GRAPHICS")
  , (PIPELINE_BIND_POINT_COMPUTE        , "COMPUTE")
  , (PIPELINE_BIND_POINT_RAY_TRACING_KHR, "RAY_TRACING_KHR")
  ]


instance Show PipelineBindPoint where
showsPrec = enumShowsPrec enumPrefixPipelineBindPoint
                          showTablePipelineBindPoint
                          conNamePipelineBindPoint
                          (\(PipelineBindPoint x) -> x)
                          (showsPrec 11)


instance Read PipelineBindPoint where
  readPrec =
    enumReadPrec enumPrefixPipelineBindPoint showTablePipelineBindPoint conNamePipelineBindPoint PipelineBindPoint

