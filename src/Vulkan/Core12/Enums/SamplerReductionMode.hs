{-# language CPP #-}
-- No documentation found for Chapter "SamplerReductionMode"
module Vulkan.Core12.Enums.SamplerReductionMode  (SamplerReductionMode( SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE
                                                                      , SAMPLER_REDUCTION_MODE_MIN
                                                                      , SAMPLER_REDUCTION_MODE_MAX
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
-- No documentation found for TopLevel "VkSamplerReductionMode"
newtype SamplerReductionMode = SamplerReductionMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSamplerReductionMode" "VK_SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE"
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE = SamplerReductionMode 0
-- No documentation found for Nested "VkSamplerReductionMode" "VK_SAMPLER_REDUCTION_MODE_MIN"
pattern SAMPLER_REDUCTION_MODE_MIN              = SamplerReductionMode 1
-- No documentation found for Nested "VkSamplerReductionMode" "VK_SAMPLER_REDUCTION_MODE_MAX"
pattern SAMPLER_REDUCTION_MODE_MAX              = SamplerReductionMode 2
{-# complete SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE,
             SAMPLER_REDUCTION_MODE_MIN,
             SAMPLER_REDUCTION_MODE_MAX :: SamplerReductionMode #-}

conNameSamplerReductionMode :: String
conNameSamplerReductionMode = "SamplerReductionMode"

enumPrefixSamplerReductionMode :: String
enumPrefixSamplerReductionMode = "SAMPLER_REDUCTION_MODE_"

showTableSamplerReductionMode :: [(SamplerReductionMode, String)]
showTableSamplerReductionMode =
  [ (SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE, "WEIGHTED_AVERAGE")
  , (SAMPLER_REDUCTION_MODE_MIN             , "MIN")
  , (SAMPLER_REDUCTION_MODE_MAX             , "MAX")
  ]


instance Show SamplerReductionMode where
showsPrec = enumShowsPrec enumPrefixSamplerReductionMode
                          showTableSamplerReductionMode
                          conNameSamplerReductionMode
                          (\(SamplerReductionMode x) -> x)
                          (showsPrec 11)


instance Read SamplerReductionMode where
  readPrec = enumReadPrec enumPrefixSamplerReductionMode
                          showTableSamplerReductionMode
                          conNameSamplerReductionMode
                          SamplerReductionMode

