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
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkSamplerReductionMode - Specify reduction mode for texture filtering
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_EXT_sampler_filter_minmax VK_EXT_sampler_filter_minmax>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
newtype SamplerReductionMode = SamplerReductionMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE' specifies that texel values
-- are combined by computing a weighted average of values in the footprint,
-- using weights as specified in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-unnormalized-to-integer the image operations chapter>.
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE = SamplerReductionMode 0
-- | 'SAMPLER_REDUCTION_MODE_MIN' specifies that texel values are combined by
-- taking the component-wise minimum of values in the footprint with
-- non-zero weights.
pattern SAMPLER_REDUCTION_MODE_MIN              = SamplerReductionMode 1
-- | 'SAMPLER_REDUCTION_MODE_MAX' specifies that texel values are combined by
-- taking the component-wise maximum of values in the footprint with
-- non-zero weights.
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

