{-# language CPP #-}
-- No documentation found for Chapter "SamplerYcbcrRange"
module Vulkan.Core11.Enums.SamplerYcbcrRange  (SamplerYcbcrRange( SAMPLER_YCBCR_RANGE_ITU_FULL
                                                                , SAMPLER_YCBCR_RANGE_ITU_NARROW
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
-- No documentation found for TopLevel "VkSamplerYcbcrRange"
newtype SamplerYcbcrRange = SamplerYcbcrRange Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSamplerYcbcrRange" "VK_SAMPLER_YCBCR_RANGE_ITU_FULL"
pattern SAMPLER_YCBCR_RANGE_ITU_FULL   = SamplerYcbcrRange 0
-- No documentation found for Nested "VkSamplerYcbcrRange" "VK_SAMPLER_YCBCR_RANGE_ITU_NARROW"
pattern SAMPLER_YCBCR_RANGE_ITU_NARROW = SamplerYcbcrRange 1
{-# complete SAMPLER_YCBCR_RANGE_ITU_FULL,
             SAMPLER_YCBCR_RANGE_ITU_NARROW :: SamplerYcbcrRange #-}

conNameSamplerYcbcrRange :: String
conNameSamplerYcbcrRange = "SamplerYcbcrRange"

enumPrefixSamplerYcbcrRange :: String
enumPrefixSamplerYcbcrRange = "SAMPLER_YCBCR_RANGE_ITU_"

showTableSamplerYcbcrRange :: [(SamplerYcbcrRange, String)]
showTableSamplerYcbcrRange = [(SAMPLER_YCBCR_RANGE_ITU_FULL, "FULL"), (SAMPLER_YCBCR_RANGE_ITU_NARROW, "NARROW")]


instance Show SamplerYcbcrRange where
showsPrec = enumShowsPrec enumPrefixSamplerYcbcrRange
                          showTableSamplerYcbcrRange
                          conNameSamplerYcbcrRange
                          (\(SamplerYcbcrRange x) -> x)
                          (showsPrec 11)


instance Read SamplerYcbcrRange where
  readPrec =
    enumReadPrec enumPrefixSamplerYcbcrRange showTableSamplerYcbcrRange conNameSamplerYcbcrRange SamplerYcbcrRange

