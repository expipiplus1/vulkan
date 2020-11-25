{-# language CPP #-}
-- No documentation found for Chapter "SamplerYcbcrModelConversion"
module Vulkan.Core11.Enums.SamplerYcbcrModelConversion  (SamplerYcbcrModelConversion( SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601
                                                                                    , SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020
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
-- No documentation found for TopLevel "VkSamplerYcbcrModelConversion"
newtype SamplerYcbcrModelConversion = SamplerYcbcrModelConversion Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY   = SamplerYcbcrModelConversion 0
-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY = SamplerYcbcrModelConversion 1
-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709      = SamplerYcbcrModelConversion 2
-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601      = SamplerYcbcrModelConversion 3
-- No documentation found for Nested "VkSamplerYcbcrModelConversion" "VK_SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020"
pattern SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020     = SamplerYcbcrModelConversion 4
{-# complete SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY,
             SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY,
             SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709,
             SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601,
             SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020 :: SamplerYcbcrModelConversion #-}

conNameSamplerYcbcrModelConversion :: String
conNameSamplerYcbcrModelConversion = "SamplerYcbcrModelConversion"

enumPrefixSamplerYcbcrModelConversion :: String
enumPrefixSamplerYcbcrModelConversion = "SAMPLER_YCBCR_MODEL_CONVERSION_"

showTableSamplerYcbcrModelConversion :: [(SamplerYcbcrModelConversion, String)]
showTableSamplerYcbcrModelConversion =
  [ (SAMPLER_YCBCR_MODEL_CONVERSION_RGB_IDENTITY  , "RGB_IDENTITY")
  , (SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_IDENTITY, "YCBCR_IDENTITY")
  , (SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_709     , "YCBCR_709")
  , (SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_601     , "YCBCR_601")
  , (SAMPLER_YCBCR_MODEL_CONVERSION_YCBCR_2020    , "YCBCR_2020")
  ]


instance Show SamplerYcbcrModelConversion where
showsPrec = enumShowsPrec enumPrefixSamplerYcbcrModelConversion
                          showTableSamplerYcbcrModelConversion
                          conNameSamplerYcbcrModelConversion
                          (\(SamplerYcbcrModelConversion x) -> x)
                          (showsPrec 11)


instance Read SamplerYcbcrModelConversion where
  readPrec = enumReadPrec enumPrefixSamplerYcbcrModelConversion
                          showTableSamplerYcbcrModelConversion
                          conNameSamplerYcbcrModelConversion
                          SamplerYcbcrModelConversion

