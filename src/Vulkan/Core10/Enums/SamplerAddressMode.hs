{-# language CPP #-}
-- No documentation found for Chapter "SamplerAddressMode"
module Vulkan.Core10.Enums.SamplerAddressMode  (SamplerAddressMode( SAMPLER_ADDRESS_MODE_REPEAT
                                                                  , SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
                                                                  , SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
                                                                  , SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
                                                                  , SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
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
-- No documentation found for TopLevel "VkSamplerAddressMode"
newtype SamplerAddressMode = SamplerAddressMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSamplerAddressMode" "VK_SAMPLER_ADDRESS_MODE_REPEAT"
pattern SAMPLER_ADDRESS_MODE_REPEAT               = SamplerAddressMode 0
-- No documentation found for Nested "VkSamplerAddressMode" "VK_SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT      = SamplerAddressMode 1
-- No documentation found for Nested "VkSamplerAddressMode" "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE        = SamplerAddressMode 2
-- No documentation found for Nested "VkSamplerAddressMode" "VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER      = SamplerAddressMode 3
-- No documentation found for Nested "VkSamplerAddressMode" "VK_SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE"
pattern SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE = SamplerAddressMode 4
{-# complete SAMPLER_ADDRESS_MODE_REPEAT,
             SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT,
             SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE,
             SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER,
             SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE :: SamplerAddressMode #-}

conNameSamplerAddressMode :: String
conNameSamplerAddressMode = "SamplerAddressMode"

enumPrefixSamplerAddressMode :: String
enumPrefixSamplerAddressMode = "SAMPLER_ADDRESS_MODE_"

showTableSamplerAddressMode :: [(SamplerAddressMode, String)]
showTableSamplerAddressMode =
  [ (SAMPLER_ADDRESS_MODE_REPEAT              , "REPEAT")
  , (SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT     , "MIRRORED_REPEAT")
  , (SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE       , "CLAMP_TO_EDGE")
  , (SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER     , "CLAMP_TO_BORDER")
  , (SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE, "MIRROR_CLAMP_TO_EDGE")
  ]


instance Show SamplerAddressMode where
showsPrec = enumShowsPrec enumPrefixSamplerAddressMode
                          showTableSamplerAddressMode
                          conNameSamplerAddressMode
                          (\(SamplerAddressMode x) -> x)
                          (showsPrec 11)


instance Read SamplerAddressMode where
  readPrec =
    enumReadPrec enumPrefixSamplerAddressMode showTableSamplerAddressMode conNameSamplerAddressMode SamplerAddressMode

