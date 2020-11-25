{-# language CPP #-}
-- No documentation found for Chapter "SamplerMipmapMode"
module Vulkan.Core10.Enums.SamplerMipmapMode  (SamplerMipmapMode( SAMPLER_MIPMAP_MODE_NEAREST
                                                                , SAMPLER_MIPMAP_MODE_LINEAR
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
-- No documentation found for TopLevel "VkSamplerMipmapMode"
newtype SamplerMipmapMode = SamplerMipmapMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- No documentation found for Nested "VkSamplerMipmapMode" "VK_SAMPLER_MIPMAP_MODE_NEAREST"
pattern SAMPLER_MIPMAP_MODE_NEAREST = SamplerMipmapMode 0
-- No documentation found for Nested "VkSamplerMipmapMode" "VK_SAMPLER_MIPMAP_MODE_LINEAR"
pattern SAMPLER_MIPMAP_MODE_LINEAR  = SamplerMipmapMode 1
{-# complete SAMPLER_MIPMAP_MODE_NEAREST,
             SAMPLER_MIPMAP_MODE_LINEAR :: SamplerMipmapMode #-}

conNameSamplerMipmapMode :: String
conNameSamplerMipmapMode = "SamplerMipmapMode"

enumPrefixSamplerMipmapMode :: String
enumPrefixSamplerMipmapMode = "SAMPLER_MIPMAP_MODE_"

showTableSamplerMipmapMode :: [(SamplerMipmapMode, String)]
showTableSamplerMipmapMode = [(SAMPLER_MIPMAP_MODE_NEAREST, "NEAREST"), (SAMPLER_MIPMAP_MODE_LINEAR, "LINEAR")]


instance Show SamplerMipmapMode where
showsPrec = enumShowsPrec enumPrefixSamplerMipmapMode
                          showTableSamplerMipmapMode
                          conNameSamplerMipmapMode
                          (\(SamplerMipmapMode x) -> x)
                          (showsPrec 11)


instance Read SamplerMipmapMode where
  readPrec =
    enumReadPrec enumPrefixSamplerMipmapMode showTableSamplerMipmapMode conNameSamplerMipmapMode SamplerMipmapMode

