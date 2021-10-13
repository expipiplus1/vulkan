{-# language CPP #-}
-- No documentation found for Chapter "Filter"
module Vulkan.Core10.Enums.Filter  (Filter( FILTER_NEAREST
                                          , FILTER_LINEAR
                                          , FILTER_CUBIC_IMG
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

-- | VkFilter - Specify filters used for texture lookups
--
-- = Description
--
-- These filters are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Extensions.VK_KHR_copy_commands2.BlitImageInfo2KHR',
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'
newtype Filter = Filter Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FILTER_NEAREST' specifies nearest filtering.
pattern FILTER_NEAREST   = Filter 0
-- | 'FILTER_LINEAR' specifies linear filtering.
pattern FILTER_LINEAR    = Filter 1
-- No documentation found for Nested "VkFilter" "VK_FILTER_CUBIC_IMG"
pattern FILTER_CUBIC_IMG = Filter 1000015000
{-# complete FILTER_NEAREST,
             FILTER_LINEAR,
             FILTER_CUBIC_IMG :: Filter #-}

conNameFilter :: String
conNameFilter = "Filter"

enumPrefixFilter :: String
enumPrefixFilter = "FILTER_"

showTableFilter :: [(Filter, String)]
showTableFilter = [(FILTER_NEAREST, "NEAREST"), (FILTER_LINEAR, "LINEAR"), (FILTER_CUBIC_IMG, "CUBIC_IMG")]

instance Show Filter where
  showsPrec = enumShowsPrec enumPrefixFilter showTableFilter conNameFilter (\(Filter x) -> x) (showsPrec 11)

instance Read Filter where
  readPrec = enumReadPrec enumPrefixFilter showTableFilter conNameFilter Filter

