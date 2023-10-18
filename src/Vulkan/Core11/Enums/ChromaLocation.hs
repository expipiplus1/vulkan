{-# language CPP #-}
-- No documentation found for Chapter "ChromaLocation"
module Vulkan.Core11.Enums.ChromaLocation  (ChromaLocation( CHROMA_LOCATION_COSITED_EVEN
                                                          , CHROMA_LOCATION_MIDPOINT
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

-- | VkChromaLocation - Position of downsampled chroma samples
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_1 VK_VERSION_1_1>,
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatProperties2ANDROID',
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Vulkan.Extensions.VK_FUCHSIA_buffer_collection.BufferCollectionPropertiesFUCHSIA',
-- 'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Extensions.VK_QNX_external_memory_screen_buffer.ScreenBufferFormatPropertiesQNX'
newtype ChromaLocation = ChromaLocation Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'CHROMA_LOCATION_COSITED_EVEN' specifies that downsampled chroma samples
-- are aligned with luma samples with even coordinates.
pattern CHROMA_LOCATION_COSITED_EVEN = ChromaLocation 0

-- | 'CHROMA_LOCATION_MIDPOINT' specifies that downsampled chroma samples are
-- located half way between each even luma sample and the nearest higher
-- odd luma sample.
pattern CHROMA_LOCATION_MIDPOINT = ChromaLocation 1

{-# COMPLETE
  CHROMA_LOCATION_COSITED_EVEN
  , CHROMA_LOCATION_MIDPOINT ::
    ChromaLocation
  #-}

conNameChromaLocation :: String
conNameChromaLocation = "ChromaLocation"

enumPrefixChromaLocation :: String
enumPrefixChromaLocation = "CHROMA_LOCATION_"

showTableChromaLocation :: [(ChromaLocation, String)]
showTableChromaLocation =
  [ (CHROMA_LOCATION_COSITED_EVEN, "COSITED_EVEN")
  , (CHROMA_LOCATION_MIDPOINT, "MIDPOINT")
  ]

instance Show ChromaLocation where
  showsPrec =
    enumShowsPrec
      enumPrefixChromaLocation
      showTableChromaLocation
      conNameChromaLocation
      (\(ChromaLocation x) -> x)
      (showsPrec 11)

instance Read ChromaLocation where
  readPrec =
    enumReadPrec
      enumPrefixChromaLocation
      showTableChromaLocation
      conNameChromaLocation
      ChromaLocation
