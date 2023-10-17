{-# language CPP #-}
-- No documentation found for Chapter "ResolveModeFlagBits"
module Vulkan.Core12.Enums.ResolveModeFlagBits  ( ResolveModeFlags
                                                , ResolveModeFlagBits( RESOLVE_MODE_NONE
                                                                     , RESOLVE_MODE_SAMPLE_ZERO_BIT
                                                                     , RESOLVE_MODE_AVERAGE_BIT
                                                                     , RESOLVE_MODE_MIN_BIT
                                                                     , RESOLVE_MODE_MAX_BIT
                                                                     , RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID
                                                                     , ..
                                                                     )
                                                ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
type ResolveModeFlags = ResolveModeFlagBits

-- | VkResolveModeFlagBits - Bitmask indicating supported depth and stencil
-- resolve modes
--
-- = Description
--
-- If no resolve mode is otherwise specified, 'RESOLVE_MODE_AVERAGE_BIT' is
-- used.
--
-- Note
--
-- No range compression or Y′CBCR model conversion is performed by
-- 'RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID'; applications have to
-- do these conversions themselves. Value outputs are expected to match
-- those that would be read through a
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-sampler-YCbCr-conversion-modelconversion Y′CBCR sampler using >.
-- The color space that the values should be in is defined by the platform
-- and is not exposed via Vulkan.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_stencil_resolve VK_KHR_depth_stencil_resolve>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo',
-- 'ResolveModeFlags',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
newtype ResolveModeFlagBits = ResolveModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'RESOLVE_MODE_NONE' indicates that no resolve operation is done.
pattern RESOLVE_MODE_NONE = ResolveModeFlagBits 0x00000000

-- | 'RESOLVE_MODE_SAMPLE_ZERO_BIT' indicates that result of the resolve
-- operation is equal to the value of sample 0.
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT = ResolveModeFlagBits 0x00000001

-- | 'RESOLVE_MODE_AVERAGE_BIT' indicates that result of the resolve
-- operation is the average of the sample values.
pattern RESOLVE_MODE_AVERAGE_BIT = ResolveModeFlagBits 0x00000002

-- | 'RESOLVE_MODE_MIN_BIT' indicates that result of the resolve operation is
-- the minimum of the sample values.
pattern RESOLVE_MODE_MIN_BIT = ResolveModeFlagBits 0x00000004

-- | 'RESOLVE_MODE_MAX_BIT' indicates that result of the resolve operation is
-- the maximum of the sample values.
pattern RESOLVE_MODE_MAX_BIT = ResolveModeFlagBits 0x00000008

-- | 'RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID' indicates that rather
-- than a multisample resolve, a single sampled color attachment will be
-- downsampled into a Y′CBCR format image specified by an external Android
-- format. Unlike other resolve modes, implementations can resolve multiple
-- times during rendering, or even bypass writing to the color attachment
-- altogether, as long as the final value is resolved to the resolve
-- attachment. Values in the G, B, and R channels of the color attachment
-- will be written to the Y, CB, and CR channels of the external format
-- image, respectively. Chroma values are calculated as if sampling with a
-- linear filter from the color attachment at full rate, at the location
-- the chroma values sit according to
-- 'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID'::@chromaOffsetX@,
-- 'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID'::@chromaOffsetY@,
-- and the chroma sample rate of the resolved image.
pattern RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID = ResolveModeFlagBits 0x00000010

conNameResolveModeFlagBits :: String
conNameResolveModeFlagBits = "ResolveModeFlagBits"

enumPrefixResolveModeFlagBits :: String
enumPrefixResolveModeFlagBits = "RESOLVE_MODE_"

showTableResolveModeFlagBits :: [(ResolveModeFlagBits, String)]
showTableResolveModeFlagBits =
  [ (RESOLVE_MODE_NONE, "NONE")
  ,
    ( RESOLVE_MODE_SAMPLE_ZERO_BIT
    , "SAMPLE_ZERO_BIT"
    )
  , (RESOLVE_MODE_AVERAGE_BIT, "AVERAGE_BIT")
  , (RESOLVE_MODE_MIN_BIT, "MIN_BIT")
  , (RESOLVE_MODE_MAX_BIT, "MAX_BIT")
  ,
    ( RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID
    , "EXTERNAL_FORMAT_DOWNSAMPLE_ANDROID"
    )
  ]

instance Show ResolveModeFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixResolveModeFlagBits
      showTableResolveModeFlagBits
      conNameResolveModeFlagBits
      (\(ResolveModeFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read ResolveModeFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixResolveModeFlagBits
      showTableResolveModeFlagBits
      conNameResolveModeFlagBits
      ResolveModeFlagBits
