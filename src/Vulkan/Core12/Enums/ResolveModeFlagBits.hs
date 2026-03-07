{-# language CPP #-}
-- No documentation found for Chapter "ResolveModeFlagBits"
module Vulkan.Core12.Enums.ResolveModeFlagBits  ( ResolveModeFlags
                                                , ResolveModeFlagBits( RESOLVE_MODE_NONE
                                                                     , RESOLVE_MODE_SAMPLE_ZERO_BIT
                                                                     , RESOLVE_MODE_AVERAGE_BIT
                                                                     , RESOLVE_MODE_MIN_BIT
                                                                     , RESOLVE_MODE_MAX_BIT
                                                                     , RESOLVE_MODE_CUSTOM_BIT_EXT
                                                                     , RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID
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
-- -   'RESOLVE_MODE_NONE' specifies that no resolve operation is done.
--
-- -   'RESOLVE_MODE_SAMPLE_ZERO_BIT' specifies that result of the resolve
--     operation is equal to the value of sample 0.
--
-- -   'RESOLVE_MODE_AVERAGE_BIT' specifies that result of the resolve
--     operation is the average of the sample values.
--
-- -   'RESOLVE_MODE_MIN_BIT' specifies that result of the resolve
--     operation is the minimum of the sample values.
--
-- -   'RESOLVE_MODE_MAX_BIT' specifies that result of the resolve
--     operation is the maximum of the sample values.
--
-- -   'RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID' specifies that
--     rather than a multisample resolve, a single sampled color attachment
--     will be downsampled into a Y′CBCR format image specified by an
--     external Android format. Unlike other resolve modes, implementations
--     can resolve multiple times during rendering, or even bypass writing
--     to the color attachment altogether, as long as the final value is
--     resolved to the resolve attachment. Values in the G, B, and R
--     channels of the color attachment will be written to the Y, CB, and
--     CR channels of the external format image, respectively. Chroma
--     values are calculated as if sampling with a linear filter from the
--     color attachment at full rate, at the location the chroma values sit
--     according to
--     'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID'::@externalFormatResolveChromaOffsetX@,
--     'Vulkan.Extensions.VK_ANDROID_external_format_resolve.PhysicalDeviceExternalFormatResolvePropertiesANDROID'::@externalFormatResolveChromaOffsetY@,
--     and the chroma sample rate of the resolved image.
--
-- -   'RESOLVE_MODE_CUSTOM_BIT_EXT' specifies that the attachment will be
--     resolved by shaders in the render pass instead of fixed-function
--     operations.
--
-- If no resolve mode is otherwise specified, 'RESOLVE_MODE_AVERAGE_BIT' is
-- used.
--
-- If 'RESOLVE_MODE_AVERAGE_BIT' is used, and the source format is a
-- floating-point or normalized type, the sample values for each pixel are
-- resolved with implementation-defined numerical precision.
--
-- If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#formats-numericformat numeric format>
-- of the resolve attachment uses sRGB encoding, the implementation
-- /should/ convert samples from nonlinear to linear before averaging
-- samples as described in the “sRGB EOTF” section of the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#data-format Khronos Data Format Specification>.
-- In this case, the implementation /must/ convert the linear averaged
-- value to nonlinear before writing the resolved result to resolve
-- attachment. If the
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#features-maintenance10 maintenance10>
-- feature is enabled, whether a nonlinear to linear conversion happens for
-- sRGB resolve is defined by
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#limits-resolveSrgbFormatAppliesTransferFunction resolveSrgbFormatAppliesTransferFunction>.
-- This behavior /can/ be overridden with appropriate
-- @VK_*@/RESOLVE/{SKIP,ENABLE}_TRANSFER_FUNCTION_BIT_KHR flag usage.
--
-- No range compression or Y′CBCR model conversion is performed by
-- 'RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID'; applications have
-- to do these conversions themselves. Value outputs are expected to match
-- those that would be read through a
-- <https://registry.khronos.org/vulkan/specs/latest/html/vkspec.html#textures-sampler-YCbCr-conversion-modelconversion Y′CBCR sampler using >.
-- The color space that the values should be in is defined by the platform
-- and is not exposed via Vulkan.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_KHR_depth_stencil_resolve VK_KHR_depth_stencil_resolve>,
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_2 VK_VERSION_1_2>,
-- 'Vulkan.Core13.Promoted_From_VK_KHR_dynamic_rendering.RenderingAttachmentInfo',
-- 'Vulkan.Extensions.VK_KHR_maintenance10.ResolveImageModeInfoKHR',
-- 'ResolveModeFlags',
-- 'Vulkan.Core12.Promoted_From_VK_KHR_depth_stencil_resolve.SubpassDescriptionDepthStencilResolve'
newtype ResolveModeFlagBits = ResolveModeFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_NONE"
pattern RESOLVE_MODE_NONE = ResolveModeFlagBits 0x00000000

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_SAMPLE_ZERO_BIT"
pattern RESOLVE_MODE_SAMPLE_ZERO_BIT = ResolveModeFlagBits 0x00000001

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_AVERAGE_BIT"
pattern RESOLVE_MODE_AVERAGE_BIT = ResolveModeFlagBits 0x00000002

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_MIN_BIT"
pattern RESOLVE_MODE_MIN_BIT = ResolveModeFlagBits 0x00000004

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_MAX_BIT"
pattern RESOLVE_MODE_MAX_BIT = ResolveModeFlagBits 0x00000008

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_CUSTOM_BIT_EXT"
pattern RESOLVE_MODE_CUSTOM_BIT_EXT = ResolveModeFlagBits 0x00000020

-- No documentation found for Nested "VkResolveModeFlagBits" "VK_RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID"
pattern RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID = ResolveModeFlagBits 0x00000010

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
  , (RESOLVE_MODE_CUSTOM_BIT_EXT, "CUSTOM_BIT_EXT")
  ,
    ( RESOLVE_MODE_EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID
    , "EXTERNAL_FORMAT_DOWNSAMPLE_BIT_ANDROID"
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
