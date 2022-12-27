{-# language CPP #-}
-- No documentation found for Chapter "SamplerCreateFlagBits"
module Vulkan.Core10.Enums.SamplerCreateFlagBits  ( SamplerCreateFlags
                                                  , SamplerCreateFlagBits( SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM
                                                                         , SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT
                                                                         , SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT
                                                                         , SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
                                                                         , SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
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
type SamplerCreateFlags = SamplerCreateFlagBits

-- | VkSamplerCreateFlagBits - Bitmask specifying additional parameters of
-- sampler
--
-- = Description
--
-- Note
--
-- The approximations used when
-- 'SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT' is specified
-- are implementation defined. Some implementations /may/ interpolate
-- between fragment density levels in a subsampled image. In that case,
-- this bit /may/ be used to decide whether the interpolation factors are
-- calculated per fragment or at a coarser granularity.
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'SamplerCreateFlags'
newtype SamplerCreateFlagBits = SamplerCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | #samplers-imageprocessingsampler#
-- 'SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM' specifies that the sampler
-- will read from images using only @OpImageWeightedSampleQCOM@,
-- @OpImageBoxFilterQCOM@, @OpImageBlockMatchSSDQCOM@, or
-- @OpImageBlockMatchSADQCOM@.
pattern SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM = SamplerCreateFlagBits 0x00000010

-- | 'SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT' specifies that
-- <https://registry.khronos.org/vulkan/specs/1.3-extensions/html/vkspec.html#textures-cubemapedge cube map edge handling>
-- is not performed.
pattern SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT = SamplerCreateFlagBits 0x00000004

-- | 'SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT' specifies that
-- the sampler /can/ be used with descriptor buffers when capturing and
-- replaying (e.g. for trace capture and replay), see
-- 'Vulkan.Extensions.VK_EXT_descriptor_buffer.OpaqueCaptureDescriptorDataCreateInfoEXT'
-- for more detail.
pattern SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT = SamplerCreateFlagBits 0x00000008

-- | 'SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT' specifies that
-- the implementation /may/ use approximations when reconstructing a full
-- color value for texture access from a subsampled image.
pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = SamplerCreateFlagBits 0x00000002

-- | #samplers-subsamplesampler# 'SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
-- specifies that the sampler will read from an image created with @flags@
-- containing
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'.
pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT = SamplerCreateFlagBits 0x00000001

conNameSamplerCreateFlagBits :: String
conNameSamplerCreateFlagBits = "SamplerCreateFlagBits"

enumPrefixSamplerCreateFlagBits :: String
enumPrefixSamplerCreateFlagBits = "SAMPLER_CREATE_"

showTableSamplerCreateFlagBits :: [(SamplerCreateFlagBits, String)]
showTableSamplerCreateFlagBits =
  [
    ( SAMPLER_CREATE_IMAGE_PROCESSING_BIT_QCOM
    , "IMAGE_PROCESSING_BIT_QCOM"
    )
  ,
    ( SAMPLER_CREATE_NON_SEAMLESS_CUBE_MAP_BIT_EXT
    , "NON_SEAMLESS_CUBE_MAP_BIT_EXT"
    )
  ,
    ( SAMPLER_CREATE_DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT
    , "DESCRIPTOR_BUFFER_CAPTURE_REPLAY_BIT_EXT"
    )
  ,
    ( SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
    , "SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT"
    )
  ,
    ( SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
    , "SUBSAMPLED_BIT_EXT"
    )
  ]

instance Show SamplerCreateFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixSamplerCreateFlagBits
      showTableSamplerCreateFlagBits
      conNameSamplerCreateFlagBits
      (\(SamplerCreateFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read SamplerCreateFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixSamplerCreateFlagBits
      showTableSamplerCreateFlagBits
      conNameSamplerCreateFlagBits
      SamplerCreateFlagBits
