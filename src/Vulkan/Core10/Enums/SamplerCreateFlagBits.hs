{-# language CPP #-}
-- No documentation found for Chapter "SamplerCreateFlagBits"
module Vulkan.Core10.Enums.SamplerCreateFlagBits  ( SamplerCreateFlags
                                                  , SamplerCreateFlagBits( SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
                                                                         , SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
                                                                         , ..
                                                                         )
                                                  ) where

import Vulkan.Internal.Utils (enumReadPrec)
import Vulkan.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import Vulkan.Core10.FundamentalTypes (Flags)
import Vulkan.Zero (Zero)
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
-- 'SamplerCreateFlags'
newtype SamplerCreateFlagBits = SamplerCreateFlagBits Flags
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- | 'SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT' specifies that
-- the implementation /may/ use approximations when reconstructing a full
-- color value for texture access from a subsampled image.
pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = SamplerCreateFlagBits 0x00000002
-- | #samplers-subsamplesampler# 'SAMPLER_CREATE_SUBSAMPLED_BIT_EXT'
-- specifies that the sampler will read from an image created with @flags@
-- containing
-- 'Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'.
pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT                       = SamplerCreateFlagBits 0x00000001

conNameSamplerCreateFlagBits :: String
conNameSamplerCreateFlagBits = "SamplerCreateFlagBits"

enumPrefixSamplerCreateFlagBits :: String
enumPrefixSamplerCreateFlagBits = "SAMPLER_CREATE_SUBSAMPLED_"

showTableSamplerCreateFlagBits :: [(SamplerCreateFlagBits, String)]
showTableSamplerCreateFlagBits =
  [ (SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT, "COARSE_RECONSTRUCTION_BIT_EXT")
  , (SAMPLER_CREATE_SUBSAMPLED_BIT_EXT                      , "BIT_EXT")
  ]

instance Show SamplerCreateFlagBits where
  showsPrec = enumShowsPrec enumPrefixSamplerCreateFlagBits
                            showTableSamplerCreateFlagBits
                            conNameSamplerCreateFlagBits
                            (\(SamplerCreateFlagBits x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read SamplerCreateFlagBits where
  readPrec = enumReadPrec enumPrefixSamplerCreateFlagBits
                          showTableSamplerCreateFlagBits
                          conNameSamplerCreateFlagBits
                          SamplerCreateFlagBits

