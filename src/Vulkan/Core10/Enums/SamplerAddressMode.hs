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
import Vulkan.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | VkSamplerAddressMode - Specify behavior of sampling with texture
-- coordinates outside an image
--
-- = See Also
--
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#VK_VERSION_1_0 VK_VERSION_1_0>,
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
newtype SamplerAddressMode = SamplerAddressMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SAMPLER_ADDRESS_MODE_REPEAT' specifies that the repeat wrap mode will
-- be used.
pattern SAMPLER_ADDRESS_MODE_REPEAT               = SamplerAddressMode 0
-- | 'SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT' specifies that the mirrored
-- repeat wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT      = SamplerAddressMode 1
-- | 'SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE' specifies that the clamp to edge
-- wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE        = SamplerAddressMode 2
-- | 'SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER' specifies that the clamp to
-- border wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER      = SamplerAddressMode 3
-- | 'SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE' specifies that the mirror
-- clamp to edge wrap mode will be used. This is only valid if
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#features-samplerMirrorClampToEdge samplerMirrorClampToEdge>
-- is enabled, or if the @VK_KHR_sampler_mirror_clamp_to_edge@ extension is
-- enabled.
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

