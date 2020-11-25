{-# language CPP #-}
-- No documentation found for Chapter "SamplerAddressMode"
module Vulkan.Core10.Enums.SamplerAddressMode  (SamplerAddressMode( SAMPLER_ADDRESS_MODE_REPEAT
                                                                  , SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT
                                                                  , SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE
                                                                  , SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER
                                                                  , SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE
                                                                  , ..
                                                                  )) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import GHC.Show (showsPrec)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Vulkan.Zero (Zero)
-- | VkSamplerAddressMode - Specify behavior of sampling with texture
-- coordinates outside an image
--
-- = See Also
--
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
newtype SamplerAddressMode = SamplerAddressMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SAMPLER_ADDRESS_MODE_REPEAT' specifies that the repeat wrap mode will
-- be used.
pattern SAMPLER_ADDRESS_MODE_REPEAT = SamplerAddressMode 0
-- | 'SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT' specifies that the mirrored
-- repeat wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT = SamplerAddressMode 1
-- | 'SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE' specifies that the clamp to edge
-- wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE = SamplerAddressMode 2
-- | 'SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER' specifies that the clamp to
-- border wrap mode will be used.
pattern SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER = SamplerAddressMode 3
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

instance Show SamplerAddressMode where
  showsPrec p = \case
    SAMPLER_ADDRESS_MODE_REPEAT -> showString "SAMPLER_ADDRESS_MODE_REPEAT"
    SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT -> showString "SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT"
    SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE -> showString "SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE"
    SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER -> showString "SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER"
    SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE -> showString "SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE"
    SamplerAddressMode x -> showParen (p >= 11) (showString "SamplerAddressMode " . showsPrec 11 x)

instance Read SamplerAddressMode where
  readPrec = parens (choose [("SAMPLER_ADDRESS_MODE_REPEAT", pure SAMPLER_ADDRESS_MODE_REPEAT)
                            , ("SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT", pure SAMPLER_ADDRESS_MODE_MIRRORED_REPEAT)
                            , ("SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE", pure SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE)
                            , ("SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER", pure SAMPLER_ADDRESS_MODE_CLAMP_TO_BORDER)
                            , ("SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE", pure SAMPLER_ADDRESS_MODE_MIRROR_CLAMP_TO_EDGE)]
                     +++
                     prec 10 (do
                       expectP (Ident "SamplerAddressMode")
                       v <- step readPrec
                       pure (SamplerAddressMode v)))

