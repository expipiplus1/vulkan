{-# language CPP #-}
module Vulkan.Core10.Enums.SamplerMipmapMode  (SamplerMipmapMode( SAMPLER_MIPMAP_MODE_NEAREST
                                                                , SAMPLER_MIPMAP_MODE_LINEAR
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
-- | VkSamplerMipmapMode - Specify mipmap mode used for texture lookups
--
-- = Description
--
-- These modes are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo'
newtype SamplerMipmapMode = SamplerMipmapMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SAMPLER_MIPMAP_MODE_NEAREST' specifies nearest filtering.
pattern SAMPLER_MIPMAP_MODE_NEAREST = SamplerMipmapMode 0
-- | 'SAMPLER_MIPMAP_MODE_LINEAR' specifies linear filtering.
pattern SAMPLER_MIPMAP_MODE_LINEAR = SamplerMipmapMode 1
{-# complete SAMPLER_MIPMAP_MODE_NEAREST,
             SAMPLER_MIPMAP_MODE_LINEAR :: SamplerMipmapMode #-}

instance Show SamplerMipmapMode where
  showsPrec p = \case
    SAMPLER_MIPMAP_MODE_NEAREST -> showString "SAMPLER_MIPMAP_MODE_NEAREST"
    SAMPLER_MIPMAP_MODE_LINEAR -> showString "SAMPLER_MIPMAP_MODE_LINEAR"
    SamplerMipmapMode x -> showParen (p >= 11) (showString "SamplerMipmapMode " . showsPrec 11 x)

instance Read SamplerMipmapMode where
  readPrec = parens (choose [("SAMPLER_MIPMAP_MODE_NEAREST", pure SAMPLER_MIPMAP_MODE_NEAREST)
                            , ("SAMPLER_MIPMAP_MODE_LINEAR", pure SAMPLER_MIPMAP_MODE_LINEAR)]
                     +++
                     prec 10 (do
                       expectP (Ident "SamplerMipmapMode")
                       v <- step readPrec
                       pure (SamplerMipmapMode v)))

