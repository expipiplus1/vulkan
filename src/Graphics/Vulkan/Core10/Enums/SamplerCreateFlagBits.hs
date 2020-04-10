{-# language CPP #-}
module Graphics.Vulkan.Core10.Enums.SamplerCreateFlagBits  ( SamplerCreateFlagBits( SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
                                                                                  , SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
                                                                                  , ..
                                                                                  )
                                                           , SamplerCreateFlags
                                                           ) where

import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadPrec ((+++))
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
import Graphics.Vulkan.Core10.BaseType (Flags)
import Graphics.Vulkan.Zero (Zero)
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
  deriving newtype (Eq, Ord, Storable, Zero, Bits)

-- | 'SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT' specifies that
-- the implementation /may/ use approximations when reconstructing a full
-- color value for texture access from a subsampled image.
pattern SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT = SamplerCreateFlagBits 0x00000002
-- | 'SAMPLER_CREATE_SUBSAMPLED_BIT_EXT' specifies that the sampler will read
-- from an image created with @flags@ containing
-- 'Graphics.Vulkan.Core10.Enums.ImageCreateFlagBits.IMAGE_CREATE_SUBSAMPLED_BIT_EXT'.
pattern SAMPLER_CREATE_SUBSAMPLED_BIT_EXT = SamplerCreateFlagBits 0x00000001

type SamplerCreateFlags = SamplerCreateFlagBits

instance Show SamplerCreateFlagBits where
  showsPrec p = \case
    SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT -> showString "SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT"
    SAMPLER_CREATE_SUBSAMPLED_BIT_EXT -> showString "SAMPLER_CREATE_SUBSAMPLED_BIT_EXT"
    SamplerCreateFlagBits x -> showParen (p >= 11) (showString "SamplerCreateFlagBits 0x" . showHex x)

instance Read SamplerCreateFlagBits where
  readPrec = parens (choose [("SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT", pure SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT)
                            , ("SAMPLER_CREATE_SUBSAMPLED_BIT_EXT", pure SAMPLER_CREATE_SUBSAMPLED_BIT_EXT)]
                     +++
                     prec 10 (do
                       expectP (Ident "SamplerCreateFlagBits")
                       v <- step readPrec
                       pure (SamplerCreateFlagBits v)))

