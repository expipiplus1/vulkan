{-# language CPP #-}
-- No documentation found for Chapter "SamplerCreateFlagBits"
module Vulkan.Core10.Enums.SamplerCreateFlagBits  ( SamplerCreateFlags
                                                  , SamplerCreateFlagBits( SAMPLER_CREATE_SUBSAMPLED_COARSE_RECONSTRUCTION_BIT_EXT
                                                                         , SAMPLER_CREATE_SUBSAMPLED_BIT_EXT
                                                                         , ..
                                                                         )
                                                  ) where

import Data.Foldable (asum)
import GHC.Base ((<$))
import GHC.Read (choose)
import GHC.Read (expectP)
import GHC.Read (parens)
import GHC.Show (showParen)
import GHC.Show (showString)
import Numeric (showHex)
import Text.ParserCombinators.ReadP (skipSpaces)
import Text.ParserCombinators.ReadP (string)
import Text.ParserCombinators.ReadPrec ((+++))
import qualified Text.ParserCombinators.ReadPrec (lift)
import Text.ParserCombinators.ReadPrec (prec)
import Text.ParserCombinators.ReadPrec (step)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import Text.Read.Lex (Lexeme(Ident))
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
  showsPrec p e = case lookup e showTableSamplerCreateFlagBits of
    Just s -> showString enumPrefixSamplerCreateFlagBits . showString s
    Nothing ->
      let SamplerCreateFlagBits x = e
      in  showParen (p >= 11) (showString conNameSamplerCreateFlagBits . showString " 0x" . showHex x)

instance Read SamplerCreateFlagBits where
  readPrec = parens
    (   Text.ParserCombinators.ReadPrec.lift
        (do
          skipSpaces
          _ <- string enumPrefixSamplerCreateFlagBits
          asum ((\(e, s) -> e <$ string s) <$> showTableSamplerCreateFlagBits)
        )
    +++ prec
          10
          (do
            expectP (Ident conNameSamplerCreateFlagBits)
            v <- step readPrec
            pure (SamplerCreateFlagBits v)
          )
    )

