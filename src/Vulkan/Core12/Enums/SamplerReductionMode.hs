{-# language CPP #-}
module Vulkan.Core12.Enums.SamplerReductionMode  (SamplerReductionMode( SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE
                                                                      , SAMPLER_REDUCTION_MODE_MIN
                                                                      , SAMPLER_REDUCTION_MODE_MAX
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
-- | VkSamplerReductionMode - Specify reduction mode for texture filtering
--
-- = See Also
--
-- 'Vulkan.Core12.Promoted_From_VK_EXT_sampler_filter_minmax.SamplerReductionModeCreateInfo'
newtype SamplerReductionMode = SamplerReductionMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE' specifies that texel values
-- are combined by computing a weighted average of values in the footprint,
-- using weights as specified in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-unnormalized-to-integer the image operations chapter>.
pattern SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE = SamplerReductionMode 0
-- | 'SAMPLER_REDUCTION_MODE_MIN' specifies that texel values are combined by
-- taking the component-wise minimum of values in the footprint with
-- non-zero weights.
pattern SAMPLER_REDUCTION_MODE_MIN = SamplerReductionMode 1
-- | 'SAMPLER_REDUCTION_MODE_MAX' specifies that texel values are combined by
-- taking the component-wise maximum of values in the footprint with
-- non-zero weights.
pattern SAMPLER_REDUCTION_MODE_MAX = SamplerReductionMode 2
{-# complete SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE,
             SAMPLER_REDUCTION_MODE_MIN,
             SAMPLER_REDUCTION_MODE_MAX :: SamplerReductionMode #-}

instance Show SamplerReductionMode where
  showsPrec p = \case
    SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE -> showString "SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE"
    SAMPLER_REDUCTION_MODE_MIN -> showString "SAMPLER_REDUCTION_MODE_MIN"
    SAMPLER_REDUCTION_MODE_MAX -> showString "SAMPLER_REDUCTION_MODE_MAX"
    SamplerReductionMode x -> showParen (p >= 11) (showString "SamplerReductionMode " . showsPrec 11 x)

instance Read SamplerReductionMode where
  readPrec = parens (choose [("SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE", pure SAMPLER_REDUCTION_MODE_WEIGHTED_AVERAGE)
                            , ("SAMPLER_REDUCTION_MODE_MIN", pure SAMPLER_REDUCTION_MODE_MIN)
                            , ("SAMPLER_REDUCTION_MODE_MAX", pure SAMPLER_REDUCTION_MODE_MAX)]
                     +++
                     prec 10 (do
                       expectP (Ident "SamplerReductionMode")
                       v <- step readPrec
                       pure (SamplerReductionMode v)))

