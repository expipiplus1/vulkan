{-# language CPP #-}
module Vulkan.Core10.Enums.Filter  (Filter( FILTER_NEAREST
                                          , FILTER_LINEAR
                                          , FILTER_CUBIC_IMG
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
-- | VkFilter - Specify filters used for texture lookups
--
-- = Description
--
-- These filters are described in detail in
-- <https://www.khronos.org/registry/vulkan/specs/1.2-extensions/html/vkspec.html#textures-texel-filtering Texel Filtering>.
--
-- = See Also
--
-- 'Vulkan.Core10.Sampler.SamplerCreateInfo',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo',
-- 'Vulkan.Core10.CommandBufferBuilding.cmdBlitImage'
newtype Filter = Filter Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'FILTER_NEAREST' specifies nearest filtering.
pattern FILTER_NEAREST = Filter 0
-- | 'FILTER_LINEAR' specifies linear filtering.
pattern FILTER_LINEAR = Filter 1
-- No documentation found for Nested "VkFilter" "VK_FILTER_CUBIC_IMG"
pattern FILTER_CUBIC_IMG = Filter 1000015000
{-# complete FILTER_NEAREST,
             FILTER_LINEAR,
             FILTER_CUBIC_IMG :: Filter #-}

instance Show Filter where
  showsPrec p = \case
    FILTER_NEAREST -> showString "FILTER_NEAREST"
    FILTER_LINEAR -> showString "FILTER_LINEAR"
    FILTER_CUBIC_IMG -> showString "FILTER_CUBIC_IMG"
    Filter x -> showParen (p >= 11) (showString "Filter " . showsPrec 11 x)

instance Read Filter where
  readPrec = parens (choose [("FILTER_NEAREST", pure FILTER_NEAREST)
                            , ("FILTER_LINEAR", pure FILTER_LINEAR)
                            , ("FILTER_CUBIC_IMG", pure FILTER_CUBIC_IMG)]
                     +++
                     prec 10 (do
                       expectP (Ident "Filter")
                       v <- step readPrec
                       pure (Filter v)))

