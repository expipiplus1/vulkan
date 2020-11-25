{-# language CPP #-}
-- No documentation found for Chapter "ChromaLocation"
module Vulkan.Core11.Enums.ChromaLocation  (ChromaLocation( CHROMA_LOCATION_COSITED_EVEN
                                                          , CHROMA_LOCATION_MIDPOINT
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
-- | VkChromaLocation - Position of downsampled chroma samples
--
-- = See Also
--
-- 'Vulkan.Extensions.VK_ANDROID_external_memory_android_hardware_buffer.AndroidHardwareBufferFormatPropertiesANDROID',
-- 'Vulkan.Core11.Promoted_From_VK_KHR_sampler_ycbcr_conversion.SamplerYcbcrConversionCreateInfo'
newtype ChromaLocation = ChromaLocation Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'CHROMA_LOCATION_COSITED_EVEN' specifies that downsampled chroma samples
-- are aligned with luma samples with even coordinates.
pattern CHROMA_LOCATION_COSITED_EVEN = ChromaLocation 0
-- | 'CHROMA_LOCATION_MIDPOINT' specifies that downsampled chroma samples are
-- located half way between each even luma sample and the nearest higher
-- odd luma sample.
pattern CHROMA_LOCATION_MIDPOINT = ChromaLocation 1
{-# complete CHROMA_LOCATION_COSITED_EVEN,
             CHROMA_LOCATION_MIDPOINT :: ChromaLocation #-}

instance Show ChromaLocation where
  showsPrec p = \case
    CHROMA_LOCATION_COSITED_EVEN -> showString "CHROMA_LOCATION_COSITED_EVEN"
    CHROMA_LOCATION_MIDPOINT -> showString "CHROMA_LOCATION_MIDPOINT"
    ChromaLocation x -> showParen (p >= 11) (showString "ChromaLocation " . showsPrec 11 x)

instance Read ChromaLocation where
  readPrec = parens (choose [("CHROMA_LOCATION_COSITED_EVEN", pure CHROMA_LOCATION_COSITED_EVEN)
                            , ("CHROMA_LOCATION_MIDPOINT", pure CHROMA_LOCATION_MIDPOINT)]
                     +++
                     prec 10 (do
                       expectP (Ident "ChromaLocation")
                       v <- step readPrec
                       pure (ChromaLocation v)))

