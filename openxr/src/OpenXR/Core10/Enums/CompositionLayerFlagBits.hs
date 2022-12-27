{-# language CPP #-}
-- No documentation found for Chapter "CompositionLayerFlagBits"
module OpenXR.Core10.Enums.CompositionLayerFlagBits  ( CompositionLayerFlags
                                                     , CompositionLayerFlagBits( COMPOSITION_LAYER_CORRECT_CHROMATIC_ABERRATION_BIT
                                                                               , COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT
                                                                               , COMPOSITION_LAYER_UNPREMULTIPLIED_ALPHA_BIT
                                                                               , ..
                                                                               )
                                                     ) where

import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
type CompositionLayerFlags = CompositionLayerFlagBits

-- No documentation found for TopLevel "XrCompositionLayerFlagBits"
newtype CompositionLayerFlagBits = CompositionLayerFlagBits Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)

-- No documentation found for Nested "XrCompositionLayerFlagBits" "XR_COMPOSITION_LAYER_CORRECT_CHROMATIC_ABERRATION_BIT"
pattern COMPOSITION_LAYER_CORRECT_CHROMATIC_ABERRATION_BIT = CompositionLayerFlagBits 0x0000000000000001

-- No documentation found for Nested "XrCompositionLayerFlagBits" "XR_COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT"
pattern COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT = CompositionLayerFlagBits 0x0000000000000002

-- No documentation found for Nested "XrCompositionLayerFlagBits" "XR_COMPOSITION_LAYER_UNPREMULTIPLIED_ALPHA_BIT"
pattern COMPOSITION_LAYER_UNPREMULTIPLIED_ALPHA_BIT = CompositionLayerFlagBits 0x0000000000000004

conNameCompositionLayerFlagBits :: String
conNameCompositionLayerFlagBits = "CompositionLayerFlagBits"

enumPrefixCompositionLayerFlagBits :: String
enumPrefixCompositionLayerFlagBits = "COMPOSITION_LAYER_"

showTableCompositionLayerFlagBits :: [(CompositionLayerFlagBits, String)]
showTableCompositionLayerFlagBits =
  [
    ( COMPOSITION_LAYER_CORRECT_CHROMATIC_ABERRATION_BIT
    , "CORRECT_CHROMATIC_ABERRATION_BIT"
    )
  ,
    ( COMPOSITION_LAYER_BLEND_TEXTURE_SOURCE_ALPHA_BIT
    , "BLEND_TEXTURE_SOURCE_ALPHA_BIT"
    )
  ,
    ( COMPOSITION_LAYER_UNPREMULTIPLIED_ALPHA_BIT
    , "UNPREMULTIPLIED_ALPHA_BIT"
    )
  ]

instance Show CompositionLayerFlagBits where
  showsPrec =
    enumShowsPrec
      enumPrefixCompositionLayerFlagBits
      showTableCompositionLayerFlagBits
      conNameCompositionLayerFlagBits
      (\(CompositionLayerFlagBits x) -> x)
      (\x -> showString "0x" . showHex x)

instance Read CompositionLayerFlagBits where
  readPrec =
    enumReadPrec
      enumPrefixCompositionLayerFlagBits
      showTableCompositionLayerFlagBits
      conNameCompositionLayerFlagBits
      CompositionLayerFlagBits
