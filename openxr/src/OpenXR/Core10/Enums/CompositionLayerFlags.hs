{-# language CPP #-}
-- No documentation found for Chapter "CompositionLayerFlags"
module OpenXR.Core10.Enums.CompositionLayerFlags  (CompositionLayerFlags(..)) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showString)
import Numeric (showHex)
import OpenXR.Zero (Zero)
import Data.Bits (Bits)
import Data.Bits (FiniteBits)
import Foreign.Storable (Storable)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Core10.FundamentalTypes (Flags64)
-- | XrCompositionLayerFlags - Composition layer flags
--
-- = Description
--
-- 'CompositionLayerFlags' specify options for individual composition
-- layers.
--
-- == Flag Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.OtherTypes.CompositionLayerBaseHeader',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerProjection',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerQuad'
newtype CompositionLayerFlags = CompositionLayerFlags Flags64
  deriving newtype (Eq, Ord, Storable, Zero, Bits, FiniteBits)



conNameCompositionLayerFlags :: String
conNameCompositionLayerFlags = "CompositionLayerFlags"

enumPrefixCompositionLayerFlags :: String
enumPrefixCompositionLayerFlags = ""

showTableCompositionLayerFlags :: [(CompositionLayerFlags, String)]
showTableCompositionLayerFlags = []

instance Show CompositionLayerFlags where
  showsPrec = enumShowsPrec enumPrefixCompositionLayerFlags
                            showTableCompositionLayerFlags
                            conNameCompositionLayerFlags
                            (\(CompositionLayerFlags x) -> x)
                            (\x -> showString "0x" . showHex x)

instance Read CompositionLayerFlags where
  readPrec = enumReadPrec enumPrefixCompositionLayerFlags
                          showTableCompositionLayerFlags
                          conNameCompositionLayerFlags
                          CompositionLayerFlags

