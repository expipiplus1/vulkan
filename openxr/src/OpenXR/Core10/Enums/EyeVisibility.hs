{-# language CPP #-}
-- No documentation found for Chapter "EyeVisibility"
module OpenXR.Core10.Enums.EyeVisibility  (EyeVisibility( EYE_VISIBILITY_BOTH
                                                        , EYE_VISIBILITY_LEFT
                                                        , EYE_VISIBILITY_RIGHT
                                                        , ..
                                                        )) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import OpenXR.Zero (Zero)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))

-- | XrEyeVisibility - Eye visibility selector
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cube.CompositionLayerCubeKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_cylinder.CompositionLayerCylinderKHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect2.CompositionLayerEquirect2KHR',
-- 'OpenXR.Extensions.XR_KHR_composition_layer_equirect.CompositionLayerEquirectKHR',
-- 'OpenXR.Core10.OtherTypes.CompositionLayerQuad'
newtype EyeVisibility = EyeVisibility Int32
  deriving newtype (Eq, Ord, Storable, Zero)

-- | 'EYE_VISIBILITY_BOTH' displays the layer to both eyes.
pattern EYE_VISIBILITY_BOTH = EyeVisibility 0

-- | 'EYE_VISIBILITY_LEFT' displays the layer to the viewer’s physical left
-- eye.
pattern EYE_VISIBILITY_LEFT = EyeVisibility 1

-- | 'EYE_VISIBILITY_RIGHT' displays the layer to the viewer’s physical right
-- eye.
pattern EYE_VISIBILITY_RIGHT = EyeVisibility 2

{-# COMPLETE
  EYE_VISIBILITY_BOTH
  , EYE_VISIBILITY_LEFT
  , EYE_VISIBILITY_RIGHT ::
    EyeVisibility
  #-}

conNameEyeVisibility :: String
conNameEyeVisibility = "EyeVisibility"

enumPrefixEyeVisibility :: String
enumPrefixEyeVisibility = "EYE_VISIBILITY_"

showTableEyeVisibility :: [(EyeVisibility, String)]
showTableEyeVisibility =
  [ (EYE_VISIBILITY_BOTH, "BOTH")
  , (EYE_VISIBILITY_LEFT, "LEFT")
  , (EYE_VISIBILITY_RIGHT, "RIGHT")
  ]

instance Show EyeVisibility where
  showsPrec =
    enumShowsPrec
      enumPrefixEyeVisibility
      showTableEyeVisibility
      conNameEyeVisibility
      (\(EyeVisibility x) -> x)
      (showsPrec 11)

instance Read EyeVisibility where
  readPrec =
    enumReadPrec
      enumPrefixEyeVisibility
      showTableEyeVisibility
      conNameEyeVisibility
      EyeVisibility
