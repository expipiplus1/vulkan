{-# language CPP #-}
-- No documentation found for Chapter "EnvironmentBlendMode"
module OpenXR.Core10.Enums.EnvironmentBlendMode  (EnvironmentBlendMode( ENVIRONMENT_BLEND_MODE_OPAQUE
                                                                      , ENVIRONMENT_BLEND_MODE_ADDITIVE
                                                                      , ENVIRONMENT_BLEND_MODE_ALPHA_BLEND
                                                                      , ..
                                                                      )) where

import OpenXR.Internal.Utils (enumReadPrec)
import OpenXR.Internal.Utils (enumShowsPrec)
import GHC.Show (showsPrec)
import Foreign.Storable (Storable)
import Data.Int (Int32)
import GHC.Read (Read(readPrec))
import GHC.Show (Show(showsPrec))
import OpenXR.Zero (Zero)
-- | XrEnvironmentBlendMode - Environment blend modes
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Core10.DisplayTiming.FrameEndInfo',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationLayerInfoMSFT',
-- 'OpenXR.Core10.Device.enumerateEnvironmentBlendModes'
newtype EnvironmentBlendMode = EnvironmentBlendMode Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'ENVIRONMENT_BLEND_MODE_OPAQUE'. The composition layers will be
-- displayed with no view of the physical world behind them. The composited
-- image will be interpreted as an RGB image, ignoring the composited alpha
-- channel. This is the typical mode for VR experiences, although this mode
-- can also be supported on devices that support video passthrough.
pattern ENVIRONMENT_BLEND_MODE_OPAQUE      = EnvironmentBlendMode 1
-- | 'ENVIRONMENT_BLEND_MODE_ADDITIVE'. The composition layers will be
-- additively blended with the real world behind the display. The
-- composited image will be interpreted as an RGB image, ignoring the
-- composited alpha channel during the additive blending. This will cause
-- black composited pixels to appear transparent. This is the typical mode
-- for an AR experience on a see-through headset with an additive display,
-- although this mode can also be supported on devices that support video
-- passthrough.
pattern ENVIRONMENT_BLEND_MODE_ADDITIVE    = EnvironmentBlendMode 2
-- | 'ENVIRONMENT_BLEND_MODE_ALPHA_BLEND'. The composition layers will be
-- alpha-blended with the real world behind the display. The composited
-- image will be interpreted as an RGBA image, with the composited alpha
-- channel determining each pixel’s level of blending with the real world
-- behind the display. This is the typical mode for an AR experience on a
-- phone or headset that supports video passthrough.
pattern ENVIRONMENT_BLEND_MODE_ALPHA_BLEND = EnvironmentBlendMode 3
{-# complete ENVIRONMENT_BLEND_MODE_OPAQUE,
             ENVIRONMENT_BLEND_MODE_ADDITIVE,
             ENVIRONMENT_BLEND_MODE_ALPHA_BLEND :: EnvironmentBlendMode #-}

conNameEnvironmentBlendMode :: String
conNameEnvironmentBlendMode = "EnvironmentBlendMode"

enumPrefixEnvironmentBlendMode :: String
enumPrefixEnvironmentBlendMode = "ENVIRONMENT_BLEND_MODE_"

showTableEnvironmentBlendMode :: [(EnvironmentBlendMode, String)]
showTableEnvironmentBlendMode =
  [ (ENVIRONMENT_BLEND_MODE_OPAQUE     , "OPAQUE")
  , (ENVIRONMENT_BLEND_MODE_ADDITIVE   , "ADDITIVE")
  , (ENVIRONMENT_BLEND_MODE_ALPHA_BLEND, "ALPHA_BLEND")
  ]

instance Show EnvironmentBlendMode where
  showsPrec = enumShowsPrec enumPrefixEnvironmentBlendMode
                            showTableEnvironmentBlendMode
                            conNameEnvironmentBlendMode
                            (\(EnvironmentBlendMode x) -> x)
                            (showsPrec 11)

instance Read EnvironmentBlendMode where
  readPrec = enumReadPrec enumPrefixEnvironmentBlendMode
                          showTableEnvironmentBlendMode
                          conNameEnvironmentBlendMode
                          EnvironmentBlendMode

