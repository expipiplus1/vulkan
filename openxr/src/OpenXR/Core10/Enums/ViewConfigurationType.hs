{-# language CPP #-}
-- No documentation found for Chapter "ViewConfigurationType"
module OpenXR.Core10.Enums.ViewConfigurationType  (ViewConfigurationType( VIEW_CONFIGURATION_TYPE_PRIMARY_MONO
                                                                        , VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO
                                                                        , VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT
                                                                        , VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO
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
-- | XrViewConfigurationType - Supported view configuration type
--
-- = Description
--
-- The application selects its primary view configuration type when calling
-- 'OpenXR.Core10.Session.beginSession', and that configuration remains
-- constant for the lifetime of the session, until
-- 'OpenXR.Core10.Session.endSession' is called.
--
-- The number of views and the semantic meaning of each view index within a
-- given view configuration is well-defined, specified below for all core
-- view configurations. The predefined primary view configuration types
-- are:
--
-- == Enumerant Descriptions
--
-- = See Also
--
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.EventDataVisibilityMaskChangedKHR',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationLayerInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationSessionBeginInfoMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationStateMSFT',
-- 'OpenXR.Extensions.XR_MSFT_secondary_view_configuration.SecondaryViewConfigurationSwapchainCreateInfoMSFT',
-- 'OpenXR.Core10.Session.SessionBeginInfo',
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationProperties',
-- 'OpenXR.Core10.DisplayTiming.ViewLocateInfo',
-- 'OpenXR.Core10.Device.enumerateEnvironmentBlendModes',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurationViews',
-- 'OpenXR.Core10.ViewConfigurations.enumerateViewConfigurations',
-- 'OpenXR.Core10.ViewConfigurations.getViewConfigurationProperties',
-- 'OpenXR.Extensions.XR_KHR_visibility_mask.getVisibilityMaskKHR'
newtype ViewConfigurationType = ViewConfigurationType Int32
  deriving newtype (Eq, Ord, Storable, Zero)
-- Note that the zero instance does not produce a valid value, passing 'zero' to Vulkan will result in an error

-- | 'VIEW_CONFIGURATION_TYPE_PRIMARY_MONO'. One view representing the form
-- factor’s one primary display. For example, an AR phone’s screen. This
-- configuration requires one element in
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationProperties' and one
-- projection in each 'OpenXR.Core10.OtherTypes.CompositionLayerProjection'
-- layer.
pattern VIEW_CONFIGURATION_TYPE_PRIMARY_MONO                              = ViewConfigurationType 1
-- | 'VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO'. Two views representing the
-- form factor’s two primary displays, which map to a left-eye and
-- right-eye view. This configuration requires two views in
-- 'OpenXR.Core10.ViewConfigurations.ViewConfigurationProperties' and two
-- views in each 'OpenXR.Core10.OtherTypes.CompositionLayerProjection'
-- layer. View index 0 /must/ represent the left eye and view index 1
-- /must/ represent the right eye.
pattern VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO                            = ViewConfigurationType 2
-- No documentation found for Nested "XrViewConfigurationType" "XR_VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT"
pattern VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT = ViewConfigurationType 1000054000
-- No documentation found for Nested "XrViewConfigurationType" "XR_VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO"
pattern VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO                        = ViewConfigurationType 1000037000
{-# complete VIEW_CONFIGURATION_TYPE_PRIMARY_MONO,
             VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO,
             VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT,
             VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO :: ViewConfigurationType #-}

conNameViewConfigurationType :: String
conNameViewConfigurationType = "ViewConfigurationType"

enumPrefixViewConfigurationType :: String
enumPrefixViewConfigurationType = "VIEW_CONFIGURATION_TYPE_"

showTableViewConfigurationType :: [(ViewConfigurationType, String)]
showTableViewConfigurationType =
  [ (VIEW_CONFIGURATION_TYPE_PRIMARY_MONO                             , "PRIMARY_MONO")
  , (VIEW_CONFIGURATION_TYPE_PRIMARY_STEREO                           , "PRIMARY_STEREO")
  , (VIEW_CONFIGURATION_TYPE_SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT, "SECONDARY_MONO_FIRST_PERSON_OBSERVER_MSFT")
  , (VIEW_CONFIGURATION_TYPE_PRIMARY_QUAD_VARJO                       , "PRIMARY_QUAD_VARJO")
  ]

instance Show ViewConfigurationType where
  showsPrec = enumShowsPrec enumPrefixViewConfigurationType
                            showTableViewConfigurationType
                            conNameViewConfigurationType
                            (\(ViewConfigurationType x) -> x)
                            (showsPrec 11)

instance Read ViewConfigurationType where
  readPrec = enumReadPrec enumPrefixViewConfigurationType
                          showTableViewConfigurationType
                          conNameViewConfigurationType
                          ViewConfigurationType

