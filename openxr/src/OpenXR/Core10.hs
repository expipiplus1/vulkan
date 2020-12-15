{-# language CPP #-}
-- No documentation found for Chapter "Core10"
module OpenXR.Core10  ( pattern API_VERSION_1_0
                      , module OpenXR.Core10.APIConstants
                      , module OpenXR.Core10.Device
                      , module OpenXR.Core10.DisplayTiming
                      , module OpenXR.Core10.Enums
                      , module OpenXR.Core10.FuncPointers
                      , module OpenXR.Core10.FundamentalTypes
                      , module OpenXR.Core10.Handles
                      , module OpenXR.Core10.Haptics
                      , module OpenXR.Core10.Image
                      , module OpenXR.Core10.Input
                      , module OpenXR.Core10.Instance
                      , module OpenXR.Core10.OtherTypes
                      , module OpenXR.Core10.SemanticPaths
                      , module OpenXR.Core10.Session
                      , module OpenXR.Core10.Space
                      , module OpenXR.Core10.ViewConfigurations
                      ) where
import OpenXR.Core10.APIConstants
import OpenXR.Core10.Device
import OpenXR.Core10.DisplayTiming
import OpenXR.Core10.Enums
import OpenXR.Core10.FuncPointers
import OpenXR.Core10.FundamentalTypes
import OpenXR.Core10.Handles
import OpenXR.Core10.Haptics
import OpenXR.Core10.Image
import OpenXR.Core10.Input
import OpenXR.Core10.Instance
import OpenXR.Core10.OtherTypes
import OpenXR.Core10.SemanticPaths
import OpenXR.Core10.Session
import OpenXR.Core10.Space
import OpenXR.Core10.ViewConfigurations
import Data.Word (Word32)
import OpenXR.Version (Version)
import OpenXR.Version (pattern MAKE_VERSION)
pattern API_VERSION_1_0 :: Version
pattern API_VERSION_1_0 = MAKE_VERSION 1 0 0

