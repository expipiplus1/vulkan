{-# language CPP #-}
-- | = Name
--
-- XR_EXT_eye_gaze_interaction - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_eye_gaze_interaction  XR_EXT_eye_gaze_interaction>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 31
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'EyeGazeSampleTimeEXT', 'SystemEyeGazeInteractionPropertiesEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_eye_gaze_interaction OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_eye_gaze_interaction  ( EyeGazeSampleTimeEXT
                                                      , SystemEyeGazeInteractionPropertiesEXT
                                                      ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data EyeGazeSampleTimeEXT

instance ToCStruct EyeGazeSampleTimeEXT
instance Show EyeGazeSampleTimeEXT

instance FromCStruct EyeGazeSampleTimeEXT


data SystemEyeGazeInteractionPropertiesEXT

instance ToCStruct SystemEyeGazeInteractionPropertiesEXT
instance Show SystemEyeGazeInteractionPropertiesEXT

instance FromCStruct SystemEyeGazeInteractionPropertiesEXT

