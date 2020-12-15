{-# language CPP #-}
-- | = Name
--
-- XR_VALVE_analog_threshold - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_VALVE_analog_threshold  XR_VALVE_analog_threshold>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 80
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
-- 'InteractionProfileAnalogThresholdVALVE'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_VALVE_analog_threshold OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_VALVE_analog_threshold  (InteractionProfileAnalogThresholdVALVE) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data InteractionProfileAnalogThresholdVALVE

instance ToCStruct InteractionProfileAnalogThresholdVALVE
instance Show InteractionProfileAnalogThresholdVALVE

instance FromCStruct InteractionProfileAnalogThresholdVALVE

