{-# language CPP #-}
-- | = Name
--
-- XR_FB_display_refresh_rate - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_display_refresh_rate  XR_FB_display_refresh_rate>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 102
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
-- 'EventDataDisplayRefreshRateChangedFB',
-- 'enumerateDisplayRefreshRatesFB', 'getDisplayRefreshRateFB',
-- 'requestDisplayRefreshRateFB'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_FB_display_refresh_rate OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_FB_display_refresh_rate  (EventDataDisplayRefreshRateChangedFB) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data EventDataDisplayRefreshRateChangedFB

instance ToCStruct EventDataDisplayRefreshRateChangedFB
instance Show EventDataDisplayRefreshRateChangedFB

instance FromCStruct EventDataDisplayRefreshRateChangedFB

