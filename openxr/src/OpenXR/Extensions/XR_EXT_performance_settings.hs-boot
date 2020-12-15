{-# language CPP #-}
-- | = Name
--
-- XR_EXT_performance_settings - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_performance_settings  XR_EXT_performance_settings>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 16
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
-- 'EventDataPerfSettingsEXT', 'PerfSettingsDomainEXT',
-- 'PerfSettingsLevelEXT', 'PerfSettingsNotificationLevelEXT',
-- 'PerfSettingsSubDomainEXT', 'perfSettingsSetPerformanceLevelEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_performance_settings OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_performance_settings  ( EventDataPerfSettingsEXT
                                                      , PerfSettingsDomainEXT
                                                      , PerfSettingsLevelEXT
                                                      , PerfSettingsNotificationLevelEXT
                                                      ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data EventDataPerfSettingsEXT

instance ToCStruct EventDataPerfSettingsEXT
instance Show EventDataPerfSettingsEXT

instance FromCStruct EventDataPerfSettingsEXT


data PerfSettingsDomainEXT


data PerfSettingsLevelEXT


data PerfSettingsNotificationLevelEXT

