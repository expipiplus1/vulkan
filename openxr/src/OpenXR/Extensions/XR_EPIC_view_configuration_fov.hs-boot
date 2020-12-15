{-# language CPP #-}
-- | = Name
--
-- XR_EPIC_view_configuration_fov - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EPIC_view_configuration_fov  XR_EPIC_view_configuration_fov>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 60
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'ViewConfigurationViewFovEPIC'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EPIC_view_configuration_fov OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EPIC_view_configuration_fov  (ViewConfigurationViewFovEPIC) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data ViewConfigurationViewFovEPIC

instance ToCStruct ViewConfigurationViewFovEPIC
instance Show ViewConfigurationViewFovEPIC

instance FromCStruct ViewConfigurationViewFovEPIC

