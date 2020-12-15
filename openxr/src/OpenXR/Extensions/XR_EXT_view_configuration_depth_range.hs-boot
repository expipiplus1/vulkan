{-# language CPP #-}
-- | = Name
--
-- XR_EXT_view_configuration_depth_range - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_view_configuration_depth_range  XR_EXT_view_configuration_depth_range>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 47
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
-- 'ViewConfigurationDepthRangeEXT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_view_configuration_depth_range OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_view_configuration_depth_range  (ViewConfigurationDepthRangeEXT) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data ViewConfigurationDepthRangeEXT

instance ToCStruct ViewConfigurationDepthRangeEXT
instance Show ViewConfigurationDepthRangeEXT

instance FromCStruct ViewConfigurationDepthRangeEXT

