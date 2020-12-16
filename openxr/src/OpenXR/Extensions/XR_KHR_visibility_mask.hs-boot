{-# language CPP #-}
-- | = Name
--
-- XR_KHR_visibility_mask - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_visibility_mask  XR_KHR_visibility_mask>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 32
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
-- 'EventDataVisibilityMaskChangedKHR', 'VisibilityMaskKHR',
-- 'VisibilityMaskTypeKHR', 'getVisibilityMaskKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_visibility_mask OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_visibility_mask  ( EventDataVisibilityMaskChangedKHR
                                                 , VisibilityMaskKHR
                                                 , VisibilityMaskTypeKHR
                                                 ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data EventDataVisibilityMaskChangedKHR

instance ToCStruct EventDataVisibilityMaskChangedKHR
instance Show EventDataVisibilityMaskChangedKHR

instance FromCStruct EventDataVisibilityMaskChangedKHR


data VisibilityMaskKHR

instance ToCStruct VisibilityMaskKHR
instance Show VisibilityMaskKHR

instance FromCStruct VisibilityMaskKHR


data VisibilityMaskTypeKHR

