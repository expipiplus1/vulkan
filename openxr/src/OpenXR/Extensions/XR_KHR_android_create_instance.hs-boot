{-# language CPP #-}
-- | = Name
--
-- XR_KHR_android_create_instance - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_create_instance  XR_KHR_android_create_instance>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 9
--
-- = Revision
--
-- 3
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'InstanceCreateInfoAndroidKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_android_create_instance OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_android_create_instance  (InstanceCreateInfoAndroidKHR) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data InstanceCreateInfoAndroidKHR

instance ToCStruct InstanceCreateInfoAndroidKHR
instance Show InstanceCreateInfoAndroidKHR

instance FromCStruct InstanceCreateInfoAndroidKHR

