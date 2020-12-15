{-# language CPP #-}
-- | = Name
--
-- XR_KHR_loader_init_android - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init_android  XR_KHR_loader_init_android>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 90
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @@
--
-- = See Also
--
-- 'LoaderInitInfoAndroidKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init_android OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_loader_init_android  (LoaderInitInfoAndroidKHR) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data LoaderInitInfoAndroidKHR

instance ToCStruct LoaderInitInfoAndroidKHR
instance Show LoaderInitInfoAndroidKHR

instance FromCStruct LoaderInitInfoAndroidKHR

