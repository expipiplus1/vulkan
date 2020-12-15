{-# language CPP #-}
-- | = Name
--
-- XR_KHR_loader_init - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init  XR_KHR_loader_init>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 89
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
-- 'LoaderInitInfoBaseHeaderKHR', 'initializeLoaderKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_loader_init OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_loader_init  (LoaderInitInfoBaseHeaderKHR) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data LoaderInitInfoBaseHeaderKHR

instance ToCStruct LoaderInitInfoBaseHeaderKHR
instance Show LoaderInitInfoBaseHeaderKHR

instance FromCStruct LoaderInitInfoBaseHeaderKHR

