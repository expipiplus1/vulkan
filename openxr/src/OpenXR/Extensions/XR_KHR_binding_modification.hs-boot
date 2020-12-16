{-# language CPP #-}
-- | = Name
--
-- XR_KHR_binding_modification - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_binding_modification  XR_KHR_binding_modification>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 121
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
-- 'BindingModificationBaseHeaderKHR', 'BindingModificationsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_binding_modification OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_binding_modification  ( BindingModificationBaseHeaderKHR
                                                      , BindingModificationsKHR
                                                      ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data BindingModificationBaseHeaderKHR

instance ToCStruct BindingModificationBaseHeaderKHR
instance Show BindingModificationBaseHeaderKHR

instance FromCStruct BindingModificationBaseHeaderKHR


data BindingModificationsKHR

instance ToCStruct BindingModificationsKHR
instance Show BindingModificationsKHR

instance FromCStruct BindingModificationsKHR

