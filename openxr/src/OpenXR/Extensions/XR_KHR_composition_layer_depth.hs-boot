{-# language CPP #-}
-- | = Name
--
-- XR_KHR_composition_layer_depth - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_depth  XR_KHR_composition_layer_depth>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 11
--
-- = Revision
--
-- 5
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'CompositionLayerDepthInfoKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_composition_layer_depth OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_composition_layer_depth  (CompositionLayerDepthInfoKHR) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data CompositionLayerDepthInfoKHR

instance ToCStruct CompositionLayerDepthInfoKHR
instance Show CompositionLayerDepthInfoKHR

instance FromCStruct CompositionLayerDepthInfoKHR

