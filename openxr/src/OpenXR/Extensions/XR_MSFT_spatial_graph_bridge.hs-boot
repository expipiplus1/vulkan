{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_spatial_graph_bridge - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_graph_bridge  XR_MSFT_spatial_graph_bridge>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 50
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
-- 'SpatialGraphNodeSpaceCreateInfoMSFT', 'SpatialGraphNodeTypeMSFT',
-- 'createSpatialGraphNodeSpaceMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_graph_bridge OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_spatial_graph_bridge  (SpatialGraphNodeSpaceCreateInfoMSFT) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data SpatialGraphNodeSpaceCreateInfoMSFT

instance ToCStruct SpatialGraphNodeSpaceCreateInfoMSFT
instance Show SpatialGraphNodeSpaceCreateInfoMSFT

instance FromCStruct SpatialGraphNodeSpaceCreateInfoMSFT

