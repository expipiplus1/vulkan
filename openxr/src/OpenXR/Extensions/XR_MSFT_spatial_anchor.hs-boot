{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_spatial_anchor - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_anchor  XR_MSFT_spatial_anchor>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 40
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
-- 'createSpatialAnchorMSFT', 'createSpatialAnchorSpaceMSFT',
-- 'destroySpatialAnchorMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_spatial_anchor OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_spatial_anchor  ( SpatialAnchorCreateInfoMSFT
                                                 , SpatialAnchorSpaceCreateInfoMSFT
                                                 ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data SpatialAnchorCreateInfoMSFT

instance ToCStruct SpatialAnchorCreateInfoMSFT
instance Show SpatialAnchorCreateInfoMSFT

instance FromCStruct SpatialAnchorCreateInfoMSFT


data SpatialAnchorSpaceCreateInfoMSFT

instance ToCStruct SpatialAnchorSpaceCreateInfoMSFT
instance Show SpatialAnchorSpaceCreateInfoMSFT

instance FromCStruct SpatialAnchorSpaceCreateInfoMSFT

