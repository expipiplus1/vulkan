{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_hand_tracking_mesh - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_hand_tracking_mesh  XR_MSFT_hand_tracking_mesh>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 53
--
-- = Revision
--
-- 2
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @@
--
-- = See Also
--
-- 'HandMeshIndexBufferMSFT', 'HandMeshMSFT',
-- 'HandMeshSpaceCreateInfoMSFT', 'HandMeshUpdateInfoMSFT',
-- 'HandMeshVertexBufferMSFT', 'HandMeshVertexMSFT',
-- 'HandPoseTypeInfoMSFT', 'HandPoseTypeMSFT',
-- 'SystemHandTrackingMeshPropertiesMSFT', 'createHandMeshSpaceMSFT',
-- 'updateHandMeshMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_hand_tracking_mesh OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_hand_tracking_mesh  ( HandMeshIndexBufferMSFT
                                                     , HandMeshMSFT
                                                     , HandMeshSpaceCreateInfoMSFT
                                                     , HandMeshUpdateInfoMSFT
                                                     , HandMeshVertexBufferMSFT
                                                     , HandMeshVertexMSFT
                                                     , HandPoseTypeInfoMSFT
                                                     , SystemHandTrackingMeshPropertiesMSFT
                                                     ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data HandMeshIndexBufferMSFT

instance ToCStruct HandMeshIndexBufferMSFT
instance Show HandMeshIndexBufferMSFT

instance FromCStruct HandMeshIndexBufferMSFT


data HandMeshMSFT

instance ToCStruct HandMeshMSFT
instance Show HandMeshMSFT

instance FromCStruct HandMeshMSFT


data HandMeshSpaceCreateInfoMSFT

instance ToCStruct HandMeshSpaceCreateInfoMSFT
instance Show HandMeshSpaceCreateInfoMSFT

instance FromCStruct HandMeshSpaceCreateInfoMSFT


data HandMeshUpdateInfoMSFT

instance ToCStruct HandMeshUpdateInfoMSFT
instance Show HandMeshUpdateInfoMSFT

instance FromCStruct HandMeshUpdateInfoMSFT


data HandMeshVertexBufferMSFT

instance ToCStruct HandMeshVertexBufferMSFT
instance Show HandMeshVertexBufferMSFT

instance FromCStruct HandMeshVertexBufferMSFT


data HandMeshVertexMSFT

instance ToCStruct HandMeshVertexMSFT
instance Show HandMeshVertexMSFT

instance FromCStruct HandMeshVertexMSFT


data HandPoseTypeInfoMSFT

instance ToCStruct HandPoseTypeInfoMSFT
instance Show HandPoseTypeInfoMSFT

instance FromCStruct HandPoseTypeInfoMSFT


data SystemHandTrackingMeshPropertiesMSFT

instance ToCStruct SystemHandTrackingMeshPropertiesMSFT
instance Show SystemHandTrackingMeshPropertiesMSFT

instance FromCStruct SystemHandTrackingMeshPropertiesMSFT

