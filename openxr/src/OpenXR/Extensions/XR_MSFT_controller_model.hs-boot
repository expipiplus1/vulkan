{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_controller_model - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_controller_model  XR_MSFT_controller_model>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 56
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
-- 'MAX_CONTROLLER_MODEL_NODE_NAME_SIZE_MSFT',
-- 'OpenXR.Core10.APIConstants.NULL_CONTROLLER_MODEL_KEY_MSFT',
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XrControllerModelKeyMSFT >,
-- 'ControllerModelKeyStateMSFT', 'ControllerModelNodePropertiesMSFT',
-- 'ControllerModelNodeStateMSFT', 'ControllerModelPropertiesMSFT',
-- 'ControllerModelStateMSFT', 'getControllerModelKeyMSFT',
-- 'getControllerModelPropertiesMSFT', 'getControllerModelStateMSFT',
-- 'loadControllerModelMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_controller_model OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_controller_model  ( ControllerModelKeyStateMSFT
                                                   , ControllerModelNodePropertiesMSFT
                                                   , ControllerModelNodeStateMSFT
                                                   , ControllerModelPropertiesMSFT
                                                   , ControllerModelStateMSFT
                                                   , ControllerModelKeyMSFT
                                                   ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data ControllerModelKeyStateMSFT

instance ToCStruct ControllerModelKeyStateMSFT
instance Show ControllerModelKeyStateMSFT

instance FromCStruct ControllerModelKeyStateMSFT


data ControllerModelNodePropertiesMSFT

instance ToCStruct ControllerModelNodePropertiesMSFT
instance Show ControllerModelNodePropertiesMSFT

instance FromCStruct ControllerModelNodePropertiesMSFT


data ControllerModelNodeStateMSFT

instance ToCStruct ControllerModelNodeStateMSFT
instance Show ControllerModelNodeStateMSFT

instance FromCStruct ControllerModelNodeStateMSFT


data ControllerModelPropertiesMSFT

instance ToCStruct ControllerModelPropertiesMSFT
instance Show ControllerModelPropertiesMSFT

instance FromCStruct ControllerModelPropertiesMSFT


data ControllerModelStateMSFT

instance ToCStruct ControllerModelStateMSFT
instance Show ControllerModelStateMSFT

instance FromCStruct ControllerModelStateMSFT


data ControllerModelKeyMSFT

