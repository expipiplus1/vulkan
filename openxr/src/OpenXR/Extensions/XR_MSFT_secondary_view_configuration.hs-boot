{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_secondary_view_configuration - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_secondary_view_configuration  XR_MSFT_secondary_view_configuration>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 54
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
-- 'SecondaryViewConfigurationFrameEndInfoMSFT',
-- 'SecondaryViewConfigurationFrameStateMSFT',
-- 'SecondaryViewConfigurationLayerInfoMSFT',
-- 'SecondaryViewConfigurationSessionBeginInfoMSFT',
-- 'SecondaryViewConfigurationStateMSFT',
-- 'SecondaryViewConfigurationSwapchainCreateInfoMSFT'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_secondary_view_configuration OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_secondary_view_configuration  ( SecondaryViewConfigurationFrameEndInfoMSFT
                                                               , SecondaryViewConfigurationFrameStateMSFT
                                                               , SecondaryViewConfigurationLayerInfoMSFT
                                                               , SecondaryViewConfigurationSessionBeginInfoMSFT
                                                               , SecondaryViewConfigurationStateMSFT
                                                               , SecondaryViewConfigurationSwapchainCreateInfoMSFT
                                                               ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data SecondaryViewConfigurationFrameEndInfoMSFT

instance ToCStruct SecondaryViewConfigurationFrameEndInfoMSFT
instance Show SecondaryViewConfigurationFrameEndInfoMSFT

instance FromCStruct SecondaryViewConfigurationFrameEndInfoMSFT


data SecondaryViewConfigurationFrameStateMSFT

instance ToCStruct SecondaryViewConfigurationFrameStateMSFT
instance Show SecondaryViewConfigurationFrameStateMSFT

instance FromCStruct SecondaryViewConfigurationFrameStateMSFT


data SecondaryViewConfigurationLayerInfoMSFT

instance ToCStruct SecondaryViewConfigurationLayerInfoMSFT
instance Show SecondaryViewConfigurationLayerInfoMSFT

instance FromCStruct SecondaryViewConfigurationLayerInfoMSFT


data SecondaryViewConfigurationSessionBeginInfoMSFT

instance ToCStruct SecondaryViewConfigurationSessionBeginInfoMSFT
instance Show SecondaryViewConfigurationSessionBeginInfoMSFT

instance FromCStruct SecondaryViewConfigurationSessionBeginInfoMSFT


data SecondaryViewConfigurationStateMSFT

instance ToCStruct SecondaryViewConfigurationStateMSFT
instance Show SecondaryViewConfigurationStateMSFT

instance FromCStruct SecondaryViewConfigurationStateMSFT


data SecondaryViewConfigurationSwapchainCreateInfoMSFT

instance ToCStruct SecondaryViewConfigurationSwapchainCreateInfoMSFT
instance Show SecondaryViewConfigurationSwapchainCreateInfoMSFT

instance FromCStruct SecondaryViewConfigurationSwapchainCreateInfoMSFT

