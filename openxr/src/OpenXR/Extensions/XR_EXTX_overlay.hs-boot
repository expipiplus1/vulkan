{-# language CPP #-}
-- | = Name
--
-- XR_EXTX_overlay - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXTX_overlay  XR_EXTX_overlay>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 34
--
-- = Revision
--
-- 4
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'EventDataMainSessionVisibilityChangedEXTX',
-- 'SessionCreateInfoOverlayEXTX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXTX_overlay OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXTX_overlay  ( EventDataMainSessionVisibilityChangedEXTX
                                          , SessionCreateInfoOverlayEXTX
                                          ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data EventDataMainSessionVisibilityChangedEXTX

instance ToCStruct EventDataMainSessionVisibilityChangedEXTX
instance Show EventDataMainSessionVisibilityChangedEXTX

instance FromCStruct EventDataMainSessionVisibilityChangedEXTX


data SessionCreateInfoOverlayEXTX

instance ToCStruct SessionCreateInfoOverlayEXTX
instance Show SessionCreateInfoOverlayEXTX

instance FromCStruct SessionCreateInfoOverlayEXTX

