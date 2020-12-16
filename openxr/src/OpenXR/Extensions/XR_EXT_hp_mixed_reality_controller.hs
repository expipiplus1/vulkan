{-# language CPP #-}
-- | = Name
--
-- XR_EXT_hp_mixed_reality_controller - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_hp_mixed_reality_controller  XR_EXT_hp_mixed_reality_controller>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 96
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
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_hp_mixed_reality_controller OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_hp_mixed_reality_controller  ( EXT_hp_mixed_reality_controller_SPEC_VERSION
                                                             , pattern EXT_hp_mixed_reality_controller_SPEC_VERSION
                                                             , EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME
                                                             , pattern EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME
                                                             ) where

import Data.String (IsString)

type EXT_hp_mixed_reality_controller_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_EXT_hp_mixed_reality_controller_SPEC_VERSION"
pattern EXT_hp_mixed_reality_controller_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_hp_mixed_reality_controller_SPEC_VERSION = 1


type EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME = "XR_EXT_hp_mixed_reality_controller"

-- No documentation found for TopLevel "XR_EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME"
pattern EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_HP_MIXED_REALITY_CONTROLLER_EXTENSION_NAME = "XR_EXT_hp_mixed_reality_controller"

