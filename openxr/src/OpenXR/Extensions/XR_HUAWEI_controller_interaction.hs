{-# language CPP #-}
-- | = Name
--
-- XR_HUAWEI_controller_interaction - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_HUAWEI_controller_interaction  XR_HUAWEI_controller_interaction>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 70
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
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_HUAWEI_controller_interaction OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_HUAWEI_controller_interaction  ( HUAWEI_controller_interaction_SPEC_VERSION
                                                           , pattern HUAWEI_controller_interaction_SPEC_VERSION
                                                           , HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME
                                                           , pattern HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME
                                                           ) where

import Data.String (IsString)

type HUAWEI_controller_interaction_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_HUAWEI_controller_interaction_SPEC_VERSION"
pattern HUAWEI_controller_interaction_SPEC_VERSION :: forall a . Integral a => a
pattern HUAWEI_controller_interaction_SPEC_VERSION = 1


type HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME = "XR_HUAWEI_controller_interaction"

-- No documentation found for TopLevel "XR_HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME"
pattern HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern HUAWEI_CONTROLLER_INTERACTION_EXTENSION_NAME = "XR_HUAWEI_controller_interaction"

