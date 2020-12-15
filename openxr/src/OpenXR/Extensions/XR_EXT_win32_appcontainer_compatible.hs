{-# language CPP #-}
-- | = Name
--
-- XR_EXT_win32_appcontainer_compatible - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_win32_appcontainer_compatible  XR_EXT_win32_appcontainer_compatible>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 58
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
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_EXT_win32_appcontainer_compatible OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_EXT_win32_appcontainer_compatible  ( EXT_win32_appcontainer_compatible_SPEC_VERSION
                                                               , pattern EXT_win32_appcontainer_compatible_SPEC_VERSION
                                                               , EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME
                                                               , pattern EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME
                                                               ) where

import Data.String (IsString)

type EXT_win32_appcontainer_compatible_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_EXT_win32_appcontainer_compatible_SPEC_VERSION"
pattern EXT_win32_appcontainer_compatible_SPEC_VERSION :: forall a . Integral a => a
pattern EXT_win32_appcontainer_compatible_SPEC_VERSION = 1


type EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME = "XR_EXT_win32_appcontainer_compatible"

-- No documentation found for TopLevel "XR_EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME"
pattern EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern EXT_WIN32_APPCONTAINER_COMPATIBLE_EXTENSION_NAME = "XR_EXT_win32_appcontainer_compatible"

