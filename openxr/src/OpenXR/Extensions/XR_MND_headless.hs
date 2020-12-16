{-# language CPP #-}
-- | = Name
--
-- XR_MND_headless - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MND_headless  XR_MND_headless>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 43
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
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MND_headless OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MND_headless  ( MND_headless_SPEC_VERSION
                                          , pattern MND_headless_SPEC_VERSION
                                          , MND_HEADLESS_EXTENSION_NAME
                                          , pattern MND_HEADLESS_EXTENSION_NAME
                                          ) where

import Data.String (IsString)

type MND_headless_SPEC_VERSION = 2

-- No documentation found for TopLevel "XR_MND_headless_SPEC_VERSION"
pattern MND_headless_SPEC_VERSION :: forall a . Integral a => a
pattern MND_headless_SPEC_VERSION = 2


type MND_HEADLESS_EXTENSION_NAME = "XR_MND_headless"

-- No documentation found for TopLevel "XR_MND_HEADLESS_EXTENSION_NAME"
pattern MND_HEADLESS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MND_HEADLESS_EXTENSION_NAME = "XR_MND_headless"

