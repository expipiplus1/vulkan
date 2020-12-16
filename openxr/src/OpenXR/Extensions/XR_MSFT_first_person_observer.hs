{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_first_person_observer - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_first_person_observer  XR_MSFT_first_person_observer>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 55
--
-- = Revision
--
-- 1
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- -   Requires @@
--
-- = See Also
--
-- No cross-references are available
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_first_person_observer OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_first_person_observer  ( MSFT_first_person_observer_SPEC_VERSION
                                                        , pattern MSFT_first_person_observer_SPEC_VERSION
                                                        , MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME
                                                        , pattern MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME
                                                        ) where

import Data.String (IsString)

type MSFT_first_person_observer_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_first_person_observer_SPEC_VERSION"
pattern MSFT_first_person_observer_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_first_person_observer_SPEC_VERSION = 1


type MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME = "XR_MSFT_first_person_observer"

-- No documentation found for TopLevel "XR_MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME"
pattern MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_FIRST_PERSON_OBSERVER_EXTENSION_NAME = "XR_MSFT_first_person_observer"

