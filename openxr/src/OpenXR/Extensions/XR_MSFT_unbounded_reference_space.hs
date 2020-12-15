{-# language CPP #-}
-- | = Name
--
-- XR_MSFT_unbounded_reference_space - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_unbounded_reference_space  XR_MSFT_unbounded_reference_space>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 39
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
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MSFT_unbounded_reference_space OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MSFT_unbounded_reference_space  ( MSFT_unbounded_reference_space_SPEC_VERSION
                                                            , pattern MSFT_unbounded_reference_space_SPEC_VERSION
                                                            , MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME
                                                            , pattern MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME
                                                            ) where

import Data.String (IsString)

type MSFT_unbounded_reference_space_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_MSFT_unbounded_reference_space_SPEC_VERSION"
pattern MSFT_unbounded_reference_space_SPEC_VERSION :: forall a . Integral a => a
pattern MSFT_unbounded_reference_space_SPEC_VERSION = 1


type MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME = "XR_MSFT_unbounded_reference_space"

-- No documentation found for TopLevel "XR_MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME"
pattern MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern MSFT_UNBOUNDED_REFERENCE_SPACE_EXTENSION_NAME = "XR_MSFT_unbounded_reference_space"

