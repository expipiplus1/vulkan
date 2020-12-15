{-# language CPP #-}
-- | = Name
--
-- XR_VARJO_quad_views - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_VARJO_quad_views  XR_VARJO_quad_views>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 38
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
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_VARJO_quad_views OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_VARJO_quad_views  ( VARJO_quad_views_SPEC_VERSION
                                              , pattern VARJO_quad_views_SPEC_VERSION
                                              , VARJO_QUAD_VIEWS_EXTENSION_NAME
                                              , pattern VARJO_QUAD_VIEWS_EXTENSION_NAME
                                              ) where

import Data.String (IsString)

type VARJO_quad_views_SPEC_VERSION = 1

-- No documentation found for TopLevel "XR_VARJO_quad_views_SPEC_VERSION"
pattern VARJO_quad_views_SPEC_VERSION :: forall a . Integral a => a
pattern VARJO_quad_views_SPEC_VERSION = 1


type VARJO_QUAD_VIEWS_EXTENSION_NAME = "XR_VARJO_quad_views"

-- No documentation found for TopLevel "XR_VARJO_QUAD_VIEWS_EXTENSION_NAME"
pattern VARJO_QUAD_VIEWS_EXTENSION_NAME :: forall a . (Eq a, IsString a) => a
pattern VARJO_QUAD_VIEWS_EXTENSION_NAME = "XR_VARJO_quad_views"

