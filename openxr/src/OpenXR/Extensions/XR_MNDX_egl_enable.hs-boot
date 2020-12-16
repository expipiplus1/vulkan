{-# language CPP #-}
-- | = Name
--
-- XR_MNDX_egl_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MNDX_egl_enable  XR_MNDX_egl_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 49
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
-- 'GraphicsBindingEGLMNDX'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_MNDX_egl_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_MNDX_egl_enable  (GraphicsBindingEGLMNDX) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data GraphicsBindingEGLMNDX

instance ToCStruct GraphicsBindingEGLMNDX
instance Show GraphicsBindingEGLMNDX

instance FromCStruct GraphicsBindingEGLMNDX

