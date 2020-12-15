{-# language CPP #-}
-- | = Name
--
-- XR_KHR_opengl_es_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_es_enable  XR_KHR_opengl_es_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 25
--
-- = Revision
--
-- 7
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingOpenGLESAndroidKHR', 'GraphicsRequirementsOpenGLESKHR',
-- 'SwapchainImageOpenGLESKHR', 'getOpenGLESGraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_es_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_opengl_es_enable  ( GraphicsBindingOpenGLESAndroidKHR
                                                  , GraphicsRequirementsOpenGLESKHR
                                                  , SwapchainImageOpenGLESKHR
                                                  ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data GraphicsBindingOpenGLESAndroidKHR

instance ToCStruct GraphicsBindingOpenGLESAndroidKHR
instance Show GraphicsBindingOpenGLESAndroidKHR

instance FromCStruct GraphicsBindingOpenGLESAndroidKHR


data GraphicsRequirementsOpenGLESKHR

instance ToCStruct GraphicsRequirementsOpenGLESKHR
instance Show GraphicsRequirementsOpenGLESKHR

instance FromCStruct GraphicsRequirementsOpenGLESKHR


data SwapchainImageOpenGLESKHR

instance ToCStruct SwapchainImageOpenGLESKHR
instance Show SwapchainImageOpenGLESKHR

instance FromCStruct SwapchainImageOpenGLESKHR

