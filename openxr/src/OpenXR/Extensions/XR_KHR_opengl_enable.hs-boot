{-# language CPP #-}
-- | = Name
--
-- XR_KHR_opengl_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_enable  XR_KHR_opengl_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 24
--
-- = Revision
--
-- 9
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingOpenGLWaylandKHR', 'GraphicsBindingOpenGLWin32KHR',
-- 'GraphicsBindingOpenGLXcbKHR', 'GraphicsBindingOpenGLXlibKHR',
-- 'GraphicsRequirementsOpenGLKHR', 'SwapchainImageOpenGLKHR',
-- 'getOpenGLGraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_opengl_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_opengl_enable  ( GraphicsBindingOpenGLWaylandKHR
                                               , GraphicsBindingOpenGLWin32KHR
                                               , GraphicsBindingOpenGLXcbKHR
                                               , GraphicsBindingOpenGLXlibKHR
                                               , GraphicsRequirementsOpenGLKHR
                                               , SwapchainImageOpenGLKHR
                                               ) where

import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
import Data.Kind (Type)

data GraphicsBindingOpenGLWaylandKHR

instance ToCStruct GraphicsBindingOpenGLWaylandKHR
instance Show GraphicsBindingOpenGLWaylandKHR

instance FromCStruct GraphicsBindingOpenGLWaylandKHR


data GraphicsBindingOpenGLWin32KHR

instance ToCStruct GraphicsBindingOpenGLWin32KHR
instance Show GraphicsBindingOpenGLWin32KHR

instance FromCStruct GraphicsBindingOpenGLWin32KHR


data GraphicsBindingOpenGLXcbKHR

instance ToCStruct GraphicsBindingOpenGLXcbKHR
instance Show GraphicsBindingOpenGLXcbKHR

instance FromCStruct GraphicsBindingOpenGLXcbKHR


data GraphicsBindingOpenGLXlibKHR

instance ToCStruct GraphicsBindingOpenGLXlibKHR
instance Show GraphicsBindingOpenGLXlibKHR

instance FromCStruct GraphicsBindingOpenGLXlibKHR


data GraphicsRequirementsOpenGLKHR

instance ToCStruct GraphicsRequirementsOpenGLKHR
instance Show GraphicsRequirementsOpenGLKHR

instance FromCStruct GraphicsRequirementsOpenGLKHR


data SwapchainImageOpenGLKHR

instance ToCStruct SwapchainImageOpenGLKHR
instance Show SwapchainImageOpenGLKHR

instance FromCStruct SwapchainImageOpenGLKHR

