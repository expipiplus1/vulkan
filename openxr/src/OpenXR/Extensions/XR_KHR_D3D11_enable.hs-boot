{-# language CPP #-}
-- | = Name
--
-- XR_KHR_D3D11_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D11_enable  XR_KHR_D3D11_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 28
--
-- = Revision
--
-- 5
--
-- = Extension and Version Dependencies
--
-- -   Requires OpenXR 1.0
--
-- = See Also
--
-- 'GraphicsBindingD3D11KHR', 'GraphicsRequirementsD3D11KHR',
-- 'SwapchainImageD3D11KHR', 'getD3D11GraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D11_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_D3D11_enable  ( GraphicsBindingD3D11KHR
                                              , GraphicsRequirementsD3D11KHR
                                              , SwapchainImageD3D11KHR
                                              ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data GraphicsBindingD3D11KHR

instance ToCStruct GraphicsBindingD3D11KHR
instance Show GraphicsBindingD3D11KHR

instance FromCStruct GraphicsBindingD3D11KHR


data GraphicsRequirementsD3D11KHR

instance ToCStruct GraphicsRequirementsD3D11KHR
instance Show GraphicsRequirementsD3D11KHR

instance FromCStruct GraphicsRequirementsD3D11KHR


data SwapchainImageD3D11KHR

instance ToCStruct SwapchainImageD3D11KHR
instance Show SwapchainImageD3D11KHR

instance FromCStruct SwapchainImageD3D11KHR

