{-# language CPP #-}
-- | = Name
--
-- XR_KHR_D3D12_enable - instance extension
--
-- = Specification
--
-- See
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D12_enable  XR_KHR_D3D12_enable>
-- in the main specification for complete information.
--
-- = Registered Extension Number
--
-- 29
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
-- 'GraphicsBindingD3D12KHR', 'GraphicsRequirementsD3D12KHR',
-- 'SwapchainImageD3D12KHR', 'getD3D12GraphicsRequirementsKHR'
--
-- = Document Notes
--
-- For more information, see the
-- <https://www.khronos.org/registry/OpenXR/specs/1.0/html/xrspec.html#XR_KHR_D3D12_enable OpenXR Specification>
--
-- This page is a generated document. Fixes and changes should be made to
-- the generator scripts, not directly.
module OpenXR.Extensions.XR_KHR_D3D12_enable  ( GraphicsBindingD3D12KHR
                                              , GraphicsRequirementsD3D12KHR
                                              , SwapchainImageD3D12KHR
                                              ) where

import Data.Kind (Type)
import OpenXR.CStruct (FromCStruct)
import OpenXR.CStruct (ToCStruct)
data GraphicsBindingD3D12KHR

instance ToCStruct GraphicsBindingD3D12KHR
instance Show GraphicsBindingD3D12KHR

instance FromCStruct GraphicsBindingD3D12KHR


data GraphicsRequirementsD3D12KHR

instance ToCStruct GraphicsRequirementsD3D12KHR
instance Show GraphicsRequirementsD3D12KHR

instance FromCStruct GraphicsRequirementsD3D12KHR


data SwapchainImageD3D12KHR

instance ToCStruct SwapchainImageD3D12KHR
instance Show SwapchainImageD3D12KHR

instance FromCStruct SwapchainImageD3D12KHR

