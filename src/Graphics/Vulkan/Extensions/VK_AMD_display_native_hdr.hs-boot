{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_AMD_display_native_hdr  ( DisplayNativeHdrSurfaceCapabilitiesAMD
                                                             , SwapchainDisplayNativeHdrCreateInfoAMD
                                                             ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DisplayNativeHdrSurfaceCapabilitiesAMD

instance ToCStruct DisplayNativeHdrSurfaceCapabilitiesAMD
instance Show DisplayNativeHdrSurfaceCapabilitiesAMD

instance FromCStruct DisplayNativeHdrSurfaceCapabilitiesAMD


data SwapchainDisplayNativeHdrCreateInfoAMD

instance ToCStruct SwapchainDisplayNativeHdrCreateInfoAMD
instance Show SwapchainDisplayNativeHdrCreateInfoAMD

instance FromCStruct SwapchainDisplayNativeHdrCreateInfoAMD

