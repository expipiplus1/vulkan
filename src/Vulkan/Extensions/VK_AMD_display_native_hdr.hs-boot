{-# language CPP #-}
module Vulkan.Extensions.VK_AMD_display_native_hdr  ( DisplayNativeHdrSurfaceCapabilitiesAMD
                                                    , SwapchainDisplayNativeHdrCreateInfoAMD
                                                    ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DisplayNativeHdrSurfaceCapabilitiesAMD

instance ToCStruct DisplayNativeHdrSurfaceCapabilitiesAMD
instance Show DisplayNativeHdrSurfaceCapabilitiesAMD

instance FromCStruct DisplayNativeHdrSurfaceCapabilitiesAMD


data SwapchainDisplayNativeHdrCreateInfoAMD

instance ToCStruct SwapchainDisplayNativeHdrCreateInfoAMD
instance Show SwapchainDisplayNativeHdrCreateInfoAMD

instance FromCStruct SwapchainDisplayNativeHdrCreateInfoAMD

