{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_swapchain  ( AcquireNextImageInfoKHR
                                                    , BindImageMemorySwapchainInfoKHR
                                                    , DeviceGroupPresentCapabilitiesKHR
                                                    , DeviceGroupPresentInfoKHR
                                                    , DeviceGroupSwapchainCreateInfoKHR
                                                    , ImageSwapchainCreateInfoKHR
                                                    , PresentInfoKHR
                                                    , SwapchainCreateInfoKHR
                                                    , DeviceGroupPresentModeFlagBitsKHR
                                                    , DeviceGroupPresentModeFlagsKHR
                                                    ) where

import Data.Kind (Type)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (Chain)
import Graphics.Vulkan.CStruct (FromCStruct)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PeekChain)
import {-# SOURCE #-} Graphics.Vulkan.CStruct.Extends (PokeChain)
import Graphics.Vulkan.CStruct (ToCStruct)
data AcquireNextImageInfoKHR

instance ToCStruct AcquireNextImageInfoKHR
instance Show AcquireNextImageInfoKHR

instance FromCStruct AcquireNextImageInfoKHR


data BindImageMemorySwapchainInfoKHR

instance ToCStruct BindImageMemorySwapchainInfoKHR
instance Show BindImageMemorySwapchainInfoKHR

instance FromCStruct BindImageMemorySwapchainInfoKHR


data DeviceGroupPresentCapabilitiesKHR

instance ToCStruct DeviceGroupPresentCapabilitiesKHR
instance Show DeviceGroupPresentCapabilitiesKHR

instance FromCStruct DeviceGroupPresentCapabilitiesKHR


data DeviceGroupPresentInfoKHR

instance ToCStruct DeviceGroupPresentInfoKHR
instance Show DeviceGroupPresentInfoKHR

instance FromCStruct DeviceGroupPresentInfoKHR


data DeviceGroupSwapchainCreateInfoKHR

instance ToCStruct DeviceGroupSwapchainCreateInfoKHR
instance Show DeviceGroupSwapchainCreateInfoKHR

instance FromCStruct DeviceGroupSwapchainCreateInfoKHR


data ImageSwapchainCreateInfoKHR

instance ToCStruct ImageSwapchainCreateInfoKHR
instance Show ImageSwapchainCreateInfoKHR

instance FromCStruct ImageSwapchainCreateInfoKHR


type role PresentInfoKHR nominal
data PresentInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (PresentInfoKHR es)
instance Show (Chain es) => Show (PresentInfoKHR es)

instance PeekChain es => FromCStruct (PresentInfoKHR es)


type role SwapchainCreateInfoKHR nominal
data SwapchainCreateInfoKHR (es :: [Type])

instance PokeChain es => ToCStruct (SwapchainCreateInfoKHR es)
instance Show (Chain es) => Show (SwapchainCreateInfoKHR es)

instance PeekChain es => FromCStruct (SwapchainCreateInfoKHR es)


data DeviceGroupPresentModeFlagBitsKHR

type DeviceGroupPresentModeFlagsKHR = DeviceGroupPresentModeFlagBitsKHR

