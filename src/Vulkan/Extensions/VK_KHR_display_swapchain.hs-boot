{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_display_swapchain  (DisplayPresentInfoKHR) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DisplayPresentInfoKHR

instance ToCStruct DisplayPresentInfoKHR
instance Show DisplayPresentInfoKHR

instance FromCStruct DisplayPresentInfoKHR

