{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_display_swapchain  (DisplayPresentInfoKHR) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DisplayPresentInfoKHR

instance ToCStruct DisplayPresentInfoKHR
instance Show DisplayPresentInfoKHR

instance FromCStruct DisplayPresentInfoKHR

