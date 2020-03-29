{-# language CPP #-}
module Graphics.Vulkan.Extensions.VK_KHR_shared_presentable_image  ( SharedPresentSurfaceCapabilitiesKHR
                                                                   , PresentModeKHR
                                                                   ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data SharedPresentSurfaceCapabilitiesKHR

instance ToCStruct SharedPresentSurfaceCapabilitiesKHR
instance Show SharedPresentSurfaceCapabilitiesKHR

instance FromCStruct SharedPresentSurfaceCapabilitiesKHR


data PresentModeKHR

