{-# language CPP #-}
module Vulkan.Extensions.VK_KHR_shared_presentable_image  ( SharedPresentSurfaceCapabilitiesKHR
                                                          , PresentModeKHR
                                                          ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data SharedPresentSurfaceCapabilitiesKHR

instance ToCStruct SharedPresentSurfaceCapabilitiesKHR
instance Show SharedPresentSurfaceCapabilitiesKHR

instance FromCStruct SharedPresentSurfaceCapabilitiesKHR


data PresentModeKHR

