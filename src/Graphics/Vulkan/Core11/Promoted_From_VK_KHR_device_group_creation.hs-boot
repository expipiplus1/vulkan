{-# language CPP #-}
module Graphics.Vulkan.Core11.Promoted_From_VK_KHR_device_group_creation  ( DeviceGroupDeviceCreateInfo
                                                                          , PhysicalDeviceGroupProperties
                                                                          ) where

import Data.Kind (Type)
import Graphics.Vulkan.CStruct (FromCStruct)
import Graphics.Vulkan.CStruct (ToCStruct)
data DeviceGroupDeviceCreateInfo

instance ToCStruct DeviceGroupDeviceCreateInfo
instance Show DeviceGroupDeviceCreateInfo

instance FromCStruct DeviceGroupDeviceCreateInfo


data PhysicalDeviceGroupProperties

instance ToCStruct PhysicalDeviceGroupProperties
instance Show PhysicalDeviceGroupProperties

instance FromCStruct PhysicalDeviceGroupProperties

