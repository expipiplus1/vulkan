{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2"
module Vulkan.Core11.Promoted_From_VK_KHR_device_groupAndVK_KHR_bind_memory2  ( BindBufferMemoryDeviceGroupInfo
                                                                              , BindImageMemoryDeviceGroupInfo
                                                                              ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data BindBufferMemoryDeviceGroupInfo

instance ToCStruct BindBufferMemoryDeviceGroupInfo
instance Show BindBufferMemoryDeviceGroupInfo

instance FromCStruct BindBufferMemoryDeviceGroupInfo


data BindImageMemoryDeviceGroupInfo

instance ToCStruct BindImageMemoryDeviceGroupInfo
instance Show BindImageMemoryDeviceGroupInfo

instance FromCStruct BindImageMemoryDeviceGroupInfo

