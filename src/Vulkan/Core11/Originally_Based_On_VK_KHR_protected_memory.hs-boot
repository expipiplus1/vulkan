{-# language CPP #-}
-- No documentation found for Chapter "Originally_Based_On_VK_KHR_protected_memory"
module Vulkan.Core11.Originally_Based_On_VK_KHR_protected_memory  ( DeviceQueueInfo2
                                                                  , PhysicalDeviceProtectedMemoryFeatures
                                                                  , PhysicalDeviceProtectedMemoryProperties
                                                                  , ProtectedSubmitInfo
                                                                  ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceQueueInfo2

instance ToCStruct DeviceQueueInfo2
instance Show DeviceQueueInfo2

instance FromCStruct DeviceQueueInfo2


data PhysicalDeviceProtectedMemoryFeatures

instance ToCStruct PhysicalDeviceProtectedMemoryFeatures
instance Show PhysicalDeviceProtectedMemoryFeatures

instance FromCStruct PhysicalDeviceProtectedMemoryFeatures


data PhysicalDeviceProtectedMemoryProperties

instance ToCStruct PhysicalDeviceProtectedMemoryProperties
instance Show PhysicalDeviceProtectedMemoryProperties

instance FromCStruct PhysicalDeviceProtectedMemoryProperties


data ProtectedSubmitInfo

instance ToCStruct ProtectedSubmitInfo
instance Show ProtectedSubmitInfo

instance FromCStruct ProtectedSubmitInfo

