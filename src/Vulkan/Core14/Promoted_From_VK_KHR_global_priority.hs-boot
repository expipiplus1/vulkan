{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_global_priority"
module Vulkan.Core14.Promoted_From_VK_KHR_global_priority  ( DeviceQueueGlobalPriorityCreateInfo
                                                           , PhysicalDeviceGlobalPriorityQueryFeatures
                                                           , QueueFamilyGlobalPriorityProperties
                                                           ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceQueueGlobalPriorityCreateInfo

instance ToCStruct DeviceQueueGlobalPriorityCreateInfo
instance Show DeviceQueueGlobalPriorityCreateInfo

instance FromCStruct DeviceQueueGlobalPriorityCreateInfo


data PhysicalDeviceGlobalPriorityQueryFeatures

instance ToCStruct PhysicalDeviceGlobalPriorityQueryFeatures
instance Show PhysicalDeviceGlobalPriorityQueryFeatures

instance FromCStruct PhysicalDeviceGlobalPriorityQueryFeatures


data QueueFamilyGlobalPriorityProperties

instance ToCStruct QueueFamilyGlobalPriorityProperties
instance Show QueueFamilyGlobalPriorityProperties

instance FromCStruct QueueFamilyGlobalPriorityProperties

