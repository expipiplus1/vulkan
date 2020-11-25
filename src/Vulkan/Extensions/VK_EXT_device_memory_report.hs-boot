{-# language CPP #-}
-- No documentation found for Chapter "VK_EXT_device_memory_report"
module Vulkan.Extensions.VK_EXT_device_memory_report  ( DeviceDeviceMemoryReportCreateInfoEXT
                                                      , DeviceMemoryReportCallbackDataEXT
                                                      , PhysicalDeviceDeviceMemoryReportFeaturesEXT
                                                      ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DeviceDeviceMemoryReportCreateInfoEXT

instance ToCStruct DeviceDeviceMemoryReportCreateInfoEXT
instance Show DeviceDeviceMemoryReportCreateInfoEXT

instance FromCStruct DeviceDeviceMemoryReportCreateInfoEXT


data DeviceMemoryReportCallbackDataEXT

instance ToCStruct DeviceMemoryReportCallbackDataEXT
instance Show DeviceMemoryReportCallbackDataEXT

instance FromCStruct DeviceMemoryReportCallbackDataEXT


data PhysicalDeviceDeviceMemoryReportFeaturesEXT

instance ToCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT
instance Show PhysicalDeviceDeviceMemoryReportFeaturesEXT

instance FromCStruct PhysicalDeviceDeviceMemoryReportFeaturesEXT

