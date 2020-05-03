{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_pci_bus_info  (PhysicalDevicePCIBusInfoPropertiesEXT) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data PhysicalDevicePCIBusInfoPropertiesEXT

instance ToCStruct PhysicalDevicePCIBusInfoPropertiesEXT
instance Show PhysicalDevicePCIBusInfoPropertiesEXT

instance FromCStruct PhysicalDevicePCIBusInfoPropertiesEXT

