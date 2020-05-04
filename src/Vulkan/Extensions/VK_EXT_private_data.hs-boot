{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_private_data  ( DevicePrivateDataCreateInfoEXT
                                              , PhysicalDevicePrivateDataFeaturesEXT
                                              , PrivateDataSlotCreateInfoEXT
                                              ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data DevicePrivateDataCreateInfoEXT

instance ToCStruct DevicePrivateDataCreateInfoEXT
instance Show DevicePrivateDataCreateInfoEXT

instance FromCStruct DevicePrivateDataCreateInfoEXT


data PhysicalDevicePrivateDataFeaturesEXT

instance ToCStruct PhysicalDevicePrivateDataFeaturesEXT
instance Show PhysicalDevicePrivateDataFeaturesEXT

instance FromCStruct PhysicalDevicePrivateDataFeaturesEXT


data PrivateDataSlotCreateInfoEXT

instance ToCStruct PrivateDataSlotCreateInfoEXT
instance Show PrivateDataSlotCreateInfoEXT

instance FromCStruct PrivateDataSlotCreateInfoEXT

