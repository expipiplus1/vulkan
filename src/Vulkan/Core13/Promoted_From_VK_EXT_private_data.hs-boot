{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_EXT_private_data"
module Vulkan.Core13.Promoted_From_VK_EXT_private_data  ( DevicePrivateDataCreateInfo
                                                        , PhysicalDevicePrivateDataFeatures
                                                        , PrivateDataSlotCreateInfo
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DevicePrivateDataCreateInfo

instance ToCStruct DevicePrivateDataCreateInfo
instance Show DevicePrivateDataCreateInfo

instance FromCStruct DevicePrivateDataCreateInfo


data PhysicalDevicePrivateDataFeatures

instance ToCStruct PhysicalDevicePrivateDataFeatures
instance Show PhysicalDevicePrivateDataFeatures

instance FromCStruct PhysicalDevicePrivateDataFeatures


data PrivateDataSlotCreateInfo

instance ToCStruct PrivateDataSlotCreateInfo
instance Show PrivateDataSlotCreateInfo

instance FromCStruct PrivateDataSlotCreateInfo

