{-# language CPP #-}
-- No documentation found for Chapter "Promoted_From_VK_KHR_maintenance4"
module Vulkan.Core13.Promoted_From_VK_KHR_maintenance4  ( DeviceBufferMemoryRequirements
                                                        , DeviceImageMemoryRequirements
                                                        , PhysicalDeviceMaintenance4Features
                                                        , PhysicalDeviceMaintenance4Properties
                                                        ) where

import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
import Data.Kind (Type)

data DeviceBufferMemoryRequirements

instance ToCStruct DeviceBufferMemoryRequirements
instance Show DeviceBufferMemoryRequirements

instance FromCStruct DeviceBufferMemoryRequirements


data DeviceImageMemoryRequirements

instance ToCStruct DeviceImageMemoryRequirements
instance Show DeviceImageMemoryRequirements

instance FromCStruct DeviceImageMemoryRequirements


data PhysicalDeviceMaintenance4Features

instance ToCStruct PhysicalDeviceMaintenance4Features
instance Show PhysicalDeviceMaintenance4Features

instance FromCStruct PhysicalDeviceMaintenance4Features


data PhysicalDeviceMaintenance4Properties

instance ToCStruct PhysicalDeviceMaintenance4Properties
instance Show PhysicalDeviceMaintenance4Properties

instance FromCStruct PhysicalDeviceMaintenance4Properties

