{-# language CPP #-}
module Vulkan.Extensions.VK_EXT_buffer_device_address  ( BufferDeviceAddressCreateInfoEXT
                                                       , PhysicalDeviceBufferDeviceAddressFeaturesEXT
                                                       ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data BufferDeviceAddressCreateInfoEXT

instance ToCStruct BufferDeviceAddressCreateInfoEXT
instance Show BufferDeviceAddressCreateInfoEXT

instance FromCStruct BufferDeviceAddressCreateInfoEXT


data PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT
instance Show PhysicalDeviceBufferDeviceAddressFeaturesEXT

instance FromCStruct PhysicalDeviceBufferDeviceAddressFeaturesEXT

