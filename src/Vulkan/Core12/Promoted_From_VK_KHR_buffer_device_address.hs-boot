{-# language CPP #-}
module Vulkan.Core12.Promoted_From_VK_KHR_buffer_device_address  ( BufferDeviceAddressInfo
                                                                 , BufferOpaqueCaptureAddressCreateInfo
                                                                 , DeviceMemoryOpaqueCaptureAddressInfo
                                                                 , MemoryOpaqueCaptureAddressAllocateInfo
                                                                 , PhysicalDeviceBufferDeviceAddressFeatures
                                                                 ) where

import Data.Kind (Type)
import Vulkan.CStruct (FromCStruct)
import Vulkan.CStruct (ToCStruct)
data BufferDeviceAddressInfo

instance ToCStruct BufferDeviceAddressInfo
instance Show BufferDeviceAddressInfo

instance FromCStruct BufferDeviceAddressInfo


data BufferOpaqueCaptureAddressCreateInfo

instance ToCStruct BufferOpaqueCaptureAddressCreateInfo
instance Show BufferOpaqueCaptureAddressCreateInfo

instance FromCStruct BufferOpaqueCaptureAddressCreateInfo


data DeviceMemoryOpaqueCaptureAddressInfo

instance ToCStruct DeviceMemoryOpaqueCaptureAddressInfo
instance Show DeviceMemoryOpaqueCaptureAddressInfo

instance FromCStruct DeviceMemoryOpaqueCaptureAddressInfo


data MemoryOpaqueCaptureAddressAllocateInfo

instance ToCStruct MemoryOpaqueCaptureAddressAllocateInfo
instance Show MemoryOpaqueCaptureAddressAllocateInfo

instance FromCStruct MemoryOpaqueCaptureAddressAllocateInfo


data PhysicalDeviceBufferDeviceAddressFeatures

instance ToCStruct PhysicalDeviceBufferDeviceAddressFeatures
instance Show PhysicalDeviceBufferDeviceAddressFeatures

instance FromCStruct PhysicalDeviceBufferDeviceAddressFeatures

