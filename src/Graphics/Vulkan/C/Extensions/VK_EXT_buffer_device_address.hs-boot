{-# language Strict #-}
{-# language CPP #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_buffer_device_address
  ( VkBufferDeviceAddressCreateInfoEXT
  , VkBufferDeviceAddressInfoEXT
  , VkDeviceAddress
  , VkPhysicalDeviceBufferAddressFeaturesEXT
  , VkPhysicalDeviceBufferDeviceAddressFeaturesEXT
  , FN_vkGetBufferDeviceAddressEXT
  , PFN_vkGetBufferDeviceAddressEXT
  ) where

import Data.Word
  ( Word64
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  )


import Graphics.Vulkan.NamedType
  ( (:::)
  )
import {-# source #-} Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkDevice
  )


data VkBufferDeviceAddressCreateInfoEXT

data VkBufferDeviceAddressInfoEXT

-- No documentation found for TopLevel "VkDeviceAddress"
type VkDeviceAddress = Word64

-- No documentation found for TopLevel "VkPhysicalDeviceBufferAddressFeaturesEXT"
type VkPhysicalDeviceBufferAddressFeaturesEXT = VkPhysicalDeviceBufferDeviceAddressFeaturesEXT

data VkPhysicalDeviceBufferDeviceAddressFeaturesEXT

type FN_vkGetBufferDeviceAddressEXT = ("device" ::: VkDevice) -> ("pInfo" ::: Ptr VkBufferDeviceAddressInfoEXT) -> IO VkDeviceAddress
type PFN_vkGetBufferDeviceAddressEXT = FunPtr FN_vkGetBufferDeviceAddressEXT
