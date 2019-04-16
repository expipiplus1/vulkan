{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfoKHR
  , PhysicalDeviceGroupPropertiesKHR
  , enumeratePhysicalDeviceGroupsKHR
  , pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
  , pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_MAX_DEVICE_GROUP_SIZE_KHR
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  ) where

import Data.Vector
  ( Vector
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Instance(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfo(..)
  , PhysicalDeviceGroupProperties(..)
  , enumeratePhysicalDeviceGroups
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group_creation
  ( pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  , pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
  , pattern VK_MAX_DEVICE_GROUP_SIZE_KHR
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR
  )


type DeviceGroupDeviceCreateInfoKHR = DeviceGroupDeviceCreateInfo
-- TODO: Pattern constructor alias)
type PhysicalDeviceGroupPropertiesKHR = PhysicalDeviceGroupProperties
-- TODO: Pattern constructor alias)
enumeratePhysicalDeviceGroupsKHR :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDeviceGroupProperties)
enumeratePhysicalDeviceGroupsKHR = enumeratePhysicalDeviceGroups
