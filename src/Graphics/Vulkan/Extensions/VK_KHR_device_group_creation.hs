{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.Extensions.VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfoKHR
  , PhysicalDeviceGroupPropertiesKHR
  , enumeratePhysicalDeviceGroupsKHR
  , pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  , pattern KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  ) where

import Data.String
  ( IsString
  )
import Data.Vector
  ( Vector
  )
import Data.Word
  ( Word32
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkMemoryHeapFlagBits(..)
  )
import Graphics.Vulkan.C.Extensions.VK_KHR_device_group_creation
  ( pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  , pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
  )
import Graphics.Vulkan.Core10.Core
  ( pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( Instance(..)
  , pattern MEMORY_HEAP_MULTI_INSTANCE_BIT
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( DeviceGroupDeviceCreateInfo(..)
  , PhysicalDeviceGroupProperties(..)
  , enumeratePhysicalDeviceGroups
  )


type DeviceGroupDeviceCreateInfoKHR = DeviceGroupDeviceCreateInfo
-- TODO: Pattern constructor alias)

type PhysicalDeviceGroupPropertiesKHR = PhysicalDeviceGroupProperties
-- TODO: Pattern constructor alias)

enumeratePhysicalDeviceGroupsKHR :: Instance ->  Word32 ->  IO (VkResult, Vector PhysicalDeviceGroupProperties)
enumeratePhysicalDeviceGroupsKHR = enumeratePhysicalDeviceGroups

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME"
pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME = VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME

-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION"
pattern KHR_DEVICE_GROUP_CREATION_SPEC_VERSION :: Integral a => a
pattern KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION

-- No documentation found for TopLevel "MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR"
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR :: VkMemoryHeapFlagBits
pattern MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR = MEMORY_HEAP_MULTI_INSTANCE_BIT

-- No documentation found for TopLevel "STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR"
pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR :: VkStructureType
pattern STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR = STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO

-- No documentation found for TopLevel "STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR"
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR :: VkStructureType
pattern STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR = STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
