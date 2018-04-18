{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_device_group_creation
  ( pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
  , pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  , vkEnumeratePhysicalDeviceGroupsKHR
  , VkPhysicalDeviceGroupPropertiesKHR
  , pattern VkPhysicalDeviceGroupPropertiesKHR
  , VkDeviceGroupDeviceCreateInfoKHR
  , pattern VkDeviceGroupDeviceCreateInfoKHR
  , pattern VK_MAX_DEVICE_GROUP_SIZE_KHR
  , VK_MAX_DEVICE_GROUP_SIZE_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR
  ) where

import Data.String
  ( IsString
  )
import Data.Vector.Storable.Sized
  ( Vector
  )
import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( Ptr
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkMemoryHeapFlagBits(..)
  , VkPhysicalDevice
  , VkInstance
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  , VkDeviceGroupDeviceCreateInfo(..)
  , VK_MAX_DEVICE_GROUP_SIZE
  , VkPhysicalDeviceGroupProperties(..)
  , vkEnumeratePhysicalDeviceGroups
  )


pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1
pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME = "VK_KHR_device_group_creation"
vkEnumeratePhysicalDeviceGroupsKHR :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
vkEnumeratePhysicalDeviceGroupsKHR = vkEnumeratePhysicalDeviceGroups
type VkPhysicalDeviceGroupPropertiesKHR = VkPhysicalDeviceGroupProperties


pattern VkPhysicalDeviceGroupPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("physicalDeviceCount" ::: Word32) -> ("physicalDevices" ::: Vector VK_MAX_DEVICE_GROUP_SIZE VkPhysicalDevice) -> ("subsetAllocation" ::: VkBool32) -> VkPhysicalDeviceGroupPropertiesKHR
pattern VkPhysicalDeviceGroupPropertiesKHR vkSType vkNext vkPhysicalDeviceCount vkPhysicalDevices vkSubsetAllocation = VkPhysicalDeviceGroupProperties vkSType vkNext vkPhysicalDeviceCount vkPhysicalDevices vkSubsetAllocation
type VkDeviceGroupDeviceCreateInfoKHR = VkDeviceGroupDeviceCreateInfo


pattern VkDeviceGroupDeviceCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("physicalDeviceCount" ::: Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> VkDeviceGroupDeviceCreateInfoKHR
pattern VkDeviceGroupDeviceCreateInfoKHR vkSType vkNext vkPhysicalDeviceCount vkPhysicalDevices = VkDeviceGroupDeviceCreateInfo vkSType vkNext vkPhysicalDeviceCount vkPhysicalDevices
pattern VK_MAX_DEVICE_GROUP_SIZE_KHR :: Integral a => a
pattern VK_MAX_DEVICE_GROUP_SIZE_KHR = VK_MAX_DEVICE_GROUP_SIZE


type VK_MAX_DEVICE_GROUP_SIZE_KHR = VK_MAX_DEVICE_GROUP_SIZE
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR = VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
