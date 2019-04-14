{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfoKHR
  , pattern VkDeviceGroupDeviceCreateInfoKHR
  , VkPhysicalDeviceGroupPropertiesKHR
  , pattern VkPhysicalDeviceGroupPropertiesKHR
#if defined(EXPOSE_CORE11_COMMANDS)
  , vkEnumeratePhysicalDeviceGroupsKHR
#endif
  , pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME
  , pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION
  , pattern VK_MAX_DEVICE_GROUP_SIZE_KHR
  , VK_MAX_DEVICE_GROUP_SIZE_KHR
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
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


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  )
#endif
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkMemoryHeapFlagBits(..)
  , VkPhysicalDevice
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkInstance
  )
#endif
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  )

#if defined(EXPOSE_CORE11_COMMANDS)
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( vkEnumeratePhysicalDeviceGroups
  )
#endif
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDeviceGroupDeviceCreateInfoKHR"
type VkDeviceGroupDeviceCreateInfoKHR = VkDeviceGroupDeviceCreateInfo


-- No documentation found for TopLevel "VkDeviceGroupDeviceCreateInfoKHR"
pattern VkDeviceGroupDeviceCreateInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("physicalDeviceCount" ::: Word32) -> ("pPhysicalDevices" ::: Ptr VkPhysicalDevice) -> VkDeviceGroupDeviceCreateInfoKHR
pattern VkDeviceGroupDeviceCreateInfoKHR vkSType vkPNext vkPhysicalDeviceCount vkPPhysicalDevices = VkDeviceGroupDeviceCreateInfo vkSType vkPNext vkPhysicalDeviceCount vkPPhysicalDevices
-- No documentation found for TopLevel "VkPhysicalDeviceGroupPropertiesKHR"
type VkPhysicalDeviceGroupPropertiesKHR = VkPhysicalDeviceGroupProperties


-- No documentation found for TopLevel "VkPhysicalDeviceGroupPropertiesKHR"
pattern VkPhysicalDeviceGroupPropertiesKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("physicalDeviceCount" ::: Word32) -> ("physicalDevices" ::: Vector VK_MAX_DEVICE_GROUP_SIZE VkPhysicalDevice) -> ("subsetAllocation" ::: VkBool32) -> VkPhysicalDeviceGroupPropertiesKHR
pattern VkPhysicalDeviceGroupPropertiesKHR vkSType vkPNext vkPhysicalDeviceCount vkPhysicalDevices vkSubsetAllocation = VkPhysicalDeviceGroupProperties vkSType vkPNext vkPhysicalDeviceCount vkPhysicalDevices vkSubsetAllocation

#if defined(EXPOSE_CORE11_COMMANDS)
-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceGroupsKHR"
vkEnumeratePhysicalDeviceGroupsKHR :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
vkEnumeratePhysicalDeviceGroupsKHR = vkEnumeratePhysicalDeviceGroups
#endif
-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME"
pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_DEVICE_GROUP_CREATION_EXTENSION_NAME = "VK_KHR_device_group_creation"
-- No documentation found for TopLevel "VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION"
pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION :: Integral a => a
pattern VK_KHR_DEVICE_GROUP_CREATION_SPEC_VERSION = 1
-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE_KHR"
pattern VK_MAX_DEVICE_GROUP_SIZE_KHR :: Integral a => a
pattern VK_MAX_DEVICE_GROUP_SIZE_KHR = VK_MAX_DEVICE_GROUP_SIZE


-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE_KHR"
type VK_MAX_DEVICE_GROUP_SIZE_KHR = VK_MAX_DEVICE_GROUP_SIZE
-- No documentation found for TopLevel "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR"
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT_KHR = VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO_KHR = VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES_KHR = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
