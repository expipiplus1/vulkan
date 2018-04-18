{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}

module Graphics.Vulkan.Extensions.VK_KHR_bind_memory2
  ( pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION
  , pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  , vkBindBufferMemory2KHR
  , vkBindImageMemory2KHR
  , VkBindBufferMemoryInfoKHR
  , pattern VkBindBufferMemoryInfoKHR
  , VkBindImageMemoryInfoKHR
  , pattern VkBindImageMemoryInfoKHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
  , pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR
  ) where

import Data.String
  ( IsString
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
  ( VkStructureType(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkDeviceSize
  , VkDevice
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkImage
  , VkBuffer
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_IMAGE_CREATE_ALIAS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , VkBindImageMemoryInfo(..)
  , vkBindImageMemory2
  , VkBindBufferMemoryInfo(..)
  , vkBindBufferMemory2
  )


pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1
pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"
vkBindBufferMemory2KHR :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
vkBindBufferMemory2KHR = vkBindBufferMemory2
vkBindImageMemory2KHR :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
vkBindImageMemory2KHR = vkBindImageMemory2
type VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfo


pattern VkBindBufferMemoryInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> VkBindBufferMemoryInfoKHR
pattern VkBindBufferMemoryInfoKHR vkSType vkPNext vkBuffer vkMemory vkMemoryOffset = VkBindBufferMemoryInfo vkSType vkPNext vkBuffer vkMemory vkMemoryOffset
type VkBindImageMemoryInfoKHR = VkBindImageMemoryInfo


pattern VkBindImageMemoryInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> VkBindImageMemoryInfoKHR
pattern VkBindImageMemoryInfoKHR vkSType vkPNext vkImage vkMemory vkMemoryOffset = VkBindImageMemoryInfo vkSType vkPNext vkImage vkMemory vkMemoryOffset
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR = VK_IMAGE_CREATE_ALIAS_BIT
