{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfoKHR
  , pattern VkBindBufferMemoryInfoKHR
  , VkBindImageMemoryInfoKHR
  , pattern VkBindImageMemoryInfoKHR
  , vkBindBufferMemory2KHR
  , vkBindImageMemory2KHR
  , pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR
  , pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME
  , pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
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


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkDevice
  , VkDeviceSize
  )
import Graphics.Vulkan.C.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , vkBindBufferMemory2
  , vkBindImageMemory2
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkBindBufferMemoryInfoKHR"
type VkBindBufferMemoryInfoKHR = VkBindBufferMemoryInfo


-- No documentation found for TopLevel "VkBindBufferMemoryInfoKHR"
pattern VkBindBufferMemoryInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("buffer" ::: VkBuffer) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> VkBindBufferMemoryInfoKHR
pattern VkBindBufferMemoryInfoKHR vkSType vkPNext vkBuffer vkMemory vkMemoryOffset = VkBindBufferMemoryInfo vkSType vkPNext vkBuffer vkMemory vkMemoryOffset

-- No documentation found for TopLevel "VkBindImageMemoryInfoKHR"
type VkBindImageMemoryInfoKHR = VkBindImageMemoryInfo


-- No documentation found for TopLevel "VkBindImageMemoryInfoKHR"
pattern VkBindImageMemoryInfoKHR :: ("sType" ::: VkStructureType) -> ("pNext" ::: Ptr ()) -> ("image" ::: VkImage) -> ("memory" ::: VkDeviceMemory) -> ("memoryOffset" ::: VkDeviceSize) -> VkBindImageMemoryInfoKHR
pattern VkBindImageMemoryInfoKHR vkSType vkPNext vkImage vkMemory vkMemoryOffset = VkBindImageMemoryInfo vkSType vkPNext vkImage vkMemory vkMemoryOffset

-- No documentation found for TopLevel "vkBindBufferMemory2KHR"
vkBindBufferMemory2KHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
vkBindBufferMemory2KHR = vkBindBufferMemory2

-- No documentation found for TopLevel "vkBindImageMemory2KHR"
vkBindImageMemory2KHR :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
vkBindImageMemory2KHR = vkBindImageMemory2

-- No documentation found for TopLevel "VK_IMAGE_CREATE_ALIAS_BIT_KHR"
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT_KHR = VK_IMAGE_CREATE_ALIAS_BIT

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_EXTENSION_NAME"
pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_KHR_BIND_MEMORY_2_EXTENSION_NAME = "VK_KHR_bind_memory2"

-- No documentation found for TopLevel "VK_KHR_BIND_MEMORY_2_SPEC_VERSION"
pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION :: Integral a => a
pattern VK_KHR_BIND_MEMORY_2_SPEC_VERSION = 1

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO_KHR = VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO

-- No documentation found for TopLevel "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO_KHR = VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
