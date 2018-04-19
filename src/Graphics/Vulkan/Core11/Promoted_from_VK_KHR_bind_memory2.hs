{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_bind_memory2
  ( pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  , vkBindBufferMemory2
  , vkBindImageMemory2
  , VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


import Graphics.Vulkan.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  , VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.Core10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkImage
  , VkBuffer
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO = VkStructureType 1000157000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO = VkStructureType 1000157001
-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_ALIAS_BIT"
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateFlagBits 0x00000400
-- | vkBindBufferMemory2 - Bind device memory to buffer objects
foreign import ccall "vkBindBufferMemory2" vkBindBufferMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
-- | vkBindImageMemory2 - Bind device memory to image objects
foreign import ccall "vkBindImageMemory2" vkBindImageMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
-- | VkBindBufferMemoryInfo - Structure specifying how to bind a buffer to
-- memory
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo
  { -- No documentation found for Nested "VkBindBufferMemoryInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "vkBuffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "vkMemory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "vkMemoryOffset"
  vkMemoryOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBindBufferMemoryInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindBufferMemoryInfo <$> peek (ptr `plusPtr` 0)
                                    <*> peek (ptr `plusPtr` 8)
                                    <*> peek (ptr `plusPtr` 16)
                                    <*> peek (ptr `plusPtr` 24)
                                    <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindBufferMemoryInfo))
-- | VkBindImageMemoryInfo - Structure specifying how to bind an image to
-- memory
data VkBindImageMemoryInfo = VkBindImageMemoryInfo
  { -- No documentation found for Nested "VkBindImageMemoryInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "vkImage"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "vkMemory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "vkMemoryOffset"
  vkMemoryOffset :: VkDeviceSize
  }
  deriving (Eq, Show)

instance Storable VkBindImageMemoryInfo where
  sizeOf ~_ = 40
  alignment ~_ = 8
  peek ptr = VkBindImageMemoryInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
                                   <*> peek (ptr `plusPtr` 24)
                                   <*> peek (ptr `plusPtr` 32)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindImageMemoryInfo))
