{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_bind_memory2
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


import Graphics.Vulkan.Version10.Core
  ( VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Version10.DeviceInitialization
  ( VkDeviceSize
  , VkDevice
  , VkImageCreateFlagBits(..)
  )
import Graphics.Vulkan.Version10.Memory
  ( VkDeviceMemory
  )
import Graphics.Vulkan.Version10.MemoryManagement
  ( VkImage
  , VkBuffer
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO = VkStructureType 1000157000
-- | Nothing
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO = VkStructureType 1000157001
-- | Nothing
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateFlagBits 0x00000400
-- | 
foreign import ccall "vkBindBufferMemory2" vkBindBufferMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
-- | 
foreign import ccall "vkBindImageMemory2" vkBindImageMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
-- | TODO: Struct comments
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkBuffer :: VkBuffer
  , vkMemory :: VkDeviceMemory
  , vkMemoryOffset :: VkDeviceSize
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkBuffer (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindBufferMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindBufferMemoryInfo))
-- | TODO: Struct comments
data VkBindImageMemoryInfo = VkBindImageMemoryInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkImage :: VkImage
  , vkMemory :: VkDeviceMemory
  , vkMemoryOffset :: VkDeviceSize
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
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 24) (vkMemory (poked :: VkBindImageMemoryInfo))
                *> poke (ptr `plusPtr` 32) (vkMemoryOffset (poked :: VkBindImageMemoryInfo))
