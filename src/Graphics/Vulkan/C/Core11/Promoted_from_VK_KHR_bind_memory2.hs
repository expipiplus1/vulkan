{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_bind_memory2
  ( VkBindBufferMemoryInfo(..)
  , VkBindImageMemoryInfo(..)
  , FN_vkBindBufferMemory2
  , PFN_vkBindBufferMemory2
  , vkBindBufferMemory2
  , FN_vkBindImageMemory2
  , PFN_vkBindImageMemory2
  , vkBindImageMemory2
  , pattern VK_IMAGE_CREATE_ALIAS_BIT
  , pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
  , pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
  ) where

import Data.Word
  ( Word32
  )
import Foreign.Ptr
  ( FunPtr
  , Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
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
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkBindBufferMemoryInfo"
data VkBindBufferMemoryInfo = VkBindBufferMemoryInfo
  { -- No documentation found for Nested "VkBindBufferMemoryInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "buffer"
  vkBuffer :: VkBuffer
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkBindBufferMemoryInfo" "memoryOffset"
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

instance Zero VkBindBufferMemoryInfo where
  zero = VkBindBufferMemoryInfo VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO
                                zero
                                zero
                                zero
                                zero

-- No documentation found for TopLevel "VkBindImageMemoryInfo"
data VkBindImageMemoryInfo = VkBindImageMemoryInfo
  { -- No documentation found for Nested "VkBindImageMemoryInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "memory"
  vkMemory :: VkDeviceMemory
  , -- No documentation found for Nested "VkBindImageMemoryInfo" "memoryOffset"
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

instance Zero VkBindImageMemoryInfo where
  zero = VkBindImageMemoryInfo VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO
                               zero
                               zero
                               zero
                               zero

-- No documentation found for TopLevel "vkBindBufferMemory2"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindBufferMemory2" vkBindBufferMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
#else
vkBindBufferMemory2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
vkBindBufferMemory2 deviceCmds = mkVkBindBufferMemory2 (pVkBindBufferMemory2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindBufferMemory2
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult)
#endif

type FN_vkBindBufferMemory2 = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindBufferMemoryInfo) -> IO VkResult
type PFN_vkBindBufferMemory2 = FunPtr FN_vkBindBufferMemory2

-- No documentation found for TopLevel "vkBindImageMemory2"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkBindImageMemory2" vkBindImageMemory2 :: ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
#else
vkBindImageMemory2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
vkBindImageMemory2 deviceCmds = mkVkBindImageMemory2 (pVkBindImageMemory2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkBindImageMemory2
  :: FunPtr (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult) -> (("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult)
#endif

type FN_vkBindImageMemory2 = ("device" ::: VkDevice) -> ("bindInfoCount" ::: Word32) -> ("pBindInfos" ::: Ptr VkBindImageMemoryInfo) -> IO VkResult
type PFN_vkBindImageMemory2 = FunPtr FN_vkBindImageMemory2

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_ALIAS_BIT"
pattern VK_IMAGE_CREATE_ALIAS_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_ALIAS_BIT = VkImageCreateFlagBits 0x00000400

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_BUFFER_MEMORY_INFO = VkStructureType 1000157000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO"
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_BIND_IMAGE_MEMORY_INFO = VkStructureType 1000157001
