{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_From_VK_KHR_protected_memory
  ( pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , pattern VK_QUEUE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_BUFFER_CREATE_PROTECTED_BIT
  , pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
  , vkGetDeviceQueue2
  , VkProtectedSubmitInfo(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkDeviceQueueInfo2(..)
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


import Graphics.Vulkan.Version10.Buffer
  ( VkBufferCreateFlagBits(..)
  )
import Graphics.Vulkan.Version10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  )
import Graphics.Vulkan.Version10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Version10.Device
  ( VkDeviceQueueCreateFlags
  , VkDeviceQueueCreateFlagBits(..)
  )
import Graphics.Vulkan.Version10.DeviceInitialization
  ( VkDevice
  , VkImageCreateFlagBits(..)
  , VkMemoryPropertyFlagBits(..)
  , VkQueueFlagBits(..)
  )
import Graphics.Vulkan.Version10.Queue
  ( VkQueue
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO = VkStructureType 1000145000
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES = VkStructureType 1000145001
-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES = VkStructureType 1000145002
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 = VkStructureType 1000145003
-- | Just "Queue is a protected-capable device queue"
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT :: VkDeviceQueueCreateFlagBits
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT = VkDeviceQueueCreateFlagBits 0x00000001
-- | Just "Queues may support protected operations"
pattern VK_QUEUE_PROTECTED_BIT :: VkQueueFlagBits
pattern VK_QUEUE_PROTECTED_BIT = VkQueueFlagBits 0x00000010
-- | Just "Memory is protected"
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT = VkMemoryPropertyFlagBits 0x00000020
-- | Just "Buffer requires protected memory"
pattern VK_BUFFER_CREATE_PROTECTED_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_PROTECTED_BIT = VkBufferCreateFlagBits 0x00000008
-- | Just "Image requires protected memory"
pattern VK_IMAGE_CREATE_PROTECTED_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_PROTECTED_BIT = VkImageCreateFlagBits 0x00000800
-- | Just "Command buffers allocated from pool are protected command buffers"
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT = VkCommandPoolCreateFlagBits 0x00000004
-- | 
foreign import ccall "vkGetDeviceQueue2" vkGetDeviceQueue2 :: ("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
-- | TODO: Struct comments
data VkProtectedSubmitInfo = VkProtectedSubmitInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkProtectedSubmit :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkProtectedSubmitInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkProtectedSubmitInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkProtectedSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkProtectedSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkProtectedSubmit (poked :: VkProtectedSubmitInfo))
-- | TODO: Struct comments
data VkPhysicalDeviceProtectedMemoryFeatures = VkPhysicalDeviceProtectedMemoryFeatures
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkProtectedMemory :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProtectedMemoryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProtectedMemoryFeatures <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
                *> poke (ptr `plusPtr` 16) (vkProtectedMemory (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
-- | TODO: Struct comments
data VkPhysicalDeviceProtectedMemoryProperties = VkPhysicalDeviceProtectedMemoryProperties
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkProtectedNoFault :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProtectedMemoryProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProtectedMemoryProperties <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProtectedMemoryProperties))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkPhysicalDeviceProtectedMemoryProperties))
                *> poke (ptr `plusPtr` 16) (vkProtectedNoFault (poked :: VkPhysicalDeviceProtectedMemoryProperties))
-- | TODO: Struct comments
data VkDeviceQueueInfo2 = VkDeviceQueueInfo2
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkFlags :: VkDeviceQueueCreateFlags
  , vkQueueFamilyIndex :: Word32
  , vkQueueIndex :: Word32
  }
  deriving (Eq, Show)

instance Storable VkDeviceQueueInfo2 where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceQueueInfo2 <$> peek (ptr `plusPtr` 0)
                                <*> peek (ptr `plusPtr` 8)
                                <*> peek (ptr `plusPtr` 16)
                                <*> peek (ptr `plusPtr` 20)
                                <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 24) (vkQueueIndex (poked :: VkDeviceQueueInfo2))
