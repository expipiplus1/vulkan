{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_device_group_creation
  ( pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  , VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  , vkEnumeratePhysicalDeviceGroups
  , VkPhysicalDeviceGroupProperties(..)
  , VkDeviceGroupDeviceCreateInfo(..)
  ) where

import Data.Vector.Storable.Sized
  ( Vector
  )
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.DeviceInitialization
  ( VkPhysicalDevice
  , VkInstance
  , VkMemoryHeapFlagBits(..)
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES = VkStructureType 1000070000
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO = VkStructureType 1000070001
-- | Just "If set, heap allocations allocate multiple instances by default"
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT = VkMemoryHeapFlagBits 0x00000002
type VK_MAX_DEVICE_GROUP_SIZE = 32
pattern VK_MAX_DEVICE_GROUP_SIZE :: Integral a => a
pattern VK_MAX_DEVICE_GROUP_SIZE = 32
-- | 
foreign import ccall "vkEnumeratePhysicalDeviceGroups" vkEnumeratePhysicalDeviceGroups :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
-- | TODO: Struct comments
data VkPhysicalDeviceGroupProperties = VkPhysicalDeviceGroupProperties
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkPhysicalDeviceCount :: Word32
  , vkPhysicalDevices :: Vector VK_MAX_DEVICE_GROUP_SIZE VkPhysicalDevice
  , vkSubsetAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceGroupProperties where
  sizeOf ~_ = 288
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceGroupProperties <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
                                             <*> peek (ptr `plusPtr` 24)
                                             <*> peek (ptr `plusPtr` 280)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDeviceCount (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 24) (vkPhysicalDevices (poked :: VkPhysicalDeviceGroupProperties))
                *> poke (ptr `plusPtr` 280) (vkSubsetAllocation (poked :: VkPhysicalDeviceGroupProperties))
-- | TODO: Struct comments
data VkDeviceGroupDeviceCreateInfo = VkDeviceGroupDeviceCreateInfo
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkPhysicalDeviceCount :: Word32
  , vkPPhysicalDevices :: Ptr VkPhysicalDevice
  }
  deriving (Eq, Show)

instance Storable VkDeviceGroupDeviceCreateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDeviceGroupDeviceCreateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDeviceGroupDeviceCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceGroupDeviceCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkPhysicalDeviceCount (poked :: VkDeviceGroupDeviceCreateInfo))
                *> poke (ptr `plusPtr` 24) (vkPPhysicalDevices (poked :: VkDeviceGroupDeviceCreateInfo))
