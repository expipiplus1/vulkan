{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_From_VK_KHR_protected_memory
  ( VkDeviceQueueInfo2(..)
  , VkPhysicalDeviceProtectedMemoryFeatures(..)
  , VkPhysicalDeviceProtectedMemoryProperties(..)
  , VkProtectedSubmitInfo(..)
  , FN_vkGetDeviceQueue2
  , PFN_vkGetDeviceQueue2
  , vkGetDeviceQueue2
  , pattern VK_BUFFER_CREATE_PROTECTED_BIT
  , pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT
  , pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT
  , pattern VK_IMAGE_CREATE_PROTECTED_BIT
  , pattern VK_MEMORY_PROPERTY_PROTECTED_BIT
  , pattern VK_QUEUE_PROTECTED_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
  , pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
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


import Graphics.Vulkan.C.Core10.Buffer
  ( VkBufferCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.CommandPool
  ( VkCommandPoolCreateFlagBits(..)
  )
import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.Device
  ( VkDeviceQueueCreateFlagBits(..)
  , VkDeviceQueueCreateFlags
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkImageCreateFlagBits(..)
  , VkMemoryPropertyFlagBits(..)
  , VkQueueFlagBits(..)
  , VkDevice
  )
import Graphics.Vulkan.C.Core10.Queue
  ( VkQueue
  )
import Graphics.Vulkan.C.Dynamic
  ( DeviceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDeviceQueueInfo2"
data VkDeviceQueueInfo2 = VkDeviceQueueInfo2
  { -- No documentation found for Nested "VkDeviceQueueInfo2" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceQueueInfo2" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceQueueInfo2" "flags"
  vkFlags :: VkDeviceQueueCreateFlags
  , -- No documentation found for Nested "VkDeviceQueueInfo2" "queueFamilyIndex"
  vkQueueFamilyIndex :: Word32
  , -- No documentation found for Nested "VkDeviceQueueInfo2" "queueIndex"
  vkQueueIndex :: Word32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 16) (vkFlags (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 20) (vkQueueFamilyIndex (poked :: VkDeviceQueueInfo2))
                *> poke (ptr `plusPtr` 24) (vkQueueIndex (poked :: VkDeviceQueueInfo2))

instance Zero VkDeviceQueueInfo2 where
  zero = VkDeviceQueueInfo2 VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2
                            zero
                            zero
                            zero
                            zero

-- No documentation found for TopLevel "VkPhysicalDeviceProtectedMemoryFeatures"
data VkPhysicalDeviceProtectedMemoryFeatures = VkPhysicalDeviceProtectedMemoryFeatures
  { -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryFeatures" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryFeatures" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryFeatures" "protectedMemory"
  vkProtectedMemory :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProtectedMemoryFeatures where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProtectedMemoryFeatures <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProtectedMemoryFeatures))
                *> poke (ptr `plusPtr` 16) (vkProtectedMemory (poked :: VkPhysicalDeviceProtectedMemoryFeatures))

instance Zero VkPhysicalDeviceProtectedMemoryFeatures where
  zero = VkPhysicalDeviceProtectedMemoryFeatures VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES
                                                 zero
                                                 zero

-- No documentation found for TopLevel "VkPhysicalDeviceProtectedMemoryProperties"
data VkPhysicalDeviceProtectedMemoryProperties = VkPhysicalDeviceProtectedMemoryProperties
  { -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceProtectedMemoryProperties" "protectedNoFault"
  vkProtectedNoFault :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceProtectedMemoryProperties where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceProtectedMemoryProperties <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceProtectedMemoryProperties))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceProtectedMemoryProperties))
                *> poke (ptr `plusPtr` 16) (vkProtectedNoFault (poked :: VkPhysicalDeviceProtectedMemoryProperties))

instance Zero VkPhysicalDeviceProtectedMemoryProperties where
  zero = VkPhysicalDeviceProtectedMemoryProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES
                                                   zero
                                                   zero

-- No documentation found for TopLevel "VkProtectedSubmitInfo"
data VkProtectedSubmitInfo = VkProtectedSubmitInfo
  { -- No documentation found for Nested "VkProtectedSubmitInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkProtectedSubmitInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkProtectedSubmitInfo" "protectedSubmit"
  vkProtectedSubmit :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkProtectedSubmitInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkProtectedSubmitInfo <$> peek (ptr `plusPtr` 0)
                                   <*> peek (ptr `plusPtr` 8)
                                   <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkProtectedSubmitInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkProtectedSubmitInfo))
                *> poke (ptr `plusPtr` 16) (vkProtectedSubmit (poked :: VkProtectedSubmitInfo))

instance Zero VkProtectedSubmitInfo where
  zero = VkProtectedSubmitInfo VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO
                               zero
                               zero

-- No documentation found for TopLevel "vkGetDeviceQueue2"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkGetDeviceQueue2" vkGetDeviceQueue2 :: ("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
#else
vkGetDeviceQueue2 :: DeviceCmds -> ("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
vkGetDeviceQueue2 deviceCmds = mkVkGetDeviceQueue2 (pVkGetDeviceQueue2 deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkGetDeviceQueue2
  :: FunPtr (("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()) -> (("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ())
#endif

type FN_vkGetDeviceQueue2 = ("device" ::: VkDevice) -> ("pQueueInfo" ::: Ptr VkDeviceQueueInfo2) -> ("pQueue" ::: Ptr VkQueue) -> IO ()
type PFN_vkGetDeviceQueue2 = FunPtr FN_vkGetDeviceQueue2

-- No documentation found for Nested "VkBufferCreateFlagBits" "VK_BUFFER_CREATE_PROTECTED_BIT"
pattern VK_BUFFER_CREATE_PROTECTED_BIT :: VkBufferCreateFlagBits
pattern VK_BUFFER_CREATE_PROTECTED_BIT = VkBufferCreateFlagBits 0x00000008

-- No documentation found for Nested "VkCommandPoolCreateFlagBits" "VK_COMMAND_POOL_CREATE_PROTECTED_BIT"
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT :: VkCommandPoolCreateFlagBits
pattern VK_COMMAND_POOL_CREATE_PROTECTED_BIT = VkCommandPoolCreateFlagBits 0x00000004

-- No documentation found for Nested "VkDeviceQueueCreateFlagBits" "VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT"
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT :: VkDeviceQueueCreateFlagBits
pattern VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT = VkDeviceQueueCreateFlagBits 0x00000001

-- No documentation found for Nested "VkImageCreateFlagBits" "VK_IMAGE_CREATE_PROTECTED_BIT"
pattern VK_IMAGE_CREATE_PROTECTED_BIT :: VkImageCreateFlagBits
pattern VK_IMAGE_CREATE_PROTECTED_BIT = VkImageCreateFlagBits 0x00000800

-- No documentation found for Nested "VkMemoryPropertyFlagBits" "VK_MEMORY_PROPERTY_PROTECTED_BIT"
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT :: VkMemoryPropertyFlagBits
pattern VK_MEMORY_PROPERTY_PROTECTED_BIT = VkMemoryPropertyFlagBits 0x00000020

-- No documentation found for Nested "VkQueueFlagBits" "VK_QUEUE_PROTECTED_BIT"
pattern VK_QUEUE_PROTECTED_BIT :: VkQueueFlagBits
pattern VK_QUEUE_PROTECTED_BIT = VkQueueFlagBits 0x00000010

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2"
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_QUEUE_INFO_2 = VkStructureType 1000145003

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_FEATURES = VkStructureType 1000145001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_PROTECTED_MEMORY_PROPERTIES = VkStructureType 1000145002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO"
pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_PROTECTED_SUBMIT_INFO = VkStructureType 1000145000
