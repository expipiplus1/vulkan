{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language DataKinds #-}
{-# language TypeOperators #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_device_group_creation
  ( VkDeviceGroupDeviceCreateInfo(..)
  , VkPhysicalDeviceGroupProperties(..)
  , FN_vkEnumeratePhysicalDeviceGroups
  , PFN_vkEnumeratePhysicalDeviceGroups
  , vkEnumeratePhysicalDeviceGroups
  , VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MAX_DEVICE_GROUP_SIZE
  , pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT
  , pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
  ) where

import Data.Vector.Storable.Sized
  ( Vector
  )
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
  ( VkBool32(..)
  , VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core10.DeviceInitialization
  ( VkMemoryHeapFlagBits(..)
  , VkInstance
  , VkPhysicalDevice
  )
import Graphics.Vulkan.C.Dynamic
  ( InstanceCmds(..)
  )
import Graphics.Vulkan.NamedType
  ( (:::)
  )


-- No documentation found for TopLevel "VkDeviceGroupDeviceCreateInfo"
data VkDeviceGroupDeviceCreateInfo = VkDeviceGroupDeviceCreateInfo
  { -- No documentation found for Nested "VkDeviceGroupDeviceCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDeviceGroupDeviceCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDeviceGroupDeviceCreateInfo" "physicalDeviceCount"
  vkPhysicalDeviceCount :: Word32
  , -- No documentation found for Nested "VkDeviceGroupDeviceCreateInfo" "pPhysicalDevices"
  vkPPhysicalDevices :: Ptr VkPhysicalDevice
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

instance Zero VkDeviceGroupDeviceCreateInfo where
  zero = VkDeviceGroupDeviceCreateInfo VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO
                                       zero
                                       zero
                                       zero

-- No documentation found for TopLevel "VkPhysicalDeviceGroupProperties"
data VkPhysicalDeviceGroupProperties = VkPhysicalDeviceGroupProperties
  { -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "physicalDeviceCount"
  vkPhysicalDeviceCount :: Word32
  , -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "physicalDevices"
  vkPhysicalDevices :: Vector VK_MAX_DEVICE_GROUP_SIZE VkPhysicalDevice
  , -- No documentation found for Nested "VkPhysicalDeviceGroupProperties" "subsetAllocation"
  vkSubsetAllocation :: VkBool32
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

instance Zero VkPhysicalDeviceGroupProperties where
  zero = VkPhysicalDeviceGroupProperties VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES
                                         zero
                                         zero
                                         zero
                                         zero

-- No documentation found for TopLevel "vkEnumeratePhysicalDeviceGroups"
#if defined(EXPOSE_CORE11_COMMANDS)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "vkEnumeratePhysicalDeviceGroups" vkEnumeratePhysicalDeviceGroups :: ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
#else
vkEnumeratePhysicalDeviceGroups :: InstanceCmds -> ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
vkEnumeratePhysicalDeviceGroups deviceCmds = mkVkEnumeratePhysicalDeviceGroups (pVkEnumeratePhysicalDeviceGroups deviceCmds)
foreign import ccall
#if !defined(SAFE_FOREIGN_CALLS)
  unsafe
#endif
  "dynamic" mkVkEnumeratePhysicalDeviceGroups
  :: FunPtr (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult) -> (("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult)
#endif

type FN_vkEnumeratePhysicalDeviceGroups = ("instance" ::: VkInstance) -> ("pPhysicalDeviceGroupCount" ::: Ptr Word32) -> ("pPhysicalDeviceGroupProperties" ::: Ptr VkPhysicalDeviceGroupProperties) -> IO VkResult
type PFN_vkEnumeratePhysicalDeviceGroups = FunPtr FN_vkEnumeratePhysicalDeviceGroups

-- No documentation found for TopLevel "VK_MAX_DEVICE_GROUP_SIZE"
type VK_MAX_DEVICE_GROUP_SIZE = 32
-- No documentation found for Nested "Integral a => a" "VK_MAX_DEVICE_GROUP_SIZE"
pattern VK_MAX_DEVICE_GROUP_SIZE :: Integral a => a
pattern VK_MAX_DEVICE_GROUP_SIZE = 32

-- No documentation found for Nested "VkMemoryHeapFlagBits" "VK_MEMORY_HEAP_MULTI_INSTANCE_BIT"
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT :: VkMemoryHeapFlagBits
pattern VK_MEMORY_HEAP_MULTI_INSTANCE_BIT = VkMemoryHeapFlagBits 0x00000002

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEVICE_GROUP_DEVICE_CREATE_INFO = VkStructureType 1000070001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_GROUP_PROPERTIES = VkStructureType 1000070000
