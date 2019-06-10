{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_EXT_memory_priority
  ( VkMemoryPriorityAllocateInfoEXT(..)
  , VkPhysicalDeviceMemoryPriorityFeaturesEXT(..)
  , pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME
  , pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
  , pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
  ) where

import Data.String
  ( IsString
  )
import Foreign.C.Types
  ( CFloat(..)
  )
import Foreign.Ptr
  ( Ptr
  , plusPtr
  )
import Foreign.Storable
  ( Storable
  , Storable(..)
  )


import Graphics.Vulkan.C.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  , Zero(..)
  )


-- No documentation found for TopLevel "VkMemoryPriorityAllocateInfoEXT"
data VkMemoryPriorityAllocateInfoEXT = VkMemoryPriorityAllocateInfoEXT
  { -- No documentation found for Nested "VkMemoryPriorityAllocateInfoEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryPriorityAllocateInfoEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryPriorityAllocateInfoEXT" "priority"
  vkPriority :: CFloat
  }
  deriving (Eq, Show)

instance Storable VkMemoryPriorityAllocateInfoEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryPriorityAllocateInfoEXT <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryPriorityAllocateInfoEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryPriorityAllocateInfoEXT))
                *> poke (ptr `plusPtr` 16) (vkPriority (poked :: VkMemoryPriorityAllocateInfoEXT))

instance Zero VkMemoryPriorityAllocateInfoEXT where
  zero = VkMemoryPriorityAllocateInfoEXT VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT
                                         zero
                                         zero

-- No documentation found for TopLevel "VkPhysicalDeviceMemoryPriorityFeaturesEXT"
data VkPhysicalDeviceMemoryPriorityFeaturesEXT = VkPhysicalDeviceMemoryPriorityFeaturesEXT
  { -- No documentation found for Nested "VkPhysicalDeviceMemoryPriorityFeaturesEXT" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryPriorityFeaturesEXT" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkPhysicalDeviceMemoryPriorityFeaturesEXT" "memoryPriority"
  vkMemoryPriority :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkPhysicalDeviceMemoryPriorityFeaturesEXT where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkPhysicalDeviceMemoryPriorityFeaturesEXT <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkPhysicalDeviceMemoryPriorityFeaturesEXT))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkPhysicalDeviceMemoryPriorityFeaturesEXT))
                *> poke (ptr `plusPtr` 16) (vkMemoryPriority (poked :: VkPhysicalDeviceMemoryPriorityFeaturesEXT))

instance Zero VkPhysicalDeviceMemoryPriorityFeaturesEXT where
  zero = VkPhysicalDeviceMemoryPriorityFeaturesEXT VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT
                                                   zero
                                                   zero

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME"
pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_EXT_MEMORY_PRIORITY_EXTENSION_NAME = "VK_EXT_memory_priority"

-- No documentation found for TopLevel "VK_EXT_MEMORY_PRIORITY_SPEC_VERSION"
pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION :: Integral a => a
pattern VK_EXT_MEMORY_PRIORITY_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT"
pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_PRIORITY_ALLOCATE_INFO_EXT = VkStructureType 1000238001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT"
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT :: VkStructureType
pattern VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PRIORITY_FEATURES_EXT = VkStructureType 1000238000
