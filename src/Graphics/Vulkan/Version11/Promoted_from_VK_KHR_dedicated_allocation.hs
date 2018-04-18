{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_dedicated_allocation
  ( pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , VkMemoryDedicatedRequirements(..)
  , VkMemoryDedicatedAllocateInfo(..)
  ) where

import Foreign.Ptr
  ( plusPtr
  , Ptr
  )
import Foreign.Storable
  ( Storable(..)
  , Storable
  )


import Graphics.Vulkan.Version10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Version10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS = VkStructureType 1000127000
-- | Nothing
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO = VkStructureType 1000127001
-- | TODO: Struct comments
data VkMemoryDedicatedRequirements = VkMemoryDedicatedRequirements
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkPrefersDedicatedAllocation :: VkBool32
  , vkRequiresDedicatedAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkMemoryDedicatedRequirements where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkMemoryDedicatedRequirements <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 20)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 16) (vkPrefersDedicatedAllocation (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 20) (vkRequiresDedicatedAllocation (poked :: VkMemoryDedicatedRequirements))
-- | TODO: Struct comments
data VkMemoryDedicatedAllocateInfo = VkMemoryDedicatedAllocateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkImage :: VkImage
  , vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkMemoryDedicatedAllocateInfo where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkMemoryDedicatedAllocateInfo <$> peek (ptr `plusPtr` 0)
                                           <*> peek (ptr `plusPtr` 8)
                                           <*> peek (ptr `plusPtr` 16)
                                           <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkMemoryDedicatedAllocateInfo))
