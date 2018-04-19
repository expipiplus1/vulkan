{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_dedicated_allocation
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


import Graphics.Vulkan.Core10.Core
  ( VkBool32(..)
  , VkStructureType(..)
  )
import Graphics.Vulkan.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS = VkStructureType 1000127000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO = VkStructureType 1000127001
-- | VkMemoryDedicatedRequirements - Structure describing dedicated
-- allocation requirements of buffer and image resources
data VkMemoryDedicatedRequirements = VkMemoryDedicatedRequirements
  { -- No documentation found for Nested "VkMemoryDedicatedRequirements" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "vkPrefersDedicatedAllocation"
  vkPrefersDedicatedAllocation :: VkBool32
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "vkRequiresDedicatedAllocation"
  vkRequiresDedicatedAllocation :: VkBool32
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 16) (vkPrefersDedicatedAllocation (poked :: VkMemoryDedicatedRequirements))
                *> poke (ptr `plusPtr` 20) (vkRequiresDedicatedAllocation (poked :: VkMemoryDedicatedRequirements))
-- | VkMemoryDedicatedAllocateInfo - Specify a dedicated memory allocation
-- resource
data VkMemoryDedicatedAllocateInfo = VkMemoryDedicatedAllocateInfo
  { -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "vkImage"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "vkBuffer"
  vkBuffer :: VkBuffer
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
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkMemoryDedicatedAllocateInfo))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkMemoryDedicatedAllocateInfo))
