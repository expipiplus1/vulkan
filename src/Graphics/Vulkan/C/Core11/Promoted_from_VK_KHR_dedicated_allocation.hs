{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_dedicated_allocation
  ( VkMemoryDedicatedAllocateInfo(..)
  , VkMemoryDedicatedRequirements(..)
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS
  ) where

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
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for TopLevel "VkMemoryDedicatedAllocateInfo"
data VkMemoryDedicatedAllocateInfo = VkMemoryDedicatedAllocateInfo
  { -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkMemoryDedicatedAllocateInfo" "buffer"
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

instance Zero VkMemoryDedicatedAllocateInfo where
  zero = VkMemoryDedicatedAllocateInfo zero
                                       zero
                                       zero
                                       zero
-- No documentation found for TopLevel "VkMemoryDedicatedRequirements"
data VkMemoryDedicatedRequirements = VkMemoryDedicatedRequirements
  { -- No documentation found for Nested "VkMemoryDedicatedRequirements" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "prefersDedicatedAllocation"
  vkPrefersDedicatedAllocation :: VkBool32
  , -- No documentation found for Nested "VkMemoryDedicatedRequirements" "requiresDedicatedAllocation"
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

instance Zero VkMemoryDedicatedRequirements where
  zero = VkMemoryDedicatedRequirements zero
                                       zero
                                       zero
                                       zero
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_ALLOCATE_INFO = VkStructureType 1000127001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS"
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS :: VkStructureType
pattern VK_STRUCTURE_TYPE_MEMORY_DEDICATED_REQUIREMENTS = VkStructureType 1000127000
