{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}

module Graphics.Vulkan.C.Extensions.VK_NV_dedicated_allocation
  ( VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  , pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  ) where

import Data.String
  ( IsString
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
import Graphics.Vulkan.C.Core10.MemoryManagement
  ( VkBuffer
  , VkImage
  )


-- No documentation found for TopLevel "VkDedicatedAllocationBufferCreateInfoNV"
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDedicatedAllocationBufferCreateInfoNV" "dedicatedAllocation"
  vkDedicatedAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDedicatedAllocationBufferCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDedicatedAllocationBufferCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                     <*> peek (ptr `plusPtr` 8)
                                                     <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDedicatedAllocationBufferCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDedicatedAllocationBufferCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkDedicatedAllocation (poked :: VkDedicatedAllocationBufferCreateInfoNV))

instance Zero VkDedicatedAllocationBufferCreateInfoNV where
  zero = VkDedicatedAllocationBufferCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
                                                 zero
                                                 zero

-- No documentation found for TopLevel "VkDedicatedAllocationImageCreateInfoNV"
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDedicatedAllocationImageCreateInfoNV" "dedicatedAllocation"
  vkDedicatedAllocation :: VkBool32
  }
  deriving (Eq, Show)

instance Storable VkDedicatedAllocationImageCreateInfoNV where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkDedicatedAllocationImageCreateInfoNV <$> peek (ptr `plusPtr` 0)
                                                    <*> peek (ptr `plusPtr` 8)
                                                    <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDedicatedAllocationImageCreateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDedicatedAllocationImageCreateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkDedicatedAllocation (poked :: VkDedicatedAllocationImageCreateInfoNV))

instance Zero VkDedicatedAllocationImageCreateInfoNV where
  zero = VkDedicatedAllocationImageCreateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
                                                zero
                                                zero

-- No documentation found for TopLevel "VkDedicatedAllocationMemoryAllocateInfoNV"
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV
  { -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "image"
  vkImage :: VkImage
  , -- No documentation found for Nested "VkDedicatedAllocationMemoryAllocateInfoNV" "buffer"
  vkBuffer :: VkBuffer
  }
  deriving (Eq, Show)

instance Storable VkDedicatedAllocationMemoryAllocateInfoNV where
  sizeOf ~_ = 32
  alignment ~_ = 8
  peek ptr = VkDedicatedAllocationMemoryAllocateInfoNV <$> peek (ptr `plusPtr` 0)
                                                       <*> peek (ptr `plusPtr` 8)
                                                       <*> peek (ptr `plusPtr` 16)
                                                       <*> peek (ptr `plusPtr` 24)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 16) (vkImage (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))
                *> poke (ptr `plusPtr` 24) (vkBuffer (poked :: VkDedicatedAllocationMemoryAllocateInfoNV))

instance Zero VkDedicatedAllocationMemoryAllocateInfoNV where
  zero = VkDedicatedAllocationMemoryAllocateInfoNV VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
                                                   zero
                                                   zero
                                                   zero

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME"
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a, IsString a) => a
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_NV_dedicated_allocation"

-- No documentation found for TopLevel "VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION"
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV = VkStructureType 1000026001

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV = VkStructureType 1000026000

-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV"
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000026002
