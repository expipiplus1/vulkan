{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language OverloadedStrings #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Extensions.VK_NV_dedicated_allocation
  ( pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV
  , pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV
  , pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION
  , pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME
  , VkDedicatedAllocationImageCreateInfoNV(..)
  , VkDedicatedAllocationBufferCreateInfoNV(..)
  , VkDedicatedAllocationMemoryAllocateInfoNV(..)
  ) where

import Data.String
  ( IsString
  )
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


-- | Nothing
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_IMAGE_CREATE_INFO_NV = VkStructureType 1000026000
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_BUFFER_CREATE_INFO_NV = VkStructureType 1000026001
-- | Nothing
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV :: VkStructureType
pattern VK_STRUCTURE_TYPE_DEDICATED_ALLOCATION_MEMORY_ALLOCATE_INFO_NV = VkStructureType 1000026002
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION :: Integral a => a
pattern VK_NV_DEDICATED_ALLOCATION_SPEC_VERSION = 1
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME :: (Eq a ,IsString a) => a
pattern VK_NV_DEDICATED_ALLOCATION_EXTENSION_NAME = "VK_NV_dedicated_allocation"
-- | TODO: Struct comments
data VkDedicatedAllocationImageCreateInfoNV = VkDedicatedAllocationImageCreateInfoNV
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkDedicatedAllocation :: VkBool32
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
-- | TODO: Struct comments
data VkDedicatedAllocationBufferCreateInfoNV = VkDedicatedAllocationBufferCreateInfoNV
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkDedicatedAllocation :: VkBool32
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
-- | TODO: Struct comments
data VkDedicatedAllocationMemoryAllocateInfoNV = VkDedicatedAllocationMemoryAllocateInfoNV
  { vkSType :: VkStructureType
  , vkPNext :: Ptr ()
  , vkImage :: VkImage
  , vkBuffer :: VkBuffer
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
