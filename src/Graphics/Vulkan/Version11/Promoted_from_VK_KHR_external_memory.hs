{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Version11.Promoted_from_VK_KHR_external_memory
  ( pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_QUEUE_FAMILY_EXTERNAL
  , VkExternalMemoryImageCreateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExportMemoryAllocateInfo(..)
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


import Graphics.Vulkan.Version10.Core
  ( VkStructureType(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Version11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlags
  )


-- | Nothing
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE :: VkResult
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE = VkResult (-1000072003)
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO = VkStructureType 1000072000
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO = VkStructureType 1000072001
-- | Nothing
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO = VkStructureType 1000072002
pattern VK_QUEUE_FAMILY_EXTERNAL :: Word32
pattern VK_QUEUE_FAMILY_EXTERNAL = 0xfffffffe
-- | TODO: Struct comments
data VkExternalMemoryImageCreateInfo = VkExternalMemoryImageCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryImageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExternalMemoryImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfo))
-- | TODO: Struct comments
data VkExternalMemoryBufferCreateInfo = VkExternalMemoryBufferCreateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryBufferCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryBufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryBufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExternalMemoryBufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryBufferCreateInfo))
-- | TODO: Struct comments
data VkExportMemoryAllocateInfo = VkExportMemoryAllocateInfo
  { vkSType :: VkStructureType
  , vkNext :: Ptr ()
  , vkHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryAllocateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkNext (poked :: VkExportMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfo))
