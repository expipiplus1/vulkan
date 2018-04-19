{-# language Strict #-}
{-# language CPP #-}
{-# language PatternSynonyms #-}
{-# language DuplicateRecordFields #-}

module Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory
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


import Graphics.Vulkan.Core10.Core
  ( VkStructureType(..)
  , VkResult(..)
  )
import Graphics.Vulkan.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlags
  )


-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_EXTERNAL_HANDLE"
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE :: VkResult
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE = VkResult (-1000072003)
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO = VkStructureType 1000072000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO = VkStructureType 1000072001
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO = VkStructureType 1000072002
-- No documentation found for Nested "Word32" "VK_QUEUE_FAMILY_EXTERNAL"
pattern VK_QUEUE_FAMILY_EXTERNAL :: Word32
pattern VK_QUEUE_FAMILY_EXTERNAL = 0xfffffffe
-- | VkExternalMemoryImageCreateInfo - Specify that an image may be backed by
-- external memory
data VkExternalMemoryImageCreateInfo = VkExternalMemoryImageCreateInfo
  { -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "vkHandleTypes"
  vkHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryImageCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryImageCreateInfo <$> peek (ptr `plusPtr` 0)
                                             <*> peek (ptr `plusPtr` 8)
                                             <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryImageCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryImageCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryImageCreateInfo))
-- | VkExternalMemoryBufferCreateInfo - Specify that a buffer may be backed
-- by external memory
data VkExternalMemoryBufferCreateInfo = VkExternalMemoryBufferCreateInfo
  { -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "vkHandleTypes"
  vkHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExternalMemoryBufferCreateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExternalMemoryBufferCreateInfo <$> peek (ptr `plusPtr` 0)
                                              <*> peek (ptr `plusPtr` 8)
                                              <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExternalMemoryBufferCreateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExternalMemoryBufferCreateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExternalMemoryBufferCreateInfo))
-- | VkExportMemoryAllocateInfo - Specify exportable handle types for a
-- device memory object
data VkExportMemoryAllocateInfo = VkExportMemoryAllocateInfo
  { -- No documentation found for Nested "VkExportMemoryAllocateInfo" "vkSType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportMemoryAllocateInfo" "vkPNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportMemoryAllocateInfo" "vkHandleTypes"
  vkHandleTypes :: VkExternalMemoryHandleTypeFlags
  }
  deriving (Eq, Show)

instance Storable VkExportMemoryAllocateInfo where
  sizeOf ~_ = 24
  alignment ~_ = 8
  peek ptr = VkExportMemoryAllocateInfo <$> peek (ptr `plusPtr` 0)
                                        <*> peek (ptr `plusPtr` 8)
                                        <*> peek (ptr `plusPtr` 16)
  poke ptr poked = poke (ptr `plusPtr` 0) (vkSType (poked :: VkExportMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 8) (vkPNext (poked :: VkExportMemoryAllocateInfo))
                *> poke (ptr `plusPtr` 16) (vkHandleTypes (poked :: VkExportMemoryAllocateInfo))
