{-# language Strict #-}
{-# language CPP #-}
{-# language DuplicateRecordFields #-}
{-# language PatternSynonyms #-}

module Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory
  ( VkExportMemoryAllocateInfo(..)
  , VkExternalMemoryBufferCreateInfo(..)
  , VkExternalMemoryImageCreateInfo(..)
  , pattern VK_ERROR_INVALID_EXTERNAL_HANDLE
  , pattern VK_QUEUE_FAMILY_EXTERNAL
  , pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO
  , pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO
  ) where

import Data.Word
  ( Word32
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
  ( VkResult(..)
  , VkStructureType(..)
  , Zero(..)
  )
import Graphics.Vulkan.C.Core11.Promoted_from_VK_KHR_external_memory_capabilities
  ( VkExternalMemoryHandleTypeFlags
  )


-- No documentation found for TopLevel "VkExportMemoryAllocateInfo"
data VkExportMemoryAllocateInfo = VkExportMemoryAllocateInfo
  { -- No documentation found for Nested "VkExportMemoryAllocateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExportMemoryAllocateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExportMemoryAllocateInfo" "handleTypes"
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

instance Zero VkExportMemoryAllocateInfo where
  zero = VkExportMemoryAllocateInfo zero
                                    zero
                                    zero
-- No documentation found for TopLevel "VkExternalMemoryBufferCreateInfo"
data VkExternalMemoryBufferCreateInfo = VkExternalMemoryBufferCreateInfo
  { -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalMemoryBufferCreateInfo" "handleTypes"
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

instance Zero VkExternalMemoryBufferCreateInfo where
  zero = VkExternalMemoryBufferCreateInfo zero
                                          zero
                                          zero
-- No documentation found for TopLevel "VkExternalMemoryImageCreateInfo"
data VkExternalMemoryImageCreateInfo = VkExternalMemoryImageCreateInfo
  { -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "sType"
  vkSType :: VkStructureType
  , -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "pNext"
  vkPNext :: Ptr ()
  , -- No documentation found for Nested "VkExternalMemoryImageCreateInfo" "handleTypes"
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

instance Zero VkExternalMemoryImageCreateInfo where
  zero = VkExternalMemoryImageCreateInfo zero
                                         zero
                                         zero
-- No documentation found for Nested "VkResult" "VK_ERROR_INVALID_EXTERNAL_HANDLE"
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE :: VkResult
pattern VK_ERROR_INVALID_EXTERNAL_HANDLE = VkResult (-1000072003)
-- No documentation found for Nested "Word32" "VK_QUEUE_FAMILY_EXTERNAL"
pattern VK_QUEUE_FAMILY_EXTERNAL :: Word32
pattern VK_QUEUE_FAMILY_EXTERNAL = 0xfffffffe
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO"
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXPORT_MEMORY_ALLOCATE_INFO = VkStructureType 1000072002
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_BUFFER_CREATE_INFO = VkStructureType 1000072000
-- No documentation found for Nested "VkStructureType" "VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO"
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO :: VkStructureType
pattern VK_STRUCTURE_TYPE_EXTERNAL_MEMORY_IMAGE_CREATE_INFO = VkStructureType 1000072001
